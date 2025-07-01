const std = @import("std");
const testing = std.testing;

const Obj = @import("obj.zig");

pub const Type = enum {
    Bool,
    Nil,
    Number,
    Obj,
};

pub const Value = union(Type) {
    Bool: bool,
    Nil: void,
    Number: f64,
    Obj: *Obj,

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .Bool => |val| try writer.print("{}", .{val}),
            .Nil => try writer.writeAll("nil"),
            .Number => |val| try writer.print("{d}", .{val}),
            .Obj => |obj| {
                switch (obj.type) {
                    inline else => |obj_t| try writer.print("{}", .{obj.as(obj_t)}),
                }
            },
        }
    }

    pub fn is(self: Value, val_t: Type) bool {
        return switch (self) {
            inline else => |_, tag| tag == val_t,
        };
    }

    pub fn isObj(self: Value, obj_t: Obj.Type) bool {
        return switch (self) {
            .Obj => |obj| obj.type == obj_t,
            else => false,
        };
    }

    pub fn as(self: Value, comptime val_t: Type) ?@FieldType(Value, @tagName(val_t)) {
        if (!self.is(val_t)) return null;
        return @field(self, @tagName(val_t));
    }

    pub fn asObj(self: Value, comptime obj_t: Obj.Type) ?*obj_t.VariantType() {
        if (!self.isObj(obj_t)) return null;
        return self.Obj.as(obj_t);
    }

    pub fn eql(self: Value, other: Value) bool {
        switch (self) {
            .Obj => |obj| {
                switch (obj.type) {
                    inline else => |t| return other.isObj(t) and self.Obj.as(t).eql(other.Obj.as(t)),
                }
            },
            inline else => |value, tag| {
                return tag == std.meta.activeTag(other) and value == @field(other, @tagName(tag));
            },
        }
    }

    pub fn isFalsey(self: Value) bool {
        return switch (self) {
            .Nil => true,
            .Bool => |b| !b,
            else => false,
        };
    }
};

test "Value methods" {
    const num1 = Value{ .Number = 1 };
    const num2 = Value{ .Number = 5 - 4 };
    try testing.expect(num1.eql(num2));
    try testing.expect(num1.is(.Number));
    try testing.expect(num2.is(.Number));
    try testing.expectEqual(1, num1.as(.Number).?);
    try testing.expectEqual(null, num1.as(.Bool));

    const bool1 = Value{ .Bool = true };
    const bool2 = Value{ .Bool = 5 > 4 };
    try testing.expect(bool1.eql(bool2));
    try testing.expect(bool1.is(.Bool));
    try testing.expect(bool2.is(.Bool));
    try testing.expectEqual(true, bool2.as(.Bool).?);
    try testing.expectEqual(null, bool1.as(.Nil));

    const nil1 = Value{ .Nil = {} };
    const nil2 = Value{ .Nil = std.debug.print("", .{}) };
    try testing.expect(nil1.eql(nil2));
    try testing.expect(nil1.is(.Nil));
    try testing.expect(nil2.is(.Nil));
    try testing.expectEqual({}, nil2.as(.Nil).?);
    try testing.expectEqual(null, nil1.as(.Bool));

    var vm = try @import("vm.zig").VM.init(testing.allocator);
    defer vm.deinit();
    const str_obj1 = try Obj.String.init(vm, "hello world");
    const str_obj2 = try Obj.String.init(vm, "hello ");
    const str_obj3 = try Obj.String.init(vm, "world");
    const str_obj4 = try str_obj2.concat(vm, str_obj3);
    const str1 = Value{ .Obj = @ptrCast(str_obj1) };
    const str2 = Value{ .Obj = &str_obj4.obj };
    try testing.expect(str1.eql(str2));
    try testing.expect(str1.isObj(.String));
    try testing.expect(str2.isObj(.String));
    try testing.expectEqual(str_obj1, str1.asObj(.String).?);
    // try testing.expectEqual(null, str1.asObj(.Function));
}
