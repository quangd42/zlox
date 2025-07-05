const std = @import("std");
const testing = std.testing;

const NAN_BOXING = @import("debug").@"nan-boxing";

const Obj = @import("obj.zig");

const DEBUGGING = @import("builtin").mode == .Debug;

pub const Value = if (NAN_BOXING) NanBoxed else Union;

const NanBoxed = packed struct {
    value: u64,

    const QNAN: u64 = 0x7ffc000000000000;
    const SIGN_BIT: u64 = 0x8000000000000000;

    const TAG_NIL = 1; // 01
    const TAG_FALSE = 2; // 10
    const TAG_TRUE = 3; // 11

    const NIL_VAL = QNAN | TAG_NIL;
    const TRUE_VAL = QNAN | TAG_TRUE;
    const FALSE_VAL = QNAN | TAG_FALSE;

    pub fn is(self: NanBoxed, val_t: Type) bool {
        return switch (val_t) {
            .Bool => (self.value | 1) == TRUE_VAL,
            .Nil => self.value == NIL_VAL,
            .Number => (self.value & QNAN) != QNAN,
            .Obj => (self.value & (QNAN | SIGN_BIT)) == (QNAN | SIGN_BIT),
        };
    }

    pub fn as(self: NanBoxed, comptime val_t: Type) ?@FieldType(Union, @tagName(val_t)) {
        if (!self.is(val_t)) return null;
        return switch (val_t) {
            .Bool => self.value == TRUE_VAL,
            .Nil => {},
            .Number => @bitCast(self.value),
            .Obj => @ptrFromInt(self.value & ~(SIGN_BIT | QNAN)),
        };
    }

    pub fn isObj(self: NanBoxed, obj_t: Obj.Type) bool {
        const obj = self.as(.Obj) orelse return false;
        return obj.type == obj_t;
    }

    pub fn asObj(self: NanBoxed, comptime obj_t: Obj.Type) ?*obj_t.VariantType() {
        const obj = self.as(.Obj) orelse return null;
        if (obj.type != obj_t) return null;
        return obj.as(obj_t);
    }

    pub fn from(value: anytype) NanBoxed {
        return switch (@TypeOf(value)) {
            bool => .{ .value = if (value) TRUE_VAL else FALSE_VAL },
            void => .{ .value = NIL_VAL },
            f64 => .{ .value = @bitCast(value) },
            comptime_float => .{ .value = @bitCast(@as(f64, value)) },
            u64, comptime_int => .{ .value = @bitCast(@as(f64, @floatFromInt(value))) },
            *Obj => .{ .value = SIGN_BIT | QNAN | @intFromPtr(value) },
            else => |t| {
                if (DEBUGGING) std.debug.print("captured value type = {}\n", .{t});
                unreachable;
            },
        };
    }

    pub const Nil = NanBoxed.from({});

    pub fn eql(self: NanBoxed, other: NanBoxed) bool {
        // If we care that "NaN values are not equal to themselves":
        if (self.is(.Number) and other.is(.Number)) {
            return self.as(.Number) == other.as(.Number);
        }
        return self.value == other.value;
    }

    pub fn isFalsey(self: NanBoxed) bool {
        if (self.is(.Nil)) return true;
        const b = self.as(.Bool) orelse return false;
        return !b;
    }

    pub fn format(
        self: NanBoxed,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        if (self.is(.Bool)) {
            try writer.print("{}", .{self.as(.Bool).?});
        } else if (self.is(.Nil)) {
            try writer.writeAll("nil");
        } else if (self.is(.Number)) {
            try writer.print("{d}", .{self.as(.Number).?});
        } else if (self.is(.Obj)) {
            const obj = self.as(.Obj).?;
            switch (obj.type) {
                inline else => |t| try writer.print("{}", .{obj.as(t)}),
            }
        } else unreachable;
    }
};

test "nan-boxing" {
    const expect = testing.expect;
    const expectEqual = testing.expectEqual;

    const n1 = NanBoxed.from(@as(f64, 1));
    const b1 = NanBoxed{ .value = @as(u64, NanBoxed.QNAN | 2) };
    const b2 = NanBoxed{ .value = @as(u64, NanBoxed.QNAN | 3) };
    const nil1 = NanBoxed{ .value = @as(u64, NanBoxed.QNAN | 1) };
    try expect(n1.is(.Number));
    try expect(b1.is(.Bool));
    try expect(b2.is(.Bool));
    try expect(nil1.is(.Nil));

    try expectEqual(@as(u64, 1), n1.as(.Number).?);
    try expect(b1.as(.Bool) == false);
    try expect(b2.as(.Bool) == true);
    try expect(nil1.as(.Nil) == {});

    const ctf314 = NanBoxed.from(3.14); // comptime_float
    const cti314 = NanBoxed.from(314); // comptime_int
    const uint314 = NanBoxed.from(@as(u64, 314)); // u64
    try expectEqual(@as(f64, 3.14), ctf314.as(.Number).?);
    try expectEqual(@as(f64, 314), cti314.as(.Number).?);
    try expectEqual(@as(f64, 314), uint314.as(.Number).?);

    var vm = try @import("vm.zig").VM.init(testing.allocator);
    defer vm.deinit();
    const str_obj1 = try Obj.String.init(vm, "hello world");
    const str_obj2 = try Obj.String.init(vm, "hello ");
    const str_obj3 = try Obj.String.init(vm, "world");
    const str_obj4 = try str_obj2.concat(vm, str_obj3);
    const str1 = NanBoxed.from(&str_obj1.obj);
    const str2 = NanBoxed.from(&str_obj4.obj);
    const fromstr2 = NanBoxed.from(&str_obj4.obj);
    try expect(str1.eql(str2));
    try expect(str1.eql(fromstr2));
    try expect(str1.isObj(.String));
    try expect(str2.isObj(.String));
    try expect(fromstr2.isObj(.String));
    try expectEqual(&str_obj1.obj, str1.as(.Obj));
    try expectEqual(str_obj1, str1.asObj(.String).?);
    try expectEqual(str_obj1, fromstr2.asObj(.String).?);
}

pub const Type = enum {
    Bool,
    Nil,
    Number,
    Obj,
};

const Union = union(Type) {
    Bool: bool,
    Nil: void,
    Number: f64,
    Obj: *Obj,

    pub fn format(
        self: Union,
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

    pub fn is(self: Union, val_t: Type) bool {
        return switch (self) {
            inline else => |_, tag| tag == val_t,
        };
    }

    pub fn isObj(self: Union, obj_t: Obj.Type) bool {
        return switch (self) {
            .Obj => |obj| obj.type == obj_t,
            else => false,
        };
    }

    pub fn as(self: Union, comptime val_t: Type) ?@FieldType(Union, @tagName(val_t)) {
        if (!self.is(val_t)) return null;
        return @field(self, @tagName(val_t));
    }

    pub fn asObj(self: Union, comptime obj_t: Obj.Type) ?*obj_t.VariantType() {
        if (!self.isObj(obj_t)) return null;
        return self.Obj.as(obj_t);
    }

    pub fn from(value: anytype) Union {
        return switch (@TypeOf(value)) {
            bool => .{ .Bool = value },
            u64, f64, comptime_int, comptime_float => .{ .Number = value },
            *Obj => .{ .Obj = value },
            void => .{ .Nil = {} },
            else => |t| {
                if (DEBUGGING) std.debug.print("captured value type = {}\n", .{t});
                unreachable;
            },
        };
    }

    pub fn eql(self: Union, other: Union) bool {
        switch (self) {
            .Obj => |obj| return other.isObj(obj.type) and obj == other.Obj,
            inline else => |value, tag| {
                return tag == std.meta.activeTag(other) and value == @field(other, @tagName(tag));
            },
        }
    }

    pub fn isFalsey(self: Union) bool {
        return switch (self) {
            .Nil => true,
            .Bool => |b| !b,
            else => false,
        };
    }
};

test "Value methods" {
    const num1 = Union{ .Number = 1 };
    const num2 = Union{ .Number = 5 - 4 };
    const fromnum1 = Union.from(1);
    try testing.expect(num1.eql(num2));
    try testing.expect(num1.eql(fromnum1));
    try testing.expect(num1.is(.Number));
    try testing.expect(num2.is(.Number));
    try testing.expect(fromnum1.is(.Number));
    try testing.expectEqual(1, num1.as(.Number).?);
    try testing.expectEqual(1, fromnum1.as(.Number).?);
    try testing.expectEqual(null, num1.as(.Bool));

    const bool1 = Union{ .Bool = true };
    const bool2 = Union{ .Bool = 5 > 4 };
    const frombool2 = Union.from(5 > 4);
    try testing.expect(bool1.eql(bool2));
    try testing.expect(bool1.eql(frombool2));
    try testing.expect(bool1.is(.Bool));
    try testing.expect(bool2.is(.Bool));
    try testing.expect(frombool2.is(.Bool));
    try testing.expectEqual(true, bool2.as(.Bool).?);
    try testing.expectEqual(true, frombool2.as(.Bool).?);
    try testing.expectEqual(null, bool1.as(.Nil));

    const nil1 = Union{ .Nil = {} };
    const nil2 = Union{ .Nil = std.debug.print("", .{}) };
    const fromnil1 = Union{ .Nil = {} };
    const fromnil2 = Union.from({});
    try testing.expect(nil1.eql(nil2));
    try testing.expect(nil1.eql(fromnil2));
    try testing.expect(nil1.eql(fromnil1));
    try testing.expect(nil1.is(.Nil));
    try testing.expect(fromnil1.is(.Nil));
    try testing.expect(nil2.is(.Nil));
    try testing.expectEqual({}, nil2.as(.Nil).?);
    try testing.expectEqual({}, fromnil2.as(.Nil).?);
    try testing.expectEqual(null, nil1.as(.Bool));

    var vm = try @import("vm.zig").VM.init(testing.allocator);
    defer vm.deinit();
    const str_obj1 = try Obj.String.init(vm, "hello world");
    const str_obj2 = try Obj.String.init(vm, "hello ");
    const str_obj3 = try Obj.String.init(vm, "world");
    const str_obj4 = try str_obj2.concat(vm, str_obj3);
    const str1 = Union{ .Obj = @ptrCast(str_obj1) };
    const str2 = Union{ .Obj = &str_obj4.obj };
    const fromstr2 = Union.from(&str_obj4.obj);
    try testing.expect(str1.eql(str2));
    try testing.expect(str1.eql(fromstr2));
    try testing.expect(str1.isObj(.String));
    try testing.expect(str2.isObj(.String));
    try testing.expect(fromstr2.isObj(.String));
    try testing.expectEqual(str_obj1, str1.asObj(.String).?);
    try testing.expectEqual(str_obj1, fromstr2.asObj(.String).?);
}
