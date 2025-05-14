const std = @import("std");
const _obj = @import("obj.zig");
const Obj = _obj.Obj;
const ObjType = _obj.Type;

pub const Value = union(enum) {
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
            // TODO:
            .Obj => |obj| {
                switch (obj.type) {
                    .String => try writer.print("{s}", .{obj.asString().?.val}),
                }
            },
        }
    }

    pub inline fn asBool(self: *Value) ?bool {
        return switch (self) {
            .Bool => |val| val,
            else => null,
        };
    }

    pub inline fn asNumber(self: Value) ?f64 {
        return switch (self) {
            .Number => |val| val,
            else => null,
        };
    }

    pub inline fn asObj(self: Value) ?*Obj {
        return switch (self) {
            .Obj => |val| val,
            else => null,
        };
    }

    pub fn equal(self: Value, other: Value) bool {
        return switch (self) {
            .Obj => |obj| blk: {
                if (std.meta.activeTag(other) != .Obj or other.Obj.type != obj.type) break :blk false;
                switch (obj.type) {
                    .String => {
                        const str_a = obj.asString().?;
                        const str_b = obj.asString().?;
                        break :blk std.mem.eql(u8, str_a.val, str_b.val);
                    },
                }
            },
            inline else => |value, tag| tag == std.meta.activeTag(other) and value == @field(other, @tagName(tag)),
        };
    }

    pub inline fn isFalsey(self: Value) bool {
        return switch (self) {
            .Nil => true,
            .Bool => |b| !b,
            else => false,
        };
    }

    pub inline fn isObjType(self: Value, obj_type: ObjType) bool {
        return switch (self) {
            .Obj => |obj| obj.type == obj_type,
            else => false,
        };
    }
};

test "isObjType" {
    const testing = std.testing;
    const obj: Obj = .{ .type = .String };
    // const str = ObjString{ .obj = obj, .val = "some obj string" };
    var val = Value{ .Obj = &.{ .type = .String } };
    try testing.expectEqual(true, val.isObjType(.String));
    val = Value{ .Obj = &obj };
}
