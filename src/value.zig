const std = @import("std");

pub const ValueType = enum {
    Bool,
    Nil,
    Number,
};

pub const Value = union(ValueType) {
    Bool: bool,
    Nil: void,
    Number: f64,

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
        }
    }

    fn asBool(self: *Value) ?bool {
        return switch (self) {
            .Bool => |val| val,
            else => null,
        };
    }

    pub fn asNumber(self: Value) ?f64 {
        return switch (self) {
            .Number => |val| val,
            else => null,
        };
    }

    pub fn equal(self: Value, other: Value) bool {
        return switch (self) {
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
};

pub const ValueArray = std.ArrayList(Value);
