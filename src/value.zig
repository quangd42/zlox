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
};

pub const ValueArray = std.ArrayList(Value);
