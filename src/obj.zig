const std = @import("std");
const Allocator = std.mem.Allocator;

pub const Obj = struct {
    type: Type,
    isConst: bool,
};

pub const Type = enum {
    String,

    pub fn asStruct(comptime self: @This()) type {
        return switch (self) {
            .String => String,
        };
    }
};

pub const String = struct {
    obj: Obj,
    val: []const u8,

    pub fn init(allocator: Allocator, chars: []const u8) !*String {
        const string = try allocator.create(String);
        string.* = .{ .obj = .{ .type = .String, .isConst = true }, .val = chars };
        return string;
    }

    pub fn deinit(self: *String, allocator: Allocator) void {
        if (!self.obj.isConst) allocator.free(self.val);
        allocator.destroy(self);
    }

    pub fn concat(self: *String, allocator: Allocator, other: *String) !*String {
        const string = try allocator.create(String);
        const buffer = try allocator.alloc(u8, self.val.len + other.val.len);
        @memcpy(buffer[0..self.val.len], self.val);
        @memcpy(buffer[self.val.len..][0..other.val.len], other.val);
        string.* = .{
            .obj = .{ .type = .String, .isConst = false },
            .val = buffer,
        };
        return string;
    }
};

test "concatenate strings" {
    const testing = std.testing;
    const original_str: []const u8 = "Hello ";
    const a_str = try String.init(testing.allocator, original_str);
    const b_str = try String.init(testing.allocator, "world!");
    const out = try a_str.concat(testing.allocator, b_str);
    defer b_str.deinit(testing.allocator);
    defer out.deinit(testing.allocator);
    try testing.expectEqualSlices(u8, "Hello world!", out.val);
    // Make sure that the "const" source string was not accidentally freed
    // when a_str is freed
    a_str.deinit(testing.allocator);
    try testing.expectEqualStrings("Hello ", original_str);
}
