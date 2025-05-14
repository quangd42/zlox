const std = @import("std");
const Allocator = std.mem.Allocator;

pub const Obj = struct {
    type: Type,

    pub fn asString(self: *Obj) ?*String {
        return if (self.type == .String) @alignCast(@fieldParentPtr("obj", self)) else null;
    }
};

pub const Type = enum { String };

pub const String = struct {
    obj: Obj,
    val: []u8,

    pub fn init(allocator: Allocator, chars: []const u8) !*String {
        const string = try allocator.create(String);
        string.* = .{ .obj = .{ .type = .String }, .val = try allocator.dupe(u8, chars) };
        return string;
    }

    pub fn deinit(self: *String, allocator: Allocator) void {
        allocator.free(self.val);
        allocator.destroy(self);
    }
};
