const std = @import("std");
const Allocator = std.mem.Allocator;
const VM = @import("vm.zig").VM;

pub const Obj = struct {
    type: Type,
    isConst: bool,

    next: ?*Obj = null,

    pub fn deinit(self: *Obj, vm: *VM) void {
        switch (self.type) {
            inline else => {
                const ObjVariant = self.type.VariantType();
                const obj: *ObjVariant = @alignCast(@fieldParentPtr("obj", self));
                obj.deinit(vm);
            },
        }
    }
};

pub const Type = enum {
    String,

    pub fn VariantType(comptime self: @This()) type {
        return switch (self) {
            .String => String,
        };
    }
};

pub const String = struct {
    obj: Obj,
    chars: []const u8,

    pub fn init(vm: *VM, chars: []const u8) !*String {
        const out = try vm.allocator.create(String);
        const obj: Obj = .{ .type = .String, .isConst = true, .next = vm.objects };
        out.* = .{ .obj = obj, .chars = chars };
        vm.objects = &out.obj;
        return out;
    }

    pub fn deinit(self: *String, vm: *VM) void {
        if (!self.obj.isConst) vm.allocator.free(self.chars);
        vm.allocator.destroy(self);
    }

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("{s}", .{self.chars});
    }

    pub fn concat(self: *String, vm: *VM, other: *String) !*String {
        const out = try vm.allocator.create(String);
        const buffer = try vm.allocator.alloc(u8, self.chars.len + other.chars.len);
        @memcpy(buffer[0..self.chars.len], self.chars);
        @memcpy(buffer[self.chars.len..][0..other.chars.len], other.chars);
        out.* = .{
            .obj = .{ .type = .String, .isConst = false, .next = vm.objects },
            .chars = buffer,
        };
        vm.objects = &out.obj;
        return out;
    }
};

test "concatenate strings" {
    const testing = std.testing;
    const vm: *VM = @constCast(&VM.init(testing.allocator));
    const original_str: []const u8 = "Hello ";
    const a_str = try String.init(vm, original_str);
    const b_str = try String.init(vm, "world!");
    const out = try a_str.concat(vm, b_str);
    defer b_str.deinit(vm);
    defer out.deinit(vm);
    // Expect new string has all the right chars
    try testing.expectEqualSlices(u8, "Hello world!", out.chars);
    // Make sure that the "const" source string was not accidentally freed
    // when a_str is freed
    a_str.deinit(vm);
    try testing.expectEqualStrings("Hello ", original_str);
}
