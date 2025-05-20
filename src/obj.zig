const std = @import("std");
const Allocator = std.mem.Allocator;
const VM = @import("vm.zig").VM;

pub const Obj = struct {
    type: Type,
    is_const: bool,

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
    hash: u32,
    chars: []const u8,

    pub fn init(vm: *VM, chars: []const u8) !*String {
        const hash = fnvHash(chars);
        const interned = vm.strings.findString(chars, hash);
        if (interned) |s| return s;
        return allocate(vm, chars, true, hash);
    }

    pub fn deinit(self: *String, vm: *VM) void {
        if (!self.obj.is_const) vm.allocator.free(self.chars);
        vm.allocator.destroy(self);
    }

    fn allocate(vm: *VM, chars: []const u8, is_const: bool, hash: u32) !*String {
        const out = try vm.allocator.create(String);
        out.* = .{
            .hash = hash,
            .chars = chars,
            .obj = .{ .type = .String, .is_const = is_const, .next = vm.objects },
        };
        vm.objects = &out.obj;
        _ = try vm.strings.set(out, .{ .Nil = {} });
        return out;
    }

    pub fn concat(self: *String, vm: *VM, other: *String) !*String {
        const buffer = try vm.allocator.alloc(u8, self.chars.len + other.chars.len);
        @memcpy(buffer[0..self.chars.len], self.chars);
        @memcpy(buffer[self.chars.len..][0..other.chars.len], other.chars);
        const hash = fnvHash(buffer);
        const interned = vm.strings.findString(buffer, hash);
        if (interned) |s| {
            vm.allocator.free(buffer);
            return s;
        }
        return allocate(vm, buffer, false, hash);
    }

    pub fn eql(self: *String, other: *String) bool {
        return self.chars.ptr == other.chars.ptr;
    }

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("{s}", .{self.chars});
    }
};

const testing = std.testing;
test "concatenate strings" {
    var vm = VM.init(testing.allocator);
    // explicitly free table beause all strings are going
    // to be freed individually in this test
    defer vm.strings.deinit();
    const original_str: []const u8 = "Hello ";
    const a_str = try String.init(&vm, original_str);
    const b_str = try String.init(&vm, "world!");
    const out = try a_str.concat(&vm, b_str);
    defer b_str.deinit(&vm);
    defer out.deinit(&vm);
    // Expect new string has all the right chars
    try testing.expectEqualSlices(u8, "Hello world!", out.chars);
    // Make sure that the "const" source string was not accidentally freed
    // when a_str is freed
    a_str.deinit(&vm);
    try testing.expectEqualStrings("Hello ", original_str);
}

fn fnvHash(key: []const u8) u32 {
    var hash: u32 = 2166136261;
    for (key) |c| {
        hash ^= c;
        hash *%= 16777619;
    }
    return hash;
}

test "fnv hash" {
    const stdfnv = std.hash.Fnv1a_32;
    try testing.expect(fnvHash("foobar") == 0xbf9cf968);
    try testing.expectEqual(stdfnv.hash("foobar"), fnvHash("foobar"));
}
