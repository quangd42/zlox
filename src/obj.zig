const std = @import("std");
const Allocator = std.mem.Allocator;
const testing = std.testing;

const Chunk = @import("chunk.zig").Chunk;
const Value = @import("value.zig").Value;
const VM = @import("vm.zig").VM;

pub const Obj = struct {
    type: Type,
    next: ?*Obj = null,

    pub fn deinit(self: *Obj, vm: *VM) void {
        switch (self.type) {
            inline else => |t| {
                const ObjVariant = t.VariantType();
                const obj: *ObjVariant = @alignCast(@fieldParentPtr("obj", self));
                obj.deinit(vm);
            },
        }
    }
};

pub const Type = enum {
    Function,
    Native,
    String,

    pub fn VariantType(comptime self: @This()) type {
        return switch (self) {
            .Function => Function,
            .Native => Native,
            .String => String,
        };
    }
};

pub const FunctionType = enum {
    Function,
    Script,
};

pub const Function = struct {
    obj: Obj,
    arity: u8,
    chunk: Chunk,
    name: ?*String,

    const Self = @This();
    pub fn init(vm: *VM) !*Self {
        return allocate(vm);
    }

    pub fn deinit(self: *Self, vm: *VM) void {
        self.chunk.deinit();
        vm.allocator.destroy(self);
    }

    fn allocate(vm: *VM) !*Self {
        const out = try vm.allocator.create(Self);
        out.* = .{
            .obj = .{ .type = .Function, .next = vm.objects },
            .arity = 0,
            .chunk = try Chunk.init(vm.allocator),
            .name = null,
        };
        vm.objects = &out.obj;
        return out;
    }

    pub fn eql(self: *Self, other: *Self) bool {
        return self == other;
    }

    pub fn format(
        self: Self,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        if (self.name) |obj| {
            try writer.print("<fn {s}>", .{obj.chars});
        } else {
            try writer.writeAll("<script>");
        }
    }
};

pub const NativeFn = *const fn (arg_count: usize, args: [*]const Value) Value;

pub const Native = struct {
    obj: Obj,
    function: NativeFn,

    const Self = @This();

    pub fn init(vm: *VM, function: NativeFn) !*Self {
        const out = try vm.allocator.create(Self);
        out.* = .{
            .obj = .{ .type = .Native, .next = vm.objects },
            .function = function,
        };
        vm.objects = &out.obj;
        return out;
    }

    pub fn deinit(self: *Self, vm: *VM) void {
        vm.allocator.destroy(self);
    }

    pub fn eql(self: *Self, other: *Self) bool {
        return self == other;
    }

    pub fn format(
        _: Self,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.writeAll("<native fn>");
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
        const duped = try vm.allocator.dupe(u8, chars);
        return allocate(vm, duped, hash);
    }

    pub fn deinit(self: *String, vm: *VM) void {
        vm.allocator.free(self.chars);
        vm.allocator.destroy(self);
    }

    fn allocate(vm: *VM, chars: []const u8, hash: u32) !*String {
        const out = try vm.allocator.create(String);
        out.* = .{
            .hash = hash,
            .chars = chars,
            .obj = .{ .type = .String, .next = vm.objects },
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
        return allocate(vm, buffer, hash);
    }

    pub fn eql(self: *String, other: *String) bool {
        return self.chars.ptr == other.chars.ptr;
    }

    pub fn format(
        self: String,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("{s}", .{self.chars});
    }
};

test "string interning" {
    var vm = VM.init(testing.allocator);
    defer vm.deinit();
    const original_str = "hello world";
    const a_str = try String.init(&vm, original_str);
    const b_str = try String.init(&vm, "hello world");
    try testing.expect(a_str == b_str);
}

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
