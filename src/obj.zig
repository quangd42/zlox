const std = @import("std");
const Allocator = std.mem.Allocator;
const testing = std.testing;

const LOG_GC = @import("debug").@"log-gc";

const Chunk = @import("chunk.zig").Chunk;
const Value = @import("value.zig").Value;
const VM = @import("vm.zig").VM;

pub const Obj = struct {
    type: Type,
    next: ?*Obj = null,
    is_marked: bool = false,

    pub fn deinit(self: *Obj, vm: *VM) void {
        switch (self.type) {
            inline else => |t| {
                const VT = t.VariantType();
                const obj: *VT = @alignCast(@fieldParentPtr("obj", self));
                if (LOG_GC) {
                    std.debug.print("{*} free type {s}\n", .{ self, @typeName(VT) });
                }
                obj.deinit(vm);
            },
        }
    }

    pub inline fn as(self: *Obj, obj_t: Type) ?*obj_t.VariantType() {
        switch (self.type) {
            inline else => |t| {
                if (obj_t != t) return null;
                const VT = t.VariantType();
                const obj: *VT = @alignCast(@fieldParentPtr("obj", self));
                return obj;
            },
        }
    }
};

pub const Type = enum {
    Class,
    Closure,
    Function,
    Native,
    String,
    Upvalue,

    pub fn VariantType(comptime self: @This()) type {
        return switch (self) {
            .Class => Class,
            .Closure => Closure,
            .Function => Function,
            .Native => Native,
            .String => String,
            .Upvalue => Upvalue,
        };
    }
};

fn allocateObj(vm: *VM, comptime obj_t: Type) !*obj_t.VariantType() {
    const VT = obj_t.VariantType();
    const out = try vm.allocator.create(VT);
    out.obj = .{ .type = obj_t, .next = vm.objects };
    vm.objects = &out.obj;
    if (LOG_GC) {
        std.debug.print("{*} allocate {} for {s}\n", .{ &out.obj, @sizeOf(VT), @typeName(VT) });
    }
    return out;
}

pub const FunctionType = enum {
    Function,
    Script,
};

pub const Function = struct {
    obj: Obj,
    arity: u8,
    upvalue_count: u8,
    chunk: Chunk,
    name: ?*String,

    const Self = @This();
    pub fn init(vm: *VM) !*Self {
        const out = try allocateObj(vm, .Function);
        try vm.push(.{ .Obj = &out.obj });
        defer _ = vm.pop();
        out.* = .{
            .obj = out.obj,
            .arity = 0,
            .upvalue_count = 0,
            .chunk = Chunk.init(vm.allocator),
            .name = null,
        };
        return out;
    }

    pub fn deinit(self: *Self, vm: *VM) void {
        self.chunk.deinit();
        vm.allocator.destroy(self);
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
        const out = try allocateObj(vm, .Native);
        out.function = function;
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
        return allocateString(vm, duped, hash);
    }

    pub fn deinit(self: *String, vm: *VM) void {
        vm.allocator.free(self.chars);
        vm.allocator.destroy(self);
    }

    fn allocateString(vm: *VM, chars: []const u8, hash: u32) !*String {
        const out = try allocateObj(vm, .String);
        try vm.push(.{ .Obj = &out.obj });
        defer _ = vm.pop();
        out.hash = hash;
        out.chars = chars;
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
        return allocateString(vm, buffer, hash);
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
    var vm = try VM.init(testing.allocator);
    defer vm.deinit();
    const original_str = "hello world";
    const a_str = try String.init(vm, original_str);
    const b_str = try String.init(vm, "hello world");
    try testing.expect(a_str == b_str);
}

test "concatenate strings" {
    var vm = try VM.init(testing.allocator);
    // explicitly set vm.objects = null because all strings are going
    // to be freed individually in this test
    defer vm.deinit();
    defer vm.objects = null;
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

pub const Upvalue = struct {
    obj: Obj,
    location: [*]Value,
    closed: Value,
    next: ?*Self = null,

    const Self = @This();

    pub fn init(vm: *VM, slot: [*]Value) !*Self {
        const out = try allocateObj(vm, .Upvalue);
        out.location = slot;
        out.closed = Value.Nil;
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
        try writer.writeAll("upvalue");
    }
};

pub const Closure = struct {
    obj: Obj,
    function: *Function,
    upvalues: std.ArrayList(*Upvalue),

    const Self = @This();

    pub fn init(vm: *VM, function: *Function) !*Self {
        const out = try allocateObj(vm, .Closure);
        out.function = function;
        out.upvalues = try std.ArrayList(*Upvalue).initCapacity(vm.allocator, function.upvalue_count);
        return out;
    }

    pub fn deinit(self: *Self, vm: *VM) void {
        self.upvalues.deinit();
        vm.allocator.destroy(self);
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
        try writer.print("{}", .{self.function});
    }
};

pub const Class = struct {
    obj: Obj,
    name: *String,

    const Self = @This();

    pub fn init(vm: *VM, name: *String) !*Self {
        const out = try allocateObj(vm, .Class);
        out.name = name;
        return out;
    }

    pub fn deinit(self: *Self, vm: *VM) void {
        // .name will be collected by gc
        vm.allocator.destroy(self);
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
        try writer.print("{s}", .{self.name.chars});
    }
};

test "init class" {
    var vm = try VM.init(std.testing.allocator);
    defer vm.deinit();

    const name = try String.init(vm, "klass");
    const klass = try Class.init(vm, name);

    const printed_name = try std.fmt.allocPrint(testing.allocator, "{}", .{klass});
    defer testing.allocator.free(printed_name);
    try testing.expectEqualStrings("klass", printed_name);
}
