const std = @import("std");
const Allocator = std.mem.Allocator;
const testing = std.testing;

const LOG_GC = @import("debug").@"log-gc";

const Chunk = @import("chunk.zig").Chunk;
const Table = @import("table.zig").Table;
const Value = @import("value.zig").Value;
const VM = @import("vm.zig").VM;

const Obj = @This();
type: Type,
next: ?*Obj = null,
is_marked: bool = false,

pub fn deinit(obj: *Obj, vm: *VM) void {
    switch (obj.type) {
        inline else => |t| {
            const obj_variant = obj.as(t);
            if (LOG_GC) {
                std.debug.print("{*} free type {s}\n", .{ obj, @typeName(t.VariantType()) });
            }
            obj_variant.deinit(vm);
        },
    }
}

/// Meant to be used together with a switch on obj.type
pub fn as(obj: *Obj, comptime obj_t: Type) *obj_t.VariantType() {
    std.debug.assert(obj.type == obj_t);
    return @alignCast(@fieldParentPtr("obj", obj));
}

pub const Type = enum {
    BoundMethod,
    Class,
    Closure,
    Function,
    Instance,
    Native,
    String,
    Upvalue,

    pub fn VariantType(self: Type) type {
        return switch (self) {
            .BoundMethod => BoundMethod,
            .Class => Class,
            .Closure => Closure,
            .Function => Function,
            .Instance => Instance,
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
    Initializer,
    Method,
    Script,
};

pub const Function = struct {
    obj: Obj,
    arity: u8,
    upvalue_count: u16,
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
        out.* = .{
            .obj = out.obj,
            .hash = hash,
            .chars = chars,
        };
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
    defer vm.deinit();
    const original_str: []const u8 = "Hello ";
    const a_str = try String.init(vm, original_str);
    const b_str = try String.init(vm, "world!");
    const out = try a_str.concat(vm, b_str);
    // Expect new string has all the right chars
    try testing.expectEqualSlices(u8, "Hello world!", out.chars);
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
        out.* = .{
            .obj = out.obj,
            .location = slot,
            .closed = Value.Nil,
        };
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
    upvalues: []?*Upvalue,

    const Self = @This();

    pub fn init(vm: *VM, function: *Function) !*Self {
        // important to init upvalues before allocate obj otherwise gc will clean obj up
        const upvalues = try vm.allocator.alloc(?*Upvalue, function.upvalue_count);
        for (upvalues) |*upvalue| upvalue.* = null;
        const out = try allocateObj(vm, .Closure);
        out.* = .{
            .obj = out.obj,
            .function = function,
            .upvalues = upvalues,
        };
        return out;
    }

    pub fn deinit(self: *Self, vm: *VM) void {
        vm.allocator.free(self.upvalues);
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
    methods: Table,

    const Self = @This();

    pub fn init(vm: *VM, name: *String) !*Self {
        const out = try allocateObj(vm, .Class);
        out.* = .{
            .obj = out.obj,
            .name = name,
            .methods = .init(vm.allocator),
        };
        return out;
    }

    pub fn deinit(self: *Self, vm: *VM) void {
        // .name will be collected by gc
        self.methods.deinit();
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

pub const Instance = struct {
    obj: Obj,
    class: *Class,
    fields: Table,

    const Self = @This();

    pub fn init(vm: *VM, class: *Class) !*Self {
        const out = try allocateObj(vm, .Instance);
        out.* = .{
            .obj = out.obj,
            .class = class,
            .fields = .init(vm.allocator),
        };
        return out;
    }

    pub fn deinit(self: *Self, vm: *VM) void {
        // .class will be collected by gc
        self.fields.deinit();
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
        try writer.print("{s} instance", .{self.class.name.chars});
    }
};

pub const BoundMethod = struct {
    obj: Obj,
    receiver: Value,
    method: *Closure,

    const Self = @This();

    pub fn init(vm: *VM, receiver: Value, method: *Closure) !*Self {
        const out = try allocateObj(vm, .BoundMethod);
        out.* = .{
            .obj = out.obj,
            .receiver = receiver,
            .method = method,
        };
        return out;
    }

    pub fn deinit(self: *Self, vm: *VM) void {
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
        try writer.print("{}", .{self.method.function});
    }
};
