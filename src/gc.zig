const std = @import("std");
const print = std.debug.print;
const mem = std.mem;
const Allocator = mem.Allocator;

const LOG_GC = @import("debug").@"log-gc";
const STRESS_GC = @import("debug").@"stress-gc";

const Obj = @import("obj.zig");
const Table = @import("table.zig");
const Value = @import("value.zig").Value;
const VM = @import("vm.zig");

const HEAP_GROW_FACTOR = 2;

pub const GC = @This();

backing_allocator: Allocator,
out_of_memory: bool = false,
bytes_allocated: usize = 0,
next_gc: usize = 1024 * 1024,
vm: *VM,

pub fn init(backing_allocator: Allocator, vm: *VM) GC {
    return .{ .backing_allocator = backing_allocator, .vm = vm };
}

pub fn allocator(self: *GC) Allocator {
    return .{ .ptr = self, .vtable = &.{ .alloc = alloc, .resize = resize, .remap = remap, .free = free } };
}

fn alloc(ctx: *anyopaque, len: usize, alignment: mem.Alignment, ret_addr: usize) ?[*]u8 {
    const self: *GC = @ptrCast(@alignCast(ctx));
    if (STRESS_GC) self.collectGarbage();
    self.updateAllocation(len, 0);
    if (self.bytes_allocated > self.next_gc) self.collectGarbage();
    return self.backing_allocator.rawAlloc(len, alignment, ret_addr);
}

fn resize(ctx: *anyopaque, buf: []u8, alignment: mem.Alignment, new_len: usize, ret_addr: usize) bool {
    const self: *GC = @ptrCast(@alignCast(ctx));
    if (new_len > buf.len and STRESS_GC) self.collectGarbage();
    self.updateAllocation(new_len, buf.len);
    if (self.bytes_allocated > self.next_gc) self.collectGarbage();
    return self.backing_allocator.rawResize(buf, alignment, new_len, ret_addr);
}

fn remap(
    ctx: *anyopaque,
    buf: []u8,
    alignment: mem.Alignment,
    new_len: usize,
    ret_addr: usize,
) ?[*]u8 {
    const self: *GC = @ptrCast(@alignCast(ctx));
    if (new_len > buf.len and STRESS_GC) self.collectGarbage();
    self.updateAllocation(new_len, buf.len);
    if (self.bytes_allocated > self.next_gc) self.collectGarbage();
    return self.backing_allocator.rawRemap(buf, alignment, new_len, ret_addr);
}

fn free(ctx: *anyopaque, buf: []u8, alignment: mem.Alignment, ret_addr: usize) void {
    const self: *GC = @ptrCast(@alignCast(ctx));
    self.updateAllocation(0, buf.len);
    if (self.bytes_allocated > self.next_gc) self.collectGarbage();
    return self.backing_allocator.rawFree(buf, alignment, ret_addr);
}

fn collectGarbage(self: *GC) void {
    if (self.vm.compiler == null) return; // make sure vm is fully initialized before any collection
    var before: usize = 0;
    if (LOG_GC) {
        print("-- gc begin\n", .{});
        before = self.bytes_allocated;
    }
    defer if (LOG_GC) {
        print("-- gc end\n", .{});
        print("   collected {d} bytes (from {d} to {d}) next at {d}\n", .{
            before - self.bytes_allocated,
            before,
            self.bytes_allocated,
            self.next_gc,
        });
    };

    self.markRoots() catch {
        self.out_of_memory = true;
    };
    self.traceReferences() catch {
        self.out_of_memory = true;
    };
    tableRemoveWhite(&self.vm.strings);
    self.sweep();

    self.next_gc = self.bytes_allocated * HEAP_GROW_FACTOR;
}

fn markRoots(self: *GC) !void {
    // stack
    var cursor: [*]Value = &self.vm.stack.data;
    while (self.vm.stack.top - cursor > 0) : (cursor += 1) {
        try self.markValue(@ptrCast(cursor));
    }

    // globals
    try self.markTable(&self.vm.globals);

    // call frames
    for (self.vm.frames.items) |*frame| try self.markObj(&frame.closure.obj);

    // upvalues
    var cur_upvalue = self.vm.open_upvalues;
    while (cur_upvalue) |upvalue| : (cur_upvalue = upvalue.next) {
        try self.markObj(&upvalue.obj);
    }

    // compilers
    try self.markCompilerRoots();

    // init_string
    try self.markObj(&self.vm.init_string.obj);
}

fn traceReferences(self: *GC) !void {
    while (self.vm.gray_stack.pop()) |obj| {
        try self.blackenObj(obj);
    }
}

fn sweep(self: *GC) void {
    var prev: ?*Obj = null;
    var cur: ?*Obj = self.vm.objects;
    while (cur) |obj| {
        // if black, make it white for next gc run
        if (obj.is_marked) {
            obj.is_marked = false;
            prev = obj;
            cur = obj.next;
            continue;
        }
        // if white (unreachable), take it out of the chain and free it
        cur = obj.next;
        if (prev != null) {
            prev.?.next = cur;
        } else {
            self.vm.objects = cur;
        }
        obj.deinit(self.vm);
    }
}

fn markObj(self: *GC, obj: *Obj) !void {
    if (obj.is_marked) return;
    if (LOG_GC) print("{*} mark {}\n", .{ obj, Value.from(obj) });
    obj.is_marked = true;
    try self.vm.gray_stack.append(obj);
}

fn markValue(self: *GC, value: *Value) !void {
    const obj = value.as(.Obj) orelse return;
    try self.markObj(obj);
}

fn blackenObj(self: *GC, obj: *Obj) !void {
    if (LOG_GC) print("{*} blacken {}\n", .{ obj, Value.from(obj) });
    switch (obj.type) {
        .Native, .String => {},
        .Upvalue => try self.markValue(&obj.as(.Upvalue).closed),
        .Function => {
            const function = obj.as(.Function);
            if (function.name) |str| try self.markObj(&str.obj);
            for (function.chunk.constants.items) |*value| {
                try self.markValue(value);
            }
        },
        .Closure => {
            const closure = obj.as(.Closure);
            try self.markObj(&closure.function.obj);
            for (closure.upvalues) |mb_upvalue| {
                const upvalue = mb_upvalue orelse continue;
                try self.markObj(&upvalue.obj);
            }
        },
        .Class => {
            const class = obj.as(.Class);
            try self.markObj(&class.name.obj);
            try self.markTable(&class.methods);
        },
        .Instance => {
            const instance = obj.as(.Instance);
            try self.markObj(&instance.class.obj);
            try self.markTable(&instance.fields);
        },
        .BoundMethod => {
            const bound = obj.as(.BoundMethod);
            try self.markValue(&bound.receiver);
            try self.markObj(&bound.method.obj);
        },
    }
}

fn markTable(self: *GC, table: *Table) !void {
    for (table.entries) |*entry| {
        const e = &(entry.* orelse continue);
        if (e.key) |k| try self.markObj(&k.obj);
        try self.markValue(&e.value);
    }
}

fn tableRemoveWhite(table: *Table) void {
    for (table.entries) |entry| {
        const e = entry orelse continue;
        const k = e.key orelse continue;
        if (!k.obj.is_marked) _ = table.delete(k);
    }
}

fn markCompilerRoots(self: *GC) !void {
    const compiler = self.vm.compiler orelse return;
    var cur_func = compiler.current_func;
    while (cur_func) |cur| : (cur_func = cur.enclosing) {
        try self.markObj(&cur.function.obj);
    }
}

fn updateAllocation(self: *GC, new_size: usize, old_size: usize) void {
    if (new_size > old_size) {
        self.bytes_allocated += (new_size - old_size);
    } else if (old_size > new_size) {
        if (self.bytes_allocated < (old_size - new_size)) {
            self.bytes_allocated = 0;
            return;
        }
        self.bytes_allocated -= (old_size - new_size);
    }
}
