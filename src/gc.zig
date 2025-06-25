const std = @import("std");
const print = std.debug.print;
const mem = std.mem;
const Allocator = mem.Allocator;

const LOG_GC = @import("debug").@"log-gc";
const STRESS_GC = @import("debug").@"stress-gc";

const Obj = @import("obj.zig").Obj;
const Table = @import("table.zig").Table;
const Value = @import("value.zig").Value;
const VM = @import("vm.zig").VM;

const HEAP_GROW_FACTOR = 2;

pub const GC = struct {
    backing_allocator: Allocator,
    vm: *VM,

    pub fn init(backing_allocator: Allocator, vm: *VM) GC {
        return .{ .backing_allocator = backing_allocator, .vm = vm };
    }

    pub fn allocator(self: *GC) Allocator {
        return .{ .ptr = self, .vtable = &.{
            .alloc = alloc,
            .resize = resize,
            .remap = remap,
            .free = free,
        } };
    }

    fn alloc(ctx: *anyopaque, len: usize, alignment: mem.Alignment, ret_addr: usize) ?[*]u8 {
        const self: *GC = @ptrCast(@alignCast(ctx));
        if (STRESS_GC) self.collectGarbage();
        self.updateAllocation(len, 0);
        if (self.vm.bytes_allocated > self.vm.next_gc) self.collectGarbage();
        return self.backing_allocator.rawAlloc(len, alignment, ret_addr);
    }

    fn resize(ctx: *anyopaque, buf: []u8, alignment: mem.Alignment, new_len: usize, ret_addr: usize) bool {
        const self: *GC = @ptrCast(@alignCast(ctx));
        if (new_len > buf.len and STRESS_GC) self.collectGarbage();
        self.updateAllocation(new_len, buf.len);
        if (self.vm.bytes_allocated > self.vm.next_gc) self.collectGarbage();
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
        if (self.vm.bytes_allocated > self.vm.next_gc) self.collectGarbage();
        return self.backing_allocator.rawRemap(buf, alignment, new_len, ret_addr);
    }

    fn free(ctx: *anyopaque, buf: []u8, alignment: mem.Alignment, ret_addr: usize) void {
        const self: *GC = @ptrCast(@alignCast(ctx));
        self.updateAllocation(0, buf.len);
        if (self.vm.bytes_allocated > self.vm.next_gc) self.collectGarbage();
        return self.backing_allocator.rawFree(buf, alignment, ret_addr);
    }

    fn collectGarbage(self: *GC) void {
        if (self.vm.compiler == null) return; // make sure vm is fully initialized before any collection
        var before: usize = 0;
        if (LOG_GC) {
            print("-- gc begin\n", .{});
            before = self.vm.bytes_allocated;
        }
        defer if (LOG_GC) {
            print("-- gc end\n", .{});
            print("   collected {d} bytes (from {d} to {d}) next at {d}\n", .{ before - self.vm.bytes_allocated, before, self.vm.bytes_allocated, self.vm.next_gc });
        };

        self.markRoots() catch @panic("failed to allocate memory for gc");
        self.traceReferences() catch @panic("failed to allocate memory for gc");
        tableRemoveWhite(&self.vm.strings);
        self.sweep();

        self.vm.next_gc = self.vm.bytes_allocated * HEAP_GROW_FACTOR;
    }

    fn markRoots(self: *GC) !void {
        // stack
        for (self.vm.stack.items) |*slot| try self.markValue(slot);

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
    }

    fn traceReferences(self: *GC) !void {
        while (self.vm.grayStack.pop()) |obj| {
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
        if (LOG_GC) print("{*} mark {}\n", .{ obj, Value{ .Obj = obj } });
        obj.is_marked = true;
        try self.vm.grayStack.append(obj);
    }

    fn markValue(self: *GC, value: *Value) !void {
        switch (value.*) {
            .Obj => |obj| try self.markObj(obj),
            else => {},
        }
    }

    fn blackenObj(self: *GC, obj: *Obj) !void {
        if (LOG_GC) print("{*} blacken {}\n", .{ obj, Value{ .Obj = obj } });
        switch (obj.type) {
            inline .Native, .String => {},
            inline .Upvalue => |t| try self.markValue(&obj.as(t).?.closed),
            inline .Function => |t| {
                const function = obj.as(t).?;
                if (function.name) |str| try self.markObj(&str.obj);
                for (function.chunk.constants.items) |*value| {
                    try self.markValue(value);
                }
            },
            inline .Closure => |t| {
                const closure = obj.as(t).?;
                try self.markObj(&closure.function.obj);
                for (closure.upvalues.items) |upvalue| {
                    try self.markObj(&upvalue.obj);
                }
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
        // the inner type "compiler" is what holds the function declaration
        var cur_compiler = compiler.compiler;
        while (cur_compiler) |cur| : (cur_compiler = cur.enclosing) {
            try self.markObj(&cur.function.obj);
        }
    }

    fn updateAllocation(self: *GC, new_size: usize, old_size: usize) void {
        if (new_size > old_size) {
            self.vm.bytes_allocated += (new_size - old_size);
        } else if (old_size > new_size) {
            if (self.vm.bytes_allocated < (old_size - new_size)) {
                self.vm.bytes_allocated = 0;
                return;
            }
            self.vm.bytes_allocated -= (old_size - new_size);
        }
    }
};
