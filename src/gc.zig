const std = @import("std");
const print = std.debug.print;
const mem = std.mem;
const Allocator = mem.Allocator;
const VM = @import("vm.zig").VM;

const LOG_GC = @import("debug").@"log-gc";
const STRESS_GC = @import("debug").@"stress-gc";

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
        if (STRESS_GC) collectGarbage();
        return self.backing_allocator.rawAlloc(len, alignment, ret_addr);
    }

    fn resize(ctx: *anyopaque, buf: []u8, alignment: mem.Alignment, new_len: usize, ret_addr: usize) bool {
        const self: *GC = @ptrCast(@alignCast(ctx));
        if (new_len > buf.len and STRESS_GC) collectGarbage();
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
        if (new_len > buf.len and STRESS_GC) collectGarbage();
        return self.backing_allocator.rawRemap(buf, alignment, new_len, ret_addr);
    }

    fn free(ctx: *anyopaque, buf: []u8, alignment: mem.Alignment, ret_addr: usize) void {
        const self: *GC = @ptrCast(@alignCast(ctx));
        return self.backing_allocator.rawFree(buf, alignment, ret_addr);
    }

    fn collectGarbage() void {
        if (LOG_GC) print("-- gc begin\n", .{});
        defer if (LOG_GC) print("-- gc end\n", .{});

        print("-- gc run\n", .{});
    }
};
