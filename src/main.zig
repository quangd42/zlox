const std = @import("std");
const Allocator = std.mem.Allocator;

const dbg = @import("builtin").mode == .Debug;

const Chunk = @import("chunk.zig").Chunk;
const debug = @import("debug.zig");
const vm = @import("vm.zig");
const VM = vm.VM;

pub fn main() !void {
    var alloc_type = if (dbg) std.heap.DebugAllocator(.{}).init else std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = alloc_type.allocator();
    defer {
        const deinit_status = alloc_type.deinit();
        if (dbg and deinit_status == .leak) std.testing.expect(false) catch @panic("TEST FAILED");
    }

    var my_vm = VM.init(allocator);
    defer my_vm.deinit();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len == 1) {
        try my_vm.repl();
    } else if (args.len == 2) {
        try my_vm.runFile(args[1]);
    } else {
        try std.io.getStdErr().writer().print("Usage: zlox <path_to_file>", .{});
        std.process.exit(64);
    }
}
