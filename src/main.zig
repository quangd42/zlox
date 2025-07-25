const std = @import("std");
const Allocator = std.mem.Allocator;

const Chunk = @import("chunk.zig");
const debug = @import("debug.zig");
const VM = @import("vm.zig");

const DEBUGGING = @import("builtin").mode == .Debug;

pub fn main() !void {
    var alloc_type = if (DEBUGGING) std.heap.DebugAllocator(.{}).init else std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = alloc_type.allocator();
    defer {
        const deinit_status = alloc_type.deinit();
        if (DEBUGGING and deinit_status == .leak) std.testing.expect(false) catch @panic("TEST FAILED");
    }

    var my_vm = try VM.init(allocator);
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

test "all project tests" {
    @import("std").testing.refAllDeclsRecursive(@This());
}
