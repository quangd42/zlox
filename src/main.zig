const std = @import("std");
const Allocator = std.mem.Allocator;

const ch = @import("chunk.zig");
const Chunk = ch.Chunk;
const debug = @import("debug.zig");
const vm = @import("vm.zig");
const VM = vm.VM;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

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
