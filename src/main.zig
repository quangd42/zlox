const std = @import("std");

const ch = @import("chunk.zig");
const Chunk = ch.Chunk;
const VM = @import("vm.zig").VM;
const debug = @import("debug.zig");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();
    var chunk = Chunk.init(allocator);
    defer chunk.deinit();
    var my_vm = VM.init(allocator);
    defer my_vm.deinit();

    try chunk.writeConstant(1.2, 13);
    try chunk.writeConstant(3.4, 13);

    try chunk.writeOpCode(.OP_ADD, 13);

    try chunk.writeConstant(5.6, 13);
    try chunk.writeOpCode(.OP_DIVIDE, 13);
    try chunk.writeOpCode(.OP_NEGATE, 13);

    try chunk.writeOpCode(.OP_RETURN, 13);
    // debug.disassembleChunk(&chunk, "test chunk");
    _ = try my_vm.interpret(&chunk);
}
