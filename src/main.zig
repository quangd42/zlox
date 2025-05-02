const std = @import("std");

const ch = @import("chunk.zig");
const Chunk = ch.Chunk;

const debug = @import("debug.zig");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();
    var chunk = Chunk.init(allocator);
    defer chunk.deinit();

    const constant: ch.Byte = try chunk.addConstant(1.345);
    try chunk.writeOpCode(.OP_CONSTANT, 13);
    try chunk.writeByte(constant, 13);

    try chunk.writeOpCode(.OP_RETURN, 13);
    debug.disassembleChunk(&chunk, "test chunk");
}
