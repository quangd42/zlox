const std = @import("std");
const print = std.debug.print;

const ch = @import("chunk.zig");
const Chunk = ch.Chunk;

pub fn disassembleChunk(chunk: *Chunk, name: []const u8) void {
    print("== {s} ==\n", .{name});

    var offset: usize = 0;
    while (offset < chunk.code.items.len) {
        offset = disassembleInstruction(chunk, offset);
    }
}

pub fn disassembleInstruction(chunk: *Chunk, offset: usize) usize {
    print("{d:0>4} ", .{offset});

    if (offset == 0 or chunk.lines.items[offset] != chunk.lines.items[offset - 1]) {
        print("{d:>4} ", .{chunk.lines.items[offset]});
    } else {
        print("   | ", .{});
    }

    const byte: u8 = chunk.getByteAt(offset) catch unreachable;
    const oc: ch.OpCode = @enumFromInt(byte);
    return switch (oc) {
        .OP_CONSTANT => constantInstruction(@tagName(.OP_CONSTANT), chunk, offset),
        .OP_CONSTANT_LONG => constantLongInstruction(@tagName(.OP_CONSTANT_LONG), chunk, offset),
        .OP_ADD => simpleInstruction(@tagName(.OP_ADD), offset),
        .OP_SUBTRACT => simpleInstruction(@tagName(.OP_SUBTRACT), offset),
        .OP_MULTIPLY => simpleInstruction(@tagName(.OP_MULTIPLY), offset),
        .OP_DIVIDE => simpleInstruction(@tagName(.OP_DIVIDE), offset),
        .OP_NEGATE => simpleInstruction(@tagName(.OP_NEGATE), offset),
        .OP_RETURN => simpleInstruction(@tagName(.OP_RETURN), offset),
    };
}

fn constantInstruction(name: []const u8, chunk: *Chunk, offset: usize) usize {
    const constant_idx = chunk.code.items[offset + 1];
    print("{s:-<16} {d:4} '{d}'\n", .{ name, constant_idx, chunk.constants.items[constant_idx] });
    return offset + 2;
}

fn constantLongInstruction(name: []const u8, chunk: *Chunk, offset: usize) usize {
    const byte1: u24 = @intCast(chunk.code.items[offset + 1]);
    const byte2: u24 = @intCast(chunk.code.items[offset + 2]);
    const byte3: u24 = @intCast(chunk.code.items[offset + 3]);
    const constant_idx: u24 = byte1 | byte2 << 8 | byte3 << 16;

    print("{s:-<16} {d:4} '{d}'\n", .{ name, constant_idx, chunk.constants.items[constant_idx] });
    return offset + 2;
}

fn simpleInstruction(name: []const u8, offset: usize) usize {
    print("{s}\n", .{name});
    return offset + 1;
}
