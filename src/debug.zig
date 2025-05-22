const std = @import("std");
const print = std.debug.print;

const _chunk = @import("chunk.zig");
const Chunk = _chunk.Chunk;
const OpCode = _chunk.OpCode;

pub fn disassembleChunk(chunk: *Chunk, name: []const u8) void {
    print("== {s} ==\n", .{name});

    var offset: usize = 0;
    while (offset < chunk.code.items.len) {
        offset = disassembleInstruction(chunk, offset);
    }
}

pub fn disassembleInstruction(chunk: *Chunk, offset: usize) usize {
    print("{d:0>4} ", .{offset});

    const lines = chunk.lines.items;
    if (offset == 0 or lines[offset] != lines[offset - 1]) {
        print("{d:>4} ", .{lines[offset]});
    } else {
        print("   | ", .{});
    }

    const oc: OpCode = @enumFromInt(chunk.getByteAt(offset) catch unreachable);
    return switch (oc) {
        .CONSTANT, .DEFINE_GLOBAL, .GET_GLOBAL, .SET_GLOBAL => constantInstruction(oc, chunk, offset),
        .SET_LOCAL, .GET_LOCAL => byteInstruction(oc, chunk, offset),
        else => simpleInstruction(oc, offset),
    };
}

fn constantInstruction(oc: OpCode, chunk: *Chunk, offset: usize) usize {
    const const_idx = chunk.code.items[offset + 1];
    print("{s:-<16} {d:4} '{}'\n", .{ @tagName(oc), const_idx, chunk.constants.items[const_idx] });
    return offset + 2;
}

fn byteInstruction(oc: OpCode, chunk: *Chunk, offset: usize) usize {
    const const_idx = chunk.code.items[offset + 1];
    print("{s:-<16} {d:4}\n", .{ @tagName(oc), const_idx });
    return offset + 2;
}

fn simpleInstruction(oc: OpCode, offset: usize) usize {
    print("{s}\n", .{@tagName(oc)});
    return offset + 1;
}
