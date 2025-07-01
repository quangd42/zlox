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
        .CONSTANT, .DEFINE_GLOBAL, .GET_GLOBAL, .SET_GLOBAL, .CLASS, .GET_PROPERTY, .SET_PROPERTY, .METHOD => constantInstruction(oc, chunk, offset),
        .SET_LOCAL, .GET_LOCAL, .CALL, .GET_UPVALUE, .SET_UPVALUE => byteInstruction(oc, chunk, offset),
        .JUMP, .JUMP_IF_TRUE, .JUMP_IF_FALSE => jumpInstruction(oc, 1, chunk, offset),
        .LOOP => jumpInstruction(oc, -1, chunk, offset),
        .CLOSURE => blk: {
            var idx = offset + 1;
            const constant = chunk.code.items[idx];
            idx += 1;
            print("{s:-<16} {d:4} {}\n", .{ @tagName(oc), constant, chunk.constants.items[constant] });
            const fun = chunk.constants.items[constant].asObj(.Function).?;
            var j: u8 = 0;
            while (j < fun.upvalue_count) : (j += 1) {
                const is_local = chunk.code.items[idx + 1] == 1;
                const index = chunk.code.items[idx + 2];
                print("{d:4}      |                     {s} {d}\n", .{
                    idx,
                    if (is_local) "local" else "upvalue",
                    index,
                });
                idx += 2;
            }
            break :blk idx;
        },
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

fn jumpInstruction(oc: OpCode, sign: i32, chunk: *Chunk, offset: usize) usize {
    const jump = @as(u16, chunk.code.items[offset + 1]) << 8 | @as(u16, chunk.code.items[offset + 2]);
    const location = @as(i32, @intCast(offset)) + 3 + @as(i32, jump) * sign;
    print("{s:-<16} {d:4} -> {d}\n", .{ @tagName(oc), offset, location });
    return offset + 3;
}

fn simpleInstruction(oc: OpCode, offset: usize) usize {
    print("{s}\n", .{@tagName(oc)});
    return offset + 1;
}
