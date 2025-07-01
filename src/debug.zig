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

pub fn disassembleInstruction(c: *Chunk, offset: usize) usize {
    print("{d:0>4} ", .{offset});

    const lines = c.lines.items;
    if (offset == 0 or lines[offset] != lines[offset - 1]) {
        print("{d:>4} ", .{lines[offset]});
    } else {
        print("   | ", .{});
    }

    const oc: OpCode = @enumFromInt(c.byteAt(offset));
    return switch (oc) {
        .CONSTANT,
        .DEFINE_GLOBAL,
        .GET_GLOBAL,
        .SET_GLOBAL,
        .CLASS,
        .GET_PROPERTY,
        .SET_PROPERTY,
        .METHOD,
        .GET_SUPER,
        => constantInstruction(oc, c, offset),
        .SET_LOCAL, .GET_LOCAL, .CALL, .GET_UPVALUE, .SET_UPVALUE => byteInstruction(oc, c, offset),
        .JUMP, .JUMP_IF_TRUE, .JUMP_IF_FALSE => jumpInstruction(oc, 1, c, offset),
        .LOOP => jumpInstruction(oc, -1, c, offset),
        .CLOSURE => blk: {
            var idx = offset + 1;
            const const_idx = c.byteAt(idx);
            const const_val = c.constAt(const_idx);
            idx += 1;
            print("{s:-<16} {d:4} {}\n", .{ @tagName(oc), const_idx, const_val });
            const fun = const_val.asObj(.Function).?;
            var j: u8 = 0;
            while (j < fun.upvalue_count) : (j += 1) {
                const is_local = c.byteAt(idx + 1) == 1;
                const index = c.byteAt(idx + 2);
                print("{d:4}      |                     {s} {d}\n", .{
                    idx,
                    if (is_local) "local" else "upvalue",
                    index,
                });
                idx += 2;
            }
            break :blk idx;
        },
        .INVOKE, .SUPER_INVOKE => invokeInstruction(oc, c, offset),
        else => simpleInstruction(oc, offset),
    };
}

fn invokeInstruction(oc: OpCode, c: *Chunk, offset: usize) usize {
    const const_idx = c.byteAt(offset + 1);
    const arg_count = c.byteAt(offset + 2);
    const const_val = c.constAt(const_idx);
    print("{s:-<16} ({d} args) {d:4} '{}'\n", .{ @tagName(oc), arg_count, const_idx, const_val });
    return offset + 3;
}

fn constantInstruction(oc: OpCode, c: *Chunk, offset: usize) usize {
    const const_idx = c.byteAt(offset + 1);
    const const_val = c.constAt(const_idx);
    print("{s:-<16} {d:4} '{}'\n", .{ @tagName(oc), const_idx, const_val });
    return offset + 2;
}

fn byteInstruction(oc: OpCode, c: *Chunk, offset: usize) usize {
    const const_idx = c.byteAt(offset + 1);
    print("{s:-<16} {d:4}\n", .{ @tagName(oc), const_idx });
    return offset + 2;
}

fn jumpInstruction(oc: OpCode, sign: i32, c: *Chunk, offset: usize) usize {
    const jump = @as(u16, c.byteAt(offset + 1)) << 8 | @as(u16, c.byteAt(offset + 2));
    const location = @as(i32, @intCast(offset)) + 3 + @as(i32, jump) * sign;
    print("{s:-<16} {d:4} -> {d}\n", .{ @tagName(oc), offset, location });
    return offset + 3;
}

fn simpleInstruction(oc: OpCode, offset: usize) usize {
    print("{s}\n", .{@tagName(oc)});
    return offset + 1;
}
