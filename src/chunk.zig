const std = @import("std");
const value = @import("value.zig");
const Allocator = std.mem.Allocator;

pub const Byte = u8;

pub const OpCode = enum(Byte) {
    OP_CONSTANT,
    OP_RETURN,
};

pub const Chunk = struct {
    code: std.ArrayList(Byte),
    constants: value.ValueArray,
    lines: std.ArrayList(usize),
    allocator: Allocator,

    pub fn init(allocator: Allocator) Chunk {
        return Chunk{
            .code = std.ArrayList(Byte).init(allocator),
            .constants = value.ValueArray.init(allocator),
            .lines = std.ArrayList(usize).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Chunk) void {
        self.code.deinit();
        self.constants.deinit();
        self.lines.deinit();
    }

    pub fn writeByte(self: *Chunk, byte: Byte, line: usize) !void {
        try self.code.append(byte);
        try self.lines.append(line);
    }

    pub fn writeOpCode(self: *Chunk, oc: OpCode, line: usize) !void {
        try self.writeByte(@intFromEnum(oc), line);
    }

    pub fn addConstant(self: *Chunk, val: value.Value) !Byte {
        // Constant index has to fit into a u8
        if (self.constants.items.len >= 255) {
            return error.TooManyConstants;
        }
        try self.constants.append(val);
        return @intCast(self.constants.items.len - 1);
    }

    pub fn getByteAt(self: *Chunk, offset: usize) !Byte {
        if (offset >= self.code.items.len) {
            return error.OutOfBounds;
        }
        return self.code.items[offset];
    }
};
