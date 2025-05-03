const std = @import("std");
const value = @import("value.zig");
const Allocator = std.mem.Allocator;

pub const Byte = u8;

pub const OpCode = enum(Byte) {
    OP_CONSTANT,
    OP_CONSTANT_LONG,
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

    pub fn addConstant(self: *Chunk, val: value.Value) !u24 {
        // Constant index has to fit into a u24
        if (self.constants.items.len > std.math.maxInt(u24)) {
            return error.TooManyConstants;
        }
        try self.constants.append(val);
        return @intCast(self.constants.items.len - 1);
    }

    pub fn writeConstant(self: *Chunk, val: value.Value, line: usize) !void {
        const constant_idx: u24 = try self.addConstant(val);
        const max_byte_val = std.math.maxInt(Byte);

        if (constant_idx <= max_byte_val) {
            try self.writeOpCode(.OP_CONSTANT, line);
            try self.writeByte(@intCast(constant_idx), line);
            return;
        }

        try self.writeOpCode(.OP_CONSTANT_LONG, line);
        // Write the index as three bytes (little-endian)
        try self.writeByte(@intCast(constant_idx & 0xFF), line);
        try self.writeByte(@intCast((constant_idx >> 8) & 0xFF), line);
        try self.writeByte(@intCast((constant_idx >> 16) & 0xFF), line);
    }

    pub fn getByteAt(self: *Chunk, offset: usize) !Byte {
        if (offset >= self.code.items.len) {
            return error.OutOfBounds;
        }
        return self.code.items[offset];
    }
};
