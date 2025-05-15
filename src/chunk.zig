const std = @import("std");
const Allocator = std.mem.Allocator;
const testing = std.testing;

const Value = @import("value.zig").Value;

pub const OpCode = enum(u8) {
    CONSTANT,
    CONSTANT_LONG,
    NIL,
    TRUE,
    FALSE,
    EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,
    ADD,
    SUBTRACT,
    MULTIPLY,
    DIVIDE,
    NOT,
    NEGATE,
    RETURN,
};

pub const Chunk = struct {
    code: std.ArrayList(u8),
    constants: std.ArrayList(Value),
    lines: std.ArrayList(usize),
    allocator: Allocator,

    pub fn init(allocator: Allocator) Chunk {
        return Chunk{
            .code = std.ArrayList(u8).init(allocator),
            .constants = std.ArrayList(Value).init(allocator),
            .lines = std.ArrayList(usize).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Chunk) void {
        self.code.deinit();
        self.constants.deinit();
        self.lines.deinit();
    }

    pub fn writeByte(self: *Chunk, byte: u8, line: usize) Allocator.Error!void {
        try self.code.append(byte);
        try self.lines.append(line);
    }

    pub fn writeOpCode(self: *Chunk, oc: OpCode, line: usize) Allocator.Error!void {
        try self.writeByte(@intFromEnum(oc), line);
    }

    pub fn addConstant(self: *Chunk, val: Value) !u24 {
        // Constant index has to fit into a u24
        if (self.constants.items.len > std.math.maxInt(u24)) {
            return error.TooManyConstants;
        }
        try self.constants.append(val);
        return @intCast(self.constants.items.len - 1);
    }

    pub fn writeConstant(self: *Chunk, val: Value, line: usize) !void {
        const constant_idx: u24 = try self.addConstant(val);
        const max_byte_val = std.math.maxInt(u8);

        if (constant_idx <= max_byte_val) {
            try self.writeOpCode(.CONSTANT, line);
            try self.writeByte(@intCast(constant_idx), line);
            return;
        }

        try self.writeOpCode(.CONSTANT_LONG, line);
        // Write the index as three bytes (little-endian)
        try self.writeByte(@intCast(constant_idx & 0xFF), line);
        try self.writeByte(@intCast((constant_idx >> 8) & 0xFF), line);
        try self.writeByte(@intCast((constant_idx >> 16) & 0xFF), line);
    }

    pub fn getByteAt(self: *Chunk, offset: usize) !u8 {
        if (offset >= self.code.items.len) {
            return error.OutOfBounds;
        }
        return self.code.items[offset];
    }

    pub fn getConstantAt(self: *Chunk, offset: usize) !Value {
        if (offset >= self.constants.items.len) {
            return error.OutOfBounds;
        }
        return self.constants.items[offset];
    }
};

test "Chunk write byte and line tracking" {
    var c = Chunk.init(testing.allocator);
    defer c.deinit();

    try c.writeByte(42, 123);

    try testing.expectEqual(1, c.code.items.len);
    try testing.expectEqual(42, c.code.items[0]);
    try testing.expectEqual(1, c.lines.items.len);
    try testing.expectEqual(123, c.lines.items[0]);
}

test "Chunk write OpCode" {
    var c = Chunk.init(testing.allocator);
    defer c.deinit();

    try c.writeOpCode(.RETURN, 456);

    try testing.expectEqual(1, c.code.items.len);
    try testing.expectEqual(@intFromEnum(OpCode.RETURN), c.code.items[0]);
    try testing.expectEqual(456, c.lines.items[0]);
}

test "Chunk add constant" {
    var c = Chunk.init(testing.allocator);
    defer c.deinit();

    const val: Value = .{ .Number = 3.14 };
    const idx = try c.addConstant(val);

    try testing.expectEqual(0, idx);
    try testing.expectEqual(1, c.constants.items.len);
    try testing.expectEqual(3.14, c.constants.items[0].Number);
}

test "Chunk write constant - small index" {
    var c = Chunk.init(testing.allocator);
    defer c.deinit();

    const val: Value = .{ .Number = 2.71 };
    try c.writeConstant(val, 789);

    // Should use OP_CONSTANT for small indexes
    try testing.expectEqual(2, c.code.items.len);
    try testing.expectEqual(@intFromEnum(OpCode.CONSTANT), c.code.items[0]);
    try testing.expectEqual(0, c.code.items[1]);
    try testing.expectEqual(val, c.constants.items[0]);
}

test "Chunk write constant - large index" {
    var c = Chunk.init(testing.allocator);
    defer c.deinit();

    // First, fill the constant pool to exceed what fits in a single byte
    var i: usize = 0;
    const max_byte_val = std.math.maxInt(u8);
    while (i <= max_byte_val) : (i += 1) {
        const val: Value = .{ .Number = @floatFromInt(i) };
        _ = try c.addConstant(val);
    }

    // Now add one more that will require the long format
    const val: Value = .{ .Number = 999.999 };
    try c.writeConstant(val, 101);

    // Should use OP_CONSTANT_LONG
    const op_index = c.code.items.len - 4;
    try testing.expectEqual(@intFromEnum(OpCode.CONSTANT_LONG), c.code.items[op_index]);

    // Check 3-byte encoding (little endian)
    const byte1 = c.code.items[op_index + 1];
    const byte2 = c.code.items[op_index + 2];
    const byte3 = c.code.items[op_index + 3];

    const idx: u24 = byte1 | (@as(u24, byte2) << 8) | (@as(u24, byte3) << 16);
    try testing.expectEqual(@as(u24, max_byte_val + 1), idx);
    try testing.expectEqual(val, c.constants.items[idx]);
}

test "Chunk get byte at valid offset" {
    var c = Chunk.init(testing.allocator);
    defer c.deinit();

    try c.writeByte(55, 999);
    const byte = try c.getByteAt(0);

    try testing.expectEqual(55, byte);
}

test "Chunk get byte at invalid offset" {
    var c = Chunk.init(testing.allocator);
    defer c.deinit();

    // Try to get a byte from an empty chunk
    const result = c.getByteAt(0);
    try testing.expectError(error.OutOfBounds, result);
}

test "Chunk multiple operations sequence" {
    var c = Chunk.init(testing.allocator);
    defer c.deinit();

    // Write a sequence of operations like you might in real code
    try c.writeOpCode(.RETURN, 1);
    const val: Value = .{ .Number = 42.0 };
    try c.writeConstant(val, 2);

    // Verify the byte sequence
    try testing.expectEqual(3, c.code.items.len);
    try testing.expectEqual(@intFromEnum(OpCode.RETURN), c.code.items[0]);
    try testing.expectEqual(@intFromEnum(OpCode.CONSTANT), c.code.items[1]);
    try testing.expectEqual(0, c.code.items[2]); // Constant index

    // Verify line numbers
    try testing.expectEqual(1, c.lines.items[0]);
    try testing.expectEqual(2, c.lines.items[1]);
    try testing.expectEqual(2, c.lines.items[2]);
}
