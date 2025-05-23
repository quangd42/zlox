const std = @import("std");
const Allocator = std.mem.Allocator;
const testing = std.testing;

const Value = @import("value.zig").Value;

pub const OpCode = enum(u8) {
    CONSTANT,
    // TODO: add CONSTANT_LONG
    NIL,
    TRUE,
    FALSE,
    POP,
    GET_LOCAL,
    SET_LOCAL,
    GET_GLOBAL,
    DEFINE_GLOBAL,
    SET_GLOBAL,
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
    PRINT,
    JUMP,
    JUMP_IF_TRUE,
    JUMP_IF_FALSE,
    RETURN,
};

pub const Chunk = struct {
    code: std.ArrayList(u8),
    constants: std.ArrayList(Value),
    lines: std.ArrayList(usize),
    allocator: Allocator,

    pub fn init(allocator: Allocator) !Chunk {
        const max = std.math.maxInt(u8);
        return Chunk{
            // Set max cap 256 and use appendAssumeCapacity to make sure
            // no more memory is allocated
            .constants = try std.ArrayList(Value).initCapacity(allocator, max),
            .code = std.ArrayList(u8).init(allocator),
            .lines = std.ArrayList(usize).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Chunk) void {
        self.code.deinit();
        self.constants.deinit();
        self.lines.deinit();
    }

    pub fn writeByte(self: *Chunk, byte: u8, line: usize) !void {
        try self.code.append(byte);
        try self.lines.append(line);
    }

    pub fn writeOpCode(self: *Chunk, oc: OpCode, line: usize) !void {
        try self.writeByte(@intFromEnum(oc), line);
    }

    pub fn addConstant(self: *Chunk, val: Value) u8 {
        self.constants.appendAssumeCapacity(val);
        return @intCast(self.constants.items.len - 1);
    }

    pub fn writeConstant(self: *Chunk, constant_idx: u8, line: usize) !void {
        try self.writeOpCode(.CONSTANT, line);
        try self.writeByte(constant_idx, line);
    }

    pub fn getByteAt(self: *Chunk, offset: usize) !u8 {
        if (offset > self.code.items.len) return error.OutOfBounds;
        return self.code.items[offset];
    }

    pub fn getConstantAt(self: *Chunk, offset: u8) !Value {
        if (offset > self.constants.items.len) return error.OutOfBounds;
        return self.constants.items[offset];
    }
};

test "Chunk write byte and line tracking" {
    var c = try Chunk.init(testing.allocator);
    defer c.deinit();

    try c.writeByte(42, 123);

    try testing.expectEqual(1, c.code.items.len);
    try testing.expectEqual(42, c.code.items[0]);
    try testing.expectEqual(1, c.lines.items.len);
    try testing.expectEqual(123, c.lines.items[0]);
}

test "Chunk write OpCode" {
    var c = try Chunk.init(testing.allocator);
    defer c.deinit();

    try c.writeOpCode(.RETURN, 456);

    try testing.expectEqual(1, c.code.items.len);
    try testing.expectEqual(@intFromEnum(OpCode.RETURN), c.code.items[0]);
    try testing.expectEqual(456, c.lines.items[0]);
}

test "Chunk add constant" {
    var c = try Chunk.init(testing.allocator);
    defer c.deinit();

    const val: Value = .{ .Number = 3.14 };
    const idx = c.addConstant(val);

    try testing.expectEqual(0, idx);
    try testing.expectEqual(1, c.constants.items.len);
    try testing.expectEqual(3.14, c.constants.items[0].Number);
}

test "Chunk write constant - small index" {
    var c = try Chunk.init(testing.allocator);
    defer c.deinit();

    const val: Value = .{ .Number = 2.71 };
    const idx = c.addConstant(val);
    try c.writeConstant(idx, 789);

    // Should use OP_CONSTANT for small indexes
    try testing.expectEqual(2, c.code.items.len);
    try testing.expectEqual(@intFromEnum(OpCode.CONSTANT), c.code.items[0]);
    try testing.expectEqual(0, c.code.items[1]);
    try testing.expectEqual(val, c.constants.items[0]);
}

test "Chunk get byte at valid offset" {
    var c = try Chunk.init(testing.allocator);
    defer c.deinit();

    try c.writeByte(55, 999);
    const byte = c.getByteAt(0);

    try testing.expectEqual(55, byte);
}

test "Chunk multiple operations sequence" {
    var c = try Chunk.init(testing.allocator);
    defer c.deinit();

    // Write a sequence of operations like you might in real code
    try c.writeOpCode(.RETURN, 1);
    const val: Value = .{ .Number = 42.0 };
    try c.writeConstant(c.addConstant(val), 2);

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
