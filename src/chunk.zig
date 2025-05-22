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
            .code = try std.ArrayList(u8).initCapacity(allocator, max),
            .constants = try std.ArrayList(Value).initCapacity(allocator, max),
            .lines = try std.ArrayList(usize).initCapacity(allocator, max),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Chunk) void {
        self.code.deinit();
        self.constants.deinit();
        self.lines.deinit();
    }

    pub fn writeByte(self: *Chunk, byte: u8, line: usize) void {
        self.code.appendAssumeCapacity(byte);
        self.lines.appendAssumeCapacity(line);
    }

    pub fn writeOpCode(self: *Chunk, oc: OpCode, line: usize) void {
        self.writeByte(@intFromEnum(oc), line);
    }

    pub fn addConstant(self: *Chunk, val: Value) u8 {
        self.constants.appendAssumeCapacity(val);
        return @intCast(self.constants.items.len - 1);
    }

    pub fn writeConstant(self: *Chunk, constant_idx: u8, line: usize) void {
        self.writeOpCode(.CONSTANT, line);
        self.writeByte(constant_idx, line);

        // try self.writeOpCode(.CONSTANT_LONG, line);
        // // Write the index as three bytes (little-endian)
        // try self.writeByte(@intCast(constant_idx & 0xFF), line);
        // try self.writeByte(@intCast((constant_idx >> 8) & 0xFF), line);
        // try self.writeByte(@intCast((constant_idx >> 16) & 0xFF), line);
    }

    pub fn getByteAt(self: *Chunk, offset: usize) u8 {
        return self.code.items[offset];
    }

    pub fn getConstantAt(self: *Chunk, offset: usize) Value {
        return self.constants.items[offset];
    }
};

test "Chunk write byte and line tracking" {
    var c = try Chunk.init(testing.allocator);
    defer c.deinit();

    c.writeByte(42, 123);

    try testing.expectEqual(1, c.code.items.len);
    try testing.expectEqual(42, c.code.items[0]);
    try testing.expectEqual(1, c.lines.items.len);
    try testing.expectEqual(123, c.lines.items[0]);
}

test "Chunk write OpCode" {
    var c = try Chunk.init(testing.allocator);
    defer c.deinit();

    c.writeOpCode(.RETURN, 456);

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
    c.writeConstant(idx, 789);

    // Should use OP_CONSTANT for small indexes
    try testing.expectEqual(2, c.code.items.len);
    try testing.expectEqual(@intFromEnum(OpCode.CONSTANT), c.code.items[0]);
    try testing.expectEqual(0, c.code.items[1]);
    try testing.expectEqual(val, c.constants.items[0]);
}

test "Chunk get byte at valid offset" {
    var c = try Chunk.init(testing.allocator);
    defer c.deinit();

    c.writeByte(55, 999);
    const byte = c.getByteAt(0);

    try testing.expectEqual(55, byte);
}

test "Chunk multiple operations sequence" {
    var c = try Chunk.init(testing.allocator);
    defer c.deinit();

    // Write a sequence of operations like you might in real code
    c.writeOpCode(.RETURN, 1);
    const val: Value = .{ .Number = 42.0 };
    c.writeConstant(c.addConstant(val), 2);

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
