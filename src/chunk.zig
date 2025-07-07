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
    GET_UPVALUE,
    SET_UPVALUE,
    GET_PROPERTY,
    SET_PROPERTY,
    GET_SUPER,
    EQUAL,
    NOT_EQUAL,
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
    LOOP,
    CALL,
    INVOKE,
    SUPER_INVOKE,
    CLOSURE,
    CLOSE_UPVALUE,
    RETURN,
    CLASS,
    INHERIT,
    METHOD,
};

const CONSTANT_MAX = std.math.maxInt(u8) + 1;

const LineEncoding = struct {
    start: usize,
    line: usize,

    fn order(context: usize, item: @This()) std.math.Order {
        if (context < item.start) {
            return .lt;
        } else if (context > item.start) {
            return .gt;
        } else {
            return .eq;
        }
    }
};

pub const Chunk = @This();
code: std.ArrayListUnmanaged(u8),
constants: std.ArrayListUnmanaged(Value),
lines: std.ArrayListUnmanaged(LineEncoding),
allocator: Allocator,

pub fn init(allocator: Allocator) Chunk {
    return Chunk{
        .constants = .empty,
        .code = .empty,
        .lines = .empty,
        .allocator = allocator,
    };
}

pub fn deinit(self: *Chunk) void {
    self.code.deinit(self.allocator);
    self.constants.deinit(self.allocator);
    self.lines.deinit(self.allocator);
}

pub fn writeByte(self: *Chunk, byte: u8, line: usize) !void {
    try self.code.append(self.allocator, byte);
    const current_line: LineEncoding = self.lines.getLastOrNull() orelse .{ .start = 0, .line = 0 };
    if (line != current_line.line) try self.lines.append(self.allocator, .{
        .start = self.code.items.len - 1,
        .line = line,
    });
}

pub fn writeOpCode(self: *Chunk, oc: OpCode, line: usize) !void {
    try self.writeByte(@intFromEnum(oc), line);
}

pub fn addConstant(self: *Chunk, val: Value) !u8 {
    // Check if the constant already exists in chunk.
    // Linear search is adequate for 255 items
    // Unsure why the book doesn't want this - presumably for performance
    for (self.constants.items, 0..) |constant, i| {
        if (constant.eql(val)) {
            return @intCast(i);
        }
    }

    if (self.constants.items.len >= CONSTANT_MAX) return error.ConstantTooLarge;
    try self.constants.append(self.allocator, val);
    return @intCast(self.constants.items.len - 1);
}

pub fn writeConst(self: *Chunk, constant_idx: u8, line: usize) !void {
    try self.writeOpCode(.CONSTANT, line);
    try self.writeByte(constant_idx, line);
}

pub fn byteAt(self: *Chunk, offset: usize) u8 {
    std.debug.assert(offset <= self.code.items.len);
    return self.code.items[offset];
}

pub fn constAt(self: *Chunk, offset: u8) Value {
    std.debug.assert(offset <= self.constants.items.len);
    return self.constants.items[offset];
}

pub fn lineOfByteAt(self: *Chunk, offset: usize) usize {
    const line_idx = std.sort.upperBound(LineEncoding, self.lines.items, offset, LineEncoding.order);
    return self.lines.items[line_idx - 1].line;
}

test "Chunk write byte and line tracking" {
    var c = Chunk.init(testing.allocator);
    defer c.deinit();

    try c.writeByte(42, 123);
    try c.writeByte(43, 123);
    try c.writeByte(44, 124);

    try testing.expectEqual(3, c.code.items.len);
    try testing.expectEqual(42, c.code.items[0]);
    try testing.expectEqual(43, c.code.items[1]);
    try testing.expectEqual(44, c.code.items[2]);
    try testing.expectEqual(2, c.lines.items.len);
    try testing.expectEqual(LineEncoding{ .start = 0, .line = 123 }, c.lines.items[0]);
    try testing.expectEqual(LineEncoding{ .start = 2, .line = 124 }, c.lines.items[1]);
    try testing.expectEqual(123, c.lineOfByteAt(0));
    try testing.expectEqual(123, c.lineOfByteAt(1));
    try testing.expectEqual(124, c.lineOfByteAt(2));
}

test "Chunk write OpCode" {
    var c = Chunk.init(testing.allocator);
    defer c.deinit();

    try c.writeOpCode(.RETURN, 456);

    try testing.expectEqual(1, c.code.items.len);
    try testing.expectEqual(@intFromEnum(OpCode.RETURN), c.code.items[0]);
    try testing.expectEqual(456, c.lineOfByteAt(0));
}

test "Chunk add constant" {
    var c = Chunk.init(testing.allocator);
    defer c.deinit();

    const val: Value = .from(3.14);
    const idx = try c.addConstant(val);

    try testing.expectEqual(0, idx);
    try testing.expectEqual(1, c.constants.items.len);
    try testing.expectEqual(val, c.constants.items[0]);
}

test "Chunk write constant - small index" {
    var c = Chunk.init(testing.allocator);
    defer c.deinit();

    const val: Value = .from(2.71);
    const idx = try c.addConstant(val);
    try c.writeConst(idx, 789);

    // Should use OP_CONSTANT for small indexes
    try testing.expectEqual(2, c.code.items.len);
    try testing.expectEqual(@intFromEnum(OpCode.CONSTANT), c.code.items[0]);
    try testing.expectEqual(0, c.code.items[1]);
    try testing.expectEqual(val, c.constants.items[0]);
}

test "Chunk get byte at valid offset" {
    var c = Chunk.init(testing.allocator);
    defer c.deinit();

    try c.writeByte(55, 999);
    const byte = c.byteAt(0);

    try testing.expectEqual(55, byte);
}

test "Chunk multiple operations sequence" {
    var c = Chunk.init(testing.allocator);
    defer c.deinit();

    try c.writeOpCode(.RETURN, 1);
    const val: Value = .from(42.0);
    try c.writeConst(try c.addConstant(val), 2);

    // Verify the byte sequence
    try testing.expectEqual(3, c.code.items.len);
    try testing.expectEqual(@intFromEnum(OpCode.RETURN), c.code.items[0]);
    try testing.expectEqual(@intFromEnum(OpCode.CONSTANT), c.code.items[1]);
    try testing.expectEqual(0, c.code.items[2]); // Constant index

    // Verify line numbers
    try testing.expectEqual(1, c.lineOfByteAt(0));
    try testing.expectEqual(2, c.lineOfByteAt(1));
    try testing.expectEqual(2, c.lineOfByteAt(2));
}
