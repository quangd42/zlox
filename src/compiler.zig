const std = @import("std");
const Allocator = std.mem.Allocator;
const print = std.debug.print;
const testing = std.testing;

const dbg = if (@import("builtin").is_test) true else @import("config").@"debug-trace";

const _chunk = @import("chunk.zig");
const Chunk = _chunk.Chunk;
const OpCode = _chunk.OpCode;
const debug = @import("debug.zig");
const _obj = @import("obj.zig");
const Obj = _obj.Obj;
const scanner = @import("scanner.zig");
const Scanner = scanner.Scanner;
const Token = scanner.Token;
const TokenType = scanner.TokenType;
const Value = @import("value.zig").Value;
const VM = @import("vm.zig").VM;

pub const Compiler = struct {
    scanner: Scanner,
    parser: Parser,
    chunk: *Chunk,
    vm: *VM,

    pub fn init(vm: *VM, source: []const u8, chunk: *Chunk) Compiler {
        var compiler = Compiler{
            .scanner = Scanner.init(source),
            .parser = Parser{},
            .chunk = chunk,
            .vm = vm,
        };
        compiler.advance();
        return compiler;
    }

    pub fn compile(self: *Compiler) !void {
        try self.expression();
        self.consume(.EOF, "Expect end of expression.");
        self.emitOpCode(.RETURN);
        if (dbg and self.parser.had_error) {
            debug.disassembleChunk(self.chunk, "code");
        }
        if (self.parser.had_error) return error.ParsingError;
    }

    fn emitByte(self: *Compiler, byte: u8) void {
        self.chunk.writeByte(byte, self.parser.previous.line);
    }

    fn emitOpCode(self: *Compiler, oc: OpCode) void {
        self.emitByte(@intFromEnum(oc));
    }

    fn emitConstant(self: *Compiler, value: Value) void {
        const const_idx = self.chunk.addConstant(value);
        self.chunk.writeConstant(const_idx, self.parser.previous.line);
    }

    fn advance(self: *Compiler) void {
        const parser = &self.parser;
        parser.previous = parser.current;
        while (true) {
            parser.current = self.scanner.scanToken();
            if (parser.current.type != .ERROR) break;
            self.errorAtCurrent(parser.current.lexeme);
        }
    }

    fn consume(self: *Compiler, expected: TokenType, message: []const u8) void {
        if (self.parser.current.type != expected) {
            self.errorAtCurrent(message);
            return;
        }
        self.advance();
    }

    fn errorAtPrev(self: *Compiler, message: []const u8) void {
        self.errorAt(&self.parser.previous, message);
    }

    fn errorAtCurrent(self: *Compiler, message: []const u8) void {
        self.errorAt(&self.parser.current, message);
    }

    fn errorAt(self: *Compiler, token: *Token, message: []const u8) void {
        if (self.parser.panic_mode) return;
        print("[line {d}] Error", .{token.line});
        switch (token.type) {
            .EOF => print(" at end", .{}),
            .ERROR => {},
            else => print(" at '{s}'", .{token.lexeme}),
        }
        print(": {s}\n", .{message});
        self.parser.had_error = true;
        self.parser.panic_mode = true;
    }

    fn parsePrecedence(self: *Compiler, precedence: Precedence) !void {
        self.advance();
        const prefixFn = Rules.get(self.parser.previous.type).prefix orelse {
            self.errorAtPrev("Expect expression.");
            return;
        };
        try prefixFn(self);

        while (@intFromEnum(precedence) <= @intFromEnum(Rules.get(self.parser.current.type).precedence)) {
            self.advance();
            const infixFn = Rules.get(self.parser.previous.type).infix orelse {
                self.errorAtPrev("Expect infix operator.");
                return;
            };
            try infixFn(self);
        }
    }

    fn expression(self: *Compiler) !void {
        try self.parsePrecedence(.ASSIGNMENT);
    }
};

test "compiler init & parse simple expression" {
    const source = "1 + 275";
    var chunk = Chunk.init(testing.allocator);
    var compiler = Compiler.init(testing.allocator, source, &chunk);
    try testing.expect(std.mem.eql(u8, compiler.parser.current.lexeme, "1"));
    compiler.advance();
    try testing.expect(std.mem.eql(u8, compiler.parser.previous.lexeme, "1"));
    try testing.expect(std.mem.eql(u8, compiler.parser.current.lexeme, "+"));
    compiler.advance();
    compiler.errorAtCurrent("=> expect: Error at '275'.");
    try testing.expect(std.mem.eql(u8, compiler.parser.current.lexeme, "275"));
    compiler.advance();
    compiler.errorAtCurrent("=> expect: THIS SHOULD NOT SHOW UP.");
    try testing.expect(compiler.parser.had_error);
}

const Parser = struct {
    previous: Token = undefined,
    current: Token = undefined,
    had_error: bool = false,
    panic_mode: bool = false,
};

const Precedence = enum {
    NONE,
    ASSIGNMENT, // =
    OR, // or
    AND, // and
    EQUALITY, // == !=
    COMPARISON, // < > <= >=
    TERM, // + -
    FACTOR, // * /
    UNARY, // ! -
    CALL, // . ()
    PRIMARY,
};

const ParseFn = *const fn (*Compiler) Allocator.Error!void;
const ParseRule = struct {
    infix: ?ParseFn = null,
    prefix: ?ParseFn = null,
    precedence: Precedence = .NONE,
};
const ParseRuleArray = std.EnumArray(TokenType, ParseRule);
const Rules = ParseRuleArray.init(.{
    // Single-character tokens.
    .LEFT_PAREN = .{ .prefix = grouping },
    .RIGHT_PAREN = .{},
    .LEFT_BRACE = .{},
    .RIGHT_BRACE = .{},
    .COMMA = .{},
    .DOT = .{},
    .MINUS = .{ .prefix = unary, .infix = binary, .precedence = .TERM },
    .PLUS = .{ .infix = binary, .precedence = .TERM },
    .SEMICOLON = .{},
    .SLASH = .{ .infix = binary, .precedence = .FACTOR },
    .STAR = .{ .infix = binary, .precedence = .FACTOR },
    // One or two character tokens.
    .BANG = .{ .prefix = unary },
    .BANG_EQUAL = .{ .infix = binary, .precedence = .EQUALITY },
    .EQUAL = .{ .infix = binary, .precedence = .COMPARISON },
    .EQUAL_EQUAL = .{ .infix = binary, .precedence = .COMPARISON },
    .GREATER = .{ .infix = binary, .precedence = .COMPARISON },
    .GREATER_EQUAL = .{ .infix = binary, .precedence = .COMPARISON },
    .LESS = .{ .infix = binary, .precedence = .COMPARISON },
    .LESS_EQUAL = .{ .infix = binary, .precedence = .COMPARISON },
    // Literals.
    .IDENTIFIER = .{},
    .STRING = .{ .prefix = string },
    .NUMBER = .{ .prefix = number },
    // Keywords.
    .AND = .{},
    .CLASS = .{},
    .ELSE = .{},
    .FALSE = .{ .prefix = literal },
    .FOR = .{},
    .FUN = .{},
    .IF = .{},
    .NIL = .{ .prefix = literal },
    .OR = .{},
    .PRINT = .{},
    .RETURN = .{},
    .SUPER = .{},
    .THIS = .{},
    .TRUE = .{ .prefix = literal },
    .VAR = .{},
    .WHILE = .{},

    .ERROR = .{},
    .EOF = .{},
});

test "test parse rule table" {
    const minus = Rules.get(.MINUS);
    try testing.expectEqual(.TERM, minus.precedence);
    try testing.expectEqual(binary, minus.infix);
    try testing.expectEqual(unary, minus.prefix);
}

// Parse Fns: expressions

fn number(c: *Compiler) Allocator.Error!void {
    const val = std.fmt.parseFloat(f64, c.parser.previous.lexeme) catch |err| {
        print("{any}: ", .{err});
        @panic("failed to parse number literal.");
    };
    c.emitConstant(.{ .Number = val });
}

fn grouping(c: *Compiler) Allocator.Error!void {
    try c.expression();
    c.consume(.RIGHT_PAREN, "Expect ')' after expression.");
}

fn unary(c: *Compiler) Allocator.Error!void {
    const operator_type = c.parser.previous.type;
    try c.parsePrecedence(.UNARY);

    switch (operator_type) {
        .MINUS => c.emitOpCode(.NEGATE),
        .BANG => c.emitOpCode(.NOT),
        else => unreachable,
    }
}

fn binary(c: *Compiler) Allocator.Error!void {
    const operator_type = c.parser.previous.type;
    const target_prec_int = @intFromEnum(Rules.get(operator_type).precedence) + 1;
    try c.parsePrecedence(@enumFromInt(target_prec_int));

    switch (operator_type) {
        .PLUS => c.emitOpCode(.ADD),
        .MINUS => c.emitOpCode(.SUBTRACT),
        .STAR => c.emitOpCode(.MULTIPLY),
        .SLASH => c.emitOpCode(.DIVIDE),
        .EQUAL_EQUAL => c.emitOpCode(.EQUAL),
        .BANG_EQUAL => {
            c.emitOpCode(.EQUAL);
            c.emitOpCode(.NOT);
        },
        .GREATER => c.emitOpCode(.GREATER),
        .GREATER_EQUAL => c.emitOpCode(.GREATER_EQUAL),
        .LESS => c.emitOpCode(.LESS),
        .LESS_EQUAL => c.emitOpCode(.LESS_EQUAL),
        else => |tt| {
            if (dbg) print("{}\n", .{tt});
            unreachable;
        },
    }
}

fn literal(c: *Compiler) Allocator.Error!void {
    switch (c.parser.previous.type) {
        .FALSE => c.emitOpCode(.FALSE),
        .NIL => c.emitOpCode(.NIL),
        .TRUE => c.emitOpCode(.TRUE),
        else => unreachable,
    }
}

fn string(c: *Compiler) Allocator.Error!void {
    const prev = &c.parser.previous;
    const str_obj = try ObjString.init(c.vm, prev.lexeme[1 .. prev.lexeme.len - 1]);
    c.emitConstant(.{ .Obj = &str_obj.obj });
}
