const std = @import("std");
const Allocator = std.mem.Allocator;
const print = std.debug.print;
const testing = std.testing;

const dbg = @import("config").@"debug-trace";

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
    rules: ParseRuleArray,

    pub fn init(vm: *VM, source: []const u8, chunk: *Chunk) Compiler {
        var compiler = Compiler{
            .scanner = Scanner.init(source),
            .parser = Parser{},
            .chunk = chunk,
            .rules = makeParseRules(), // TODO: make this comptime
            .vm = vm,
        };
        compiler.advance();
        return compiler;
    }

    pub fn compile(self: *Compiler) !void {
        try self.expression();
        self.consume(.EOF, "Expect end of expression.");
        try self.emitOpCode(.RETURN);
        if (dbg and self.parser.had_error) {
            debug.disassembleChunk(self.chunk, "code");
        }
        if (self.parser.had_error) return error.ParsingError;
    }

    fn emitByte(self: *Compiler, byte: u8) Allocator.Error!void {
        try self.chunk.writeByte(byte, self.parser.previous.line);
    }

    fn emitOpCode(self: *Compiler, oc: OpCode) Allocator.Error!void {
        try self.emitByte(@intFromEnum(oc));
    }

    fn emitConstant(self: *Compiler, value: Value) Allocator.Error!void {
        self.chunk.writeConstant(value, self.parser.previous.line) catch |err| switch (err) {
            error.TooManyConstants => {
                self.errorAtPrev("Too many constants in one chunk.");
                return;
            },
            else => return error.OutOfMemory,
        };
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

    fn parsePrecedence(self: *Compiler, precedence: Precedence) Allocator.Error!void {
        self.advance();
        const prefixFn = self.rules.get(self.parser.previous.type).prefix;
        if (prefixFn == null) {
            self.errorAtPrev("Expect expression.");
            return;
        }
        try prefixFn.?(self);

        while (@intFromEnum(precedence) <= @intFromEnum(self.rules.get(self.parser.current.type).precedence)) {
            self.advance();
            const infixFn = self.rules.get(self.parser.previous.type).infix;
            try infixFn.?(self);
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
fn makeParseRules() ParseRuleArray {
    return ParseRuleArray.init(.{
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
}

test "test parse rule table" {
    const rules = makeParseRules();
    const minus = rules.get(.MINUS);
    try testing.expectEqual(.TERM, minus.precedence);
    try testing.expectEqual(binary, minus.infix);
    try testing.expectEqual(unary, minus.prefix);
}

// Parse Fns

fn number(self: *Compiler) Allocator.Error!void {
    const val = std.fmt.parseFloat(f64, self.parser.previous.lexeme) catch |err| {
        print("{any}: ", .{err});
        @panic("failed to parse number literal.");
    };
    try self.emitConstant(.{ .Number = val });
}

fn grouping(self: *Compiler) Allocator.Error!void {
    try self.expression();
    self.consume(.RIGHT_PAREN, "Expect ')' after expression.");
}

fn unary(self: *Compiler) Allocator.Error!void {
    const operator_type = self.parser.previous.type;
    try self.parsePrecedence(.UNARY);

    switch (operator_type) {
        .MINUS => try self.emitOpCode(.NEGATE),
        .BANG => try self.emitOpCode(.NOT),
        else => unreachable,
    }
}

fn binary(self: *Compiler) Allocator.Error!void {
    const operator_type = self.parser.previous.type;
    const target_prec_int = @intFromEnum(self.rules.get(operator_type).precedence) + 1;
    try self.parsePrecedence(@enumFromInt(target_prec_int));

    switch (operator_type) {
        .PLUS => try self.emitOpCode(.ADD),
        .MINUS => try self.emitOpCode(.SUBTRACT),
        .STAR => try self.emitOpCode(.MULTIPLY),
        .SLASH => try self.emitOpCode(.DIVIDE),
        .EQUAL_EQUAL => try self.emitOpCode(.EQUAL),
        .BANG_EQUAL => {
            try self.emitOpCode(.EQUAL);
            try self.emitOpCode(.NOT);
        },
        .GREATER => try self.emitOpCode(.GREATER),
        .GREATER_EQUAL => try self.emitOpCode(.GREATER_EQUAL),
        .LESS => try self.emitOpCode(.LESS),
        .LESS_EQUAL => try self.emitOpCode(.LESS_EQUAL),
        else => |tt| {
            if (dbg) print("{}\n", .{tt});
            unreachable;
        },
    }
}

fn literal(self: *Compiler) Allocator.Error!void {
    switch (self.parser.previous.type) {
        .FALSE => try self.emitOpCode(.FALSE),
        .NIL => try self.emitOpCode(.NIL),
        .TRUE => try self.emitOpCode(.TRUE),
        else => unreachable,
    }
}

fn string(self: *Compiler) Allocator.Error!void {
    const prev = &self.parser.previous;
    const str_obj = try _obj.String.init(self.vm, prev.lexeme[1 .. prev.lexeme.len - 1]);
    try self.emitConstant(.{ .Obj = &str_obj.obj });
}
