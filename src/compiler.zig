const std = @import("std");
const Allocator = std.mem.Allocator;
const print = std.debug.print;
const testing = std.testing;

const dbg = if (@import("builtin").is_test) true else @import("config").@"debug-trace";

const _chunk = @import("chunk.zig");
const Chunk = _chunk.Chunk;
const OpCode = _chunk.OpCode;
const _obj = @import("obj.zig");
const Obj = _obj.Obj;
const ObjString = _obj.String;
const debug = @import("debug.zig");
const scanner = @import("scanner.zig");
const Scanner = scanner.Scanner;
const Token = scanner.Token;
const TokenType = scanner.TokenType;
const Value = @import("value.zig").Value;
const VM = @import("vm.zig").VM;

pub const Compiler = struct {
    scanner: Scanner,
    parser: Parser,
    locals: std.ArrayList(Local),
    scope_depth: u8,
    chunk: *Chunk,
    vm: *VM,

    pub fn init(vm: *VM, source: []const u8, chunk: *Chunk) !Compiler {
        var compiler = Compiler{
            .scanner = Scanner.init(source),
            .parser = Parser{},
            .locals = try std.ArrayList(Local).initCapacity(vm.allocator, std.math.maxInt(u8)),
            .scope_depth = 0,
            .chunk = chunk,
            .vm = vm,
        };
        compiler.advance();
        return compiler;
    }

    pub fn deinit(self: *Compiler) void {
        self.locals.deinit();
    }

    pub fn compile(self: *Compiler) !void {
        while (!self.match(.EOF)) try self.declaration();
        self.consume(.EOF, "Expect end of statement.");
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
        if (!self.check(expected)) {
            self.errorAtCurrent(message);
            return;
        }
        self.advance();
    }

    fn match(self: *Compiler, expected: TokenType) bool {
        if (!self.check(expected)) return false;
        self.advance();
        return true;
    }

    fn check(self: *Compiler, expected: TokenType) bool {
        return self.parser.current.type == expected;
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

    fn synchronize(self: *Compiler) void {
        const parser = &self.parser;
        parser.panic_mode = false;

        while (parser.current.type != .EOF) : (self.advance()) {
            if (parser.previous.type == .SEMICOLON) return;
            switch (parser.current.type) {
                .CLASS, .FUN, .VAR, .FOR, .IF, .WHILE, .PRINT, .RETURN => return,
                else => {},
            }
        }
    }

    fn parsePrecedence(self: *Compiler, precedence: Precedence) !void {
        self.advance();
        const prefixFn = Rules.get(self.parser.previous.type).prefix orelse {
            self.errorAtPrev("Expect expression.");
            return;
        };
        const can_assign = precedence.isLessEql(.ASSIGNMENT);
        try prefixFn(self, can_assign);

        while (precedence.isLessEql(Rules.get(self.parser.current.type).precedence)) {
            self.advance();
            const infixFn = Rules.get(self.parser.previous.type).infix orelse {
                self.errorAtPrev("Expect infix operator.");
                return;
            };
            try infixFn(self, can_assign);
        }
    }

    fn expression(self: *Compiler) !void {
        try self.parsePrecedence(.ASSIGNMENT);
    }

    fn declaration(self: *Compiler) !void {
        const tok = self.parser.current;
        switch (tok.type) {
            .VAR => try self.varDeclaration(),
            else => try self.statement(),
        }
        if (self.parser.panic_mode) self.synchronize();
    }

    fn varDeclaration(self: *Compiler) !void {
        self.advance(); // VAR keyword
        const global_idx = try parseVariable(self);
        if (self.match(.EQUAL)) {
            try self.expression();
        } else {
            self.emitOpCode(.NIL);
        }
        self.consume(.SEMICOLON, "Expect ';' after variable declaration.");
        defineVariable(self, global_idx);
    }

    fn parseVariable(c: *Compiler) !u8 {
        c.consume(.IDENTIFIER, "Expect variable name.");
        c.declareVariable();
        if (c.scope_depth > 0) return 0;
        return makeIdentConstant(c, c.parser.previous.lexeme);
    }

    fn declareVariable(c: *Compiler) void {
        if (c.scope_depth == 0) return; // If global scope just skip
        var i = c.locals.items.len;
        while (i > 0) : (i -= 1) {
            const local = c.locals.items[i - 1];
            if (local.depth == null or local.depth.? < c.scope_depth) break;
            if (std.mem.eql(u8, c.parser.previous.lexeme, local.lexeme)) {
                c.errorAtPrev("Variable with the this name exists in this scope.");
            }
        }
        c.addLocal(c.parser.previous.lexeme);
    }

    fn addLocal(c: *Compiler, lexeme: []const u8) void {
        // depth = null to mark local as uninitialized
        c.locals.appendAssumeCapacity(.{ .lexeme = lexeme, .depth = null });
    }

    fn resolveLocal(c: *Compiler, lexeme: []const u8) ?u8 {
        var i = c.locals.items.len;
        while (i > 0) : (i -= 1) {
            const local = c.locals.items[i - 1];
            if (std.mem.eql(u8, local.lexeme, lexeme)) {
                if (local.depth == null) {
                    c.errorAtPrev("Can't read local variable in its own initializer.");
                }
                return @intCast(i - 1);
            }
        }
        return null;
    }

    fn statement(self: *Compiler) !void {
        const tok = self.parser.current;
        switch (tok.type) {
            .PRINT => try self.printStatement(),
            .LEFT_BRACE => {
                self.beginScope();
                defer self.endScope();
                try self.block();
            },
            else => try self.expressionStatement(),
        }
    }

    fn printStatement(self: *Compiler) !void {
        self.advance();
        try self.expression();
        self.consume(.SEMICOLON, "Expect ';' after value.");
        self.emitOpCode(.PRINT);
    }

    fn expressionStatement(self: *Compiler) !void {
        try self.expression();
        self.consume(.SEMICOLON, "Expect ';' after value.");
        self.emitOpCode(.POP);
    }

    fn block(self: *Compiler) Allocator.Error!void {
        self.advance(); // .LEFT_BRACE
        while (!self.check(.RIGHT_BRACE) and !self.check(.EOF)) {
            try self.declaration();
        }
        self.consume(.RIGHT_BRACE, "Expect '}' after block.");
    }

    fn beginScope(c: *Compiler) void {
        c.scope_depth += 1;
    }

    fn endScope(c: *Compiler) void {
        c.scope_depth -= 1;
        while (c.locals.items.len > 0 and c.locals.getLast().depth.? > c.scope_depth) {
            c.emitOpCode(.POP);
            _ = c.locals.pop();
        }
    }
};

test "compiler init & parse simple expression" {
    const source = "1 + 275";
    var vm = @import("vm.zig").VM.init(testing.allocator);
    defer vm.deinit();
    var chunk = try Chunk.init(testing.allocator);
    defer chunk.deinit();
    var compiler = try Compiler.init(&vm, source, &chunk);
    defer compiler.deinit();
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

const Local = struct {
    lexeme: []const u8,
    depth: ?u8,
};

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

    fn isLessEql(self: Precedence, other: Precedence) bool {
        return @intFromEnum(self) <= @intFromEnum(other);
    }
};

const ParseFn = *const fn (*Compiler, bool) Allocator.Error!void;
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
    .IDENTIFIER = .{ .prefix = variable },
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

fn number(c: *Compiler, can_assign: bool) Allocator.Error!void {
    _ = can_assign;
    const val = std.fmt.parseFloat(f64, c.parser.previous.lexeme) catch |err| {
        print("{any}: ", .{err});
        @panic("failed to parse number literal.");
    };
    c.emitConstant(.{ .Number = val });
}

fn grouping(c: *Compiler, can_assign: bool) Allocator.Error!void {
    _ = can_assign;
    try c.expression();
    c.consume(.RIGHT_PAREN, "Expect ')' after expression.");
}

fn unary(c: *Compiler, can_assign: bool) Allocator.Error!void {
    _ = can_assign;
    const operator_type = c.parser.previous.type;
    try c.parsePrecedence(.UNARY);

    switch (operator_type) {
        .MINUS => c.emitOpCode(.NEGATE),
        .BANG => c.emitOpCode(.NOT),
        else => unreachable,
    }
}

fn binary(c: *Compiler, can_assign: bool) Allocator.Error!void {
    _ = can_assign;
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

fn literal(c: *Compiler, can_assign: bool) Allocator.Error!void {
    _ = can_assign;
    switch (c.parser.previous.type) {
        .FALSE => c.emitOpCode(.FALSE),
        .NIL => c.emitOpCode(.NIL),
        .TRUE => c.emitOpCode(.TRUE),
        else => unreachable,
    }
}

fn string(c: *Compiler, can_assign: bool) Allocator.Error!void {
    _ = can_assign;
    const prev = &c.parser.previous;
    const str_obj = try ObjString.init(c.vm, prev.lexeme[1 .. prev.lexeme.len - 1]);
    c.emitConstant(.{ .Obj = &str_obj.obj });
}

// prefix parseFn for variable
fn variable(c: *Compiler, can_assign: bool) Allocator.Error!void {
    try namedVariable(c, c.parser.previous.lexeme, can_assign);
}

fn namedVariable(c: *Compiler, lexeme: []const u8, can_assign: bool) !void {
    var getOp: OpCode = .GET_LOCAL;
    var setOp: OpCode = .SET_LOCAL;
    var const_idx: u8 = 0;
    const local_idx = c.resolveLocal(lexeme);
    if (local_idx) |idx| {
        const_idx = idx;
    } else {
        getOp = .GET_GLOBAL;
        setOp = .SET_GLOBAL;
        const_idx = try makeIdentConstant(c, lexeme);
    }
    if (can_assign and c.match(.EQUAL)) {
        try c.expression();
        c.emitOpCode(setOp);
    } else {
        c.emitOpCode(getOp);
    }
    c.emitByte(const_idx);
}

fn makeIdentConstant(c: *Compiler, lexeme: []const u8) !u8 {
    const str_obj = try ObjString.init(c.vm, lexeme);
    return c.chunk.addConstant(.{ .Obj = &str_obj.obj });
}

fn defineVariable(c: *Compiler, constant_idx: u8) void {
    if (c.scope_depth > 0) {
        // mark variable as initialized
        c.locals.items[c.locals.items.len - 1].depth = c.scope_depth;
        return;
    }

    c.emitOpCode(.DEFINE_GLOBAL);
    c.emitByte(constant_idx);
}
