const std = @import("std");
const Allocator = std.mem.Allocator;
const print = std.debug.print;
const testing = std.testing;

const TRACE_EXECUTION = @import("debug").@"trace-execution";

const _chunk = @import("chunk.zig");
const Chunk = _chunk.Chunk;
const OpCode = _chunk.OpCode;
const Obj = @import("obj.zig");
const FunctionType = Obj.FunctionType;
const _scanner = @import("scanner.zig");
const Scanner = _scanner.Scanner;
const Token = _scanner.Token;
const TokenType = _scanner.TokenType;
const debug = @import("debug.zig");
const Value = @import("value.zig").Value;
const VM = @import("vm.zig").VM;

const DEBUGGING = @import("builtin").mode == .Debug;

const Error = error{OutOfMemory};

scanner: Scanner,
parser: Parser,
current_func: ?*Compiler,
current_class: ?*ClassCompiler,
vm: *VM,

const Self = @This();
pub fn init(vm: *VM, source: []const u8) !Self {
    var self = Self{
        .scanner = Scanner.init(source),
        .parser = Parser{},
        .current_func = null,
        .current_class = null,
        .vm = vm,
    };
    self.advance();
    return self;
}

fn setupCompiler(self: *Self, compiler: *Compiler) !void {
    if (compiler.type != .Script) {
        compiler.function.name = try Obj.String.init(self.vm, self.parser.previous.lexeme);
    }
    compiler.enclosing = self.current_func;
    self.current_func = compiler; // important to happen before any further allocation to keep compiler alive
    try compiler.locals.append(.{
        .depth = 0,
        .lexeme = if (compiler.type != .Function) "this" else "",
    });
}

fn endCompiler(self: *Self) !*Obj.Function {
    try self.emitReturn();

    if (TRACE_EXECUTION and self.parser.had_error) {
        const name = if (self.current_func.?.function.name) |str| str.chars else "<script>";
        debug.disassembleChunk(self.chunk(), name);
    }
    const current = self.current_func.?;
    defer current.deinit(); // clean up early, but can be left to gc
    self.current_func = current.enclosing;
    return current.function;
}

pub fn compile(self: *Self) !*Obj.Function {
    var compiler = try Compiler.init(self.vm, .Script);
    try self.setupCompiler(&compiler);
    while (!self.match(.EOF)) try self.declaration();
    self.consume(.EOF, "Expect end of statement.");
    const fun = try self.endCompiler();

    if (self.parser.had_error) return error.ParsingError;
    return fun;
}

fn chunk(self: *Self) *Chunk {
    return &self.current_func.?.function.chunk;
}
fn beginScope(self: *Self) void {
    self.current_func.?.scope_depth += 1;
}

fn endScope(self: *Self) !void {
    const scope_depth = &self.current_func.?.scope_depth;
    scope_depth.* -= 1;
    const locals = &self.current_func.?.locals;
    while (true) : (_ = locals.pop()) {
        const stack_top = locals.getLastOrNull() orelse break;
        if (stack_top.depth.? <= scope_depth.*) break;
        if (stack_top.is_captured) {
            try self.emitOpCode(.CLOSE_UPVALUE);
        } else {
            try self.emitOpCode(.POP);
        }
    }
}

fn emitByte(self: *Self, byte: u8) !void {
    try self.chunk().writeByte(byte, self.parser.previous.line);
}

fn emitOpCode(self: *Self, oc: OpCode) !void {
    try self.emitByte(@intFromEnum(oc));
}

fn emitJump(self: *Self, instr: OpCode) !usize {
    try self.emitOpCode(instr);
    try self.emitByte(0xff);
    try self.emitByte(0xff);
    return self.chunk().code.items.len - 2;
}

fn emitLoop(self: *Self, loop_start: usize) !void {
    try self.emitOpCode(.LOOP);
    const distance = self.chunk().code.items.len - loop_start + 2;
    if (distance > std.math.maxInt(u16))
        return self.err("Loop body too large.");
    try self.emitByte(@intCast(distance >> 8 & 0xff));
    try self.emitByte(@intCast(distance & 0xff));
}

fn emitReturn(self: *Self) !void {
    if (self.current_func.?.type == .Initializer) {
        // always return instance (at slot 0 of call frame) form initializer
        try self.emitOpCode(.GET_LOCAL);
        try self.emitByte(0);
    } else {
        // Implicit nil return at the end of function
        try self.emitOpCode(.NIL);
    }
    try self.emitOpCode(.RETURN);
}

fn emitConstant(self: *Self, value: Value) !void {
    const const_idx = self.chunk().addConstant(value) catch {
        return self.err("Too many constants in one chunk.");
    };
    try self.chunk().writeConstant(const_idx, self.parser.previous.line);
}

fn patchJump(self: *Self, offset: usize) void {
    // distance is how far to jump back after reading the offset
    // -2 to account for the offset bytes itself
    const distance = self.chunk().code.items.len - offset - 2;

    if (distance > std.math.maxInt(u16))
        return self.err("Too much code to jump over.");

    self.chunk().code.items[offset] = @as(u8, @intCast(distance >> 8)) & 0xff;
    self.chunk().code.items[offset + 1] = @as(u8, @intCast(distance)) & 0xff;
}

fn advance(self: *Self) void {
    const parser = &self.parser;
    parser.previous = parser.current;
    while (true) {
        parser.current = self.scanner.scanToken();
        if (parser.current.type != .ERROR) break;
        self.errorAtCurrent(parser.current.lexeme);
    }
}

fn consume(self: *Self, expected: TokenType, message: []const u8) void {
    if (!self.check(expected)) return self.errorAtCurrent(message);

    self.advance();
}

fn match(self: *Self, expected: TokenType) bool {
    if (!self.check(expected)) return false;
    self.advance();
    return true;
}

fn check(self: *Self, expected: TokenType) bool {
    return self.parser.current.type == expected;
}

fn err(self: *Self, message: []const u8) void {
    self.errorAt(&self.parser.previous, message);
}

fn errorAtCurrent(self: *Self, message: []const u8) void {
    self.errorAt(&self.parser.current, message);
}

fn errorAt(self: *Self, token: *Token, message: []const u8) void {
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

fn synchronize(self: *Self) void {
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

fn parsePrecedence(self: *Self, precedence: Precedence) !void {
    self.advance();
    const prefixFn = Rules.get(self.parser.previous.type).prefix orelse {
        return self.err("Expect expression.");
    };
    const can_assign = precedence.isLessEql(.ASSIGNMENT);
    try prefixFn(self, can_assign);

    while (precedence.isLessEql(Rules.get(self.parser.current.type).precedence)) {
        self.advance();
        const infixFn = Rules.get(self.parser.previous.type).infix orelse {
            return self.err("Expect infix operator.");
        };
        try infixFn(self, can_assign);
    }

    // At this point EQUAL should have been consumed already by valid parseFn
    // so the target must be invalid
    if (can_assign and self.match(.EQUAL)) {
        return self.err("Invalid assignment target.");
    }
}

fn makeIdentConstant(self: *Self, lexeme: []const u8) !u8 {
    const str_obj = try Obj.String.init(self.vm, lexeme);
    try self.vm.push(.{ .Obj = &str_obj.obj });
    defer _ = self.vm.pop();
    return try self.chunk().addConstant(.{ .Obj = &str_obj.obj });
}

fn addLocal(self: *Self, lexeme: []const u8) !void {
    const locals = &self.current_func.?.locals;
    if (locals.items.len >= U8_COUNT)
        return self.err("Too many local variables in function.");
    // depth = null to mark local as uninitialized
    try locals.append(.{ .lexeme = lexeme, .depth = null });
}

fn resolveLocal(self: *Self, compiler: *Compiler, lexeme: []const u8) ?u8 {
    var i = compiler.locals.items.len;
    while (i > 0) : (i -= 1) {
        const local = compiler.locals.items[i - 1];
        if (std.mem.eql(u8, local.lexeme, lexeme)) {
            const depth = local.depth orelse {
                self.err("Can't read local variable in its own initializer.");
                return null;
            };
            if (depth <= compiler.scope_depth) return @intCast(i - 1);
        }
    }
    return null;
}

fn addUpvalue(self: *Self, compiler: *Compiler, index: u8, is_local: bool) u8 {
    const upvalue_count = &compiler.function.upvalue_count;
    for (0..upvalue_count.*) |i| {
        const upvalue = &compiler.upvalues[i];
        if (upvalue.index == index and upvalue.is_local == is_local) return @intCast(i);
    }

    if (upvalue_count.* == std.math.maxInt(u8)) {
        self.err("Too many closure variables in function.");
        return 0;
    }

    compiler.upvalues[upvalue_count.*] = .{ .index = index, .is_local = is_local };
    upvalue_count.* += 1;
    return upvalue_count.* - 1;
}

fn resolveUpvalue(self: *Self, compiler: *Compiler, lexeme: []const u8) ?u8 {
    const enclosing = compiler.enclosing orelse return null;

    const local = self.resolveLocal(enclosing, lexeme);
    if (local) |idx| {
        compiler.enclosing.?.locals.items[idx].is_captured = true;
        return self.addUpvalue(self.current_func.?, idx, true);
    }

    const upvalue = self.resolveUpvalue(enclosing, lexeme);
    if (upvalue) |idx| return self.addUpvalue(self.current_func.?, idx, false);

    return null;
}

fn declareVariable(self: *Self) !void {
    const current = self.current_func.?;
    // If we're at global scope, no work to do
    if (current.scope_depth == 0) return;
    // Look in current scope if variable with the same name is already declared
    var i = current.locals.items.len;
    while (i > 0) : (i -= 1) {
        const local = current.locals.items[i - 1];
        if (local.depth == null or local.depth.? < current.scope_depth) break;
        if (std.mem.eql(u8, self.parser.previous.lexeme, local.lexeme)) {
            return self.err("Variable with the this name exists in this scope.");
        }
    }
    // Add it to local stack
    try self.addLocal(self.parser.previous.lexeme);
}

fn parseVariable(self: *Self, err_msg: []const u8) !u8 {
    self.consume(.IDENTIFIER, err_msg);
    try self.declareVariable();
    if (self.current_func.?.scope_depth > 0) return 0;
    return self.makeIdentConstant(self.parser.previous.lexeme);
}

fn markInitialized(self: *Self) void {
    // mark variable as initialized
    const compiler = self.current_func.?;
    compiler.locals.items[compiler.locals.items.len - 1].depth = compiler.scope_depth;
}

fn defineVariable(self: *Self, constant_idx: u8) !void {
    const compiler = self.current_func.?;
    if (compiler.scope_depth > 0) {
        self.markInitialized();
        return;
    }

    try self.emitOpCode(.DEFINE_GLOBAL);
    try self.emitByte(constant_idx);
}

fn argumentList(self: *Self) !u8 {
    var arg_count: u8 = 0;
    while (!self.check(.RIGHT_PAREN)) {
        try self.expression();
        if (arg_count == 255) {
            self.err("Can't have more than 255 arguments.");
            return arg_count;
        }
        arg_count += 1;
        if (!self.match(.COMMA)) break;
    }
    self.consume(.RIGHT_PAREN, "Expect ')' after arguments.");
    return arg_count;
}

fn expression(self: *Self) !void {
    try self.parsePrecedence(.ASSIGNMENT);
}

fn declaration(self: *Self) !void {
    const tok = self.parser.current;
    switch (tok.type) {
        .CLASS => try self.classDeclaration(),
        .FUN => try self.funDeclaration(),
        .VAR => try self.varDeclaration(),
        .IF => try self.ifStatement(),
        .WHILE => try self.whileStatement(),
        .FOR => try self.forStatement(),
        else => try self.statement(),
    }
    if (self.parser.panic_mode) self.synchronize();
}

fn varDeclaration(self: *Self) !void {
    self.advance(); // VAR keyword
    const global_idx = try self.parseVariable("Expect variable name.");
    if (self.match(.EQUAL)) {
        try self.expression();
    } else {
        try self.emitOpCode(.NIL);
    }
    self.consume(.SEMICOLON, "Expect ';' after variable declaration.");
    try self.defineVariable(global_idx);
}

fn statement(self: *Self) Error!void {
    const tok = self.parser.current;
    switch (tok.type) {
        .PRINT => try self.printStatement(),
        .FOR => try self.forStatement(),
        .IF => try self.ifStatement(),
        .RETURN => try self.returnStatement(),
        .WHILE => try self.whileStatement(),
        .LEFT_BRACE => {
            self.beginScope();
            try self.block();
            try self.endScope();
        },
        else => try self.expressionStatement(),
    }
}

fn returnStatement(self: *Self) !void {
    self.advance(); // RETURN
    if (self.current_func.?.type == .Script) {
        return self.err("Can't return from top-level code.");
    }
    if (self.match(.SEMICOLON)) {
        return self.emitReturn();
    }
    if (self.current_func.?.type == .Initializer) {
        self.err("Can't return a value from an initializer.");
    }
    try self.expression();
    self.consume(.SEMICOLON, "Expect ';' after return statement.");
    try self.emitOpCode(.RETURN);
}

fn printStatement(self: *Self) !void {
    self.advance(); // PRINT
    try self.expression();
    self.consume(.SEMICOLON, "Expect ';' after value.");
    try self.emitOpCode(.PRINT);
}

fn ifStatement(self: *Self) !void {
    self.advance(); // IF
    self.consume(.LEFT_PAREN, "Expect '(' after 'if'.");
    try self.expression();
    self.consume(.RIGHT_PAREN, "Expect ')' after condition.");
    const skip_then_loc = try self.emitJump(.JUMP_IF_FALSE);
    try self.emitOpCode(.POP);
    try self.statement();
    const skip_else_loc = try self.emitJump(.JUMP);
    self.patchJump(skip_then_loc);
    try self.emitOpCode(.POP);
    if (self.match(.ELSE)) try self.statement();
    self.patchJump(skip_else_loc);
}

fn whileStatement(self: *Self) !void {
    self.advance(); // WHILE
    const loop_start_loc = self.chunk().code.items.len;
    self.consume(.LEFT_PAREN, "Expect '(' after 'while'.");
    try self.expression();
    self.consume(.RIGHT_PAREN, "Expect ')' after condition.");

    const skip_body_loc = try self.emitJump(.JUMP_IF_FALSE);
    try self.emitOpCode(.POP);
    try self.statement();
    try self.emitLoop(loop_start_loc);

    self.patchJump(skip_body_loc);
    try self.emitOpCode(.POP);
}

fn forStatement(self: *Self) !void {
    self.advance(); // FOR
    self.beginScope();
    self.consume(.LEFT_PAREN, "Expect '(' after 'for'.");
    if (!self.match(.SEMICOLON)) {
        switch (self.parser.current.type) {
            .VAR => try self.varDeclaration(),
            else => try self.expressionStatement(),
        }
    }
    var loop_start = self.chunk().code.items.len;
    var skip_body_loc: ?usize = null;
    if (!self.match(.SEMICOLON)) {
        try self.expression();
        self.consume(.SEMICOLON, "Expect ';' after loop condition.");
        skip_body_loc = try self.emitJump(.JUMP_IF_FALSE);
        try self.emitOpCode(.POP);
    }
    if (!self.match(.RIGHT_PAREN)) {
        const skip_increment_loc = try self.emitJump(.JUMP);
        const increment_start_loc = self.chunk().code.items.len;
        try self.expression();
        try self.emitOpCode(.POP);
        self.consume(.RIGHT_PAREN, "Expect ')' after for clauses.");

        try self.emitLoop(loop_start);
        loop_start = increment_start_loc;
        self.patchJump(skip_increment_loc);
    }

    try self.statement();
    try self.emitLoop(loop_start);

    if (skip_body_loc) |loc| {
        self.patchJump(loc);
        try self.emitOpCode(.POP);
    }

    try self.endScope();
}

fn expressionStatement(self: *Self) !void {
    try self.expression();
    self.consume(.SEMICOLON, "Expect ';' after expression.");
    try self.emitOpCode(.POP);
}

fn block(self: *Self) Error!void {
    self.advance(); // LEFT_BRACE
    while (!self.check(.RIGHT_BRACE) and !self.check(.EOF)) {
        try self.declaration();
    }
    self.consume(.RIGHT_BRACE, "Expect '}' after block.");
}

fn function(self: *Self, fun_type: FunctionType) !void {
    var compiler = try Compiler.init(self.vm, fun_type);
    try self.setupCompiler(&compiler);
    self.beginScope();

    self.consume(.LEFT_PAREN, "Expect '(' after function name.");
    while (!self.check(.RIGHT_PAREN)) {
        self.current_func.?.function.arity += 1;
        const const_idx = try self.parseVariable("Expect parameter name.");
        try self.defineVariable(const_idx);
        if (self.check(.COMMA)) {
            self.advance(); // COMMA
            if (self.current_func.?.function.arity == 255) {
                self.errorAtCurrent("Can't have more than 255 parameters.");
                // skip until ')' to avoid recursive panic
                while (!self.check(.RIGHT_PAREN)) self.advance();
            }
        } else break;
    }
    self.consume(.RIGHT_PAREN, "Expect ')' after parameters.");
    if (!self.check(.LEFT_BRACE))
        self.errorAtCurrent("Expect '{' before function body.");
    try self.block();
    const fun = try self.endCompiler();
    const const_idx = try self.chunk().addConstant(.{ .Obj = &fun.obj });
    try self.emitOpCode(.CLOSURE);
    try self.emitByte(const_idx);

    for (0..fun.upvalue_count) |i| {
        try self.emitByte(@intFromBool(compiler.upvalues[i].is_local));
        try self.emitByte(compiler.upvalues[i].index);
    }
}

fn method(self: *Self) !void {
    self.consume(.IDENTIFIER, "Expect method name.");
    const method_name = self.parser.previous.lexeme;
    const method_name_idx = try self.makeIdentConstant(method_name);

    const method_type: FunctionType = if (std.mem.eql(u8, method_name, "init")) .Initializer else .Method;
    try self.function(method_type);
    try self.emitOpCode(.METHOD);
    try self.emitByte(method_name_idx);
}

fn funDeclaration(self: *Self) !void {
    self.advance(); // FUN
    const fun_name_idx = try self.parseVariable("Expect function name.");
    self.markInitialized();
    try self.function(.Function);
    try self.defineVariable(fun_name_idx);
}

fn classDeclaration(self: *Self) !void {
    self.advance(); // CLASS
    self.consume(.IDENTIFIER, "Expect class name.");
    const class_name = self.parser.previous.lexeme;
    const class_name_idx = try self.makeIdentConstant(self.parser.previous.lexeme);
    try self.declareVariable();

    try self.emitOpCode(.CLASS);
    try self.emitByte(class_name_idx);
    try self.defineVariable(class_name_idx);

    var class_compiler = ClassCompiler{};
    class_compiler.enclosing = self.current_class;
    self.current_class = &class_compiler;

    try self.namedVariable(class_name, false);
    self.consume(.LEFT_BRACE, "Expect '{' before class body.");
    while (!self.check(.RIGHT_BRACE) and !self.check(.EOF)) {
        try self.method();
    }
    self.consume(.RIGHT_BRACE, "Expect '}' after class body.");
    try self.emitOpCode(.POP);

    self.current_class = self.current_class.?.enclosing;
}

test "compiler init & parse simple expression" {
    const source = "print 1 + 275;";
    var vm = try @import("vm.zig").VM.init(testing.allocator);
    defer vm.deinit();
    var state = try Self.init(vm, source);
    const fun = try state.compile();

    try testing.expectEqual(null, fun.name);
    try testing.expectEqual(0, fun.arity);
    try testing.expectEqual(2, fun.chunk.constants.items.len);
    try testing.expectEqual(Value{ .Number = 1 }, fun.chunk.constants.items[0]);
    try testing.expectEqual(Value{ .Number = 275 }, fun.chunk.constants.items[1]);
    try testing.expectEqual(OpCode.ADD, @as(OpCode, @enumFromInt(fun.chunk.code.items[4])));
}

const Local = struct {
    lexeme: []const u8,
    depth: ?u8,
    is_captured: bool = false,
};

const Upvalue = struct {
    index: u8,
    is_local: bool,
};

const U8_COUNT = std.math.maxInt(u8) + 1;

pub const Compiler = struct {
    enclosing: ?*Compiler = null,

    locals: std.ArrayList(Local),
    scope_depth: u8,
    upvalues: [U8_COUNT]Upvalue,

    function: *Obj.Function,
    type: FunctionType,

    pub fn init(vm: *VM, fun_type: FunctionType) !Compiler {
        return Compiler{
            .locals = std.ArrayList(Local).init(vm.allocator),
            .scope_depth = 0,
            .upvalues = [_]Upvalue{.{ .index = 0, .is_local = false }} ** U8_COUNT,
            .function = try Obj.Function.init(vm),
            .type = fun_type,
        };
    }

    pub fn deinit(self: *Compiler) void {
        self.locals.deinit();
        // .function will be cleaned up by GC
    }
};

pub const ClassCompiler = struct {
    enclosing: ?*ClassCompiler = null,
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

const ParseFn = *const fn (*Self, bool) Error!void;
const ParseRule = struct {
    infix: ?ParseFn = null,
    prefix: ?ParseFn = null,
    precedence: Precedence = .NONE,
};
const ParseRuleArray = std.EnumArray(TokenType, ParseRule);
const Rules = ParseRuleArray.init(.{
    // Single-character tokens.
    .LEFT_PAREN = .{ .prefix = grouping, .infix = call, .precedence = .CALL },
    .RIGHT_PAREN = .{},
    .LEFT_BRACE = .{},
    .RIGHT_BRACE = .{},
    .COMMA = .{},
    .DOT = .{ .infix = dot, .precedence = .CALL },
    .MINUS = .{ .prefix = unary, .infix = binary, .precedence = .TERM },
    .PLUS = .{ .infix = binary, .precedence = .TERM },
    .SEMICOLON = .{},
    .SLASH = .{ .infix = binary, .precedence = .FACTOR },
    .STAR = .{ .infix = binary, .precedence = .FACTOR },
    // One or two character tokens.
    .BANG = .{ .prefix = unary },
    .BANG_EQUAL = .{ .infix = binary, .precedence = .EQUALITY },
    .EQUAL = .{},
    .EQUAL_EQUAL = .{ .infix = binary, .precedence = .EQUALITY },
    .GREATER = .{ .infix = binary, .precedence = .COMPARISON },
    .GREATER_EQUAL = .{ .infix = binary, .precedence = .COMPARISON },
    .LESS = .{ .infix = binary, .precedence = .COMPARISON },
    .LESS_EQUAL = .{ .infix = binary, .precedence = .COMPARISON },
    // Literals.
    .IDENTIFIER = .{ .prefix = variable },
    .STRING = .{ .prefix = string },
    .NUMBER = .{ .prefix = number },
    // Keywords.
    .AND = .{ .infix = and_, .precedence = .AND },
    .CLASS = .{},
    .ELSE = .{},
    .FALSE = .{ .prefix = literal },
    .FOR = .{},
    .FUN = .{},
    .IF = .{},
    .NIL = .{ .prefix = literal },
    .OR = .{ .infix = or_, .precedence = .OR },
    .PRINT = .{},
    .RETURN = .{},
    .SUPER = .{},
    .THIS = .{ .prefix = this },
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

fn number(self: *Self, can_assign: bool) Error!void {
    _ = can_assign;
    const val = std.fmt.parseFloat(f64, self.parser.previous.lexeme) catch |e| {
        print("{any}: ", .{e});
        @panic("failed to parse number literal.");
    };
    try self.emitConstant(.{ .Number = val });
}

fn grouping(self: *Self, can_assign: bool) Error!void {
    _ = can_assign;
    try self.expression();
    self.consume(.RIGHT_PAREN, "Expect ')' after expression.");
}

fn unary(self: *Self, can_assign: bool) Error!void {
    _ = can_assign;
    const operator_type = self.parser.previous.type;
    try self.parsePrecedence(.UNARY);

    try switch (operator_type) {
        .MINUS => self.emitOpCode(.NEGATE),
        .BANG => self.emitOpCode(.NOT),
        else => unreachable,
    };
}

fn binary(self: *Self, can_assign: bool) Error!void {
    _ = can_assign;
    const operator_type = self.parser.previous.type;
    const target_prec_int = @intFromEnum(Rules.get(operator_type).precedence) + 1;
    try self.parsePrecedence(@enumFromInt(target_prec_int));

    try switch (operator_type) {
        .PLUS => self.emitOpCode(.ADD),
        .MINUS => self.emitOpCode(.SUBTRACT),
        .STAR => self.emitOpCode(.MULTIPLY),
        .SLASH => self.emitOpCode(.DIVIDE),
        .EQUAL_EQUAL => self.emitOpCode(.EQUAL),
        .BANG_EQUAL => {
            try self.emitOpCode(.EQUAL);
            try self.emitOpCode(.NOT);
        },
        .GREATER => self.emitOpCode(.GREATER),
        .GREATER_EQUAL => self.emitOpCode(.GREATER_EQUAL),
        .LESS => self.emitOpCode(.LESS),
        .LESS_EQUAL => self.emitOpCode(.LESS_EQUAL),
        else => |tt| {
            if (DEBUGGING) print("{}\n", .{tt});
            unreachable;
        },
    };
}

fn call(self: *Self, can_assign: bool) Error!void {
    _ = can_assign;
    const arg_count = try self.argumentList();
    try self.emitOpCode(.CALL);
    try self.emitByte(arg_count);
}

fn dot(self: *Self, can_assign: bool) Error!void {
    self.consume(.IDENTIFIER, "Expect property name after '.'.");
    const field_name_idx = try self.makeIdentConstant(self.parser.previous.lexeme);

    if (can_assign and self.match(.EQUAL)) {
        try self.expression();
        try self.emitOpCode(.SET_PROPERTY);
    } else {
        try self.emitOpCode(.GET_PROPERTY);
    }
    try self.emitByte(field_name_idx);
}

fn literal(self: *Self, can_assign: bool) Error!void {
    _ = can_assign;
    try switch (self.parser.previous.type) {
        .FALSE => self.emitOpCode(.FALSE),
        .NIL => self.emitOpCode(.NIL),
        .TRUE => self.emitOpCode(.TRUE),
        else => unreachable,
    };
}

fn string(self: *Self, can_assign: bool) Error!void {
    _ = can_assign;
    const prev = &self.parser.previous;
    const str_obj = try Obj.String.init(self.vm, prev.lexeme[1 .. prev.lexeme.len - 1]);
    const val: Value = .{ .Obj = &str_obj.obj };
    try self.vm.push(val);
    defer _ = self.vm.pop();
    try self.emitConstant(val);
}

fn namedVariable(self: *Self, lexeme: []const u8, can_assign: bool) !void {
    var getOp: OpCode = .GET_LOCAL;
    var setOp: OpCode = .SET_LOCAL;
    const const_idx: u8 = blk: {
        if (self.resolveLocal(self.current_func.?, lexeme)) |idx| {
            break :blk idx;
        }
        if (self.resolveUpvalue(self.current_func.?, lexeme)) |idx| {
            getOp = .GET_UPVALUE;
            setOp = .SET_UPVALUE;
            break :blk idx;
        }
        getOp = .GET_GLOBAL;
        setOp = .SET_GLOBAL;
        break :blk try self.makeIdentConstant(lexeme);
    };
    if (can_assign and self.match(.EQUAL)) {
        try self.expression();
        try self.emitOpCode(setOp);
    } else {
        try self.emitOpCode(getOp);
    }
    try self.emitByte(const_idx);
}

// prefix parseFn for variable
fn variable(self: *Self, can_assign: bool) Error!void {
    try namedVariable(self, self.parser.previous.lexeme, can_assign);
}

fn this(self: *Self, can_assign: bool) Error!void {
    _ = can_assign;
    if (self.current_class == null)
        return self.err("Can't use 'this' outside of a class.");
    try self.variable(false);
}

fn and_(self: *Self, can_assign: bool) Error!void {
    _ = can_assign;
    const skip_rhs_loc = try self.emitJump(.JUMP_IF_FALSE);
    try self.emitOpCode(.POP);
    try self.parsePrecedence(.AND);
    self.patchJump(skip_rhs_loc);
}

fn or_(self: *Self, can_assign: bool) Error!void {
    _ = can_assign;
    const skip_rhs_loc = try self.emitJump(.JUMP_IF_TRUE);
    try self.emitOpCode(.POP);
    try self.parsePrecedence(.OR);
    self.patchJump(skip_rhs_loc);
}
