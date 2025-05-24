const std = @import("std");
const Allocator = std.mem.Allocator;
const testing = std.testing;

const dbg = @import("builtin").mode == .Debug;

const _chunk = @import("chunk.zig");
const Chunk = _chunk.Chunk;
const OpCode = _chunk.OpCode;
const _obj = @import("obj.zig");
const Obj = _obj.Obj;
const ObjString = _obj.String;
const ObjFunction = _obj.Function;
const Compiler = @import("compiler.zig");
const debug = @import("debug.zig");
const Table = @import("table.zig").Table;
const Value = @import("value.zig").Value;

const FRAME_MAX = 64;
const STACK_MAX = FRAME_MAX * std.math.maxInt(u8);

pub const InterpretError = error{
    CompileError,
    RuntimeError,
} || Allocator.Error;

const CallFrame = struct {
    function: *ObjFunction,
    ip: usize,
    slots: []Value,
};

pub const VM = struct {
    frames: std.ArrayList(CallFrame),
    stack: std.ArrayList(Value),
    chunk: *Chunk = undefined,
    objects: ?*Obj = null,
    strings: Table,
    globals: Table,
    allocator: Allocator,

    pub fn init(allocator: Allocator) VM {
        return VM{
            .frames = std.ArrayList(CallFrame).init(allocator),
            .stack = std.ArrayList(Value).init(allocator),
            .strings = Table.init(allocator),
            .globals = Table.init(allocator),
            .allocator = allocator,
        };
    }
    pub fn deinit(vm: *VM) void {
        vm.strings.deinit();
        vm.globals.deinit();
        var object = vm.objects;
        while (object) |obj| {
            const next = obj.next;
            obj.deinit(vm);
            object = next;
        }
    }

    pub fn repl(self: *VM) !void {
        const stdin = std.io.getStdIn().reader();
        const stdout = std.io.getStdOut().writer();

        try stdout.print("zlox 0.1.0\n", .{});

        var buffer: [1024]u8 = undefined;
        while (true) {
            try stdout.writeAll("> ");
            const input = try stdin.readUntilDelimiterOrEof(&buffer, '\n');
            if (input) |line| {
                self.interpret(line) catch continue;
            } else {
                break;
            }
        }
    }

    pub fn runFile(self: *VM, file_path: []const u8) !void {
        const stderr = std.io.getStdErr().writer();
        const exit = std.process.exit;

        const file = std.fs.cwd().openFile(file_path, .{}) catch |err| {
            try stderr.print("Could not open file {s}: {s}\n", .{ file_path, @errorName(err) });
            exit(74);
        };
        defer file.close();

        const source = file.readToEndAlloc(self.allocator, 10 * 1024 * 1024) catch |err| {
            try stderr.print("Could not read file {s}: {s}\n", .{ file_path, @errorName(err) });
            exit(74);
        };
        defer self.allocator.free(source);

        self.interpret(source) catch |err| switch (err) {
            InterpretError.CompileError => exit(65),
            InterpretError.RuntimeError => exit(70),
            InterpretError.OutOfMemory => exit(70),
        };
        exit(0);
    }

    fn interpret(self: *VM, source: []const u8) !void {
        self.resetStack();

        var compiler = try Compiler.init(self, source);
        const function = compiler.compile() catch return InterpretError.CompileError;

        try self.push(.{ .Obj = &function.obj });
        try self.frames.append(.{
            .function = function,
            .ip = 0,
            .slots = self.stack.items,
        });

        return self.run();
    }

    fn run(self: *VM) !void {
        const frame = &self.frames.items[self.frames.items.len - 1];
        while (frame.ip < frame.function.chunk.code.items.len) {
            if (dbg) {
                std.debug.print("          ", .{});
                for (self.stack.items) |slot| {
                    std.debug.print("[ {} ]", .{slot});
                }
                std.debug.print("\n", .{});
                _ = debug.disassembleInstruction(&frame.function.chunk, frame.ip);
            }
            const instruction: _chunk.OpCode = @enumFromInt(self.readByte());
            switch (instruction) {
                .CONSTANT => try self.push(self.readConstant()),
                .NIL => try self.push(Value.Nil),
                .TRUE => try self.push(.{ .Bool = true }),
                .FALSE => try self.push(.{ .Bool = false }),
                .POP => _ = self.pop(),
                .GET_LOCAL => try self.push(frame.slots[self.readByte()]),
                .SET_LOCAL => frame.slots[self.readByte()] = self.peek(0).?,
                .GET_GLOBAL => {
                    const name = self.readString();
                    const val = self.globals.get(name) orelse {
                        self.runtimeError("Undefined variable '{s}'.", .{name.chars});
                        return InterpretError.RuntimeError;
                    };
                    try self.push(val);
                },
                .DEFINE_GLOBAL => {
                    const name = self.readString();
                    _ = try self.globals.set(name, self.peek(0).?);
                    _ = self.pop(); // see book for why
                },
                .SET_GLOBAL => {
                    const name = self.readString();
                    if (try self.globals.set(name, self.peek(0).?)) {
                        _ = self.globals.delete(name);
                        self.runtimeError("Undefined variable '{s}'.", .{name.chars});
                        return InterpretError.RuntimeError;
                    }
                },
                .EQUAL => try self.equalOp(),
                .GREATER => try self.binaryOp(.GREATER),
                .GREATER_EQUAL => try self.binaryOp(.GREATER_EQUAL),
                .LESS => try self.binaryOp(.LESS),
                .LESS_EQUAL => try self.binaryOp(.LESS_EQUAL),
                .ADD => {
                    const rhs = self.peek(0).?;
                    const lhs = self.peek(1).?;
                    if (rhs.is(.Number) and lhs.is(.Number)) {
                        try self.binaryOp(.ADD);
                    } else {
                        errdefer self.runtimeError("Operands must be strings or numbers.", .{});
                        if (rhs.isObj(.String) and lhs.isObj(.String)) {
                            try self.concatenate();
                        } else return error.RuntimeError;
                    }
                },
                .SUBTRACT => try self.binaryOp(.SUBTRACT),
                .MULTIPLY => try self.binaryOp(.MULTIPLY),
                .DIVIDE => try self.binaryOp(.DIVIDE),
                .NOT => try self.push(.{ .Bool = self.pop().?.isFalsey() }),
                .NEGATE => try self.negateOp(),
                .PRINT => std.io.getStdOut().writer().print("{?}\n", .{self.pop()}) catch {
                    return InterpretError.RuntimeError;
                },
                .JUMP => {
                    const offset = self.readShort();
                    frame.ip += offset;
                },
                .JUMP_IF_TRUE => {
                    const offset = self.readShort();
                    if (!self.peek(0).?.isFalsey()) frame.ip += offset;
                },
                .JUMP_IF_FALSE => {
                    const offset = self.readShort();
                    if (self.peek(0).?.isFalsey()) frame.ip += offset;
                },
                .LOOP => {
                    const offset = self.readShort();
                    frame.ip -= offset;
                },
                .RETURN => {
                    // std.debug.print("{}\n", .{self.pop().?});
                    return;
                },
            }
        }
    }

    fn runtimeError(self: *VM, comptime fmt: []const u8, args: anytype) void {
        const frame = &self.frames.items[self.frames.items.len - 1];
        std.debug.print(fmt, args);
        std.debug.print("\n[line {d}] in script\n", .{frame.function.chunk.lines.items[frame.ip - 1]});
        self.resetStack();
    }

    inline fn readByte(self: *VM) u8 {
        const frame = &self.frames.items[self.frames.items.len - 1];
        frame.ip += 1;
        return frame.function.chunk.getByteAt(frame.ip - 1) catch unreachable;
    }

    inline fn readConstant(self: *VM) Value {
        const frame = &self.frames.getLast();
        const constant_idx = self.readByte();
        return frame.function.chunk.getConstantAt(constant_idx) catch unreachable;
    }

    inline fn readShort(self: *VM) u16 {
        const byte1 = self.readByte();
        const byte2 = self.readByte();
        return @as(u16, byte1) << 8 | byte2;
    }

    inline fn readString(self: *VM) *ObjString {
        const constant = self.readConstant();
        return constant.asObj(.String).?;
    }

    fn peek(self: *VM, distance: usize) ?Value {
        const len = self.stack.items.len;
        if (len == 0 or distance > len - 1) return null;
        return self.stack.items[len - 1 - distance];
    }

    fn pop(self: *VM) ?Value {
        return self.stack.pop();
    }

    fn push(self: *VM, val: Value) !void {
        if (self.stack.items.len == STACK_MAX) return error.OutOfMemory;
        return self.stack.append(val);
    }

    fn resetStack(self: *VM) void {
        self.stack.clearRetainingCapacity();
    }

    fn negateOp(self: *VM) !void {
        switch (self.pop().?) {
            .Number => |val| try self.push(.{ .Number = -val }),
            else => {
                self.runtimeError("Operand must be a number.", .{});
                return InterpretError.RuntimeError;
            },
        }
    }

    fn binaryOp(self: *VM, comptime op: OpCode) !void {
        const b = self.pop().?.as(.Number) orelse return InterpretError.RuntimeError;
        const a = self.pop().?.as(.Number) orelse return InterpretError.RuntimeError;
        errdefer self.runtimeError("Operand must be numbers.", .{});

        try self.push(switch (op) {
            .ADD => .{ .Number = a + b },
            .SUBTRACT => .{ .Number = a - b },
            .MULTIPLY => .{ .Number = a * b },
            .DIVIDE => .{ .Number = a / b },
            .GREATER => .{ .Bool = a > b },
            .GREATER_EQUAL => .{ .Bool = a >= b },
            .LESS => .{ .Bool = a < b },
            .LESS_EQUAL => .{ .Bool = a <= b },
            else => unreachable,
        });
    }

    fn concatenate(self: *VM) !void {
        const b = self.pop().?;
        const a = self.pop().?;
        std.debug.assert(b.isObj(.String));
        std.debug.assert(a.isObj(.String));
        const b_str = b.asObj(.String) orelse return InterpretError.RuntimeError;
        const a_str = a.asObj(.String) orelse return InterpretError.RuntimeError;
        const out = try a_str.concat(self, b_str);
        try self.push(.{ .Obj = &out.obj });
    }

    fn equalOp(self: *VM) !void {
        const b = self.pop().?;
        const a = self.pop().?;
        try self.push(.{ .Bool = a.eql(b) });
    }
};

test "vm deinit" {
    var vm = VM.init(testing.allocator);
    defer vm.deinit(); // Expect this to free all string allocations
    const String = @import("obj.zig").String;

    const original_str: []const u8 = "Hello ";
    const a_str = try String.init(&vm, original_str);
    const b_str = try String.init(&vm, "world!");
    const out = try a_str.concat(&vm, b_str);
    try testing.expectEqualSlices(u8, "Hello world!", out.chars);
}
