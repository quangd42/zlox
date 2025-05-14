const std = @import("std");
const Allocator = std.mem.Allocator;
const testing = std.testing;

const dbg = @import("config").@"debug-trace";

const _chunk = @import("chunk.zig");
const Chunk = _chunk.Chunk;
const OpCode = _chunk.OpCode;
const Compiler = @import("compiler.zig").Compiler;
const debug = @import("debug.zig");
const value = @import("value.zig");
const Value = value.Value;

pub const InterpretError = error{
    CompileError,
    RuntimeError,
} || Allocator.Error;

pub const VM = struct {
    chunk: *Chunk = undefined,
    ip: usize = 0,
    stack: std.ArrayList(Value),
    allocator: Allocator,

    pub fn init(allocator: Allocator) VM {
        return VM{
            .stack = std.ArrayList(Value).init(allocator),
            .allocator = allocator,
        };
    }
    pub fn deinit(self: *VM) void {
        _ = self;
    }

    pub fn repl(self: *VM) !void {
        const stdin = std.io.getStdIn().reader();
        const stdout = std.io.getStdOut().writer();

        try stdout.print("zlox 0.1.0\n", .{});

        var buffer: [1024]u8 = undefined;
        while (true) {
            try stdout.writeAll("> ");
            const input = try stdin.readUntilDelimiterOrEof(&buffer, '\n');
            if (input) |content| {
                self.interpret(content) catch continue;
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

        var chunk = Chunk.init(self.allocator);
        defer chunk.deinit();

        var compiler = Compiler.init(self.allocator, source, &chunk);
        compiler.compile() catch return InterpretError.CompileError;

        self.chunk = &chunk;
        self.ip = 0;

        return self.run();
    }

    fn run(self: *VM) !void {
        while (self.ip < self.chunk.code.items.len) {
            if (dbg) {
                std.debug.print("          ", .{});
                for (self.stack.items) |slot| {
                    std.debug.print("[ {} ]", .{slot});
                }
                std.debug.print("\n", .{});
                _ = debug.disassembleInstruction(self.chunk, self.ip);
            }
            const instruction: _chunk.OpCode = @enumFromInt(self.readByte());
            switch (instruction) {
                .CONSTANT => try self.push(self.readConstant()),
                .CONSTANT_LONG => try self.push(self.readConstantLong()),
                .NIL => try self.push(Value.Nil),
                .TRUE => try self.push(.{ .Bool = true }),
                .FALSE => try self.push(.{ .Bool = false }),
                .EQUAL => try self.equalOp(),
                .GREATER => try self.binaryOp(.GREATER),
                .GREATER_EQUAL => try self.binaryOp(.GREATER_EQUAL),
                .LESS => try self.binaryOp(.LESS),
                .LESS_EQUAL => try self.binaryOp(.LESS_EQUAL),
                .ADD => try self.binaryOp(.ADD),
                .SUBTRACT => try self.binaryOp(.SUBTRACT),
                .MULTIPLY => try self.binaryOp(.MULTIPLY),
                .DIVIDE => try self.binaryOp(.DIVIDE),
                .NOT => try self.push(.{ .Bool = self.pop().?.isFalsey() }),
                .NEGATE => try self.negateOp(),
                .RETURN => {
                    std.debug.print("{}\n", .{self.pop().?});
                    return;
                },
            }
        }
    }

    fn runtimeError(self: *VM, comptime fmt: []const u8, args: anytype) void {
        std.debug.print(fmt, args);
        std.debug.print("\n[line {d}] in script\n", .{self.chunk.lines.items[self.ip - 1]});
        self.resetStack();
    }

    fn readByte(self: *VM) u8 {
        defer self.ip += 1;
        return self.chunk.getByteAt(self.ip) catch unreachable;
    }

    fn readConstant(self: *VM) value.Value {
        const constant_idx = self.readByte();
        return self.chunk.getConstantAt(constant_idx) catch unreachable;
    }

    fn readConstantLong(self: *VM) value.Value {
        defer self.ip += 3;
        const byte1: u24 = self.chunk.getByteAt(self.ip) catch unreachable;
        const byte2: u24 = self.chunk.getByteAt(self.ip + 1) catch unreachable;
        const byte3: u24 = self.chunk.getByteAt(self.ip + 2) catch unreachable;
        const idx: u24 = byte1 | @as(u24, byte2) << 8 | @as(u24, byte3) << 16;
        return self.chunk.getConstantAt(idx) catch unreachable;
    }

    fn peek(self: *VM, distance: usize) Value {
        const len = self.stack.items.len;
        const idx = if (distance > len - 1) 0 else len - 1 - distance;
        return self.stack.items[idx];
    }

    fn pop(self: *VM) ?Value {
        return self.stack.pop();
    }

    fn push(self: *VM, val: Value) !void {
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
        const b = self.pop().?.asNumber() orelse return InterpretError.RuntimeError;
        const a = self.pop().?.asNumber() orelse return InterpretError.RuntimeError;

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

    fn equalOp(self: *VM) !void {
        const b = self.pop().?;
        const a = self.pop().?;
        try self.push(.{ .Bool = a.equal(b) });
    }
};
