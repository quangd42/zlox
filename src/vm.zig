const std = @import("std");
const Allocator = std.mem.Allocator;
const testing = std.testing;

const dbg = @import("config").@"debug-trace";

const _chunk = @import("chunk.zig");
const Chunk = _chunk.Chunk;
const OpCode = _chunk.OpCode;
const Compiler = @import("compiler.zig").Compiler;
const debug = @import("debug.zig");
const Obj = @import("obj.zig").Obj;
const Table = @import("table.zig").Table;
const Value = @import("value.zig").Value;

pub const InterpretError = error{
    CompileError,
    RuntimeError,
} || Allocator.Error;

pub const VM = struct {
    ip: usize = 0,
    stack: std.ArrayList(Value),
    chunk: *Chunk = undefined,
    objects: ?*Obj = null,
    strings: Table,
    allocator: Allocator,

    pub fn init(allocator: Allocator) VM {
        return VM{
            .stack = std.ArrayList(Value).init(allocator),
            .strings = Table.init(allocator),
            .allocator = allocator,
        };
    }
    pub fn deinit(vm: *VM) void {
        vm.strings.deinit();
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

        var chunk = try Chunk.init(self.allocator);
        defer chunk.deinit();

        var compiler = Compiler.init(self, source, &chunk);
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
                .NIL => try self.push(Value.Nil),
                .TRUE => try self.push(.{ .Bool = true }),
                .FALSE => try self.push(.{ .Bool = false }),
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
        return self.chunk.getByteAt(self.ip);
    }

    fn readConstant(self: *VM) Value {
        const constant_idx = self.readByte();
        return self.chunk.getConstantAt(constant_idx);
    }

    // fn readConstantLong(self: *VM) Value {
    //     defer self.ip += 3;
    //     const byte1: u24 = self.chunk.getByteAt(self.ip);
    //     const byte2: u24 = self.chunk.getByteAt(self.ip + 1);
    //     const byte3: u24 = self.chunk.getByteAt(self.ip + 2);
    //     const idx: u24 = byte1 | @as(u24, byte2) << 8 | @as(u24, byte3) << 16;
    //     return self.chunk.getConstantAt(idx);
    // }
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
