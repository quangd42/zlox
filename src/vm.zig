const std = @import("std");
const Allocator = std.mem.Allocator;
const testing = std.testing;

const dbg = @import("config").@"debug-trace";

const chunk = @import("chunk.zig");
const Chunk = chunk.Chunk;
const Compiler = @import("compiler.zig").Compiler;
const debug = @import("debug.zig");
const value = @import("value.zig");
const Value = value.Value;

pub const InterpretResult = enum {
    OK,
    COMPTIME_ERROR,
    RUNTIME_ERROR,
};

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
                _ = try self.interpret(content);
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

        const res = try self.interpret(source);
        switch (res) {
            .OK => exit(0),
            .COMPTIME_ERROR => exit(65),
            .RUNTIME_ERROR => exit(70),
        }
    }

    fn interpret(self: *VM, source: []u8) !InterpretResult {
        self.chunk = undefined;
        var compiler = Compiler.init(self.allocator, source);
        compiler.compile();

        // while (self.ip < self.chunk.code.items.len) {
        //     if (dbg) {
        //         std.debug.print("          ", .{});
        //         for (self.stack.items) |*slot| {
        //             std.debug.print("[ {d:3.3} ]", .{slot.*});
        //         }
        //         std.debug.print("\n", .{});
        //         _ = debug.disassembleInstruction(self.chunk, self.ip);
        //     }
        //     const instruction: chunk.OpCode = @enumFromInt(self.readByte());
        //     switch (instruction) {
        //         .OP_CONSTANT => try self.stack.append(self.readConstant()),
        //         .OP_ADD => try self.binaryOp(.add),
        //         .OP_SUBTRACT => try self.binaryOp(.subtract),
        //         .OP_MULTIPLY => try self.binaryOp(.multiply),
        //         .OP_DIVIDE => try self.binaryOp(.divide),
        //         .OP_NEGATE => try self.stack.append(-self.stack.pop().?),
        //         .OP_RETURN => {
        //             std.debug.print("{d}\n", .{self.stack.pop().?});
        //             return .OK;
        //         },
        //         // missing OP_CONSTANT_LONG
        //         else => return .COMPTIME_ERROR,
        //     }
        // }
        // WARNING: Implicit OK return
        return .OK;
    }

    fn readByte(self: *VM) u8 {
        defer self.ip += 1;
        return self.chunk.code.items[self.ip];
    }

    fn readConstant(self: *VM) value.Value {
        const constant_idx = self.readByte();
        return self.chunk.constants.items[constant_idx];
    }

    fn binaryOp(self: *VM, comptime op: enum { add, subtract, multiply, divide }) !void {
        const b = self.stack.pop().?;
        const a = self.stack.pop().?;

        try self.stack.append(switch (op) {
            .add => a + b,
            .subtract => a - b,
            .multiply => a * b,
            .divide => a / b,
        });
    }
};
