const std = @import("std");
const Allocator = std.mem.Allocator;
const testing = std.testing;

const dbg = @import("config").@"debug-trace";

const _chunk = @import("chunk.zig");
const Chunk = _chunk.Chunk;
const Compiler = @import("compiler.zig").Compiler;
const debug = @import("debug.zig");
const value = @import("value.zig");
const Value = value.Value;

pub const InterpretError = error{
    COMPILE_ERROR,
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
                try self.interpret(content);
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
            InterpretError.COMPILE_ERROR => exit(65),
            InterpretError.RUNTIME_ERROR => exit(70),
        };
        exit(0);
    }

    fn interpret(self: *VM, source: []const u8) InterpretError!void {
        var chunk = Chunk.init(self.allocator);
        defer chunk.deinit();

        var compiler = Compiler.init(self.allocator, source, &chunk);
        compiler.compile() catch return InterpretError.COMPILE_ERROR;

        self.chunk = &chunk;
        self.ip = 0;

        return self.run();
    }

    // TODO:
    fn run(self: *VM) InterpretError!void {
        _ = self;
        return;
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
