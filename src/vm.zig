const std = @import("std");
const Allocator = std.mem.Allocator;
const print = std.debug.print;
const testing = std.testing;

const TRACE_EXECUTION = @import("debug").@"trace-execution";

const _chunk = @import("chunk.zig");
const Chunk = _chunk.Chunk;
const OpCode = _chunk.OpCode;
const Obj = @import("obj.zig");
const Compiler = @import("compiler.zig");
const debug = @import("debug.zig");
const GC = @import("gc.zig").GC;
const Table = @import("table.zig").Table;
const Value = @import("value.zig").Value;

const FRAME_MAX = 64;
const STACK_MAX = FRAME_MAX * std.math.maxInt(u8);

pub const Error = error{ CompileError, RuntimeError, OutOfMemory };

const CallFrame = struct {
    closure: *Obj.Closure,
    ip: usize,
    start: usize,
};

pub const VM = struct {
    frames: std.ArrayList(CallFrame),
    stack: std.ArrayList(Value),
    objects: ?*Obj = null,
    open_upvalues: ?*Obj.Upvalue = null,
    init_string: *Obj.String,
    strings: Table,
    globals: Table,
    compiler: ?Compiler,
    gray_stack: std.ArrayList(*Obj),
    bytes_allocated: usize = 0,
    next_gc: usize = 1024 * 1024,
    gc: GC,
    allocator: Allocator,

    pub fn init(backing_allocator: Allocator) !*VM {
        var vm = try backing_allocator.create(VM);
        vm.* = .{
            // init memory manager
            .gc = GC.init(backing_allocator, vm),
            .gray_stack = std.ArrayList(*Obj).init(backing_allocator),
            // managed memory
            .allocator = vm.gc.allocator(),
            .compiler = null,
            .globals = Table.init(vm.allocator),
            .strings = Table.init(vm.allocator),
            .open_upvalues = null,
            .objects = null,
            .frames = std.ArrayList(CallFrame).init(vm.allocator),
            // pre allocate enough memory for the stack to avoid triggering gc
            // when using the stack for keeping other objects connected
            .stack = try std.ArrayList(Value).initCapacity(vm.allocator, 64),
            // init_string MUST be initialized after the stack
            .init_string = try .init(vm, "init"),
        };
        vm.defineNative("clock", clockNative) catch {
            @panic("failed to define native function.");
        };
        return vm;
    }

    pub fn deinit(vm: *VM) void {
        vm.strings.deinit();
        vm.globals.deinit();
        vm.stack.deinit();
        vm.frames.deinit();
        vm.gray_stack.deinit();
        var object = vm.objects;
        while (object) |obj| {
            const next = obj.next;
            obj.deinit(vm);
            object = next;
        }
        vm.gc.backing_allocator.destroy(vm);
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
            Error.CompileError => exit(65),
            Error.RuntimeError => exit(70),
            Error.OutOfMemory => exit(70),
        };
        exit(0);
    }

    fn interpret(self: *VM, source: []const u8) !void {
        self.compiler = try Compiler.init(self, source);
        const function = self.compiler.?.compile() catch return Error.CompileError;

        try self.push(.{ .Obj = &function.obj });
        const closure = try Obj.Closure.init(self, function);
        _ = self.pop();
        try self.push(.{ .Obj = &closure.obj });
        try self.call(closure, 0);

        return self.run();
    }

    fn run(self: *VM) !void {
        var frame = self.topFrame();
        while (frame.ip < frame.closure.function.chunk.code.items.len) {
            if (TRACE_EXECUTION) {
                print("          ", .{});
                for (self.stack.items) |slot| {
                    print("[ {} ]", .{slot});
                }
                print("\n", .{});
                _ = debug.disassembleInstruction(&frame.closure.function.chunk, frame.ip);
            }
            const instruction: _chunk.OpCode = @enumFromInt(self.readByte());
            switch (instruction) {
                .CONSTANT => try self.push(self.readConstant()),
                .NIL => try self.push(Value.Nil),
                .TRUE => try self.push(.{ .Bool = true }),
                .FALSE => try self.push(.{ .Bool = false }),
                .POP => _ = self.pop(),
                .GET_LOCAL => try self.push(self.frameSlotAt(frame, self.readByte()).*),
                .SET_LOCAL => self.frameSlotAt(frame, self.readByte()).* = self.peek(0),
                .GET_GLOBAL => {
                    const name = self.readString();
                    const val = self.globals.get(name) orelse {
                        return self.runtimeError("Undefined variable '{s}'.", .{name.chars});
                    };
                    try self.push(val);
                },
                .DEFINE_GLOBAL => {
                    const name = self.readString();
                    _ = try self.globals.set(name, self.peek(0));
                    _ = self.pop(); // see book for why
                },
                .SET_GLOBAL => {
                    const name = self.readString();
                    if (try self.globals.set(name, self.peek(0))) {
                        _ = self.globals.delete(name);
                        return self.runtimeError("Undefined variable '{s}'.", .{name.chars});
                    }
                },
                .GET_UPVALUE => {
                    const slot = self.readByte();
                    try self.push(frame.closure.upvalues.items[slot].location[0]);
                },
                .SET_UPVALUE => {
                    const slot = self.readByte();
                    frame.closure.upvalues.items[slot].location[0] = self.peek(0);
                },
                .GET_PROPERTY => {
                    const maybe_instance = self.peek(0);
                    const instance = maybe_instance.asObj(.Instance) orelse {
                        return self.runtimeError("Only instances have properties.", .{});
                    };
                    const name = self.readString();
                    const maybe_value = instance.fields.get(name);
                    if (maybe_value) |value| {
                        _ = self.pop(); // instance
                        try self.push(value);
                        continue;
                    }
                    try self.bindMethod(instance.class, name);
                },
                .SET_PROPERTY => {
                    const value = self.peek(0);
                    const maybe_instance = self.peek(1);
                    const instance = maybe_instance.asObj(.Instance) orelse {
                        return self.runtimeError("Only instances have fields.", .{});
                    };
                    const name = self.readString();
                    _ = try instance.fields.set(name, value);
                    _ = self.pop(); // value
                    _ = self.pop(); // instance
                    try self.push(value); // push value back
                },
                .GET_SUPER => {
                    const name = self.readString();
                    const superclass = self.pop().asObj(.Class).?;
                    try self.bindMethod(superclass, name);
                },
                .EQUAL => try self.equalOp(),
                .GREATER => try self.binaryOp(.GREATER),
                .GREATER_EQUAL => try self.binaryOp(.GREATER_EQUAL),
                .LESS => try self.binaryOp(.LESS),
                .LESS_EQUAL => try self.binaryOp(.LESS_EQUAL),
                .ADD => {
                    const rhs = self.peek(0);
                    const lhs = self.peek(1);
                    if (rhs.is(.Number) and lhs.is(.Number)) {
                        try self.binaryOp(.ADD);
                    } else if (rhs.isObj(.String) and lhs.isObj(.String)) {
                        try self.concatenate();
                    } else return self.runtimeError("Operands must be two numbers or two strings.", .{});
                },
                .SUBTRACT => try self.binaryOp(.SUBTRACT),
                .MULTIPLY => try self.binaryOp(.MULTIPLY),
                .DIVIDE => try self.binaryOp(.DIVIDE),
                .NOT => try self.push(.{ .Bool = self.pop().isFalsey() }),
                .NEGATE => try self.negateOp(),
                .PRINT => std.io.getStdOut().writer().print("{}\n", .{self.pop()}) catch {
                    return Error.RuntimeError;
                },
                .JUMP => {
                    const offset = self.readShort();
                    frame.ip += offset;
                },
                .JUMP_IF_TRUE => {
                    const offset = self.readShort();
                    if (!self.peek(0).isFalsey()) frame.ip += offset;
                },
                .JUMP_IF_FALSE => {
                    const offset = self.readShort();
                    if (self.peek(0).isFalsey()) frame.ip += offset;
                },
                .LOOP => {
                    const offset = self.readShort();
                    frame.ip -= offset;
                },
                .CALL => {
                    const arg_count = self.readByte();
                    try self.callValue(self.peek(arg_count), arg_count);
                    frame = self.topFrame();
                },
                .INVOKE => {
                    const method = self.readString();
                    const arg_count = self.readByte();
                    try self.invoke(method, arg_count);
                    frame = self.topFrame();
                },
                .SUPER_INVOKE => {
                    const method = self.readString();
                    const arg_count = self.readByte();
                    const superclass = self.pop().asObj(.Class).?;
                    try self.invokeFromClass(superclass, method, arg_count);
                    frame = self.topFrame();
                },
                .CLOSURE => {
                    const function = self.readConstant().asObj(.Function).?;
                    const closure = try Obj.Closure.init(self, function);
                    try self.push(.{ .Obj = &closure.obj });
                    // closure.upvalues was inited with the correct amount of upvalue slots
                    for (0..closure.upvalues.capacity) |_| {
                        const is_local = self.readByte();
                        const index = self.readByte();
                        if (is_local == 1) {
                            closure.upvalues.appendAssumeCapacity(try self.captureUpvalue(self.stack.items.ptr + frame.start + index));
                        } else {
                            closure.upvalues.appendAssumeCapacity(frame.closure.upvalues.items[index]);
                        }
                    }
                },
                .CLOSE_UPVALUE => {
                    self.closeUpvalues(self.stack.items.ptr + self.stack.items.len - 1);
                    _ = self.pop();
                },
                .RETURN => {
                    const result = self.pop();
                    self.closeUpvalues(self.stack.items.ptr + frame.start);
                    _ = self.frames.pop();
                    if (self.frames.items.len == 0) {
                        _ = self.pop();
                        return;
                    }

                    self.stack.shrinkRetainingCapacity(frame.start);
                    try self.push(result);
                    frame = self.topFrame();
                },
                .CLASS => {
                    const class = try Obj.Class.init(self, self.readString());
                    try self.push(.{ .Obj = &class.obj });
                },
                .INHERIT => {
                    const superclass = self.peek(1).asObj(.Class) orelse
                        return self.runtimeError("Superclass must be a class.", .{});
                    const subclass = self.peek(0).asObj(.Class).?;
                    try subclass.methods.addAll(&superclass.methods);
                    _ = self.pop(); // subclass
                },
                .METHOD => try self.defineMethod(self.readString()),
            }
        }
    }

    fn runtimeError(self: *VM, comptime fmt: []const u8, args: anytype) !void {
        print(fmt ++ "\n", args);
        var i = self.frames.items.len;
        while (i > 0) : (i -= 1) {
            const frame = self.frames.items[i - 1];
            const function = frame.closure.function;
            const instruction = frame.ip - 1;
            print("[line {d}] in ", .{function.chunk.lines.items[instruction]});
            if (function.name) |obj| {
                print("{s}()\n", .{obj.chars});
            } else {
                print("script\n", .{});
            }
        }
        self.resetState();
        return Error.RuntimeError;
    }

    inline fn readByte(self: *VM) u8 {
        const frame = self.topFrame();
        frame.ip += 1;
        return frame.closure.function.chunk.byteAt(frame.ip - 1);
    }

    inline fn readConstant(self: *VM) Value {
        const frame = &self.frames.getLast();
        const constant_idx = self.readByte();
        return frame.closure.function.chunk.constAt(constant_idx);
    }

    inline fn readShort(self: *VM) u16 {
        const byte1 = self.readByte();
        const byte2 = self.readByte();
        return @as(u16, byte1) << 8 | byte2;
    }

    inline fn readString(self: *VM) *Obj.String {
        const constant = self.readConstant();
        return constant.asObj(.String).?;
    }

    fn peek(self: *VM, distance: usize) Value {
        const len = self.stack.items.len;
        std.debug.assert(len > 0 and distance <= len - 1);
        return self.stack.items[len - 1 - distance];
    }

    pub fn pop(self: *VM) Value {
        std.debug.assert(self.stack.items.len > 0);
        return self.stack.pop().?;
    }

    pub fn push(self: *VM, val: Value) !void {
        if (self.stack.items.len == STACK_MAX) return error.OutOfMemory;
        return self.stack.append(val);
    }

    fn resetState(self: *VM) void {
        self.frames.clearRetainingCapacity();
        self.stack.clearRetainingCapacity();
    }

    inline fn topFrame(self: *VM) *CallFrame {
        return &self.frames.items[self.frames.items.len - 1];
    }

    inline fn frameSlotAt(self: *VM, frame: *CallFrame, slot: u8) *Value {
        return &self.stack.items[frame.start + slot];
    }

    fn negateOp(self: *VM) !void {
        switch (self.pop()) {
            .Number => |val| try self.push(.{ .Number = -val }),
            else => {
                return self.runtimeError("Operand must be a number.", .{});
            },
        }
    }

    fn binaryOp(self: *VM, comptime op: OpCode) !void {
        const b = self.pop().as(.Number) orelse return self.runtimeError("Operands must be numbers.", .{});
        const a = self.pop().as(.Number) orelse return self.runtimeError("Operands must be numbers.", .{});

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
        const b = self.pop();
        const a = self.pop();
        std.debug.assert(b.isObj(.String));
        std.debug.assert(a.isObj(.String));
        const b_str = b.asObj(.String) orelse return Error.RuntimeError;
        const a_str = a.asObj(.String) orelse return Error.RuntimeError;
        const out = try a_str.concat(self, b_str);
        try self.push(.{ .Obj = &out.obj });
    }

    fn equalOp(self: *VM) !void {
        const b = self.pop();
        const a = self.pop();
        try self.push(.{ .Bool = a.eql(b) });
    }

    fn defineNative(self: *VM, name: []const u8, function: Obj.NativeFn) !void {
        // this function will be called when setting up the vm. the GC won't collect until
        // the vm is fully initialized, so no need to dance around the GC here.
        const str = try Obj.String.init(self, name);
        const fun = try Obj.Native.init(self, function);
        _ = try self.globals.set(str, .{ .Obj = &fun.obj });
    }

    fn call(self: *VM, closure: *Obj.Closure, arg_count: u8) !void {
        if (arg_count != closure.function.arity) {
            return self.runtimeError("Expected {d} arguments but got {d}.", .{ closure.function.arity, arg_count });
        }

        if (self.frames.items.len == FRAME_MAX) {
            return self.runtimeError("Stack overflow.", .{});
        }
        try self.frames.append(.{
            .closure = closure,
            .ip = 0,
            .start = self.stack.items.len - arg_count - 1,
        });
    }

    fn callValue(self: *VM, callee: Value, arg_count: u8) !void {
        return switch (callee) {
            .Obj => |callee_obj| switch (callee_obj.type) {
                .BoundMethod => {
                    const bound = callee_obj.as(.BoundMethod);
                    const callee_idx = self.stack.items.len - arg_count - 1;
                    self.stack.items[callee_idx] = bound.receiver;
                    try self.call(bound.method, arg_count);
                },
                .Class => {
                    const class = callee_obj.as(.Class);
                    const instance: *Obj.Instance = try .init(self, class);
                    const callee_idx = self.stack.items.len - arg_count - 1; // index of the callee Value on the stack
                    self.stack.items[callee_idx] = .{ .Obj = &instance.obj };
                    const maybe_init = class.methods.get(self.init_string);
                    if (maybe_init) |init_val| {
                        try self.call(init_val.Obj.as(.Closure), arg_count);
                    } else if (arg_count != 0)
                        return self.runtimeError("Expected 0 arguments but got {d}.", .{arg_count});
                },
                .Closure => self.call(callee_obj.as(.Closure), arg_count),
                .Native => {
                    const native = callee_obj.as(.Native).function;
                    const first_arg_idx = self.stack.items.len - arg_count; // index of the first arg Value on the stack
                    const result = native(arg_count, self.stack.items.ptr + first_arg_idx);
                    self.stack.shrinkRetainingCapacity(first_arg_idx - 1);
                    try self.push(result);
                },
                else => self.runtimeError("Can only call functions and classes.", .{}),
            },
            else => self.runtimeError("Can only call functions and classes.", .{}),
        };
    }

    fn invoke(self: *VM, name: *Obj.String, arg_count: u8) !void {
        const receiver = self.peek(arg_count);
        const instance = receiver.asObj(.Instance) orelse
            return self.runtimeError("Only instances have methods.", .{});
        const maybe_value = instance.fields.get(name);
        if (maybe_value) |value| {
            const callee_idx = self.stack.items.len - arg_count - 1;
            self.stack.items[callee_idx] = value;
            return self.callValue(value, arg_count);
        }
        try self.invokeFromClass(instance.class, name, arg_count);
    }

    fn invokeFromClass(self: *VM, class: *Obj.Class, name: *Obj.String, arg_count: u8) !void {
        const method = class.methods.get(name) orelse
            return self.runtimeError("Undefined property '{s}'.", .{name.chars});
        return self.call(method.asObj(.Closure).?, arg_count);
    }

    fn captureUpvalue(self: *VM, local: [*]Value) !*Obj.Upvalue {
        var prev: ?*Obj.Upvalue = null;
        var cur: ?*Obj.Upvalue = self.open_upvalues;
        while (cur) |value| {
            if (value.location - local > 0) {
                prev = value;
                cur = value.next;
                continue;
            }
            if (value.location == local) return value;
            break;
        }
        var createdUpvalue = try Obj.Upvalue.init(self, local);
        createdUpvalue.next = cur;
        if (prev) |p| {
            p.next = createdUpvalue;
        } else {
            self.open_upvalues = createdUpvalue;
        }
        return createdUpvalue;
    }

    fn closeUpvalues(self: *VM, last: [*]Value) void {
        while (self.open_upvalues) |upvalue| : (self.open_upvalues = upvalue.next) {
            if (upvalue.location - last < 0) break;
            upvalue.closed = upvalue.location[0];
            upvalue.location = @ptrCast(&upvalue.closed);
        }
    }

    fn defineMethod(self: *VM, name: *Obj.String) !void {
        const method = self.peek(0);
        const class = self.peek(1).asObj(.Class).?;
        _ = try class.methods.set(name, method);
        _ = self.pop();
    }

    fn bindMethod(self: *VM, class: *Obj.Class, name: *Obj.String) !void {
        const method = class.methods.get(name) orelse
            return self.runtimeError("Undefined property '{s}'.", .{name.chars});
        const bound = try Obj.BoundMethod.init(self, self.peek(0), method.asObj(.Closure).?);
        _ = self.pop();
        try self.push(.{ .Obj = &bound.obj });
    }
};

test "vm deinit" {
    var vm = try VM.init(testing.allocator);
    defer vm.deinit(); // Expect this to free all string allocations
    const String = @import("obj.zig").String;

    const original_str: []const u8 = "Hello ";
    const a_str = try String.init(vm, original_str);
    const b_str = try String.init(vm, "world!");
    const out = try a_str.concat(vm, b_str);
    try testing.expectEqualSlices(u8, "Hello world!", out.chars);
}

fn clockNative(arg_count: usize, args: [*]const Value) Value {
    _ = arg_count;
    _ = args;
    return .{ .Number = @floatFromInt(std.time.timestamp()) };
}
