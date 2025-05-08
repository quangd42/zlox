const std = @import("std");
const Allocator = std.mem.Allocator;

const scanner = @import("scanner.zig");
const Scanner = scanner.Scanner;

pub const Compiler = struct {
    scanner: Scanner,
    allocator: Allocator,

    pub fn init(allocator: Allocator, source: []u8) Compiler {
        return .{
            .scanner = Scanner.init(source),
            .allocator = allocator,
        };
    }

    pub fn compile(self: *Compiler) void {
        var line: usize = 0;
        while (true) {
            const token = self.scanner.scanToken();
            if (token.line != line) {
                line = token.line;
                std.debug.print("{d:4} ", .{line});
            } else {
                std.debug.print("   | ", .{});
            }
            std.debug.print("{s} '{s}'\n", .{ @tagName(token.type), token.lexeme });

            if (token.type == .EOF) break;
        }
    }
};
