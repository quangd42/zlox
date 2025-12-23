// adapted from https://github.com/jwmerrill/zig-lox/blob/main/util/test.zig

const std = @import("std");
const process = std.process;
const Allocator = std.mem.Allocator;

const ignore_paths = [_][]const u8{
    "benchmark",
    "benchmark2",
    "scanning",
    "expressions",
    // "class",
    // "constructor",
    // "field",
    // "inheritance",
    // "method",
    // "super",
    // "this",
    // "method",
    // "this",
    // "class",
    // "394",
    // "object",
    // "inherit",
    "for/closure_in_body.lox",
    "limit/no_reuse_constants.lox",
};

pub fn main() !void {
    var dba = std.heap.DebugAllocator(.{}).init;
    defer {
        const deinit_status = dba.deinit();
        std.debug.assert(deinit_status != .leak);
    }
    var arena = std.heap.ArenaAllocator.init(dba.allocator());
    defer arena.deinit();
    const gpa = arena.allocator();

    const args = try std.process.argsAlloc(gpa);
    if (args.len < 2) std.debug.panic("Missing interpreter path arg", .{});
    const lox_path = args[1];

    var npassed: usize = 0;
    var nfailed: usize = 0;

    var test_list = std.ArrayList([]const u8).init(gpa);

    // Determine which files to test
    if (args.len > 2) {
        for (args[2..]) |path| try test_list.append(path);
    } else {
        var dir = try std.fs.cwd().openDir("test", .{ .iterate = true });
        defer dir.close();
        var walker = try dir.walk(gpa);
        defer walker.deinit();
        find_script: while (try walker.next()) |entry| {
            switch (entry.kind) {
                .file => {
                    for (ignore_paths) |p| {
                        if (std.mem.containsAtLeast(u8, entry.path, 1, p)) continue :find_script;
                    }
                    if (std.mem.endsWith(u8, entry.path, ".lox")) {
                        try test_list.append(try std.fs.path.join(gpa, &[_][]const u8{ "test", entry.path }));
                    }
                },
                else => {},
            }
        }
    }

    for (test_list.items) |path| {
        if (try run_test(gpa, lox_path, path)) {
            npassed += 1;
        } else {
            nfailed += 1;
        }
    }

    std.debug.print("Summary: {d} passed, {d} failed\n", .{ npassed, nfailed });
    if (nfailed > 0) std.process.exit(1);
}

fn run_test(allocator: Allocator, lox_path: []const u8, test_path: []const u8) !bool {
    const argv = [_][]const u8{ lox_path, test_path };
    const result = try std.process.Child.run(.{ .allocator = allocator, .argv = argv[0..] });
    const expected = try parse_test_file(allocator, test_path);
    const passed = (validate_compile_error(result.stderr, expected.compile_error_message) and
        validate_runtime_error(result.stderr, expected.runtime_error_message) and
        validate_output(result.stdout, expected.output) and
        validate_exit_code(result.term.Exited, expected.exit_code));
    if (!passed) std.debug.print("in {s}\n---\n\n", .{test_path});
    return passed;
}

fn validate_exit_code(actual: u32, expected: u32) bool {
    if (actual == expected) return true;
    std.debug.print("Incorrect exit code\nActual: {}\nExpected: {}\n", .{ actual, expected });
    return false;
}

fn validate_output(actual: []const u8, expected: []const u8) bool {
    if (std.mem.eql(u8, actual, expected)) return true;
    std.debug.print("Output differs:\nActual:\n{s}\n\nExpected:\n{s}\n\n", .{ actual, expected });
    return false;
}

fn validate_runtime_error(actual: []const u8, expected: []const u8) bool {
    if (expected.len == 0) return true;
    if (std.mem.indexOf(u8, actual, expected) != null) return true;
    std.debug.print("Missing expected runtime error:\nActual:\n{s}\n\nExpected:\n{s} ...\n\n", .{ actual, expected });
    return false;
}

fn validate_compile_error(actual: []const u8, expected: []const u8) bool {
    if (expected.len == 0) return true;
    if (std.mem.eql(u8, actual, expected)) return true;
    std.debug.print("Missing expected compile error:\nActual:\n{s}\n\nExpected:\n{s}\n\n", .{ actual, expected });
    return false;
}

const Expected = struct {
    output: []const u8,
    compile_error_message: []const u8,
    runtime_error_message: []const u8,
    exit_code: u32,
};

fn matches(source: []const u8, needle: []const u8) bool {
    return std.mem.eql(u8, source[0..@min(needle.len, source.len)], needle);
}

fn parse_test_file(allocator: Allocator, test_path: []const u8) !Expected {
    const source = try std.fs.cwd().readFileAlloc(allocator, test_path, 1_000_000);

    var output_buffer = std.ArrayList(u8).init(allocator);
    var compile_error_buffer = std.ArrayList(u8).init(allocator);
    var runtime_error_buffer = std.ArrayList(u8).init(allocator);

    const expect_prefix = "// expect: ";
    const error_prefix = "// Error";
    const line_error_prefix = "// [line ";
    const c_line_error_prefix = "// [c line ";
    const runtime_error_prefix = "// expect runtime error: ";

    var exit_code: u32 = 0;
    var line: usize = 1;
    var i: usize = 0;
    while (i < source.len) : (i += 1) {
        if (matches(source[i..], expect_prefix)) {
            i += expect_prefix.len;
            const j = std.mem.indexOfScalarPos(u8, source, i, '\n') orelse source.len;
            try output_buffer.appendSlice(source[i..j]);
            try output_buffer.append('\n');
            i = j;
        } else if (matches(source[i..], error_prefix)) {
            exit_code = 65;
            i += error_prefix.len;
            const j = std.mem.indexOfScalarPos(u8, source, i, '\n') orelse source.len;
            try compile_error_buffer.writer().print("[line {}] Error", .{line});
            try compile_error_buffer.appendSlice(source[i..j]);
            try compile_error_buffer.append('\n');
            i = j;
        } else if (matches(source[i..], line_error_prefix)) {
            exit_code = 65;
            i += line_error_prefix.len;
            const j = std.mem.indexOfScalarPos(u8, source, i, '\n') orelse source.len;
            try compile_error_buffer.appendSlice("[line ");
            try compile_error_buffer.appendSlice(source[i..j]);
            try compile_error_buffer.append('\n');
            i = j;
        } else if (matches(source[i..], c_line_error_prefix)) {
            exit_code = 65;
            i += c_line_error_prefix.len;
            const j = std.mem.indexOfScalarPos(u8, source, i, '\n') orelse source.len;
            try compile_error_buffer.appendSlice("[line ");
            try compile_error_buffer.appendSlice(source[i..j]);
            try compile_error_buffer.append('\n');
            i = j;
        } else if (matches(source[i..], runtime_error_prefix)) {
            exit_code = 70;
            i += runtime_error_prefix.len;
            const j = std.mem.indexOfScalarPos(u8, source, i, '\n') orelse source.len;
            try runtime_error_buffer.appendSlice(source[i..j]);
            try runtime_error_buffer.writer().print("\n[line {}]", .{line});
            i = j;
        }

        if (i < source.len and source[i] == '\n') line += 1;
    }

    return Expected{
        .output = try output_buffer.toOwnedSlice(),
        .runtime_error_message = try runtime_error_buffer.toOwnedSlice(),
        .compile_error_message = try compile_error_buffer.toOwnedSlice(),
        .exit_code = exit_code,
    };
}
