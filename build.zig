const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    // To pass options from the build script and into the project’s Zig code, use the Options step.
    const debug_options = b.addOptions();
    debug_options.addOption(bool, "trace-execution", b.option(bool, "trace-execution", "Stack trace execution") orelse false);
    debug_options.addOption(bool, "stress-gc", b.option(bool, "stress-gc", "Stressing garbage collection") orelse false);
    debug_options.addOption(bool, "log-gc", b.option(bool, "log-gc", "Logging garbage collection") orelse false);
    debug_options.addOption(bool, "nan-boxing", b.option(bool, "nan-boxing", "NaN boxing Value") orelse false);

    // All executables using exe_mod will have these options
    exe_mod.addOptions("debug", debug_options);

    const exe = b.addExecutable(.{
        .name = "zlox",
        .root_module = exe_mod,
    });

    b.installArtifact(exe);

    // This *creates* a Run step in the build graph
    const run_cmd = b.addRunArtifact(exe);

    // By making the run step depend on the install step, it will be run from the
    // installation directory rather than directly from within the cache directory.
    run_cmd.step.dependOn(b.getInstallStep());

    // This allows the user to pass arguments to the application in the build
    // command itself, like this: `zig build run -- arg1 arg2 etc`
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    // This creates a build step. It will be visible in the `zig build --help` menu,
    // and can be selected like this: `zig build run`
    // This will evaluate the `run` step rather than the default, which is "install".
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    // This is where build-on-save check step begins.
    // As you can see we are re-defining the same executable but
    // we're binding it to a dedicated build step.
    const exe_check = b.addExecutable(.{
        .name = "zlox",
        .root_module = exe_mod,
    });
    // There is no `b.installArtifact(exe_check);` here.

    // Finally we add the "check" step which will be detected
    // by ZLS and automatically enable Build-On-Save.
    const check = b.step("check", "Check if zlox compiles");
    check.dependOn(&exe_check.step);

    // Creates a step for unit testing. This only builds the test executable
    // but does not run it.
    const unit_tests = b.addTest(.{
        .root_module = exe_mod,
    });

    // Again, this *creates* the Run step...
    const run_exe_unit_tests = b.addRunArtifact(unit_tests);

    // ... and this exposes a `test` step to the `zig build --help` menu
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_exe_unit_tests.step);
}
