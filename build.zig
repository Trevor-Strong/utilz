const std = @import("std");
const builtin = @import("builtin");
const Build = std.Build;

pub fn build(b: *Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const single_threaded = b.option(bool, "single_threaded", "Build in single threaded mode");
    const strip = b.option(bool, "strip", "Strip (or don't strip) debug information");
    const omit_frame_pointer = b.option(bool, "omit_frame_pointer", "Omit saving the frame pointer");

    const test_filter: []const []const u8 = b.option(
        []const []const u8,
        "test-filter",
        "Skip tests that do not match the filter",
    ) orelse &[_][]const u8{};

    const check_fmt = b.option(
        bool,
        "check_fmt",
        "Check the formatting of files instead of fixing it",
    ) orelse false;

    const utilz = b.addModule("utilz", .{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
        .single_threaded = single_threaded,
        .strip = strip,
        .omit_frame_pointer = omit_frame_pointer,
    });
    utilz.addImport("utilz", utilz);

    // Create Tests

    const unit_tests = b.addTest(.{
        .root_module = utilz,
        .filters = test_filter,
    });
    // unit_tests.root_module.addImport("utilz", unit_tests.root_module);
    const run_tests = b.addRunArtifact(unit_tests);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_tests.step);

    // Formatting step

    const fmt = b.addFmt(.{
        .check = check_fmt,
        .paths = &.{
            "src",
            "build.zig",
            "build.zig.zon",
        },
    });

    const fmt_step = b.step("fmt", "Format source files");
    fmt_step.dependOn(&fmt.step);
}
