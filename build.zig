const std = @import("std");

const Build = std.Build;

pub fn build(b: *Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const build_test_only = b.option(
        bool,
        "test-compile",
        "Only build the tests, don't run them",
    ) orelse false;

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

    const root_source_file = b.path("src/util.zig");

    const utilz = b.addModule("utilz", .{
        .target = target,
        .optimize = optimize,
        .root_source_file = root_source_file,
    });
    utilz.addImport("utilz", utilz);

    // Formatting

    const fmt = b.addFmt(.{
        .paths = &.{ "src", "build.zig", "build.zig.zon" },
        .check = check_fmt,
    });

    const fmt_step = b.step("fmt", "'zig fmt' the source files");
    fmt_step.dependOn(&fmt.step);

    // Test

    const unit_tests = b.addTest(.{
        .root_source_file = root_source_file,
        .target = target,
        .optimize = optimize,
        .filters = test_filter,
    });
    unit_tests.root_module.addImport("utilz", &unit_tests.root_module);

    const test_step: *Build.Step = if (build_test_only)
        &unit_tests.step
    else
        &b.addRunArtifact(unit_tests).step;
    const test_tls = b.step("test", "Run unit tests");
    test_tls.dependOn(test_step);
}
