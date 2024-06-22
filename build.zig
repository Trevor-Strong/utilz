const std = @import("std");

const Build = std.Build;

pub fn build(b: *Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const pic = b.option(bool, "pic", "Generate Position Independent Code");
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
        .pic = pic,
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
        .pic = pic,
    });
    unit_tests.root_module.addImport("utilz", &unit_tests.root_module);

    const run_unit_tests = b.addRunArtifact(unit_tests);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests.step);
}
