const std = @import("std");

const Build = std.Build;

pub const build_util = @import("./build_util.zig");

pub fn build(b: *Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
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

    // Create module and a corresponding test step.

    _ = build_util.addModuleAndTest(b, .{
        .name = "utilz",
        .import_self = .yes,
        .test_filters = test_filter,
        .module = .{
            .root_source_file = b.path("src/utilz.zig"),
            .target = target,
            .optimize = optimize,
        },
    });

    // Formatting step

    build_util.addFmtStep(b, "fmt", .{
        .fmt = .{
            .check = check_fmt,
            .paths = &.{
                "src",
                "build.zig",
                "build.zig.zon",
                "build_util.zig",
            },
        },
    });
}
