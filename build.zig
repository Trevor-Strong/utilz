const std = @import("std");
const builtin = @import("builtin");
const Build = std.Build;

pub fn build(b: *Build) void {
    const target = targetOption(b, .{});
    const optimize = optimizeOption(b, .{
        .default_release_mode = .ReleaseSafe,
    });
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
        .root_source_file = b.path("src/utilz.zig"),
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

pub const TargetOptionConfig = struct {
    default: ?std.Target.Query = .{},
    whitelist: []const std.Target.Query = &.{},
};

pub fn targetOption(b: *Build, config: TargetOptionConfig) ?Build.ResolvedTarget {
    const query = parseTargetQuery(b, config) orelse return null;
    return b.resolveTargetQuery(query);
}

fn parseTargetQuery(b: *Build, config: TargetOptionConfig) ?std.Target.Query {
    const maybe_triple = b.option(
        []const u8,
        "target",
        "The CPU architecture, OS, and ABI to build for",
    );
    const mcpu = b.option(
        []const u8,
        "cpu",
        "Target CPU features to add or subtract",
    );
    const dynamic_linker = b.option(
        []const u8,
        "dynamic-linker",
        "Path to interpreter on the target system",
    );

    if (maybe_triple == null and mcpu == null and dynamic_linker == null)
        return config.default;

    const triple = maybe_triple orelse "native";

    const target = Build.parseTargetQuery(.{
        .arch_os_abi = triple,
        .cpu_features = mcpu,
        .dynamic_linker = dynamic_linker,
    }) catch |err| switch (err) {
        error.ParseFailed => {
            b.invalid_user_input = true;
            return config.default;
        },
    };

    for (config.whitelist) |q| {
        if (q.eql(target)) return target;
    }

    for (config.whitelist) |q| {
        std.log.info("Allowed target: -Dtarget={} -Dcpu={cpu}", .{
            fmtTargetQuery(q),
            fmtTargetQuery(q),
        });
    }

    std.log.err("Chosen target '{}' does not match any of the allowed targets.", .{
        fmtTargetQuery(target),
    });
    b.invalid_user_input = true;
    return config.default;
}

pub const OptimizeOptionConfig = struct {
    /// Optimization level when `release_mode` is `.auto` and `-Doptimize`
    /// is not set.
    default_release_mode: ?std.builtin.OptimizeMode = null,
};
pub fn optimizeOption(
    b: *Build,
    config: OptimizeOptionConfig,
) ?std.builtin.OptimizeMode {
    const optimize = b.option(
        std.builtin.OptimizeMode,
        "optimize",
        "Optimization mode to build with. Prioritize safety, speed, or binary size",
    );
    var is_release = false;
    if (config.default_release_mode != null) {
        is_release = b.option(bool, "release", "Build in the default release mode") orelse false;
    }

    if (optimize) |mode| return mode;

    if (is_release) {
        return config.default_release_mode.?;
    }

    return switch (b.release_mode) {
        .off => null,
        .any => config.default_release_mode orelse @panic("No default release mode"),
        .safe => .ReleaseSafe,
        .fast => .ReleaseFast,
        .small => .ReleaseSmall,
    };
}

pub fn lazyDep(b: *Build, dep_name: []const u8, dep_options: anytype) LazyDependency {
    return .{
        .inner = b.lazyDependency(dep_name, dep_options),
    };
}

pub const LazyDependency = struct {
    inner: ?*Build.Dependency,

    pub fn builder(self: LazyDependency) ?*Build {
        const dep = self.inner orelse return null;
        return dep.builder;
    }

    pub fn module(self: LazyDependency, name: []const u8) ?*Build.Module {
        const dep = self.inner orelse return null;
        return dep.module(name);
    }

    pub fn artifact(self: LazyDependency, name: []const u8) ?*Build.Step.Compile {
        const dep = self.inner orelse return null;
        return dep.artifact(name);
    }

    pub fn namedWriteFiles(self: LazyDependency, name: []const u8) ?*Build.Step.WriteFile {
        const dep = self.inner orelse return null;
        return dep.namedWriteFiles(name);
    }

    pub fn path(self: LazyDependency, sub_path: []const u8) ?Build.LazyPath {
        const dep = self.inner orelse return null;
        return dep.path(sub_path);
    }
};

pub fn fmtTarget(target: std.Target) std.fmt.Formatter(formatTargetQuery) {
    return fmtTargetQuery(std.Target.Query.fromTarget(target));
}

pub fn fmtTargetQuery(query: std.Target.Query) std.fmt.Formatter(formatTargetQuery) {
    return .{ .data = query };
}

pub fn formatTargetQuery(
    query: std.Target.Query,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    _ = options;
    if (fmt.len == 0) {
        if (query.isNativeTriple()) return writer.writeAll("native");
        const arch = if (query.cpu_arch) |arch| @tagName(arch) else "native";
        const os = if (query.os_tag) |os| @tagName(os) else "native";

        try writer.print("{s}-{s}", .{ arch, os });

        if (query.os_version_min) |min| {
            if (min != .none) {
                try writer.writeAll(".");
                try formatOsVersion(min, writer);
            }
        }
        if (query.os_version_max) |max| {
            if (max != .none) {
                try writer.writeAll("...");
                try formatOsVersion(max, writer);
            }
        }

        if (query.glibc_version) |v| {
            const abi = if (query.abi) |abi| @tagName(abi) else "gnu";
            try writer.print("-{s}.", .{abi});
            try formatSemver(v, writer);
        } else if (query.android_api_level) |lvl| {
            const abi = if (query.abi) |abi| @tagName(abi) else "android";
            try writer.print("-{s}.{d}", .{ abi, lvl });
        } else if (query.abi) |abi| {
            try writer.print("-{s}", .{@tagName(abi)});
        }
    } else if (comptime std.mem.eql(u8, fmt, "cpu")) {
        switch (query.cpu_model) {
            .native => {
                try writer.writeAll("native");
            },
            .baseline => {
                try writer.writeAll("baseline");
            },
            .determined_by_arch_os => {
                if (query.cpu_arch == null) {
                    try writer.writeAll("native");
                } else {
                    try writer.writeAll("baseline");
                }
            },
            .explicit => |model| {
                try writer.writeAll(model.name);
            },
        }

        if (query.cpu_features_add.isEmpty() and query.cpu_features_sub.isEmpty()) {
            // The CPU name alone is sufficient.
            return;
        }

        const cpu_arch = query.cpu_arch orelse builtin.cpu.arch;
        const all_features = cpu_arch.allFeaturesList();

        for (all_features, 0..) |feature, i_usize| {
            const i: std.Target.Cpu.Feature.Set.Index = @intCast(i_usize);

            if (query.cpu_features_sub.isEnabled(i)) {
                try writer.writeAll("-");
            } else if (query.cpu_features_add.isEnabled(i)) {
                try writer.writeAll("+");
            } else {
                continue;
            }
            try writer.writeAll(feature.name);
        }
    }
}

fn formatSemver(semver: std.SemanticVersion, writer: anytype) !void {
    try writer.print("{d}.{d}", .{ semver.major, semver.minor });
    if (semver.patch != 0) {
        try writer.print(".{d}", .{semver.patch});
    }
}

fn formatOsVersion(osv: std.Target.Query.OsVersion, writer: anytype) !void {
    switch (osv) {
        .none => {},
        .semver => |v| {
            try formatSemver(v, writer);
        },
        .windows => |v| {
            if (std.enums.tagName(std.Target.Os.WindowsVersion, v)) |name| {
                try writer.writeAll(name);
            } else {
                try std.fmt.formatInt(@intFromEnum(v), 10, .lower, .{}, writer);
            }
        },
    }
}
