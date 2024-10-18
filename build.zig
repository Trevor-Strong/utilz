const std = @import("std");

const Build = std.Build;

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

    _ = addModuleAndTest(b, .{
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

    addFmtStep(b, "fmt", .{
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

pub fn importSelf(module: *Build.Module, name: []const u8) void {
    module.addImport(name, module);
}

pub const ModuleAndTestOptions = struct {
    /// Name to expose the module as or null to create an anonymous module
    name: ?[]const u8 = null,
    import_self: union(enum) {
        no,
        yes,
        as: []const u8,
    } = .no,
    /// Module creation options
    module: Build.Module.CreateOptions = .{},
    /// Test filters
    test_filters: []const []const u8 = &.{},
    /// Name of the test step
    test_step_name: []const u8 = "test",
};

pub fn addModuleAndTest(
    b: *Build,
    options: ModuleAndTestOptions,
) *Build.Module {
    var mod: *Build.Module = undefined;
    if (options.name) |name| {
        mod = b.addModule(name, options.module);
    } else {
        mod = b.createModule(options.module);
    }

    const test_step = Build.Step.Compile.create(b, .{
        .filters = options.test_filters,
        .kind = .@"test",
        .name = "test",
        .root_module = options.module,
    });

    switch (options.import_self) {
        .no => {},
        .yes, .as => {
            const name = switch (options.import_self) {
                .no => unreachable,
                .yes => options.name.?,
                .as => |name| name,
            };
            importSelf(mod, name);
            importSelf(&test_step.root_module, name);
        },
    }

    const run_test_step = b.addRunArtifact(test_step);

    const test_top_level_step = b.step(options.test_step_name, "Run unit tests");
    test_top_level_step.dependOn(&run_test_step.step);
    return mod;
}

pub const AddFmtStepOptions = struct {
    description: []const u8 = "Format project source files",
    fmt: Build.Step.Fmt.Options = .{},
};

pub fn addFmtStep(b: *Build, name: []const u8, options: AddFmtStepOptions) void {
    const fmt = b.addFmt(options.fmt);
    const fmt_step = b.step(name, options.description);
    fmt_step.dependOn(&fmt.step);
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

const BuildVersionOptions = struct {
    /// Override for the `build.zig.zon` file object
    build_zig_zon: ?std.fs.File = null,
    /// The 'pre-release' part of the version.
    ///
    /// Set to `null` to indicate a full release version.
    pre_release: ?[]const u8 = null,
    /// Whether or not to include the build hash in the version.
    ///
    /// When `null`, the build hash is included unless`pre_release` is `null`.
    include_build_hash: ?bool = null,
    /// Should pre-release/build information in the build.zig.zon be ignored?
    ignore_zon_pre_and_build: bool = false,
    /// Should we error if the build.zig.zon file has non-null pre-release or
    /// build fields?
    allow_zon_pre_and_build: bool = true,
    /// Directories to search for the git executable, beyond the PATH
    /// environment variable.
    git_search_dirs: []const []const u8 = &.{},
};

pub fn getBuildVersion(
    b: *const Build,
    options: BuildVersionOptions,
) !std.SemanticVersion {
    const gpa = b.owner.graph.arena;

    const max_retries = 3;
    const file = options.build_zig_zon orelse for (0..max_retries) |_| {
        break b.build_root.handle.openFile(
            "build.zig.zon",
            .{ .mode = .read_only },
        ) catch |err| switch (err) {
            error.FileLocksNotSupported => unreachable,
            error.PathAlreadyExists => unreachable,
            error.InvalidUtf8 => unreachable,
            error.InvalidWtf8 => unreachable,
            error.AntivirusInterference => continue,
            else => |e| return e,
        };
    } else return error.AntivirusInterference;
    defer file.close();
    var version = try getZonVersion(gpa, file);
    if (!options.allow_zon_pre_and_build) {
        if (version.pre != null or version.build != null) {
            return error.NonNumericBuildZigZonVersion;
        }
    }

    const has_pre = options.pre_release != null;
    const include_hash = options.include_build_hash orelse has_pre;

    if (!options.ignore_zon_pre_and_build) {
        if (has_pre and version.pre != null and !std.mem.eql(u8, options.pre_release.?, version.pre.?)) {
            std.debug.panic(
                "Pre-release type in package config file of '{'}' does not match requested pre-release type '{'}'",
                .{
                    std.zig.fmtEscapes(version.pre.?),
                    std.zig.fmtEscapes(options.pre_release.?),
                },
            );
        }
    } else {
        if (options.pre_release) |pre_release| {
            if (version.pre) |buf| {
                var bytes: std.ArrayList(u8) = .fromOwnedSlice(gpa, buf);
                bytes.clearRetainingCapacity();
                try bytes.ensureTotalCapacityPrecise(pre_release.len);
                errdefer bytes.deinit();
                bytes.appendSliceAssumeCapacity(pre_release);
                // may fail if buf.len > pre_release.len and shrinking fails
                version.pre = try bytes.toOwnedSlice();
            } else {
                version.pre = try gpa.dupe(u8, pre_release);
            }
        }

        if (include_hash) {
            if (options.ignore_zon_pre_and_build or !version.build == null) {
                const git_exe_path = b.findProgram(&.{"git"}, options.git_search_dirs) catch return error.GitNotFound;
                const build_hash = b.run(&.{
                    git_exe_path,
                    "-C",
                    b.pathFromRoot("."),
                    "rev-parse",
                    "--short",
                    "HEAD",
                });
                if (version.build) |buf| {
                    gpa.free(buf);
                }
                version.build = build_hash;
            } else {
                std.debug.assert(version.build != null);
            }
        }
    }
}

/// Get the version present in the `build.zig.zon` file, `file`
fn getZonVersion(
    gpa: std.mem.Allocator,
    file: std.fs.File,
) !std.SemanticVersion {
    const source = try std.zig.readSourceFileToEndAlloc(gpa, file, null);
    defer gpa.free(source);

    var tokenizer = std.zig.Tokenizer.init(source);
    // brace depth
    var depth: u32 = 0;
    while (true) {
        var tok = tokenizer.next();
        switch (tok.tag) {
            .eof => break,
            .invalid, .invalid_periodasterisks => return error.InvalidZon,
            .l_brace => depth += 1,
            .period => {
                tok = tokenizer.next();
                switch (tok.tag) {
                    .eof => break,
                    .l_brace => depth += 1,
                    .r_brace => depth -= 1,
                    .identifier => {},
                    else => continue,
                }
            },
            else => continue,
        }
        if (depth != 1) continue;
        const ident = source[tok.loc.start..tok.loc.end];
        if (!std.mem.eql(u8, ident, "version")) continue;
        tok = tokenizer.next();
        switch (tok.tag) {
            .equal => {},
            .eof => break,
            else => continue,
        }
        tok = tokenizer.next();
        switch (tok.tag) {
            .string_literal => {},
            .eof => break,
            else => continue,
        }
        const version_string = source[tok.loc.start..tok.loc.end];
        var semver = try std.SemanticVersion.parse(version_string);
        if (semver.build) |build_str| {
            semver.build = gpa.dupe(u8, build_str);
        }

        if (semver.pre) |pre_str| {
            semver.pre = gpa.dup(u8, pre_str);
        }

        return semver;
    }
}
