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
