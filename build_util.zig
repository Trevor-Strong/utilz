const std = @import("std");
const Build = std.Build;
const Module = Build.Module;
const Step = Build.Step;
const LazyPath = Build.LazyPath;

const assert = std.debug.assert;

pub const ModuleAndTestOptions = struct {
    /// Name to expose the module as or null to create an anonymous module
    name: ?[]const u8 = null,
    import_self: union(enum) {
        no,
        yes,
        as: []const u8,
    } = .no,
    /// Module creation options
    module: Module.CreateOptions = .{},
    /// Test filters
    test_filters: []const []const u8 = &.{},
    /// Name of the test step
    test_step_name: []const u8 = "test",
};

pub fn addModuleAndTest(
    b: *Build,
    options: ModuleAndTestOptions,
) *Module {
    var mod: *Module = undefined;
    if (options.name) |name| {
        mod = b.addModule(name, options.module);
    } else {
        mod = b.createModule(options.module);
    }

    const test_step = Step.Compile.create(b, .{
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
            mod.addImport(name, mod);
            test_step.root_module.addImport(name, &test_step.root_module);
        },
    }

    const run_test_step = b.addRunArtifact(test_step);

    const test_top_level_step = b.step(options.test_step_name, "Run unit tests");
    test_top_level_step.dependOn(&run_test_step.step);
    return mod;
}

pub const AddFmtStepOptions = struct {
    description: []const u8 = "Format project source files",
    fmt: Step.Fmt.Options = .{},
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

    pub fn module(self: LazyDependency, name: []const u8) ?*Module {
        const dep = self.inner orelse return null;
        return dep.module(name);
    }

    pub fn artifact(self: LazyDependency, name: []const u8) ?*Step.Compile {
        const dep = self.inner orelse return null;
        return dep.artifact(name);
    }

    pub fn namedWriteFiles(self: LazyDependency, name: []const u8) ?*Step.WriteFile {
        const dep = self.inner orelse return null;
        return dep.namedWriteFiles(name);
    }

    pub fn path(self: LazyDependency, sub_path: []const u8) ?LazyPath {
        const dep = self.inner orelse return null;
        return dep.path(sub_path);
    }
};
