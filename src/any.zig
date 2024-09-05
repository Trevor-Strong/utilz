const std = @import("std");
const utilz = @import("util.zig");

const AnyName = @TypeOf(.literal);

pub fn Struct(comptime T: type) type {
    const object_info = switch (@typeInfo(T)) {
        .Struct => |s_info| s_info,
        else => @compileError("Expected struct type, found '" ++ @typeName(T) ++ "'"),
    };

    return struct {
        const Self = @This();

        pub inline fn hasFn(comptime name: AnyName) bool {
            return @hasField(Fn, @tagName(name));
        }

        pub inline fn hasMethod(comptime name: AnyName) bool {
            return @hasField(Method, @tagName(name));
        }

        pub inline fn hasField(comptime field_name: AnyName) bool {
            return @hasField(T, @tagName(field_name));
        }

        pub inline fn FieldType(comptime field: Field) type {
            return object_info.fields[@intFromEnum(field)].type;
        }

        pub inline fn fieldType(comptime field_name: AnyName) ?type {
            if (!hasField(field_name)) return null;
            return FieldType(field_name);
        }

        /// Enum of the fields of `T`
        pub const Field = std.meta.FieldEnum(T);

        /// Enum of the function declarations of `T`
        pub const Fn = Fn: {
            var enum_fields: [fn_infos.len]std.builtin.Type.EnumField = undefined;
            for (fn_infos, 0..) |func, i| {
                enum_fields[i] = .{
                    .name = func.name ++ "",
                    .value = i,
                };
            }
            const enum_fields_const = enum_fields;
            break :Fn @Type(.{
                .Enum = .{
                    .tag_type = std.math.IntFittingRange(0, fn_infos.len),
                    .fields = &enum_fields_const,
                    .decls = &.{},
                    .is_exhaustive = true,
                },
            });
        };

        pub const Method = Method: {
            var enum_fields: [fn_infos.len]std.builtin.Type.EnumField = undefined;
            var i = 0;
            for (fn_infos, 0..) |func, info_index| {
                if (func.is_method) {
                    enum_fields[i] = .{
                        .name = func.name ++ "",
                        .value = info_index,
                    };
                    i += 1;
                }
            }
            const enum_fields_const = enum_fields[0..i].*;
            break :Method @Type(.{
                .Enum = .{
                    .tag_type = std.math.IntFittingRange(0, fn_infos.len),
                    .fields = &enum_fields_const,
                    .decls = &.{},
                    .is_exhaustive = true,
                },
            });
        };

        pub const fn_infos: []const FnDecl = fn_infos: {
            const decls = object_info.decls;
            var infos: [decls.len]FnDecl = undefined;
            var i = 0;
            for (decls) |decl| {
                const F = @TypeOf(@field(T, decl.name));
                switch (@typeInfo(F)) {
                    .Fn => |fn_info| {
                        infos[i] = .{
                            .name = decl.name,
                            .type = F,
                            .cc = fn_info.calling_convention,
                            .is_var_args = fn_info.is_var_args,
                            .return_type = fn_info.return_type,
                            .params = fn_info.params,
                            .is_method = (fn_info.params.len > 0 and fn_info.params[0].type == null) or
                                utilz.meta.isMethodInfo(T, fn_info),
                        };
                        i += 1;
                    },
                    else => {},
                }
            }
            const infos_const = infos[0..i].*;
            break :fn_infos &infos_const;
        };
    };
}

pub const FnDecl = struct {
    name: []const u8,
    type: type,
    cc: std.builtin.CallingConvention,
    is_var_args: bool,
    return_type: ?type,
    params: []const std.builtin.Type.Fn.Param,
    is_method: bool,
};

test {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(Struct(std.mem.Allocator));
}
