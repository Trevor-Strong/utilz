const std = @import("std");
const utilz = @import("utilz");

pub fn isTuple(comptime T: type) bool {
    return switch (@typeInfo(T)) {
        .Struct => |s_info| s_info.is_tuple,
        else => false,
    };
}

pub fn Concat(comptime A: type, comptime B: type) type {
    if (!isTuple(A) or !isTuple(B)) {
        @compileError("Expect tuple types instead found '" ++ @typeName(A) ++
            "' and '" ++ @typeName(B) ++ "'");
    }
}

pub fn Prepend(comptime T: type, comptime Tuple: type) type {
    comptime {
        if (!isTuple(Tuple)) @compileError("Expected tuple type, found '" ++ @typeName(Tuple) ++ "'");
        const og_info = @typeInfo(Tuple).Struct;
        var fields: [og_info.fields.len + 1]std.builtin.Type.StructField = undefined;
        fields[0] = .{
            .name = "0",
            .type = T,
            .default_value = null,
            .is_comptime = false,
            .alignment = @alignOf(T),
        };
        const print = std.fmt.comptimePrint;
        for (og_info.fields, 1..) |f, i| {
            f[i] = .{
                .name = print("{d}", .{i}),
                .type = f.type,
                .default_value = f.default_value,
                .is_comptime = f.is_comptime,
                .alignment = f.alignment,
            };
        }
        const fields_const = fields;
        return @Type(.{
            .Struct = .{
                .layout = .auto,
                .backing_integer = null,
                .fields = &fields_const,
                .decls = &.{},
                .is_tuple = true,
            },
        });
    }
}

pub fn prepend(value: anytype, tuple: anytype) Prepend(@TypeOf(value), @TypeOf(tuple)) {
    const Result = Prepend(@TypeOf(value), @TypeOf(tuple));
    var result: Result = undefined;
    result[0] = value;
    inline for (tuple, 1..) |item, i| {
        result[i] = item;
    }
    return result;
}
