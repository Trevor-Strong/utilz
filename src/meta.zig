const std = @import("std");
const assert = std.debug.assert;

/// If `T` is a pointer, or optional pointer, returns the pointer child type,
/// otherwise returns `T`
pub fn Object(comptime T: type) type {
    return switch (@typeInfo(T)) {
        .Pointer => |ptr_info| ptr_info.child,
        .Optional => |opt_info| switch (@typeInfo(opt_info.child)) {
            .Pointer => |ptr_info| ptr_info.child,
            else => T,
        },
        else => T,
    };
}

/// If `T` is an optional type `?U`, return `U`; otherwise, return `T`.
pub fn Required(comptime T: type) type {
    return switch (@typeInfo(T)) {
        .Optional => |opt_info| opt_info.child,
        else => T,
    };
}

pub fn isComptimeOnly(comptime T: type) bool {
    switch (@typeInfo(T)) {
        .Type,
        .ComptimeInt,
        .ComptimeFloat,
        .Fn,
        .EnumLiteral,
        => return true,
        .Struct => |s_info| for (s_info.fields) |f| {
            if (!f.is_comptime and isComptimeOnly(f.type)) return true;
        } else return false,
        .Union => |u_info| for (u_info.fields) |f| {
            if (isComptimeOnly(f.type)) return true;
        } else return false,
        inline .Vector,
        .Array,
        .Optional,
        => |info| return isComptimeOnly(info.child),
        .ErrorUnion => |eu_info| return isComptimeOnly(eu_info.payload),
        else => return false,
    }
}
