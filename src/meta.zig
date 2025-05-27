const std = @import("std");
const utilz = @import("utilz");
const assert = std.debug.assert;

pub const ptr = @import("meta/ptr.zig");

pub fn UInt(comptime bits: u16) type {
    return std.meta.Int(.unsigned, bits);
}

pub fn SInt(comptime bits: u16) type {
    return std.meta.Int(.signed, bits);
}

pub inline fn toInt(value: anytype) IntRepr(@TypeOf(value)) {
    switch (@typeInfo(@TypeOf(value))) {
        .int => return value,
        .bool => return @intFromBool(value),
        .float => return @bitCast(value),
        .@"enum" => return @intFromEnum(value),
        .@"struct" => |struct_info| switch (struct_info.layout) {
            .@"packed" => return @bitCast(value),
            .@"extern" => return toInt(@field(value, struct_info.fields[0].name)),
        },
        .@"union" => |union_info| switch (union_info.layout) {
            .@"packed" => return @bitCast(value),
            else => unreachable,
        },
        .optional, .pointer => return @intFromPtr(value),
        else => unreachable,
    }
}

pub inline fn fromInt(comptime T: type, value: IntRepr(T)) T {
    switch (@typeInfo(@TypeOf(value))) {
        .int => return value,
        .bool => return value == 1,
        .float => return @bitCast(value),
        .@"enum" => return @enumFromInt(value),
        .@"struct" => |struct_info| switch (struct_info.layout) {
            .auto => unreachable,
            .@"packed" => return @bitCast(value),
            .@"extern" => return @as(
                *align(@alignOf(IntRepr(T))) const T,
                @ptrCast(&value),
            ),
        },
        .@"union" => |union_info| switch (union_info.layout) {
            .@"packed" => return @bitCast(value),
            else => unreachable,
        },
        .optional, .pointer => return @ptrFromInt(value),
        else => unreachable,
    }
}

pub fn getIntRepr(comptime T: type) ?type {
    switch (@typeInfo(T)) {
        .int => return T,
        .float => |float_info| return UInt(float_info.bits),
        .pointer => |ptr_info| switch (ptr_info.size) {
            .slice => return null,
            .c, .one, .many => return usize,
        },
        .@"enum" => |enum_info| enum_info.tag_type,
        .@"struct" => |struct_info| switch (struct_info.layout) {
            .auto => return null,
            .@"packed" => return struct_info.backing_integer.?,
            .@"extern" => if (struct_info.fields.len == 1)
                return struct_info.fields[0].type
            else
                return null,
        },
        .@"union" => |union_info| switch (union_info.layout) {
            .auto, .@"extern" => return null,
            .@"packed" => return UInt(@bitSizeOf(T)),
        },
        .optional => |opt_info| switch (@typeInfo(opt_info.child)) {
            .pointer => |ptr_info| switch (ptr_info.size) {
                .slice, .c => return null,
                .one, .many => if (ptr_info.is_allowzero)
                    return null
                else
                    return usize,
            },
            else => return null,
        },
        .error_set => return UInt(@bitSizeOf(anyerror)),
        .null => return u0,
        else => return null,
    }
}

pub fn IntRepr(comptime T: type) type {
    return getIntRepr(T).?;
}

pub fn hasIntRepr(comptime T: type) bool {
    return getIntRepr(T) != null;
}

pub fn isContainer(comptime T: type) bool {
    return isContainerTag(@typeInfo(T));
}

pub inline fn isContainerTag(type_id: std.builtin.TypeId) bool {
    return switch (type_id) {
        .@"enum", .@"union", .@"struct", .@"opaque" => true,
        else => false,
    };
}

/// If `T` is a pointer, or optional pointer, returns the pointer child type,
/// otherwise returns `T`
pub fn Object(comptime T: type) type {
    return switch (@typeInfo(T)) {
        .pointer => |ptr_info| ptr_info.child,
        .optional => |opt_info| switch (@typeInfo(opt_info.child)) {
            .pointer => |ptr_info| ptr_info.child,
            else => T,
        },
        else => T,
    };
}

pub fn SingleObject(comptime T: type) type {
    const ptr_info = switch (@typeInfo(T)) {
        .pointer => |ptr_info| ptr_info,
        .optional => |opt_info| switch (@typeInfo(opt_info.child)) {
            .pointer => |ptr_info| ptr_info,
            else => return T,
        },
        else => return T,
    };

    return switch (ptr_info.size) {
        .one => ptr_info.child,
        else => T,
    };
}

/// If `T` is an optional type `?U`, return `U`; otherwise, return `T`.
pub fn Required(comptime T: type) type {
    return switch (@typeInfo(T)) {
        .optional => |opt_info| opt_info.child,
        else => T,
    };
}

pub inline fn hasPadding(comptime T: type) bool {
    switch (@typeInfo(T)) {
        .int, .float => return @sizeOf(T) * 8 != @bitSizeOf(T),
        .@"enum" => |enum_info| return hasPadding(enum_info.tag_type),
        .array => |array_info| return (array_info.len > 0 or
            array_info.sentinel_ptr != null) and hasPadding(array_info.child),
        .optional => |opt_info| switch (@typeInfo(opt_info.child)) {
            .pointer => |ptr_info| return ptr_info.is_allowzero or
                ptr_info.size == .c,
            else => return true,
        },
        .@"struct" => |struct_info| {
            if (struct_info.layout == .@"packed") return false;
            const size = @sizeOf(T);
            var bitsize = 0;
            for (struct_info.fields) |f| {
                if (!f.is_comptime) {
                    if (hasPadding(f.type)) return true;
                    bitsize += @bitSizeOf(f.type);
                }
            }
            return size * 8 != bitsize;
        },
        .@"union" => |union_info| switch (union_info.layout) {
            .auto, .@"extern" => {
                var max_size = 0;
                var max_align = 0;

                var max_has_padding = true;
                for (union_info.fields) |f| {
                    const size = @sizeOf(f.type);
                    const alignment = @alignOf(f.type);
                    max_align = @max(max_align, alignment);
                    if (max_size == size and max_has_padding) {
                        max_has_padding = hasPadding(f.type);
                    } else if (max_size < size) {
                        max_size = size;
                        max_has_padding = hasPadding(f.type);
                    }
                }
                if (max_has_padding) return true;
                if (!std.mem.isAligned(max_size, max_align)) return true;
                if (max_align < @alignOf(T)) return true;

                if (union_info.tag_type) |Tag| {
                    if (!std.mem.isAligned(@sizeOf(Tag), max_align))
                        return true;
                    if (!std.mem.isAligned(@sizeOf(Tag) + max_size, max_align))
                        return true;
                    if (@sizeOf(Tag) + max_size != @sizeOf(T)) return true;
                }
            },
            .@"packed" => return false,
        },
        .bool,
        .null,
        .type,
        .void,
        .@"fn",
        .vector,
        .pointer,
        .noreturn,
        .error_set,
        .@"opaque",
        .undefined,
        .comptime_int,
        .enum_literal,
        .comptime_float,
        => return false,
        .frame,
        .error_union,
        .@"anyframe",
        => return true,
    }
}

/// `true` if `T` is an integer type, including `comptime_int`
pub inline fn isInt(comptime T: type) bool {
    return T == comptime_int or @typeInfo(T) == .int;
}

/// `true` if `T` is an float type, including `comptime_float`
pub inline fn isFloat(comptime T: type) bool {
    return T == comptime_float or @typeInfo(T) == .float;
}

pub fn isComptimeOnly(comptime T: type) bool {
    return isComptimeOnlyInfo(@typeInfo(T));
}

pub fn isComptimeOnlyInfo(comptime type_info: std.builtin.Type) bool {
    switch (type_info) {
        .type,
        .comptime_int,
        .comptime_float,
        .@"fn",
        .enum_literal,
        => return true,
        .@"struct" => |s_info| for (s_info.fields) |f| {
            if (!f.is_comptime and isComptimeOnly(f.type)) return true;
        } else return false,
        .@"union" => |u_info| for (u_info.fields) |f| {
            if (isComptimeOnly(f.type)) return true;
        } else return false,
        inline .vector,
        .array,
        .optional,
        => |info| return isComptimeOnly(info.child),
        .error_union => |eu_info| return isComptimeOnly(eu_info.payload),
        else => return false,
    }
}

pub fn isMultiValueZst(comptime T: type) bool {
    return isMultiValueZstInfo(@typeInfo(T));
}

/// Determines if `T` is a Zero Size Type (ZST) that can represent multiple
/// values (`undefined` isn't a valid value of `T`). All of these types are also
/// `comptime` only types.
pub fn isMultiValueZstInfo(comptime type_info: std.builtin.Type) bool {
    return switch (@typeInfo(type_info)) {
        .@"fn",
        .type,
        .comptime_int,
        .comptime_float,
        .enum_literal,
        => true,
        inline .vector, .array => |info| info.len == 0 or
            isMultiValueZst(info.child),
        .@"struct" => |s_info| for (s_info.fields) |f| {
            if (!f.is_comptime and isMultiValueZst(f.type)) break true;
        } else false,
        .@"union" => |u_info| for (u_info.fields) |f| {
            if (isMultiValueZst(f.type)) break true;
        } else false,
        .error_union => |eu_info| isMultiValueZst(eu_info.payload),
        else => false,
    };
}

/// `true` if `T` is `noreturn` like in that it has *no* possible values;
/// otherwise, `false`.
///
/// This function returns true for
/// - `noreturn`
/// - `opaque` types
/// - `error{}` and equivalent inferred function error sets
/// - Exhaustive `enum`s with 0 fields
/// - `union`s with 0 fields
/// - `struct`s with at least one field that is `noreturn`-like
/// - Arrays and vectors of a `noreturn`-like type
///
/// See also: `isNoReturnLikeInfo`, `isZst`
pub fn isNoReturnLike(comptime T: type) bool {
    return isNoReturnLikeInfo(@typeInfo(T));
}

/// `true` if `T` is `noreturn` like in that it has *no* possible values;
/// otherwise, `false`.
///
/// This function returns true for
/// - `noreturn`
/// - `opaque` types
/// - `error{}` and equivalent inferred function error sets
/// - Exhaustive `enum`s with 0 fields
/// - `union`s with 0 fields
/// - `struct`s with at least one field that is `noreturn`-like
/// - Arrays and vectors of a `noreturn`-like type
pub fn isNoReturnLikeInfo(comptime type_info: std.builtin.Type) bool {
    switch (type_info) {
        .noreturn, .@"opaque" => return true,
        .@"enum" => |enum_info| return enum_info.is_exhaustive and
            enum_info.fields.len == 0,
        .@"union" => |union_info| return union_info.fields.len == 0,
        .error_set => |errors| return errors != null and errors.?.len == 0,
        .array => |arr_info| return isNoReturnLike(arr_info.child),
        .vector => |vec_info| return isNoReturnLike(vec_info.child),
        .@"struct" => |struct_info| {
            for (struct_info.fields) |f| {
                if (!f.is_comptime and isNoReturnLike(f.type))
                    return true;
            }
            return false;
        },
        else => return false,
    }
}

pub fn isZstInfo(comptime type_info: std.builtin.Type) bool {
    switch (type_info) {
        .@"fn",
        .type,
        .void,
        .null,
        .undefined,
        .comptime_int,
        .comptime_float,
        .enum_literal,
        => return true,
        .int, .float => |num_info| return num_info.bits == 0,
        .vector => |vec_info| return vec_info.len == 0 or
            @sizeOf(vec_info.child) == 0,
        .array => |arr_info| return @sizeOf(arr_info.child) == 0 or
            (arr_info.len == 0 and arr_info.sentinel == null),
        .@"enum" => |enum_info| return @sizeOf(enum_info.tag_type) == 0,
        .@"union" => |union_info| {
            if (union_info.tag_type) |Tag| {
                if (@sizeOf(Tag) != 0) return false;
            }
            for (union_info.fields) |f| {
                if (@sizeOf(f.type) != 0) return false;
            }
            return true;
        },
        .@"struct" => |struct_info| {
            for (struct_info.fields) |f| {
                if (!f.is_comptime and @sizeOf(f.type) != 0) return false;
            }
            return true;
        },
        .optional => |opt_info| return @sizeOf(?opt_info.child) == 0,
        else => return false,
    }
}
