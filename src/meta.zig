const std = @import("std");
const utilz = @import("utilz");
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

pub const TraitFn = @TypeOf(struct {
    inline fn f(comptime T: type) bool {
        _ = T;
        return false;
    }
}.f);

pub inline fn is(comptime tag: std.builtin.TypeId) TraitFn {
    return struct {
        inline fn f(comptime T: type) bool {
            return @typeInfo(T) == tag;
        }
    }.f;
}

/// `true` if `T` is an integer type, including `comptime_int`
pub inline fn isInt(comptime T: type) bool {
    return T == comptime_int or @typeInfo(T) == .Int;
}

/// `true` if `T` is an float type, including `comptime_float`
pub inline fn isFloat(comptime T: type) bool {
    return T == comptime_float or @typeInfo(T) == .Float;
}

pub fn isComptimeOnly(comptime T: type) bool {
    return isComptimeOnlyInfo(@typeInfo(T));
}

pub fn isComptimeOnlyInfo(comptime type_info: std.builtin.Type) bool {
    switch (type_info) {
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

pub fn isMultiValueZst(comptime T: type) bool {
    return isMultiValueZstInfo(@typeInfo(T));
}

/// Determines if `T` is a Zero Size Type (ZST) that can represent multiple
/// values (`undefined` isn't a valid value of `T`). All of these types are also
/// `comptime` only types.
pub fn isMultiValueZstInfo(comptime type_info: std.builtin.Type) bool {
    return switch (@typeInfo(type_info)) {
        .Fn,
        .Type,
        .ComptimeInt,
        .ComptimeFloat,
        .EnumLiteral,
        => true,
        inline .Vector, .Array => |info| info.len == 0 or isMultiValueZst(info.child),
        .Struct => |s_info| for (s_info.fields) |f| {
            if (!f.is_comptime and isMultiValueZst(f.type)) break true;
        } else false,
        .Union => |u_info| for (u_info.fields) |f| {
            if (isMultiValueZst(f.type)) break true;
        } else false,
        .ErrorUnion => |eu_info| isMultiValueZst(eu_info.payload),
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
        .NoReturn, .Opaque => return true,
        .Enum => |enum_info| return enum_info.is_exhaustive and enum_info.fields.len == 0,
        .Union => |union_info| return union_info.fields.len == 0,
        .ErrorSet => |errors| return errors != null and errors.?.len == 0,
        .Array => |arr_info| return isNoReturnLike(arr_info.child),
        .Vector => |vec_info| return isNoReturnLike(vec_info.child),
        .Struct => |struct_info| {
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
        .Fn,
        .Type,
        .Void,
        .Null,
        .Undefined,
        .ComptimeInt,
        .ComptimeFloat,
        .EnumLiteral,
        => return true,
        .Int, .Float => |num_info| return num_info.bits == 0,
        .Vector => |vec_info| return vec_info.len == 0 or @sizeOf(vec_info.child) == 0,
        .Array => |arr_info| return @sizeOf(arr_info.child) == 0 or (arr_info.len == 0 and arr_info.sentinel == null),
        .Enum => |enum_info| return @sizeOf(enum_info.tag_type) == 0,
        .Union => |union_info| {
            if (union_info.tag_type) |Tag| {
                if (@sizeOf(Tag) != 0) return false;
            }
            for (union_info.fields) |f| {
                if (@sizeOf(f.type) != 0) return false;
            }
            return true;
        },
        .Struct => |struct_info| {
            for (struct_info.fields) |f| {
                if (!f.is_comptime and @sizeOf(f.type) != 0) return false;
            }
            return true;
        },
        .Optional => |opt_info| return @sizeOf(?opt_info.child) == 0,
        else => return false,
    }
}

/// Returns `true` if `Fn` would be callable as a method of `Self`; otherwise
/// returns `false`. If the first parameter of `Fn` is generic, this function
/// always returns `false`
pub fn isMethod(comptime Self: type, comptime Fn: type) bool {
    const fn_info = @typeInfo(Fn);
    if (fn_info != .Fn) return false;
    return isMethodInfo(Self, fn_info.Fn);
}

pub fn isMethodInfo(
    comptime Self: type,
    comptime fn_info: std.builtin.Type.Fn,
) bool {
    if (fn_info.params.len == 0) return false;
    const T = fn_info.params[0].type orelse return false;
    if (T == Self) return true;
    const ptr_info = switch (@typeInfo(T)) {
        .Pointer => |ptr_info| return switch (ptr_info.size) {
            .C, .One => ptr_info.child == Self,
            .Many, .Slice => false,
        },
        .Optional => |opt_info| opt_info.child == Self or switch (@typeInfo(opt_info.child)) {
            .Pointer => |ptr_info| switch (ptr_info.size) {
                .One => ptr_info.child == Self,
                .C => false, // optional `C` pointers don't count
                .Many, .Slice => false,
            },
        },
        .ErrorUnion => |eu_info| return eu_info.payload == Self,
        else => return false,
    };
    switch (ptr_info.size) {
        .C, .One => {},
        .Many, .Slice => return false,
    }
    return ptr_info.child == Self;
}

pub const PointerOptions = struct {
    allow_optional: bool = false,
};

pub fn isThinPtrEx(comptime T: type, comptime options: PointerOptions) bool {
    return switch (@typeInfo(T)) {
        .Pointer => |ptr_info| ptr_info.size != .Slice,
        .Optional => |opt_info| options.allow_optional and switch (@typeInfo(opt_info.child)) {
            .Pointer => |ptr_info| !ptr_info.is_allowzero and switch (ptr_info.size) {
                .One, .Many => true,
                .C, .Slice => false,
            },
            else => false,
        },
        else => false,
    };
}

pub fn PtrChild(comptime T: type) type {
    return getPtrChild(T) catch @compileError(
        "Expected pointer or optional pointer type, found '" ++ @typeName(T) ++ "'",
    );
}

pub fn getPtrChild(comptime T: type) error{NonPointerType}!type {
    return switch (@typeInfo(T)) {
        .Pointer => |ptr_info| ptr_info.child,
        .Optional => |opt_info| switch (@typeInfo(opt_info.child)) {
            .Pointer => |ptr_info| ptr_info.child,
            else => return error.NonPointerType,
        },
        else => return error.NonPointerType,
    };
}
