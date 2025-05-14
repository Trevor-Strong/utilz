const std = @import("std");
const util = @import("root.zig");

pub fn Ops(comptime Self: type) type {
    return OpsExtra(Self, IntRepr(Self));
}

pub fn OpsExtra(comptime Self: type, comptime IntType: type) type {
    const _fromInt = fromInt;
    return struct {
        pub const Int = IntType;

        pub inline fn int(self: Self) Int {
            return @bitCast(toInt(self));
        }

        pub inline fn fromInt(value: Int) Self {
            return _fromInt(Self, value);
        }

        pub inline fn set(self: Self, flags: Self) Self {
            return @This().fromInt(int(self) | int(flags));
        }

        pub inline fn clear(self: Self, flags: Self) Self {
            return @This().fromInt(int(self) & ~int(flags));
        }

        pub inline fn keepOnly(self: Self, flags: Self) Self {
            return @This().fromInt(int(self) & int(flags));
        }

        pub inline fn plus(lhs: Self, rhs: Self) Self {
            return @This().fromInt(int(lhs) + int(rhs));
        }

        pub inline fn minus(lhs: Self, rhs: Self) Self {
            return @This().fromInt(int(lhs) - int(rhs));
        }

        pub inline fn inv(self: Self) Self {
            return @This().fromInt(~int(self));
        }

        pub inline fn negate(self: Self) Self {
            return @This().fromInt(-%int(self));
        }
    };
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
        .float => |float_info| return util.meta.UInt(float_info.bits),
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
            .@"packed" => return util.meta.UInt(@bitSizeOf(T)),
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
        .error_set => return util.meta.UInt(@bitSizeOf(anyerror)),
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
