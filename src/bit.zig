const std = @import("std");
const util = @import("root.zig");

pub fn Ops(comptime Self: type) type {
    return OpsExtra(Self, util.meta.IntRepr(Self));
}

pub fn OpsExtra(comptime Self: type, comptime IntType: type) type {
    return struct {
        pub const Int = IntType;

        pub inline fn int(self: Self) Int {
            return @bitCast(util.meta.toInt(self));
        }

        pub inline fn fromInt(value: Int) Self {
            return util.meta.fromInt(Self, value);
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
