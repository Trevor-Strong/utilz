const std = @import("std");
const utilz = @import("util.zig");
const assert = std.debug.assert;

pub fn Unsigned(comptime T: type) type {
    switch (@typeInfo(T)) {
        .Int => |int_info| switch (int_info.signedness) {
            .signed => @Type(.{
                .Int = .{ .signedness = .unsigned, .bits = int_info.bits },
            }),
            .unsigned => T,
        },
        else => @compileError("Sized integer type required, found '" ++ @typeName(T) ++ "'"),
    }
}

pub fn Signed(comptime T: type) type {
    switch (@typeInfo(T)) {
        .Int => |int_info| switch (int_info.signedness) {
            .signed => T,
            .unsigned => @Type(.{
                .Int = .{ .signedness = .signed, .bits = int_info.bits },
            }),
        },
        else => @compileError("Sized integer type required, found '" ++ @typeName(T) ++ "'"),
    }
}

pub inline fn SizedTypeOf(value: anytype) type {
    return switch (@TypeOf(value)) {
        comptime_int => std.math.IntFittingRange(value, value),
        else => |T| switch (@typeInfo(T)) {
            .Int => T,
            else => @compileError("Expected integer type, found '" ++ @typeName(T) ++ "'"),
        },
    };
}

/// Bit casts the integer `int` to an *unsigned* integer of the same size,
/// preserving the bits of the original value.
pub inline fn unsignedCast(int: anytype) Unsigned(@TypeOf(int)) {
    return @bitCast(int);
}

/// Bit casts the integer `int` to a *signed* integer of the same size,
/// preserving the bits of the original value.
pub inline fn signedCast(int: anytype) Signed(@TypeOf(int)) {
    return @bitCast(int);
}

const testing = std.testing;

test SizedTypeOf {
    assert(SizedTypeOf(0) == u0);
    assert(SizedTypeOf((1 << 12) - 1) == u12);
    assert(SizedTypeOf(-1) == i1);
    assert(SizedTypeOf(-(1 << 31)) == i32);
    assert(SizedTypeOf(@as(i0, 0)) == i0);
    assert(SizedTypeOf(@as(i64, 0)) == i64);
    assert(SizedTypeOf(@as(u16, 0)) == u16);
}
