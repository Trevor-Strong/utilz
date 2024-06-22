//! Packed type utilities (`packed struct` and `packed union`)


pub fn isPacked(comptime T: type) bool {
    return switch (@typeInfo(T)) {
        inline .Struct, .Union => |info| info.layout == .@"packed",
        else => false,
    };
}


/// Get the integer type backing the packed type `T`
pub fn Int(comptime T: type) type {
    switch (@typeInfo(T)) {
        .Struct => |s_info| if (s_info.backing_integer) |I| return I,
        .Union => |u_info| switch (u_info.layout) {
            .@"packed" => return std.meta.Int(.unsigned, @bitSizeOf(T)),
            .@"extern", .auto => {},
        },
        else => {},
    }

    @compileError(utilz.unexpectedTypeMsg("Expected a packed struct or union,", T));
}

pub inline fn byteSwap(packed_val: anytype) @TypeOf(packed_val) {
    const T = @TypeOf(packed_val);
    return fromInt(T, @byteSwap(toInt(packed_val)));
}

/// Converts a packed struct or union to its backing integer type
pub fn toInt(packed_val: anytype) Int(@TypeOf(packed_val)) {
    return @bitCast(packed_val);
}

/// Converts an integer to a packed struct or union of the same size
pub fn fromInt(comptime T: type, int: Int(T)) T {
    return @bitCast(int);
}

pub fn eql(lhs: anytype, rhs: anytype) bool {
    const L = @TypeOf(lhs);
    const R = @TypeOf(rhs);

    const Packed = Packed: {
        if (isPacked(L)) {
            if (isPacked(R) and L != R) {
                @compileError(utilz.p("Cannot compare different types '{}' " ++
                    "and '{}' for equality", .{ L, R }));
            }
            break :Packed L;
        } else if (isPacked(R)) {
            break :Packed R;
        } else {
            @compileError(utilz.p(
                "Expected packed union or struct, found {} and {}",
                .{ L, R },
            ));
        }
    };

    return toInt(@as(Packed, lhs)) == toInt(@as(Packed, rhs));
}

/// Creates a function that performs a bit cast from the type `From` to the
/// type `To`.
pub fn boundBitCast(comptime From: type, comptime To: type) fn (From) To {
    if (@bitSizeOf(From) != @bitSizeOf(To)) {
        @compileError(utilz.p("Cannot bit cast '{}' to '{}' because they " ++
            "are different sizes! from: {d}, to: {d}", .{
            From,
            To,
            @bitSizeOf(From),
            @bitSizeOf(To),
        }));
    }

    return struct {
        fn f(from: From) To {
            return @bitCast(from);
        }
    }.f;
}

const std = @import("std");
const utilz = @import("utilz");
const testing = std.testing;

test {
    testing.refAllDecls(@This());
}
