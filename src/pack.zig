//! Packed type utilities (`packed struct` and `packed union`)
const std = @import("std");
const utilz = @import("utilz");

/// True if `T` is a packed union or struct type
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
        .Enum => |e_info| return e_info.tag_type,
        else => {},
    }

    @compileError("Expected an enum, packed struct, or packed union '" ++ @typeName(T) ++ "'");
}

/// does `@byteSwap` on a packed struct or union
pub inline fn byteSwap(packed_val: anytype) @TypeOf(packed_val) {
    const T = @TypeOf(packed_val);
    return fromInt(T, @byteSwap(toInt(packed_val)));
}

/// Converts a packed struct or union to its backing integer type
pub fn toInt(packed_val: anytype) Int(@TypeOf(packed_val)) {
    return switch (@typeInfo(@TypeOf(packed_val))) {
        .Struct, .Union => @bitCast(packed_val),
        .Enum => @intFromEnum(packed_val),
        else => unreachable,
    };
}

/// Converts an integer to a packed struct or union of the same size
pub fn fromInt(comptime T: type, int: Int(T)) T {
    return switch (@typeInfo(T)) {
        .Enum => @enumFromInt(int),
        .Struct, .Union => @bitCast(int),
        else => unreachable,
    };
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

const testing = std.testing;

const U32 = packed struct { x: u32 };

fn testToInt(x: u32) !void {
    try testing.expectEqual(x, toInt(U32{ .x = x }));
}

test toInt {
    try testToInt(0);
    try testToInt(15);
    try testToInt(0xFEEDBEEF);
    try testToInt(69420);
}

fn testFromInt(x: u32) !void {
    try testing.expectEqual(U32{ .x = x }, fromInt(U32, x));
}

test fromInt {
    try testFromInt(0);
    try testFromInt(15);
    try testFromInt(0xFEEDBEEF);
    try testFromInt(69420);
}

fn testByteSwap(x: u32) !void {
    try testing.expectEqual(U32{ .x = @byteSwap(x) }, byteSwap(U32{ .x = x }));
}

test byteSwap {
    try testByteSwap(0);
    try testByteSwap(0x11_22_33_44);
    try testByteSwap(0xFEEDBEEF);
}
