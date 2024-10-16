//! Packed type utilities (`packed struct` and `packed union`)
const std = @import("std");
const utilz = @import("utilz");

pub fn isPacked(comptime T: type) bool {
    return switch (@typeInfo(T)) {
        .void, .int, .float, .@"enum" => true,
        .vector => |vec_info| isPacked(vec_info.child),
        inline .@"struct", .@"union" => |info| info.layout == .@"packed",
        .pointer => |ptr_info| ptr_info.size != .Slice,
        .optional => |opt_info| switch (@typeInfo(opt_info.child)) {
            .pointer => |ptr_info| switch (ptr_info.size) {
                .Slice, .C => false,
                .One, .Many => !ptr_info.is_allowzero,
            },
            else => false,
        },
        else => false,
    };
}

/// True if `T` is a packed union or struct type
pub fn isLayoutPacked(comptime T: type) bool {
    return switch (@typeInfo(T)) {
        inline .@"struct", .@"union" => |info| info.layout == .@"packed",
        else => false,
    };
}

/// Get the integer type backing the packed type `T`
pub fn Int(comptime T: type) type {
    switch (@typeInfo(T)) {
        .@"struct" => |s_info| if (s_info.backing_integer) |I| return I,
        .@"union" => |u_info| switch (u_info.layout) {
            .@"packed" => return std.meta.Int(.unsigned, @bitSizeOf(T)),
            .@"extern", .auto => {},
        },
        .@"enum" => |e_info| return e_info.tag_type,
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
        .@"struct", .@"union" => @bitCast(packed_val),
        .@"enum" => @intFromEnum(packed_val),
        else => unreachable,
    };
}

/// Converts an integer to a packed struct or union of the same size
pub fn fromInt(comptime T: type, int: Int(T)) T {
    return switch (@typeInfo(T)) {
        .@"enum" => @enumFromInt(int),
        .@"struct", .@"union" => @bitCast(int),
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
