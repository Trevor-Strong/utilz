//! Packed type utilities (`packed struct` and `packed union`)
const std = @import("std");

/// Returns `true` if `T` can be used as a field of a packed `struct` or `union`.
pub inline fn isPacked(comptime T: type) bool {
    return comptime switch (@typeInfo(T)) {
        // Void has zero bits, so it's packable
        .void => true,
        // These are the most basic packable types
        .int, .float, .bool => true,
        // Enums are represented as integers, and so are packable because
        // integers are packable
        .@"enum" => true,
        // TODO: Determine if the 'isPacked(vec_info.child)' is necessary.
        .vector => |vec_info| isPacked(vec_info.child),
        inline .@"struct", .@"union" => |info| info.layout == .@"packed",
        // Pointers are just a `usize` in memory, and so can be packed.
        // 'Slice' pointers are special because they carry a 'len' with them
        // and so are not represented in memory the same way
        .pointer => |ptr_info| ptr_info.size != .Slice,
        // Zig pointers are not nullable by default (address 0 isn't allowed),
        // meaning that optional pointers just use address 0 as the null value
        // and keep the same memory representation (aside from the some
        // exceptions handled here)
        .optional => |opt_info| switch (@typeInfo(opt_info.child)) {
            .pointer => |ptr_info| switch (ptr_info.size) {
                // Slices are never packable
                .Slice => false,
                // C pointers are nullable by default, so an optional C pointer
                // requires more storage for the optional bit
                .C => false,
                // Same as with C pointers, but only applying when the pointer
                // is 'allowzero'
                .One, .Many => !ptr_info.is_allowzero,
            },
            // All other optional types are not packable
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

/// Get the integer type backing the packed type `T`.
///
/// `enum` and `packed struct` types both have the ability to specify the
/// integer type that represents them so this function will return that type,
/// and `int` types are already integers, so the same type is returned. In all
/// other cases, an unsigned integer with the same bit size as `T` is returned.
///
/// If `T` is not a packed type (`isPacked(T)` is `false`), a compile error is
/// generated.
pub fn Int(comptime T: type) type {
    if (!isPacked(T))
        @compileError("Expected packed type, found '" ++ @typeName(T) ++ "'");
    switch (@typeInfo(T)) {
        .int => return T,
        .@"struct" => |s_info| if (s_info.backing_integer) |I| return I,
        .@"enum" => |e_info| return e_info.tag_type,
        else => return std.meta.Int(.unsigned, @bitSizeOf(T)),
    }

    @compileError("Expected an enum, packed struct, or packed union '" ++ @typeName(T) ++ "'");
}

/// Does `@byteSwap` on any packed type. This is unnecessary for integers, but
/// other packed types must be converted to an integer for `@byteSwap` to work,
/// which this function handles for you.
pub inline fn byteSwap(packed_val: anytype) @TypeOf(packed_val) {
    const T = @TypeOf(packed_val);
    return fromInt(T, @byteSwap(toInt(packed_val)));
}

/// Converts a packed value to an integer with the same bits as the original
/// value
pub inline fn toInt(packed_val: anytype) Int(@TypeOf(packed_val)) {
    const T = @TypeOf(packed_val);
    if (!isPacked(T))
        @compileError("Expected packed type, found '" ++ @typeName(T) ++ "'");
    return switch (@typeInfo(@TypeOf(packed_val))) {
        .@"enum" => @intFromEnum(packed_val),
        .optional, .pointer => @intFromPtr(packed_val),
        else => @bitCast(packed_val),
    };
}

/// Converts an integer to a packed type with the same number of bits.
///
/// Note that some conversions are safety checked, such as integer to
/// non-optional pointer or integer to exhaustive enum conversions. The behavior
/// of such safety checks is not affected by this function, so `fromInt(*i32, 0)`
/// will cause a safety check the same way that `@as(*i32, @ptrFromInt(0))`
/// will.
pub fn fromInt(comptime T: type, int: Int(T)) T {
    if (!isPacked(T))
        @compileError("Expected packed type, found '" ++ @typeName(T) ++ "'");

    return switch (@typeInfo(T)) {
        // Special case where T == Int(T)
        .int => int,
        .@"enum" => @as(T, @enumFromInt(int)),
        .optional, .pointer => @as(T, @ptrFromInt(int)),
        else => @as(T, @bitCast(int)),
    };
}

/// Creates a function that performs a bit cast from the type `From` to the
/// type `To`.
pub fn boundBitCast(comptime From: type, comptime To: type) fn (From) To {
    if (@bitSizeOf(From) != @bitSizeOf(To)) {
        @compileError(std.fmt.comptimePrint("Cannot bit cast '{}' to '{}' because they " ++
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
const Enum = enum(u21) { a = 0, b = 1, c = 0xfff11 };

test {
    testing.refAllDecls(@This());
}

test isPacked {
    const assertPacked = struct {
        fn f(comptime T: type) void {
            comptime std.debug.assert(isPacked(T));
        }
    }.f;

    const assertNotPacked = struct {
        fn f(comptime T: type) void {
            comptime std.debug.assert(!isPacked(T));
        }
    }.f;

    assertPacked(u0);
    assertPacked(u32);
    assertPacked(f64);
    assertPacked(Enum);
    assertPacked(U32);
    assertPacked(*anyopaque);
    assertPacked(?*const std.Target);

    assertNotPacked(union { a: void, b: u32, c: f64 });
    assertNotPacked(extern struct { a: u32, b: i16 });
    assertNotPacked([15]u8);
    assertNotPacked([]const u16);
    assertNotPacked(?*allowzero anyopaque);
    assertNotPacked(?[*c]i8);
    assertNotPacked(?u32);
}

test toInt {
    const t = std.testing;

    try t.expectEqual(0, toInt(U32{ .x = 0 }));
    try t.expectEqual(15, toInt(U32{ .x = 15 }));
    try t.expectEqual(0xFEEDBEEF, toInt(U32{ .x = 0xFEEDBEEF }));
    try t.expectEqual(69420, toInt(U32{ .x = 69420 }));

    try t.expectEqual(Enum.a, fromInt(Enum, @intFromEnum(Enum.a)));
    try t.expectEqual(Enum.b, fromInt(Enum, @intFromEnum(Enum.b)));
    try t.expectEqual(Enum.c, fromInt(Enum, @intFromEnum(Enum.c)));

    var rng_inst = std.Random.DefaultPrng.init(t.random_seed);
    const rng = rng_inst.random();

    const max_addr = std.math.maxInt(usize);

    const x_addr: usize = std.mem.alignBackward(usize, rng.intRangeAtMost(usize, 100, max_addr), @alignOf(u32));
    const y_addr: usize = std.mem.alignBackward(usize, rng.intRangeAtMost(usize, 100, max_addr), @alignOf(i128));
    const z_addr: usize = std.mem.alignBackward(usize, rng.intRangeAtMost(usize, 100, max_addr), @alignOf(f64));

    const x: *u32 = @ptrFromInt(x_addr);
    const y: *const i128 = @ptrFromInt(y_addr);
    const z: *volatile f64 = @ptrFromInt(z_addr);

    try t.expectEqual(x_addr, toInt(x));
    try t.expectEqual(y_addr, toInt(y));
    try t.expectEqual(z_addr, toInt(z));
}

fn testFromInt(x: u32) !void {
    try testing.expectEqual(U32{ .x = x }, fromInt(U32, x));
}

test fromInt {
    const t = std.testing;

    try t.expectEqual(U32{ .x = 0 }, fromInt(U32, 0));
    try t.expectEqual(U32{ .x = 15 }, fromInt(U32, 15));
    try t.expectEqual(U32{ .x = 0xFEEDBEEF }, fromInt(U32, 0xFEEDBEEF));
    try t.expectEqual(U32{ .x = 69420 }, fromInt(U32, 69420));

    try t.expectEqual(@intFromEnum(Enum.a), toInt(Enum.a));
    try t.expectEqual(@intFromEnum(Enum.b), toInt(Enum.b));
    try t.expectEqual(@intFromEnum(Enum.c), toInt(Enum.c));

    var rng_inst = std.Random.DefaultPrng.init(t.random_seed);
    const rng = rng_inst.random();

    const max_addr = std.math.maxInt(usize);

    const x_addr: usize = std.mem.alignBackward(usize, rng.intRangeAtMost(usize, 100, max_addr), @alignOf(u32));
    const y_addr: usize = std.mem.alignBackward(usize, rng.intRangeAtMost(usize, 100, max_addr), @alignOf(i128));
    const z_addr: usize = std.mem.alignBackward(usize, rng.intRangeAtMost(usize, 100, max_addr), @alignOf(f64));

    const x: *u32 = @ptrFromInt(x_addr);
    const y: *const i128 = @ptrFromInt(y_addr);
    const z: *volatile f64 = @ptrFromInt(z_addr);

    try t.expectEqual(x, fromInt(@TypeOf(x), x_addr));
    try t.expectEqual(y, fromInt(@TypeOf(y), y_addr));
    try t.expectEqual(z, fromInt(@TypeOf(z), z_addr));
}

fn testByteSwap(x: u32) !void {
    comptime std.debug.assert((U32{ .x = 0 }) == (U32{ .x = 0 }));
    try testing.expectEqual(U32{ .x = @byteSwap(x) }, byteSwap(U32{ .x = x }));
}

test byteSwap {
    try testByteSwap(0);
    try testByteSwap(0x11_22_33_44);
    try testByteSwap(0xFEEDBEEF);
}
