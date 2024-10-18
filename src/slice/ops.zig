//! Implementations of operations on slices.

const std = @import("std");
const utilz = @import("utilz");

const assert = std.debug.assert;

/// Converts the given pointer to a slice.
///
/// Accepts:
///
/// - Slices
/// - Pointers to arrays
/// - Pointers to vectors of power of 2 sized elements (`u8`, `f32` ect. but not `i17`)
/// - Many-item pointers with a sentinel
pub fn toSlice(ptr: anytype) utilz.slice.ToSlice(@TypeOf(ptr)) {
    const T = @TypeOf(ptr);
    const slice = utilz.slice.forType(T) catch unreachable;
    return slice.convert(ptr);
}

/// Breaks the slice into 2 pieces.
///
/// Returns the slices `slice[0..index]` and `slice[index..]`.
///
/// See also: `splitAt`,
pub fn breakAt(slice: anytype, index: usize) Break(@TypeOf(slice)) {
    const first = slice[0..index];
    const second = if (std.meta.sentinel(@TypeOf(slice))) |x|
        slice[index.. :x]
    else
        slice[index..];
    return .{ first, second };
}

/// Splits the slice at the index, excluding the value at the index.
///
/// Returns the slices `slice[0..index]` and `slice[index + 1..]` checking for
/// out of bounds
pub fn splitAt(slice: anytype, index: usize) Break(@TypeOf(slice)) {
    const first = slice[0..index];
    const i = if (index == slice.len) index else index + 1;
    const second = if (std.meta.sentinel(@TypeOf(slice))) |x|
        slice[i.. :x]
    else
        slice[i..];
    return .{ first, second };
}

/// Takes a slice type `T` and returns a tuple of
pub fn Break(comptime T: type) type {
    const slice: utilz.slice.Slice = utilz.slice.forType(T) catch {
        @compileError(utilz.expected("a slice or pointer to array type, ").foundType(T));
    };
    const first_half = slice.withoutSentinel();
    const second_half = slice;
    return struct { first_half.Type(), second_half.Type() };
}

pub fn window(slice: anytype, size: usize, step: usize) WindowIterator(utilz.slice.Elem(@TypeOf(slice))) {
    return .{
        .slice = slice,
        .size = size,
        .step = step,
    };
}

pub fn WindowIterator(comptime T: type) type {
    return struct {
        slice: []const T,
        index: usize = 0,
        size: usize,
        step: usize,

        const Self = @This();

        pub const Item = T;

        pub fn peek(it: Self) ?[]const T {
            const start = it.index;
            const slice = it.slice;
            if (start == slice.len) return null;
            const len = @min(it.size, slice.len - start);
            return slice[start..][0..len];
        }

        pub fn next(it: *Self) ?[]const T {
            const slice = it.peek() orelse return null;
            it.index += @min(it.step, it.slice.len - it.index);
            return slice;
        }

        pub fn reset(it: *Self) void {
            it.index = 0;
        }
    };
}

const testing = std.testing;

test {
    testing.refAllDecls(@This());
}

test window {
    {
        // moving average size 3
        var it = window("abcdefg", 3, 1);
        try testing.expectEqualStrings("abc", it.next().?);
        try testing.expectEqualStrings("bcd", it.next().?);
        try testing.expectEqualStrings("cde", it.next().?);
        try testing.expectEqualStrings("def", it.next().?);
        try testing.expectEqualStrings("efg", it.next().?);

        try testing.expectEqualStrings("fg", it.next().?);
        try testing.expectEqualStrings("g", it.next().?);

        try testing.expectEqual(it.next(), null);

        // multibyte
        const L = std.unicode.utf8ToUtf16LeStringLiteral;
        var it16 = window(L("abcdefg"), 3, 1);
        try testing.expectEqualSlices(u16, L("abc"), it16.next().?);
        try testing.expectEqualSlices(u16, L("bcd"), it16.next().?);
        try testing.expectEqualSlices(u16, L("cde"), it16.next().?);
        try testing.expectEqualSlices(u16, L("def"), it16.next().?);
        try testing.expectEqualSlices(u16, L("efg"), it16.next().?);
        try testing.expectEqualSlices(u16, L("fg"), it16.next().?);
        try testing.expectEqualSlices(u16, L("g"), it16.next().?);
        try testing.expectEqual(it16.next(), null);
    }

    {
        // chunk/split every 3
        var it = window("abcdefg", 3, 3);
        try testing.expectEqualStrings("abc", it.next().?);
        try testing.expectEqualStrings("def", it.next().?);
        try testing.expectEqualStrings("g", it.next().?);
        try testing.expectEqual(null, it.next());
    }

    {
        // pick even
        var it = window("abcdefg", 1, 2);
        try testing.expectEqualStrings("a", it.next().?);
        try testing.expectEqualStrings("c", it.next().?);
        try testing.expectEqualStrings("e", it.next().?);
        try testing.expectEqualStrings("g", it.next().?);
        try testing.expectEqual(null, it.next());
    }

    {
        // empty
        var it = window("", 1, 1);
        try testing.expectEqual(null, it.next());

        it = window("", 10, 1);
        try testing.expectEqual(null, it.next());

        it = window("", 1, 10);
        try testing.expectEqual(null, it.next());

        it = window("", 10, 10);
        try testing.expectEqual(null, it.next());
    }

    {
        // peek
        var it = window("abcdefg", 3, 3);
        try testing.expectEqualStrings("abc", it.peek().?);
        try testing.expectEqualStrings("abc", it.peek().?);
        try testing.expectEqualStrings("abc", it.next().?);
        try testing.expectEqualStrings("def", it.peek().?);
        try testing.expectEqualStrings("def", it.next().?);
        try testing.expectEqualStrings("g", it.peek().?);
        try testing.expectEqualStrings("g", it.next().?);
        try testing.expectEqual(null, it.peek());
        try testing.expectEqual(null, it.next());
    }

    {
        // reset
        var it = window("abcdefg", 3, 3);
        try testing.expectEqualStrings("abc", it.next().?);
        try testing.expectEqualStrings("def", it.next().?);
        try testing.expectEqualStrings("g", it.next().?);
        try testing.expectEqual(it.next(), null);

        it.reset();
        try testing.expectEqualStrings("abc", it.next().?);
        try testing.expectEqualStrings("def", it.next().?);
        try testing.expectEqualStrings("g", it.next().?);
        try testing.expectEqual(it.next(), null);
    }
}

fn testBreakAt(first: []const u8, second: []const u8) !void {
    const slice = try std.mem.concat(testing.allocator, u8, &.{ first, second });
    defer testing.allocator.free(slice);
    const broken = breakAt(slice, first.len);
    try testing.expectEqualStrings(first, broken[0]);
    try testing.expectEqualStrings(second, broken[1]);
}

fn expectTupleTypes(comptime T: type, comptime types: []const type) !void {
    comptime {
        const info = @typeInfo(T).@"struct";
        try testing.expect(info.is_tuple);
        var field_types: [info.fields.len]type = undefined;
        for (info.fields, &field_types) |f, *t| {
            t.* = f.type;
        }
        try testing.expectEqualDeep(types, &field_types);
    }
}

test Break {
    try expectTupleTypes(@TypeOf(breakAt(@as([:0]u8, undefined), 15)), &.{ []u8, [:0]u8 });
    try expectTupleTypes(@TypeOf(breakAt(@as(*[9:0]u8, undefined), 0)), &.{ []u8, [:0]u8 });
    try expectTupleTypes(@TypeOf(breakAt(@as(*[9:0]u16, undefined), 0)), &.{ []u16, [:0]u16 });
    try expectTupleTypes(@TypeOf(breakAt(@as([]const volatile std.ArrayList(i49), undefined), 0)), &.{
        []const volatile std.ArrayList(i49),
        []const volatile std.ArrayList(i49),
    });
}

test breakAt {
    try testBreakAt("abc", "123");
    try testBreakAt("", "123");
    try testBreakAt("abc", "");
    try testBreakAt(
        "longer string",
        "not empty",
    );
    try testBreakAt("a", "bcdef");
}

fn testSplitAt(comptime T: type, first: []const T, ch: T, second: []const T) !void {
    const slice = try std.mem.concat(testing.allocator, T, &.{
        first,
        @as(*const [1]T, &ch),
        second,
    });
    defer testing.allocator.free(slice);
    const split = splitAt(slice, first.len);
    if (T == u8) {
        try testing.expectEqualStrings(first, split[0]);
        try testing.expectEqualStrings(second, split[1]);
    } else {
        try testing.expectEqualSlices(T, first, split[0]);
        try testing.expectEqualSlices(T, second, split[1]);
    }
}

test splitAt {
    const L = std.unicode.utf8ToUtf16LeStringLiteral;
    try testSplitAt(u8, "abc", '=', "123");
    try testSplitAt(u16, L("abc"), '=', L("123"));
    try testSplitAt(u8, "", '=', "123");
    try testSplitAt(u8, "abc", '=', "");
    try testSplitAt(u8, "longer string", 0x87, "not empty");
    try testSplitAt(u8, "a", 'c', "bcdef");
}

test toSlice {
    const eql = testing.expectEqual;
    var buffer: [10:0]u8 align(16) = "0123456789".*;
    const ptr: *align(16) [10:0]u8 = &buffer;
    try eql(ptr, toSlice(@as([:0]u8, ptr)));
    try eql(ptr, toSlice(@as(*volatile [10]u8, ptr)));
    try eql(ptr, toSlice(@as([*:0]align(16) u8, ptr)));
    const vector: @Vector(10, u8) = buffer;
    const v_ptr: *const @Vector(10, u8) = &vector;
    try eql(@as(*const [10]u8, v_ptr), toSlice(v_ptr));
    const wptr = std.unicode.utf8ToUtf16LeStringLiteral("0123");
    try eql(wptr, toSlice(wptr));
    // TODO: `std.mem.sliceTo` doesn't support under-aligned slices
    // try eql(wptr, toSlice(@as([*:0]const align(1) u16, wptr)));
}
