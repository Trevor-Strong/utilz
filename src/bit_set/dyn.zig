//! Functions implementing dynamic bit set operations with a generic `Mask` type.

const std = @import("std");
const assert = std.debug.assert;
const boolMask = std.math.boolMask;
const Range = @import("../bit_set.zig").Range;
const IteratorOptions = std.bit_set.IteratorOptions;

/// Mask with the only bits set being the bits that are
pub fn lastWordMask(comptime Mask: type, bit_len: usize) Mask {
    const n_sig_bits: Shift(Mask) = @intCast(@mod(bit_len, @bitSizeOf(Mask)));
    const full_mask = comptime boolMask(Mask, true);
    return ~(full_mask >> n_sig_bits);
}

pub fn isSet(comptime Mask: type, masks: [*]const Mask, index: usize) bool {
    return masks[maskIndex(Mask, index)] & maskBit(Mask, index) != 0;
}

pub fn set(comptime Mask: type, masks: [*]Mask, index: usize) void {
    masks[maskIndex(Mask, index)] |= maskBit(Mask, index);
}

pub fn unset(comptime Mask: type, masks: [*]Mask, index: usize) void {
    masks[maskIndex(Mask, index)] &= ~maskBit(Mask, index);
}

pub fn toggle(comptime Mask: type, masks: [*]Mask, index: usize) void {
    masks[maskIndex(Mask, index)] ^= maskBit(Mask, index);
}

pub fn setValue(comptime Mask: type, masks: [*]Mask, index: usize, value: bool) void {
    const bit = maskBit(Mask, index);
    const mask_index = maskIndex(Mask, index);
    const new_bit = bit & boolMask(Mask, value);
    masks[mask_index] = (masks[mask_index] & ~bit) | new_bit;
}

pub fn setRange(comptime Mask: type, masks: [*]Mask, bit_len: usize, range: Range) void {
    setRangeValueImpl(Mask, masks, bit_len, range, .set);
}

pub fn unsetRange(comptime Mask: type, masks: [*]Mask, bit_len: usize, range: Range) void {
    setRangeValueImpl(Mask, masks, bit_len, range, .unset);
}

pub fn toggleRange(comptime Mask: type, masks: [*]Mask, bit_len: usize, range: Range) void {
    setRangeValueImpl(Mask, masks, bit_len, range, .toggle);
}

pub fn setRangeValue(comptime Mask: type, masks: [*]Mask, bit_len: usize, range: Range, value: bool) void {
    switch (value) {
        true => setRangeValueImpl(Mask, masks, bit_len, range, .set),
        false => setRangeValueImpl(Mask, masks, bit_len, range, .unset),
    }
}

pub fn setAll(comptime Mask: type, masks: [*]Mask, bit_len: usize) void {
    if (bit_len == 0) return;
    const num_masks = numMasks(Mask, bit_len);
    const full_mask = comptime boolMask(Mask, true);
    @memset(masks[0 .. num_masks - 1], full_mask);
    masks[num_masks - 1] = lastWordMask(Mask, bit_len);
}

pub fn unsetAll(comptime Mask: type, masks: [*]Mask, bit_len: usize) void {
    @memset(masks[0..numMasks(Mask, bit_len)], 0);
}

pub fn toggleAll(comptime Mask: type, masks: [*]Mask, bit_len: usize) void {
    if (bit_len == 0) return;
    const num_masks = numMasks(Mask, bit_len);
    for (masks[0 .. num_masks - 1]) |*mask| {
        mask.* = ~mask.*;
    }
    masks[num_masks - 1] ^= lastWordMask(Mask, bit_len);
}

pub fn setUnion(comptime Mask: type, dest: [*]Mask, source: [*]const Mask, bit_len: usize) void {
    doBitOp(Mask, dest, source, bit_len, .merge);
}

pub fn setIntersection(comptime Mask: type, dest: [*]Mask, source: [*]const Mask, bit_len: usize) void {
    doBitOp(Mask, dest, source, bit_len, .intersect);
}

pub fn toggleSet(comptime Mask: type, dest: [*]Mask, source: [*]const Mask, bit_len: usize) void {
    doBitOp(Mask, dest, source, bit_len, .xor);
}

pub fn Shift(comptime Mask: type) type {
    return std.math.Log2Int(Mask);
}

pub fn maskBit(comptime MaskInt: type, index: usize) MaskInt {
    return @as(MaskInt, 1) << @as(Shift(MaskInt), @truncate(index));
}

pub fn maskIndex(comptime MaskInt: type, index: usize) usize {
    return index >> @bitSizeOf(Shift(MaskInt));
}

pub fn eql(comptime Mask: type, lhs: [*]const Mask, rhs: [*]const Mask, bit_len: usize) bool {
    const num_masks = numMasks(Mask, bit_len);

    for (lhs[0..num_masks], rhs[0..num_masks]) |a, b| {
        if (a != b) return false;
    }
    return true;
}

pub fn isSubset(comptime Mask: type, lhs: [*]const Mask, rhs: [*]const Mask, bit_len: usize) bool {
    if (bit_len == 0 or lhs == rhs) return true;
    const len = numMasks(Mask, bit_len);
    for (lhs[0..len], rhs[0..len]) |a, b| {
        if (a & b != a) return false;
    }
    return true;
}

const BitOp = enum {
    merge,
    intersect,
    xor,
};

inline fn assertMaskLenAndBitLenConsistent(
    comptime mask_bit_size: comptime_int,
    masks_len: usize,
    bit_len: usize,
) void {
    assert((masks_len * mask_bit_size) >= bit_len and
        (masks_len * mask_bit_size) - bit_len < mask_bit_size);
}

fn doBitOp(comptime T: type, dest: [*]T, source: [*]const T, bit_len: usize, comptime op: BitOp) void {
    if (dest == source) {
        switch (op) {
            .merge, .intersect => return,
            .xor => {
                unsetAll(T, dest, bit_len);
                return;
            },
        }
    }
    if (bit_len == 0) return;
    const len = numMasks(T, bit_len);
    for (dest[0..len], source[0..len], 0..) |lhs, rhs, i| {
        dest[i] = switch (op) {
            .merge => lhs | rhs,
            .intersect => lhs & rhs,
            .xor => lhs ^ rhs,
        };
    }
}

pub fn numMasks(comptime Mask: type, bit_len: usize) usize {
    const mask_size = @bitSizeOf(Mask);
    if (std.math.isPowerOfTwo(mask_size)) {
        const shift = std.math.log2(mask_size);
        return (bit_len >> shift) + @as(usize, @intFromBool(bit_len & (mask_size - 1) != 0));
    } else {
        return (bit_len + (@bitSizeOf(Mask) - 1)) / @bitSizeOf(Mask);
    }
}

fn setRangeValueImpl(
    comptime Mask: type,
    masks: [*]Mask,
    bit_len: usize,
    range: Range,
    comptime mode: enum { set, unset, toggle },
) void {
    const start = range.start;
    const end = range.end orelse bit_len;
    assert(end <= bit_len);
    assert(start <= end);
    if (bit_len == 0) return;
    const num_masks = numMasks(Mask, bit_len);
    if (start == end) return;
    assert(end != 0);

    const start_mask_index = maskIndex(Mask, start);
    const start_bit: Shift(Mask) = @truncate(start);
    const end_mask_index = maskIndex(Mask, end - 1);
    const end_bit: Shift(Mask) = @truncate(end - 1);

    assert(end_bit != 0);
    const full_mask = comptime boolMask(Mask, true);
    const start_mask = full_mask << start_bit;
    const end_mask = full_mask >> @as(Shift(Mask), @truncate(@as(Mask, @bitSizeOf(Mask)) - end_bit));

    if (start_mask_index == end_mask_index) {
        const mask: Mask = start_mask & end_mask;
        switch (mode) {
            .set => masks[start_mask_index] |= mask,
            .unset => masks[start_mask_index] &= ~mask,
            .toggle => masks[start_mask_index] ^= mask,
        }
    } else {
        var i = start_mask_index;
        if (start_bit > 0) {
            switch (mode) {
                .set => masks[i] |= start_mask,
                .unset => masks[i] &= ~start_mask,
                .toggle => masks[i] ^= start_mask,
            }
            i += 1;
        }

        while (i != end_mask_index) : (i += 1) {
            switch (mode) {
                .set => masks[i] = full_mask,
                .unset => masks[i] = 0,
                .toggle => masks[i] = ~masks[i],
            }
        }

        if (end_bit > 0) {
            assert(end_mask_index == i);
            assert(end_mask_index < num_masks);

            switch (mode) {
                .set => masks[i] |= end_mask,
                .unset => masks[i] &= ~end_mask,
                .toggle => masks[i] ^= end_mask,
            }
        }
    }
}
