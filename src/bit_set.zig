const std = @import("std");

const assert = std.debug.assert;
const boolMask = std.math.boolMask;
const Allocator = std.mem.Allocator;

/// Range of bit indices. If `start == end` the range is empty
pub const Range = struct {
    /// Start (inclusive) of the range to operate on.
    start: usize = 0,
    /// End (exclusive) of the range to operate on.
    end: ?usize = null,

    pub fn isEmpty(range: Range) bool {
        return range.start == range.end;
    }
};

pub const IteratorOptions = std.bit_set.IteratorOptions;

pub fn StaticBitSet(comptime bit_len: usize) type {
    return StaticBitSetExtra(bit_len, .auto);
}

pub const StaticBitSetConfig = union(enum) {
    /// Means `.int=null` when `bit_len <= @bitSizeOf(usize)` and `.array=usize`
    /// when `bit_len > @bitSizeOf(usize)`
    auto,
    /// A static bitset backed by a single integer of the given type. If `null`
    /// then an unsigned integer of `bit_len` bits is used.
    ///
    /// In any case, it must be possible to construct an integer of `bit_len`
    /// bits or else integer backed bitset cannot be used.
    int: ?type,
    /// A static bitset backed by an array of integers of the given type
    array: type,
};

pub fn StaticBitSetExtra(
    comptime bit_len: usize,
    comptime config: StaticBitSetConfig,
) type {
    return extern struct {
        const Impl = switch (config) {
            .auto => if (bit_len > @bitSizeOf(usize))
                ArrayBitSet(usize, bit_len)
            else
                IntBitSet(bit_len),
            .int => |ty| if (ty) |Int|
                IntBitSetEx(.{ .bit_len = bit_len, .backing_type = Int })
            else
                IntBitSet(bit_len),
            .array => |Int| ArrayBitSet(Int, bit_len),
        };

        const Self = @This();

        impl: Impl = .{},

        pub fn initFull() Self {
            return .{ .impl = Impl.initFull() };
        }

        /// Returns the number of bits there are in this bit set.
        ///
        /// This value is comptime known and the same for all bit sets of the
        /// same type.
        pub inline fn capacity(self: Self) comptime_int {
            _ = self;
            return bit_len;
        }

        const Count = std.math.IntFittingRange(0, config.bit_len);

        /// Returns the number of set bits.
        pub fn count(self: Self) Count {
            return self.impl.count();
        }

        pub fn isEmpty(self: Self) bool {
            return self.impl.isEmpty();
        }

        pub fn isFull(self: Self) bool {
            return self.impl.isFull();
        }

        /// `true` if the bit at the given index is set.
        ///
        /// Asserts that `index` is less than the capacity.
        pub fn isSet(self: Self, index: usize) bool {
            return self.impl.isSet(index);
        }

        /// Sets the bit at the given index.
        ///
        /// Asserts that `index` is less than the capacity.
        pub fn set(self: *Self, index: usize) void {
            self.impl.set(index);
        }

        /// Clears the bit at the given index.
        ///
        /// Asserts that `index` is less than the capacity.
        pub fn unset(self: *Self, index: usize) void {
            self.impl.unset(index);
        }

        /// Toggles the bit at the given index.
        ///
        /// Asserts that `index` is less than the capacity.
        pub fn toggle(self: *Self, index: usize) void {
            self.impl.toggle(index);
        }

        /// Sets or unsets the bit at the given index. The bit is set if `value`
        /// is `true` and unsets the bit if `value` is `false`.
        pub fn setValue(self: *Self, index: usize, value: bool) void {
            self.impl.setValue(index, value);
        }

        /// Sets all the bits in the specified range of indices. If the range is
        /// empty no bits are changed.
        ///
        /// Asserts that `end` is less than or equal to the capacity and that
        /// `start` is less than or equal to `end`.
        pub fn setRange(self: *Self, range: Range) void {
            return self.setRange(range);
        }

        /// Clears all the bits in the specified range of indices. If the range
        /// is empty no bits are changed.
        ///
        /// Asserts that `end` is less than or equal to the capacity and that
        /// `start` is less than or equal to `end`.
        pub fn unsetRange(self: *Self, range: Range) void {
            return self.unsetRange(range);
        }

        /// Toggles all the bits in the specified range of indices. If the range
        /// is empty no bits are changed.
        ///
        /// Asserts that `end` is less than or equal to the capacity and that
        /// `start` is less than or equal to `end`.
        pub fn toggleRange(self: *Self, range: Range) void {
            return self.toggleRange(range);
        }

        /// Sets or clears all the bits in the specified range of indices. If
        /// the range is empty no bits are changed.
        ///
        /// The range of bits is set if `value` is `true` and cleared if `value`
        /// is `false`.
        ///
        /// Asserts that `end` is less than or equal to the capacity and that
        /// `start` is less than or equal to `end`.
        pub fn setRangeValue(self: *Self, range: Range, value: bool) void {
            self.impl.setRangeValue(range, value);
        }

        /// Sets all the bits of the bitset
        pub fn setAll(self: *Self) void {
            self.impl.setAll();
        }

        /// Un-sets all the bits of the bitset
        pub fn unsetAll(self: *Self) void {
            self.impl.unsetAll();
        }

        /// Toggles all the bits of the bitset
        pub fn toggleAll(self: *Self) void {
            self.impl.toggleAll();
        }

        /// Sets all the bits that are set in `other`
        pub fn setUnion(self: *Self, other: Self) void {
            self.impl.setUnion(other.impl);
        }

        /// Un-sets the bits not set in `other`
        pub fn setIntersection(self: *Self, other: Self) void {
            self.impl.setIntersection(other.impl);
        }

        /// Toggles the bits of self that are `other` has set
        pub fn toggleSet(self: *Self, other: Self) void {
            self.impl.toggleSet(other.impl);
        }

        /// Returns the *complement* of this bit set - that is a bit set with
        /// all of the bits toggled
        pub fn complement(self: Self) Self {
            return .{ .impl = self.impl.complement() };
        }

        /// Returns a bit set containing all of the set bits of both bit sets
        pub fn unionWith(lhs: Self, rhs: Self) Self {
            return .{ .impl = lhs.unionWith(rhs) };
        }

        /// Returns a bit set containing only the set bits that are set by both
        /// sets.
        pub fn intersectWith(lhs: Self, rhs: Self) Self {
            return .{ .impl = lhs.intersectWith(rhs) };
        }

        /// Returns a bit set containing only the set bits that were set in
        /// *either* `lhs` or `rhs` but not both.
        pub fn xorWith(lhs: Self, rhs: Self) Self {
            return .{ .impl = lhs.xorWith(rhs) };
        }
        /// Returns a bit set containing the bits that are both **set** in `lhs`
        /// and **not set** in `rhs`
        pub fn differenceWith(lhs: Self, rhs: Self) Self {
            return .{ .impl = lhs.differenceWith(rhs) };
        }

        /// `true` if the only set bits of this bitset are also set in `other`
        pub fn subsetOf(lhs: Self, rhs: Self) bool {
            return lhs.intersectWith(rhs).eql(lhs);
        }

        /// Equivalent to `other.subset(self)`
        pub fn supersetOf(lhs: Self, rhs: Self) bool {
            return rhs.subsetOf(lhs);
        }

        /// Returns the index of the first set bit
        pub fn findFirstSet(self: Self) ?usize {
            return self.impl.findFirstSet();
        }

        /// Toggles the first set bit
        pub fn toggleFirstSet(self: *Self) ?usize {
            return self.impl.toggleFirstSet();
        }

        /// Determines if this bitset is equal to the other bitset
        pub fn eql(lhs: Self, rhs: Self) bool {
            return bit_len == 0 or lhs.impl.eql(rhs.impl);
        }

        /// Formats the bit set as a binary, octal, or hexadecimal integer.
        /// Width fill format options are supported
        ///
        /// Supported format specifiers are `{}`, `{b}`, `{o}`, `{x}`, `{X}`
        /// with `{}` being the same as `{b}` and the rest having the same
        /// meaning as when formatting integers.
        pub fn format(
            self: Self,
            comptime spec: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) @TypeOf(writer).Error!void {
            return try self.impl.format(spec, options, writer);
        }

        /// Returns an iterator over the bits of this bitset
        pub fn iterator(
            self: *const Self,
            comptime options: IteratorOptions,
        ) Iterator(options) {
            return .{ .impl = self.impl.iterator(options) };
        }

        pub fn Iterator(comptime options: IteratorOptions) type {
            const IterImpl = Impl.Iterator(options);

            return struct {
                impl: IterImpl,

                pub fn next(it: *@This()) ?usize {
                    return it.impl.next();
                }
            };
        }
    };
}

/// A bitset backed by an integer of exactly `bit_len` bits.
///
/// Performance of this type of bit set is generally best when `bit_len` is less
/// than or equal to `@bitSizeOf(usize)`, and degrades as `bit_len` exceeds the
/// bit size of `usize` and is especially noticeable in debug builds.
// TODO: Verify performance
pub fn IntBitSet(comptime bit_len: u16) type {
    return IntBitSetEx(.{ .bit_len = bit_len });
}

pub const IntBitSetConfig = struct {
    /// Number of bits the bit set may hold
    bit_len: u16,
    /// Backing integer type. If `null` then an unsigned integer of `bit_len`
    /// bits is used.
    ///
    /// This configuration option is intended to allow customization of the
    /// bit-set's alignment.
    backing_type: ?type = null,
};

/// A bitset `config.bit_len` bits backed by a `config.backing_type` integer.
/// If `config.backing_type` is `null` then an unsigned integer of exactly
/// `config.bit_len` bits is used.
///
/// Performance of this type of bit set is generally best when `bit_len` is less
/// than or equal to `@bitSizeOf(usize)`, and degrades as `bit_len` exceeds the
/// bit size of `usize` and is especially noticeable in debug builds.
// TODO: Verify performance
pub fn IntBitSetEx(comptime config: IntBitSetConfig) type {
    const Backing = config.backing_type orelse std.meta.Int(.unsigned, config.bit_len);
    return packed struct(Backing) {
        const Self = @This();
        mask: MaskInt = 0,
        __padding: if (Backing == MaskInt) u0 else std.meta.Int(.unsigned, @bitSizeOf(Backing) - config.bit_len) = 0,

        pub const MaskInt = std.meta.Int(.unsigned, config.bit_len);
        pub const ShiftInt = std.math.Log2Int(MaskInt);

        /// Creates a bit set with all bits *set*
        pub fn initFull() Self {
            return .{ .mask = comptime boolMask(Backing, true) };
        }

        pub fn fromInt(int: MaskInt) Self {
            return .{ .mask = int };
        }

        /// Returns the number of bits there are in this bit set.
        ///
        /// This value is comptime known and the same for all bit sets of the
        /// same type.
        pub inline fn capacity(self: Self) comptime_int {
            _ = self;
            return config.bit_len;
        }

        const Count = std.math.IntFittingRange(0, config.bit_len);

        /// Returns the number of set bits.
        pub fn count(self: Self) Count {
            return @popCount(self.mask);
        }

        /// Returns `true` if no bits are set and `false` otherwise
        pub fn isEmpty(self: Self) bool {
            return self.mask == 0;
        }

        /// Returns `true` if all bits are set and `false` otherwise.
        pub fn isFull(self: Self) bool {
            return self.mask == std.math.maxInt(MaskInt);
        }

        /// `true` if the bit at the given index is set.
        ///
        /// Asserts that `index` is less than the capacity.
        pub fn isSet(self: Self, index: usize) bool {
            return (self.mask & maskBit(index)) != 0;
        }

        /// Sets the bit at the given index.
        ///
        /// Asserts that `index` is less than the capacity.
        pub fn set(self: *Self, index: usize) void {
            self.mask |= maskBit(index);
        }

        /// Clears the bit at the given index.
        ///
        /// Asserts that `index` is less than the capacity.
        pub fn unset(self: *Self, index: usize) void {
            self.mask &= ~maskBit(index);
        }

        /// Toggles the bit at the given index.
        ///
        /// Asserts that `index` is less than the capacity.
        pub fn toggle(self: *Self, index: usize) void {
            self.mask ^= maskBit(index);
        }

        /// Sets or unsets the bit at the given index. The bit is set if `value`
        /// is `true` and unsets the bit if `value` is `false`.
        pub fn setValue(self: *Self, index: usize, value: bool) void {
            const bit = maskBit(index);
            const new_state = bit & boolMask(MaskInt, value);
            self.mask = (self.mask & ~bit) | new_state;
        }

        /// Sets all the bits in the specified range of indices. If the range is
        /// empty no bits are changed.
        ///
        /// Asserts that `end` is less than or equal to the capacity and that
        /// `start` is less than or equal to `end`.
        pub fn setRange(self: *Self, range: Range) void {
            return self.setRangeValueImpl(range, .set);
        }

        /// Clears all the bits in the specified range of indices. If the range
        /// is empty no bits are changed.
        ///
        /// Asserts that `end` is less than or equal to the capacity and that
        /// `start` is less than or equal to `end`.
        pub fn unsetRange(self: *Self, range: Range) void {
            return self.setRangeValueImpl(range, .unset);
        }

        /// Toggles all the bits in the specified range of indices. If the range
        /// is empty no bits are changed.
        ///
        /// Asserts that `end` is less than or equal to the capacity and that
        /// `start` is less than or equal to `end`.
        pub fn toggleRange(self: *Self, range: Range) void {
            return self.setRangeValueImpl(range, .toggle);
        }

        /// Sets or clears all the bits in the specified range of indices. If
        /// the range is empty no bits are changed.
        ///
        /// The range of bits is set if `value` is `true` and cleared if `value`
        /// is `false`.
        ///
        /// Asserts that `end` is less than or equal to the capacity and that
        /// `start` is less than or equal to `end`.
        pub fn setRangeValue(self: *Self, range: Range, value: bool) void {
            return if (value)
                self.setRangeValueImpl(range, .set)
            else
                self.setRangeValueImpl(range, .unset);
        } //0011110011001111 - IntBitSet
        //0001110001101111 - Array/DynBitSet
        //       876543210

        fn setRangeValueImpl(
            self: *Self,
            range: Range,
            comptime mode: SetRangeMode,
        ) void {
            const start = range.start;
            const end = range.end orelse config.bit_len;
            assert(end <= config.bit_len);
            assert(start <= end);
            if (start == end) return;
            const start_bit: ShiftInt = @intCast(start);

            var mask = boolMask(MaskInt, true) << start_bit;
            if (end != config.bit_len) {
                const shift: ShiftInt = @truncate(@bitSizeOf(MaskInt) - end);
                mask &= boolMask(MaskInt, true) >> shift;
            }

            switch (mode) {
                .set => self.mask |= mask,
                .unset => self.mask &= ~mask,
                .toggle => self.mask ^= mask,
            }
        }

        /// Sets all the bits of the bitset
        pub fn setAll(self: *Self) void {
            self.mask = ~@as(MaskInt, 0);
        }

        /// Un-sets all the bits of the bitset
        pub fn unsetAll(self: *Self) void {
            self.mask = 0;
        }

        /// Toggles all the bits of the bitset
        pub fn toggleAll(self: *Self) void {
            self.mask = ~self.mask;
        }

        /// Sets all the bits that are set in `other`
        pub fn setUnion(self: *Self, other: Self) void {
            self.mask |= other.mask;
        }

        /// Un-sets the bits not set in `other`
        pub fn setIntersection(self: *Self, other: Self) void {
            self.mask &= other.mask;
        }

        /// Toggles the bits of self that are `other` has set
        pub fn toggleSet(self: *Self, other: Self) void {
            self.mask ^= other.mask;
        }

        /// Returns the *complement* of this bit set - that is a bit set with
        /// all of the bits toggled
        pub fn complement(self: Self) Self {
            return .{ .mask = ~self.mask };
        }

        /// Returns a bit set containing all of the set bits of both bit sets
        pub fn unionWith(rhs: Self, lhs: Self) Self {
            return .{ .mask = rhs.mask | lhs.mask };
        }

        /// Returns a bit set containing only the set bits that are set by both
        /// sets.
        pub fn intersectWith(lhs: Self, rhs: Self) Self {
            return .{ .mask = lhs.mask & rhs.mask };
        }

        /// Returns a bit set containing only the set bits that were set in
        /// *either* `lhs` or `rhs` but not both.
        pub fn xorWith(lhs: Self, rhs: Self) Self {
            return .{ .mask = lhs.mask ^ rhs.mask };
        }
        /// Returns a bit set containing the bits that are both **set** in `lhs`
        /// and **not set** in `rhs`
        pub fn differenceWith(lhs: Self, rhs: Self) Self {
            return .{ .mask = lhs.mask & ~rhs.mask };
        }

        /// `true` if the only set bits of this bitset are also set in `other`
        pub fn subsetOf(lhs: Self, rhs: Self) bool {
            return lhs.intersectWith(rhs).eql(lhs);
        }

        /// Equivalent to `other.subset(self)`
        pub fn supersetOf(lhs: Self, rhs: Self) bool {
            return rhs.subsetOf(lhs);
        }

        /// Returns the index of the first set bit
        pub fn findFirstSet(self: Self) ?usize {
            const mask = self.mask;
            if (mask == 0) return null;
            return @ctz(mask);
        }

        /// Toggles the first set bit
        pub fn toggleFirstSet(self: *Self) ?usize {
            const mask = self.mask;
            if (mask == 0) return null;
            self.mask = mask & (mask -% 1);
            return @ctz(mask);
        }

        /// Determines if this bitset is equal to the other bitset
        pub fn eql(lhs: Self, rhs: Self) bool {
            return config.bit_len == 0 or lhs.mask == rhs.mask;
        }

        /// Formats the bit set as a binary, octal, or hexadecimal integer.
        /// Width fill format options are supported
        ///
        /// Supported format specifiers are `{}`, `{b}`, `{o}`, `{x}`, `{X}`
        /// with `{}` being the same as `{b}` and the rest having the same
        /// meaning as when formatting integers.
        pub fn format(
            self: Self,
            comptime spec: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) @TypeOf(writer).Error!void {
            var buffer: [config.bit_len]u8 = undefined;
            const fmt = comptime bitSetFormat(spec) catch std.fmt.invalidFmtError(spec, self);
            var fbs = std.io.fixedBufferStream(&buffer);
            std.fmt.formatInt(
                self.mask,
                fmt.base,
                fmt.case,
                .{
                    .fill = '0',
                    .alignment = .right,
                    .width = comptime width: {
                        var count_writer = std.io.countingWriter(std.io.null_writer);
                        std.fmt.formatInt(switch (@typeInfo(MaskInt).Int.signedness) {
                            .signed => std.math.minInt(MaskInt),
                            .unsigned => std.math.maxInt(MaskInt),
                        }, fmt.base, fmt.case, .{}, count_writer.writer()) catch unreachable;
                        break :width @as(usize, @intCast(count_writer.bytes_written));
                    },
                },
                fbs.writer(),
            ) catch unreachable;
            return std.fmt.formatBuf(fbs.getWritten(), options, writer);
        }

        /// Returns an iterator over the bits of this bitset
        pub fn iterator(
            self: Self,
            comptime options: IteratorOptions,
        ) Iterator(options) {
            const word = switch (options.kind) {
                .set => self.mask,
                .unset => ~self.mask,
            };
            return .{ .bits = word };
        }

        pub fn Iterator(comptime options: IteratorOptions) type {
            return WordIterator(MaskInt, options.direction);
        }

        fn maskBit(index: usize) MaskInt {
            assert(index < config.bit_len);
            if (MaskInt == u0) return 0;
            return @as(MaskInt, 1) << @as(ShiftInt, @intCast(index));
        }
    };
}

/// A bitset implemented using an array of `MaskType` integers. Can be used to
/// construct bit sets significantly larger than `IntBitSet`, due to not needing
/// to be backed by a single integer
pub fn ArrayBitSet(comptime MaskType: type, comptime bit_len: comptime_int) type {
    if (bit_len < 0) @compileError("Negative ArrayBitSet 'bit_len'");
    if (@typeInfo(MaskType) != .Int) @compileError("Mask type of ArrayBitSet must be an integer");
    if (@sizeOf(MaskType) == 0 and bit_len != 0)
        @compileError("Mask type of ArrayBitSet may not be zero sized");
    return extern struct {
        pub const MaskInt = MaskType;
        pub const ShiftInt = std.math.Log2Int(MaskInt);

        const full_mask: MaskInt = boolMask(MaskInt, true);
        const mask_len: comptime_int = @bitSizeOf(MaskInt);
        const num_masks = if (mask_len == 0) 0 else dyn.numMasks(MaskType, bit_len);
        const last_pad_bits: comptime_int = (mask_len * num_masks) - bit_len;
        /// Mask of valid bits in the last mask.
        ///
        /// `ArrayBitSet` will ensure that the invalid bits of the final mask
        /// are zero internally.
        pub const last_item_mask: MaskInt = ~@as(MaskInt, 0) >> last_pad_bits;
        const max_int = (1 << bit_len) - 1;

        /// Mask integers used to implement the bit set.
        masks: [num_masks]MaskInt = if (num_masks == 0) .{} else [_]MaskInt{0} ** num_masks,

        const Self = @This();

        /// Creates a bit set with all bits *set*
        pub fn initFull() Self {
            if (num_masks == 0) return .{};
            const mask = comptime boolMask(MaskInt, true);
            return .{ .masks = .{mask} ** num_masks };
        }

        /// Returns the number of bits there are in this bit set.
        ///
        /// This value is comptime known and the same for all bit sets of the
        /// same type.
        pub inline fn capacity(self: Self) comptime_int {
            _ = self;
            return bit_len;
        }

        pub const Count = std.math.IntFittingRange(0, mask_len);

        /// Returns the number of set bits.
        pub fn count(self: Self) Count {
            var total: usize = 0;
            for (self.masks) |mask| total += @popCount(mask);
            return total;
        }
        /// Returns `true` if no bits are set and `false` otherwise
        pub fn isEmpty(self: Self) bool {
            if (bit_len == 0) return true;
            std.mem.allEqual(
                MaskInt,
                self.masks[0 .. num_masks - 1],
                0,
            ) and self.masks[num_masks - 1] & last_item_mask == 0;
        }
        /// Returns `true` if all bits are set and `false` otherwise.
        pub fn isFull(self: Self) bool {
            if (bit_len == 0) return true;
            return std.mem.allEqual(
                MaskInt,
                self.masks[0 .. num_masks - 1],
                full_mask,
            ) and self.masks[num_masks - 1] & last_item_mask == last_item_mask;
        }
        /// `true` if the bit at the given index is set.
        ///
        /// Asserts that `index` is less than the capacity.
        pub fn isSet(self: Self, index: usize) bool {
            assert(index < bit_len);
            if (num_masks == 0) return false;
            return (self.masks[maskIndex(index)] & maskBit(index)) != 0;
        }
        /// Sets the bit at the given index.
        ///
        /// Asserts that `index` is less than the capacity.
        pub fn set(self: *Self, index: usize) void {
            assert(index < bit_len);
            if (num_masks == 0) return;
            self.masks[maskIndex(index)] |= maskBit(index);
        }
        /// Clears the bit at the given index.
        ///
        /// Asserts that `index` is less than the capacity.
        pub fn unset(self: *Self, index: usize) void {
            assert(index < bit_len);
            if (num_masks == 0) return;
            self.masks[maskIndex(index)] &= ~maskBit(index);
        }
        /// Toggles the bit at the given index.
        ///
        /// Asserts that `index` is less than the capacity.
        pub fn toggle(self: *Self, index: usize) void {
            assert(index < bit_len);
            if (num_masks == 0) return;
            self.masks[maskIndex(index)] ^= maskBit(index);
        }
        /// Sets or unsets the bit at the given index. The bit is set if `value`
        /// is `true` and unsets the bit if `value` is `false`.
        pub fn setValue(self: *Self, index: usize, value: bool) void {
            assert(index < bit_len);
            if (num_masks == 0) return;
            const shift = maskBitShift(index);
            const bit: MaskInt = 1 << shift;
            const mask_index = maskIndex(index);
            const new_bit = @as(MaskInt, @intFromBool(value)) << shift;
            self.masks[mask_index] = (self.masks[mask_index] & ~bit) | new_bit;
        }
        /// Sets all the bits in the specified range of indices. If the range is
        /// empty no bits are changed.
        ///
        /// Asserts that `end` is less than or equal to the capacity and that
        /// `start` is less than or equal to `end`.
        pub fn setRange(self: *Self, range: Range) void {
            self.setRangeValueImpl(range, .set);
        }
        /// Clears all the bits in the specified range of indices. If the range
        /// is empty no bits are changed.
        ///
        /// Asserts that `end` is less than or equal to the capacity and that
        /// `start` is less than or equal to `end`.
        pub fn unsetRange(self: *Self, range: Range) void {
            self.setRangeValueImpl(range, .unset);
        }
        /// Toggles all the bits in the specified range of indices. If the range
        /// is empty no bits are changed.
        ///
        /// Asserts that `end` is less than or equal to the capacity and that
        /// `start` is less than or equal to `end`.
        pub fn toggleRange(self: *Self, range: Range) void {
            self.setRangeValueImpl(range, .toggle);
        }
        /// Sets or clears all the bits in the specified range of indices. If
        /// the range is empty no bits are changed.
        ///
        /// The range of bits is set if `value` is `true` and cleared if `value`
        /// is `false`.
        ///
        /// Asserts that `end` is less than or equal to the capacity and that
        /// `start` is less than or equal to `end`.
        pub fn setRangeValue(self: *Self, range: Range, value: bool) void {
            if (value)
                self.setRange(range)
            else
                self.unsetRange(range);
        }

        fn setRangeValueImpl(
            self: *Self,
            range: Range,
            comptime mode: SetRangeMode,
        ) void {
            const start = range.start;
            const end = range.end orelse bit_len;
            assert(end <= bit_len);
            assert(start <= end);
            if (num_masks == 0) return;
            if (start == end) return;
            assert(end != 0);

            const start_mask_index = maskIndex(start);
            const start_bit = maskBitShift(start);
            const end_mask_index = maskIndex(end);
            const end_bit = maskBitShift(end);

            const start_mask = full_mask << start_bit;
            const end_mask = full_mask >> @as(ShiftInt, @truncate(@as(std.math.Log2IntCeil(MaskInt), mask_len) - end_bit));

            if (start_mask_index == end_mask_index) {
                const mask: MaskInt = start_mask & end_mask;
                switch (mode) {
                    .set => self.masks[start_mask_index] |= mask,
                    .unset => self.masks[start_mask_index] &= ~mask,
                    .toggle => self.masks[start_mask_index] ^= mask,
                }
            } else {
                var i = start_mask_index;
                if (start_bit > 0) {
                    switch (mode) {
                        .set => self.masks[i] |= start_mask,
                        .unset => self.masks[i] &= ~start_mask,
                        .toggle => self.masks[i] ^= start_mask,
                    }
                    i += 1;
                }

                for (self.masks[i..end_mask_index]) |*mask| {
                    switch (mode) {
                        .set => mask.* = full_mask,
                        .unset => mask.* = 0,
                        .toggle => mask.* = ~mask.*,
                    }
                }

                if (end_bit > 0) {
                    assert(end_mask_index == i);
                    assert(end_mask_index < num_masks);

                    switch (mode) {
                        .set => self.masks[i] |= end_mask,
                        .unset => self.masks[i] &= ~end_mask,
                        .toggle => self.masks[i] ^= end_mask,
                    }
                }
            }
        }

        /// Sets all the bits of the bitset
        pub fn setAll(self: *Self) void {
            if (num_masks == 0) return;
            if (last_pad_bits == 0) {
                @memset(&self.masks, full_mask);
            } else {
                if (num_masks > 1) {
                    @memset(self.masks[0 .. num_masks - 1], full_mask);
                }
                self.masks[num_masks - 1] = last_item_mask;
            }
        }
        /// Un-sets all the bits of the bitset
        pub fn unsetAll(self: *Self) void {
            if (num_masks == 0) return;
            @memset(&self.masks, 0);
        }
        /// Toggles all the bits of the bitset
        pub fn toggleAll(self: *Self) void {
            if (num_masks == 0) return;
            if (last_pad_bits == 0) {
                for (&self.masks) |*mask| {
                    mask.* = ~mask;
                }
            } else {
                for (self.masks[0 .. num_masks - 1]) |*mask| {
                    mask.* = ~mask;
                }
                self.masks[num_masks - 1] ^= last_item_mask;
            }
        }
        /// Sets all the bits that are set in `other`
        pub fn setUnion(self: *Self, other: Self) void {
            for (&self.masks, other.masks) |*mask, union_bits| {
                mask.* |= union_bits;
            }
        }
        /// Un-sets the bits not set in `other`
        pub fn setIntersection(self: *Self, other: Self) void {
            for (&self.masks, other.masks) |*mask, intersect| {
                mask.* &= intersect;
            }
        }
        /// Toggles the bits of self that are `other` has set
        pub fn toggleSet(self: *Self, other: Self) void {
            for (&self.masks, other.masks) |*mask, toggles| {
                mask.* = toggles;
            }
        }
        /// Returns the *complement* of this bit set - that is a bit set with
        /// all of the bits toggled
        pub fn complement(self: Self) Self {
            var tmp = self;
            tmp.toggleAll();
            return tmp;
        }
        /// Returns a bit set containing all of the set bits of both bit sets
        pub fn unionWith(lhs: Self, rhs: Self) Self {
            var tmp = lhs;
            tmp.setUnion(rhs);
            return tmp;
        }
        /// Returns a bit set containing only the set bits that were set in
        /// *either* `lhs` or `rhs` but not both.
        pub fn xorWith(lhs: Self, rhs: Self) Self {
            var tmp = lhs;
            tmp.toggleSet(rhs);
        }

        pub fn intersectWith(lhs: Self, rhs: Self) Self {
            var tmp = lhs;
            tmp.setIntersection(rhs);
            return tmp;
        }

        /// Returns a bit set containing the bits that are both **set** in `lhs`
        /// and **not set** in `rhs`
        pub fn differenceWith(lhs: Self, rhs: Self) Self {
            var result = lhs;
            result.setIntersection(rhs.complement());
            return result;
        }
        /// `true` if the only set bits of this bitset are also set in `other`
        pub fn subsetOf(lhs: Self, rhs: Self) bool {
            return lhs.intersectWith(rhs).eql(lhs);
        }
        /// Equivalent to `other.subset(self)`
        pub fn supersetOf(lhs: Self, rhs: Self) bool {
            return rhs.subsetOf(lhs);
        }
        /// Returns the index of the first set bit
        pub fn findFirstSet(self: Self) ?usize {
            var offset: usize = 0;
            const mask = for (self.masks) |mask| {
                if (mask != 0) break mask;
                offset += @bitSizeOf(MaskInt);
            } else return null;
            return offset + @ctz(mask);
        }
        /// Toggles the first set bit
        pub fn toggleFirstSet(self: *Self) ?usize {
            var i: usize = 0;
            const mask = for (&self.masks) |*mask| {
                if (mask.* != 0) break mask;
                i += @bitSizeOf(MaskInt);
            } else return;
            i += @ctz(mask.*);
            mask.* &= mask.* - 1;
            return i;
        }
        /// Determines if this bitset is equal to the other bitset
        pub fn eql(lhs: Self, rhs: Self) bool {
            var i: usize = 0;
            return while (i < num_masks) : (i += 1) {
                if (lhs.masks[i] != rhs.masks[i]) {
                    break false;
                }
            } else true;
        }
        /// Formats the bit set as a binary, octal, or hexadecimal integer.
        /// Width fill format options are supported
        ///
        /// Supported format specifiers are `{}`, `{b}`, `{o}`, `{x}`, `{X}`
        /// with `{}` being the same as `{b}` and the rest having the same
        /// meaning as when formatting integers.
        pub fn format(
            self: Self,
            comptime spec: []const u8,
            options: std.fmt.FormatOptions,
            writer: std.io.AnyWriter,
        ) @TypeOf(writer).Error!void {
            const fmt = comptime bitSetFormat(spec) catch std.fmt.invalidFmtError(spec, self);
            try formatBitArray(MaskInt, &self.masks, bit_len, fmt, options, writer);
        }
        /// Returns an iterator over the bits of this bitset
        pub fn iterator(
            self: *const Self,
            comptime options: IteratorOptions,
        ) Iterator(options) {
            return Iterator(options).init(&self.masks, last_item_mask);
        }

        pub fn Iterator(comptime options: IteratorOptions) type {
            return BitArrayIterator(MaskInt, options);
        }

        inline fn maskBitShift(index: usize) ShiftInt {
            return @truncate(index);
        }
        fn maskBit(index: usize) MaskInt {
            return @as(MaskInt, 1) << @as(ShiftInt, @truncate(index));
        }
        fn maskIndex(index: usize) usize {
            return index >> @bitSizeOf(ShiftInt);
        }
        fn boolMaskBit(index: usize, value: bool) MaskInt {
            return @as(MaskInt, @intFromBool(value)) << @as(ShiftInt, @intCast(index));
        }
    };
}

pub const dyn = @import("bit_set/dyn.zig");

/// A dynamically sized bit set, backed by a dynamically allocated a array of
/// `MaskType` integers. This is basically an `ArrayBitSet` with a dynamic size.
///
/// `DynBitSet` internally maintins all the information needed to
pub fn DynBitSet(comptime T: type) type {
    if (@typeInfo(T) != .Int or @sizeOf(T) == 0) {
        @compileError("Invalid dynamic bitset mask type: '" ++ @typeName(T) ++ "'");
    }
    return struct {
        pub const MaskInt = T;
        pub const ShiftInt = std.math.Log2Int(T);
        const Self = @This();

        pub const Unmanaged = DynBitSetUnmanaged(T);
        pub const Raw = RawDynBitSet(T);

        unmanaged: Unmanaged,
        allocator: Allocator,

        pub fn init(allocator: Allocator) Self {
            return .{
                .unmanaged = Unmanaged.initEmpty(),
                .allocator = allocator,
            };
        }

        pub fn initValue(
            allocator: Allocator,
            bit_len: usize,
            fill_value: bool,
        ) Allocator.Error!Self {
            return initBufferValue(
                allocator,
                try allocator.alloc(MaskInt, dyn.numMasks(MaskInt, bit_len)),
                bit_len,
                fill_value,
            );
        }

        pub fn initBufferValue(
            allocator: Allocator,
            buffer: []MaskInt,
            bit_len: usize,
            fill_value: bool,
        ) Self {
            var self = fromOwnedSlice(allocator, buffer, bit_len);
            self.setRangeValue(.{}, fill_value);
            return self;
        }

        pub fn fromOwnedSlice(
            allocator: Allocator,
            mask_slice: []MaskInt,
            bit_len: usize,
        ) Self {
            assert(mask_slice.len * @bitSizeOf(MaskInt) >= bit_len);
            return .{
                .unmanaged = .{
                    .masks = mask_slice,
                    .bit_len = bit_len,
                },
                .allocator = allocator,
            };
        }

        pub fn toUnmanaged(self: *Self) Unmanaged {
            defer self.unmanaged = Unmanaged.initEmpty();
            return self.unmanaged;
        }

        pub fn toRaw(self: *Self) Raw {
            defer self.unmanaged = Unmanaged.initEmpty();
            return self.unmanaged.toRaw();
        }

        pub inline fn capacity(self: *Self) usize {
            return self.bit_len;
        }

        pub fn isSet(self: Self, index: usize) bool {
            return self.unmanaged.isSet(index);
        }

        pub fn set(self: Self, index: usize) void {
            self.unmanaged.set(index);
        }

        pub fn unset(self: Self, index: usize) void {
            self.unmanaged.unset(index);
        }

        pub fn toggle(self: Self, index: usize) void {
            self.unmanaged.toggle(index);
        }
        pub fn setValue(self: Self, index: usize, value: bool) void {
            self.unmanaged.setValue(index, value);
        }

        pub fn setRange(self: Self, range: Range) void {
            self.unmanaged.setRange(range);
        }

        pub fn unsetRange(self: Self, range: Range) void {
            self.unmanaged.unsetRange(range);
        }

        pub fn toggleRange(self: Self, range: Range) void {
            self.unmanaged.toggleRange(range);
        }

        pub fn setRangeValue(self: Self, range: Range, value: bool) void {
            self.unmanaged.setRangeValue(range, value);
        }

        pub fn setAll(self: Self) void {
            self.unmanaged.setAll();
        }

        pub fn unsetAll(self: Self) void {
            self.unmanaged.unsetAll();
        }

        pub fn toggleAll(self: Self) void {
            self.unmanaged.toggleAll();
        }

        pub fn setUnion(self: Self, other: Self) void {
            self.unmanaged.setUnion(other.unmanaged);
        }

        pub fn setIntersection(self: Self, other: Self) void {
            self.unmanaged.setIntersection(other.unmanaged);
        }

        pub fn toggleSet(self: Self, other: Self) void {
            self.unmanaged.toggleSet(other.unmanaged);
        }

        pub fn subsetOf(self: Self, other: Self) bool {
            return self.unmanaged.subsetOf(other.unmanaged);
        }

        pub fn supersetOf(self: Self, other: Self) bool {
            return other.subsetOf(self);
        }

        pub fn eql(self: Self, other: Self) bool {
            return self.unmanaged.eql(other.unmanaged);
        }

        pub fn iterator(self: *const Self, comptime options: IteratorOptions) Iterator(options) {
            return self.unmanaged.iterator(options);
        }

        /// Formats the bit set as a binary, octal, or hexadecimal integer.
        /// Width fill format options are supported
        ///
        /// Supported format specifiers are `{}`, `{b}`, `{o}`, `{x}`, `{X}`
        /// with `{}` being the same as `{b}` and the rest having the same
        /// meaning as when formatting integers.
        pub fn format(
            self: Self,
            comptime spec: []const u8,
            options: std.fmt.FormatOptions,
            writer: std.io.AnyWriter,
        ) @TypeOf(writer).Error!void {
            return self.unmanaged.format(spec, options, writer);
        }

        pub const Iterator = Unmanaged.Iterator;
    };
}

pub fn DynBitSetUnmanaged(comptime T: type) type {
    if (@typeInfo(T) != .Int or @sizeOf(T) == 0) {
        @compileError("Invalid dynamic bitset mask type: '" ++ @typeName(T) ++ "'");
    }
    return struct {
        pub const MaskInt = T;
        pub const ShiftInt = std.math.Log2Int(T);
        pub const Managed = DynBitSet(T);
        pub const Raw = RawDynBitSet(T);
        const Self = @This();
        masks: []T,
        bit_len: usize,

        pub fn initEmpty() Self {
            return .{
                .masks = undefined,
                .bit_len = 0,
            };
        }

        pub fn initSet(allocator: Allocator, bit_len: usize) Allocator.Error!Self {
            return initComptimeValue(allocator, bit_len, true);
        }

        pub fn initUnset(allocator: Allocator, bit_len: usize) Allocator.Error!Self {
            return initComptimeValue(allocator, bit_len, false);
        }

        pub fn initComptimeValue(
            allocator: Allocator,
            bit_len: usize,
            comptime value: bool,
        ) Allocator.Error!Self {
            var self = fromOwnedSlice(
                try allocator.alloc(T, dyn.numMasks(T, bit_len)),
                bit_len,
            );
            if (value) self.setAll() else self.unsetAll();
            return self;
        }

        pub fn initValue(
            allocator: Allocator,
            bit_len: usize,
            fill_value: bool,
        ) Allocator.Error!Self {
            return initBufferValue(
                try allocator.alloc(MaskInt, dyn.numMasks(MaskInt, bit_len)),
                bit_len,
                fill_value,
            );
        }

        pub fn initBufferValue(buffer: []MaskInt, bit_len: usize, fill_value: bool) Self {
            var self = fromOwnedSlice(buffer, bit_len);
            self.setRangeValue(.{}, fill_value);
            return self;
        }

        pub fn fromOwnedSlice(mask_slice: []MaskInt, bit_len: usize) Self {
            assert(mask_slice.len * @bitSizeOf(MaskInt) >= bit_len);
            return .{
                .masks = mask_slice,
                .bit_len = bit_len,
            };
        }

        pub fn toManaged(self: *Self, allocator: Allocator) Managed {
            defer self.* = .{ .masks = &.{}, .bit_len = 0 };
            return .{
                .unmanaged = self.*,
                .allocator = allocator,
            };
        }

        pub fn toRaw(self: *Self) Raw {
            defer self.* = .{
                .masks = &.{},
                .bit_len = 0,
            };

            return .{
                .masks = self.masks.ptr,
                .bit_len = self.bit_len,
            };
        }

        pub inline fn capacity(self: *Self) usize {
            return self.bit_len;
        }

        pub fn isSet(self: Self, index: usize) bool {
            assert(index < self.bit_len);
            return dyn.isSet(T, self.masks.ptr, index);
        }

        pub fn set(self: Self, index: usize) void {
            assert(index < self.bit_len);
            dyn.set(T, self.masks.ptr, index);
        }

        pub fn unset(self: Self, index: usize) void {
            assert(index < self.bit_len);
            dyn.unset(T, self.masks.ptr, index);
        }

        pub fn toggle(self: Self, index: usize) void {
            assert(index < self.bit_len);
            dyn.toggle(T, self.masks.ptr, index);
        }
        pub fn setValue(self: Self, index: usize, value: bool) void {
            assert(index < self.bit_len);
            dyn.setValue(T, self.masks.ptr, index, value);
        }

        pub fn setRange(self: Self, range: Range) void {
            dyn.setRange(T, self.masks.ptr, self.bit_len, range);
        }

        pub fn unsetRange(self: Self, range: Range) void {
            dyn.unsetRange(T, self.masks.ptr, self.bit_len, range);
        }

        pub fn toggleRange(self: Self, range: Range) void {
            dyn.toggleRange(T, self.masks.ptr, self.bit_len, range);
        }

        pub fn setRangeValue(self: Self, range: Range, value: bool) void {
            dyn.setRangeValue(T, self.masks.ptr, self.bit_len, range, value);
        }

        pub fn setAll(self: Self) void {
            dyn.setAll(T, self.masks.ptr, self.bit_len);
        }

        pub fn unsetAll(self: Self) void {
            dyn.setAll(T, self.masks.ptr, self.bit_len);
        }

        pub fn toggleAll(self: Self) void {
            dyn.toggleAll(T, self.masks.ptr, self.bit_len);
        }

        pub fn setUnion(self: Self, other: Self) void {
            assert(self.bit_len == other.bit_len);
            dyn.setUnion(T, self.masks.ptr, other.masks.ptr, self.bit_len);
        }

        pub fn setIntersection(self: Self, other: Self) void {
            assert(self.bit_len == other.bit_len);
            dyn.setIntersection(T, self.masks.ptr, other.masks.ptr, self.bit_len);
        }

        pub fn toggleSet(self: Self, other: Self) void {
            assert(self.bit_len == other.bit_len);
            dyn.toggleSet(T, self.masks.ptr, other.masks.ptr, self.bit_len);
        }

        pub fn subsetOf(self: Self, other: Self) bool {
            return self.bit_len == other.bit_len and dyn.isSubset(T, self.masks.ptr, other.masks.ptr, self.bit_len);
        }

        pub fn supersetOf(self: Self, other: Self) bool {
            return other.subsetOf(self);
        }

        pub fn eql(self: Self, other: Self) bool {
            return self.bit_len == other.bit_len and dyn.eql(T, self.masks.ptr, other.masks.ptr, self.bit_len);
        }

        pub fn iterator(self: *const Self, comptime options: IteratorOptions) Iterator(options) {
            const num_masks = dyn.numMasks(T, self.bit_len);
            const padding = num_masks * @bitSizeOf(T) - self.bit_len;
            const last_item_mask = ~@as(T, 0) >> @as(ShiftInt, @intCast(padding));
            return Iterator(options).init(self.masks[0..num_masks], last_item_mask);
        }

        /// Formats the bit set as a binary, octal, or hexadecimal integer.
        /// Width fill format options are supported
        ///
        /// Supported format specifiers are `{}`, `{b}`, `{o}`, `{x}`, `{X}`
        /// with `{}` being the same as `{b}` and the rest having the same
        /// meaning as when formatting integers.
        pub fn format(
            self: Self,
            comptime spec: []const u8,
            options: std.fmt.FormatOptions,
            writer: std.io.AnyWriter,
        ) @TypeOf(writer).Error!void {
            const fmt = comptime bitSetFormat(spec) catch std.fmt.invalidFmtError(spec, self);
            try formatBitArray(T, self.masks.ptr, self.bit_len, fmt, options, writer);
        }

        pub const Iterator = Raw.Iterator;
    };
}

/// An even lighter weight version `DynBitSetUnmanaged`. `RawDynBitSet` only
/// tracks the `masks` pointer and the bit length.
pub fn RawDynBitSet(comptime T: type) type {
    if (@typeInfo(T) != .Int or @sizeOf(T) == 0) {
        @compileError("Invalid dynamic bitset mask type: '" ++ @typeName(T) ++ "'");
    }
    return extern struct {
        pub const MaskInt = T;
        pub const ShiftInt = std.math.Log2Int(T);
        const Self = @This();
        pub const Unmanaged = DynBitSetUnmanaged(T);
        pub const Managed = DynBitSet(T);
        masks: [*]T,
        bit_len: usize,

        pub fn init() Self {
            return .{
                .masks = undefined,
                .bit_len = 0,
            };
        }

        pub fn initBufferValue(buffer: []MaskInt, bit_len: usize, fill_value: bool) Self {
            var self = fromOwnedSlice(buffer, bit_len);
            self.setRangeValue(.{}, fill_value);
            return self;
        }

        pub fn fromOwnedSlice(mask_slice: []MaskInt, bit_len: usize) Self {
            assert(mask_slice.len * @bitSizeOf(MaskInt) >= bit_len);
            return .{
                .masks = mask_slice.ptr,
                .bit_len = bit_len,
            };
        }

        pub fn toUnmanaged(self: *Self, mask_buffer_len: usize) Unmanaged {
            defer self.* = .{ .masks = undefined, .bit_len = 0 };
            return .{
                .masks = self.masks[0..mask_buffer_len],
                .bit_len = self.bit_len,
            };
        }

        pub fn toManaged(self: *Self, allocator: Allocator, mask_buffer_len: usize) Managed {
            return .{
                .unmanaged = self.toUnmanaged(mask_buffer_len),
                .allocator = allocator,
            };
        }

        pub inline fn capacity(self: *Self) usize {
            return self.bit_len;
        }

        pub fn isSet(self: Self, index: usize) bool {
            assert(index < self.bit_len);
            return dyn.isSet(T, self.masks, index);
        }

        pub fn set(self: Self, index: usize) void {
            assert(index < self.bit_len);
            dyn.set(T, self.masks, index);
        }

        pub fn unset(self: Self, index: usize) void {
            assert(index < self.bit_len);
            dyn.unset(T, self.masks, index);
        }

        pub fn toggle(self: Self, index: usize) void {
            assert(index < self.bit_len);
            dyn.toggle(T, self.masks, index);
        }
        pub fn setValue(self: Self, index: usize, value: bool) void {
            assert(index < self.bit_len);
            dyn.setValue(T, self.masks, index, value);
        }

        pub fn setRange(self: Self, range: Range) void {
            dyn.setRange(T, self.masks, self.bit_len, range);
        }

        pub fn unsetRange(self: Self, range: Range) void {
            dyn.unsetRange(T, self.masks, self.bit_len, range);
        }

        pub fn toggleRange(self: Self, range: Range) void {
            dyn.toggleRange(T, self.masks, self.bit_len, range);
        }

        pub fn setRangeValue(self: Self, range: Range, value: bool) void {
            dyn.setRangeValue(T, self.masks, self.bit_len, range, value);
        }

        pub fn setAll(self: Self) void {
            dyn.setAll(T, self.masks, self.bit_len);
        }

        pub fn unsetAll(self: Self) void {
            dyn.setAll(T, self.masks, self.bit_len);
        }

        pub fn toggleAll(self: Self) void {
            dyn.toggleAll(T, self.masks, self.bit_len);
        }

        pub fn setUnion(self: Self, other: Self) void {
            assert(self.bit_len == other.bit_len);
            dyn.setUnion(T, self.masks, other.masks, self.bit_len);
        }

        pub fn setIntersection(self: Self, other: Self) void {
            assert(self.bit_len == other.bit_len);
            dyn.setIntersection(T, self.masks, other.masks, self.bit_len);
        }

        pub fn toggleSet(self: Self, other: Self) void {
            assert(self.bit_len == other.bit_len);
            dyn.toggleSet(T, self.masks, other.masks, self.bit_len);
        }

        pub fn subsetOf(self: Self, other: Self) bool {
            return self.bit_len == other.bit_len and dyn.isSubset(T, self.masks, other.masks, self.bit_len);
        }

        pub fn supersetOf(self: Self, other: Self) bool {
            return other.subsetOf(self);
        }

        pub fn eql(self: Self, other: Self) bool {
            return self.bit_len == other.bit_len and dyn.eql(T, self.masks, other.masks, self.bit_len);
        }

        pub fn iterator(self: *const Self, comptime options: IteratorOptions) Iterator(options) {
            const num_masks = dyn.numMasks(T, self.bit_len);
            const padding = num_masks * @bitSizeOf(T) - self.bit_len;
            const last_item_mask = ~@as(T, 0) >> @as(ShiftInt, @intCast(padding));
            return Iterator(options).init(self.masks[0..num_masks], last_item_mask);
        }

        /// Formats the bit set as a binary, octal, or hexadecimal integer.
        /// Width fill format options are supported
        ///
        /// Supported format specifiers are `{}`, `{b}`, `{o}`, `{x}`, `{X}`
        /// with `{}` being the same as `{b}` and the rest having the same
        /// meaning as when formatting integers.
        pub fn format(
            self: Self,
            comptime spec: []const u8,
            options: std.fmt.FormatOptions,
            writer: std.io.AnyWriter,
        ) @TypeOf(writer).Error!void {
            const fmt = comptime bitSetFormat(spec) catch std.fmt.invalidFmtError(spec, self);
            try formatBitArray(T, self.masks, self.bit_len, fmt, options, writer);
        }

        pub fn Iterator(comptime options: IteratorOptions) type {
            return BitArrayIterator(MaskInt, options);
        }
    };
}

const SetRangeMode = enum { set, unset, toggle };

fn WordIterator(comptime MaskInt: type, direction: IteratorOptions.Direction) type {
    return struct {
        const Self = @This();
        bits: MaskInt,
        const ShiftInt = std.math.Log2Int(MaskInt);
        const bit_len = @bitSizeOf(MaskInt);
        pub fn next(it: *Self) ?usize {
            if (it.bits == 0) return null;
            switch (direction) {
                .forward => {
                    const index = @ctz(it.bits);
                    it.bits &= it.bits - 1;
                    return index;
                },
                .reverse => {
                    const lead_zeros: ShiftInt = @clz(it.bits);
                    const c: ShiftInt = comptime @intCast(bit_len - 1);
                    const top_bit: ShiftInt = c - lead_zeros;
                    it.bits &= (@as(MaskInt, 1) << top_bit) - 1;
                    return top_bit;
                },
            }
        }
    };
}

fn BitArrayIterator(comptime MaskInt: type, comptime options: IteratorOptions) type {
    const ShiftInt = std.math.Log2Int(MaskInt);
    const kind = options.kind;
    const direction = options.direction;
    return struct {
        const Self = @This();

        current_word: MaskInt,
        remaining_words: []const MaskInt,
        bit_offset: usize,
        last_word_mask: MaskInt,

        fn init(masks: []const MaskInt, last_word_mask: MaskInt) Self {
            if (masks.len == 0) {
                return .{
                    .bit_offset = 0,
                    .bits = 0,
                    .words = &.{},
                    .last_word_mask = last_word_mask,
                };
            } else {
                var it = Self{
                    .current_word = 0,
                    .remaining_words = masks,
                    .last_word_mask = last_word_mask,
                    .bit_offset = switch (direction) {
                        .forward => 0,
                        .reverse => (masks.len - 1) * @bitSizeOf(MaskInt),
                    },
                };
                it.nextWord(true);
                return it;
            }
        }

        pub fn next(self: *Self) ?usize {
            while (self.bits_remain == 0) {
                if (self.words_remain.len == 0) return null;
                self.nextWord(false);
                switch (direction) {
                    .forward => self.bit_offset += @bitSizeOf(MaskInt),
                    .reverse => self.bit_offset -= @bitSizeOf(MaskInt),
                }
            }

            switch (direction) {
                .forward => {
                    const next_index = @ctz(self.bits_remain) + self.bit_offset;
                    self.bits_remain &= self.bits_remain - 1;
                    return next_index;
                },
                .reverse => {
                    const leading_zeroes = @clz(self.bits_remain);
                    const top_bit = (@bitSizeOf(MaskInt) - 1) - leading_zeroes;
                    const no_top_bit_mask = (@as(MaskInt, 1) << @as(ShiftInt, @intCast(top_bit))) - 1;
                    self.bits_remain &= no_top_bit_mask;
                    return top_bit + self.bit_offset;
                },
            }
        }

        fn nextWord(it: *Self, comptime is_first: bool) void {
            var word = switch (direction) {
                .forward => it.remaining_words[0],
                .reverse => it.remaining_words[it.remaining_words.len - 1],
            };
            switch (kind) {
                .set => {},
                .unset => {
                    word = ~word;
                    if ((direction == .reverse and is_first) or
                        (direction == .forward and it.remaining_words.len == 1))
                    {
                        word &= it.last_word_mask;
                    }
                },
            }
            switch (direction) {
                .forward => it.remaining_words = it.remaining_words[1..],
                .reverse => it.remaining_words.len -= 1,
            }
            it.current_word = word;
        }
    };
}

const BitSetFormat = struct {
    base: Base = .bin,
    case: std.fmt.Case = .lower,

    pub fn format(
        fmt: BitSetFormat,
        comptime spec: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        if (spec.len != 0) std.fmt.invalidFmtError(spec, fmt);
        const base: u8 = switch (fmt.case) {
            .upper => 'A',
            .lower => 'a',
        };
        const offset: u8 = switch (fmt.base) {
            .bin => 'b' - 'a',
            .oct => 'o' - 'a',
            .hex => 'x' - 'a',
        };

        const buf = [_]u8{ '{', base + offset, '}' };

        try std.fmt.formatBuf(&buf, options, writer);
    }

    fn apply(comptime fmt: BitSetFormat, int: anytype, writer: anytype) @TypeOf(writer).Error!void {
        try std.fmt.formatInt(int, @intFromEnum(fmt.base), fmt.case, .{
            .width = fmt.base.width(@typeInfo(@TypeOf(int)).Int.bits),
            .alignment = .right,
            .fill = '0',
        }, writer);
    }

    const Base = enum(u5) {
        bin = 2,
        oct = 8,
        hex = 16,

        inline fn width(base: Base, bit_count: usize) usize {
            const bits_per_digit = switch (base) {
                .bin => 1,
                .oct => 3,
                .hex => 4,
            };
            const max_digits = (bit_count / bits_per_digit) + @intFromBool(bit_count % bits_per_digit != 0);

            return @max(max_digits, 1);
        }

        test width {
            const testWidth = struct {
                fn f(bit_count: usize, expected_widths: [3]usize) !void {
                    try testing.expectEqual(expected_widths[0], width(.bin, bit_count));
                    try testing.expectEqual(expected_widths[1], width(.oct, bit_count));
                    try testing.expectEqual(expected_widths[2], width(.hex, bit_count));
                }
            }.f;

            // Always need at least one digit
            try testWidth(0, .{1} ** 3);

            // Test "critical points" where the number of required digits changes
            try testWidth(2, .{ 2, 1, 1 });
            try testWidth(3, .{ 3, 1, 1 });
            try testWidth(4, .{ 4, 2, 1 });
            try testWidth(5, .{ 5, 2, 2 });
            try testWidth(6, .{ 6, 2, 2 });
            try testWidth(7, .{ 7, 3, 2 });
            try testWidth(8, .{ 8, 3, 2 });
            try testWidth(9, .{ 9, 3, 3 });

            // Sanity check for larger values. If the previous checks pass, then
            // these should as well.
            try testWidth(16, .{ 16, 6, 4 });
            try testWidth(32, .{ 32, 11, 8 });
        }
    };
};

fn bitSetFormat(fmt: []const u8) error{InvalidFormat}!BitSetFormat {
    if (fmt.len == 0) return .{};
    if (fmt.len != 1) return error.InvalidFormat;
    switch (fmt[0]) {
        'b' => return .{ .base = .bin },
        'o' => return .{ .base = .oct },
        'x' => return .{ .base = .hex },
        'X' => return .{ .base = .hex, .case = .lower },
        else => return error.InvalidFormat,
    }
}

inline fn formatBitArray(
    comptime T: type,
    bits: [*]const T,
    bit_len: usize,
    comptime fmt: BitSetFormat,
    options: std.fmt.FormatOptions,
    writer: anytype,
) @TypeOf(writer).Error!void {
    const num_masks = dyn.numMasks(T, bit_len);
    if (num_masks == 0) {
        return try std.fmt.formatBuf("0", options, writer);
    }

    const fmt_width = fmt.base.width(bit_len);

    var pre: usize = 0;
    var post: usize = 0;
    var fill = std.BoundedArray(u8, 4){};

    if (options.width) |width| padding_setup: {
        if (width <= fmt_width) break :padding_setup;
        const total_padding = width - fmt_width;

        switch (options.alignment) {
            .left => pre = total_padding,
            .right => post = total_padding,
            .center => {
                const half = total_padding / 2;
                pre = half;
                post = half;
                // account for 'total_padding / 2' truncating when total_padding is odd
                post += (total_padding & 1);
            },
        }

        if (std.unicode.utf8Encode(options.fill, &fill.buffer)) |len| {
            fill.len = len;
        } else |_| {
            const replacement_character_bytes = comptime std.unicode.utf8EncodeComptime(
                std.unicode.replacement_character,
            );
            const len = replacement_character_bytes.len;
            fill.len = len;
            fill.buffer[0..len].* = replacement_character_bytes;
        }
    }

    try writer.writeBytesNTimes(fill.constSlice(), pre);

    const trail_bits = bit_len % @bitSizeOf(T);

    var i = num_masks;

    if (trail_bits != 0) {
        i - 1;
        const last_word_mask = @as(T, ~@as(T, 0) << @as(std.math.Log2Int(T), @truncate(@bitSizeOf(T) - trail_bits)));
        try std.fmt.formatInt(
            bits[num_masks - 1] & last_word_mask,
            @intFromEnum(fmt.base),
            fmt.case,
            .{ .fill = '-', .width = trail_bits, .alignment = .right },
            writer,
        );
    }

    while (i != 0) {
        i -= 1;
        const word = word: {
            @setRuntimeSafety(false);
            break :word bits[i];
        };
        try fmt.apply(word, writer);
    }

    try writer.writeBytesNTimes(fill.constSlice(), post);
}

inline fn rangeMask(
    comptime MaskInt: type,
    start_bit: std.math.Log2Int(MaskInt),
    end_bit: std.math.Log2IntCeil(MaskInt),
) MaskInt {
    @setRuntimeSafety(false);
    const ShiftInt = @TypeOf(start_bit);
    const full_mask: MaskInt = comptime boolMask(MaskInt, true);
    const lower = full_mask << start_bit;
    const upper = full_mask >> @as(ShiftInt, @intCast((@bitSizeOf(MaskInt) - 1) - (end_bit - 1)));
    return @as(MaskInt, lower & upper);
}

const testing = std.testing;


test IntBitSet {
    const BitSet = IntBitSet(16);
    var bits = BitSet{};

    try testing.expectEqual(0, bits.mask);

    bits.set(1);
    bits.set(6);
    bits.set(15);
    bits.setRange(.{ .start = 16 });

    try testing.expectEqual(0b10000000_01000010, bits.mask);

    bits.setRange(.{ .end = 7 });
    bits.toggleRange(.{ .end = 4 });

    try testing.expectEqual(0b10000000_01110000, bits.mask);

    bits.setUnion(BitSet.fromInt(0b00111110_00000000));

    try testing.expectEqual(0b10111110_01110000, bits.mask);

    try testing.expectEqual(~bits.mask, bits.complement().mask);

    bits.unsetAll();

    try testing.expectEqual(0, bits.mask);

    bits.setAll();

    try testing.expectEqual(std.math.maxInt(u16), bits.mask);
}

test "[Array/Dyn]BitSet.format" {
    var int = IntBitSet(16){};
    var bit_set = ArrayBitSet(u8, 16){};

    comptime assert(@TypeOf(bit_set).num_masks == 2);

    bit_set.setRange(.{ .end = 8 });

    bit_set.setRange(.{ .start = 10, .end = 14 });

    bit_set.unsetRange(.{ .start = 4, .end = 6 });
    int.setRange(.{ .start = 0, .end = 8 });
    int.setRange(.{ .start = 10, .end = 14 });
    int.unsetRange(.{ .start = 4, .end = 6 });

    var buffer: [16]u8 = undefined;

    try testing.expectFmt(
        std.fmt.bufPrint(buffer[0..], "{b:0>16}", .{int.mask}) catch unreachable,
        "{}",
        .{bit_set},
    );
    try testing.expectFmt(
        std.fmt.bufPrint(buffer[0..], "{x:0>4}", .{int.mask}) catch unreachable,
        "{x}",
        .{bit_set},
    );
}
