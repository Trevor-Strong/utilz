const std = @import("std");
const assert = std.debug.assert;
const meta = @import("../meta.zig");

pub const Alignment = enum(std.math.Log2Int(usize)) {
    @"1" = 0,
    @"2" = 1,
    @"4" = 2,
    @"8" = 3,
    @"16" = 4,
    /// Static page size aligned. Actually defined as `page_size_min`
    page_size = std.math.log2(std.heap.page_size_min),
    _,

    pub const pointer: Alignment = .of(usize);

    /// Twice the alignment of a pointer
    pub const double_word: Alignment = @enumFromInt(@intFromEnum(pointer) + 1);

    pub fn of(comptime T: type) Alignment {
        return .fromByteUnits(@alignOf(T));
    }

    pub fn fromByteUnits(n: usize) Alignment {
        assert(std.math.isPowerOfTwo(n));
        return @enumFromInt(@ctz(n));
    }

    pub inline fn toByteUnits(alignment: Alignment) usize {
        return @as(usize, 1) << @intFromEnum(alignment);
    }

    pub inline fn mask(alignment: Alignment) usize {
        return alignment.toByteUnits() - 1;
    }

    pub fn order(lhs: Alignment, rhs: Alignment) bool {
        return std.math.order(@intFromEnum(lhs), @intFromEnum(rhs));
    }

    pub inline fn compare(
        lhs: Alignment,
        op: std.math.CompareOperator,
        rhs: Alignment,
    ) bool {
        if (meta.isComptimeKnown(op)) {
            return switch (op) {
                .eq => lhs == rhs,
                .neq => lhs != rhs,
                .gt => @intFromEnum(lhs) > @intFromEnum(rhs),
                .lt => @intFromEnum(lhs) < @intFromEnum(rhs),
                .gte => @intFromEnum(lhs) >= @intFromEnum(rhs),
                .lte => @intFromEnum(lhs) <= @intFromEnum(rhs),
            };
        } else {
            return std.math.compare(@intFromEnum(lhs), op, @intFromEnum(rhs));
        }
    }

    pub fn max(lhs: Alignment, rhs: Alignment) Alignment {
        return @enumFromInt(@max(@intFromEnum(lhs), @intFromEnum(rhs)));
    }

    pub fn min(lhs: Alignment, rhs: Alignment) Alignment {
        return @enumFromInt(@min(@intFromEnum(lhs), @intFromEnum(rhs)));
    }

    /// Return the next address after `addr` that is aligned to `alignment`. If
    /// `addr` is aligned to `alignment`, then `addr` is returned.
    ///
    /// `addr <= alignment.forward(addr) < addr + alignment.toByteUnits()`
    pub fn forward(alignment: Alignment, addr: usize) usize {
        const m = alignment.toByteUnits() - 1;
        return (addr + m) & m;
    }

    /// Return the next address after `addr` that is aligned to `alignment`. If
    /// `addr` is aligned to `alignment`, then `addr` is returned.
    ///
    /// `addr - alignment.toByteUnits() < alignment.forward(addr) <= addr`
    pub fn backward(alignment: Alignment, addr: usize) usize {
        return addr & ~alignment.mask();
    }

    /// Return weather `addr` is aligned to `alignment`.
    pub fn isAligned(alignment: Alignment, addr: usize) bool {
        return addr & alignment.mask() == 0;
    }
};

/// Gets the child type of the potentially optional pointer type `T`. If `T` is
/// not an optional, this function is equivalent to `@typeInfo(T).pointer.child`
pub fn Child(comptime T: type) type {
    return getChildOrElem(T, .child).?;
}

/// Gets the element type of the indexable pointer type `T`. An *indexable*
/// *pointer type* is a slice, C pointer, many item pointer, single item pointer
/// to array, or an optional of those types.
///
/// When `T` **is not** a pointer to an array or an optional pointer to an
/// array, this function is equivalent to `Child(T)`. When `T` **is** a pointer
/// to an array or an optional pointer to an array, this function returns the
/// child type of the array.
pub fn Elem(comptime T: type) type {
    return getChildOrElem(T, .elem).?;
}

pub fn ElemPtr(comptime T: type) type {
    return getElemSinglePtr(T).?;
}

fn getChildOrElem(comptime T: type, comptime mode: enum { child, elem }) ?type {
    const ptr_info = switch (@typeInfo(T)) {
        .pointer => |ptr_info| ptr_info,
        .optional => |opt_info| switch (@typeInfo(opt_info.child)) {
            .pointer => |ptr_info| ptr_info,
            else => return null,
        },
        else => return null,
    };

    if (mode == .elem and ptr_info.size == .one) switch (@typeInfo(ptr_info.child)) {
        .array => |arr_info| return arr_info.child,
        else => unreachable,
    };

    return ptr_info.child;
}

pub fn getElemSinglePtr(comptime T: type) ?type {
    var ptr_info, const is_optional = switch (@typeInfo(T)) {
        .pointer => |ptr_info| .{ ptr_info, false },
        .optional => |opt_info| switch (@typeInfo(opt_info.child)) {
            .pointer => |ptr_info| .{ ptr_info, true },
            else => return null,
        },
        else => return null,
    };
    switch (ptr_info.size) {
        .one => switch (@typeInfo(ptr_info.child)) {
            .array => |array_info| {
                ptr_info.child = array_info.child;
            },
            else => return null,
        },
        .c, .many, .slice => {
            ptr_info.size = .one;
            ptr_info.sentinel_ptr = null;
        },
    }
    const Ptr = @Type(.{ .pointer = ptr_info });
    return if (is_optional) ?Ptr else Ptr;
}

/// Wrapper around `@alignCast` that allows the alignment to be explicitly
/// specified. Most useful in generic code where it would be difficult to name
/// the aligned pointer type.
pub inline fn alignCast(new_alignment: ?Alignment, ptr: anytype) SetAlignment(@TypeOf(ptr), new_alignment) {
    return @alignCast(ptr);
}

/// Creates a pointer type with the same attributes as `T` except that the
/// alignment of the resulting pointer type is `new_alignment`
pub fn SetAlignment(comptime T: type, comptime new_alignment: ?Alignment) type {
    const C = Child(T);
    const alignment = new_alignment orelse return SetAlignment(T, .of(C));
    return Rebind(T, C, .{ .allow_optional = .yes, .alignment = .exactly(alignment.toByteUnits()) });
}

/// A `@ptrCast` that is guaranteed to only change the size of the pointer type.
/// (i.e. `*T` to `[*]T`).
///
/// This function is identical to `@as(SetSize(@TypeOf(ptr), new_size), @ptrCast(ptr))`.
pub inline fn sizeCast(ptr: anytype, comptime new_size: std.builtin.Type.Pointer.Size) WithSize(@TypeOf(ptr), new_size) {
    return @ptrCast(ptr);
}

pub fn WithSize(comptime T: type, new_size: std.builtin.Type.Pointer.Size) type {
    switch (@typeInfo(T)) {
        .pointer => |ptr_info| {
            var new_ptr_info = ptr_info;
            new_ptr_info.size = new_size;
            return @Type(.{ .pointer = new_ptr_info });
        },
        .optional => |opt_info| switch (@typeInfo(opt_info.child)) {
            .pointer => |ptr_info| {
                var new_ptr_info = ptr_info;
                new_ptr_info.size = new_size;
                return ?@Type(.{ .pointer = new_ptr_info });
            },
            else => unreachable,
        },
        else => unreachable,
    }
}

/// Coerces `ptr` to an equivalent pointer type as `@TypeOf(ptr)` except that
/// it is `const`.
///
/// This is effectively the inverse of `@constCast` which does the opposite of
/// this (removes `const`)
pub inline fn asConst(ptr: anytype) AddConst(@TypeOf(ptr)) {
    return ptr;
}

pub fn AddConst(comptime T: type) type {
    var info: Info = .of(T);
    info.is_const = true;
    return info.Type();
}

pub fn RemoveConst(comptime T: type) type {
    var info: Info = .of(T);
    info.is_const = true;
    return info.Type();
}

/// Coerces `ptr` to an equivalent pointer type as `@TypeOf(ptr)` except that
/// it is `volatile`.
///
/// This is effectively the inverse of `@volatileCast` which does the opposite
/// of this (removes `volatile`)
pub inline fn asVolatile(ptr: anytype) AddVolatile(@TypeOf(ptr)) {
    return ptr;
}

pub fn AddVolatile(comptime T: type) type {
    var info: Info = .of(T);
    info.is_volatile = true;
    return info.Type();
}

pub fn RemoveVolatile(comptime T: type) type {
    var info: Info = .of(T);
    info.is_volatile = false;
    return info.Type();
}

/// A form of `childCast` that additionally requires that `NewChild` and the
/// current child type of `ptr` are the same size. If this is not the case, a
/// compile error is raised instead.
///
/// If `Tptr`'s child type is an `opaque` type or `noreturn`, then `NewChild`
/// must also an `opaque` type or `noreturn`. `noreturn` and `opaque` types are
/// counted as having a *null* size, since  using `@sizeOf()` on them normally
/// causes a compile error. Any additional types with this property should be
/// treated as having the same "size" as `noreturn` and `opaque` types.
pub inline fn childBitCast(
    comptime NewChild: type,
    ptr: anytype,
) WithSameSizeChild(@TypeOf(ptr), NewChild) {
    return @ptrCast(ptr);
}

/// Same as `WithChild()` except that this function asserts that `T`'s child
/// type and `NewChild` are the same size.
///
/// If `T`'s child is an `opaque` type or `noreturn`, then `NewChild` must also
/// an `opaque` type or `noreturn`. `noreturn` and `opaque` types are counted as
/// having a *null* size, since  using `@sizeOf()` on them normally causes a
/// compile error. Any additional types with this property should be treated as
/// having the same "size" as `noreturn` and `opaque` types
pub fn WithSameSizeChild(comptime T: type, comptime NewChild: type) type {
    assert(meta.sizeOf(NewChild) == meta.sizeOf(Child(T))); // Types must have the same size
    return WithChild(T, NewChild);
}

/// A `@ptrCast` that is guaranteed to only change the pointer's child type
pub inline fn childCast(comptime NewChild: type, ptr: anytype) WithChild(@TypeOf(ptr), NewChild) {
    return @ptrCast(ptr);
}

/// Returns a new pointer type that is the same as `T` except that the child
/// type is `NewChild`.
pub fn WithChild(comptime T: type, comptime NewChild: type) type {
    var info: Info = .of(T);
    info.child = NewChild;
    return info.Type();
}

/// A `@ptrCast` that is guaranteed to only change the pointer's sentinel. Works
/// on many pointers, slices, and pointers to arrays. `ptr` may be optional.
pub inline fn sentinelCast(ptr: anytype, comptime new_sentinel: ?Elem(@TypeOf(ptr))) SetSentinel(@TypeOf(ptr), new_sentinel) {
    return @ptrCast(ptr);
}

/// Returns a type with the sentinel value of the pointer type `T` set to
/// `new_sentinel`. If `new_sentinel` is `null` then the resulting type has no
/// sentinel terminator.
///
/// Works on slices, many pointers, pointers to arrays, and optionals of those
/// types. For pointers to arrays, the array type's sentinel is adjusted instead
/// of the pointer's. If `T` is none of the listed type a compile error is
/// raised.
pub fn SetSentinel(comptime T: type, comptime new_sentinel: ?Elem(T)) type {
    var ptr_info = switch (@typeInfo(T)) {
        .pointer => |ptr_info| ptr_info,
        .optional => |opt_info| {
            // @typeInfo(opt_info.child) == .pointer is guaranteed. `Elem()` would
            // raise a compile error first if this is not the case
            return ?SetSentinel(opt_info.child, new_sentinel);
        },
        else => unreachable, // Expected pointer or optional pointer
    };
    const sentinel_ptr: ?*const anyopaque = if (new_sentinel) |value| &value else null;
    switch (ptr_info.size) {
        .many, .slice => {
            // Sanity check
            comptime assert(Elem(T) == ptr_info.child);
            ptr_info.sentinel_ptr = sentinel_ptr;
        },
        .one => {
            // This shouldn't generate a compile error ever. If `T` *is* a
            // single item pointer to anything other than an array, `Elem()`
            // should fail first.
            var array_info = @typeInfo(ptr_info.child).array;

            // Sanity check
            comptime assert(array_info.child == Elem(T));

            array_info.sentinel_ptr = sentinel_ptr;
            ptr_info.child = @Type(.{ .array = array_info });
        },
        .c => unreachable, // Sentinels not supported on C pointers
    }

    return @Type(.{ .pointer = ptr_info });
}

pub const RebindOptions = struct {
    /// Child of the input pointer type. If non-`null` must equal the child type
    /// of input pointer else a compile error is raised
    old_child: ?type = null,
    /// Alignment requirements of the original type.
    required_alignment: RequiredAlignment = .any,
    /// Alignment of the resulting type. `null` means to use whatever the
    /// original
    alignment: ?NewAlignment = null,
    /// The sentinel of the resulting type.
    new_sentinel: NewSentinel = .inherit_unless_different,
    /// When (or if) optional pointers should be allowed
    allow_optional: AllowOptionalMode = .no,
    /// The size that each input pointer size should map to
    size: Sizes = .passthrough,
    /// List of allowed address spaces. An empty list means any address space is
    /// allowed.
    allowed_addrspace: []const std.builtin.AddressSpace = &.{},
    /// Address Space of the resulting pointer. If `null`, the address space of
    /// the input pointer is used.
    address_space: ?std.builtin.AddressSpace = null,
    /// Is the `Ptr` argument allowed to be `const`?
    allow_const: bool = true,
    /// Is the `Ptr` argument allowed to be `volatile`?
    allow_volatile: bool = true,
    /// Is the `Ptr` argument allowed to be `allowzero`?
    allow_allowzero: bool = true,

    /// Should the resulting pointer be `const`. If `null`, the const-ness of
    /// the input pointer is used.
    is_const: ?bool = null,
    /// Should the resulting pointer be `volatile`. If `null`, the volatility of
    /// the input pointer is used.
    is_volatile: ?bool = null,
    /// Should the resulting pointer be `allowzero`. If `null`, the resulting
    /// pointer is `allowzero` if the input pointer is `allowzero`.
    is_allowzero: ?bool = null,
    /// Should the resulting pointer be optional or not. If `null` the resulting
    /// pointer is optional only if the input pointer type is optional.
    is_optional: ?bool = null,

    pub const RequiredAlignment = struct {
        /// Minimum required alignment. `null` means the natural alignment of
        /// of `Ptr`s child type
        min: ?Alignment = .@"1",
        /// Maximum allowed alignment. `null` means the natural alignment of
        /// of `Ptr`s child type
        max: ?Alignment = .fromByteUnits(1 << (@bitSizeOf(usize) - 1)),

        pub const any: RequiredAlignment = .{};

        pub fn atLeast(min: ?Alignment) @This() {
            return .{ .min = min };
        }

        pub fn atMost(max: ?Alignment) @This() {
            return .{ .max = max };
        }

        pub fn exactly(alignment: ?Alignment) @This() {
            return .{ .min = alignment, .max = alignment };
        }

        pub fn range(min: ?Alignment, max: ?Alignment) @This() {
            assert(min == null or max == null or min.?.compare(.gte, max.?));
            return .{ .min = min, .max = max };
        }
    };

    pub const AllowOptionalMode = enum {
        no,
        /// Only allow optional's that represent null by using address `0`.
        ///
        /// This mode disallows any optional pointer types where `@sizeOf(T) != @sizeOf(?T)`
        /// where `?T` is the `Ptr` argument to `Rebind`
        only_addr_0_null,
        yes,
    };

    pub const NewSentinel = union(enum) {
        /// The sentinel of the original pointer type is preserved if the old
        /// child and new child types are identical, otherwise the resulting
        /// type will have no sentinel.
        inherit_unless_different,
        /// Always retain the sentinel of the original type
        inherit,
        /// Use this value as the sentinel of the resulting type.
        ///
        /// `null` means no sentinel
        value: ?*const anyopaque,

        pub const none: NewSentinel = .{ .value = null };
    };

    pub const NewAlignment = struct {
        /// Minimum alignment, `null` means to use the natural alignment of the
        /// new child.
        ///
        /// If the original pointer type has an alignment less than this,
        /// the resulting pointer type will use this alignment instead of
        /// the original type's alignment.
        min: ?Alignment,
        /// Maximum alignment, `null` means to use the natural alignment of the
        /// new child.
        ///
        /// If the original pointer type has an alignment greater than this,
        /// the resulting pointer type will use this alignment instead of
        /// the original type's alignment.
        max: ?Alignment,
        pub const natural: RebindOptions = .exactly(null);

        pub fn between(min: ?Alignment, max: ?Alignment) @This() {
            return .{ .min = min, .max = max };
        }

        pub fn exactly(alignment: ?Alignment) @This() {
            return .between(alignment, alignment);
        }

        pub fn atLeast(alignment: ?Alignment) @This() {
            return .init(alignment, @enumFromInt(std.math.maxInt(@typeInfo(Alignment).@"enum".tag_type)));
        }

        pub fn atMost(alignment: ?Alignment) @This() {
            return .init(.@"1", alignment);
        }
    };

    /// Size of the resulting pointer when the input pointer is the specified
    /// size. If a field is `null`, then that size is not allowed for the input
    /// type.
    pub const Sizes = struct {
        c: ?std.builtin.Type.Pointer.Size = null,
        one: ?std.builtin.Type.Pointer.Size = null,
        many: ?std.builtin.Type.Pointer.Size = null,
        slice: ?std.builtin.Type.Pointer.Size = null,

        pub const passthrough: @This() = .{
            .c = .c,
            .one = .one,
            .many = .many,
            .slice = .slice,
        };

        /// Maps all input pointer sizes to the specified size
        pub fn to(size: std.builtin.Type.Pointer.Size) @This() {
            return .{
                .c = size,
                .one = size,
                .many = size,
                .slice = size,
            };
        }

        pub fn except(size: @This(), drop: struct {
            c: bool = false,
            one: bool = false,
            many: bool = false,
            slice: bool = false,
        }) @This() {
            var tmp = size;
            if (drop.c) tmp.c = null;
            if (drop.one) tmp.one = null;
            if (drop.many) tmp.many = null;
            if (drop.slice) tmp.slice = null;
            return tmp;
        }
    };
};

pub fn Rebind(
    comptime Ptr: type,
    comptime NewChild: type,
    comptime options: RebindOptions,
) type {
    var info: Info = .of(Ptr);
    info.rebind(NewChild, options);
    return info.Type();
}

pub fn makeRebind(comptime NewChild: type, comptime options: RebindOptions) fn (comptime T: type) type {
    return struct {
        fn f(comptime T: type) type {
            return Rebind(T, NewChild, options);
        }
    }.f;
}

/// Coerces `ptr` to a slice. `ptr` must be a slice or a pointer to an array and
/// may be an optional pointer.
pub inline fn asSlice(ptr: anytype) AsSlice(@TypeOf(ptr)) {
    return ptr;
}

pub fn AsSlice(comptime T: type) type {
    return tryAsSlice(T).?;
}

pub fn tryAsSlice(comptime T: type) ?type {
    var ptr_info, const is_optional = switch (@typeInfo(T)) {
        .pointer => |ptr_info| .{ ptr_info, false },
        .optional => |opt_info| switch (opt_info.child) {
            .pointer => |ptr_info| .{ ptr_info, true },
            else => return null,
        },
        else => return null,
    };
    switch (ptr_info.size) {
        .slice => return T,
        .one => switch (@typeInfo(ptr_info.child)) {
            .array => |array_info| {
                ptr_info.size = .slice;
                ptr_info.child = array_info.child;
                ptr_info.sentinel_ptr = array_info.sentinel_ptr;
                const SliceType = @Type(.{ .pointer = ptr_info });
                return if (is_optional) ?SliceType else SliceType;
            },
            else => return null,
        },
        else => return null,
    }
    comptime unreachable;
}

test AsSlice {
    const t = std.testing;
    try t.expectEqual([]u8, AsSlice([]u8));
    try t.expectEqual([]const u8, AsSlice([]const u8));
    try t.expectEqual([]align(32) volatile std.SemanticVersion, AsSlice([]align(32) volatile std.SemanticVersion));
    try t.expectEqual([:0]u8, AsSlice(*[0:0]u8));
    try t.expectEqual([:0]align(4) u8, AsSlice(*align(4) [64:0]u8));
}

pub const Info = struct {
    size: std.builtin.Type.Pointer.Size,
    child: type,
    is_const: bool,
    is_volatile: bool,
    is_allowzero: bool,
    is_optional: bool,
    alignment: Alignment,
    sentinel_ptr: ?*const anyopaque,
    address_space: std.builtin.AddressSpace,

    pub fn init(comptime T: type) ?Info {
        const type_info = @typeInfo(T);
        const ptr_info = switch (type_info) {
            .pointer => |ptr_info| ptr_info,
            .optional => |opt_info| switch (@typeInfo(opt_info.child)) {
                .pointer => |ptr_info| ptr_info,
                else => return null,
            },
            else => return null,
        };
        return .{
            .size = ptr_info.size,
            .child = ptr_info.child,
            .is_const = ptr_info.is_const,
            .is_volatile = ptr_info.is_volatile,
            .is_allowzero = ptr_info.is_allowzero,
            .is_optional = type_info == .optional,
            .alignment = .fromByteUnits(ptr_info.alignment),
            .sentinel_ptr = ptr_info.sentinel_ptr,
            .address_space = ptr_info.address_space,
        };
    }

    pub fn of(comptime T: type) Info {
        return init(T) orelse unreachable; // Expected pointer or optional pointer type
    }

    pub fn sentinel(comptime info: Info) ?info.child {
        const ptr: ?*const info.child = @ptrCast(@alignCast(info.sentinel_ptr));
        return if (ptr) |p| p.* else null;
    }

    pub fn isNullable(comptime info: Info) bool {
        return info.is_optional or info.allowsAddressZero();
    }

    pub fn allowsAddressZero(comptime info: Info) bool {
        return info.is_allowzero or info.size == .c;
    }

    pub fn isThin(comptime info: Info) bool {
        return switch (info.size) {
            .c => !info.is_optional,
            .slice => false,
            .one, .many => !info.is_optional or !info.is_allowzero,
        };
    }

    pub fn toSlice(comptime info: Info) Info {
        return info.tryToSlice() catch unreachable;
    }

    pub fn tryToSlice(comptime info: Info) error{NotSliceLike}!Info {
        switch (info.size) {
            .slice => return info,
            .c, .many => return error.NotSliceLike,
            .one => switch (@typeInfo(info.child)) {
                .array => |array_info| {
                    var new_info = info;
                    new_info.child = array_info.child;
                    new_info.sentinel_ptr = array_info.sentinel_ptr;
                    return new_info;
                },
                else => return error.NotSliceLike,
            },
        }
    }

    pub fn Type(comptime info: Info) type {
        const PointerType = @Type(.{
            .pointer = .{
                .size = info.size,
                .child = info.child,
                .is_const = info.is_const,
                .is_volatile = info.is_volatile,
                .is_allowzero = info.is_allowzero,
                .alignment = info.alignment.toByteUnits(),
                .sentinel_ptr = info.sentinel_ptr,
                .address_space = info.address_space,
            },
        });
        return if (info.is_optional) ?PointerType else PointerType;
    }
    pub fn rebind(comptime info: *Info, comptime NewChild: type, comptime options: RebindOptions) !void {
        const OldChild = info.child;

        if (options.old_child) |ExpectedOldChild| {
            if (OldChild != ExpectedOldChild) return error.UnexpectedChildType;
        }

        switch (options.allow_optional) {
            .no => if (info.isNullable()) return error.Nullable,
            .only_addr_0_null => if (info.is_optional and info.allowsAddressZero()) return error.Nullable,
            .yes => {},
        }
        {
            const my_flags = [_]bool{
                info.is_const,
                info.is_volatile,
                info.is_allowzero,
            };
            const allowed_flags = [_]bool{
                options.allow_const,
                options.allow_volatile,
                options.allow_allowzero,
            };
            const Flag = enum {
                @"const",
                @"volatile",
                @"allowzero",
            };
            const flag_names = [_]Flag{ .@"const", .@"volatile", .@"allowzero" };
            for (my_flags, allowed_flags, flag_names) |my_flag, is_allowed, name| {
                if (my_flag and !is_allowed) switch (name) {
                    .@"const" => return error.ConstNotAllowed,
                    .@"volatile" => return error.VolatileNotAllowed,
                    .@"allowzero" => return error.AllowzeroNotAllowed,
                };
            }
        }
        if (options.allowed_addrspace.len > 0) {
            for (options.allowed_addrspace) |address_space| {
                if (info.address_space == address_space) break;
            } else {
                return error.BadAddressSpace;
            }
        }

        var min_alignment, var max_alignment = initAlignmentRange(
            options.required_alignment.min,
            options.required_alignment.max,
            @alignOf(OldChild),
        ) orelse return error.InvalidRequiredAlignmentRange;

        if (info.alignment.compare(.lt, min_alignment) or info.alignment.compare(.gt, max_alignment)) {
            return error.AlignmentOutOfRange;
        }

        const new_size = @as(?std.builtin.Type.Pointer.Size, switch (info.size) {
            .c => options.size.c,
            .one => options.size.one,
            .many => options.size.many,
            .slice => options.size.slice,
        }) orelse return error.BadSize;

        info.child = NewChild;
        info.size = new_size;

        if (options.alignment) |new_alignment| {
            min_alignment, max_alignment = initAlignmentRange(new_alignment.min, new_alignment.max, @alignOf(NewChild));
            info.alignment = info.alignment.max(min_alignment).min(max_alignment);
        }

        switch (options.new_sentinel) {
            .inherit => {},
            .inherit_unless_different => if (NewChild != OldChild) {
                info.sentinel_ptr = null;
            },
            .value => |sentinel_ptr| {
                info.sentinel_ptr = sentinel_ptr;
            },
        }

        if (options.address_space) |address_space| {
            info.address_space = address_space;
        }

        inline for (.{ "is_const", "is_volatile", "is_allowzero", "is_optional" }) |name| {
            if (@field(options, name)) |new| {
                @field(info, name) = new;
            }
        }
    }

    fn initAlignmentRange(opt_min: ?Alignment, opt_max: ?Alignment, natural: Alignment) ?[2]Alignment {
        var min = opt_min orelse natural;
        var max = opt_max orelse natural;
        if (max.compare(.lt, min)) {
            if (opt_max == null) {
                max = min;
            } else if (opt_min == null) {
                min = max;
            } else {
                return null;
            }
        }
        return .{ min, max };
    }
};

test {
    std.testing.refAllDecls(@This());
}
