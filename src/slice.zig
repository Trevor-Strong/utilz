//! Utilities relating to slices

pub const ops = @import("slice/ops.zig");

pub const toSlice = ops.toSlice;
pub const breakAt = ops.breakAt;
pub const splitAt = ops.splitAt;
pub const Break = ops.Break;
pub const window = ops.window;
pub const WindowIterator = ops.WindowIterator;

/// Slice type information.
pub const Slice = struct {
    /// Static length of the original type this `Slice` was constructed from or
    /// `null` if the original type did not have a statically known length
    len: ?usize,
    /// Information on how to convert the original type to a slice.
    conv: Conversion,
    /// The element type of the slice
    child: type,
    /// The sentinel of the slice
    sentinel: ?*const anyopaque,
    /// The alignment of the slice pointer
    alignment: u29,
    /// Address space of the slice pointer
    addr_space: AddressSpace,
    is_const: bool,
    is_volatile: bool,
    is_allowzero: bool,

    pub const Error = error{NotSliceConvertible};

    pub fn fromPtrInfo(comptime ptr_info: PtrInfo) Error!Slice {
        var slice = Slice{
            .alignment = ptr_info.alignment,
            .addr_space = ptr_info.address_space,
            .is_const = ptr_info.is_const,
            .is_volatile = ptr_info.is_volatile,
            .is_allowzero = ptr_info.is_allowzero,
            .len = undefined,
            .child = undefined,
            .sentinel = undefined,
            .conv = undefined,
        };

        switch (ptr_info.size) {
            .Slice => {
                slice.len = null;
                slice.child = ptr_info.child;
                slice.sentinel = ptr_info.sentinel;
                slice.conv = .{};
            },
            .One => switch (@typeInfo(ptr_info.child)) {
                .array => |arr_info| {
                    slice.len = arr_info.len;
                    slice.child = arr_info.child;
                    slice.sentinel = arr_info.sentinel;
                    slice.conv = .{};
                },
                .vector => |vec_info| {
                    const bit_size = @bitSizeOf(vec_info.child);
                    if (bit_size % 8 != 0 or !std.math.isPowerOfTwo(bit_size)) {
                        return error.NotSliceCoercible;
                    }
                    slice.len = vec_info.len;
                    slice.child = vec_info.child;
                    slice.sentinel = null;
                    slice.conv = .{ .vec_to_arr = true };
                },
                else => return error.NotSliceConvertible,
            },
            .Many => if (ptr_info.sentinel) |sentinel| {
                slice.len = null;
                slice.child = ptr_info.child;
                slice.sentinel = sentinel;
                slice.conv = .{ .slice_to = true };
            },
            .C => switch (@typeInfo(ptr_info.child)) {
                .array => |arr_info| {
                    slice.len = arr_info.len;
                    slice.child = arr_info.child;
                    slice.sentinel = arr_info.sentinel;
                    slice.conv = .{ .is_c = true };
                },
                .vector => |vec_info| {
                    slice.len = vec_info.len;
                    slice.child = vec_info.child;
                    slice.conv = .{ .is_c = true, .vec_to_arr = true };
                },
            },
        }
        return slice;
    }

    /// Gets the sentinel value of this `Slice` object.
    pub fn getSentinel(comptime slice: Slice) ?slice.child {
        const Dest = ?*const slice.child;
        const ptr: Dest = @ptrCast(@alignCast(slice.sentinel));
        return if (ptr) |p| p.* else null;
    }

    /// Typed setter for the sentinel value of this `Slice` object.
    pub fn setSentinel(comptime slice: *Slice, comptime new_sentinel: ?slice.child) void {
        const s = new_sentinel orelse {
            slice.sentinel = null;
            return;
        };
        slice.sentinel = &s;
    }

    pub fn canConvert(comptime slice: Slice, comptime opts: struct { fast: bool = false }) bool {
        if (slice.conv.slice_to) {
            return !opts.fast and (slice.sentinel != null or slice.conv.is_c);
        }

        return true;
    }

    pub fn requiresSentinel(comptime slice: Slice) bool {
        return slice.conv.slice_to and !slice.conv.is_c;
    }

    pub fn dropSentinel(comptime slice: *Slice) void {
        slice.sentinel = null;
    }

    pub fn withoutSentinel(comptime slice: Slice) Slice {
        var tmp = slice;
        tmp.dropSentinel();
        return tmp;
    }

    pub fn convert(comptime slice: Slice, from: slice.Original()) slice.Type() {
        if (comptime slice.conv.isDirect()) {
            return from;
        } else if (comptime slice.conv.slice_to) {
            if (slice.sentinel == null and !slice.conv.is_c)
                @compileError("Unable to determine the length of type '" ++
                    @typeName(@TypeOf(from)) ++ "'");

            const sentinel: slice.child = comptime slice.getSentinel() orelse 0;
            return mem.sliceTo(from, sentinel);
        } else {
            return @as(slice.ArrayPtr(), from);
        }
    }

    /// Constructs the array pointer type represented by this `Slice` object.
    ///
    /// Raises a compile error when `slice.len` is `null`
    pub fn ArrayPtr(comptime slice: Slice) type {
        const len = slice.len orelse @compileError("Length not statically known");
        return @Type(.{
            .pointer = .{
                .size = .One,
                .child = if (slice.getSentinel()) |sentinel| [len:sentinel]slice.child else [len]slice.child,
                .sentinel = null,
                .alignment = slice.alignment,
                .address_space = slice.addr_space,
                .is_const = slice.is_const,
                .is_volatile = slice.is_volatile,
                .is_allowzero = slice.is_allowzero,
            },
        });
    }

    /// Constructs the slice type represented by this `Slice` object
    pub fn Type(comptime slice: Slice) type {
        return @Type(slice.typeInfo());
    }

    pub fn typeInfo(comptime slice: Slice) std.builtin.Type {
        return .{
            .pointer = .{
                .child = slice.child,
                .size = .Slice,
                .alignment = slice.alignment,
                .address_space = slice.addr_space,
                .sentinel = slice.sentinel,
                .is_const = slice.is_const,
                .is_volatile = slice.is_volatile,
                .is_allowzero = slice.is_allowzero,
            },
        };
    }

    pub fn Original(comptime slice: Slice) type {
        return @Type(slice.originalInfo());
    }

    pub fn originalInfo(comptime slice: Slice) std.builtin.Type {
        var ptr_info = PtrInfo{
            .child = undefined,
            .size = undefined,
            .sentinel = undefined,
            .alignment = slice.alignment,
            .address_space = slice.addr_space,
            .is_const = slice.is_const,
            .is_volatile = slice.is_volatile,
            .is_allowzero = slice.is_allowzero,
        };

        if (slice.conv.isDirect()) {
            if (slice.len) |arr_len| {
                ptr_info.size = .One;
                ptr_info.child = @Type(.{
                    .array = .{
                        .child = slice.child,
                        .len = arr_len,
                        .sentinel = slice.sentinel,
                    },
                });
                ptr_info.sentinel = null;
            } else {
                ptr_info.size = .Slice;
                ptr_info.child = slice.child;
                ptr_info.sentinel = slice.sentinel;
            }
        } else if (slice.conv.vec_to_arr) {
            ptr_info.child = @Type(.{
                .vector = .{
                    .child = slice.child,
                    .len = slice.len.?,
                },
            });
            ptr_info.size = if (slice.conv.is_c) .C else .One;
            ptr_info.sentinel = null;
            assert(slice.sentinel == null);
        } else if (slice.conv.slice_to) {
            ptr_info.child = slice.child;
            if (slice.conv.is_c) {
                assert(slice.sentinel == null); // sentinel is assumed to be 0
                ptr_info.size = .C;
                ptr_info.sentinel = null;
            } else {
                assert(slice.sentinel != null);
                ptr_info.size = .Many;
                ptr_info.sentinel = slice.sentinel;
            }
        } else { // C pointer to array
            assert(slice.conv == .{ .is_c = true });
            ptr_info.size = .C;
            ptr_info.child = @Type(.{
                .array = .{
                    .len = slice.len.?,
                    .child = slice.child,
                    .sentinel = slice.sentinel,
                },
            });
            ptr_info.sentinel = null;
        }
        return .{ .pointer = ptr_info };
    }

    /// Identifies the "shape" of the type used to construct this `Slice`.
    pub fn originalKind(comptime slice: Slice) OriginalTypeKind {
        if (slice.conv.isDirect()) {
            return if (slice.len == null) .slice else .array_ptr;
        }
        if (slice.conv.is_c) {
            return if (slice.conv.slice_to)
                .c_ptr
            else if (slice.conv.vec_to_arr)
                .vector_c_ptr
            else
                .array_c_ptr;
        } else if (slice.conv.vec_to_arr) {
            return .vector_ptr;
        } else { // slice.conv.slice_to
            return .sentineled_many_ptr;
        }
    }

    /// The different kinds of pointer types that are recognized as being slice
    /// like.
    pub const OriginalTypeKind = enum {
        /// Already a slice
        slice,
        /// Pointer to an array
        array_ptr,
        /// Pointer to a vector. The elements of the vector must have a power of
        /// 2 bit-size.
        vector_ptr,
        /// Many item pointer with a sentinel
        sentineled_many_ptr,
        /// A c-pointer.
        c_ptr,
        /// A c-pointer to an array
        array_c_ptr,
        /// A c-pointer to a vector. The elements of the vector must have a
        /// power of 2 bit-size.
        vector_c_ptr,
    };

    /// How one would convert a value of the original type to a slice
    pub const Conversion = packed struct {
        /// Original type needs to be cast to an array type.
        ///
        /// When `true`, this asserts that the original type had a statically
        /// known length
        vec_to_arr: bool = false,
        /// Original type is a `C` pointer
        is_c: bool = false,
        /// Original type is many item pointer with a sentinel or a `C` pointer.
        slice_to: bool = false,

        /// Returns `true` if no additional casting is needed to convert the
        /// original type to a slice
        pub fn isDirect(conv: Conversion) bool {
            return !conv.vec_to_arr and !conv.is_c and !conv.slice_to;
        }
    };
};

/// Returns `true` if `@as(AsSlice(T), value)` would compile for `value: T`;
/// otherwise, returns `false`.
///
/// See also: `AsSlice`
pub fn isCoercible(comptime T: type) bool {
    return if (forType(T)) |slice| slice.conv.isDirect() else |_| false;
}

/// Gets type information about `T` as a slice type. If `T` cannot be viewed as
/// a slice type, `null` is returned.
///
/// See also: `Slice`, `Slice.fromPtrInfo`, `isCoercible`
pub fn forType(comptime T: type) Slice.Error!Slice {
    return switch (@typeInfo(T)) {
        .pointer => |ptr_info| return Slice.fromPtrInfo(ptr_info),
        else => error.NotSliceConvertible,
    };
}

/// Gets the element type of a slice-like type.
///
/// NOTE: This function uses the loosest definition of "slice type" when
/// retrieving the element type. Use other functions to restrict `T` if a
/// stricter definition of slice type is needed
pub fn Elem(comptime T: type) type {
    const slice = forType(T) catch {
        @compileError("Expected slice like type, found '" ++ @typeName(T) ++ "'");
    };

    return slice.child;
}

/// Coerces `T` to a slice type.
///
/// `T` must be coercible to a slice type via `@as`
/// (i.e. `forType(T) != null and forType(T).conv.isDirect()` is `true).
pub fn AsSlice(comptime T: type) type {
    if (forType(T)) |info| {
        if (info.conv.isDirect()) {
            return info.Type();
        }
    } else |_| {}
    @compileError("Expected slice-like type, found '" ++ @typeName(T) ++ "'");
}

/// Coerces `T` to a slice type, removing any sentinel present in `T`
///
/// `T` must be coercible to a slice type via `@as`
/// (i.e. `forType(T) != null and forType(T).conv.isDirect()` is `true).
///
/// See also: `AsSlice`, `AsSliceWithSentinel`, `AsSliceOfNoSentinel`
pub fn AsSliceNoSentinel(comptime T: type) type {
    if (forType(T)) |slice| {
        if (slice.conv.isDirect()) {
            return slice.withoutSentinel().Type();
        }
    } else |_| {}
    @compileError("Expected slice-like type, found '" ++ @typeName(T) ++ "'");
}

/// Coerces `T` to a slice type with the given sentinel value, overriding the
/// sentinel of `T`
///
/// `T` must be coercible to a slice type via `@as`
/// (i.e. `forType(T) != null and forType(T).conv.isDirect()` is `true).
///
/// See also: `AsSlice`, `AsSliceNoSentinel`, `AsSliceOfWithSentinel`
pub fn AsSliceWithSentinel(comptime T: type, comptime sentinel: Elem(T)) type {
    var slice = forType(T) catch {
        @compileError("Expected slice-like type, found '" ++ @typeName(T) ++ "'");
    };

    if (!slice.conv.isDirect())
        @compileError("Expected slice-like type, found '" ++ @typeName(T) ++ "'");

    slice.sentinel = &sentinel;

    return slice.Type();
}

/// Behaves exactly like `AsSlice`, except that the child type of the slice is
/// required to be `E`.
///
/// See also: `AsSlice`, `AsSliceOfNoSentinel`, `AsSliceOfWithSentinel`
pub fn AsSliceOf(comptime T: type, comptime E: type) type {
    if (forType(T)) |slice| {
        if (slice.conv.isDirect() and slice.child == E)
            return slice.Type();
    } else |_| {}
    @compileError(
        "Expected slice of '" ++ @typeName(E) ++ "', found '" ++ @typeName(T) ++ "'",
    );
}

/// Behaves exactly like `AsSliceNoSentinel`, except that the child type of the
/// slice is required to be `E`.
///
/// See also: `AsSliceOfNoSentinel`, `AsSlice`, `AsSliceOfWithSentinel`
pub fn AsSliceOfNoSentinel(comptime T: type, comptime E: type) type {
    if (forType(T)) |slice| {
        if (slice.conv.isDirect() and slice.child == E) {
            return slice.withoutSentinel().Type();
        }
    } else |_| {}
    @compileError("Expected slice-like type, found '" ++ @typeName(T) ++ "'");
}

/// Behaves exactly like `AsSliceWithSentinel`, except that the child type of
/// the slice is required to be `E`.
///
/// See also: `AsSliceOfWithSentinel`, `AsSlice`, `AsSliceOfNoSentinel`
pub fn AsSliceOfWithSentinel(comptime T: type, comptime E: type, comptime sentinel: E) type {
    var slice = forType(T) catch {
        @compileError("Expected slice-like type, found '" ++ @typeName(T) ++ "'");
    };

    if (!slice.conv.isDirect() or slice.child != E)
        @compileError("Expected slice-like type, found '" ++ @typeName(T) ++ "'");

    slice.sentinel = &sentinel;

    return slice.Type();
}

/// Converts `T` to a slice type.
///
/// Unlike `AsSlice`, this function is as permissive as possible so an implicit
/// coercion from `T` to `ToSlice(T)` may not always compile. Use `toSlice` to
/// properly coerce `T` to a slice
///
/// **WARNING**: Some conversions that are allowed by this function may take
/// O(n) time to complete
///
/// See also: `AsSlice`, `ToSliceOf`, `AsSliceOf`
pub fn ToSlice(comptime T: type) type {
    const slice = forType(T) catch {
        @compileError("Expected slice-like type, found '" ++ @typeName(T) ++ "'");
    };

    return slice.Type();
}

/// Behaves exactly like `ToSlice`, except that the child type of the slice is
/// required to be `E`
///
/// Unlike `AsSliceOf`, this function is as permissive as possible so an implicit
/// coercion from `T` to `ToSliceOf(T, E)` may not always compile. Use
/// `forType(T).convert()` to convert a `T` to a slice automatically.
///
/// **WARNING**: Some conversions that are allowed by this function may take
/// O(n) time to complete
///
/// See also: `ToSlice`, `AsSliceOf`, `AsSlice`
pub fn ToSliceOf(comptime T: type, comptime E: type) type {
    if (forType(T)) |slice| {
        if (slice.child == E)
            return slice.Type();
    } else |_| {}
    @compileError(
        "Expected slice of '" ++ @typeName(E) ++ "', found '" ++ @typeName(T) ++ "'",
    );
}

const std = @import("std");
const utilz = @import("utilz");

const mem = std.mem;

const AddressSpace = std.builtin.AddressSpace;
const Type = std.builtin.Type;
const PtrInfo = Type.Pointer;
const ArrayInfo = Type.Array;
const VecInfo = Type.Vector;
const assert = std.debug.assert;

const testing = std.testing;

test {
    testing.refAllDecls(@This());
}

test isCoercible {
    const expect = testing.expect;

    try expect(isCoercible([]const u8));
    try expect(!isCoercible(void));
    try expect(isCoercible(*[15:75]i128));
    try expect(isCoercible(*const [15]Type));
    try expect(!isCoercible([15]u8));
}
