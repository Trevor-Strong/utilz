//! Utilities relating to slices

pub const ConversionError = error{ NotSliceConvertible };

pub const Slice = struct {
    len: ?usize,
    conv: Conversion,
    child: type,
    sentinel: ?*const anyopaque,
    alignment: u29,
    addr_space: AddressSpace,
    is_const: bool,
    is_volatile: bool,
    is_allowzero: bool,

    pub fn fromPtrInfo(comptime ptr_info: PtrInfo) ConversionError!Slice {
        var slice = Slice {
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
                .Array => |arr_info| {
                    slice.len = arr_info.len;
                    slice.child = arr_info.child;
                    slice.sentinel = arr_info.sentinel;
                    slice.conv = .{};
                },
                .Vector => |vec_info| {
                    const bit_size = @bitSizeOf(vec_info.child);
                    if (bit_size % 8 != 0 or !std.math.isPowerOfTwo(bit_size)) {
                        return error.NotSliceCoercible;
                    }
                    slice.len = vec_info.len;
                    slice.child = vec_info.child;
                    slice.sentinel = null;
                    slice.conv = .{ .array = true };
                },
                else => return error.NotSliceConvertible
            },
            .Many => if (ptr_info.sentinel) |sentinel| {
                slice.len = null;
                slice.child = ptr_info.child;
                slice.sentinel = sentinel;
                slice.conv = .{ .slice_to = true };
            },
            .C => switch (@typeInfo(ptr_info.child)) {
                .Array => |arr_info| {
                    slice.len = arr_info.len;
                    slice.child = arr_info.child;
                    slice.sentinel = arr_info.sentinel;
                    slice.conv = .{ .is_c = true };
                },
                .Vector => |vec_info| {
                    slice.len = vec_info.len;
                    slice.child = vec_info.child;
                    slice.conv = .{ .is_c = true, .array = true };
                },
            }
        }
    }

    pub fn getSentinel(comptime slice: Slice) ?slice.child {
        const Dest = ?*const slice.child;
        const ptr: Dest = @ptrCast(@alignCast(slice.sentinel));
        return if (ptr) |p| p.* else null;
    }

    pub fn setSentinel(comptime slice: *Slice, comptime new_sentinel: ?slice.child) void {
        const s = new_sentinel orelse {
            slice.sentinel = null;
            return;
        };
        slice.sentinel = &s;
    }

    pub fn dropSentinel(comptime slice: *Slice) ConversionError!void {
        if (slice.conv.slice_to) {
            assert(slice.sentinel != null);
            return error.NotSliceConvertible;
        }
        slice.sentinel = null;
    }

    pub fn convert(comptime slice: Slice, from: slice.Original()) slice.Type() {
        if (slice.conv.isDirect()) return from;

        if (slice.conv.slice_to) return mem.span(from);

        const ArrayPtr = slice.arrayPtrType().?;
        return @as(ArrayPtr, from);
    }

    pub fn arrayPtrType(comptime slice: Slice) ?type {
        const len = slice.len orelse return null;
        return @Type(.{
            .Pointer = .{
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

    pub fn Type(comptime slice: Slice) type {
        return @Type(slice.resultInfo());
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
                    .Array = .{
                        .child = slice.child,
                        .len = arr_len,
                        .sentinel = slice.sentinel,
                    },
                });
                ptr_info.sentinel = null;
            } else {
                ptr_info.size = .Slice;
                ptr_info.child = slice.child;
                ptr_info.sentinel = null;
            }
        } else if (slice.conv.vec_to_arr) {
            ptr_info.child = @Type(.{
                .Vector = .{
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
            assert(utilz.pack.eql(slice.conv, .{ .is_c = true }));
            ptr_info.size = .C;
            ptr_info.child = @Type(.{
                .Array = .{
                    .len = slice.len.?,
                    .child = slice.child,
                    .sentinel = slice.sentinel,
                },
            });
            ptr_info.sentinel = null;
        }
        return .{ .Pointer = ptr_info };
    }

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

        pub fn isDirect(conv: Conversion) bool {
            return utilz.pack.toInt(conv) == 0;
        }

    };
};

pub fn isCoercible(comptime T: type) bool {
    return if (getInfo(T)) |slice| slice.conv == .direct else false;
}

pub fn getInfo(comptime T: type) ?Slice {
    switch (@typeInfo(T)) {
        .Pointer => |ptr_info| if (Slice.fromPtrInfo(ptr_info)) |slice|
            return slice
        else |_| {},
        else => {},
    }
    return null;
}

/// Gets the element type of a slice-like type.
pub fn Elem(comptime T: type) type {
    if (getInfo(T)) |slice| {
        if (slice.conv.isDirect()) return slice.child;
    }
    @compileError(utilz.unexpectedTypeMsg("Expected slice-like type,", T));
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

test {
    std.testing.refAllDecls(@This());
}
