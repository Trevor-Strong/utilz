const std = @import("std");
const builtin = @import("builtin");
const util = @import("../root.zig");

const native_endian = builtin.cpu.arch.endian();

const assert = std.debug.assert;

pub const Config = struct {
    allow_surrogates: bool,

    pub const utf16: Config = .{ .allow_surrogates = false };
    pub const wtf16: Config = .{ .allow_surrogates = true };
};

pub fn codepointLen(codepoint: u21, comptime config: Config) !u2 {
    if (config.allow_surrogates) {
        switch (codepoint) {
            0x0000...0xFFFF => return 1,
            0x10000...0x10FFFF => return 2,
            else => return error.CodepointTooLarge,
        }
    } else {
        switch (codepoint) {
            0x0000...0xD7FF, 0xE000...0xFFFF => return 1,
            0xD800...0xDFFF => return error.SurrogateCodepoint,
            0x10000...0x10FFFF => return 2,
            0x110000...0x1FFFFF => return error.CodepointTooLarge,
        }
    }
}

pub fn isBmpCodepoint(codepoint: u21) bool {
    return codepoint <= 0xFFFF and !isSurrogate(@truncate(codepoint));
}

pub fn isSupplementaryCodepoint(codepoint: u21) bool {
    return codepoint > 0x10000;
}

pub fn isSurrogateCodepoint(codepoint: u21) bool {
    const mask = 0x1FF800;
    assert(@clz(@as(u21, mask)) == 0);
    assert(@ctz(mask) == 10);
    return codepoint & mask == 0xD800;
}

/// Returns `true` if `c` is a surrogate codeunit
pub fn isSurrogate(c: u16) bool {
    return c & 0xF800 == 0xD800;
}

/// Returns `true` if the codeunit `c` is a high (leading) surrogate.
pub fn isHighSurrogate(c: u16) bool {
    return c & 0xFC00 == 0xD800;
}

/// Returns `true` if the codeunit `c` is a low (trailing) surrogate.
pub fn isLowSurrogate(c: u16) bool {
    return c & 0xFC00 == 0xDC00;
}

/// Decodes the codepoint encoded by the surrogate pair `hi` and `lo`. Both
/// `hi` and `lo` are assumed to be in native byte order and to form a valid
/// surrogate pair.
pub fn decodeSurrogatePair(hi: u16, lo: u16) u21 {
    return (0x10000 + (@as(u21, @as(u10, @truncate(hi))) << 10)) |
        @as(u10, @truncate(lo));
}

/// Encodes `codepoint` into the buffer `dest` with the given endianness. The
/// number of codeunits `codepoint` was encoded as is returned (either 1 or 2).
pub inline fn encodeInto(
    dest: []u16,
    codepoint: u21,
    endian: std.builtin.Endian,
    comptime config: Config,
) !u2 {
    if (isSupplementaryCodepoint(codepoint)) {
        if (dest.len < 2) return error.NoSpaceLeft;
        dest[0..2].* = encodeSurrogatePair(codepoint, endian);
        return 2;
    } else {
        if (dest.len < 1) return error.NoSpaceLeft;
        const c: u16 = @truncate(codepoint);
        if (!config.allow_surrogates) {
            if (isSurrogate(c)) {
                return error.SurrogateCodepoint;
            }
        }
        dest[0] = std.mem.nativeTo(u16, c, endian);
        return 1;
    }
}

pub inline fn encode(
    codepoint: u21,
    endian: std.builtin.Endian,
    comptime config: Config,
) !std.BoundedArray(u16, 2) {
    var buf: std.BoundedArray(u16, 2) = undefined;
    buf.len = encodeInto(
        buf.slice(),
        codepoint,
        endian,
        config,
    ) catch |err| switch (err) {
        error.NoSpaceLeft => unreachable,
        else => |e| return e,
    };
    return buf;
}

pub inline fn encodeSurrogatePair(
    codepoint: u21,
    endian: std.builtin.Endian,
) [2]u16 {
    assert(isSupplementaryCodepoint(codepoint));
    var c: [2]u16 = undefined;

    encodeIntoSurrogatePairNative(&c, codepoint);
    if (endian != native_endian) {
        inline for (0..c.len) |i| {
            c[i] = @byteSwap(c[i]);
        }
    }
    return c;
}

fn encodeIntoSurrogatePairNative(c: *[2]u16, codepoint: u21) void {
    c[0] = @as(u16, @intCast((codepoint - 0x10000) >> 10)) | 0xD800;
    c[1] = @as(u10, @truncate(codepoint)) | @as(u16, 0xDC00);
}

pub inline fn decode(
    source: []const u16,
    endian: std.builtin.Endian,
    comptime config: Config,
) !struct { u21, u2 } {
    switch (endian) {
        inline else => |tag| return decodeImpl(source, tag, true, config),
    }
}

pub inline fn decodeUnaligned(
    source: []align(1) const u16,
    endian: std.builtin.Endian,
    comptime config: Config,
) !struct { u21, u2 } {
    switch (endian) {
        inline else => |tag| return decodeImpl(source, tag, false, config),
    }
}

fn decodeImpl(
    raw_data: []align(1) const u16,
    comptime endian: std.builtin.Endian,
    comptime is_naturally_aligned: bool,
    comptime config: Config,
) !struct { u21, u2 } {
    const data: if (is_naturally_aligned)
        []const u16
    else
        void = if (is_naturally_aligned) @alignCast(raw_data) else {};

    const first = if (is_naturally_aligned)
        std.mem.toNative(u16, data[0], endian)
    else
        std.mem.readInt(u16, @ptrCast(&raw_data[0]), endian);
    if (config.allow_surrogates) {
        if (first & 0xFC00 != 0xD800) return .{ first, 1 };
        if (data.len < 2) return first;
    } else {
        if (first & 0xF800 != 0xD800) return .{ first, 1 };

        if (first & 0xFC00 == 0xDC00) return error.LeadingLowSurrogate;
        if (raw_data.len < 2) return error.SequenceTruncated;
    }
    assert(first & 0xFC00 == 0xD800);
    assert(raw_data.len >= 2);

    const second = if (is_naturally_aligned)
        std.mem.toNative(u16, data[1], endian)
    else
        std.mem.readInt(u16, @ptrCast(&raw_data[1]), endian);

    if (second & 0xFC00 != 0xDC00) {
        if (config.allow_surrogates)
            return .{ first, 1 }
        else
            return error.UnpairedHighSurrogate;
    }

    return .{ decodeSurrogatePair(first, second), 2 };
}
