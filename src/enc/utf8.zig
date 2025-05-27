const std = @import("std");
const builtin = @import("builtin");
const util = @import("../root.zig");
const assert = std.debug.assert;

pub const Config = struct {
    allow_surrogates: bool,

    pub const utf8: Config = .{ .allow_surrogates = false };
    pub const wtf8: Config = .{ .allow_surrogates = true };
};

pub fn Count(comptime config: Config) type {
    return struct {
        /// Amount counted
        amt: usize,
        /// Length of the slice traversed
        len: usize,
        /// Error that was encountered or `null` if the entire slice was evaluated
        /// without error
        err: ?Error,

        pub const Error = if (config.allow_surrogates)
            Wtf8DecodeError
        else
            Utf8DecodeError;

        /// Returns `amt` if `err` is `null`; otherwise, returns `err.?`.
        pub fn get(self: *const @This()) Error!usize {
            return self.err orelse {
                @branchHint(.likely);
                return self.amt;
            };
        }
    };
}

pub const Wtf8DecodeError = error{
    /// The leading byte of a codepoint sequence was invalid
    InvalidLeadByte,
    /// A codepoint is encoded using more bytes than necessary
    OverlongEncoding,
    /// A codepoint with a value greater than `0x10FFFF` was encoded
    CodepointTooLarge,
    /// The end of the slice would be reached before the end of the last
    /// codepoint sequence
    SequenceTruncated,
    /// A byte other than a continuation byte was encountered when a
    /// continuation byte was expected.
    ExpectedContinuation,
};

pub const Utf8DecodeError = error{
    /// A surrogate codepoint was encoded.
    SurrogateCodepoint,
} || Wtf8DecodeError;

const CountMode = enum {
    loose,
    wtf8,
    utf8,

    fn toConfig(mode: CountMode) Config {
        return .{
            .allow_surrogates = mode != .utf8,
        };
    }
};

/// Inspect `bytes` and return an error if it contains invalid UTF-8/WTF-8
/// (depending on `config`).
pub fn validate(
    bytes: []const u8,
    comptime config: Config,
) !void {
    var i: usize = 0;
    if (std.simd.suggestVectorLength(u8)) |chunk_len| {
        const Chunk = @Vector(chunk_len, u8);
        if (bytes.len >= chunk_len) {
            const final = bytes.len - chunk_len;
            const mask: Chunk = @splat(0x7F);
            while (true) {
                const chunk: Chunk = bytes[i..][0..chunk_len];
                if (@reduce(.Or, chunk > mask)) break;
                i += chunk_len;
                if (i > final) break;
            }
        }
    }

    var n: u3 = undefined;
    while (i != bytes.len) : (i += n) {
        n = try sequenceLen(bytes[i]);
        assert(n >= 1 and n <= 4); // 1 <= n <= 4
        if (bytes.len - i < n) return error.SequenceTruncated;
        switch (n) {
            1 => {},
            2 => {
                if (!isContinuation(bytes[i + 1])) return error.ExpectedContinuation;
                const first: u5 = @truncate(bytes[i]);
                if (first < @as(u5, 0x80 >> 6)) return error.OverlongEncoding;
            },
            3 => {
                if (!isContinuation(bytes[i + 1])) return error.ExpectedContinuation;
                if (!isContinuation(bytes[i + 2])) return error.ExpectedContinuation;

                if (config.allow_surrogates) {
                    const hi_bits = (@as(u10, @as(u4, @truncate(bytes[i]))) << 6) |
                        @as(u6, @truncate(bytes[i + 1]));
                    if (hi_bits < @as(u10, 0x800 >> 6)) return error.OverlongEncoding;
                } else {
                    const codepoint = decode3Unchecked(bytes[0..3]);
                    if (codepoint < 0x800) return error.OverlongEncoding;
                    if (isSurrogate(codepoint)) return error.SurrogateCodepoint;
                }
            },
            4 => {
                if (!isContinuation(bytes[i + 1])) return error.ExpectedContinuation;
                if (!isContinuation(bytes[i + 2])) return error.ExpectedContinuation;
                if (!isContinuation(bytes[i + 3])) return error.ExpectedContinuation;
                const hi_bits = (@as(u9, @as(u3, @truncate(bytes[i]))) << 6) |
                    @as(u6, @truncate(bytes[i + 1]));
                if (hi_bits < @as(u9, 0x10000 >> 12)) return error.OverlongEncoding;
                if (hi_bits > @as(u9, 0x10FFFF >> 12)) return error.CodepointTooLarge;
            },
            else => unreachable,
        }
    }
}

/// Count the number of UTF-8 codepoints in `bytes`, stopping on the first
/// invalid UTF-8 sequence.
///
/// The number of codepoints in the valid UTF-8 prefix of `bytes`, the length of
/// the prefix of `bytes` that is valid UTF-8, and the first error that was
/// encountered are all counted and returned.
pub fn countCodepoints(
    bytes: []const u8,
    comptime config: Config,
    comptime loose: bool,
) Count(config) {
    return countUnitsImpl(
        bytes,
        .codepoints,
        if (loose) .loose else if (config.allow_surrogates) .wtf8 else .utf8,
    );
}

/// Count the number of UTF-16 codeunits are required to encode the UTF-8 prefix
/// of `bytes`, stopping on the first invalid UTF-8 sequence.
///
/// The number of UTF-16 codeunits need to encode the UTF-8 prefix of `bytes`,
/// the length of the prefix of `bytes` that is valid UTF-8, and the first error
/// that was encountered are all counted and returned.
pub fn countUtf16Units(
    bytes: []const u8,
    comptime config: Config,
    comptime loose: bool,
) Count(config) {
    return countUnitsImpl(
        bytes,
        .utf16,
        if (loose) .loose else if (config.allow_surrogates) .wtf8 else .utf8,
    );
}

fn countUnitsImpl(
    bytes: []const u8,
    comptime kind: enum { codepoints, utf16 },
    comptime mode: Count.Mode,
) Count {
    const checked = mode != .loose;
    var ptr = bytes.ptr;
    if (std.simd.suggestVectorLength(u8)) |chunk_len| {
        const Chunk = @Vector(chunk_len, u8);
        const mask: Chunk = @splat(0x80);
        if (bytes.len >= chunk_len) {
            const end_chunk = ptr + (bytes.len - chunk_len);
            while (true) {
                const chunk: Chunk = ptr[0..chunk_len].*;
                if (std.simd.firstTrue(chunk >= mask)) |first_non_ascii| {
                    ptr += first_non_ascii;
                    break;
                }

                ptr += chunk_len;
                if (@intFromPtr(ptr) > end_chunk) break;
            }
        }
    }
    var count: Count = .{
        .amt = ptr - bytes.ptr,
        .len = undefined,
        .err = null,
    };
    var surrogate_pairs: switch (kind) {
        .codepoints => u0,
        .utf16 => usize,
    } = 0;
    const end = bytes.ptr + bytes.len;
    while (ptr != end) {
        const n = try sequenceLen(ptr[0]);
        if (end - ptr < n) {
            count.err = error.SequenceTruncated;
            break;
        }
        if (checked) check: {
            if (n == 1) break :check;
            if (ptr[1] & 0xC0 != 0x80) {
                count.err = error.ExpectedContinuation;
                break;
            }
            if (n > 2) {
                if (ptr[2] & 0xC0 != 0x80) {
                    count.err = error.ExpectedContinuation;
                    break;
                }
                if (n == 4) {
                    if (ptr[3] & 0xC0 != 0x80) {
                        count.err = error.ExpectedContinuation;
                        break;
                    }
                }
            }
            var min: u32 = undefined;
            var val: u32 = undefined;
            switch (n) {
                2 => {
                    min = 0x80;
                    val = decode2Unchecked(ptr[0..2]);
                },
                3 => {
                    min = 0x800 >> 6;
                    val = (@as(u10, @as(u4, @truncate(ptr[0]))) << 6) |
                        @as(u6, @truncate(ptr[1]));
                    if (mode == .utf8) {
                        const mask = 0xF8 >> 6;
                        const surrogate_mask = 0xD8 >> 6;
                        if (val & mask == surrogate_mask) {
                            count.err = error.SurrogateCodepoint;
                            break;
                        }
                    }
                },
                4 => {
                    min = 0x10000 >> 12;
                    const max = 0x10FFFF >> 12;
                    val = (@as(u9, @as(u3, @truncate(ptr[0]))) << 6) |
                        @as(u6, @truncate(ptr[1]));

                    if (val > max) {
                        count.err = error.CodepointTooLarge;
                        break;
                    }
                },
                else => unreachable,
            }
            if (val < min) {
                count.err = error.OverlongEncoding;
                break;
            }
        }
        ptr += n;
        count.amt += 1;
        surrogate_pairs += switch (kind) {
            .codepoints => 0,
            .utf16 => n >> 1,
        };
    }
    count.len = ptr - bytes.ptr;
    count.amt += surrogate_pairs;
    return count;
}

pub fn codepointByteLen(cp: u21, comptime config: Config) !u3 {
    if (config.allow_surrogates)
        return try codepointByteLenAllowSurrogates(cp)
    else
        return try codepointByteLenNoSurrogates(cp);
}

pub fn codepointByteLenAllowSurrogates(cp: u21) error{CodepointTooLarge}!u3 {
    switch (cp) {
        0...0x7f => return 1,
        0x80...0x7ff => return 2,
        0x800...0xffff => return 3,
        0x10000...0x10ffff => return 4,
        else => return error.CodepointTooLarge,
    }
}

pub fn codepointByteLenNoSurrogates(cp: u21) !u3 {
    switch (cp) {
        0...0x7F => return 1,
        0x80...0x7FF => return 2,
        0x800...0xD7FF, 0xE000...0xFFFF => return 3,
        0xD800...0xDFFF => return error.SurrogateCodepoint,
        0x10000...0x10FFFF => return 4,
        else => return error.CodepointTooLarge,
    }
}

pub fn encodeComptime(
    comptime codepoint: u21,
    comptime config: Config,
) *const [codepointByteLen(codepoint) catch unreachable]u8 {
    var buf: [4]u8 = undefined;
    const n = encodeInto(&buf, codepoint, config) catch unreachable;
    const copy = buf[0..n].*;
    return &copy;
}

pub fn encode(codepoint: u21, comptime config: Config) !std.BoundedArray(u8, 4) {
    var array: std.BoundedArray(u8, 4) = undefined;
    array.len = try encodeInto(&array.buffer, codepoint, config);
    return array;
}

pub fn encodeInto(
    dest: []u8,
    codepoint: u21,
    comptime config: Config,
) !u3 {
    if (dest.len == 0) return error.NoSpaceLeft;
    const len = try sequenceLen(dest[0]);
    switch (codepoint) {
        1 => {
            dest[0] = @as(u7, @intCast(codepoint));
        },
        2 => {
            dest[0..2].* = encode2Unchecked(codepoint);
        },
        3 => {
            if (!config.allow_surrogates) {
                if (codepoint & 0xF800 == 0xD800)
                    return error.SurrogateCodepoint;
            }
            dest[0..3].* = encode3Unchecked(codepoint);
        },
        4 => {
            dest[0..4].* = encode4Unchecked(codepoint);
        },
        else => unreachable,
    }
    return len;
}

pub fn encode2Unchecked(codepoint: u21) [2]u8 {
    return .{
        @as(u8, 0xC0) | @as(u5, @truncate(codepoint >> 6)),
        @as(u8, 0x80) | @as(u6, @truncate(codepoint)),
    };
}

pub fn encode3Unchecked(codepoint: u21) [3]u8 {
    return .{
        @as(u8, 0xE0) | @as(u4, @truncate(codepoint >> 12)),
        @as(u8, 0x80) | @as(u6, @truncate(codepoint >> 6)),
        @as(u8, 0x80) | @as(u6, @truncate(codepoint)),
    };
}

pub fn encode4Unchecked(codepoint: u21) [3]u8 {
    return .{
        @as(u8, 0xF0) | @as(u3, @truncate(codepoint >> 18)),
        @as(u8, 0x80) | @as(u6, @truncate(codepoint >> 12)),
        @as(u8, 0x80) | @as(u6, @truncate(codepoint >> 6)),
        @as(u8, 0x80) | @as(u6, @truncate(codepoint)),
    };
}

pub fn sequenceLen(lead_byte: u8) error{InvalidLeadByte}!u3 {
    switch (lead_byte) {
        0x00...0x7f => return 1,
        0xC0...0xDF => return 2,
        0xE0...0xEF => return 3,
        0xF0...0xF7 => return 4,
        else => return error.InvalidLeadByte,
    }
}

pub fn decode(source: []u8, comptime config: Config) !struct { u21, u3 } {
    assert(source.len > 0);
    const n = try sequenceLen(source[0]);
    if (source.len < n) return error.SequenceTruncated;
    const codepoint: u21 = switch (n) {
        1 => source[0],
        2 => try decode2(source[0..2], config),
        3 => try decode3(source[0..3], config),
        4 => try decode4(source[0..4], config),
        else => unreachable,
    };
    return .{ codepoint, n };
}

pub const Decode2Error = error{
    ExpectedContinuation,
    OverlongEncoding,
};

/// Decodes the 2 byte sequence `bytes`.
///
/// Assumes that the lead byte is a valid leading byte for a 2 byte UTF-8
/// sequence
///
/// Other than the above assumption, this function checks for the validity of
/// the sequence and returns an error if the sequence is not a valid 2 byte
/// UTF-8 sequence.
pub fn decode2(bytes: *const [2]u8, _: Config) Decode2Error!u11 {
    if (bytes[1] & 0xC0 != 0x80) return error.ExpectedContinuation;
    const codepoint = decode2Unchecked(bytes);
    if (codepoint < 0x80) return error.OverlongEncoding;
    return codepoint;
}

/// Decodes the 3 byte sequences `bytes`.
///
/// Assumes that the lead byte is a valid leading byte for a 3 byte UTF-8
/// sequence
///
/// Other than the above assumption, this function checks for the validity of
/// the sequence and returns an error if the sequence is not a valid 3 byte
/// UTF-8 sequence. This function also errors if `bytes` encodes a surrogate
/// codepoint and `config.allow_surrogates` is `false`.
pub fn decode3(bytes: *const [3]u8, comptime config: Config) !u16 {
    if (bytes[1] & 0xC0 != 0x80) return error.ExpectedContinuation;
    if (bytes[2] & 0xC0 != 0x80) return error.ExpectedContinuation;
    const codepoint = decode3Unchecked(bytes);
    if (codepoint < 0x800) return error.OverlongEncoding;
    if (config.allow_surrogates)
        if (isSurrogate(codepoint)) return error.SurrogateCodepoint;
    return codepoint;
}

pub const Decode4Error = error{
    ExpectedContinuation,
    OverlongEncoding,
    CodepointTooLarge,
};

/// Decodes the 4 byte sequences `bytes`.
///
/// Assumes that the lead byte is a valid leading byte for a 4 byte UTF-8
/// sequence
///
/// Other than the above assumption, this function checks for the validity of
/// the sequence and returns an error if the sequence is not a valid 4 byte
/// UTF-8 sequence.
pub fn decode4(bytes: *const [4]u8, _: Config) !u21 {
    if (bytes[1] & 0xC0 != 0x80) return error.ExpectedContinuation;
    if (bytes[2] & 0xC0 != 0x80) return error.ExpectedContinuation;
    if (bytes[3] & 0xC0 != 0x80) return error.ExpectedContinuation;
    const codepoint = decode4Unchecked(bytes);
    if (codepoint < 0x1_0000) return error.OverlongEncoding;
    if (codepoint > 0x10FFFF) return error.CodepointTooLarge;
    return codepoint;
}

/// Decodes the 2 byte UTF-8 sequence without validating it.
pub fn decode2Unchecked(bytes: *const [2]u8) u11 {
    return (@as(u11, @as(u5, @truncate(bytes[0]))) << 6) |
        @as(u6, @truncate(bytes[1]));
}

/// Decodes the 3 byte UTF-8 sequence without validating it.
pub fn decode3Unchecked(bytes: *const [3]u8) u16 {
    return (@as(u16, @as(u4, @truncate(bytes[0]))) << 12) |
        (@as(u12, @as(u6, @truncate(bytes[1]))) << 6) |
        @as(u6, @truncate(bytes[2]));
}

/// Decodes the 4 byte UTF-8 sequence without validating it.
pub fn decode4Unchecked(bytes: *const [4]u8) u21 {
    return (@as(u21, @as(u3, @truncate(bytes[0]))) << 18) |
        (@as(u18, @as(u6, @truncate(bytes[1]))) << 12) |
        (@as(u12, @as(u6, @truncate(bytes[2]))) << 6) |
        @as(u6, @truncate(bytes[3]));
}

fn isSurrogate(c: u16) bool {
    const native_endian = comptime builtin.cpu.arch.endian();
    return util.enc.utf16.isSurrogate(c, native_endian);
}

fn isContinuation(c: u8) bool {
    return c & 0xC0 == 0x80;
}

test isContinuation {
    try t.expect(isContinuation(0b10_000000));
    try t.expect(isContinuation(0b10_100001));
    try t.expect(isContinuation(0b10_100100));
    try t.expect(isContinuation(0b10_000001));
    try t.expect(isContinuation(0b10_111111));
    try t.expect(!isContinuation(0b11_011111));
    try t.expect(!isContinuation(0b01_111111));
    try t.expect(!isContinuation(0b01_100000));
    try t.expect(!isContinuation(0b0000_0000));
    try t.expect(!isContinuation(0b1111_1111));
}

const t = std.testing;

test encode {
    const expect = struct {
        fn f(expected: []const u8, codepoint: u21, comptime config: Config) !void {
            const buf = try encode(codepoint, config);
            try t.expectEqualStrings(expected, buf.slice());
        }
    }.f;

    try expect("$", '$', .utf8);
    try expect("\u{000090}", '\u{000090}', .utf8);
    try expect("\u{0020AC}", '\u{0020AC}', .utf8);
    try expect("\u{010348}", '\u{010348}', .utf8);

    try expect("$", '$', .wtf8);
    try expect("\u{000090}", '\u{000090}', .wtf8);
    try expect("\u{0020AC}", '\u{0020AC}', .wtf8);
    try expect("\u{010348}", '\u{010348}', .wtf8);
}

test "encode - Errors" {
    try t.expectError(error.CodepointTooLarge, encode(0x10FFFF + 1, .utf8));
    try t.expectError(error.CodepointTooLarge, encode(0x10FFFF + 1, .wtf8));
    try t.expectError(error.CodepointTooLarge, encode(std.math.maxInt(u21), .utf8));
    try t.expectError(error.CodepointTooLarge, encode(std.math.maxInt(u21), .wtf8));

    try t.expectEqualStrings("\u{10FFFF}", (try encode(0x10FFFF, .utf8)).slice());
    try t.expectEqualStrings("\u{10FFFF}", (try encode(0x10FFFF, .wtf8)).slice());

    try t.expectError(error.SurrogateCodepoint, encode(0xD800, .utf8));
    try t.expectError(error.SurrogateCodepoint, encode(0xDBFF, .utf8));
    try t.expectError(error.SurrogateCodepoint, encode(0xDC00, .utf8));
    try t.expectError(error.SurrogateCodepoint, encode(0xDFFF, .utf8));
    try t.expectError(error.SurrogateCodepoint, encode(0xDB80, .utf8));
    try t.expectError(error.SurrogateCodepoint, encode(0xDF80, .utf8));

    try t.expect(std.meta.isError(encode(0xD800, .wtf8)) == false);
    try t.expect(std.meta.isError(encode(0xDBFF, .wtf8)) == false);
    try t.expect(std.meta.isError(encode(0xDC00, .wtf8)) == false);
    try t.expect(std.meta.isError(encode(0xDFFF, .wtf8)) == false);
    try t.expect(std.meta.isError(encode(0xDB80, .wtf8)) == false);
    try t.expect(std.meta.isError(encode(0xDF80, .wtf8)) == false);
}

test decode {
    const expect = struct {
        fn f(expected: u21, expected_n: u3, slice: []const u8, comptime config: Config) !void {
            const actual = try decode(slice, config);
            try t.expectEqual(.{ expected, expected_n }, actual);
        }
    }.f;

    // Basic functionality

    try expect('$', 1, "$", .utf8);
    try expect('\u{000090}', 2, "\u{000090}", .utf8);
    try expect('\u{0020AC}', 3, "\u{0020AC}", .utf8);
    try expect('\u{010348}', 4, "\u{010348}", .utf8);

    try expect('$', 1, "$", .wtf8);
    try expect('\u{000090}', 2, "\u{000090}", .wtf8);
    try expect('\u{0020AC}', 3, "\u{0020AC}", .wtf8);
    try expect('\u{010348}', 4, "\u{010348}", .wtf8);
}

test "decode - Only first codepoint" {
    const expect = struct {
        fn f(expected: u21, expected_n: u3, slice: []const u8, comptime config: Config) !void {
            const actual = try decode(slice, config);
            try t.expectEqual(.{ expected, expected_n }, actual);
        }
    }.f;

    // Decodes only the first codepoint and ignores the rest of the buffer.

    try expect('a', 1, "a|some junk that is \x0B\xAD}", .utf8);
    try expect('\u{000123}', 1, "\u{000123}|some junk that is \x0B\xAD}", .utf8);
    try expect('\u{0000FF}', 2, "\u{0000FF}|some junk that is \x0B\xAD}", .utf8);
    try expect('\u{00FF00}', 3, "\u{00FF00}|some junk that is \x0B\xAD}", .utf8);
    try expect('\u{100123}', 4, "\u{100123}|some junk that is \x0B\xAD}", .utf8);

    try expect('a', 1, "a|some junk that is \x0B\xAD}", .wtf8);
    try expect('\u{000123}', 1, "\u{000123}|some junk that is \x0B\xAD}", .wtf8);
    try expect('\u{0000FF}', 2, "\u{0000FF}|some junk that is \x0B\xAD}", .wtf8);
    try expect('\u{00FF00}', 3, "\u{00FF00}|some junk that is \x0B\xAD}", .wtf8);
    try expect('\u{100123}', 4, "\u{100123}|some junk that is \x0B\xAD}", .wtf8);
}

test "decode - Errors" {
    const expectError = struct {
        fn f(expected: anyerror, bytes: []const u8, comptime config: Config) !void {
            try t.expectError(expected, decode(bytes, config));
        }
    }.f;

    // error.InvalidLeadByte

    try expectError(error.InvalidLeadByte, "\x80", .utf8);
    try expectError(error.InvalidLeadByte, "\x80", .wtf8);

    try expectError(error.InvalidLeadByte, &.{0b1111_1000}, .wtf8);
    try expectError(error.InvalidLeadByte, &.{0b1111_1010}, .wtf8);

    try expectError(error.ExpectedContinuation, "\xD0\x00", .utf8);
    try expectError(error.ExpectedContinuation, "\xD0\x00", .wtf8);

    try expectError(error.ExpectedContinuation, "\xE3\x80\x00", .utf8);
    try expectError(error.ExpectedContinuation, "\xE3\x80\x00", .wtf8);

    try expectError(error.ExpectedContinuation, "\xE3\x00\x80", .utf8);
    try expectError(error.ExpectedContinuation, "\xE3\x00\x80", .wtf8);

    // error.ExpectedContinuation

    try expectError(error.ExpectedContinuation, "\xF0\x10\x80\x80", .utf8);
    try expectError(error.ExpectedContinuation, "\xF0\x10\x80\x80", .wtf8);

    try expectError(error.ExpectedContinuation, "\xF0\xA0\x80\x00", .utf8);
    try expectError(error.ExpectedContinuation, "\xF0\xA0\x80\x00", .wtf8);

    try expectError(error.ExpectedContinuation, "\xF0\xA0\x00\x80", .utf8);
    try expectError(error.ExpectedContinuation, "\xF0\xA0\x00\x80", .wtf8);

    // error.SequenceTruncated

    try expectError(error.SequenceTruncated, "\xD0", .utf8);
    try expectError(error.SequenceTruncated, "\xD0", .wtf8);

    try expectError(error.SequenceTruncated, "\xE3", .utf8);
    try expectError(error.SequenceTruncated, "\xE3", .wtf8);

    try expectError(error.SequenceTruncated, "\xE3\x80", .utf8);
    try expectError(error.SequenceTruncated, "\xE3\x80", .wtf8);

    try expectError(error.SequenceTruncated, "\xF0\xA0\x80", .utf8);
    try expectError(error.SequenceTruncated, "\xF0\xA0\x80", .wtf8);

    try expectError(error.SequenceTruncated, "\xF0\xA0", .utf8);
    try expectError(error.SequenceTruncated, "\xF0\xA0", .wtf8);

    // error.OverlongEncoding

    try expectError(error.OverlongEncoding, "\xC0\x81", .utf8);
    try expectError(error.OverlongEncoding, "\xC0\x81", .wtf8);

    try expectError(error.OverlongEncoding, "\xE0\x80\x81", .utf8);
    try expectError(error.OverlongEncoding, "\xE0\x80\x81", .wtf8);

    try expectError(error.OverlongEncoding, "\xF0\x80\x80\x81", .utf8);
    try expectError(error.OverlongEncoding, "\xF0\x80\x80\x81", .wtf8);

    // error.CodepointTooLarge

    // max codepoint     = 0x10FFFF = 0b100001111111111111111
    // max codepoint + 1 = 0x110000 = 0b100010000000000000000
    //
    //  0b100 010000 000000 000000
    // |-----|------|------|------|
    //   1st   2nd    3rd    4th
    const max_codepoint_plus_1 = &[_]u8{
        0b11110_000 | 0b100,
        0b10_000000 | 0b010000,
        0b10_000000 | 0b000000,
        0b10_000000 | 0b000000,
    };

    try expectError(error.CodepointTooLarge, max_codepoint_plus_1, .utf8);
    try expectError(error.CodepointTooLarge, max_codepoint_plus_1, .wtf8);

    try expectError(error.CodepointTooLarge, "\xF7\xBF\xBF\xBF", .utf8);
    try expectError(error.CodepointTooLarge, "\xF7\xBF\xBF\xBF", .wtf8);

    // error.SurrogateCodepoint

    try expectError(error.SurrogateCodepoint, encodeComptime(0xD800), .utf8);
    try expectError(error.SurrogateCodepoint, encodeComptime(0xDBFF), .utf8);
    try expectError(error.SurrogateCodepoint, encodeComptime(0xDC00), .utf8);
    try expectError(error.SurrogateCodepoint, encodeComptime(0xDFFF), .utf8);
    try expectError(error.SurrogateCodepoint, encodeComptime(0xDB10), .utf8);
    try expectError(error.SurrogateCodepoint, encodeComptime(0xDD10), .utf8);

    try t.expectEqual(.{ 0xD800, 3 }, try decode(0xD800, .wtf8));
    try t.expectEqual(.{ 0xDBFF, 3 }, try decode(0xDBFF, .wtf8));
    try t.expectEqual(.{ 0xDC00, 3 }, try decode(0xDC00, .wtf8));
    try t.expectEqual(.{ 0xDFFF, 3 }, try decode(0xDFFF, .wtf8));
    try t.expectEqual(.{ 0xDB10, 3 }, try decode(0xDB10, .wtf8));
    try t.expectEqual(.{ 0xDD10, 3 }, try decode(0xDD10, .wtf8));
}

pub fn IteratorImpl(comptime config: Config) type {
    return struct {
        bytes: []const u8,
        pos: usize,

        const Self = @This();

        pub const Error = PreCheckedIteratorImpl(config).Error;

        pub fn init(bytes: []const u8) Self {
            validate(bytes, config);
        }
    };
}

pub fn PreCheckedIteratorImpl(comptime config: Config) type {
    return struct {
        bytes: []const u8,
        pos: usize,

        pub const Error = switch (config.allow_surrogates) {
            true => Wtf8DecodeError,
            false => Utf8DecodeError,
        };

        const Self = @This();

        pub fn init(bytes: []const u8) Error!Self {
            try validate(bytes, config);
            return .initUnchecked(bytes);
        }

        pub fn initUnchecked(bytes: []const u8) Self {
            return .{
                .bytes = bytes,
                .pos = 0,
            };
        }

        pub fn nextSlice(it: *Self) ?[]const u8 {
            if (it.pos == it.bytes.len) return null;
            const n = sequenceLen(it.bytes[it.pos]) catch unreachable;
            const slice = it.bytes[it.pos..][0..n];
            it.pos += n;
            return slice;
        }

        pub fn nextCodepoint(it: *Self) ?u21 {
            const slice = it.nextSlice() orelse return null;
            switch (slice.len) {
                1 => return slice[0],
                2 => return decode2Unchecked(slice[0..2]),
                3 => return decode3Unchecked(slice[0..3]),
                4 => return decode4Unchecked(slice[0..4]),
                else => unreachable,
            }
        }

        pub fn reset(it: *Self) void {
            it.pos = 0;
        }
    };
}
