const std = @import("std");
const builtin = @import("builtin");
const util = @import("../root.zig");
const utf8_impl = @import("utf8.zig");
const utf16_impl = @import("utf16.zig");

const native_endian = builtin.cpu.arch.endian();

const assert = std.debug.assert;

/// Byte Order Mark (BOM) character that may be placed at the start of a
/// document to indicate to the parser the byte order of the document. May also
/// be used to indicate that the document is unicode encoded.
///
/// When not the first character in a document, this codepoint is a zero-width,
/// non-breaking space.
pub const byte_order_mark = 0xFEFF;

/// Special character unicode recommends as using to represent any invalid or
/// unknown codepoints and character sequences.
///
/// The behavior to replace invalid data with a placeholder is not itself
/// recommended or discouraged. Only that this character be used as the
/// placeholder when such behavior is desired.
pub const replacement_character = 0xFFFD;

/// Maximum valid codepoint
pub const max_codepoint = 0x10FFFF;

/// Minimum surrogate codepoint.
///
/// Surrogate codepoints are not valid Unicode characters.
pub const min_surrogate_codepoint = 0xD800;
/// Maximum surrogate codepoint.
///
/// Surrogate codepoints are not valid Unicode characters.
pub const max_surrogate_codepoint = 0xDFFF;

/// Minimum low/trailing surrogate value.
pub const min_low_surrogate = 0xDC00;
/// Maximum low/trailing surrogate value.
pub const max_low_surrogate = 0xDFFF;
/// Minimum high/leading surrogate value.
pub const min_high_surrogate = 0xD800;
/// Maximum high/leading surrogate value.
pub const max_high_surrogate = 0xDBFF;

/// Maximum codepoint value of the basic-multilingual-plane (BMP).
pub const max_bmp_codepoint = 0xFFFF;

/// Returns `true` if `codepoint` is a *supplementary* codepoint - or a codepoint
/// outside of the basic multilingual plane. Supplementary codepoints always
/// require 4 bytes to encode (reminder: 4 bytes == 2 UTF-16 codeunits).
pub fn isSupplementary(codepoint: u21) bool {
    return utf16_impl.isSupplementaryCodepoint(codepoint);
}

/// Returns `true` if `codepoint` is a *supplementary* codepoint - or a codepoint
/// outside of the basic multilingual plane. BMP codepoints can be encoded in a
/// single UTF-16 codeunit.
pub fn isBmpCodepoint(codepoint: u21) bool {
    utf16_impl.isBmpCodepoint(codepoint);
}

/// Returns `true` if `codepoint` is a surrogate codepoint.
///
/// Note: Surrogate codepoints are not valid Unicode characters
pub fn isSurrogate(codepoint: u21) bool {
    return utf16_impl.isSurrogateCodepoint(codepoint);
}

/// Returns `true` if `codepoint` is a high/leading surrogate codepoint.
///
/// Note: Surrogate codepoints are not valid Unicode characters.
pub fn isHighSurrogate(codepoint: u21) bool {
    const mask = ~@as(u21, std.math.maxInt(u10));
    return codepoint & mask == 0xD800;
}

/// Returns `true` if `codepoint` is a low/trailing surrogate codepoint.
///
/// Note: Surrogate codepoints are not valid Unicode characters.
pub fn isLowSurrogate(codepoint: u21) bool {
    const mask = ~@as(u21, std.math.maxInt(u10));
    return codepoint & mask == 0xDC00;
}

/// Unicode Transformation Format - 8 Bit
///
/// Encodes unicode codepoints in 8-bit units. Compatible with US-ASCII.
pub const utf8 = struct {
    pub const Count = utf8_impl.Count(.utf8);

    const config: utf8_impl.Config = .utf8;
    pub fn sequenceLen(lead_byte: u8) error{InvalidLeadByte}!u3 {
        return utf8_impl.sequenceLen(lead_byte);
    }

    pub const CodepointByteLenError = error{
        CodepointTooLarge,
        SurrogateCodepoint,
    };
    pub fn codepointLen(codepoint: u21) CodepointByteLenError!u3 {
        return utf8_impl.codepointByteLen(codepoint, .utf8);
    }

    /// Count the number of codepoints in `bytes`
    pub fn countCodepoints(bytes: []const u8) Count {
        return utf8_impl.countCodepoints(bytes, .utf8, false);
    }

    /// Count the number of codepoints in `bytes` checking only what is
    /// necessary to determine the number of codepoints and not fully validating
    /// that `bytes` is valid UTF-8.
    ///
    /// See also: `countCodepoints()`, `countUtf16CodeunitsFast()`
    pub fn countCodepointsFast(bytes: []const u8) Count {
        return utf8_impl.countCodepoints(bytes, .utf8, true);
    }

    pub fn countUtf16Codeunits(bytes: []const u8) Count {
        return utf8_impl.countUtf16Units(bytes, .utf8, false);
    }

    pub fn countUtf16CodeunitsFast(bytes: []const u8) Count {
        return utf8_impl.countUtf16Units(bytes, .utf8, true);
    }

    pub const EncodeIntoError = error{NoSpaceLeft} || EncodeError;

    pub fn encodeInto(dest: []u8, codepoint: u21) EncodeIntoError!u3 {
        return try utf8_impl.encodeInto(dest, codepoint, .utf8);
    }

    pub const EncodeError = error{
        CodepointTooLarge,
        SurrogateCodepoint,
    };

    pub fn encode(codepoint: u21) EncodeError!std.BoundedArray(u8, 4) {
        return utf8_impl.encode(codepoint, .utf8);
    }

    pub fn encode2Unchecked(codepoint: u21) [2]u8 {
        return utf8_impl.encode2Unchecked(codepoint);
    }

    pub fn encode3Unchecked(codepoint: u21) [3]u8 {
        return utf8_impl.encode3Unchecked(codepoint);
    }

    pub fn encode4Unchecked(codepoint: u21) [4]u8 {
        return utf8_impl.encode4Unchecked(codepoint);
    }

    pub const DecodeError = utf8_impl.Utf8DecodeError;

    pub fn decode(source: []const u8) DecodeError!struct { u21, u3 } {
        return utf8_impl.decode(source, .utf8);
    }

    pub const Decode2Error = error{
        ExpectedContinuation,
        OverlongEncoding,
    };

    pub fn decode2(source: *const [2]u8) Decode2Error!u21 {
        return try utf8_impl.decode2(source, .utf8);
    }

    pub const Decode3Error = error{SurrogateCodepoint} || Decode2Error;

    pub fn decode3(source: *const [3]u8) Decode3Error!u21 {
        return try utf8_impl.decode3(source, .utf8);
    }
    pub const Decode4Error = error{CodepointTooLarge} || Decode2Error;

    pub fn decode4(source: *const [4]u8) Decode4Error!u21 {
        return try utf8_impl.decode4(source, .utf8);
    }

    pub const ToUtf16 = Conversion(utf16.EncodeIntoError || DecodeError);

    pub inline fn toUtf16(
        dest: []u16,
        source: []const u8,
        endian: std.builtin.Endian,
    ) ToUtf16 {
        switch (endian) {
            .big => return toUtf16Be(dest, source),
            .little => return toUtf16Le(dest, source),
        }
    }

    pub fn toUtf16Be(
        dest: []u16,
        source: []const u8,
    ) ToUtf16 {
        return convert8To16(dest, source, .big, false, ToUtf16.Error);
    }

    pub fn toUtf16Le(
        dest: []u16,
        source: []const u8,
    ) ToUtf16 {
        return convert8To16(dest, source, .little, false, ToUtf16.Error);
    }

    pub fn toUtf16Ne(
        dest: []u16,
        source: []const u8,
    ) ToUtf16 {
        toUtf16(dest, source, native_endian);
    }

    pub const Iterator = utf8_impl.IteratorImpl(.utf8);
    pub const ValidatedIterator = utf8_impl.PreCheckedIteratorImpl(.utf8);
};

/// Wobbly Transformation Format - 8 Bit.
///
/// Super set of UTF-8 that allows unpaired surrogates to be encoded. Other than
/// the fact that surrogate codepoints are allowed to be encoded, this encoding
/// is identical to UTF-8.
pub const wtf8 = struct {
    pub const Count = utf8_impl.Count(.wtf8);

    const config: utf8_impl.Config = .wtf8;
    pub fn sequenceLen(lead_byte: u8) error{InvalidLeadByte}!u3 {
        return utf8_impl.sequenceLen(lead_byte);
    }

    pub const CodepointByteLenError = error{
        CodepointTooLarge,
    };
    pub fn codepointLen(codepoint: u21) CodepointByteLenError!u3 {
        return utf8_impl.codepointByteLen(codepoint, .wtf8);
    }

    /// Count the number of codepoints in `bytes`
    pub fn countCodepoints(bytes: []const u8) Count {
        return utf8_impl.countCodepoints(bytes, .wtf8, false);
    }

    /// Count the number of codepoints in `bytes` checking only what is
    /// necessary to determine the number of codepoints and not fully validating
    /// that `bytes` is valid WTF-8.
    ///
    /// See also: `countCodepoints()`, `countUtf16CodeunitsFast()`
    pub fn countCodepointsFast(bytes: []const u8) Count {
        return utf8_impl.countCodepoints(bytes, .wtf8, true);
    }

    pub fn countUtf16Codeunits(bytes: []const u8) Count {
        return utf8_impl.countUtf16Units(bytes, .wtf8, false);
    }

    pub fn countWtf16CodeunitsFast(bytes: []const u8) Count {
        return utf8_impl.countUtf16Units(bytes, .wtf8, true);
    }

    pub const EncodeIntoError = error{NoSpaceLeft} || EncodeError;

    pub fn encodeInto(dest: []u8, codepoint: u21) EncodeIntoError!u3 {
        return try utf8_impl.encodeInto(dest, codepoint, .wtf8);
    }

    pub const EncodeError = error{
        CodepointTooLarge,
    };

    pub fn encode(codepoint: u21) EncodeError!std.BoundedArray(u8, 4) {
        return utf8_impl.encode(codepoint, .wtf8);
    }

    pub fn encode2Unchecked(codepoint: u21) [2]u8 {
        return utf8_impl.encode2Unchecked(codepoint);
    }

    pub fn encode3Unchecked(codepoint: u21) [3]u8 {
        return utf8_impl.encode3Unchecked(codepoint);
    }

    pub fn encode4Unchecked(codepoint: u21) [4]u8 {
        return utf8_impl.encode4Unchecked(codepoint);
    }

    pub const DecodeError = utf8_impl.Wtf8DecodeError;

    pub fn decode(source: []const u8) DecodeError!struct { u21, u3 } {
        return utf8_impl.decode(source, .wtf8);
    }

    pub const Decode2Error = error{
        ExpectedContinuation,
        OverlongEncoding,
    };

    pub fn decode2(source: *const [2]u8) Decode2Error!u21 {
        return try utf8_impl.decode2(source, .wtf8);
    }

    pub const Decode3Error = error{SurrogateCodepoint} || Decode2Error;

    pub fn decode3(source: *const [3]u8) Decode3Error!u21 {
        return try utf8_impl.decode3(source, .wtf8);
    }
    pub const Decode4Error = error{CodepointTooLarge} || Decode2Error;

    pub fn decode4(source: *const [4]u8) Decode4Error!u21 {
        return try utf8_impl.decode4(source, .wtf8);
    }

    pub const ToWtf16 = Conversion(wtf16.EncodeIntoError || DecodeError);

    pub inline fn toWtf16(
        dest: []u16,
        source: []const u8,
        endian: std.builtin.Endian,
    ) ToWtf16 {
        switch (endian) {
            .big => return toWtf16Be(dest, source),
            .little => return toWtf16Le(dest, source),
        }
    }

    pub fn toWtf16Be(
        dest: []u16,
        source: []const u8,
    ) ToWtf16 {
        return convert8To16(dest, source, .big, true, ToWtf16.Error);
    }

    pub fn toWtf16Le(
        dest: []u16,
        source: []const u8,
    ) ToWtf16 {
        return convert8To16(dest, source, .little, true, ToWtf16.Error);
    }

    pub fn toWtf16Ne(
        dest: []u16,
        source: []const u8,
    ) ToWtf16 {
        toWtf16(dest, source, native_endian);
    }

    pub const Iterator = utf8_impl.IteratorImpl(.wtf8);
    pub const ValidatedIterator = utf8_impl.PreCheckedIteratorImpl(.wtf8);
};

/// Unicode Transformation Format - 16 Bit
///
/// Encodes unicode codepoints in 16-bit units.
pub const utf16 = struct {
    const config: utf16_impl.Config = .utf16;

    /// Returns the endian-ness of the BOM character. If `maybe_bom` is not
    /// `0xFEFF` or `0xFFFE` (Unicode BOM or its byte-swapped form) then `null`
    /// is returned. `maybe_bom` should be created by ORing the first two bytes
    /// of the text like `(@as(u16, first_byte) << 8) | second_byte` where
    /// `first_byte` and `second_byte` are the first and second bytes of the
    /// text respectively.
    pub fn interpretBom(maybe_bom: u16) ?std.builtin.Endian {
        switch (maybe_bom) {
            @as(u16, byte_order_mark) => native_endian,
            @byteSwap(@as(u16, byte_order_mark)) => switch (native_endian) {
                .big => .little,
                .little => .big,
            },
            else => return null,
        }
    }

    pub fn codepointLen(codepoint: u21) EncodeError!u2 {
        return utf16_impl.codepointLen(codepoint, config);
    }

    pub fn sequenceLen(first: u16, endian: std.builtin.Endian) u2 {
        return @as(u2, 1) + @intFromBool(utf16.isHighSurrogate(first, endian));
    }

    pub const EncodeError = error{
        /// Codepoint value is a surrogate codepoint
        SurrogateCodepoint,
    } || wtf16.EncodeError;

    pub inline fn encode(
        codepoint: u21,
        endian: std.builtin.Endian,
    ) EncodeError!std.BoundedArray(u16, 2) {
        return try utf16_impl.encode(
            codepoint,
            endian,
            config,
        );
    }

    pub const EncodeIntoError = error{
        /// Not Enough space in `dest` to fit the encoded form of the codepoint
        NoSpaceLeft,
    } || EncodeError;

    pub inline fn encodeInto(
        dest: []u16,
        codepoint: u21,
        endian: std.builtin.Endian,
    ) EncodeIntoError!u2 {
        return try utf16_impl.encodeInto(
            dest,
            codepoint,
            endian,
            config,
        );
    }

    pub inline fn encodeSurrogatePair(
        codepoint: u21,
        endian: std.builtin.Endian,
    ) [2]u16 {
        return utf16_impl.encodeSurrogatePair(codepoint, endian);
    }

    pub const DecodeError = error{
        /// A high surrogate codeunit was followed by a non low surrogate
        /// codeunit
        UnpairedHighSurrogate,
        /// A low surrogate codeunit was encountered without a high surrogate
        /// codeunit preceding it
        LeadingLowSurrogate,
        /// The last codeunit in the buffer was a high surrogate.
        SequenceTruncated,
    } || wtf16.DecodeError;

    pub inline fn decode(
        source: []const u16,
        endian: std.builtin.Endian,
    ) DecodeError!struct { u21, u2 } {
        return utf16_impl.decode(source, endian, config);
    }

    pub inline fn decodeUnaligned(
        source: []align(1) const u16,
        endian: std.builtin.Endian,
    ) DecodeError!struct { u21, u2 } {
        return utf16_impl.decodeUnaligned(source, endian, config);
    }

    pub inline fn decodeSurrogatePair(
        hi: u16,
        lo: u16,
        endian: std.builtin.Endian,
    ) u21 {
        assert(utf16.isHighSurrogate(hi, endian));
        assert(utf16.isLowSurrogate(lo, endian));
        switch (endian) {
            .big => return be.decodeSurrogatePair(hi, lo),
            .little => return le.decodeSurrogatePair(hi, lo),
        }
    }

    pub const ToUtf8 = Conversion(utf8.EncodeIntoError || DecodeError);
    pub inline fn toUtf8(dest: []u8, source: []const u16, endian: std.builtin.Endian) ToUtf8 {
        switch (endian) {
            .big => return be.toUtf8(dest, source),
            .little => return le.toUtf8(dest, source),
        }
    }

    pub inline fn isSurrogate(c: u16, endian: std.builtin.Endian) bool {
        const native = std.mem.toNative(u16, c, endian);
        return native & 0xF800 == 0xD800;
    }

    pub inline fn isHighSurrogate(c: u16, endian: std.builtin.Endian) bool {
        const native = std.mem.toNative(u16, c, endian);
        return native & 0xFC00 == 0xD800;
    }

    pub inline fn isLowSurrogate(c: u16, endian: std.builtin.Endian) bool {
        const native = std.mem.toNative(u16, c, endian);
        return native & 0xFC00 == 0xDC00;
    }

    /// Little endian
    pub const be = Order(.big);
    /// Big endian
    pub const le = Order(.little);
    // Native endian
    pub const ne = Order(native_endian);

    fn Order(comptime endian: std.builtin.Endian) type {
        return struct {
            pub fn sequenceLen(first: u16) u2 {
                return @as(u2, 1) + @intFromBool(@This().isHighSurrogate(first));
            }

            pub fn encode(codepoint: u21) EncodeError!std.BoundedArray(u16, 2) {
                return utf16.encode(codepoint, endian);
            }

            pub fn encodeInto(dest: []u16, codepoint: u21) EncodeIntoError!u2 {
                return utf16.encodeInto(dest, codepoint, endian);
            }

            pub fn decode(source: []const u16) DecodeError!struct { u21, u2 } {
                return utf16.decode(source, endian);
            }

            pub fn decodeUnaligned(
                source: []align(1) const u16,
            ) DecodeError!struct { u21, u2 } {
                return utf16.decodeUnaligned(source, endian);
            }

            pub fn decodeSurrogatePair(hi: u16, lo: u16) u21 {
                assert(@This().isHighSurrogate(hi));
                assert(@This().isLowSurrogate(lo));
                if (endian == native_endian) {
                    return utf16_impl.decodeSurrogatePair(hi, lo);
                } else {
                    return utf16_impl.decodeSurrogatePair(
                        @byteSwap(hi),
                        @byteSwap(lo),
                    );
                }
            }

            pub fn toUtf8(dest: []u8, source: []const u16) ToUtf8 {
                return convert16To8(
                    dest,
                    source,
                    endian,
                    config.allow_surrogates,
                    ToUtf8.Error,
                );
            }

            pub fn isSurrogate(c: u16) bool {
                const mask = comptime std.mem.nativeTo(u16, 0xF800, endian);
                const surrogate_mask = comptime std.mem.nativeTo(
                    u16,
                    0xD800,
                    endian,
                );
                return c & mask == surrogate_mask;
            }

            pub fn isHighSurrogate(c: u16) bool {
                const mask = comptime std.mem.nativeTo(u16, 0xFC00, endian);
                const surrogate_mask = comptime std.mem.nativeTo(
                    u16,
                    0xD800,
                    endian,
                );
                return c & mask == surrogate_mask;
            }

            pub fn isLowSurrogate(c: u16) bool {
                const mask = comptime std.mem.nativeTo(u16, 0xFC00, endian);
                const surrogate_mask = comptime std.mem.nativeTo(
                    u16,
                    0xDC00,
                    endian,
                );
                return c & mask == surrogate_mask;
            }
        };
    }
};

pub const wtf16 = struct {
    const config: utf16_impl.Config = .wtf16;
    pub fn codepointLen(codepoint: u21) EncodeError!u2 {
        return utf16_impl.codepointLen(codepoint, config);
    }

    pub fn sequenceLen(first: u16, endian: std.builtin.Endian) u2 {
        return @as(u2, 1) + @intFromBool(utf16.isHighSurrogate(first, endian));
    }

    pub const EncodeError = error{
        /// Codepoint value exceeds maximum unicode codepoint
        CodepointTooLarge,
    };

    pub inline fn encode(
        codepoint: u21,
        endian: std.builtin.Endian,
    ) EncodeError!std.BoundedArray(u16, 2) {
        return try utf16_impl.encode(
            codepoint,
            endian,
            config,
        );
    }

    pub const EncodeIntoError = error{
        /// Not Enough space in `dest` to fit the encoded form of the codepoint
        NoSpaceLeft,
    } || EncodeError;

    pub inline fn encodeInto(
        dest: []u16,
        codepoint: u21,
        endian: std.builtin.Endian,
    ) EncodeIntoError!u2 {
        return try utf16_impl.encodeInto(
            dest,
            codepoint,
            endian,
            config,
        );
    }

    pub inline fn encodeSurrogatePair(
        codepoint: u21,
        endian: std.builtin.Endian,
    ) [2]u16 {
        return utf16_impl.encodeSurrogatePair(codepoint, endian);
    }

    pub const DecodeError = error{};

    pub inline fn decode(
        source: []const u16,
        endian: std.builtin.Endian,
    ) DecodeError!struct { u21, u2 } {
        return utf16_impl.decode(source, endian, config);
    }

    pub inline fn decodeUnaligned(
        source: []align(1) const u16,
        endian: std.builtin.Endian,
    ) DecodeError!struct { u21, u2 } {
        return utf16_impl.decodeUnaligned(source, endian, config);
    }

    pub inline fn decodeSurrogatePair(
        hi: u16,
        lo: u16,
        endian: std.builtin.Endian,
    ) u21 {
        assert(utf16.isHighSurrogate(hi, endian));
        assert(utf16.isLowSurrogate(lo, endian));
        switch (endian) {
            .big => return be.decodeSurrogatePair(hi, lo),
            .little => return le.decodeSurrogatePair(hi, lo),
        }
    }

    pub const ToWtf8 = Conversion(wtf8.EncodeIntoError || DecodeError);

    pub inline fn toWtf8(
        dest: []u8,
        source: []const u16,
        endian: std.builtin.Endian,
    ) ToWtf8 {
        switch (endian) {
            .big => return be.toWtf8(dest, source),
            .little => return le.toWtf8(dest, source),
        }
    }

    pub const isSurrogate = utf16.isSurrogate;
    pub const isHighSurrogate = utf16.isHighSurrogate;
    pub const isLowSurrogate = utf16.isLowSurrogate;

    pub const be = Order(.big);
    pub const le = Order(.little);
    pub const ne = Order(native_endian);

    fn Order(comptime endian: std.builtin.Endian) type {
        return struct {
            pub fn sequenceLen(first: u16) u2 {
                return @as(u2, 1) + @intFromBool(@This().isHighSurrogate(first));
            }

            pub fn encode(codepoint: u21) EncodeError!std.BoundedArray(u16, 2) {
                return wtf16.encode(codepoint, endian);
            }

            pub fn encodeInto(dest: []u16, codepoint: u21) EncodeIntoError!u2 {
                return wtf16.encodeInto(dest, codepoint, endian);
            }

            pub fn decode(source: []const u16) DecodeError!struct { u21, u2 } {
                return wtf16.decode(source, endian);
            }

            pub fn decodeUnaligned(
                source: []align(1) const u16,
            ) DecodeError!struct { u21, u2 } {
                return wtf16.decodeUnaligned(source, endian);
            }

            pub const decodeSurrogatePair = utf16.Order(endian).decodeSurrogatePair;

            pub fn toWtf8(dest: []u8, source: []const u16) ToWtf8 {
                return convert16To8(
                    dest,
                    source,
                    endian,
                    config.allow_surrogates,
                );
            }

            pub const isSurrogate = utf16.Order(endian).isSurrogate;
            pub const isHighSurrogate = utf16.Order(endian).isHighSurrogate;
            pub const isLowSurrogate = utf16.Order(endian).isLowSurrogate;
        };
    }
};

fn Conversion(comptime Err: type) type {
    return struct {
        /// Length of the destination buffer that was filled.
        dest: usize,
        /// Length of the source buffer that was converted.
        source: usize,
        /// Error that occurred during conversion or `null` if the conversion
        /// completed without error.
        err: ?Err,

        pub const Error = Err;

        pub const init: @This() = .{
            .source = 0,
            .dest = 0,
            .err = null,
        };

        /// Returns a pair `.{dest, source}` if no error occurred during
        /// conversion otherwise returns the error.
        pub fn get(self: @This()) Error![2]usize {
            return self.err orelse {
                @branchHint(.likely);
                return .{ self.dest, self.source };
            };
        }
    };
}

fn convert16To8(
    dest: []u8,
    source: []const u16,
    comptime endian: std.builtin.Endian,
    comptime allow_surrogates: bool,
    comptime Err: type,
) Conversion(Err) {
    const conf16: utf16_impl.Config = .{ .allow_surrogates = allow_surrogates };
    const conf8: utf8_impl.Config = .{ .allow_surrogates = allow_surrogates };
    var i: usize = 0;
    if (std.simd.suggestVectorLength(u16)) |chunk_len| {
        const Chunk = @Vector(chunk_len, u16);
        const ByteChunk = @Vector(chunk_len, u8);

        const len = @min(source.len, dest.len);
        if (len >= chunk_len) {
            const end = len - chunk_len;
            const mask: Chunk = std.mem.nativeTo(Chunk, @splat(0x7F), endian);
            while (true) {
                const chunk: Chunk = source[i..][0..chunk_len].*;
                if (@reduce(.Or, chunk > mask)) {
                    break;
                }
                const byte_chunk: ByteChunk = @truncate(std.mem.toNative(
                    Chunk,
                    chunk,
                    endian,
                ));
                dest[i..][0..chunk_len].* = byte_chunk;
                i += chunk_len;
                if (i > end) break;
            }
        }
        var j = i;
        var err: ?Err = null;
        while (i != source.len) {
            const codepoint, const src_n = utf16_impl.decode(
                source[i..],
                endian,
                conf16,
            ) catch |e| {
                err = e;
                break;
            };
            const dst_n = utf8_impl.encodeInto(
                dest[j..],
                codepoint,
                conf8,
            ) catch |e| {
                err = e;
                break;
            };
            i += src_n;
            j += dst_n;
        }
        return .{
            .source = i,
            .dest = j,
            .err = err,
        };
    }
}

fn convert8To16(
    dest: []u16,
    source: []const u8,
    comptime endian: std.builtin.Endian,
    comptime allow_surrogates: bool,
    comptime Err: type,
) Conversion(Err) {
    const conf8: utf8_impl.Config = .{ .allow_surrogates = allow_surrogates };
    var i: usize = 0;
    if (std.simd.suggestVectorLength(u8)) |chunk_len| {
        const Chunk = @Vector(chunk_len, u8);
        const WideChunk = @Vector(chunk_len, u16);

        const len = @min(source.len, dest.len);
        if (len >= chunk_len) {
            const end = len - chunk_len;
            const mask: Chunk = @splat(0x7F);
            while (true) {
                const chunk: Chunk = source[i..][0..chunk_len].*;
                if (@reduce(.Or, chunk > mask)) {
                    break;
                }
                const wide_chunk: WideChunk = chunk;
                dest[i..][0..chunk_len].* = wide_chunk;
                i += chunk_len;
                if (i > end) break;
            }
        }
        var j = i;
        var err: ?Err = null;
        while (i != source.len) {
            const codepoint, const src_n = utf8_impl.decode(
                source[i..],
                endian,
                conf8,
            ) catch |e| {
                err = e;
                break;
            };
            const dst_n = 1 + (src_n >> 2);

            const dst_remaining = dest[j..].len;
            if (dst_remaining < dst_n) return error.NoSpaceLeft;
            if (dst_n == 2) {
                @branchHint(.unlikely);
                dest[j..][0..2].* = utf16_impl.encodeSurrogatePair(
                    codepoint,
                    endian,
                );
            } else {
                dest[j..][0] = @intCast(codepoint);
            }

            i += src_n;
            j += dst_n;
        }
        return .{
            .source = i,
            .dest = j,
            .err = err,
        };
    }
}

test {
    _ = utf8_impl;
    _ = utf16_impl;
}

test convert16To8 {
    return util.tst.skip();
}

test convert8To16 {
    return util.tst.skip();
}
