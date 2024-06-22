const std = @import("std");

pub const pack = @import("pack.zig");
pub const slice = @import("slice.zig");

/// Constructs a string of the form "<message> found '@typeName(T)'". If
/// `message` ends with a '.', then the 'f' in "found" is capitalized; otherwise,
/// the following is appended to `message`: `" found '" ++ @typeName(T) ++ "'"`
///
/// Assumes that `message` is non-empty
///
/// The primary use for this function is to generate compile error messages for
/// generic functions.
pub fn unexpectedTypeMsg(comptime T: type, comptime message: []const u8) [:0]const u8 {
    if (message[message.len - 1] == '.') {
        return message ++ " Found '" ++ @typeName(T) ++ "'";
    } else {
        return message ++ " found '" ++ @typeName(T) ++ "'";
    }
}

/// Alias of `std.fmt.comptimePrint`
pub const p = std.fmt.comptimePrint;

test {
    std.testing.refAllDecls(@This());
}
