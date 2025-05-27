const std = @import("std");

pub const unicode = @import("enc/unicode.zig");
pub const utf8 = unicode.utf8;
pub const wtf8 = unicode.wtf8;
pub const utf16 = unicode.utf16;
pub const wtf16 = unicode.wtf16;

test {
    _ = unicode;
}
