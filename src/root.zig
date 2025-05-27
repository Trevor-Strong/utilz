const std = @import("std");

pub const atomic = @import("atomic.zig");
pub const bit = @import("bit.zig");
pub const meta = @import("meta.zig");
pub const enc = @import("enc.zig");
pub const tst = @import("tst.zig");

pub const NullAllocator = @import("NullAllocator.zig");

pub const null_allocator: std.mem.Allocator = .{
    .ptr = undefined,
    .vtable = &NullAllocator.vtable,
};

pub const zero_reader = ConstReader(0).reader();

pub fn ConstReader(comptime value: u8) type {
    return struct {
        pub const Reader = std.io.Reader(@This(), Error, read);
        pub const Error = error{};

        const Self = @This();

        pub fn read(self: Self, buf: []u8) Error!usize {
            _ = self;
            @memset(buf, value);
            return buf.len;
        }

        pub fn reader(self: Self) Reader {
            return .{ .context = self };
        }
    };
}

test {
    std.testing.refAllDecls(@This());
}
