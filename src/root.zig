pub const bit = @import("bit.zig");
pub const int = @import("int.zig");
pub const meta = @import("meta.zig");
pub const NullAllocator = @import("NullAllocator.zig");

pub const null_allocator = @import("std").mem.Allocator{
    .ptr = undefined,
    .vtable = &NullAllocator.vtable,
};

pub const zero_reader = ConstReader(0).reader();

pub fn ConstReader(comptime value: u8) type {
    return struct {
        pub const Reader = @import("std").io.Reader(@This(), Error, read);
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
    @import("std").testing.refAllDeclsRecursive(@This());
}
