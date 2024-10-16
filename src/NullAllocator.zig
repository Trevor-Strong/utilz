const std = @import("std");

const NullAllocator = @This();

pub fn allocator(self: NullAllocator) std.mem.Allocator {
    _ = self;
    return .{
        .ptr = undefined,
        .vtable = &vtable,
    };
}

fn alloc(ctx: *anyopaque, len: usize, ptr_align: u8, ret_addr: usize) ?[*]u8 {
    _ = ctx;
    _ = len;
    _ = ptr_align;
    _ = ret_addr;
    return null;
}

pub const vtable = std.mem.Allocator.VTable{
    .alloc = alloc,
    .resize = std.mem.Allocator.noResize,
    .free = std.mem.Allocator.noFree,
};
