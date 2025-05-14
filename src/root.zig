pub const int = @import("int.zig");
pub const pack = @import("pack.zig");
pub const slice = @import("slice.zig");
pub const meta = @import("meta.zig");
pub const NullAllocator = @import("NullAllocator.zig");

pub const null_allocator = @import("std").mem.Allocator{
    .ptr = undefined,
    .vtable = &NullAllocator.vtable,
};

test {
    @import("std").testing.refAllDeclsRecursive(@This());
}
