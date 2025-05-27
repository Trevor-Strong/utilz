const std = @import("std");
const atomic = @import("../atomic.zig");

pub fn RefCount(comptime T: type) type {
    return extern struct {
        pub const Counter = T;

        count: atomic.Value(T),
        const Self = @This();
        pub const one: Self = .{ .count = .init(1) };
        pub fn init(initial_count: T) Self {
            return .{ .count = .init(initial_count) };
        }

        /// Increments the reference count
        pub fn ref(rc: *Self) void {
            rc.count.fetchAdd(1, .monotonic);
        }

        /// Decrements the reference count. If the reference count hits zero as
        /// a result of this decrement `true` is returned, otherwise `false` is
        /// returned.
        pub fn unref(rc: *Self) bool {
            if (rc.count.fetchSub(1, .release) == 1) {
                rc.count.fence(.acquire);
                return true;
            } else {
                return false;
            }
        }
    };
}
