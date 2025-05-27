const std = @import("std");
const builtin = @import("builtin");
const util = @import("root.zig");

pub const RefCount = @import("atomic/refcnt.zig").RefCount;

pub fn Value(comptime T: type) type {
    return extern struct {
        raw: T,
        const Self = @This();

        pub inline fn init(initial_value: T) Self {
            return .{ .raw = initial_value };
        }

        pub inline fn cast(self: anytype, comptime NewChild: type) Cast(@TypeOf(self), NewChild) {
            return @ptrCast(self);
        }

        pub fn Cast(comptime SelfType: type, comptime NewChild: type) type {
            return util.meta.ptr.Rebind(SelfType, NewChild, .{ .old_child = Self });
        }

        pub inline fn castMut(self: *Self, comptime NewChild: type) *align(@alignOf(Self)) Value(NewChild) {
            return @ptrCast(self);
        }

        pub inline fn castConst(self: *const Self, comptime NewChild: type) *align(@alignOf(Self)) const Value(NewChild) {
            return @ptrCast(self);
        }

        pub inline fn fromPtr(ptr: anytype) FromPtr(@TypeOf(ptr)) {
            return @ptrCast(ptr);
        }

        pub fn FromPtr(comptime Ptr: type) type {
            return util.meta.ptr.Rebind(Ptr, Self, .{ .old_child = T });
        }

        pub inline fn fromMutPtr(ptr: *T) *Self {
            return @ptrCast(ptr);
        }

        pub inline fn fromConstPtr(ptr: *const T) *const Self {
            return @ptrCast(ptr);
        }

        pub inline fn fence(self: *const Self, comptime order: Ordering) void {
            switch (order) {
                .acquire, .seq_cst => _ = self.load(order),
                .release, .acq_rel => {
                    // We cast to an integer so that we can do a plain `
                    // fetchAdd(0)`. We do this instead of a
                    // `fetchAdd(std.mem.zeroes(T))` because `T` *could* be an
                    // exhaustive enum without a `0` field, which would cause
                    // `std.mem.zeroes(T)` to be a compile error when it doesn't
                    // need to be. for the effect we want.
                    //
                    // We need the const cast because the only operations that
                    // support the `.release` and `.acq_rel` orderings are
                    // mutating. It's safe for use to do the const cast b/c
                    // `x + 0 == x` so the resulting value is the same as the
                    // input.
                    const as_int: *Value(util.meta.IntRepr(T)) = @constCast(self.cast(util.meta.IntRepr(T)));
                    _ = as_int.fetchAdd(0, order);
                },
                .monotonic, .none => @compileError("Invalid fence order: " ++ @tagName(order)),
            }
        }

        /// Equivalent to `load(.monotonic)`
        pub inline fn get(self: *const Self) T {
            return self.load(.monotonic);
        }

        /// Equivalent to `load(.acquire)`
        pub inline fn getAcq(self: *const Self) T {
            return self.load(.acquire);
        }

        /// Equivalent to `load(.seq_cst)`
        pub inline fn getSeq(self: *const Self) T {
            return self.load(.seq_cst);
        }

        /// Equivalent to `store(.monotonic)`
        pub inline fn set(self: *Self, new_value: T) void {
            return self.store(new_value, .monotonic);
        }

        /// Equivalent to `store(.release)`
        pub inline fn setRel(self: *Self, new_value: T) void {
            self.store(new_value, .release);
        }

        /// Equivalent to `store(.seq_cst)`
        pub inline fn setSeq(self: *Self, new_value: T) void {
            self.store(new_value, .seq_cst);
        }

        /// Atomically reads the value of `self` using the requested ordering.
        ///
        /// `order` may be any ordering **except** `.release` and `.acq_rel`.
        pub inline fn load(self: *const Self, comptime order: Ordering) T {
            return @atomicLoad(T, &self.raw, order.toAtomicOrder());
        }

        /// Atomically writes `new_value` to `self` using the requested
        /// ordering.
        ///
        /// `order` may be any ordering **except** `.release` and `.acq_rel`.
        pub inline fn store(self: *Self, new_value: T, comptime order: Ordering) void {
            @atomicStore(T, &self.raw, new_value, order.toAtomicOrder());
        }

        pub inline fn swap(self: *Self, new_value: T, comptime order: Ordering) T {
            return self.rmw(.swap, new_value, order);
        }

        /// Atomically compares the current value of the variable to `expected`.
        /// If they are equal sets the value of the variable to `new`. If they
        /// differ
        pub inline fn tryCmpxchg(
            self: *Self,
            expected: T,
            new: T,
            comptime success_order: Ordering,
        ) ?T {
            return self.cmpxchgWeak(expected, new, success_order, .monotonic);
        }

        pub inline fn cmpxchg(
            self: *Self,
            expected: T,
            new: T,
            comptime success_order: Ordering,
        ) ?T {
            return self.cmpxchgStrong(expected, new, success_order, .monotonic);
        }

        pub inline fn cmpxchgStrong(
            self: *Self,
            expected: T,
            new: T,
            comptime success_order: Ordering,
            comptime fail_order: Ordering,
        ) ?T {
            return @cmpxchgStrong(
                T,
                &self.raw,
                expected,
                new,
                success_order.toAtomicOrder(),
                fail_order.toAtomicOrder(),
            );
        }

        pub inline fn cmpxchgWeak(
            self: *Self,
            expected: T,
            new: T,
            comptime success_order: Ordering,
            comptime fail_order: Ordering,
        ) ?T {
            return @cmpxchgWeak(
                T,
                &self.raw,
                expected,
                new,
                success_order.toAtomicOrder(),
                fail_order.toAtomicOrder(),
            );
        }

        pub inline fn fetchAdd(self: *Self, operand: T, comptime order: Ordering) T {
            return self.rmw(.add, operand, order);
        }

        pub inline fn fetchSub(self: *Self, operand: T, comptime order: Ordering) T {
            return self.rmw(.sub, operand, order);
        }

        pub inline fn fetchMin(self: *Self, operand: T, comptime order: Ordering) T {
            return self.rmw(.min, operand, order);
        }

        pub inline fn fetchMax(self: *Self, operand: T, comptime order: Ordering) T {
            return self.rmw(.max, operand, order);
        }

        pub inline fn fetchAnd(self: *Self, operand: T, comptime order: Ordering) T {
            return self.rmw(.@"and", operand, order);
        }

        pub inline fn fetchNand(self: *Self, operand: T, comptime order: Ordering) T {
            return self.rmw(.nand, operand, order);
        }

        pub inline fn fetchOr(self: *Self, operand: T, comptime order: Ordering) T {
            return self.rmw(.@"or", operand, order);
        }

        pub inline fn fetchXor(self: *Self, operand: T, comptime order: Ordering) T {
            return self.rmw(.xor, operand, order);
        }

        pub const Bit = std.math.Log2Int(util.meta.IntRepr(T));

        pub inline fn bitSet(self: *Self, bit: Bit, comptime order: Ordering) u1 {
            const Int = util.meta.IntRepr(T);
            const ptr: *Int = @ptrCast(&self.raw);
            const mask: Int = @as(Int, 1) << bit;
            const old = @atomicRmw(
                Int,
                ptr,
                RmwOp.toAtomicRmwOp(.@"or"),
                mask,
                order.toAtomicOrder(),
            );
            return @truncate(old >> bit);
        }

        pub inline fn bitClear(self: *Self, bit: Bit, comptime order: Ordering) u1 {
            const Int = util.meta.IntRepr(T);
            const ptr: *Int = @ptrCast(&self.raw);
            const mask: Int = ~@as(Int, 1) << bit;
            const old = @atomicRmw(
                Int,
                ptr,
                RmwOp.toAtomicRmwOp(.@"and"),
                mask,
                order.toAtomicOrder(),
            );
            return @truncate(old >> bit);
        }

        pub inline fn bitToggle(self: *Self, bit: Bit, comptime order: Ordering) u1 {
            const Int = util.meta.IntRepr(T);
            const ptr: *Int = @ptrCast(&self.raw);
            const mask: Int = @as(Int, 1) << bit;
            const old = @atomicRmw(
                Int,
                ptr,
                RmwOp.toAtomicRmwOp(.xor),
                mask,
                order.toAtomicOrder(),
            );
            return @truncate(old >> bit);
        }

        pub inline fn rmw(self: *Self, comptime op: RmwOp, operand: T, comptime order: Ordering) T {
            return util.meta.fromInt(T, @atomicRmw(
                util.meta.IntRepr(T),
                @as(util.meta.IntRepr(T), @ptrCast(&self.raw)),
                op.toAtomicRmwOp(),
                util.meta.toInt(operand),
                order.toAtomicOrder(),
            ));
        }

        pub fn addFetch(self: *Self, operand: T, comptime order: Ordering) T {
            return self.rmwFetch(.add, operand, order);
        }

        pub fn subFetch(self: *Self, operand: T, comptime order: Ordering) T {
            return self.rmwFetch(.sub, operand, order);
        }

        pub fn andFetch(self: *Self, operand: T, comptime order: Ordering) T {
            return self.rmwFetch(.@"and", operand, order);
        }

        pub fn orFetch(self: *Self, operand: T, comptime order: Ordering) T {
            return self.rmwFetch(.@"or", operand, order);
        }

        pub fn nandFetch(self: *Self, operand: T, comptime order: Ordering) T {
            return self.rmwFetch(.nand, operand, order);
        }

        pub fn xorFetch(self: *Self, operand: T, comptime order: Ordering) T {
            return self.rmwFetch(.xor, operand, order);
        }

        pub fn minFetch(self: *Self, operand: T, comptime order: Ordering) T {
            return self.rmwFetch(.min, operand, order);
        }

        pub fn maxFetch(self: *Self, operand: T, comptime order: Ordering) T {
            return self.rmwFetch(.max, operand, order);
        }

        /// Same as `rmw()` but returns the *written* value instead of the
        /// *read* value.
        pub inline fn rmwFetch(self: *Self, comptime op: RmwOp, operand: T, comptime order: Ordering) T {
            const arg = util.meta.toInt(operand);
            const prev = @atomicRmw(
                util.meta.IntRepr(T),
                @as(util.meta.IntRepr(T), @ptrCast(&self.raw)),
                op.toAtomicRmwOp(),
                arg,
                order.toAtomicOrder(),
            );

            switch (op) {
                .swap => return arg,
                .add => return prev +% arg,
                .sub => return prev -% arg,
                .@"and" => return prev & arg,
                .@"or" => return prev | arg,
                .nand => return ~(prev & arg),
                .xor => return prev ^ arg,
                .min => return @min(prev, arg),
                .max => return @max(prev, arg),
            }
        }
    };
}

/// Ordering requirements to place on an atomic operation.
pub const Ordering = enum {
    /// No ordering on the operation is enforced. The only garantee made by this
    /// order is that the operation is atomic.
    ///
    /// This ordering is valid on all operations except `cmpxchg` operations
    none,
    /// No ordering requirements are imposed from using *just* this ordering;
    /// however, unlike `.none`, `.monotonic` operation *synchronize with* the
    /// stronger orderings
    ///
    /// This ordering is valid on all operations.
    monotonic,
    /// Forces atomic operations on effected variable and non-atomic operations
    /// on other variables that occur *after* an operation with this ordering to
    /// actually execute after the `.acquire` operation.
    ///
    /// Like the name suggests, the `.acquire` ordering is the minimal strength
    /// of ordering required to *acquire* a mutex lock.
    ///
    /// This ordering is valid on all operations except for the basic `store`
    /// operation.
    acquire,
    /// Forces atomic operations on effected variable and non-atomic operations
    /// on other variables that occur *before* an operation with this ordering
    /// to actually execute before the `.release` operation.
    ///
    /// Like the name suggests, the `.release` ordering is the minimal strength
    /// of ordering required to *release* a mutex lock.
    ///
    /// This ordering is valid on all operations except for the basic `load`
    /// operation.
    release,
    /// Acts as both an `.acquire` and `.release` operation: All atomic
    /// operations on the same variable actually execute *before* the operation
    /// and all atomic operations that occur after the operation on the same
    /// variable actually execute *after* the operation. Non atomic operations
    /// may not be reordered past the point where this operation occurs.
    ///
    /// For Read-Modify-Write operations, this ordering can be thought of as
    /// doing an `.acquire` ordered *read* and a `.release` ordered *write*.
    ///
    /// This ordering is invalid the basic `load` and `store` operations.
    acq_rel,
    /// Same as `.acq_rel` the following additional guarantees:
    ///
    /// - Atomic operations on *other* variables are additionally fenced from
    ///   being reordered before or after an operation with this ordering.
    /// - A single and total ordering of the modifications made to a variable is
    ///   established and all modifications in this ordering are visible to all
    ///   threads.
    ///
    /// The only *slight* difference between this ordering and `.acq_rel` is
    /// that this ordering is valid on basic `load` and `store` operations,
    /// acting as just an `.acquire` or `.release` respectively in addition to
    /// establishing the total modification order.
    seq_cst,

    pub const unordered: Ordering = .none;

    pub const acq: Ordering = .acquire;
    pub const rel: Ordering = .release;
    pub const seq: Ordering = .seq_cst;

    pub inline fn fromAtomicOrder(atomic_order: std.builtin.AtomicOrder) Ordering {
        return switch (atomic_order) {
            .unordered => .none,
            .monotonic => .monotonic,
            .acquire => .acquire,
            .release => .release,
            .acq_rel => .acq_rel,
            .seq_cst => .seq_cst,
        };
    }
    pub inline fn toAtomicOrder(order: Ordering) std.builtin.AtomicOrder {
        return switch (order) {
            .none => .unordered,
            .monotonic => .monotonic,
            .acquire => .acquire,
            .release => .release,
            .acq_rel => .acq_rel,
            .seq_cst => .seq_cst,
        };
    }
};

/// Atomic **R**ead-**M**odify-**W**rite operations.
pub const RmwOp = enum {
    swap,
    add,
    sub,
    @"and",
    nand,
    @"or",
    xor,
    max,
    min,

    pub const Xchg: RmwOp = .swap;
    pub const Add: RmwOp = .add;
    pub const Sub: RmwOp = .sub;
    pub const And: RmwOp = .@"and";
    pub const Nand: RmwOp = .nand;
    pub const Or: RmwOp = .@"or";
    pub const Xor: RmwOp = .xor;
    pub const Max: RmwOp = .max;
    pub const Min: RmwOp = .min;

    pub fn fromAtomicRmwOp(op: std.builtin.AtomicRmwOp) std.builtin.AtomicRmwOp {
        switch (op) {
            .Xchg => .swap,
            .Add => .add,
            .Sub => .sub,
            .And => .@"and",
            .Nand => .nand,
            .Or => .@"or",
            .Xor => .xor,
            .Max => .max,
            .Min => .min,
        }
    }

    pub fn toAtomicRmwOp(op: RmwOp) std.builtin.AtomicRmwOp {
        switch (op) {
            .swap => .Xchg,
            .add => .Add,
            .sub => .Sub,
            .@"and" => .And,
            .nand => .Nand,
            .@"or" => .Or,
            .xor => .Xor,
            .max => .Max,
            .min => .Min,
        }
    }
};

/// Creates a point where the *compiler* cannot reorder read or write
/// instructions past.
///
/// In other words: the compiler is restricted from moving reads and writes that
/// are *before* a call to this function in source code *after* the call to this
/// function in the compiled machine code and may not move reads and writes that
/// are *after* a call to this function in source code *before* the call to this
/// function in the compile machine code.
///
/// Calling this function has no effect on what the CPU is allowed to do with
/// the code it gets and only restricts what the *compiler* is allowed to do.
pub fn compilerRwBarrier() void {
    // ASM is not allowed at compile time, but `comptime` logic is also single
    // threaded, so we can just omit the barrier
    if (!@inComptime()) {
        asm volatile ("" ::: "memory");
    }
}

/// Prevent reordering operations across this function call that would affect
/// synchronization with respect to asynchronous signals.
///
/// Operations that appear before this function are visible to asynchronous
/// signal handlers that run after this function is called.
pub fn signalFence() void {
    compilerRwBarrier();
}

/// Hint to the CPU that a spin loop is executing. This *may* allow the CPU
/// to deprioritize the current thread while in a spin loop
pub fn spinLoopHint() void {
    std.atomic.spinLoopHint();
}

/// Introduce a memory fence of the specified type.
pub inline fn fence(comptime order: Ordering) void {
    const static = struct {
        var dummy: Value(u0) = .init(0);
    };

    static.dummy.fence(order);
}

/// Maximum bit size supported by atomic operations.
pub const max_bits = maxBitSizeForTarget(builtin.target);

/// Returns the maximum bitsize that atomic operations are supported for on the
/// given target.
pub fn maxBitSizeForTarget(target: std.Target) u16 {
    // This is what the Zig compiler currently does. If that ever changes, this
    // should be updated.
    return target.ptrBitWidth();
}

/// The size of a single CPU cache line (or a best estimate of it) for the
/// targeted CPU.
pub const cache_line: comptime_int = cacheLineForCpu(builtin.cpu);

/// The size of a single CPU cache line for the given CPU.
pub fn cacheLineForCpu(cpu: std.Target.Cpu) u16 {
    return std.atomic.cacheLineForCpu(cpu);
}
