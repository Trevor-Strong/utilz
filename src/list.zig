const std = @import("std");
const utilz = @import("utilz");
const assert = std.debug.assert;

pub const meta = @import("list/meta.zig");

/// Implements basic linked list node operations, namely linking a node after
/// another node and removing a node. For node removal, only removing the "next"
/// node or the first node in a non-circular list is supported as a single node
/// doesn't have enough information to properly remove itself.
///
/// These operations can also be used to implement circular lists. The only
/// somewhat special care needed to use these operations in circular lists is
/// that the single element list must have it's `next` (and `prev` if doubly
/// linked) link refer to itself (i.e. `node.next == node`).
pub fn NodeOps(
    comptime Node: type,
    comptime next_field: std.meta.FieldEnum(Node),
    comptime prev_field: ?std.meta.FieldEnum(Node),
) type {
    if (prev_field == next_field) {
        @compileError("'prev' field and 'next' field may not be the same. Got '" ++ @tagName(next_field) ++ "' for both");
    }
    return struct {
        const doubly_linked = prev_field != null;
        const optional_links = @typeInfo(std.meta.FieldType(Node, next_field)) == .Optional;

        const Link = if (prev_field) |f|
            @TypeOf(
                @as(std.meta.FieldType(Node, next_field), undefined),
                @as(std.meta.FieldType(Node, f), undefined),
            )
        else
            std.meta.FieldType(Node, next_field);
        const NodePtr = switch (@typeInfo(Link)) {
            .Optional => |opt_info| opt_info.child,
            .Pointer => Link,
            else => unreachable,
        };

        /// Inserts `new_next` after `self` in the list
        pub fn linkNext(noalias self: NodePtr, noalias new_next: NodePtr) void {
            const old_next = getNext(self);
            setNext(new_next, old_next);
            setNext(self, new_next);
            if (doubly_linked) link_prev: {
                setPrev(new_next, self);
                const n = if (!optional_links)
                    old_next
                else
                    old_next orelse break :link_prev;
                setPrev(n, new_next);
            }
        }

        /// If a doubly linked node, unlinks `self` from the list.
        pub fn unlink(self: NodePtr) void {
            if (!doubly_linked) {
                @compileError("Cannot unlink singly linked node without previous node reference");
            }
            const prev = getPrev(self);
            const next = getNext(self);
            if (optional_links) {
                assert(self != prev and self != next);
                setNext(self, null);
                setPrev(self, null);
            } else {
                if (prev == self and next == self) {
                    return;
                }
                setNext(self, self);
                setPrev(self, self);
            }
            if (optional_links) {
                if (prev != null) setNext(prev.?, next);
                if (next != null) setPrev(next.?, prev);
            } else {
                setNext(prev, next);
                setPrev(next, prev);
            }
        }

        /// Unlinks the node, assuming that the node is the head of the list.
        ///
        /// If the node is singly linked and has a non-optional `next` pointer,
        /// a compile error is raised as there isn't anything sensible to do in
        /// the general case and you need the tail node anyways when part of a
        /// circular linked list, the most likely case when the `next` pointer
        /// isn't allowed to be `null`.
        pub fn unlinkAssumeHead(self: NodePtr) void {
            if (doubly_linked) {
                return unlink(self);
            }
            if (!optional_links) {
                @compileError("Cannot unlink head of singly linked list with non-optional link");
            }

            setNext(self, null);
        }

        /// Unlinks the node after this node returning the unlinked node
        pub fn unlinkNext(self: NodePtr) Link {
            if (doubly_linked) {
                const next = if (optional_links) next: {
                    break :next getNext(self) orelse return null;
                } else getNext(self);
                unlink(next);
                return next;
            } else {
                const to_unlink = to_unlink: {
                    if (optional_links) break :to_unlink getNext(self) orelse {
                        return null;
                    };
                    const n = getNext(self);
                    if (n == self) return self;
                    break :to_unlink n;
                };
                const next = getNext(to_unlink);
                if (optional_links)
                    setNext(to_unlink, null)
                else
                    setNext(to_unlink, to_unlink);
                setNext(self, next);
                return to_unlink;
            }
        }

        // Convenience field accessors. Users of this type should just access node
        // fields by name since they actually know what the name is and can just
        // use regular field access syntax.

        inline fn getNext(n: NodePtr) Link {
            return @field(n, @tagName(next_field));
        }

        inline fn setNext(n: NodePtr, new_next: Link) void {
            @field(n, @tagName(next_field)) = new_next;
        }

        // takes advantage of Zig's lazy analysis. These functions are only
        // analyzed (or compiled) when they might actually be called, and the
        // only code paths where we call these functions are after a check for
        // `prev_field != null`.

        inline fn getPrev(n: NodePtr) Link {
            return @field(n, @tagName(prev_field.?));
        }

        inline fn setPrev(n: NodePtr, new_next: Link) void {
            @field(n, @tagName(prev_field.?)) = new_next;
        }
    };
}

pub fn LinkIterator(
    comptime Node: type,
    comptime link_field: std.meta.FieldEnum(Node),
) type {
    if (meta.linkTypeOf(Node, link_field) != .optional) {
        @compileError("'" ++ @tagName(link_field) ++ "' field of '" ++
            @typeName(Node) ++ "' is not a valid optional link");
    }
    return struct {
        const Self = @This();

        const Link = std.meta.FieldType(Node, link_field);

        pub const Item = *Node;

        cur: Link = null,

        const ops = NodeOps(Node, link_field, null);

        pub fn init(start: *Node) Self {
            return .{
                .cur = start,
            };
        }

        pub fn next(it: *Self) ?Item {
            const n = it.cur orelse return null;
            it.cur = @field(n, @tagName(link_field));
            return n;
        }
    };
}

pub fn CircularLinkIterator(
    comptime Node: type,
    comptime link_field: std.meta.FieldEnum(Node),
) type {
    if (meta.linkTypeOf(Node, link_field) != .required) {
        @compileError("Field '" ++ @tagName(link_field) ++ "' of '" ++
            @typeName(Node) ++ "' is not a valid circular link");
    }

    return struct {
        const Self = @This();

        const Link = std.meta.FieldType(Node, link_field);
        pub const Item = Link;

        cur: Link,
        first: ?Link,

        pub fn init(start: Link) Self {
            return .{
                .cur = start,
                .first = null,
            };
        }

        pub fn next(it: *Self) ?Item {
            const n = it.cur;
            const first = it.first;
            if (first == n) return null;
            if (first == null) it.first = n;
            it.cur = @field(n, @tagName(link_field));
            return n;
        }

        pub fn reset(it: *Self) void {
            it.cur = it.first;
            it.iterated = false;
        }
    };
}

const testing = std.testing;

const SN = struct {
    next: ?*@This(),

    const ops = NodeOps(@This(), .next, null);

    const linkNext = ops.linkNext;
    const unlinkNext = ops.unlinkNext;
};

test "single:linkNext/unlinkNext" {
    var node = [_]SN{
        .{ .next = null },
        .{ .next = null },
        .{ .next = null },
    };
    errdefer {
        std.log.err(
            \\
            \\node[0]: {*}
            \\node[1]: {*}
            \\node[2]: {*}
        ,
            .{ &node[0], &node[1], &node[2] },
        );
    }

    node[0].linkNext(&node[2]);
    node[0].linkNext(&node[1]);

    // insert test

    try testing.expectEqual(SN{
        .next = &node[1],
    }, node[0]);
    try testing.expectEqual(SN{
        .next = &node[2],
    }, node[1]);
    try testing.expectEqual(SN{
        .next = null,
    }, node[2]);

    // unlink test

    try testing.expectEqual(&node[1], node[0].unlinkNext());
    try testing.expectEqual(SN{ .next = &node[2] }, node[0]);
    try testing.expectEqual(null, node[1].next);

    try testing.expectEqual(&node[2], node[0].unlinkNext());
    try testing.expectEqual(null, node[0].unlinkNext());
}

test "single:iterator" {
    const Iterator = LinkIterator(SN, .next);
    var it = Iterator{};

    try testing.expectEqual(null, it.next());

    var node = [_]SN{
        .{ .next = null },
        .{ .next = null },
        .{ .next = null },
    };
    errdefer {
        std.log.err(
            \\
            \\node[0]: {*}
            \\node[1]: {*}
            \\node[2]: {*}
        ,
            .{ &node[0], &node[1], &node[2] },
        );
    }

    node[0].linkNext(&node[1]);
    node[1].linkNext(&node[2]);

    it = Iterator.init(&node[0]);

    try testing.expectEqual(&node[0], it.next().?);
    try testing.expectEqual(&node[1], it.next().?);
    try testing.expectEqual(&node[2], it.next().?);
    try testing.expectEqual(null, it.next());

    node[0] = .{ .next = null };

    it = Iterator.init(&node[0]);
    try testing.expectEqual(&node[0], it.next().?);
    try testing.expectEqual(null, it.next());
}

const DN = struct {
    next: ?*@This(),
    prev: ?*@This(),

    const ops = NodeOps(@This(), .next, .prev);

    const linkNext = ops.linkNext;
    const unlink = ops.unlink;
};

test "double:linkNext/unlink" {
    var node = [_]DN{
        .{ .prev = null, .next = null },
        .{ .prev = null, .next = null },
        .{ .prev = null, .next = null },
    };

    errdefer {
        std.log.err(
            \\
            \\node[0]: {*}
            \\node[1]: {*}
            \\node[2]: {*}
        ,
            .{ &node[0], &node[1], &node[2] },
        );
    }

    node[0].linkNext(&node[2]);
    node[0].linkNext(&node[1]);

    try testing.expectEqual(DN{
        .next = &node[1],
        .prev = null,
    }, node[0]);
    try testing.expectEqual(DN{
        .next = &node[2],
        .prev = &node[0],
    }, node[1]);
    try testing.expectEqual(DN{
        .next = null,
        .prev = &node[1],
    }, node[2]);

    node[1].unlink();
    try testing.expectEqual(DN{ .prev = null, .next = null }, node[1]);
    try testing.expectEqual(&node[2], node[0].next);
    try testing.expectEqual(&node[0], node[2].prev);

    node[0].linkNext(&node[1]);

    {
        const third_copy = node[2];
        node[0].unlink();

        try testing.expectEqual(DN{ .prev = null, .next = null }, node[0]);
        try testing.expectEqual(
            DN{ .prev = null, .next = &node[2] },
            node[1],
        );
        try testing.expectEqual(third_copy, node[2]);
    }

    node[1].unlink();
    try testing.expectEqual(DN{ .prev = null, .next = null }, node[1]);
    try testing.expectEqual(DN{ .prev = null, .next = null }, node[2]);
}

test "double:forward_iterator" {
    const Iterator = LinkIterator(DN, .next);
    var it = Iterator{};

    try testing.expectEqual(null, it.next());

    var node = [_]DN{
        .{ .next = null, .prev = null },
        .{ .next = null, .prev = null },
        .{ .next = null, .prev = null },
    };
    errdefer {
        std.log.err(
            \\
            \\node[0]: {*}
            \\node[1]: {*}
            \\node[2]: {*}
        ,
            .{ &node[0], &node[1], &node[2] },
        );
    }

    node[0].linkNext(&node[1]);
    node[1].linkNext(&node[2]);

    it = Iterator.init(&node[0]);

    try testing.expectEqual(&node[0], it.next().?);
    try testing.expectEqual(&node[1], it.next().?);
    try testing.expectEqual(&node[2], it.next().?);
    try testing.expectEqual(null, it.next());

    node[0] = .{ .next = null, .prev = null };

    it = Iterator.init(&node[0]);
    try testing.expectEqual(&node[0], it.next().?);
    try testing.expectEqual(null, it.next());
}
test "double:backward_iterator" {
    const Iterator = LinkIterator(DN, .prev);
    var it = Iterator{};

    try testing.expectEqual(null, it.next());

    var node = [_]DN{
        .{ .next = null, .prev = null },
        .{ .next = null, .prev = null },
        .{ .next = null, .prev = null },
    };
    errdefer {
        std.log.err(
            \\
            \\node[0]: {*}
            \\node[1]: {*}
            \\node[2]: {*}
        ,
            .{ &node[0], &node[1], &node[2] },
        );
    }

    node[0].linkNext(&node[1]);
    node[1].linkNext(&node[2]);

    it = Iterator.init(&node[2]);

    try testing.expectEqual(&node[2], it.next().?);
    try testing.expectEqual(&node[1], it.next().?);
    try testing.expectEqual(&node[0], it.next().?);
    try testing.expectEqual(null, it.next());

    node[0] = .{ .next = null, .prev = null };

    it = Iterator.init(&node[0]);
    try testing.expectEqual(&node[0], it.next().?);
    try testing.expectEqual(null, it.next());
}

const CSN = struct {
    next: *@This(),
    const ops = NodeOps(@This(), .next, null);
    const linkNext = ops.linkNext;
    const unlinkNext = ops.unlinkNext;
};

test "circular:single:linkNext/unlinkNext" {
    var node: [3]CSN = undefined;

    errdefer {
        std.log.err(
            \\
            \\node[0]: {*}
            \\node[1]: {*}
            \\node[2]: {*}
        , .{ &node[0], &node[1], &node[2] });
    }

    inline for (&node) |*n| n.next = n;

    node[0].linkNext(&node[1]);

    try testing.expectEqual(CSN{ .next = &node[1] }, node[0]);
    try testing.expectEqual(CSN{ .next = &node[0] }, node[1]);

    node[1].linkNext(&node[2]);

    try testing.expectEqual(CSN{ .next = &node[1] }, node[0]);
    try testing.expectEqual(CSN{ .next = &node[2] }, node[1]);
    try testing.expectEqual(CSN{ .next = &node[0] }, node[2]);

    {
        const unlinked = node[0].unlinkNext();
        try testing.expectEqual(&node[1], unlinked);
        try testing.expectEqual(CSN{ .next = &node[2] }, node[0]);
        try testing.expectEqual(CSN{ .next = &node[0] }, node[2]);
        try testing.expectEqual(&node[1], node[1].next);
    }

    node[0].linkNext(&node[1]);

    try testing.expectEqual(CSN{ .next = &node[1] }, node[0]);
    try testing.expectEqual(CSN{ .next = &node[2] }, node[1]);
    try testing.expectEqual(CSN{ .next = &node[0] }, node[2]);

    try testing.expectEqual(&node[0], node[2].unlinkNext());
    try testing.expectEqual(&node[1], node[2].unlinkNext());
    try testing.expectEqual(&node[2], node[2].unlinkNext());
    try testing.expectEqual(&node[2], node[2].unlinkNext());
}

test "circular:single:iterator" {
    const Iterator = CircularLinkIterator(CSN, .next);

    var node = [_]CSN{
        undefined,
        undefined,
        undefined,
    };
    errdefer {
        std.log.err(
            \\
            \\node[0]: {*}
            \\node[1]: {*}
            \\node[2]: {*}
        ,
            .{ &node[0], &node[1], &node[2] },
        );
    }

    inline for (&node) |*n| n.next = n;

    node[0].linkNext(&node[1]);
    node[1].linkNext(&node[2]);

    var it = Iterator.init(&node[0]);

    try testing.expectEqual(&node[0], it.next().?);
    try testing.expectEqual(&node[1], it.next().?);
    try testing.expectEqual(&node[2], it.next().?);
    try testing.expectEqual(null, it.next());

    node[0] = .{ .next = &node[0] };

    it = Iterator.init(&node[0]);

    try testing.expectEqual(&node[0], it.next().?);
    try testing.expectEqual(null, it.next());
}

const CDN = struct {
    next: *@This(),
    prev: *@This(),
    const ops = NodeOps(@This(), .next, .prev);
    const linkNext = ops.linkNext;
    const unlink = ops.unlink;
    const unlinkNext = ops.unlinkNext;
};

test "circular:double:linkNext/unlinkNext" {
    var node: [3]CDN = undefined;

    errdefer {
        std.log.err(
            \\
            \\node[0]: {*}
            \\node[1]: {*}
            \\node[2]: {*}
        , .{ &node[0], &node[1], &node[2] });
    }

    inline for (&node) |*n| n.* = .{ .next = n, .prev = n };

    node[0].linkNext(&node[1]);

    try testing.expectEqual(CDN{ .next = &node[1], .prev = &node[1] }, node[0]);
    try testing.expectEqual(CDN{ .next = &node[0], .prev = &node[0] }, node[1]);

    node[1].linkNext(&node[2]);

    try testing.expectEqual(CDN{ .next = &node[1], .prev = &node[2] }, node[0]);
    try testing.expectEqual(CDN{ .next = &node[2], .prev = &node[0] }, node[1]);
    try testing.expectEqual(CDN{ .next = &node[0], .prev = &node[1] }, node[2]);

    node[1].unlink();
    try testing.expectEqual(CDN{
        .next = &node[2],
        .prev = &node[2],
    }, node[0]);
    try testing.expectEqual(CDN{
        .next = &node[0],
        .prev = &node[0],
    }, node[2]);
    try testing.expectEqual(CDN{
        .next = &node[1],
        .prev = &node[1],
    }, node[1]);

    node[0].linkNext(&node[1]);

    try testing.expectEqual(CDN{ .next = &node[1], .prev = &node[2] }, node[0]);
    try testing.expectEqual(CDN{ .next = &node[2], .prev = &node[0] }, node[1]);
    try testing.expectEqual(CDN{ .next = &node[0], .prev = &node[1] }, node[2]);

    try testing.expectEqual(&node[0], node[2].unlinkNext());
    try testing.expectEqual(&node[1], node[2].unlinkNext());
    try testing.expectEqual(&node[2], node[2].unlinkNext());
    try testing.expectEqual(&node[2], node[2].unlinkNext());
}

test "circular:double:forward_iterator" {
    const Iterator = CircularLinkIterator(CDN, .next);

    var node = [_]CDN{
        undefined,
        undefined,
        undefined,
    };
    errdefer {
        std.log.err(
            \\
            \\node[0]: {*}
            \\node[1]: {*}
            \\node[2]: {*}
        ,
            .{ &node[0], &node[1], &node[2] },
        );
    }

    inline for (&node) |*n| n.* = .{ .prev = n, .next = n };

    node[0].linkNext(&node[1]);
    node[1].linkNext(&node[2]);

    var it = Iterator.init(&node[0]);

    try testing.expectEqual(&node[0], it.next().?);
    try testing.expectEqual(&node[1], it.next().?);
    try testing.expectEqual(&node[2], it.next().?);
    try testing.expectEqual(null, it.next());

    node[0] = .{ .next = &node[0], .prev = &node[0] };

    it = Iterator.init(&node[0]);

    try testing.expectEqual(&node[0], it.next().?);
    try testing.expectEqual(null, it.next());
}

test "circular:double:backward_iterator" {
    const Iterator = CircularLinkIterator(CDN, .prev);

    var node = [_]CDN{
        undefined,
        undefined,
        undefined,
    };
    errdefer {
        std.log.err(
            \\
            \\node[0]: {*}
            \\node[1]: {*}
            \\node[2]: {*}
        ,
            .{ &node[0], &node[1], &node[2] },
        );
    }

    inline for (&node) |*n| n.* = .{ .prev = n, .next = n };

    node[0].linkNext(&node[1]);
    node[1].linkNext(&node[2]);

    var it = Iterator.init(&node[2]);

    try testing.expectEqual(&node[2], it.next().?);
    try testing.expectEqual(&node[1], it.next().?);
    try testing.expectEqual(&node[0], it.next().?);
    try testing.expectEqual(null, it.next());

    node[0] = .{ .next = &node[0], .prev = &node[0] };

    it = Iterator.init(&node[0]);

    try testing.expectEqual(&node[0], it.next().?);
    try testing.expectEqual(null, it.next());
}
