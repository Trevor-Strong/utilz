const std = @import("std");
const assert = std.debug.assert;
const LinkKind = enum { required, optional };
fn linkKind(comptime Node: type, comptime T: type) ?LinkKind {
    return switch (T) {
        ?*Node => .optional,
        *Node => .required,
        else => null,
    };
}

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

        const Link = if (optional_links) ?*Node else *Node;

        /// Inserts `new_next` after `self` in the list
        pub fn linkAfter(noalias self: *Node, noalias new_next: *Node) void {
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
        /// If a doubly linked node, unlinks `self` from the list, otherwise
        /// this is an instance of `void`.
        pub const unlink = if (doubly_linked) unlinkDouble else {};

        fn unlinkDouble(self: *Node) void {
            if (!doubly_linked) {
                @compileError("Cannot unlink singly linked node without previous node reference");
            }
            const prev = getPrev(self);
            const next = getNext(self);
            if (optional_links) {
                setNext(self, null);
                setPrev(self, null);
            }
            if (optional_links) {
                if (prev != null) setNext(prev.?, next);
                if (next != null) setPrev(next.?, prev);
            } else {
                setNext(prev, next);
                setPrev(next, prev);
            }
        }

        pub fn unlinkNext(self: *Node) void {
            if (doubly_linked) {
                const next = if (optional_links) next: {
                    break :next getNext(self) orelse return;
                } else getNext(self);
                unlinkDouble(next);
            } else {
                const to_unlink = if (!optional_links)
                    getNext(self)
                else
                    getNext(self) orelse return;
                const next = getNext(to_unlink);
                setNext(to_unlink, null);
                setNext(self, next);
            }
        }

        inline fn getNext(n: Node) Link {
            return @field(n, @tagName(next_field));
        }

        inline fn setNext(n: *Node, new_next: Link) void {
            @field(n, @tagName(next_field)) = new_next;
        }

        inline fn getPrev(n: Node) Link {
            return @field(n, @tagName(prev_field.?));
        }

        inline fn setPrev(n: *Node, new_next: Link) void {
            @field(n, @tagName(prev_field.?)) = new_next;
        }
    };
}

pub fn LinkIterator(
    comptime Node: type,
    comptime link_field: std.meta.FieldEnum(Node),
) type {
    return struct {
        const Self = @This();

        pub const Item = *Node;

        cur: ?*Node = null,

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
    return struct {
        const Self = @This();

        pub const Item = *Node;

        cur: *Node,
        first: *Node,
        iterated: bool = false,

        pub fn init(start: *Node) Self {
            return .{
                .cur = start,
                .first = start,
                .iterated = false,
            };
        }

        pub fn next(it: *Self) ?Item {
            const n = it.cur;

            const next_node = @field(n, link_field);
            if (next_node == it.first) {
                if (it.iterated) return null;
            }
            it.iterated = true;
            it.cur = next_node;
            return n;
        }

        pub fn reset(it: *Self) void {
            it.cur = it.first;
            it.iterated = false;
        }
    };
}
