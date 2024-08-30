pub const NodeInfo = struct {
    kind: enum { single, double },
    link_type: LinkType,
};

pub fn Link(comptime TNode: type) type {
    return LinkEx(TNode, .{});
}

pub fn LinkEx(comptime T: type, comptime links: NodeLinks) type {
    const info = getNodeInfo(T) orelse {
        @compileError("'" ++ @typeName(T) ++ "' is not a list node");
    };
    const Next = std.meta.FieldType(T, links.next);
    switch (info.kind) {
        .single => return Next,
        .double => {
            if (links.next == links.prev) @compileError("Next and previous " ++
                "link must be stored in different fields. 'next' and 'prev' " ++
                "of '" ++ @typeName(T) ++ "' are both in field '" ++
                @tagName(links.next) ++ "'");
            const Prev = std.meta.FieldType(T, links.prev);
            if (Next == Prev) return Next;
            return @TypeOf(@as(Next, undefined), @as(Prev, undefined));
        },
    }
}

pub fn isListNode(comptime T: type, comptime links: NodeLinks) bool {
    return getNodeInfo(T, links) != null;
}

pub fn isSingleNode(comptime TNode: type, comptime links: NodeLinks) bool {
    const node_links = getNodeInfo(TNode, links) orelse return false;
    return node_links.kind == .single;
}

pub fn isDoubleNode(comptime TNode: type, links: NodeLinks) bool {
    const node_links = getNodeInfo(TNode, links) orelse return false;
    return node_links.kind == .double;
}

pub const NodeLinks = struct {
    next: @Type(.EnumLiteral) = .next,
    prev: @Type(.EnumLiteral) = .prev,
};

pub fn getNodeInfo(comptime T: type, comptime links: NodeLinks) ?NodeInfo {
    comptime {
        if (@typeInfo(T) != .Struct) return null;
        if (!@hasField(T, @tagName(links.next))) return null;
        const Next = std.meta.FieldType(T, links.next);
        var info: NodeInfo = .{ .kind = .single };
        info.link_type = getLinkType(T, Next) orelse return null;
        if (@hasField(T, @tagName(links.prev))) {
            const Prev = std.meta.FieldType(T, links.prev);
            if (Prev == Next) {
                info.kind = .double;
            } else if (getLinkType(T, Prev)) |link_type| {
                if (info.link_type == link_type) info.kind = .double;
            }
        }
        return info;
    }
}

pub fn isNodeLinkType(comptime NodeObject: type, comptime TLink: type) bool {
    return getLinkType(NodeObject, TLink) != null;
}

pub const LinkType = enum { optional, required };

/// Gets the type of list link a link of type `MaybeLink` would be for a node of
/// type `T`.
///
/// List links may either be optional or required, the distinction is made base
/// on if `MaybeLink` is optional or not.
///
/// Returns `null` unless `MaybeLink` is a (possibly optional) single item
/// pointer to `T`.
pub fn getLinkType(comptime T: type, comptime MaybeLink: type) ?LinkType {
    switch (@typeInfo(MaybeLink)) {
        .Pointer => |ptr_info| return if (ptr_info.size == .One and ptr_info.child == T)
            .required
        else
            null,
        .Optional => |opt_info| switch (@typeInfo(opt_info.child)) {
            .Pointer => |ptr_info| return if (ptr_info.size == .One and ptr_info.child == T)
                .optional
            else
                null,
            else => return null,
        },
        else => return null,
    }
}

pub fn linkTypeOf(comptime T: type, comptime link_field: std.meta.FieldEnum(T)) ?LinkType {
    return getLinkType(T, std.meta.FieldType(T, link_field));
}

const std = @import("std");
const util = @import("utilz");
