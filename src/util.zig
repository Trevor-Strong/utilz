const std = @import("std");

pub const pack = @import("pack.zig");
pub const slice = @import("slice.zig");
pub const meta = @import("meta.zig");
pub const list = @import("list.zig");
pub const NullAllocator = @import("NullAllocator.zig");

pub const null_allocator = std.mem.Allocator{
    .ptr = undefined,
    .vtable = &NullAllocator.vtable,
};

/// Returns `"Expected " ++ message ++ "found '" ++ @typeName(T) ++ "'"`.
///
/// Caller must introduce their own separator between `message` and the rest of
/// the message.
pub fn expectedTypeMsg(comptime message: []const u8, comptime T: type) [:0]const u8 {
    return "Expected " ++ message ++ "found '" ++ @typeName(T) ++ "'";
}

pub fn expected(comptime message: []const u8) ExpectedMsgBuilder {
    return .{ .message = "Expected " ++ message };
}

pub const ExpectedMsgBuilder = struct {
    message: []const u8,

    pub fn ofType(comptime builder: ExpectedMsgBuilder, comptime T: type) ExpectedMsgBuilder {
        const sep_part = sep_part: {
            var it = std.mem.reverseIterator(builder.message);
            while (it.next()) |c| {
                if (std.ascii.isAlphanumeric(c)) break;
            }
            break :sep_part builder.message[it.index + 1 ..];
        };
        const msg = builder.message;
        // msg[0..msg.len - sep_part.len] ++ " '" ++ @typeName(T) ++ "'" ++ sep_part,
        return .{
            .message = p("{s} of '{}'{s}", .{
                msg[0 .. msg.len - sep_part.len],
                T,
                sep_part,
            }),
        };
    }

    pub fn addType(comptime builder: ExpectedMsgBuilder, comptime T: type) ExpectedMsgBuilder {
        return builder.append("'" ++ @typeName(T) ++ "'");
    }

    pub fn append(comptime builder: ExpectedMsgBuilder, comptime text: []const u8) ExpectedMsgBuilder {
        return .{
            .message = builder.message ++ text,
        };
    }

    pub fn foundType(comptime builder: ExpectedMsgBuilder, comptime T: type) []const u8 {
        return builder.append("found ").addType(T).message;
    }
};

/// Alias of `std.fmt.comptimePrint`
pub const p = std.fmt.comptimePrint;

/// Determines if `T` is a Zero Size Type (ZST) that can represent multiple
/// values (`undefined` isn't a valid value of `T`). All of these types are also
/// `comptime` only types.
pub fn isMultiValueZst(comptime T: type) bool {
    if (@sizeOf(T) != 0) return false; // Not ZST
    return switch (@typeInfo(T)) {
        .Fn,
        .Type,
        .ComptimeInt,
        .ComptimeFloat,
        .EnumLiteral,
        => true,
        inline .Vector, .Array => |info| info.len != 0 and isMultiValueZst(info.child),
        .Struct => |s_info| for (s_info.fields) |f| {
            if (!f.is_comptime and isMultiValueZst(f.type)) break true;
        } else false,
        .Union => |u_info| for (u_info.fields) |f| {
            if (isMultiValueZst(f.type)) break true;
        } else false,
        .ErrorUnion => |eu_info| isMultiValueZst(eu_info.payload),
        else => false,
    };
}

const testing = std.testing;

test {
    testing.refAllDecls(@This());
}

test ExpectedMsgBuilder {
    const expectEqlStrings = testing.expectEqualStrings;
    const X = struct {};
    try expectEqlStrings(
        "Expected <A String>found '" ++ @typeName(X) ++ "'",
        comptime expected("<A String>").foundType(X),
    );

    try expectEqlStrings(
        "Expected slice of '" ++ @typeName(u8) ++ "', found '" ++ @typeName(*anyopaque) ++ "'",
        comptime expected("slice, ").ofType(u8).foundType(*anyopaque),
    );

    const A = struct {};
    const B = struct {};
    const C = struct {};
    try expectEqlStrings(
        p("'{}''{}''{}'", .{ A, B, C }),
        comptime ExpectedMsgBuilder.addType(.{ .message = "" }, A)
            .addType(B)
            .addType(C)
            .message,
    );
}
