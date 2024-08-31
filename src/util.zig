const std = @import("std");

pub const bool_enum = @import("bool_enum.zig");
pub const pack = @import("pack.zig");
pub const slice = @import("slice.zig");
pub const meta = @import("meta.zig");
pub const list = @import("list.zig");
pub const NullAllocator = @import("NullAllocator.zig");

pub const null_allocator = std.mem.Allocator{
    .ptr = undefined,
    .vtable = &NullAllocator.vtable,
};

pub const GenericRangeOptions = struct {
    layout: std.builtin.Type.ContainerLayout = .auto,
    start: Bound = .{},
    end: Bound = .{},

    const Bound = struct {
        default: union(enum) {
            none,
            value: ?comptime_int,
        } = .none,
        nullable: ?bool = null,
    };
};

pub fn GenericRange(comptime T: type, comptime options: GenericRangeOptions) type {
    const start_nullable = options.start.nullable orelse switch (options.start.default) {
        .value => |v| v == null,
        .none => false,
    };
    const Start = if (start_nullable) ?T else T;

    const end_nullable = options.end.nullable orelse switch (options.end.default) {
        .value => |v| v == null,
        .none => false,
    };
    const End = if (end_nullable) ?T else T;

    if (options.layout != .auto and (start_nullable or end_nullable)) {
        @compileError("optionals cannot be used in '" ++ @tagName(options.layout) ++ "' structs");
    }
    return @Type(.{
        .Struct = .{
            .layout = options.layout,
            .backing_integer = if (options.layout == .@"packed")
                std.meta.Int(.unsigned, @bitSizeOf(T) * 2)
            else
                null,
            .is_tuple = false,
            .decls = &.{},
            .fields = &.{
                .{
                    .name = "start",
                    .type = Start,
                    .alignment = @alignOf(Start),
                    .is_comptime = false,
                    .default_value = switch (options.start.default) {
                        .none => null,
                        .value => |val| blk: {
                            const default: Start = val orelse null;
                            break :blk &default;
                        },
                    },
                },
                .{
                    .name = "end",
                    .type = End,
                    .alignment = @alignOf(End),
                    .is_comptime = false,
                    .default_value = switch (options.end.default) {
                        .none => null,
                        .value => |val| blk: {
                            const default: End = val orelse null;
                            break :blk &default;
                        },
                    },
                },
            },
        },
    });
}

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
