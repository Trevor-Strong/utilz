const std = @import("std");
const builtin = @import("builtin");

/// Returns an error recognized by the Zig test runner as a "skip this test"
/// code.
pub fn skip() error{SkipZigTest} {
    return error.SkipZigTest;
}

/// Prints the format message. Works at comptime as well as runtime by turning
/// into a `@compileLog()` at comptime.
pub fn print(comptime format: []const u8, args: anytype) void {
    if (@inComptime()) {
        @compileLog(std.fmt.comptimePrint(format, args));
    } else if (std.testing.backend_can_print) {
        std.debug.print(format, args);
    }
}
