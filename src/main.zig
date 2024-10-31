const std = @import("std");
const builtin = @import("builtin");
const io = std.io;
const mem = std.mem;
const Allocator = mem.Allocator;
const process = std.process;
const assert = std.debug.assert;

const kdb = @import("kdb");
const build_options = @import("build_options");

pub const std_options: std.Options = .{
    .log_level = if (builtin.mode == .Debug) .debug else .info,
};

pub fn fatal(comptime format: []const u8, args: anytype) noreturn {
    std.log.err(format, args);
    process.exit(1);
}

const usage =
    \\Usage: kdblint [command] [options]
    \\
    \\Commands:
    \\
    \\  lsp         Run language server
    // \\  ast-check   Look for simple compile errors in any set of files
    \\  fmt         Reformat kdb+ source into canonical form
    \\
    \\  env         Print lib path, std path, cache directory, and version
    \\  help        Print this help and exit
    \\  version     Print version number and exit
    \\
    \\Options:
    \\
    \\  -h, --help  Print command-specific usage
    \\
;

var general_purpose_allocator: std.heap.GeneralPurposeAllocator(.{
    .stack_trace_frames = switch (builtin.mode) {
        .Debug => 10,
        else => 0,
    },
}) = .{};

pub fn main() !void {
    const gpa = general_purpose_allocator.allocator();
    defer _ = general_purpose_allocator.deinit();
    var arena_instance: std.heap.ArenaAllocator = .init(gpa);
    defer arena_instance.deinit();
    const arena = arena_instance.allocator();

    var args = try process.argsWithAllocator(arena);

    return mainArgs(gpa, &args);
}

fn mainArgs(gpa: Allocator, args: *process.ArgIterator) !void {
    assert(args.skip());
    const cmd = args.next() orelse {
        try io.getStdOut().writeAll(usage ++ "\n");
        fatal("expected command argument", .{});
    };

    if (mem.eql(u8, cmd, "lsp")) {
        return @import("lsp.zig").main(gpa, args);
        // } else if (mem.eql(u8, cmd, "ast-check")) {
        // return @import("ast_check.zig").main(gpa, arena, args);
    } else if (mem.eql(u8, cmd, "fmt")) {
        return kdb.fmt(gpa, args);
    } else if (mem.eql(u8, cmd, "version")) {
        return io.getStdOut().writeAll(build_options.version ++ "\n");
    } else if (mem.eql(u8, cmd, "help") or mem.eql(u8, cmd, "-h") or mem.eql(u8, cmd, "--help")) {
        return io.getStdOut().writeAll(usage);
    } else {
        try io.getStdOut().writeAll(usage ++ "\n");
        fatal("unknown command: {s}", .{cmd});
    }
}

test {
    @import("std").testing.refAllDecls(@This());
}
