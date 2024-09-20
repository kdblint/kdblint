const std = @import("std");
const mem = std.mem;
const process = std.process;
const Allocator = std.mem.Allocator;
const zls = @import("zls");

const fatal = @import("main.zig").fatal;
const Server = @import("lsp/Server.zig");

const usage =
    \\Usage: kdblint lsp
    \\
    \\  Runs the language server.
    \\
    \\Options:
    \\  -h, --help                  Print this help and exit
    \\  --enable-message-tracing    Enable message tracing
    \\
;

pub fn main(gpa: Allocator, args: *std.process.ArgIterator) !void {
    var enable_message_tracing: bool = false;

    while (args.next()) |arg| {
        if (mem.eql(u8, arg, "-h") or mem.eql(u8, arg, "--help")) {
            try std.io.getStdOut().writeAll(usage);
            return process.cleanExit();
        } else if (std.mem.eql(u8, arg, "--enable-message-tracing")) {
            enable_message_tracing = true;
        } else {
            fatal("unrecognized parameter: '{s}'", .{arg});
        }
    }

    if (std.io.getStdIn().isTty()) {
        std.log.warn("Running kdblint in LSP mode is not a CLI tool, it communicates over the Language Server Protocol.", .{});
    }

    var transport: zls.lsp.ThreadSafeTransport(.{
        .ChildTransport = zls.lsp.TransportOverStdio,
        .thread_safe_read = false,
        .thread_safe_write = true,
    }) = .{ .child_transport = .init(std.io.getStdIn(), std.io.getStdOut()) };

    const server = try Server.create(gpa);
    defer server.destroy();
    server.transport = transport.any();
    server.message_tracing = enable_message_tracing;

    try server.loop();

    switch (server.status) {
        .exiting_failure => process.exit(1),
        .exiting_success => return process.cleanExit(),
        else => unreachable,
    }
}

test {
    @import("std").testing.refAllDecls(@This());
}
