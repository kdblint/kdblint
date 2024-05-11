const std = @import("std");
const builtin = @import("builtin");
const zls = @import("zls");
const types = zls.types;
const tracy = @import("tracy");
const DocumentStore = @import("DocumentStore.zig");
const Server = @import("Server.zig");
const diagnostics = @import("features/diagnostics.zig");

const log = std.log.scoped(.kdblint);

const ParseArgsResut = struct {
    action: enum { proceed, exit },
    config_path: ?[]const u8,
    message_tracing_enabled: bool,

    kdblint_exe_path: []const u8,

    fn deinit(self: ParseArgsResut, allocator: std.mem.Allocator) void {
        defer if (self.config_path) |path| allocator.free(path);
        defer allocator.free(self.kdblint_exe_path);
    }
};

fn parseArgs(allocator: std.mem.Allocator) !ParseArgsResut {
    var result = ParseArgsResut{
        .action = .exit,
        .config_path = null,
        .message_tracing_enabled = false,
        .kdblint_exe_path = "",
    };
    errdefer result.deinit(allocator);

    result.action = .proceed;

    return result;
}

pub fn main() !void {
    var allocator_state = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(allocator_state.deinit() == .ok);

    var tracy_state = if (tracy.enable_allocation) tracy.tracyAllocator(allocator_state.allocator()) else void{};
    const inner_allocator = if (tracy.enable_allocation) tracy_state.allocator() else allocator_state.allocator();

    const enable_failing_allocator = false;
    var failing_allocator_state = if (enable_failing_allocator) zls.debug.FailingAllocator.init(inner_allocator, 256) else void{};
    const allocator: std.mem.Allocator = if (enable_failing_allocator) failing_allocator_state.allocator() else inner_allocator;

    const result = try parseArgs(allocator);
    defer result.deinit(allocator);
    switch (result.action) {
        .proceed => {},
        .exit => return,
    }

    const message_tracing_enabled = false;
    var transport = zls.Transport.init(
        std.io.getStdIn().reader(),
        std.io.getStdOut().writer(),
    );
    transport.message_tracing = message_tracing_enabled;

    const server = try Server.create(allocator);
    defer server.destroy();
    server.transport = &transport;
    server.config_path = result.config_path;

    try server.loop();

    if (server.status == .exiting_failure) {
        if (builtin.mode == .Debug) {
            return;
        } else {
            std.process.exit(1);
        }
    }
}

test {
    @import("std").testing.refAllDecls(@This());
}
