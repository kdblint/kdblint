const std = @import("std");
const assert = std.debug.assert;
const builtin = @import("builtin");
const zls = @import("zls");
const tracy = @import("tracy");
const build_options = @import("build_options");

const Server = @import("Server.zig");

const log = std.log.scoped(.kdblint);

const usage =
    \\kdbLint - A kdb+ static code analysis tool and language server
    \\
    \\Commands:
    \\  help, --help              Print this help message
    \\  version, --version        Print version number
    \\
    \\General Options:
    \\  --config-file [path]      Set path to the configuration file
    \\  --enable-message-tracing  Enable message tracing
    \\  --log-file [path]         Set path to the log file
    \\  --log-level [enum]        The log level to be used.
    \\                              Supported values:
    \\                                err
    \\                                warn
    \\                                info (default)
    \\                                debug
    \\
;

pub const std_options: std.Options = .{
    .log_level = .debug,
    .logFn = logFn,
};

var runtime_log_level: std.log.Level = if (builtin.mode == .Debug) .debug else .info;

fn logFn(
    comptime level: std.log.Level,
    comptime scope: @TypeOf(.enum_literal),
    comptime format: []const u8,
    args: anytype,
) void {
    if (@intFromEnum(level) > @intFromEnum(runtime_log_level)) return;

    const level_txt = comptime level.asText();
    const scope_txt = @tagName(scope);

    blk: {
        std.debug.lockStdErr();
        defer std.debug.unlockStdErr();

        const writer = std.io.getStdErr().writer();

        const header = std.fmt.comptimePrint("{s:<7} ({s}): ", .{
            level_txt,
            comptime if (std.mem.startsWith(u8, scope_txt, "kdblint_")) scope_txt[8..] else scope_txt,
        });
        writer.writeAll(header) catch break :blk;
        writer.print(format, args) catch break :blk;
        writer.writeByte('\n') catch break :blk;
    }
}

const ParseArgsResut = struct {
    config_path: ?[]const u8 = null,
    enable_message_tracing: bool = false,
    log_level: ?std.log.Level = null,
    log_file_path: ?[]const u8 = null,
    kdblint_exe_path: []const u8 = "",

    fn deinit(self: ParseArgsResut, allocator: std.mem.Allocator) void {
        defer if (self.config_path) |path| allocator.free(path);
        defer if (self.log_file_path) |path| allocator.free(path);
        defer allocator.free(self.kdblint_exe_path);
    }
};

const ParseArgsError = std.process.ArgIterator.InitError || std.mem.Allocator.Error || std.fs.File.WriteError;

fn parseArgs(allocator: std.mem.Allocator) ParseArgsError!ParseArgsResut {
    var result: ParseArgsResut = .{};
    errdefer result.deinit(allocator);

    const stdout = std.io.getStdOut().writer();

    var args_it = try std.process.ArgIterator.initWithAllocator(allocator);
    defer args_it.deinit();

    const kdblint_exe_path = args_it.next() orelse "";
    result.kdblint_exe_path = try allocator.dupe(u8, kdblint_exe_path);

    var arg_index: usize = 0;
    while (args_it.next()) |arg| : (arg_index += 1) {
        if (arg_index == 0) {
            if (std.mem.eql(u8, arg, "help") or std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "--help")) { // help
                try std.io.getStdErr().writeAll(usage);
                std.process.exit(0);
            } else if (std.mem.eql(u8, arg, "version") or std.mem.eql(u8, arg, "-v") or std.mem.eql(u8, arg, "--version")) { // version
                try stdout.writeAll(build_options.version ++ "\n");
                std.process.exit(0);
            }
        }

        if (std.mem.eql(u8, arg, "--config-file")) { // --config-file
            const path = args_it.next() orelse {
                log.err("Expected configuration file path after --config-file argument.", .{});
                std.process.exit(1);
            };
            if (result.config_path) |old_path| allocator.free(old_path);
            result.config_path = try allocator.dupe(u8, path);
        } else if (std.mem.eql(u8, arg, "--enable-message-tracing")) { // --enable-message-tracing
            result.enable_message_tracing = true;
        } else if (std.mem.eql(u8, arg, "--log-file")) { // --log-file
            const path = args_it.next() orelse {
                log.err("Expected log file path after --log-file argument.", .{});
                std.process.exit(1);
            };
            if (result.log_file_path) |old_path| allocator.free(old_path);
            result.log_file_path = path;
        } else if (std.mem.eql(u8, arg, "--log-level")) { // --log-level
            const log_level_name = args_it.next() orelse {
                log.err("Expected log level after --log-level argument.", .{});
                std.process.exit(1);
            };
            result.log_level = std.meta.stringToEnum(std.log.Level, log_level_name) orelse {
                log.err("Invalid --log-level argument. Expected one of {{'debug', 'info', 'warn', 'err'}} but found '{s}'", .{log_level_name});
                std.process.exit(1);
            };
        }
    }

    if (std.io.getStdIn().isTty()) {
        log.warn("kdbLint is not a CLI tool, it communicates over the Language Server Protocol.", .{});
    }

    return result;
}

const stack_frames = switch (builtin.mode) {
    .Debug => 10,
    else => 0,
};

pub fn main() !u8 {
    var allocator_state = std.heap.GeneralPurposeAllocator(.{
        .stack_trace_frames = stack_frames,
    }){};
    defer assert(allocator_state.deinit() == .ok);

    var tracy_state = if (tracy.enable_allocation) tracy.tracyAllocator(allocator_state.allocator()) else void{};
    const inner_allocator = if (tracy.enable_allocation) tracy_state.allocator() else allocator_state.allocator();

    const allocator = inner_allocator;

    const result = try parseArgs(allocator);
    defer result.deinit(allocator);

    const resolved_log_level = result.log_level orelse runtime_log_level;

    log.info("Starting kdbLint {s} @ '{s}'", .{ build_options.version, result.kdblint_exe_path });
    log.info("Message tracing: {}", .{result.enable_message_tracing});
    log.info("Log level:       {s}", .{@tagName(resolved_log_level)});

    runtime_log_level = resolved_log_level;

    var transport: zls.lsp.ThreadSafeTransport(.{
        .ChildTransport = zls.lsp.TransportOverStdio,
        .thread_safe_read = false,
        .thread_safe_write = true,
    }) = .{ .child_transport = zls.lsp.TransportOverStdio.init(std.io.getStdIn(), std.io.getStdOut()) };

    const server = try Server.create(allocator);
    defer server.destroy();
    server.transport = transport.any();
    server.config_path = result.config_path;
    server.message_tracing = result.enable_message_tracing;

    try server.loop();

    switch (server.status) {
        .exiting_failure => return 1,
        .exiting_success => return 0,
        else => unreachable,
    }
}

test {
    @import("std").testing.refAllDecls(@This());
}
