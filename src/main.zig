const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const zls = @import("zls");
const Color = std.zig.Color;
const known_folders = @import("known_folders");

const kdb = @import("kdb/root.zig");
const DocumentScope = kdb.DocumentScope;
const build_options = @import("build_options");

const Server = @import("lsp/Server.zig");

const log = std.log.scoped(.main);

const usage =
    \\Usage: kdblint [command] [options]
    \\
    \\Commands:
    \\
    \\  lsp         Run language server
    \\  ast-check   Look for simple compile errors in any set of files
    \\  fmt         Reformat kdb+ source into canonical form
    \\
    \\  help        Print this help and exit
    \\  version     Print version number and exit
    \\
    \\Options:
    \\
    \\  -h, --help  Print command-specific usage
    \\
;

pub const std_options: std.Options = .{
    .wasiCwd = wasi_cwd,
    // Always set this to debug to make std.log call into our handler, then control the runtime
    // value in logFn itself
    .log_level = .debug,
    .logFn = logFn,
};

var wasi_preopens: std.fs.wasi.Preopens = undefined;
pub fn wasi_cwd() std.os.wasi.fd_t {
    // Expect the first preopen to be current working directory.
    const cwd_fd: std.posix.fd_t = 3;
    assert(std.mem.eql(u8, wasi_preopens.names[cwd_fd], "."));
    return cwd_fd;
}

/// Log messages with the LSP 'window/logMessage' message.
var log_transport: ?*zls.lsp.Transport = null;
/// Log messages to stderr.
var log_stderr: bool = true;
/// Log messages to the given file.
var log_file: ?std.fs.File = null;
var log_level: std.log.Level = if (builtin.mode == .Debug) .debug else .info;

fn logFn(
    comptime level: std.log.Level,
    comptime scope: @Type(.enum_literal),
    comptime format: []const u8,
    args: anytype,
) void {
    var buffer: [4096]u8 = undefined;
    comptime std.debug.assert(buffer.len >= zls.lsp.minimum_logging_buffer_size);

    if (log_transport) |transport| {
        const lsp_message_type: zls.lsp.types.MessageType = switch (level) {
            .err => .Error,
            .warn => .Warning,
            .info => .Info,
            .debug => .Debug,
        };
        const json_message = zls.lsp.bufPrintLogMessage(&buffer, lsp_message_type, format, args);
        transport.writeJsonMessage(json_message) catch {};
    }

    if (@intFromEnum(level) > @intFromEnum(log_level)) return;
    if (!log_stderr and log_file == null) return;

    const level_txt: []const u8 = switch (level) {
        .err => "error",
        .warn => "warn ",
        .info => "info ",
        .debug => "debug",
    };
    const scope_txt: []const u8 = comptime @tagName(scope);

    var writer: std.Io.Writer = .fixed(&buffer);
    const no_space_left = blk: {
        writer.print("{s} ({s:^6}): ", .{ level_txt, scope_txt }) catch break :blk true;
        writer.print(format, args) catch break :blk true;
        writer.writeByte('\n') catch break :blk true;
        break :blk false;
    };
    if (no_space_left) {
        const trailing = "...\n".*;
        writer.undo(trailing.len -| writer.unusedCapacityLen());
        (writer.writableArray(trailing.len) catch unreachable).* = trailing;
    }

    std.debug.lockStdErr();
    defer std.debug.unlockStdErr();

    if (log_stderr) {
        var stderr_writer = std.fs.File.stderr().writer(&.{});
        stderr_writer.interface.writeAll(writer.buffered()) catch {};
    }

    if (log_file) |file| {
        var log_writer = file.writerStreaming(&.{});
        file.seekFromEnd(0) catch {};
        log_writer.interface.writeAll(writer.buffered()) catch {};
    }
}

fn defaultLogFilePath(allocator: std.mem.Allocator) std.mem.Allocator.Error!?[]const u8 {
    if (builtin.target.os.tag == .wasi) return null;
    const cache_path = known_folders.getPath(allocator, .cache) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.ParseError => return null,
    } orelse return null;
    defer allocator.free(cache_path);
    return try std.fs.path.join(allocator, &.{ cache_path, "kdblint", "kdblint.log" });
}

fn createLogFile(allocator: std.mem.Allocator, override_log_file_path: ?[]const u8) ?struct { std.fs.File, []const u8 } {
    const log_file_path = if (override_log_file_path) |log_file_path|
        allocator.dupe(u8, log_file_path) catch return null
    else
        defaultLogFilePath(allocator) catch null orelse return null;
    errdefer allocator.free(log_file_path);

    if (std.fs.path.dirname(log_file_path)) |dirname| {
        std.fs.cwd().makePath(dirname) catch {};
    }

    const file = std.fs.cwd().createFile(log_file_path, .{ .truncate = false }) catch {
        allocator.free(log_file_path);
        return null;
    };
    errdefer file.close();

    return .{ file, log_file_path };
}

const fatal = std.process.fatal;
const cleanExit = std.process.cleanExit;

/// This can be global since stdin is a singleton.
var stdin_buffer: [4096]u8 align(std.heap.page_size_min) = undefined;
/// This can be global since stdout is a singleton.
var stdout_buffer: [4096]u8 align(std.heap.page_size_min) = undefined;

var debug_allocator: std.heap.DebugAllocator(.{}) = .init;

pub fn main() !void {
    const gpa, const is_debug = gpa: {
        if (builtin.os.tag == .wasi) break :gpa .{ std.heap.wasm_allocator, false };
        break :gpa switch (builtin.mode) {
            .Debug, .ReleaseSafe => .{ debug_allocator.allocator(), true },
            .ReleaseFast, .ReleaseSmall => .{ std.heap.smp_allocator, false },
        };
    };
    defer if (is_debug) {
        _ = debug_allocator.deinit();
    };
    var arena_instance: std.heap.ArenaAllocator = .init(gpa);
    defer arena_instance.deinit();
    const arena = arena_instance.allocator();

    const args = try std.process.argsAlloc(arena);

    if (builtin.os.tag == .wasi) {
        wasi_preopens = try std.fs.wasi.preopensAlloc(arena);
    }

    return mainArgs(gpa, arena, args);
}

fn mainArgs(gpa: Allocator, arena: Allocator, args: []const []const u8) !void {
    if (args.len <= 1) {
        log.info("{s}", .{usage});
        fatal("expected command argument", .{});
    }

    const cmd = args[1];
    const cmd_args = args[2..];
    if (std.mem.eql(u8, cmd, "lsp")) {
        return cmdLsp(gpa, args[0], cmd_args);
    } else if (std.mem.eql(u8, cmd, "ast-check")) {
        return cmdAstCheck(gpa, arena, cmd_args);
    } else if (std.mem.eql(u8, cmd, "fmt")) {
        return @import("fmt.zig").run(gpa, arena, cmd_args);
    } else if (std.mem.eql(u8, cmd, "version")) {
        return std.fs.File.stdout().writeAll(build_options.version_string ++ "\n");
    } else if (std.mem.eql(u8, cmd, "help") or std.mem.eql(u8, cmd, "-h") or std.mem.eql(u8, cmd, "--help")) {
        return std.fs.File.stdout().writeAll(usage);
    } else {
        try std.fs.File.stdout().writeAll(usage ++ "\n");
        fatal("unknown command: {s}", .{cmd});
    }
}

test {
    @import("std").testing.refAllDecls(@This());
}

const usage_ast_check =
    \\Usage: kdblint ast-check [file]
    \\
    \\    Given a .k/.q source file, reports any compile errors that can be
    \\    ascertained on the basis of the source code alone.
    \\
    \\    If [file] is omitted, stdin is used.
    \\
    \\Options:
    \\
    \\  -h, --help            Print this help and exit
    \\  --color [auto|off|on] Enable or disable colored error messages
    \\  -t                    Output IR in text form to stdout
    \\
;

fn cmdAstCheck(gpa: Allocator, arena: Allocator, args: []const []const u8) !void {
    var color: Color = .auto;
    var want_output_text = false;
    var kdb_source_path: ?[]const u8 = null;

    var i: usize = 0;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (std.mem.startsWith(u8, arg, "-")) {
            if (std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "--help")) {
                try std.fs.File.stdout().writeAll(usage_ast_check);
                return std.process.cleanExit();
            } else if (std.mem.eql(u8, arg, "-t")) {
                want_output_text = true;
            } else if (std.mem.eql(u8, arg, "--color")) {
                if (i + 1 >= args.len) {
                    fatal("expected [auto|on|off] after --color", .{});
                }
                i += 1;
                const next_arg = args[i];
                color = std.meta.stringToEnum(Color, next_arg) orelse {
                    fatal("expected [auto|on|off] after --color, found '{s}'", .{next_arg});
                };
            } else {
                fatal("unrecognized parameter: '{s}'", .{arg});
            }
        } else if (kdb_source_path == null) {
            kdb_source_path = arg;
        } else {
            fatal("extra positional parameter: '{s}'", .{arg});
        }
    }

    const display_path = kdb_source_path orelse "<stdin>";
    const source = s: {
        var f = if (kdb_source_path) |p| file: {
            break :file std.fs.cwd().openFile(p, .{}) catch |err| {
                fatal("unable to open file '{s}' for ast-check: {s}", .{ display_path, @errorName(err) });
            };
        } else std.fs.File.stdin();
        defer if (kdb_source_path != null) f.close();
        var file_reader = f.reader(&stdin_buffer);
        break :s std.zig.readSourceFileToEndAlloc(arena, &file_reader) catch |err| {
            fatal("unable to load file '{s}' for ast-check: {s}", .{ display_path, @errorName(err) });
        };
    };

    const tree = try kdb.Ast.parse(arena, source, .{
        .mode = if (std.mem.endsWith(u8, display_path, ".k")) .k else .q,
        .version = .@"4.0",
    });

    var stdout_writer = std.fs.File.stdout().writerStreaming(&stdout_buffer);
    const stdout_bw = &stdout_writer.interface;

    var document_scope: DocumentScope = .{};
    defer document_scope.deinit(gpa);
    var context: DocumentScope.ScopeContext = .{
        .gpa = gpa,
        .tree = tree,
        .doc_scope = &document_scope,
    };
    const zir = try kdb.AstGen.generate(arena, &context);

    if (zir.hasCompileErrors()) {
        try kdb.printZirErrorsToStderr(gpa, tree, zir, display_path, color);
        std.process.exit(1);
    } else if (zir.hasCompileWarnings()) {
        try kdb.printZirErrorsToStderr(gpa, tree, zir, display_path, color);
    }

    if (!want_output_text) {
        return std.process.cleanExit();
    }

    const token_bytes = @sizeOf(kdb.Ast.TokenList) +
        tree.tokens.len * (@sizeOf(kdb.Token.Tag) + @sizeOf(kdb.Ast.ByteOffset));
    const tree_bytes = @sizeOf(kdb.Ast) + tree.nodes.len *
        (@sizeOf(kdb.Ast.Node.Tag) +
            @sizeOf(kdb.Ast.Node.Data) +
            @sizeOf(kdb.Ast.TokenIndex));
    const instruction_bytes = zir.instructions.len *
        // Here we don't use @sizeOf(Zir.Inst.Data) because it would include
        // the debug safety tag but we want to measure release size.
        (@sizeOf(kdb.Zir.Inst.Tag) + 8);
    const extra_bytes = zir.extra.len * @sizeOf(u32);
    const total_bytes = @sizeOf(kdb.Zir) + instruction_bytes + extra_bytes +
        zir.string_bytes.len * @sizeOf(u8);
    // zig fmt: off
    try stdout_bw.print(
        \\# Source bytes:       {Bi}
        \\# Tokens:             {} ({Bi})
        \\# AST Nodes:          {} ({Bi})
        \\# Total ZIR bytes:    {Bi}
        \\# Instructions:       {d} ({Bi})
        \\# String Table Bytes: {}
        \\# Extra Data Items:   {d} ({Bi})
    , .{
        source.len,
        tree.tokens.len, token_bytes,
        tree.nodes.len, tree_bytes,
        total_bytes,
        zir.instructions.len, instruction_bytes,
        zir.string_bytes.len,
        zir.extra.len, extra_bytes,
    });
    // zig fmt: on

    try kdb.print_zir.renderAsText(gpa, tree, zir, stdout_bw);
    try stdout_bw.flush();

    if (zir.hasCompileErrors()) {
        std.process.exit(1);
    } else {
        return cleanExit();
    }
}

const usage_lsp =
    \\Usage: kdblint lsp
    \\
    \\  Runs the language server.
    \\
    \\Options:
    \\
    \\  -h, --help                  Print this help and exit
    \\
;

fn cmdLsp(gpa: Allocator, binary: []const u8, args: []const []const u8) !void {
    var i: usize = 0;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (std.mem.startsWith(u8, arg, "-")) {
            if (std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "--help")) {
                try std.fs.File.stdout().writeAll(usage_lsp);
                return std.process.cleanExit();
            } else {
                fatal("unrecognized parameter: '{s}'", .{arg});
            }
        } else {
            fatal("extra positional parameter: '{s}'", .{arg});
        }
    }

    if (std.fs.File.stdin().isTty()) {
        log.warn("kdblint lsp is not a CLI tool, it communicates over the Language Server Protocol.", .{});
        log.warn("Did you mean to run 'kdblint --help'?", .{});
    }

    log_file, const log_file_path = createLogFile(gpa, null) orelse .{ null, null };
    defer if (log_file_path) |path| gpa.free(path);
    defer if (log_file) |file| {
        file.close();
        log_file = null;
    };

    var read_buffer: [256]u8 = undefined;
    var stdio_transport: zls.lsp.Transport.Stdio = .init(&read_buffer, .stdin(), .stdout());

    var thread_safe_transport: zls.lsp.ThreadSafeTransport(.{
        .thread_safe_read = false,
        .thread_safe_write = true,
    }) = .init(&stdio_transport.transport);

    const transport: *zls.lsp.Transport = &thread_safe_transport.transport;

    log_transport = transport;
    log_stderr = false;
    log_level = log_level;
    defer {
        log_transport = null;
        log_stderr = true;
    }

    log.info("Starting kdblint {s} @ '{s}'", .{ build_options.version_string, binary });
    if (log_file_path) |path| {
        log.info("Log File:        {s} ({t})", .{ path, log_level });
    } else {
        log.info("Log File:        none", .{});
    }

    const server: *zls.Server = try .create(.{
        .allocator = gpa,
        .transport = transport,
        .config = null,
    });
    defer server.destroy();

    try server.loop();

    switch (server.status) {
        .exiting_failure => std.process.exit(1),
        .exiting_success => std.process.cleanExit(),
        else => unreachable,
    }
}
