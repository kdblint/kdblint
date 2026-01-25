const std = @import("std");
const builtin = @import("builtin");
const Io = std.Io;
const Allocator = std.mem.Allocator;
const Color = std.zig.Color;
const assert = std.debug.assert;

const lsp = @import("lsp");
const Server = @import("lsp/Server.zig");

const kdb = @import("kdb/root.zig");
const DocumentScope = kdb.DocumentScope;

const build_options = @import("build_options");

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
    .log_level = .debug,
};
pub const std_options_cwd = if (builtin.os.tag == .wasi) wasi_cwd else null;

var preopens: std.process.Preopens = .empty;
pub fn wasi_cwd() Io.Dir {
    // Expect the first preopen to be current working directory.
    const cwd_fd: std.posix.fd_t = 3;
    assert(std.mem.eql(u8, preopens.map.keys()[cwd_fd], "."));
    return .{ .handle = cwd_fd };
}

const fatal = std.process.fatal;
const cleanExit = std.process.cleanExit;

/// This can be global since stdin is a singleton.
var stdin_buffer: [4096]u8 align(std.heap.page_size_min) = undefined;
/// This can be global since stdout is a singleton.
var stdout_buffer: [4096]u8 align(std.heap.page_size_min) = undefined;

var debug_allocator: std.heap.DebugAllocator(.{}) = .init;

const use_debug_allocator = builtin.os.tag != .wasi and switch (builtin.mode) {
    .Debug, .ReleaseSafe => true,
    .ReleaseFast, .ReleaseSmall => false,
};

pub fn main(init: std.process.Init.Minimal) !void {
    const gpa = gpa: {
        if (use_debug_allocator) break :gpa debug_allocator.allocator();
        if (builtin.os.tag == .wasi) break :gpa std.heap.wasm_allocator;
        break :gpa std.heap.smp_allocator;
    };
    defer if (use_debug_allocator) {
        _ = debug_allocator.deinit();
    };
    var arena_instance: std.heap.ArenaAllocator = .init(gpa);
    defer arena_instance.deinit();
    const arena = arena_instance.allocator();

    const args = try init.args.toSlice(arena);

    var environ_map = init.environ.createMap(arena) catch |err| fatal("failed to parse environment: {t}", .{err});

    var threaded: Io.Threaded = .init(gpa, .{
        .environ = init.environ,
    });
    defer threaded.deinit();
    const io = threaded.io();

    if (builtin.os.tag == .wasi) {
        preopens = try .init(arena);
    }

    return mainArgs(gpa, arena, io, args, &environ_map);
}

fn mainArgs(
    gpa: Allocator,
    arena: Allocator,
    io: Io,
    args: []const []const u8,
    environ_map: *std.process.Environ.Map,
) !void {
    _ = environ_map; // autofix
    if (args.len <= 1) {
        log.info("{s}", .{usage});
        fatal("expected command argument", .{});
    }

    const cmd = args[1];
    const cmd_args = args[2..];
    if (std.mem.eql(u8, cmd, "lsp")) {
        return cmdLsp(gpa, io, cmd_args);
    } else if (std.mem.eql(u8, cmd, "ast-check")) {
        return cmdAstCheck(gpa, arena, io, cmd_args);
    } else if (std.mem.eql(u8, cmd, "fmt")) {
        return @import("fmt.zig").run(gpa, arena, io, cmd_args);
    } else if (std.mem.eql(u8, cmd, "version")) {
        return Io.File.stdout().writeStreamingAll(io, build_options.version_string ++ "\n");
    } else if (std.mem.eql(u8, cmd, "help") or std.mem.eql(u8, cmd, "-h") or std.mem.eql(u8, cmd, "--help")) {
        return Io.File.stdout().writeStreamingAll(io, usage);
    } else {
        try Io.File.stdout().writeStreamingAll(io, usage ++ "\n");
        fatal("unknown command: {s}", .{cmd});
    }
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

fn cmdAstCheck(gpa: Allocator, arena: Allocator, io: Io, args: []const []const u8) !void {
    var color: Color = .auto;
    var want_output_text = false;
    var kdb_source_path: ?[]const u8 = null;

    var i: usize = 0;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (std.mem.startsWith(u8, arg, "-")) {
            if (std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "--help")) {
                try Io.File.stdout().writeStreamingAll(io, usage_ast_check);
                return cleanExit(io);
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
            break :file Io.Dir.cwd().openFile(io, p, .{}) catch |err| {
                fatal("unable to open file '{s}' for ast-check: {s}", .{ display_path, @errorName(err) });
            };
        } else Io.File.stdin();
        defer if (kdb_source_path != null) f.close(io);
        var file_reader = f.reader(io, &stdin_buffer);
        break :s std.zig.readSourceFileToEndAlloc(arena, &file_reader) catch |err| {
            fatal("unable to load file '{s}' for ast-check: {s}", .{ display_path, @errorName(err) });
        };
    };

    const tree = try kdb.Ast.parse(arena, source, .{
        .mode = if (std.mem.endsWith(u8, display_path, ".k")) .k else .q,
        .version = .@"4.0",
    });

    var stdout_writer = Io.File.stdout().writerStreaming(io, &stdout_buffer);
    const stdout_bw = &stdout_writer.interface;

    var document_scope: DocumentScope = .{};
    var context: DocumentScope.ScopeContext = .{
        .gpa = arena,
        .tree = tree,
        .doc_scope = &document_scope,
    };
    const zir = try kdb.AstGen.generate(arena, &context);

    if (zir.hasCompileErrors()) {
        try kdb.printZirErrorsToStderr(gpa, io, tree, zir, display_path, color);
        std.process.exit(1);
    } else if (zir.hasCompileWarnings()) {
        try kdb.printZirErrorsToStderr(gpa, io, tree, zir, display_path, color);
    }

    if (!want_output_text) {
        return cleanExit(io);
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
        return cleanExit(io);
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

fn cmdLsp(gpa: Allocator, io: Io, args: []const []const u8) !void {
    var i: usize = 0;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (std.mem.startsWith(u8, arg, "-")) {
            if (std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "--help")) {
                try Io.File.stdout().writeStreamingAll(io, usage_lsp);
                return cleanExit(io);
            } else {
                fatal("unrecognized parameter: '{s}'", .{arg});
            }
        } else {
            fatal("extra positional parameter: '{s}'", .{arg});
        }
    }

    var read_buffer: [256]u8 = undefined;
    var stdio_transport: lsp.Transport.Stdio = .init(&read_buffer, .stdin(), .stdout());

    var thread_safe_transport: lsp.ThreadSafeTransport(.{
        .thread_safe_read = false,
        .thread_safe_write = true,
    }) = .init(&stdio_transport.transport);

    const transport = &thread_safe_transport.transport;

    const server: *Server = try .create(io, gpa, transport);
    defer server.destroy();

    try server.loop();

    switch (server.status) {
        .exiting_failure => std.process.exit(1),
        .exiting_success => cleanExit(io),
        else => unreachable,
    }
}

test {
    _ = @import("lsp/root.zig");
    _ = @import("kdb/root.zig");
    _ = @import("fmt.zig");
}
