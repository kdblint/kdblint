const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const Io = std.Io;
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
    .wasiCwd = wasi_cwd,
    .log_level = .debug,
};

var wasi_preopens: std.fs.wasi.Preopens = undefined;
pub fn wasi_cwd() std.os.wasi.fd_t {
    // Expect the first preopen to be current working directory.
    const cwd_fd: std.posix.fd_t = 3;
    assert(std.mem.eql(u8, wasi_preopens.names[cwd_fd], "."));
    return cwd_fd;
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

    var threaded: std.Io.Threaded = .init(gpa);
    defer threaded.deinit();
    const io = threaded.io();

    return mainArgs(gpa, arena, io, args);
}

fn mainArgs(gpa: Allocator, arena: Allocator, io: Io, args: []const []const u8) !void {
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
        return std.fs.File.stdout().writeAll(build_options.version_string ++ "\n");
    } else if (std.mem.eql(u8, cmd, "help") or std.mem.eql(u8, cmd, "-h") or std.mem.eql(u8, cmd, "--help")) {
        return std.fs.File.stdout().writeAll(usage);
    } else {
        try std.fs.File.stdout().writeAll(usage ++ "\n");
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
        var file_reader = f.reader(io, &stdin_buffer);
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
    var context: DocumentScope.ScopeContext = .{
        .gpa = arena,
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

fn cmdLsp(gpa: Allocator, io: Io, args: []const []const u8) !void {
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

    var read_buffer: [256]u8 = undefined;
    var stdio_transport: lsp.Transport.Stdio = .init(io, &read_buffer, .stdin(), .stdout());

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
        .exiting_success => cleanExit(),
        else => unreachable,
    }
}

test {
    @import("std").testing.refAllDecls(@This());
}
