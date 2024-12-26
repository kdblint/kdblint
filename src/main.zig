const std = @import("std");
const builtin = @import("builtin");
const io = std.io;
const mem = std.mem;
const Allocator = mem.Allocator;
const process = std.process;
const assert = std.debug.assert;
const zls = @import("zls");
const Color = std.zig.Color;

const kdb = @import("kdb");
const DocumentScope = kdb.DocumentScope;
const build_options = @import("build_options");

const Server = @import("lsp/Server.zig");

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

    const args = try process.argsAlloc(arena);

    return mainArgs(gpa, arena, args);
}

fn mainArgs(gpa: Allocator, arena: Allocator, args: []const []const u8) !void {
    if (args.len <= 1) {
        std.log.info("{s}", .{usage});
        fatal("expected command argument", .{});
    }

    const cmd = args[1];
    const cmd_args = args[2..];
    if (mem.eql(u8, cmd, "lsp")) {
        return cmdLsp(gpa, cmd_args);
    } else if (mem.eql(u8, cmd, "ast-check")) {
        return cmdAstCheck(gpa, arena, cmd_args);
    } else if (mem.eql(u8, cmd, "fmt")) {
        return @import("fmt.zig").run(gpa, arena, cmd_args);
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

fn cmdAstCheck(
    gpa: Allocator,
    arena: Allocator,
    args: []const []const u8,
) !void {
    var color: Color = .auto;
    var want_output_text = false;
    var kdb_source_file: ?[]const u8 = null;

    var i: usize = 0;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (mem.startsWith(u8, arg, "-")) {
            if (mem.eql(u8, arg, "-h") or mem.eql(u8, arg, "--help")) {
                try io.getStdOut().writeAll(usage_ast_check);
                return process.cleanExit();
            } else if (mem.eql(u8, arg, "-t")) {
                want_output_text = true;
            } else if (mem.eql(u8, arg, "--color")) {
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
        } else if (kdb_source_file == null) {
            kdb_source_file = arg;
        } else {
            fatal("extra positional parameter: '{s}'", .{arg});
        }
    }

    var file: kdb.File = .{
        .sub_file_path = undefined,
        .source = undefined,
        .tree = undefined,
        .zir = undefined,
    };
    if (kdb_source_file) |file_name| {
        var f = std.fs.cwd().openFile(file_name, .{}) catch |err| {
            fatal("unable to open file for ast-check '{s}': {s}", .{ file_name, @errorName(err) });
        };
        defer f.close();

        const stat = try f.stat();

        if (stat.size > std.zig.max_src_size)
            return error.FileTooBig;

        const source = try arena.allocSentinel(u8, @as(usize, @intCast(stat.size)), 0);
        const amt = try f.readAll(source);
        if (amt != stat.size)
            return error.UnexpectedEndOfFile;

        file.sub_file_path = file_name;
        file.source = source;
        file.source_loaded = true;
    } else {
        const stdin = io.getStdIn();
        const source = std.zig.readSourceFileToEndAlloc(arena, stdin, null) catch |err| {
            fatal("unable to read stdin: {}", .{err});
        };
        file.sub_file_path = "<stdin>";
        file.source = source;
        file.source_loaded = true;
    }

    file.tree = try kdb.Ast.parse(gpa, file.source, .{
        .mode = if (mem.endsWith(u8, file.sub_file_path, ".k")) .k else .q,
        .version = .@"4.0",
    });
    file.tree_loaded = true;
    defer file.tree.deinit(gpa);

    var document_scope: DocumentScope = .{};
    defer document_scope.deinit(gpa);
    var context: DocumentScope.ScopeContext = .{
        .gpa = gpa,
        .tree = file.tree,
        .doc_scope = &document_scope,
    };
    file.zir = try kdb.AstGen.generate(gpa, &context);
    file.zir_loaded = true;
    defer file.zir.deinit(gpa);

    if (file.zir.hasCompileErrors()) {
        try kdb.printZirErrorsToStderr(gpa, file.tree, file.zir, file.sub_file_path, color);
        process.exit(1);
    } else if (file.zir.hasCompileWarnings()) {
        try kdb.printZirErrorsToStderr(gpa, file.tree, file.zir, file.sub_file_path, color);
    }

    if (!want_output_text) {
        return process.cleanExit();
    }

    {
        const token_bytes = @sizeOf(std.MultiArrayList(kdb.Ast.Token)) +
            file.tree.tokens.len * (@sizeOf(std.zig.Token.Tag) + @sizeOf(kdb.Ast.ByteOffset));
        const tree_bytes = @sizeOf(kdb.Ast) + file.tree.nodes.len *
            (@sizeOf(kdb.Ast.Node.Tag) +
            @sizeOf(kdb.Ast.Node.Data) +
            @sizeOf(kdb.Ast.Token.Index));
        const instruction_bytes = file.zir.instructions.len *
            // Here we don't use @sizeOf(Zir.Inst.Data) because it would include
            // the debug safety tag but we want to measure release size.
            (@sizeOf(kdb.Zir.Inst.Tag) + 8);
        const extra_bytes = file.zir.extra.len * @sizeOf(u32);
        const total_bytes = @sizeOf(kdb.Zir) + instruction_bytes + extra_bytes +
            file.zir.string_bytes.len * @sizeOf(u8);
        const stdout = io.getStdOut();
        const fmtIntSizeBin = std.fmt.fmtIntSizeBin;
        // zig fmt: off
        try stdout.writer().print(
            \\Source bytes      | {}
            \\Tokens            | {} ({})
            \\AST nodes         | {} ({})
            \\Total IR bytes    | {}
            \\Instructions      | {d} ({})
            \\String table bytes| {}
            \\Extra data items  | {d} ({})
            \\
        , .{
            fmtIntSizeBin(file.source.len),
            file.tree.tokens.len, fmtIntSizeBin(token_bytes),
            file.tree.nodes.len, fmtIntSizeBin(tree_bytes),
            fmtIntSizeBin(total_bytes),
            file.zir.instructions.len, fmtIntSizeBin(instruction_bytes),
            fmtIntSizeBin(file.zir.string_bytes.len),
            file.zir.extra.len, fmtIntSizeBin(extra_bytes),
        });
        // zig fmt: on
    }

    return kdb.print_zir.renderAsText(gpa, &file, io.getStdOut());
}

const usage_lsp =
    \\Usage: kdblint lsp
    \\
    \\  Runs the language server.
    \\
    \\Options:
    \\
    \\  -h, --help                  Print this help and exit
    \\  --enable-message-tracing    Enable message tracing
    \\
;

fn cmdLsp(gpa: Allocator, args: []const []const u8) !void {
    var enable_message_tracing = false;

    var i: usize = 0;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (mem.startsWith(u8, arg, "-")) {
            if (mem.eql(u8, arg, "-h") or mem.eql(u8, arg, "--help")) {
                try std.io.getStdOut().writeAll(usage_lsp);
                return process.cleanExit();
            } else if (std.mem.eql(u8, arg, "--enable-message-tracing")) {
                enable_message_tracing = true;
            } else {
                fatal("unrecognized parameter: '{s}'", .{arg});
            }
        } else {
            fatal("extra positional parameter: '{s}'", .{arg});
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
