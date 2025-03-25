const std = @import("std");
const mem = std.mem;
const fs = std.fs;
const process = std.process;
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const Color = std.zig.Color;

const kdb = @import("kdb");
const Ast = kdb.Ast;
const DocumentScope = kdb.DocumentScope;

const fatal = @import("main.zig").fatal;

const usage_fmt =
    \\Usage: kdblint fmt [file]...
    \\
    \\   Formats the input files and modifies them in-place.
    \\   Arguments can be files or directories, which are searched
    \\   recursively.
    \\
    \\Options:
    \\
    \\  -h, --help             Print this help and exit
    \\  --color [auto|off|on]  Enable or disable colored error messages
    \\  --stdin                Format code from stdin; output to stdout
    \\  --check                List non-conforming files and exit with an error
    \\                         if the list is non-empty
    \\  --ast-check            Run zig ast-check on every file
    \\  --exclude [file]       Exclude file or directory from formatting
    \\
    \\Format options:
    \\
    \\  --prefer-tabs          Prefer tabs for indentation
    \\  --indent-size          Number of characters to use per indentation level
    \\
;

const Fmt = struct {
    seen: SeenMap,
    any_error: bool,
    check_ast: bool,
    color: Color,
    gpa: Allocator,
    arena: Allocator,
    out_buffer: std.ArrayList(u8),
    indent_char: u8,
    indent_delta: usize,

    const SeenMap = std.AutoHashMap(fs.File.INode, void);
};

pub fn run(
    gpa: Allocator,
    arena: Allocator,
    args: []const []const u8,
) !void {
    var color: Color = .auto;
    var stdin_flag: bool = false;
    var check_flag: bool = false;
    var check_ast_flag: bool = false;
    var prefer_tabs: bool = false;
    var indent_size: usize = 2;
    var input_files = std.ArrayList([]const u8).init(gpa);
    defer input_files.deinit();
    var excluded_files = std.ArrayList([]const u8).init(gpa);
    defer excluded_files.deinit();

    {
        var i: usize = 0;
        while (i < args.len) : (i += 1) {
            const arg = args[i];
            if (mem.startsWith(u8, arg, "-")) {
                if (mem.eql(u8, arg, "-h") or mem.eql(u8, arg, "--help")) {
                    const stdout = std.io.getStdOut().writer();
                    try stdout.writeAll(usage_fmt);
                    return process.cleanExit();
                } else if (mem.eql(u8, arg, "--color")) {
                    if (i + 1 >= args.len) {
                        fatal("expected [auto|on|off] after --color", .{});
                    }
                    i += 1;
                    const next_arg = args[i];
                    color = std.meta.stringToEnum(Color, next_arg) orelse {
                        fatal("expected [auto|on|off] after --color, found '{s}'", .{next_arg});
                    };
                } else if (mem.eql(u8, arg, "--stdin")) {
                    stdin_flag = true;
                } else if (mem.eql(u8, arg, "--check")) {
                    check_flag = true;
                } else if (mem.eql(u8, arg, "--ast-check")) {
                    check_ast_flag = true;
                } else if (mem.eql(u8, arg, "--exclude")) {
                    if (i + 1 >= args.len) {
                        fatal("expected parameter after --exclude", .{});
                    }
                    i += 1;
                    const next_arg = args[i];
                    try excluded_files.append(next_arg);
                } else if (mem.eql(u8, arg, "--prefer-tabs")) {
                    prefer_tabs = true;
                    if (indent_size == 2) indent_size = 1; // Default to 1 tab
                } else if (mem.eql(u8, arg, "--indent-size")) {
                    if (i + 1 >= args.len) {
                        fatal("expected parameter after --indent-size", .{});
                    }
                    i += 1;
                    indent_size = try std.fmt.parseUnsigned(usize, args[i], 10);
                } else {
                    fatal("unrecognized parameter: '{s}'", .{arg});
                }
            } else {
                try input_files.append(arg);
            }
        }
    }

    if (stdin_flag) {
        if (input_files.items.len != 0) {
            fatal("cannot use --stdin with positional arguments", .{});
        }

        const stdin = std.io.getStdIn();
        const source_code = std.zig.readSourceFileToEndAlloc(
            arena,
            stdin,
            null,
        ) catch |err| {
            fatal("unable to read stdin: {}", .{err});
        };
        defer arena.free(source_code);

        var tree = kdb.Ast.parse(gpa, source_code, .{
            .mode = .q,
            .version = .@"4.0",
        }) catch |err| {
            fatal("error parsing stdin: {}", .{err});
        };
        defer tree.deinit(gpa);

        if (check_ast_flag) {
            var document_scope: DocumentScope = .{};
            defer document_scope.deinit(gpa);
            var context: DocumentScope.ScopeContext = .{
                .gpa = gpa,
                .tree = tree,
                .doc_scope = &document_scope,
            };
            defer context.deinit();
            var zir = try kdb.AstGen.generate(gpa, &context);
            defer zir.deinit(gpa);

            if (zir.hasCompileErrors() or zir.hasCompileWarnings()) {
                try kdb.printZirErrorsToStderr(gpa, tree, zir, "<stdin>", color);
                process.exit(2);
            }
        } else if (tree.errors.len != 0) {
            try kdb.printAstErrorsToStderr(gpa, tree, "<stdin>", color);
            process.exit(2);
        }
        const formatted = try tree.render(gpa, .{
            .indent_char = if (prefer_tabs) '\t' else ' ',
            .indent_delta = indent_size,
        });
        defer gpa.free(formatted);

        if (check_flag) {
            const code: u8 = @intFromBool(mem.eql(u8, formatted, source_code));
            process.exit(code);
        }

        return std.io.getStdOut().writeAll(formatted);
    }

    if (input_files.items.len == 0) {
        fatal("expected at least one source file argument", .{});
    }

    var fmt = Fmt{
        .gpa = gpa,
        .arena = arena,
        .seen = Fmt.SeenMap.init(gpa),
        .any_error = false,
        .check_ast = check_ast_flag,
        .color = color,
        .out_buffer = std.ArrayList(u8).init(gpa),
        .indent_char = if (prefer_tabs) '\t' else ' ',
        .indent_delta = indent_size,
    };
    defer fmt.seen.deinit();
    defer fmt.out_buffer.deinit();

    // Mark any excluded files/directories as already seen,
    // so that they are skipped later during actual processing
    for (excluded_files.items) |file_path| {
        const stat = fs.cwd().statFile(file_path) catch |err| switch (err) {
            error.FileNotFound => continue,
            // On Windows, statFile does not work for directories
            error.IsDir => dir: {
                var dir = try fs.cwd().openDir(file_path, .{});
                defer dir.close();
                break :dir try dir.stat();
            },
            else => |e| return e,
        };
        try fmt.seen.put(stat.inode, {});
    }

    for (input_files.items) |file_path| {
        try fmtPath(&fmt, file_path, check_flag, fs.cwd(), file_path);
    }
    if (fmt.any_error) {
        process.exit(1);
    }
}

const FmtError = error{
    SystemResources,
    OperationAborted,
    IoPending,
    BrokenPipe,
    Unexpected,
    WouldBlock,
    Canceled,
    FileClosed,
    DestinationAddressRequired,
    DiskQuota,
    FileTooBig,
    InputOutput,
    NoSpaceLeft,
    AccessDenied,
    OutOfMemory,
    RenameAcrossMountPoints,
    ReadOnlyFileSystem,
    LinkQuotaExceeded,
    FileBusy,
    EndOfStream,
    Unseekable,
    NotOpenForWriting,
    UnsupportedEncoding,
    ConnectionResetByPeer,
    SocketNotConnected,
    LockViolation,
    NetNameDeleted,
    InvalidArgument,
    ProcessNotFound,
    InvalidEncoding,
    MessageTooBig,
} || fs.File.OpenError;

fn fmtPath(fmt: *Fmt, file_path: []const u8, check_mode: bool, dir: fs.Dir, sub_path: []const u8) FmtError!void {
    fmtPathFile(
        fmt,
        file_path,
        check_mode,
        dir,
        sub_path,
    ) catch |err| switch (err) {
        error.IsDir, error.AccessDenied => return fmtPathDir(
            fmt,
            file_path,
            check_mode,
            dir,
            sub_path,
        ),
        else => {
            std.log.err("unable to format '{s}': {s}", .{ file_path, @errorName(err) });
            fmt.any_error = true;
            return;
        },
    };
}

fn fmtPathDir(
    fmt: *Fmt,
    file_path: []const u8,
    check_mode: bool,
    parent_dir: fs.Dir,
    parent_sub_path: []const u8,
) FmtError!void {
    var dir = try parent_dir.openDir(parent_sub_path, .{ .iterate = true });
    defer dir.close();

    const stat = try dir.stat();
    if (try fmt.seen.fetchPut(stat.inode, {})) |_| return;

    var dir_it = dir.iterate();
    while (try dir_it.next()) |entry| {
        const is_dir = entry.kind == .directory;

        if (mem.startsWith(u8, entry.name, ".")) continue;

        if (is_dir or entry.kind == .file and
            (mem.endsWith(u8, entry.name, ".k") or
                mem.endsWith(u8, entry.name, ".q")))
        {
            const full_path = try fs.path.join(fmt.gpa, &[_][]const u8{ file_path, entry.name });
            defer fmt.gpa.free(full_path);

            if (is_dir) {
                try fmtPathDir(
                    fmt,
                    full_path,
                    check_mode,
                    dir,
                    entry.name,
                );
            } else {
                fmtPathFile(
                    fmt,
                    full_path,
                    check_mode,
                    dir,
                    entry.name,
                ) catch |err| {
                    std.log.err("unable to format '{s}': {s}", .{ full_path, @errorName(err) });
                    fmt.any_error = true;
                    return;
                };
            }
        }
    }
}

fn fmtPathFile(
    fmt: *Fmt,
    file_path: []const u8,
    check_mode: bool,
    dir: fs.Dir,
    sub_path: []const u8,
) FmtError!void {
    const source_file = try dir.openFile(sub_path, .{});
    var file_closed = false;
    errdefer if (!file_closed) source_file.close();

    const stat = try source_file.stat();

    if (stat.kind == .directory)
        return error.IsDir;

    const gpa = fmt.gpa;
    const source_code = try std.zig.readSourceFileToEndAlloc(
        fmt.arena,
        source_file,
        std.math.cast(usize, stat.size) orelse return error.FileTooBig,
    );
    defer fmt.arena.free(source_code);

    source_file.close();
    file_closed = true;

    // Add to set after no longer possible to get error.IsDir.
    if (try fmt.seen.fetchPut(stat.inode, {})) |_| return;

    var tree = try kdb.Ast.parse(gpa, source_code, .{
        .mode = if (mem.endsWith(u8, sub_path, ".k")) .k else .q,
        .version = .@"4.0",
    });
    defer tree.deinit(gpa);

    if (tree.errors.len != 0) {
        try kdb.printAstErrorsToStderr(gpa, tree, file_path, fmt.color);
        fmt.any_error = true;
        return;
    }

    if (fmt.check_ast) {
        if (stat.size > std.zig.max_src_size)
            return error.FileTooBig;

        var document_scope: DocumentScope = .{};
        defer document_scope.deinit(gpa);
        var context: DocumentScope.ScopeContext = .{
            .gpa = gpa,
            .tree = tree,
            .doc_scope = &document_scope,
        };
        defer context.deinit();
        var zir = try kdb.AstGen.generate(gpa, &context);
        defer zir.deinit(gpa);

        if (zir.hasCompileErrors() or zir.hasCompileWarnings()) {
            try kdb.printZirErrorsToStderr(gpa, tree, zir, file_path, fmt.color);
            fmt.any_error = true;
        }
    }

    // As a heuristic, we make enough capacity for the same as the input source.
    fmt.out_buffer.shrinkRetainingCapacity(0);
    try fmt.out_buffer.ensureTotalCapacity(source_code.len);

    try tree.renderToArrayList(&fmt.out_buffer, .{
        .indent_char = fmt.indent_char,
        .indent_delta = fmt.indent_delta,
    });
    if (mem.eql(u8, fmt.out_buffer.items, source_code))
        return;

    if (check_mode) {
        const stdout = std.io.getStdOut().writer();
        try stdout.print("{s}\n", .{file_path});
        fmt.any_error = true;
    } else {
        var af = try dir.atomicFile(sub_path, .{ .mode = stat.mode });
        defer af.deinit();

        try af.file.writeAll(fmt.out_buffer.items);
        try af.finish();
        const stdout = std.io.getStdOut().writer();
        try stdout.print("{s}\n", .{file_path});
    }
}
