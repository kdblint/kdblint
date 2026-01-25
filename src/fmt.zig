const std = @import("std");
const mem = std.mem;
const fs = std.fs;
const process = std.process;
const Io = std.Io;
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const Color = std.zig.Color;

const kdb = @import("kdb/root.zig");
const Ast = kdb.Ast;
const DocumentScope = kdb.DocumentScope;

const fatal = std.process.fatal;

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
    io: Io,
    out_buffer: Io.Writer.Allocating,
    stdout_writer: *Io.File.Writer,
    indent_char: u8,
    indent_delta: usize,

    const SeenMap = std.AutoHashMap(Io.File.INode, void);
};

pub fn run(gpa: Allocator, arena: Allocator, io: Io, args: []const []const u8) !void {
    var color: Color = .auto;
    var stdin_flag: bool = false;
    var check_flag: bool = false;
    var check_ast_flag: bool = false;
    var prefer_tabs: bool = false;
    var indent_size: usize = 2;
    var input_files: std.array_list.Managed([]const u8) = .init(gpa);
    defer input_files.deinit();
    var excluded_files: std.array_list.Managed([]const u8) = .init(gpa);
    defer excluded_files.deinit();

    {
        var i: usize = 0;
        while (i < args.len) : (i += 1) {
            const arg = args[i];
            if (mem.startsWith(u8, arg, "-")) {
                if (mem.eql(u8, arg, "-h") or mem.eql(u8, arg, "--help")) {
                    try Io.File.stdout().writeStreamingAll(io, usage_fmt);
                    return process.cleanExit(io);
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

        const stdin: Io.File = .stdin();
        var stdio_buffer: [1024]u8 = undefined;
        var file_reader = stdin.reader(io, &stdio_buffer);
        const source_code = std.zig.readSourceFileToEndAlloc(gpa, &file_reader) catch |err| {
            fatal("unable to read stdin: {}", .{err});
        };
        defer gpa.free(source_code);

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
                try kdb.printZirErrorsToStderr(gpa, io, tree, zir, "<stdin>", color);
                process.exit(2);
            }
        } else if (tree.errors.len != 0) {
            try kdb.printAstErrorsToStderr(gpa, io, tree, "<stdin>", color);
            process.exit(2);
        }
        const formatted = try tree.renderAlloc(gpa, .{
            .indent_char = if (prefer_tabs) '\t' else ' ',
            .indent_delta = indent_size,
        });
        defer gpa.free(formatted);

        if (check_flag) {
            const code: u8 = @intFromBool(mem.eql(u8, formatted, source_code));
            process.exit(code);
        }

        return Io.File.stdout().writeStreamingAll(io, formatted);
    }

    if (input_files.items.len == 0) {
        fatal("expected at least one source file argument", .{});
    }

    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writer = Io.File.stdout().writer(io, &stdout_buffer);

    var fmt: Fmt = .{
        .gpa = gpa,
        .arena = arena,
        .io = io,
        .seen = .init(gpa),
        .any_error = false,
        .check_ast = check_ast_flag,
        .color = color,
        .out_buffer = .init(gpa),
        .stdout_writer = &stdout_writer,
        .indent_char = if (prefer_tabs) '\t' else ' ',
        .indent_delta = indent_size,
    };
    defer fmt.seen.deinit();
    defer fmt.out_buffer.deinit();

    // Mark any excluded files/directories as already seen,
    // so that they are skipped later during actual processing
    for (excluded_files.items) |file_path| {
        const stat = Io.Dir.cwd().statFile(io, file_path, .{}) catch |err| switch (err) {
            error.FileNotFound => continue,
            // On Windows, statFile does not work for directories
            error.IsDir => dir: {
                var dir = try Io.Dir.cwd().openDir(io, file_path, .{});
                defer dir.close(io);
                break :dir try dir.stat(io);
            },
            else => |e| return e,
        };
        try fmt.seen.put(stat.inode, {});
    }

    for (input_files.items) |file_path| {
        try fmtPath(&fmt, file_path, check_flag, .cwd(), file_path);
    }
    try fmt.stdout_writer.interface.flush();
    if (fmt.any_error) {
        process.exit(1);
    }
}

fn fmtPath(fmt: *Fmt, file_path: []const u8, check_mode: bool, dir: Io.Dir, sub_path: []const u8) !void {
    fmtPathFile(fmt, file_path, check_mode, dir, sub_path) catch |err| switch (err) {
        error.IsDir, error.AccessDenied => return fmtPathDir(fmt, file_path, check_mode, dir, sub_path),
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
    parent_dir: Io.Dir,
    parent_sub_path: []const u8,
) !void {
    const io = fmt.io;

    var dir = try parent_dir.openDir(io, parent_sub_path, .{ .iterate = true });
    defer dir.close(io);

    const stat = try dir.stat(io);
    if (try fmt.seen.fetchPut(stat.inode, {})) |_| return;

    var dir_it = dir.iterate();
    while (try dir_it.next(io)) |entry| {
        const is_dir = entry.kind == .directory;

        if (mem.startsWith(u8, entry.name, ".")) continue;

        if (is_dir or entry.kind == .file and
            (mem.endsWith(u8, entry.name, ".k") or mem.endsWith(u8, entry.name, ".q")))
        {
            const full_path = try fs.path.join(fmt.gpa, &[_][]const u8{ file_path, entry.name });
            defer fmt.gpa.free(full_path);

            if (is_dir) {
                try fmtPathDir(fmt, full_path, check_mode, dir, entry.name);
            } else {
                fmtPathFile(fmt, full_path, check_mode, dir, entry.name) catch |err| {
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
    dir: Io.Dir,
    sub_path: []const u8,
) !void {
    const io = fmt.io;

    const source_file = try dir.openFile(io, sub_path, .{});
    var file_closed = false;
    errdefer if (!file_closed) source_file.close(io);

    const stat = try source_file.stat(io);

    if (stat.kind == .directory)
        return error.IsDir;

    var read_buffer: [1024]u8 = undefined;
    var file_reader = source_file.reader(fmt.io, &read_buffer);
    file_reader.size = stat.size;

    const gpa = fmt.gpa;
    const source_code = std.zig.readSourceFileToEndAlloc(gpa, &file_reader) catch |err| switch (err) {
        error.ReadFailed => return file_reader.err.?,
        else => |e| return e,
    };
    defer gpa.free(source_code);

    source_file.close(io);
    file_closed = true;

    // Add to set after no longer possible to get error.IsDir.
    if (try fmt.seen.fetchPut(stat.inode, {})) |_| return;

    const mode: kdb.Ast.Mode = if (mem.endsWith(u8, sub_path, ".k")) .k else .q;

    var tree = try kdb.Ast.parse(gpa, source_code, .{
        .mode = mode,
        .version = .@"4.0",
    });
    defer tree.deinit(gpa);

    if (tree.errors.len != 0) {
        try kdb.printAstErrorsToStderr(gpa, io, tree, file_path, fmt.color);
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
            try kdb.printZirErrorsToStderr(gpa, io, tree, zir, file_path, fmt.color);
            fmt.any_error = true;
        }
    }

    // As a heuristic, we make enough capacity for the same as the input source.
    fmt.out_buffer.clearRetainingCapacity();
    try fmt.out_buffer.ensureTotalCapacity(source_code.len);

    tree.render(gpa, &fmt.out_buffer.writer, .{
        .indent_char = fmt.indent_char,
        .indent_delta = fmt.indent_delta,
    }) catch |err| switch (err) {
        error.WriteFailed, error.OutOfMemory => return error.OutOfMemory,
    };
    if (mem.eql(u8, fmt.out_buffer.written(), source_code))
        return;

    if (check_mode) {
        try fmt.stdout_writer.interface.print("{s}\n", .{file_path});
        fmt.any_error = true;
    } else {
        var af = try dir.createFileAtomic(io, sub_path, .{ .permissions = stat.permissions, .replace = true });
        defer af.deinit(io);

        try af.file.writeStreamingAll(io, fmt.out_buffer.written());
        try af.replace(io);
        try fmt.stdout_writer.interface.print("{s}\n", .{file_path});
    }
}

test {
    std.testing.refAllDecls(@This());
}
