const std = @import("std");
const mem = std.mem;
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const InternPool = @import("InternPool.zig");

const kdb = @import("root.zig");
const Ast = kdb.Ast;
const AstGen = kdb.AstGen;
const Zir = kdb.Zir;
const File = kdb.File;
const DocumentScope = kdb.DocumentScope;

/// Write human-readable, debug formatted ZIR code to a file.
pub fn renderAsText(
    gpa: Allocator,
    scope_file: *File,
    output: anytype,
) !void {
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();

    var writer: Writer = .{
        .gpa = gpa,
        .arena = arena.allocator(),
        .file = scope_file,
        .code = scope_file.zir.?,
        .indent = 0,
        .parent_decl_node = .root,
        .recurse_blocks = true,
    };

    var raw_stream = std.io.bufferedWriter(output.writer());
    const stream = raw_stream.writer();

    const inst: Zir.Inst.Index = .file_inst;
    try stream.print("%{d} ", .{@intFromEnum(inst)});
    try writer.writeInstToStream(stream, inst);
    try stream.writeAll("\n");

    const imports_index = scope_file.zir.?.extra[@intFromEnum(Zir.ExtraIndex.imports)];
    if (imports_index != 0) {
        try stream.writeAll("Imports:\n");

        const extra = scope_file.zir.?.extraData(Zir.Inst.Imports, imports_index);
        var extra_index = extra.end;

        for (0..extra.data.imports_len) |_| {
            const item = scope_file.zir.?.extraData(Zir.Inst.Imports.Item, extra_index);
            extra_index = item.end;

            const import_path = scope_file.zir.?.nullTerminatedString(item.data.name);
            try stream.print("  @import(\"{}\") ", .{
                std.zig.fmtEscapes(import_path),
            });
            try writer.writeSrcTokAbs(stream, item.data.token);
            try stream.writeAll("\n");
        }
    }

    try raw_stream.flush();
}

const Writer = struct {
    gpa: Allocator,
    arena: Allocator,
    file: *File,
    code: Zir,
    indent: u32,
    parent_decl_node: Ast.Node.Index,
    recurse_blocks: bool,

    /// Using `std.zig.findLineColumn` whenever we need to resolve a source location makes ZIR
    /// printing O(N^2), which can have drastic effects - taking a ZIR dump from a few seconds to
    /// many minutes. Since we're usually resolving source locations close to one another,
    /// preserving state across source location resolutions speeds things up a lot.
    line_col_cursor: struct {
        line: usize = 0,
        column: usize = 0,
        line_start: usize = 0,
        off: usize = 0,

        fn find(cur: *@This(), source: []const u8, want_offset: usize) std.zig.Loc {
            if (want_offset <= cur.off) {
                // Go back to the start of this line
                cur.off = cur.line_start;
                cur.column = 0;

                while (want_offset < cur.off) {
                    // Go back to the newline
                    cur.off -= 1;

                    // Seek to the start of the previous line
                    while (cur.off > 0 and source[cur.off - 1] != '\n') {
                        cur.off -= 1;
                    }
                    cur.line_start = cur.off;
                    cur.line -= 1;
                }
            }

            // The cursor is now positioned before `want_offset`.
            // Seek forward as in `std.zig.findLineColumn`.

            while (cur.off < want_offset) : (cur.off += 1) {
                switch (source[cur.off]) {
                    '\n' => {
                        cur.line += 1;
                        cur.column = 0;
                        cur.line_start = cur.off + 1;
                    },
                    else => {
                        cur.column += 1;
                    },
                }
            }

            while (cur.off < source.len and source[cur.off] != '\n') {
                cur.off += 1;
            }

            return .{
                .line = cur.line,
                .column = cur.column,
                .source_line = source[cur.line_start..cur.off],
            };
        }
    } = .{},

    fn relativeToNodeIndex(self: *Writer, offset: i32) Ast.Node.Index {
        return @bitCast(offset + @as(i32, @bitCast(self.parent_decl_node)));
    }

    fn writeInstToStream(
        self: *Writer,
        stream: anytype,
        inst: Zir.Inst.Index,
    ) (@TypeOf(stream).Error || error{OutOfMemory})!void {
        const tags: []Zir.Inst.Tag = self.code.instructions.items(.tag);
        const tag = tags[@intFromEnum(inst)];
        try stream.print("= {s}(", .{@tagName(tag)});
        switch (tag) {
            .file => try self.writePlNodeBlockWithoutSrc(stream, inst),

            .print,
            .discard,
            => try self.writeUnNodeWithoutSrc(stream, inst),

            .list => try self.writePlNodeList(stream, inst),
            .table => try self.writePlNodeTable(stream, inst),

            .ret_node,
            .signal,
            => try self.writeUnNode(stream, inst),

            .ret_implicit => try self.writeUnTok(stream, inst),

            .assign,
            .add,
            .subtract,
            .multiply,
            .divide,
            .lesser,
            .greater,
            .fill,
            .equal,
            .less_than,
            .less_than_or_equal,
            .not_equal,
            .greater_than,
            .greater_than_or_equal,
            .cast,
            .join,
            .take,
            .drop,
            .match,
            .dict,
            .find,
            .apply_at,
            .apply_dot,
            .file_text,
            .file_binary,
            .dynamic_load,
            => try self.writeNode(stream, inst),

            .lambda => try self.writeLambda(stream, inst),

            .do => try self.writePlNodeDo(stream, inst),
            .@"if" => try self.writePlNodeIf(stream, inst),
            .@"while" => try self.writePlNodeWhile(stream, inst),

            .byte,
            .char,
            => try self.writeByte(stream, inst),

            .short => try self.writeShort(stream, inst),

            .int,
            .month,
            .date,
            .minute,
            .second,
            .time,
            => try self.writeInt(stream, inst),

            .long,
            .timestamp,
            .timespan,
            => try self.writeLong(stream, inst),

            .real => try self.writeReal(stream, inst),

            .float, .datetime => try self.writeFloat(stream, inst),

            .bool_list,
            .guid_list,
            .byte_list,
            .short_list,
            .int_list,
            .long_list,
            .real_list,
            .float_list,
            .char_list,
            .timestamp_list,
            .month_list,
            .date_list,
            .datetime_list,
            .timespan_list,
            .minute_list,
            .second_list,
            .time_list,
            => try self.writePlNodeList(stream, inst),

            .str,
            .sym,
            => try self.writeStrTok(stream, inst),

            .sym_list => try self.writePlNodeStrList(stream, inst),

            .param_node => try self.writeStrNode(stream, inst),
            .param_implicit => try self.writeUnTok(stream, inst),

            .identifier,
            .builtin,
            => try self.writeStrTok(stream, inst),

            .apply => try self.writePlNodeApply(stream, inst),
        }
    }

    fn writeUnNode(
        self: *Writer,
        stream: anytype,
        inst: Zir.Inst.Index,
    ) (@TypeOf(stream).Error || error{OutOfMemory})!void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].un_node;
        try self.writeInstRef(stream, inst_data.operand);
        try stream.writeAll(") ");
        try self.writeSrcNode(stream, inst_data.src_node);
    }

    fn writeUnNodeWithoutSrc(
        self: *Writer,
        stream: anytype,
        inst: Zir.Inst.Index,
    ) (@TypeOf(stream).Error || error{OutOfMemory})!void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].un_node;
        try self.writeInstRef(stream, inst_data.operand);
        try stream.writeAll(")");
    }

    fn writeUnTok(
        self: *Writer,
        stream: anytype,
        inst: Zir.Inst.Index,
    ) (@TypeOf(stream).Error || error{OutOfMemory})!void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].un_tok;
        try self.writeInstRef(stream, inst_data.operand);
        try stream.writeAll(") ");
        try self.writeSrcTok(stream, inst_data.src_tok);
    }

    fn writeByte(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].byte;
        try stream.print("{d})", .{inst_data});
    }

    fn writeShort(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].short;
        try stream.print("{d})", .{inst_data});
    }

    fn writeInt(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].int;
        try stream.print("{d})", .{inst_data});
    }

    fn writeLong(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].long;
        try stream.print("{d})", .{inst_data});
    }

    fn writeReal(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].real;
        try stream.print("{d})", .{inst_data});
    }

    fn writeFloat(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].float;
        try stream.print("{d})", .{inst_data});
    }

    fn writePlNodeApply(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].pl_node;
        const extra = self.code.extraData(Zir.Inst.Apply, inst_data.payload_index);
        const args = self.code.extra[extra.end..][0..extra.data.len];

        try self.writeInstRef(stream, extra.data.callee);
        try stream.writeAll(", ");
        for (args[0 .. args.len - 1]) |arg| {
            try self.writeInstRef(stream, @enumFromInt(arg));
            try stream.writeAll(", ");
        }
        try self.writeInstRef(stream, @enumFromInt(args[args.len - 1]));
        try stream.writeAll(") ");
        try self.writeSrcNode(stream, inst_data.src_node);
    }

    fn writePlNodeStrList(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].pl_node;
        const extra = self.code.extraData(Zir.Inst.StrList, inst_data.payload_index);
        const strs = self.code.extra[extra.end..][0..extra.data.len];

        for (strs[0 .. strs.len - 1]) |str| {
            try stream.print("\"{}\", ", .{std.zig.fmtEscapes(self.code.nullTerminatedString(@enumFromInt(str)))});
        }
        try stream.print("\"{}\") ", .{
            std.zig.fmtEscapes(self.code.nullTerminatedString(@enumFromInt(strs[strs.len - 1]))),
        });
        try self.writeSrcNode(stream, inst_data.src_node);
    }

    fn writePlNodeList(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].pl_node;
        const extra = self.code.extraData(Zir.Inst.List, inst_data.payload_index);
        const list = self.code.extra[extra.end..][0..extra.data.len];

        for (list[0 .. list.len - 1]) |ref| {
            try self.writeInstRef(stream, @enumFromInt(ref));
            try stream.writeAll(", ");
        }
        try self.writeInstRef(stream, @enumFromInt(list[list.len - 1]));
        try stream.writeAll(") ");
        try self.writeSrcNode(stream, inst_data.src_node);
    }

    fn writePlNodeTable(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].pl_node;
        const extra = self.code.extraData(Zir.Inst.Table, inst_data.payload_index);
        assert(extra.data.columns_len > 0);

        var extra_index = extra.end;
        if (extra.data.keys_len > 0) {
            try stream.writeAll("keys={");
            for (0..extra.data.keys_len) |i| {
                const item = self.code.extraData(Zir.Inst.Table.Item, extra_index);
                extra_index = item.end;
                try stream.print("\"{}\" = ", .{std.zig.fmtEscapes(self.code.nullTerminatedString(item.data.name))});
                try self.writeInstRef(stream, item.data.ref);
                if (i != extra.data.keys_len - 1) try stream.writeAll(", ");
            }

            try stream.writeAll("}, ");
        }

        for (0..extra.data.columns_len) |i| {
            const item = self.code.extraData(Zir.Inst.Table.Item, extra_index);
            extra_index = item.end;
            try stream.print("\"{}\" = ", .{std.zig.fmtEscapes(self.code.nullTerminatedString(item.data.name))});
            try self.writeInstRef(stream, item.data.ref);
            if (i != extra.data.columns_len - 1) try stream.writeAll(", ");
        }

        try stream.writeAll(") ");
        try self.writeSrcNode(stream, inst_data.src_node);
    }

    fn writePlNodeBlockWithoutSrc(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const zir_datas: []Zir.Inst.Data = self.code.instructions.items(.data);
        const inst_data = zir_datas[@intFromEnum(inst)].pl_node;
        const extra = self.code.extraData(Zir.Inst.Block, inst_data.payload_index);
        const body = self.code.bodySlice(extra.end, extra.data.body_len);
        try self.writeBracedBody(stream, body);
        try stream.writeAll(")");
    }

    fn writeStrNode(
        self: *Writer,
        stream: anytype,
        inst: Zir.Inst.Index,
    ) (@TypeOf(stream).Error || error{OutOfMemory})!void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].str_node;
        const str = inst_data.get(self.code);
        try stream.print("\"{}\") ", .{std.zig.fmtEscapes(str)});
        try self.writeSrcNode(stream, inst_data.src_node);
    }

    fn writeStrTok(
        self: *Writer,
        stream: anytype,
        inst: Zir.Inst.Index,
    ) (@TypeOf(stream).Error || error{OutOfMemory})!void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].str_tok;
        const str = inst_data.get(self.code);
        try stream.print("\"{}\") ", .{std.zig.fmtEscapes(str)});
        try self.writeSrcTok(stream, inst_data.src_tok);
    }

    fn writeNode(
        self: *Writer,
        stream: anytype,
        inst: Zir.Inst.Index,
    ) (@TypeOf(stream).Error || error{OutOfMemory})!void {
        const src_node = self.code.instructions.items(.data)[@intFromEnum(inst)].node;
        try stream.writeAll(") ");
        try self.writeSrcNode(stream, src_node);
    }

    fn writeLambda(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const zir_datas: []Zir.Inst.Data = self.code.instructions.items(.data);
        const inst_data = zir_datas[@intFromEnum(inst)].lambda;

        const prev_parent_decl_node = self.parent_decl_node;
        defer self.parent_decl_node = prev_parent_decl_node;
        self.parent_decl_node = inst_data.src_node;

        const extra = self.code.extraData(Zir.Inst.Lambda, inst_data.payload_index);

        var extra_index = extra.end;

        const body = self.code.bodySlice(extra_index, extra.data.body_len);
        extra_index += body.len;

        const src_locs = self.code.extraData(Zir.Inst.Lambda.SrcLocs, extra_index).data;

        try self.writeBracedBody(stream, body);
        try stream.writeAll(") ");
        if (body.len != 0) {
            try stream.print("(lbrace={d}:{d},rbrace={d}:{d}) ", .{
                src_locs.lbrace_line + 1, @as(u16, @truncate(src_locs.columns)) + 1,
                src_locs.rbrace_line + 1, @as(u16, @truncate(src_locs.columns >> 16)) + 1,
            });
        }
        try self.writeSrcNode(stream, .zero);
    }

    fn writePlNodeDo(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        try self.writePlNodeStmt(stream, inst, Zir.Inst.Do);
    }

    fn writePlNodeIf(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        try self.writePlNodeStmt(stream, inst, Zir.Inst.If);
    }

    fn writePlNodeWhile(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        try self.writePlNodeStmt(stream, inst, Zir.Inst.While);
    }

    // TODO: Needs formatting.
    fn writePlNodeStmt(self: *Writer, stream: anytype, inst: Zir.Inst.Index, comptime T: type) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].pl_node;
        const extra = self.code.extraData(T, inst_data.payload_index);
        const args = self.code.extra[extra.end..][0..extra.data.body_len];

        try self.writeInstRef(stream, extra.data.condition);
        try stream.writeAll(", ");
        for (args[0 .. args.len - 1]) |arg| {
            try self.writeInstRef(stream, @enumFromInt(arg));
            try stream.writeAll(", ");
        }
        try self.writeInstRef(stream, @enumFromInt(args[args.len - 1]));
        try stream.writeAll(") ");
        try self.writeSrcNode(stream, inst_data.src_node);
    }

    fn writeInstRef(self: *Writer, stream: anytype, ref: Zir.Inst.Ref) !void {
        if (ref == .none) {
            return stream.writeAll(".none");
        } else if (ref.toIndex()) |i| {
            return self.writeInstIndex(stream, i);
        } else {
            const val: InternPool.Index = @enumFromInt(@intFromEnum(ref));
            return stream.print("@{s}", .{@tagName(val)});
        }
    }

    fn writeInstIndex(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        _ = self;
        return stream.print("%{d}", .{@intFromEnum(inst)});
    }

    fn writeSrcNode(self: *Writer, stream: anytype, src_node: Ast.Node.Offset) !void {
        const tree = self.file.tree orelse return;
        const abs_node = src_node.toAbsolute(self.parent_decl_node);
        const src_span = tree.nodeToSpan(abs_node);
        const start = self.line_col_cursor.find(tree.source, src_span.start);
        const end = self.line_col_cursor.find(tree.source, src_span.end);
        try stream.print("node_offset:{d}:{d} to :{d}:{d}", .{
            start.line + 1, start.column + 1,
            end.line + 1,   end.column + 1,
        });
    }

    fn writeSrcTok(self: *Writer, stream: anytype, src_tok: Ast.TokenOffset) !void {
        const tree = self.file.tree orelse return;
        const abs_tok = src_tok.toAbsolute(tree.firstToken(self.parent_decl_node));
        const span_start = tree.tokenStart(abs_tok);
        const span_end = span_start + @as(u32, @intCast(tree.tokenSlice(abs_tok).len));
        const start = self.line_col_cursor.find(tree.source, span_start);
        const end = self.line_col_cursor.find(tree.source, span_end);
        try stream.print("token_offset:{d}:{d} to :{d}:{d}", .{
            start.line + 1, start.column + 1,
            end.line + 1,   end.column + 1,
        });
    }

    fn writeSrcTokAbs(self: *Writer, stream: anytype, src_tok: Ast.TokenIndex) !void {
        const tree = self.file.tree orelse return;
        const span_start = tree.tokenStart(src_tok);
        const span_end = span_start + @as(u32, @intCast(tree.tokenSlice(src_tok).len));
        const start = self.line_col_cursor.find(tree.source, span_start);
        const end = self.line_col_cursor.find(tree.source, span_end);
        try stream.print("token_abs:{d}:{d} to :{d}:{d}", .{
            start.line + 1, start.column + 1,
            end.line + 1,   end.column + 1,
        });
    }

    fn writeBracedBody(self: *Writer, stream: anytype, body: []const Zir.Inst.Index) !void {
        try self.writeBracedBodyConditional(stream, body, self.recurse_blocks);
    }

    fn writeBracedBodyConditional(self: *Writer, stream: anytype, body: []const Zir.Inst.Index, enabled: bool) !void {
        if (body.len == 0) {
            try stream.writeAll("{}");
        } else if (enabled) {
            try stream.writeAll("{\n");
            self.indent += 2;
            try self.writeBody(stream, body);
            self.indent -= 2;
            try stream.writeByteNTimes(' ', self.indent);
            try stream.writeAll("}");
        } else if (body.len == 1) {
            try stream.writeByte('{');
            try self.writeInstIndex(stream, body[0]);
            try stream.writeByte('}');
        } else if (body.len == 2) {
            try stream.writeByte('{');
            try self.writeInstIndex(stream, body[0]);
            try stream.writeAll(", ");
            try self.writeInstIndex(stream, body[1]);
            try stream.writeByte('}');
        } else {
            try stream.writeByte('{');
            try self.writeInstIndex(stream, body[0]);
            try stream.writeAll("..");
            try self.writeInstIndex(stream, body[body.len - 1]);
            try stream.writeByte('}');
        }
    }

    fn writeBody(self: *Writer, stream: anytype, body: []const Zir.Inst.Index) !void {
        for (body) |inst| {
            try stream.writeByteNTimes(' ', self.indent);
            try stream.print("%{d} ", .{@intFromEnum(inst)});
            try self.writeInstToStream(stream, inst);
            try stream.writeByte('\n');
        }
    }
};

fn testZir(source: [:0]const u8, expected: []const u8) !void {
    inline for (@typeInfo(Ast.Mode).@"enum".fields) |field| {
        try testZirMode(@enumFromInt(field.value), source, expected);
    }
    // try noFailZir(source);
}

fn testZirMode(mode: Ast.Mode, source: [:0]const u8, expected: []const u8) !void {
    const gpa = std.testing.allocator;

    var tree = try Ast.parse(gpa, source, .{
        .mode = mode,
        .version = .@"4.0",
    });
    defer tree.deinit(gpa);

    var doc_scope: DocumentScope = .{};
    defer doc_scope.deinit(gpa);
    var context: DocumentScope.ScopeContext = .{
        .gpa = gpa,
        .tree = tree,
        .doc_scope = &doc_scope,
    };
    defer context.deinit();

    var zir = try AstGen.generate(gpa, &context);
    defer zir.deinit(gpa);

    // Errors and TODO: warnings
    if (zir.hasCompileErrors()) {
        std.debug.print("error\n", .{});
        try kdb.printZirErrorsToStderr(gpa, tree, zir, "test", .auto);
        return error.Unexpected;
    }

    var file: kdb.File = .{
        .sub_file_path = "test",
        .source = source,
        .tree = tree,
        .zir = zir,
    };
    var output = std.ArrayList(u8).init(gpa);
    defer output.deinit();
    try renderAsText(gpa, &file, &output);

    try std.testing.expectEqualStrings(expected, output.items[0 .. output.items.len - 1]);
    try std.testing.expectEqual('\n', output.getLast());
}

fn warnZir(source: [:0]const u8, expected: []const u8) !void {
    inline for (@typeInfo(Ast.Mode).@"enum".fields) |field| {
        try warnZirMode(@enumFromInt(field.value), source, expected);
    }
    try noFailZir(source);
}

fn warnZirMode(mode: Ast.Mode, source: [:0]const u8, expected: []const u8) !void {
    const gpa = std.testing.allocator;

    var tree = try Ast.parse(gpa, source, .{
        .mode = mode,
        .version = .@"4.0",
    });
    defer tree.deinit(gpa);

    var doc_scope: DocumentScope = .{};
    defer doc_scope.deinit(gpa);
    var context: DocumentScope.ScopeContext = .{
        .gpa = gpa,
        .tree = tree,
        .doc_scope = &doc_scope,
    };
    defer context.deinit();

    var zir = try AstGen.generate(gpa, &context);
    defer zir.deinit(gpa);

    // Errors
    if (zir.hasCompileErrors()) {
        std.debug.print("error\n", .{});
        try kdb.printZirErrorsToStderr(gpa, tree, zir, "test", .auto);
        return error.Unexpected;
    }

    try std.testing.expect(zir.hasCompileWarnings());

    var wip_errors: kdb.ErrorBundle.Wip = undefined;
    try wip_errors.init(gpa);
    defer wip_errors.deinit();

    try wip_errors.addZirErrorMessages(zir, tree, source, "test");

    var error_bundle = try wip_errors.toOwnedBundle("");
    defer error_bundle.deinit(gpa);

    var output = std.ArrayList(u8).init(gpa);
    defer output.deinit();
    try error_bundle.renderToWriter(.{ .ttyconf = .no_color }, output.writer());

    var file: kdb.File = .{
        .sub_file_path = "test",
        .source = source,
        .tree = tree,
        .zir = zir,
    };
    try renderAsText(gpa, &file, &output);

    try std.testing.expectEqualStrings(expected, output.items[0 .. output.items.len - 1]);
    try std.testing.expectEqual('\n', output.getLast());
}

fn failZir(source: [:0]const u8, expected: []const u8) !void {
    inline for (@typeInfo(Ast.Mode).@"enum".fields) |field| {
        try failZirMode(@enumFromInt(field.value), source, expected);
    }
    try noFailZir(source);
}

fn failZirMode(mode: Ast.Mode, source: [:0]const u8, expected: []const u8) !void {
    const gpa = std.testing.allocator;

    var tree = try Ast.parse(gpa, source, .{
        .mode = mode,
        .version = .@"4.0",
    });
    defer tree.deinit(gpa);

    var doc_scope: DocumentScope = .{};
    defer doc_scope.deinit(gpa);
    var context: DocumentScope.ScopeContext = .{
        .gpa = gpa,
        .tree = tree,
        .doc_scope = &doc_scope,
    };
    defer context.deinit();

    var zir = try AstGen.generate(gpa, &context);
    defer zir.deinit(gpa);

    try std.testing.expect(zir.hasCompileErrors());

    var wip_errors: kdb.ErrorBundle.Wip = undefined;
    try wip_errors.init(gpa);
    defer wip_errors.deinit();

    try wip_errors.addZirErrorMessages(zir, tree, source, "test");

    var error_bundle = try wip_errors.toOwnedBundle("");
    defer error_bundle.deinit(gpa);

    var output = std.ArrayList(u8).init(gpa);
    defer output.deinit();
    try error_bundle.renderToWriter(.{ .ttyconf = .no_color }, output.writer());

    try std.testing.expectEqualStrings(expected, output.items[0 .. output.items.len - 1]);
    try std.testing.expectEqual('\n', output.getLast());
}

fn noFailZir(source: [:0]const u8) !void {
    inline for (@typeInfo(Ast.Mode).@"enum".fields) |mode_field| {
        inline for (@typeInfo(Ast.Version).@"enum".fields) |version_field| {
            try noFailZirModeVersion(
                @enumFromInt(mode_field.value),
                @enumFromInt(version_field.value),
                source,
            );
        }
    }
}

fn noFailZirModeVersion(mode: Ast.Mode, version: Ast.Version, source: [:0]const u8) !void {
    const gpa = std.testing.allocator;

    const settings: Ast.ParseSettings = .{ .mode = mode, .version = version };

    for (0..source.len) |i| {
        const src = try gpa.dupeZ(u8, source[0..i]);
        defer gpa.free(src);

        var tree = try Ast.parse(gpa, src, settings);
        defer tree.deinit(gpa);

        var doc_scope: DocumentScope = .{};
        defer doc_scope.deinit(gpa);
        var context: DocumentScope.ScopeContext = .{
            .gpa = gpa,
            .tree = tree,
            .doc_scope = &doc_scope,
        };
        defer context.deinit();

        var zir = try AstGen.generate(gpa, &context);
        defer zir.deinit(gpa);
    }

    for (0..source.len) |i| {
        const src = try gpa.allocSentinel(u8, source.len - 1, 0);
        defer gpa.free(src);

        @memcpy(src[0..i], source[0..i]);
        @memcpy(src[i..], source[i + 1 ..]);

        var tree = try Ast.parse(gpa, src, settings);
        defer tree.deinit(gpa);

        var doc_scope: DocumentScope = .{};
        defer doc_scope.deinit(gpa);
        var context: DocumentScope.ScopeContext = .{
            .gpa = gpa,
            .tree = tree,
            .doc_scope = &doc_scope,
        };
        defer context.deinit();

        var zir = try AstGen.generate(gpa, &context);
        defer zir.deinit(gpa);
    }
}

fn testPropertiesUpheld(_: void, source: []const u8) anyerror!void {
    const gpa = std.testing.allocator;

    const source0 = try std.testing.allocator.dupeZ(u8, source);
    defer gpa.free(source0);

    inline for (@typeInfo(Ast.Mode).@"enum".fields) |mode_field| {
        inline for (@typeInfo(Ast.Version).@"enum".fields) |version_field| {
            var tree = try Ast.parse(gpa, source0, .{
                .mode = @enumFromInt(mode_field.value),
                .version = @enumFromInt(version_field.value),
            });
            defer tree.deinit(gpa);

            var doc_scope: DocumentScope = .{};
            defer doc_scope.deinit(gpa);
            var context: DocumentScope.ScopeContext = .{
                .gpa = gpa,
                .tree = tree,
                .doc_scope = &doc_scope,
            };
            defer context.deinit();

            var zir = try AstGen.generate(gpa, &context);
            defer zir.deinit(gpa);
        }
    }
}

test "fuzzable properties upheld" {
    return std.testing.fuzz({}, testPropertiesUpheld, .{
        .corpus = &.{
            "{[]x:}",
        },
    });
}

test "print" {
    try testZir("1",
        \\%0 = file({
        \\  %1 = print(@one)
        \\})
    );
    try testZir("a:1",
        \\%0 = file({
        \\  %1 = identifier("a") token_offset:1:1 to :1:2
        \\  %2 = apply(@assign, %1, @one) node_offset:1:1 to :1:4
        \\})
    );
    try testZir("2",
        \\%0 = file({
        \\  %1 = long(2)
        \\  %2 = print(%1)
        \\})
    );
    try testZir("a:2",
        \\%0 = file({
        \\  %1 = long(2)
        \\  %2 = identifier("a") token_offset:1:1 to :1:2
        \\  %3 = apply(@assign, %2, %1) node_offset:1:1 to :1:4
        \\})
    );
}

test "discard" {
    try testZir("1;",
        \\%0 = file({
        \\  %1 = discard(@one)
        \\})
    );
    try testZir("a:1;",
        \\%0 = file({
        \\  %1 = identifier("a") token_offset:1:1 to :1:2
        \\  %2 = apply(@assign, %1, @one) node_offset:1:1 to :1:4
        \\})
    );
    try testZir("2;",
        \\%0 = file({
        \\  %1 = long(2)
        \\})
    );
    try testZir("a:2;",
        \\%0 = file({
        \\  %1 = long(2)
        \\  %2 = identifier("a") token_offset:1:1 to :1:2
        \\  %3 = apply(@assign, %2, %1) node_offset:1:1 to :1:4
        \\})
    );
}

test "empty" {
    try testZir("{[x;y]x+y}[1;]",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = param_node("y") node_offset:1:5 to :1:6
        \\    %4 = apply(@add, %2, %3) node_offset:1:7 to :1:10
        \\    %5 = ret_node(%4) node_offset:1:7 to :1:10
        \\  }) (lbrace=1:1,rbrace=1:10) node_offset:1:1 to :1:11
        \\  %6 = apply(%1, @one, @null) node_offset:1:1 to :1:15
        \\  %7 = print(%6)
        \\})
    );
}

test "grouped expression" {
    try testZir("3*4+5",
        \\%0 = file({
        \\  %1 = long(5)
        \\  %2 = long(4)
        \\  %3 = apply(@add, %2, %1) node_offset:1:3 to :1:6
        \\  %4 = long(3)
        \\  %5 = apply(@multiply, %4, %3) node_offset:1:1 to :1:6
        \\  %6 = print(%5)
        \\})
    );
    try testZir("(3*4)+5",
        \\%0 = file({
        \\  %1 = long(5)
        \\  %2 = long(4)
        \\  %3 = long(3)
        \\  %4 = apply(@multiply, %3, %2) node_offset:1:2 to :1:5
        \\  %5 = apply(@add, %4, %1) node_offset:1:1 to :1:8
        \\  %6 = print(%5)
        \\})
    );
}

test "empty list" {
    try testZir("()",
        \\%0 = file({
        \\  %1 = print(@empty_list)
        \\})
    );
    try testZir("a:()",
        \\%0 = file({
        \\  %1 = identifier("a") token_offset:1:1 to :1:2
        \\  %2 = apply(@assign, %1, @empty_list) node_offset:1:1 to :1:5
        \\})
    );
    try testZir("{[]()}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = ret_node(@empty_list) node_offset:1:4 to :1:6
        \\  }) (lbrace=1:1,rbrace=1:6) node_offset:1:1 to :1:7
        \\  %3 = print(%1)
        \\})
    );
}

test "list" {
    try testZir("(1;2;3;4;5)",
        \\%0 = file({
        \\  %1 = long(5)
        \\  %2 = long(4)
        \\  %3 = long(3)
        \\  %4 = long(2)
        \\  %5 = list(@one, %4, %3, %2, %1) node_offset:1:1 to :1:12
        \\  %6 = print(%5)
        \\})
    );
    try testZir("(a;b;c)",
        \\%0 = file({
        \\  %1 = identifier("c") token_offset:1:6 to :1:7
        \\  %2 = identifier("b") token_offset:1:4 to :1:5
        \\  %3 = identifier("a") token_offset:1:2 to :1:3
        \\  %4 = list(%3, %2, %1) node_offset:1:1 to :1:8
        \\  %5 = print(%4)
        \\})
    );
}

test "table literal" {
    try testZir("([]())",
        \\%0 = file({
        \\  %1 = table("" = @empty_list) node_offset:1:1 to :1:7
        \\  %2 = print(%1)
        \\})
    );
    try testZir("([]a:1 1 1)",
        \\%0 = file({
        \\  %1 = long_list(@one, @one, @one) node_offset:1:6 to :1:11
        \\  %2 = table("a" = %1) node_offset:1:1 to :1:12
        \\  %3 = print(%2)
        \\})
    );
    try testZir("([](a):1 1 1)",
        \\%0 = file({
        \\  %1 = long_list(@one, @one, @one) node_offset:1:8 to :1:13
        \\  %2 = table("a" = %1) node_offset:1:1 to :1:14
        \\  %3 = print(%2)
        \\})
    );
    try testZir("([]:[a;1 1 1])",
        \\%0 = file({
        \\  %1 = long_list(@one, @one, @one) node_offset:1:8 to :1:13
        \\  %2 = table("a" = %1) node_offset:1:1 to :1:15
        \\  %3 = print(%2)
        \\})
    );
    try testZir("([]:[(a);1 1 1])",
        \\%0 = file({
        \\  %1 = long_list(@one, @one, @one) node_offset:1:10 to :1:15
        \\  %2 = table("a" = %1) node_offset:1:1 to :1:17
        \\  %3 = print(%2)
        \\})
    );
    try testZir("([](:)[a;1 1 1])",
        \\%0 = file({
        \\  %1 = long_list(@one, @one, @one) node_offset:1:10 to :1:15
        \\  %2 = table("a" = %1) node_offset:1:1 to :1:17
        \\  %3 = print(%2)
        \\})
    );
    try testZir("([](:)[(a);1 1 1])",
        \\%0 = file({
        \\  %1 = long_list(@one, @one, @one) node_offset:1:12 to :1:17
        \\  %2 = table("a" = %1) node_offset:1:1 to :1:19
        \\  %3 = print(%2)
        \\})
    );
    try testZir("([]a:1 1 1;b:1 1 1)",
        \\%0 = file({
        \\  %1 = long_list(@one, @one, @one) node_offset:1:14 to :1:19
        \\  %2 = long_list(@one, @one, @one) node_offset:1:6 to :1:11
        \\  %3 = table("a" = %2, "b" = %1) node_offset:1:1 to :1:20
        \\  %4 = print(%3)
        \\})
    );
    try testZir("([](a):1 1 1;(b):1 1 1)",
        \\%0 = file({
        \\  %1 = long_list(@one, @one, @one) node_offset:1:18 to :1:23
        \\  %2 = long_list(@one, @one, @one) node_offset:1:8 to :1:13
        \\  %3 = table("a" = %2, "b" = %1) node_offset:1:1 to :1:24
        \\  %4 = print(%3)
        \\})
    );
    try testZir("([]:[a;1 1 1];:[b;1 1 1])",
        \\%0 = file({
        \\  %1 = long_list(@one, @one, @one) node_offset:1:19 to :1:24
        \\  %2 = long_list(@one, @one, @one) node_offset:1:8 to :1:13
        \\  %3 = table("a" = %2, "b" = %1) node_offset:1:1 to :1:26
        \\  %4 = print(%3)
        \\})
    );
    try testZir("([]:[(a);1 1 1];:[(b);1 1 1])",
        \\%0 = file({
        \\  %1 = long_list(@one, @one, @one) node_offset:1:23 to :1:28
        \\  %2 = long_list(@one, @one, @one) node_offset:1:10 to :1:15
        \\  %3 = table("a" = %2, "b" = %1) node_offset:1:1 to :1:30
        \\  %4 = print(%3)
        \\})
    );
    try testZir("([](:)[a;1 1 1];(:)[b;1 1 1])",
        \\%0 = file({
        \\  %1 = long_list(@one, @one, @one) node_offset:1:23 to :1:28
        \\  %2 = long_list(@one, @one, @one) node_offset:1:10 to :1:15
        \\  %3 = table("a" = %2, "b" = %1) node_offset:1:1 to :1:30
        \\  %4 = print(%3)
        \\})
    );
    try testZir("([](:)[(a);1 1 1];(:)[(b);1 1 1])",
        \\%0 = file({
        \\  %1 = long_list(@one, @one, @one) node_offset:1:27 to :1:32
        \\  %2 = long_list(@one, @one, @one) node_offset:1:12 to :1:17
        \\  %3 = table("a" = %2, "b" = %1) node_offset:1:1 to :1:34
        \\  %4 = print(%3)
        \\})
    );
    try testZir("{[]([]a:b;b:b:1 1 1)}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = long_list(@one, @one, @one) node_offset:1:15 to :1:20
        \\    %3 = identifier("b") token_offset:1:13 to :1:14
        \\    %4 = apply(@assign, %3, %2) node_offset:1:13 to :1:20
        \\    %5 = table("a" = %3, "b" = %4) node_offset:1:4 to :1:21
        \\    %6 = ret_node(%5) node_offset:1:4 to :1:21
        \\  }) (lbrace=1:1,rbrace=1:21) node_offset:1:1 to :1:22
        \\  %7 = print(%1)
        \\})
    );
    try testZir("{[]([](a):b;(b):b:1 1 1)}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = long_list(@one, @one, @one) node_offset:1:19 to :1:24
        \\    %3 = identifier("b") token_offset:1:17 to :1:18
        \\    %4 = apply(@assign, %3, %2) node_offset:1:17 to :1:24
        \\    %5 = table("a" = %3, "b" = %4) node_offset:1:4 to :1:25
        \\    %6 = ret_node(%5) node_offset:1:4 to :1:25
        \\  }) (lbrace=1:1,rbrace=1:25) node_offset:1:1 to :1:26
        \\  %7 = print(%1)
        \\})
    );
    try testZir("{[]([]:[a;b];:[b;b:1 1 1])}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = long_list(@one, @one, @one) node_offset:1:20 to :1:25
        \\    %3 = identifier("b") token_offset:1:18 to :1:19
        \\    %4 = apply(@assign, %3, %2) node_offset:1:18 to :1:25
        \\    %5 = table("a" = %3, "b" = %4) node_offset:1:4 to :1:27
        \\    %6 = ret_node(%5) node_offset:1:4 to :1:27
        \\  }) (lbrace=1:1,rbrace=1:27) node_offset:1:1 to :1:28
        \\  %7 = print(%1)
        \\})
    );
    try testZir("{[]([]:[(a);b];:[(b);b:1 1 1])}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = long_list(@one, @one, @one) node_offset:1:24 to :1:29
        \\    %3 = identifier("b") token_offset:1:22 to :1:23
        \\    %4 = apply(@assign, %3, %2) node_offset:1:22 to :1:29
        \\    %5 = table("a" = %3, "b" = %4) node_offset:1:4 to :1:31
        \\    %6 = ret_node(%5) node_offset:1:4 to :1:31
        \\  }) (lbrace=1:1,rbrace=1:31) node_offset:1:1 to :1:32
        \\  %7 = print(%1)
        \\})
    );
    try testZir("{[]([](:)[a;b];(:)[b;b:1 1 1])}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = long_list(@one, @one, @one) node_offset:1:24 to :1:29
        \\    %3 = identifier("b") token_offset:1:22 to :1:23
        \\    %4 = apply(@assign, %3, %2) node_offset:1:22 to :1:29
        \\    %5 = table("a" = %3, "b" = %4) node_offset:1:4 to :1:31
        \\    %6 = ret_node(%5) node_offset:1:4 to :1:31
        \\  }) (lbrace=1:1,rbrace=1:31) node_offset:1:1 to :1:32
        \\  %7 = print(%1)
        \\})
    );
    try testZir("{[]([](:)[(a);b];(:)[(b);b:1 1 1])}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = long_list(@one, @one, @one) node_offset:1:28 to :1:33
        \\    %3 = identifier("b") token_offset:1:26 to :1:27
        \\    %4 = apply(@assign, %3, %2) node_offset:1:26 to :1:33
        \\    %5 = table("a" = %3, "b" = %4) node_offset:1:4 to :1:35
        \\    %6 = ret_node(%5) node_offset:1:4 to :1:35
        \\  }) (lbrace=1:1,rbrace=1:35) node_offset:1:1 to :1:36
        \\  %7 = print(%1)
        \\})
    );
    try testZir("{[]([]b;b:b:1 1 1)}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = long_list(@one, @one, @one) node_offset:1:13 to :1:18
        \\    %3 = identifier("b") token_offset:1:11 to :1:12
        \\    %4 = apply(@assign, %3, %2) node_offset:1:11 to :1:18
        \\    %5 = table("" = %3, "b" = %4) node_offset:1:4 to :1:19
        \\    %6 = ret_node(%5) node_offset:1:4 to :1:19
        \\  }) (lbrace=1:1,rbrace=1:19) node_offset:1:1 to :1:20
        \\  %7 = print(%1)
        \\})
    );
    try testZir("{[]([]b;(b):b:1 1 1)}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = long_list(@one, @one, @one) node_offset:1:15 to :1:20
        \\    %3 = identifier("b") token_offset:1:13 to :1:14
        \\    %4 = apply(@assign, %3, %2) node_offset:1:13 to :1:20
        \\    %5 = table("" = %3, "b" = %4) node_offset:1:4 to :1:21
        \\    %6 = ret_node(%5) node_offset:1:4 to :1:21
        \\  }) (lbrace=1:1,rbrace=1:21) node_offset:1:1 to :1:22
        \\  %7 = print(%1)
        \\})
    );
    try testZir("{[]([]b;:[b;b:1 1 1])}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = long_list(@one, @one, @one) node_offset:1:15 to :1:20
        \\    %3 = identifier("b") token_offset:1:13 to :1:14
        \\    %4 = apply(@assign, %3, %2) node_offset:1:13 to :1:20
        \\    %5 = table("" = %3, "b" = %4) node_offset:1:4 to :1:22
        \\    %6 = ret_node(%5) node_offset:1:4 to :1:22
        \\  }) (lbrace=1:1,rbrace=1:22) node_offset:1:1 to :1:23
        \\  %7 = print(%1)
        \\})
    );
    try testZir("{[]([]b;:[(b);b:1 1 1])}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = long_list(@one, @one, @one) node_offset:1:17 to :1:22
        \\    %3 = identifier("b") token_offset:1:15 to :1:16
        \\    %4 = apply(@assign, %3, %2) node_offset:1:15 to :1:22
        \\    %5 = table("" = %3, "b" = %4) node_offset:1:4 to :1:24
        \\    %6 = ret_node(%5) node_offset:1:4 to :1:24
        \\  }) (lbrace=1:1,rbrace=1:24) node_offset:1:1 to :1:25
        \\  %7 = print(%1)
        \\})
    );
    try testZir("{[]([]b;(:)[b;b:1 1 1])}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = long_list(@one, @one, @one) node_offset:1:17 to :1:22
        \\    %3 = identifier("b") token_offset:1:15 to :1:16
        \\    %4 = apply(@assign, %3, %2) node_offset:1:15 to :1:22
        \\    %5 = table("" = %3, "b" = %4) node_offset:1:4 to :1:24
        \\    %6 = ret_node(%5) node_offset:1:4 to :1:24
        \\  }) (lbrace=1:1,rbrace=1:24) node_offset:1:1 to :1:25
        \\  %7 = print(%1)
        \\})
    );
    try testZir("{[]([]b;(:)[(b);b:1 1 1])}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = long_list(@one, @one, @one) node_offset:1:19 to :1:24
        \\    %3 = identifier("b") token_offset:1:17 to :1:18
        \\    %4 = apply(@assign, %3, %2) node_offset:1:17 to :1:24
        \\    %5 = table("" = %3, "b" = %4) node_offset:1:4 to :1:26
        \\    %6 = ret_node(%5) node_offset:1:4 to :1:26
        \\  }) (lbrace=1:1,rbrace=1:26) node_offset:1:1 to :1:27
        \\  %7 = print(%1)
        \\})
    );
    try testZir("{[]([]a:b;b:1 1 1)}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = long_list(@one, @one, @one) node_offset:1:13 to :1:18
        \\    %3 = identifier("b") token_offset:1:9 to :1:10
        \\    %4 = table("a" = %3, "b" = %2) node_offset:1:4 to :1:19
        \\    %5 = ret_node(%4) node_offset:1:4 to :1:19
        \\  }) (lbrace=1:1,rbrace=1:19) node_offset:1:1 to :1:20
        \\  %6 = print(%1)
        \\})
    );
    try testZir("{[]([](a):b;(b):1 1 1)}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = long_list(@one, @one, @one) node_offset:1:17 to :1:22
        \\    %3 = identifier("b") token_offset:1:11 to :1:12
        \\    %4 = table("a" = %3, "b" = %2) node_offset:1:4 to :1:23
        \\    %5 = ret_node(%4) node_offset:1:4 to :1:23
        \\  }) (lbrace=1:1,rbrace=1:23) node_offset:1:1 to :1:24
        \\  %6 = print(%1)
        \\})
    );
    try testZir("{[]([]:[a;b];:[b;1 1 1])}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = long_list(@one, @one, @one) node_offset:1:18 to :1:23
        \\    %3 = identifier("b") token_offset:1:11 to :1:12
        \\    %4 = table("a" = %3, "b" = %2) node_offset:1:4 to :1:25
        \\    %5 = ret_node(%4) node_offset:1:4 to :1:25
        \\  }) (lbrace=1:1,rbrace=1:25) node_offset:1:1 to :1:26
        \\  %6 = print(%1)
        \\})
    );
    try testZir("{[]([]:[(a);b];:[(b);1 1 1])}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = long_list(@one, @one, @one) node_offset:1:22 to :1:27
        \\    %3 = identifier("b") token_offset:1:13 to :1:14
        \\    %4 = table("a" = %3, "b" = %2) node_offset:1:4 to :1:29
        \\    %5 = ret_node(%4) node_offset:1:4 to :1:29
        \\  }) (lbrace=1:1,rbrace=1:29) node_offset:1:1 to :1:30
        \\  %6 = print(%1)
        \\})
    );
    try testZir("{[]([](:)[a;b];(:)[b;1 1 1])}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = long_list(@one, @one, @one) node_offset:1:22 to :1:27
        \\    %3 = identifier("b") token_offset:1:13 to :1:14
        \\    %4 = table("a" = %3, "b" = %2) node_offset:1:4 to :1:29
        \\    %5 = ret_node(%4) node_offset:1:4 to :1:29
        \\  }) (lbrace=1:1,rbrace=1:29) node_offset:1:1 to :1:30
        \\  %6 = print(%1)
        \\})
    );
    try testZir("{[]([](:)[(a);b];(:)[(b);1 1 1])}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = long_list(@one, @one, @one) node_offset:1:26 to :1:31
        \\    %3 = identifier("b") token_offset:1:15 to :1:16
        \\    %4 = table("a" = %3, "b" = %2) node_offset:1:4 to :1:33
        \\    %5 = ret_node(%4) node_offset:1:4 to :1:33
        \\  }) (lbrace=1:1,rbrace=1:33) node_offset:1:1 to :1:34
        \\  %6 = print(%1)
        \\})
    );
    try failZir("{[]([]a:a:1 1 1;a)}",
        \\test:1:17: error: use of undeclared identifier 'a'
        \\{[]([]a:a:1 1 1;a)}
        \\                ^
        \\test:1:9: note: identifier declared here
        \\{[]([]a:a:1 1 1;a)}
        \\        ^
    );
    try failZir("{[]([](a):a:1 1 1;a)}",
        \\test:1:19: error: use of undeclared identifier 'a'
        \\{[]([](a):a:1 1 1;a)}
        \\                  ^
        \\test:1:11: note: identifier declared here
        \\{[]([](a):a:1 1 1;a)}
        \\          ^
    );
    try failZir("{[]([]:[a;a:1 1 1];a)}",
        \\test:1:20: error: use of undeclared identifier 'a'
        \\{[]([]:[a;a:1 1 1];a)}
        \\                   ^
        \\test:1:11: note: identifier declared here
        \\{[]([]:[a;a:1 1 1];a)}
        \\          ^
    );
    try failZir("{[]([]:[(a);a:1 1 1];a)}",
        \\test:1:22: error: use of undeclared identifier 'a'
        \\{[]([]:[(a);a:1 1 1];a)}
        \\                     ^
        \\test:1:13: note: identifier declared here
        \\{[]([]:[(a);a:1 1 1];a)}
        \\            ^
    );
    try failZir("{[]([](:)[a;a:1 1 1];a)}",
        \\test:1:22: error: use of undeclared identifier 'a'
        \\{[]([](:)[a;a:1 1 1];a)}
        \\                     ^
        \\test:1:13: note: identifier declared here
        \\{[]([](:)[a;a:1 1 1];a)}
        \\            ^
    );
    try failZir("{[]([](:)[(a);a:1 1 1];a)}",
        \\test:1:24: error: use of undeclared identifier 'a'
        \\{[]([](:)[(a);a:1 1 1];a)}
        \\                       ^
        \\test:1:15: note: identifier declared here
        \\{[]([](:)[(a);a:1 1 1];a)}
        \\              ^
    );

    try failZir("([()])",
        \\test:1:6: error: expected expression, found ')'
        \\([()])
        \\     ^
    );
    try testZir("([()]())",
        \\%0 = file({
        \\  %1 = table(keys={"" = @empty_list}, "" = @empty_list) node_offset:1:1 to :1:9
        \\  %2 = print(%1)
        \\})
    );
    try testZir("([a:1 1 1]b:1 1 1)",
        \\%0 = file({
        \\  %1 = long_list(@one, @one, @one) node_offset:1:13 to :1:18
        \\  %2 = long_list(@one, @one, @one) node_offset:1:5 to :1:10
        \\  %3 = table(keys={"a" = %2}, "b" = %1) node_offset:1:1 to :1:19
        \\  %4 = print(%3)
        \\})
    );
    try testZir("([(a):1 1 1](b):1 1 1)",
        \\%0 = file({
        \\  %1 = long_list(@one, @one, @one) node_offset:1:17 to :1:22
        \\  %2 = long_list(@one, @one, @one) node_offset:1:7 to :1:12
        \\  %3 = table(keys={"a" = %2}, "b" = %1) node_offset:1:1 to :1:23
        \\  %4 = print(%3)
        \\})
    );
    try testZir("([:[a;1 1 1]]:[b;1 1 1])",
        \\%0 = file({
        \\  %1 = long_list(@one, @one, @one) node_offset:1:18 to :1:23
        \\  %2 = long_list(@one, @one, @one) node_offset:1:7 to :1:12
        \\  %3 = table(keys={"a" = %2}, "b" = %1) node_offset:1:1 to :1:25
        \\  %4 = print(%3)
        \\})
    );
    try testZir("([:[(a);1 1 1]]:[(b);1 1 1])",
        \\%0 = file({
        \\  %1 = long_list(@one, @one, @one) node_offset:1:22 to :1:27
        \\  %2 = long_list(@one, @one, @one) node_offset:1:9 to :1:14
        \\  %3 = table(keys={"a" = %2}, "b" = %1) node_offset:1:1 to :1:29
        \\  %4 = print(%3)
        \\})
    );
    try testZir("([(:)[a;1 1 1]](:)[b;1 1 1])",
        \\%0 = file({
        \\  %1 = long_list(@one, @one, @one) node_offset:1:22 to :1:27
        \\  %2 = long_list(@one, @one, @one) node_offset:1:9 to :1:14
        \\  %3 = table(keys={"a" = %2}, "b" = %1) node_offset:1:1 to :1:29
        \\  %4 = print(%3)
        \\})
    );
    try testZir("([(:)[(a);1 1 1]](:)[(b);1 1 1])",
        \\%0 = file({
        \\  %1 = long_list(@one, @one, @one) node_offset:1:26 to :1:31
        \\  %2 = long_list(@one, @one, @one) node_offset:1:11 to :1:16
        \\  %3 = table(keys={"a" = %2}, "b" = %1) node_offset:1:1 to :1:33
        \\  %4 = print(%3)
        \\})
    );
    try testZir("{[]([b]b:b:1 1 1)}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = long_list(@one, @one, @one) node_offset:1:12 to :1:17
        \\    %3 = identifier("b") token_offset:1:10 to :1:11
        \\    %4 = apply(@assign, %3, %2) node_offset:1:10 to :1:17
        \\    %5 = table(keys={"" = %3}, "b" = %4) node_offset:1:4 to :1:18
        \\    %6 = ret_node(%5) node_offset:1:4 to :1:18
        \\  }) (lbrace=1:1,rbrace=1:18) node_offset:1:1 to :1:19
        \\  %7 = print(%1)
        \\})
    );
    try testZir("{[]([b](b):b:1 1 1)}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = long_list(@one, @one, @one) node_offset:1:14 to :1:19
        \\    %3 = identifier("b") token_offset:1:12 to :1:13
        \\    %4 = apply(@assign, %3, %2) node_offset:1:12 to :1:19
        \\    %5 = table(keys={"" = %3}, "b" = %4) node_offset:1:4 to :1:20
        \\    %6 = ret_node(%5) node_offset:1:4 to :1:20
        \\  }) (lbrace=1:1,rbrace=1:20) node_offset:1:1 to :1:21
        \\  %7 = print(%1)
        \\})
    );
    try testZir("{[]([b]:[b;b:1 1 1])}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = long_list(@one, @one, @one) node_offset:1:14 to :1:19
        \\    %3 = identifier("b") token_offset:1:12 to :1:13
        \\    %4 = apply(@assign, %3, %2) node_offset:1:12 to :1:19
        \\    %5 = table(keys={"" = %3}, "b" = %4) node_offset:1:4 to :1:21
        \\    %6 = ret_node(%5) node_offset:1:4 to :1:21
        \\  }) (lbrace=1:1,rbrace=1:21) node_offset:1:1 to :1:22
        \\  %7 = print(%1)
        \\})
    );
    try testZir("{[]([b]:[(b);b:1 1 1])}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = long_list(@one, @one, @one) node_offset:1:16 to :1:21
        \\    %3 = identifier("b") token_offset:1:14 to :1:15
        \\    %4 = apply(@assign, %3, %2) node_offset:1:14 to :1:21
        \\    %5 = table(keys={"" = %3}, "b" = %4) node_offset:1:4 to :1:23
        \\    %6 = ret_node(%5) node_offset:1:4 to :1:23
        \\  }) (lbrace=1:1,rbrace=1:23) node_offset:1:1 to :1:24
        \\  %7 = print(%1)
        \\})
    );
    try testZir("{[]([b](:)[b;b:1 1 1])}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = long_list(@one, @one, @one) node_offset:1:16 to :1:21
        \\    %3 = identifier("b") token_offset:1:14 to :1:15
        \\    %4 = apply(@assign, %3, %2) node_offset:1:14 to :1:21
        \\    %5 = table(keys={"" = %3}, "b" = %4) node_offset:1:4 to :1:23
        \\    %6 = ret_node(%5) node_offset:1:4 to :1:23
        \\  }) (lbrace=1:1,rbrace=1:23) node_offset:1:1 to :1:24
        \\  %7 = print(%1)
        \\})
    );
    try testZir("{[]([b](:)[(b);b:1 1 1])}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = long_list(@one, @one, @one) node_offset:1:18 to :1:23
        \\    %3 = identifier("b") token_offset:1:16 to :1:17
        \\    %4 = apply(@assign, %3, %2) node_offset:1:16 to :1:23
        \\    %5 = table(keys={"" = %3}, "b" = %4) node_offset:1:4 to :1:25
        \\    %6 = ret_node(%5) node_offset:1:4 to :1:25
        \\  }) (lbrace=1:1,rbrace=1:25) node_offset:1:1 to :1:26
        \\  %7 = print(%1)
        \\})
    );
    try testZir("{[]([b;b]b:b:1 1 1)}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = long_list(@one, @one, @one) node_offset:1:14 to :1:19
        \\    %3 = identifier("b") token_offset:1:12 to :1:13
        \\    %4 = apply(@assign, %3, %2) node_offset:1:12 to :1:19
        \\    %5 = table(keys={"" = %3, "" = %3}, "b" = %4) node_offset:1:4 to :1:20
        \\    %6 = ret_node(%5) node_offset:1:4 to :1:20
        \\  }) (lbrace=1:1,rbrace=1:20) node_offset:1:1 to :1:21
        \\  %7 = print(%1)
        \\})
    );
    try testZir("{[]([b;b](b):b:1 1 1)}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = long_list(@one, @one, @one) node_offset:1:16 to :1:21
        \\    %3 = identifier("b") token_offset:1:14 to :1:15
        \\    %4 = apply(@assign, %3, %2) node_offset:1:14 to :1:21
        \\    %5 = table(keys={"" = %3, "" = %3}, "b" = %4) node_offset:1:4 to :1:22
        \\    %6 = ret_node(%5) node_offset:1:4 to :1:22
        \\  }) (lbrace=1:1,rbrace=1:22) node_offset:1:1 to :1:23
        \\  %7 = print(%1)
        \\})
    );
    try testZir("{[]([b;b]:[b;b:1 1 1])}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = long_list(@one, @one, @one) node_offset:1:16 to :1:21
        \\    %3 = identifier("b") token_offset:1:14 to :1:15
        \\    %4 = apply(@assign, %3, %2) node_offset:1:14 to :1:21
        \\    %5 = table(keys={"" = %3, "" = %3}, "b" = %4) node_offset:1:4 to :1:23
        \\    %6 = ret_node(%5) node_offset:1:4 to :1:23
        \\  }) (lbrace=1:1,rbrace=1:23) node_offset:1:1 to :1:24
        \\  %7 = print(%1)
        \\})
    );
    try testZir("{[]([b;b]:[(b);b:1 1 1])}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = long_list(@one, @one, @one) node_offset:1:18 to :1:23
        \\    %3 = identifier("b") token_offset:1:16 to :1:17
        \\    %4 = apply(@assign, %3, %2) node_offset:1:16 to :1:23
        \\    %5 = table(keys={"" = %3, "" = %3}, "b" = %4) node_offset:1:4 to :1:25
        \\    %6 = ret_node(%5) node_offset:1:4 to :1:25
        \\  }) (lbrace=1:1,rbrace=1:25) node_offset:1:1 to :1:26
        \\  %7 = print(%1)
        \\})
    );
    try testZir("{[]([b;b](:)[b;b:1 1 1])}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = long_list(@one, @one, @one) node_offset:1:18 to :1:23
        \\    %3 = identifier("b") token_offset:1:16 to :1:17
        \\    %4 = apply(@assign, %3, %2) node_offset:1:16 to :1:23
        \\    %5 = table(keys={"" = %3, "" = %3}, "b" = %4) node_offset:1:4 to :1:25
        \\    %6 = ret_node(%5) node_offset:1:4 to :1:25
        \\  }) (lbrace=1:1,rbrace=1:25) node_offset:1:1 to :1:26
        \\  %7 = print(%1)
        \\})
    );
    try testZir("{[]([b;b](:)[(b);b:1 1 1])}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = long_list(@one, @one, @one) node_offset:1:20 to :1:25
        \\    %3 = identifier("b") token_offset:1:18 to :1:19
        \\    %4 = apply(@assign, %3, %2) node_offset:1:18 to :1:25
        \\    %5 = table(keys={"" = %3, "" = %3}, "b" = %4) node_offset:1:4 to :1:27
        \\    %6 = ret_node(%5) node_offset:1:4 to :1:27
        \\  }) (lbrace=1:1,rbrace=1:27) node_offset:1:1 to :1:28
        \\  %7 = print(%1)
        \\})
    );
    try failZir("{[]([a:a:1 1 1]a)}",
        \\test:1:16: error: use of undeclared identifier 'a'
        \\{[]([a:a:1 1 1]a)}
        \\               ^
        \\test:1:8: note: identifier declared here
        \\{[]([a:a:1 1 1]a)}
        \\       ^
    );
    try failZir("{[]([(a):a:1 1 1]a)}",
        \\test:1:18: error: use of undeclared identifier 'a'
        \\{[]([(a):a:1 1 1]a)}
        \\                 ^
        \\test:1:10: note: identifier declared here
        \\{[]([(a):a:1 1 1]a)}
        \\         ^
    );
    try failZir("{[]([:[a;a:1 1 1]]a)}",
        \\test:1:19: error: use of undeclared identifier 'a'
        \\{[]([:[a;a:1 1 1]]a)}
        \\                  ^
        \\test:1:10: note: identifier declared here
        \\{[]([:[a;a:1 1 1]]a)}
        \\         ^
    );
    try failZir("{[]([:[(a);a:1 1 1]]a)}",
        \\test:1:21: error: use of undeclared identifier 'a'
        \\{[]([:[(a);a:1 1 1]]a)}
        \\                    ^
        \\test:1:12: note: identifier declared here
        \\{[]([:[(a);a:1 1 1]]a)}
        \\           ^
    );
    try failZir("{[]([(:)[a;a:1 1 1]]a)}",
        \\test:1:21: error: use of undeclared identifier 'a'
        \\{[]([(:)[a;a:1 1 1]]a)}
        \\                    ^
        \\test:1:12: note: identifier declared here
        \\{[]([(:)[a;a:1 1 1]]a)}
        \\           ^
    );
    try failZir("{[]([(:)[(a);a:1 1 1]]a)}",
        \\test:1:23: error: use of undeclared identifier 'a'
        \\{[]([(:)[(a);a:1 1 1]]a)}
        \\                      ^
        \\test:1:14: note: identifier declared here
        \\{[]([(:)[(a);a:1 1 1]]a)}
        \\             ^
    );
}

// TODO: https://kdblint.atlassian.net/browse/KLS-313
test "table literal - length check" {
    if (true) return error.SkipZigTest;
    try failZir("([a:1 1 1]())", "");
}

test "lambda" {
    try testZir("{}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = ret_implicit(@null) token_offset:1:2 to :1:3
        \\  }) (lbrace=1:1,rbrace=1:2) node_offset:1:1 to :1:3
        \\  %3 = print(%1)
        \\})
    );
    try testZir("{[]}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = ret_implicit(@null) token_offset:1:4 to :1:5
        \\  }) (lbrace=1:1,rbrace=1:4) node_offset:1:1 to :1:5
        \\  %3 = print(%1)
        \\})
    );
    try testZir("{[x]}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = ret_implicit(@null) token_offset:1:5 to :1:6
        \\  }) (lbrace=1:1,rbrace=1:5) node_offset:1:1 to :1:6
        \\  %4 = print(%1)
        \\})
    );
    try testZir("{[x;y]}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = param_node("y") node_offset:1:5 to :1:6
        \\    %4 = ret_implicit(@null) token_offset:1:7 to :1:8
        \\  }) (lbrace=1:1,rbrace=1:7) node_offset:1:1 to :1:8
        \\  %5 = print(%1)
        \\})
    );

    try testZir("{1}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = ret_node(@one) node_offset:1:2 to :1:3
        \\  }) (lbrace=1:1,rbrace=1:3) node_offset:1:1 to :1:4
        \\  %3 = print(%1)
        \\})
    );
    try testZir("{[]1}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = ret_node(@one) node_offset:1:4 to :1:5
        \\  }) (lbrace=1:1,rbrace=1:5) node_offset:1:1 to :1:6
        \\  %3 = print(%1)
        \\})
    );
    try testZir("{[x]1}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = ret_node(@one) node_offset:1:5 to :1:6
        \\  }) (lbrace=1:1,rbrace=1:6) node_offset:1:1 to :1:7
        \\  %4 = print(%1)
        \\})
    );
    try testZir("{[x;y]1}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = param_node("y") node_offset:1:5 to :1:6
        \\    %4 = ret_node(@one) node_offset:1:7 to :1:8
        \\  }) (lbrace=1:1,rbrace=1:8) node_offset:1:1 to :1:9
        \\  %5 = print(%1)
        \\})
    );

    try testZir("{2}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = long(2)
        \\    %3 = ret_node(%2) node_offset:1:2 to :1:3
        \\  }) (lbrace=1:1,rbrace=1:3) node_offset:1:1 to :1:4
        \\  %4 = print(%1)
        \\})
    );
    try testZir("{[]2}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = long(2)
        \\    %3 = ret_node(%2) node_offset:1:4 to :1:5
        \\  }) (lbrace=1:1,rbrace=1:5) node_offset:1:1 to :1:6
        \\  %4 = print(%1)
        \\})
    );
    try testZir("{[x]2}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = long(2)
        \\    %4 = ret_node(%3) node_offset:1:5 to :1:6
        \\  }) (lbrace=1:1,rbrace=1:6) node_offset:1:1 to :1:7
        \\  %5 = print(%1)
        \\})
    );
    try testZir("{[x;y]2}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = param_node("y") node_offset:1:5 to :1:6
        \\    %4 = long(2)
        \\    %5 = ret_node(%4) node_offset:1:7 to :1:8
        \\  }) (lbrace=1:1,rbrace=1:8) node_offset:1:1 to :1:9
        \\  %6 = print(%1)
        \\})
    );

    try testZir("{x}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_implicit(@x) token_offset:1:2 to :1:3
        \\    %3 = ret_node(%2) node_offset:1:2 to :1:3
        \\  }) (lbrace=1:1,rbrace=1:3) node_offset:1:1 to :1:4
        \\  %4 = print(%1)
        \\})
    );
    try testZir("{[]x}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("x") token_offset:1:4 to :1:5
        \\    %3 = ret_node(%2) node_offset:1:4 to :1:5
        \\  }) (lbrace=1:1,rbrace=1:5) node_offset:1:1 to :1:6
        \\  %4 = print(%1)
        \\})
    );
    try testZir("{[x]x}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = ret_node(%2) node_offset:1:5 to :1:6
        \\  }) (lbrace=1:1,rbrace=1:6) node_offset:1:1 to :1:7
        \\  %4 = print(%1)
        \\})
    );
    try testZir("{[x;y]x}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = param_node("y") node_offset:1:5 to :1:6
        \\    %4 = ret_node(%2) node_offset:1:7 to :1:8
        \\  }) (lbrace=1:1,rbrace=1:8) node_offset:1:1 to :1:9
        \\  %5 = print(%1)
        \\})
    );

    try testZir("{y}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_implicit(@x) token_offset:1:1 to :1:2
        \\    %3 = param_implicit(@y) token_offset:1:2 to :1:3
        \\    %4 = ret_node(%3) node_offset:1:2 to :1:3
        \\  }) (lbrace=1:1,rbrace=1:3) node_offset:1:1 to :1:4
        \\  %5 = print(%1)
        \\})
    );
    try testZir("{[]y}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("y") token_offset:1:4 to :1:5
        \\    %3 = ret_node(%2) node_offset:1:4 to :1:5
        \\  }) (lbrace=1:1,rbrace=1:5) node_offset:1:1 to :1:6
        \\  %4 = print(%1)
        \\})
    );
    try testZir("{[x]y}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = identifier("y") token_offset:1:5 to :1:6
        \\    %4 = ret_node(%3) node_offset:1:5 to :1:6
        \\  }) (lbrace=1:1,rbrace=1:6) node_offset:1:1 to :1:7
        \\  %5 = print(%1)
        \\})
    );
    try testZir("{[x;y]y}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = param_node("y") node_offset:1:5 to :1:6
        \\    %4 = ret_node(%3) node_offset:1:7 to :1:8
        \\  }) (lbrace=1:1,rbrace=1:8) node_offset:1:1 to :1:9
        \\  %5 = print(%1)
        \\})
    );

    try testZir("{z}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_implicit(@x) token_offset:1:1 to :1:2
        \\    %3 = param_implicit(@y) token_offset:1:1 to :1:2
        \\    %4 = param_implicit(@z) token_offset:1:2 to :1:3
        \\    %5 = ret_node(%4) node_offset:1:2 to :1:3
        \\  }) (lbrace=1:1,rbrace=1:3) node_offset:1:1 to :1:4
        \\  %6 = print(%1)
        \\})
    );
    try testZir("{[]z}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("z") token_offset:1:4 to :1:5
        \\    %3 = ret_node(%2) node_offset:1:4 to :1:5
        \\  }) (lbrace=1:1,rbrace=1:5) node_offset:1:1 to :1:6
        \\  %4 = print(%1)
        \\})
    );
    try testZir("{[x]z}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = identifier("z") token_offset:1:5 to :1:6
        \\    %4 = ret_node(%3) node_offset:1:5 to :1:6
        \\  }) (lbrace=1:1,rbrace=1:6) node_offset:1:1 to :1:7
        \\  %5 = print(%1)
        \\})
    );
    try testZir("{[x;y]z}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = param_node("y") node_offset:1:5 to :1:6
        \\    %4 = identifier("z") token_offset:1:7 to :1:8
        \\    %5 = ret_node(%4) node_offset:1:7 to :1:8
        \\  }) (lbrace=1:1,rbrace=1:8) node_offset:1:1 to :1:9
        \\  %6 = print(%1)
        \\})
    );
}

test "lambda semicolon" {
    try testZir("{;}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = ret_implicit(@null) token_offset:1:3 to :1:4
        \\  }) (lbrace=1:1,rbrace=1:3) node_offset:1:1 to :1:4
        \\  %3 = print(%1)
        \\})
    );
    try testZir("{[];}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = ret_implicit(@null) token_offset:1:5 to :1:6
        \\  }) (lbrace=1:1,rbrace=1:5) node_offset:1:1 to :1:6
        \\  %3 = print(%1)
        \\})
    );
    try testZir("{[x];}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = ret_implicit(@null) token_offset:1:6 to :1:7
        \\  }) (lbrace=1:1,rbrace=1:6) node_offset:1:1 to :1:7
        \\  %4 = print(%1)
        \\})
    );
    try testZir("{[x;y];}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = param_node("y") node_offset:1:5 to :1:6
        \\    %4 = ret_implicit(@null) token_offset:1:8 to :1:9
        \\  }) (lbrace=1:1,rbrace=1:8) node_offset:1:1 to :1:9
        \\  %5 = print(%1)
        \\})
    );

    try testZir("{1;}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = ret_implicit(@null) token_offset:1:4 to :1:5
        \\  }) (lbrace=1:1,rbrace=1:4) node_offset:1:1 to :1:5
        \\  %3 = print(%1)
        \\})
    );
    try testZir("{[]1;}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = ret_implicit(@null) token_offset:1:6 to :1:7
        \\  }) (lbrace=1:1,rbrace=1:6) node_offset:1:1 to :1:7
        \\  %3 = print(%1)
        \\})
    );
    try testZir("{[x]1;}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = ret_implicit(@null) token_offset:1:7 to :1:8
        \\  }) (lbrace=1:1,rbrace=1:7) node_offset:1:1 to :1:8
        \\  %4 = print(%1)
        \\})
    );
    try testZir("{[x;y]1;}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = param_node("y") node_offset:1:5 to :1:6
        \\    %4 = ret_implicit(@null) token_offset:1:9 to :1:10
        \\  }) (lbrace=1:1,rbrace=1:9) node_offset:1:1 to :1:10
        \\  %5 = print(%1)
        \\})
    );

    try testZir("{2;}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = long(2)
        \\    %3 = ret_implicit(@null) token_offset:1:4 to :1:5
        \\  }) (lbrace=1:1,rbrace=1:4) node_offset:1:1 to :1:5
        \\  %4 = print(%1)
        \\})
    );
    try testZir("{[]2;}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = long(2)
        \\    %3 = ret_implicit(@null) token_offset:1:6 to :1:7
        \\  }) (lbrace=1:1,rbrace=1:6) node_offset:1:1 to :1:7
        \\  %4 = print(%1)
        \\})
    );
    try testZir("{[x]2;}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = long(2)
        \\    %4 = ret_implicit(@null) token_offset:1:7 to :1:8
        \\  }) (lbrace=1:1,rbrace=1:7) node_offset:1:1 to :1:8
        \\  %5 = print(%1)
        \\})
    );
    try testZir("{[x;y]2;}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = param_node("y") node_offset:1:5 to :1:6
        \\    %4 = long(2)
        \\    %5 = ret_implicit(@null) token_offset:1:9 to :1:10
        \\  }) (lbrace=1:1,rbrace=1:9) node_offset:1:1 to :1:10
        \\  %6 = print(%1)
        \\})
    );

    try testZir("{x;}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_implicit(@x) token_offset:1:2 to :1:3
        \\    %3 = ret_implicit(@null) token_offset:1:4 to :1:5
        \\  }) (lbrace=1:1,rbrace=1:4) node_offset:1:1 to :1:5
        \\  %4 = print(%1)
        \\})
    );
    try testZir("{[]x;}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("x") token_offset:1:4 to :1:5
        \\    %3 = ret_implicit(@null) token_offset:1:6 to :1:7
        \\  }) (lbrace=1:1,rbrace=1:6) node_offset:1:1 to :1:7
        \\  %4 = print(%1)
        \\})
    );
    try testZir("{[x]x;}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = ret_implicit(@null) token_offset:1:7 to :1:8
        \\  }) (lbrace=1:1,rbrace=1:7) node_offset:1:1 to :1:8
        \\  %4 = print(%1)
        \\})
    );
    try testZir("{[x;y]x;}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = param_node("y") node_offset:1:5 to :1:6
        \\    %4 = ret_implicit(@null) token_offset:1:9 to :1:10
        \\  }) (lbrace=1:1,rbrace=1:9) node_offset:1:1 to :1:10
        \\  %5 = print(%1)
        \\})
    );

    try testZir("{y;}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_implicit(@x) token_offset:1:1 to :1:2
        \\    %3 = param_implicit(@y) token_offset:1:2 to :1:3
        \\    %4 = ret_implicit(@null) token_offset:1:4 to :1:5
        \\  }) (lbrace=1:1,rbrace=1:4) node_offset:1:1 to :1:5
        \\  %5 = print(%1)
        \\})
    );
    try testZir("{[]y;}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("y") token_offset:1:4 to :1:5
        \\    %3 = ret_implicit(@null) token_offset:1:6 to :1:7
        \\  }) (lbrace=1:1,rbrace=1:6) node_offset:1:1 to :1:7
        \\  %4 = print(%1)
        \\})
    );
    try testZir("{[x]y;}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = identifier("y") token_offset:1:5 to :1:6
        \\    %4 = ret_implicit(@null) token_offset:1:7 to :1:8
        \\  }) (lbrace=1:1,rbrace=1:7) node_offset:1:1 to :1:8
        \\  %5 = print(%1)
        \\})
    );
    try testZir("{[x;y]y;}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = param_node("y") node_offset:1:5 to :1:6
        \\    %4 = ret_implicit(@null) token_offset:1:9 to :1:10
        \\  }) (lbrace=1:1,rbrace=1:9) node_offset:1:1 to :1:10
        \\  %5 = print(%1)
        \\})
    );

    try testZir("{z;}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_implicit(@x) token_offset:1:1 to :1:2
        \\    %3 = param_implicit(@y) token_offset:1:1 to :1:2
        \\    %4 = param_implicit(@z) token_offset:1:2 to :1:3
        \\    %5 = ret_implicit(@null) token_offset:1:4 to :1:5
        \\  }) (lbrace=1:1,rbrace=1:4) node_offset:1:1 to :1:5
        \\  %6 = print(%1)
        \\})
    );
    try testZir("{[]z;}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("z") token_offset:1:4 to :1:5
        \\    %3 = ret_implicit(@null) token_offset:1:6 to :1:7
        \\  }) (lbrace=1:1,rbrace=1:6) node_offset:1:1 to :1:7
        \\  %4 = print(%1)
        \\})
    );
    try testZir("{[x]z;}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = identifier("z") token_offset:1:5 to :1:6
        \\    %4 = ret_implicit(@null) token_offset:1:7 to :1:8
        \\  }) (lbrace=1:1,rbrace=1:7) node_offset:1:1 to :1:8
        \\  %5 = print(%1)
        \\})
    );
    try testZir("{[x;y]z;}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = param_node("y") node_offset:1:5 to :1:6
        \\    %4 = identifier("z") token_offset:1:7 to :1:8
        \\    %5 = ret_implicit(@null) token_offset:1:9 to :1:10
        \\  }) (lbrace=1:1,rbrace=1:9) node_offset:1:1 to :1:10
        \\  %6 = print(%1)
        \\})
    );
}

test "expr block" {
    try testZir("[]",
        \\%0 = file({
        \\  %1 = print(@null)
        \\})
    );
    try testZir("[1;2;3]",
        \\%0 = file({
        \\  %1 = long(2)
        \\  %2 = long(3)
        \\  %3 = print(%2)
        \\})
    );
    try testZir("{[][a:1;a*:2;a*:2]}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("a") token_offset:1:5 to :1:6
        \\    %3 = apply(@assign, %2, @one) node_offset:1:5 to :1:8
        \\    %4 = long(2)
        \\    %5 = apply(@multiply, %2, %4) node_offset:1:9 to :1:13
        \\    %6 = apply(@assign, %2, %5) node_offset:1:9 to :1:13
        \\    %7 = long(2)
        \\    %8 = apply(@multiply, %2, %7) node_offset:1:14 to :1:18
        \\    %9 = apply(@assign, %2, %8) node_offset:1:14 to :1:18
        \\    %10 = ret_node(@null) node_offset:1:4 to :1:19
        \\  }) (lbrace=1:1,rbrace=1:19) node_offset:1:1 to :1:20
        \\  %11 = print(%1)
        \\})
    );
    try testZir("{[][a:1;a*:2;2*a*:2]}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("a") token_offset:1:5 to :1:6
        \\    %3 = apply(@assign, %2, @one) node_offset:1:5 to :1:8
        \\    %4 = long(2)
        \\    %5 = apply(@multiply, %2, %4) node_offset:1:9 to :1:13
        \\    %6 = apply(@assign, %2, %5) node_offset:1:9 to :1:13
        \\    %7 = long(2)
        \\    %8 = apply(@multiply, %2, %7) node_offset:1:16 to :1:20
        \\    %9 = apply(@assign, %2, %8) node_offset:1:16 to :1:20
        \\    %10 = long(2)
        \\    %11 = apply(@multiply, %10, %9) node_offset:1:14 to :1:20
        \\    %12 = ret_node(%11) node_offset:1:4 to :1:21
        \\  }) (lbrace=1:1,rbrace=1:21) node_offset:1:1 to :1:22
        \\  %13 = print(%1)
        \\})
    );
    try testZir("{[][a:1;a*:2;a*2]}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("a") token_offset:1:5 to :1:6
        \\    %3 = apply(@assign, %2, @one) node_offset:1:5 to :1:8
        \\    %4 = long(2)
        \\    %5 = apply(@multiply, %2, %4) node_offset:1:9 to :1:13
        \\    %6 = apply(@assign, %2, %5) node_offset:1:9 to :1:13
        \\    %7 = long(2)
        \\    %8 = apply(@multiply, %2, %7) node_offset:1:14 to :1:17
        \\    %9 = ret_node(%8) node_offset:1:4 to :1:18
        \\  }) (lbrace=1:1,rbrace=1:18) node_offset:1:1 to :1:19
        \\  %10 = print(%1)
        \\})
    );
    try testZir("{[][a:1;a*:2;a:a*2]}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("a") token_offset:1:5 to :1:6
        \\    %3 = apply(@assign, %2, @one) node_offset:1:5 to :1:8
        \\    %4 = long(2)
        \\    %5 = apply(@multiply, %2, %4) node_offset:1:9 to :1:13
        \\    %6 = apply(@assign, %2, %5) node_offset:1:9 to :1:13
        \\    %7 = long(2)
        \\    %8 = apply(@multiply, %2, %7) node_offset:1:16 to :1:19
        \\    %9 = apply(@assign, %2, %8) node_offset:1:14 to :1:19
        \\    %10 = ret_node(%9) node_offset:1:4 to :1:20
        \\  }) (lbrace=1:1,rbrace=1:20) node_offset:1:1 to :1:21
        \\  %11 = print(%1)
        \\})
    );
}

test "return" {
    try testZir(
        \\{[]
        \\  :1
        \\  }
    ,
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = ret_node(@one) node_offset:2:3 to :2:5
        \\    %3 = ret_node(%2) node_offset:2:3 to :2:5
        \\  }) (lbrace=1:1,rbrace=3:3) node_offset:1:1 to :1:2
        \\  %4 = print(%1)
        \\})
    );
    try testZir(
        \\{[]
        \\  :1;
        \\  }
    ,
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = ret_node(@one) node_offset:2:3 to :2:5
        \\    %3 = ret_implicit(@null) token_offset:3:3 to :3:4
        \\  }) (lbrace=1:1,rbrace=3:3) node_offset:1:1 to :1:2
        \\  %4 = print(%1)
        \\})
    );
}

test "signal" {
    try testZir("'`signal",
        \\%0 = file({
        \\  %1 = sym("signal") token_offset:1:2 to :1:9
        \\  %2 = signal(%1) node_offset:1:1 to :1:9
        \\  %3 = print(%2)
        \\})
    );
    try testZir("'\"signal\"",
        \\%0 = file({
        \\  %1 = str("signal") token_offset:1:2 to :1:10
        \\  %2 = signal(%1) node_offset:1:1 to :1:10
        \\  %3 = print(%2)
        \\})
    );
    try testZir("'break",
        \\%0 = file({
        \\  %1 = identifier("break") token_offset:1:2 to :1:7
        \\  %2 = signal(%1) node_offset:1:1 to :1:7
        \\  %3 = print(%2)
        \\})
    );
}

// TODO: \d .ns
// TODO: Setting .q namespace can segfault builtin references

// TODO: global undeclared identifier
// TODO: local undeclared identifier

// TODO: invalid assignment target

test "top-level assign" {
    try testZir("x:1",
        \\%0 = file({
        \\  %1 = identifier("x") token_offset:1:1 to :1:2
        \\  %2 = apply(@assign, %1, @one) node_offset:1:1 to :1:4
        \\})
    );
    try testZir("x:x:1",
        \\%0 = file({
        \\  %1 = identifier("x") token_offset:1:3 to :1:4
        \\  %2 = apply(@assign, %1, @one) node_offset:1:3 to :1:6
        \\  %3 = apply(@assign, %1, %2) node_offset:1:1 to :1:6
        \\})
    );
    try testZir("(x):1",
        \\%0 = file({
        \\  %1 = identifier("x") token_offset:1:2 to :1:3
        \\  %2 = apply(@assign, %1, @one) node_offset:1:1 to :1:6
        \\})
    );
    try testZir("(x):(x):1",
        \\%0 = file({
        \\  %1 = identifier("x") token_offset:1:6 to :1:7
        \\  %2 = apply(@assign, %1, @one) node_offset:1:5 to :1:10
        \\  %3 = apply(@assign, %1, %2) node_offset:1:1 to :1:10
        \\})
    );

    try testZir(":[x;1]",
        \\%0 = file({
        \\  %1 = identifier("x") token_offset:1:3 to :1:4
        \\  %2 = apply(@assign, %1, @one) node_offset:1:1 to :1:7
        \\})
    );
    try testZir(":[x;:[x;1]]",
        \\%0 = file({
        \\  %1 = identifier("x") token_offset:1:7 to :1:8
        \\  %2 = apply(@assign, %1, @one) node_offset:1:5 to :1:11
        \\  %3 = apply(@assign, %1, %2) node_offset:1:1 to :1:12
        \\})
    );
    try testZir(":[(x);1]",
        \\%0 = file({
        \\  %1 = identifier("x") token_offset:1:4 to :1:5
        \\  %2 = apply(@assign, %1, @one) node_offset:1:1 to :1:9
        \\})
    );
    try testZir(":[(x);:[(x);1]]",
        \\%0 = file({
        \\  %1 = identifier("x") token_offset:1:10 to :1:11
        \\  %2 = apply(@assign, %1, @one) node_offset:1:7 to :1:15
        \\  %3 = apply(@assign, %1, %2) node_offset:1:1 to :1:16
        \\})
    );
}

test "top-level global assign" {
    try testZir("x::1",
        \\%0 = file({
        \\  %1 = identifier("x") token_offset:1:1 to :1:2
        \\  %2 = apply(@assign, %1, @one) node_offset:1:1 to :1:5
        \\})
    );
    try testZir("x::x::1",
        \\%0 = file({
        \\  %1 = identifier("x") token_offset:1:4 to :1:5
        \\  %2 = apply(@assign, %1, @one) node_offset:1:4 to :1:8
        \\  %3 = apply(@assign, %1, %2) node_offset:1:1 to :1:8
        \\})
    );
    try testZir("(x)::1",
        \\%0 = file({
        \\  %1 = identifier("x") token_offset:1:2 to :1:3
        \\  %2 = apply(@assign, %1, @one) node_offset:1:1 to :1:7
        \\})
    );
    try testZir("(x)::(x)::1",
        \\%0 = file({
        \\  %1 = identifier("x") token_offset:1:7 to :1:8
        \\  %2 = apply(@assign, %1, @one) node_offset:1:6 to :1:12
        \\  %3 = apply(@assign, %1, %2) node_offset:1:1 to :1:12
        \\})
    );

    try testZir("::[x;1]",
        \\%0 = file({
        \\  %1 = identifier("x") token_offset:1:4 to :1:5
        \\  %2 = apply(@assign, %1, @one) node_offset:1:1 to :1:8
        \\})
    );
    try testZir("::[x;::[x;1]]",
        \\%0 = file({
        \\  %1 = identifier("x") token_offset:1:9 to :1:10
        \\  %2 = apply(@assign, %1, @one) node_offset:1:6 to :1:13
        \\  %3 = apply(@assign, %1, %2) node_offset:1:1 to :1:14
        \\})
    );
    try testZir("::[(x);1]",
        \\%0 = file({
        \\  %1 = identifier("x") token_offset:1:5 to :1:6
        \\  %2 = apply(@assign, %1, @one) node_offset:1:1 to :1:10
        \\})
    );
    try testZir("::[(x);::[(x);1]]",
        \\%0 = file({
        \\  %1 = identifier("x") token_offset:1:12 to :1:13
        \\  %2 = apply(@assign, %1, @one) node_offset:1:8 to :1:17
        \\  %3 = apply(@assign, %1, %2) node_offset:1:1 to :1:18
        \\})
    );
}

test "top-level namespace assign" {
    try testZir(".ns:1",
        \\%0 = file({
        \\  %1 = identifier(".ns") token_offset:1:1 to :1:4
        \\  %2 = apply(@assign, %1, @one) node_offset:1:1 to :1:6
        \\})
    );
    try testZir(".ns:.ns:1",
        \\%0 = file({
        \\  %1 = identifier(".ns") token_offset:1:5 to :1:8
        \\  %2 = apply(@assign, %1, @one) node_offset:1:5 to :1:10
        \\  %3 = apply(@assign, %1, %2) node_offset:1:1 to :1:10
        \\})
    );
    try testZir(".ns.x:1",
        \\%0 = file({
        \\  %1 = identifier(".ns.x") token_offset:1:1 to :1:6
        \\  %2 = apply(@assign, %1, @one) node_offset:1:1 to :1:8
        \\})
    );
    try testZir(".ns.x:.ns.x:1",
        \\%0 = file({
        \\  %1 = identifier(".ns.x") token_offset:1:7 to :1:12
        \\  %2 = apply(@assign, %1, @one) node_offset:1:7 to :1:14
        \\  %3 = apply(@assign, %1, %2) node_offset:1:1 to :1:14
        \\})
    );
    try testZir("(.ns):1",
        \\%0 = file({
        \\  %1 = identifier(".ns") token_offset:1:2 to :1:5
        \\  %2 = apply(@assign, %1, @one) node_offset:1:1 to :1:8
        \\})
    );
    try testZir("(.ns):(.ns):1",
        \\%0 = file({
        \\  %1 = identifier(".ns") token_offset:1:8 to :1:11
        \\  %2 = apply(@assign, %1, @one) node_offset:1:7 to :1:14
        \\  %3 = apply(@assign, %1, %2) node_offset:1:1 to :1:14
        \\})
    );
    try testZir("(.ns.x):1",
        \\%0 = file({
        \\  %1 = identifier(".ns.x") token_offset:1:2 to :1:7
        \\  %2 = apply(@assign, %1, @one) node_offset:1:1 to :1:10
        \\})
    );
    try testZir("(.ns.x):(.ns.x):1",
        \\%0 = file({
        \\  %1 = identifier(".ns.x") token_offset:1:10 to :1:15
        \\  %2 = apply(@assign, %1, @one) node_offset:1:9 to :1:18
        \\  %3 = apply(@assign, %1, %2) node_offset:1:1 to :1:18
        \\})
    );

    try testZir(":[.ns;1]",
        \\%0 = file({
        \\  %1 = identifier(".ns") token_offset:1:3 to :1:6
        \\  %2 = apply(@assign, %1, @one) node_offset:1:1 to :1:9
        \\})
    );
    try testZir(":[.ns;:[.ns;1]]",
        \\%0 = file({
        \\  %1 = identifier(".ns") token_offset:1:9 to :1:12
        \\  %2 = apply(@assign, %1, @one) node_offset:1:7 to :1:15
        \\  %3 = apply(@assign, %1, %2) node_offset:1:1 to :1:16
        \\})
    );
    try testZir(":[.ns.x;1]",
        \\%0 = file({
        \\  %1 = identifier(".ns.x") token_offset:1:3 to :1:8
        \\  %2 = apply(@assign, %1, @one) node_offset:1:1 to :1:11
        \\})
    );
    try testZir(":[.ns.x;:[.ns.x;1]]",
        \\%0 = file({
        \\  %1 = identifier(".ns.x") token_offset:1:11 to :1:16
        \\  %2 = apply(@assign, %1, @one) node_offset:1:9 to :1:19
        \\  %3 = apply(@assign, %1, %2) node_offset:1:1 to :1:20
        \\})
    );
    try testZir(":[(.ns);1]",
        \\%0 = file({
        \\  %1 = identifier(".ns") token_offset:1:4 to :1:7
        \\  %2 = apply(@assign, %1, @one) node_offset:1:1 to :1:11
        \\})
    );
    try testZir(":[(.ns);:[(.ns);1]]",
        \\%0 = file({
        \\  %1 = identifier(".ns") token_offset:1:12 to :1:15
        \\  %2 = apply(@assign, %1, @one) node_offset:1:9 to :1:19
        \\  %3 = apply(@assign, %1, %2) node_offset:1:1 to :1:20
        \\})
    );
    try testZir(":[(.ns.x);1]",
        \\%0 = file({
        \\  %1 = identifier(".ns.x") token_offset:1:4 to :1:9
        \\  %2 = apply(@assign, %1, @one) node_offset:1:1 to :1:13
        \\})
    );
    try testZir(":[(.ns.x);:[(.ns.x);1]]",
        \\%0 = file({
        \\  %1 = identifier(".ns.x") token_offset:1:14 to :1:19
        \\  %2 = apply(@assign, %1, @one) node_offset:1:11 to :1:23
        \\  %3 = apply(@assign, %1, %2) node_offset:1:1 to :1:24
        \\})
    );
}

test "top-level namespace global assign" {
    try testZir(".ns::1",
        \\%0 = file({
        \\  %1 = identifier(".ns") token_offset:1:1 to :1:4
        \\  %2 = apply(@assign, %1, @one) node_offset:1:1 to :1:7
        \\})
    );
    try testZir(".ns::.ns::1",
        \\%0 = file({
        \\  %1 = identifier(".ns") token_offset:1:6 to :1:9
        \\  %2 = apply(@assign, %1, @one) node_offset:1:6 to :1:12
        \\  %3 = apply(@assign, %1, %2) node_offset:1:1 to :1:12
        \\})
    );
    try testZir(".ns.x::1",
        \\%0 = file({
        \\  %1 = identifier(".ns.x") token_offset:1:1 to :1:6
        \\  %2 = apply(@assign, %1, @one) node_offset:1:1 to :1:9
        \\})
    );
    try testZir(".ns.x::.ns.x::1",
        \\%0 = file({
        \\  %1 = identifier(".ns.x") token_offset:1:8 to :1:13
        \\  %2 = apply(@assign, %1, @one) node_offset:1:8 to :1:16
        \\  %3 = apply(@assign, %1, %2) node_offset:1:1 to :1:16
        \\})
    );
    try testZir("(.ns)::1",
        \\%0 = file({
        \\  %1 = identifier(".ns") token_offset:1:2 to :1:5
        \\  %2 = apply(@assign, %1, @one) node_offset:1:1 to :1:9
        \\})
    );
    try testZir("(.ns)::(.ns)::1",
        \\%0 = file({
        \\  %1 = identifier(".ns") token_offset:1:9 to :1:12
        \\  %2 = apply(@assign, %1, @one) node_offset:1:8 to :1:16
        \\  %3 = apply(@assign, %1, %2) node_offset:1:1 to :1:16
        \\})
    );
    try testZir("(.ns.x)::1",
        \\%0 = file({
        \\  %1 = identifier(".ns.x") token_offset:1:2 to :1:7
        \\  %2 = apply(@assign, %1, @one) node_offset:1:1 to :1:11
        \\})
    );
    try testZir("(.ns.x)::(.ns.x)::1",
        \\%0 = file({
        \\  %1 = identifier(".ns.x") token_offset:1:11 to :1:16
        \\  %2 = apply(@assign, %1, @one) node_offset:1:10 to :1:20
        \\  %3 = apply(@assign, %1, %2) node_offset:1:1 to :1:20
        \\})
    );

    try testZir("::[.ns;1]",
        \\%0 = file({
        \\  %1 = identifier(".ns") token_offset:1:4 to :1:7
        \\  %2 = apply(@assign, %1, @one) node_offset:1:1 to :1:10
        \\})
    );
    try testZir("::[.ns;::[.ns;1]]",
        \\%0 = file({
        \\  %1 = identifier(".ns") token_offset:1:11 to :1:14
        \\  %2 = apply(@assign, %1, @one) node_offset:1:8 to :1:17
        \\  %3 = apply(@assign, %1, %2) node_offset:1:1 to :1:18
        \\})
    );
    try testZir("::[.ns.x;1]",
        \\%0 = file({
        \\  %1 = identifier(".ns.x") token_offset:1:4 to :1:9
        \\  %2 = apply(@assign, %1, @one) node_offset:1:1 to :1:12
        \\})
    );
    try testZir("::[.ns.x;::[.ns.x;1]]",
        \\%0 = file({
        \\  %1 = identifier(".ns.x") token_offset:1:13 to :1:18
        \\  %2 = apply(@assign, %1, @one) node_offset:1:10 to :1:21
        \\  %3 = apply(@assign, %1, %2) node_offset:1:1 to :1:22
        \\})
    );
    try testZir("::[(.ns);1]",
        \\%0 = file({
        \\  %1 = identifier(".ns") token_offset:1:5 to :1:8
        \\  %2 = apply(@assign, %1, @one) node_offset:1:1 to :1:12
        \\})
    );
    try testZir("::[(.ns);::[(.ns);1]]",
        \\%0 = file({
        \\  %1 = identifier(".ns") token_offset:1:14 to :1:17
        \\  %2 = apply(@assign, %1, @one) node_offset:1:10 to :1:21
        \\  %3 = apply(@assign, %1, %2) node_offset:1:1 to :1:22
        \\})
    );
    try testZir("::[(.ns.x);1]",
        \\%0 = file({
        \\  %1 = identifier(".ns.x") token_offset:1:5 to :1:10
        \\  %2 = apply(@assign, %1, @one) node_offset:1:1 to :1:14
        \\})
    );
    try testZir("::[(.ns.x);::[(.ns.x);1]]",
        \\%0 = file({
        \\  %1 = identifier(".ns.x") token_offset:1:16 to :1:21
        \\  %2 = apply(@assign, %1, @one) node_offset:1:12 to :1:25
        \\  %3 = apply(@assign, %1, %2) node_offset:1:1 to :1:26
        \\})
    );
}

test "lambda assign" {
    try testZir("{[]x:1}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("x") token_offset:1:4 to :1:5
        \\    %3 = apply(@assign, %2, @one) node_offset:1:4 to :1:7
        \\    %4 = ret_node(%3) node_offset:1:4 to :1:7
        \\  }) (lbrace=1:1,rbrace=1:7) node_offset:1:1 to :1:8
        \\  %5 = print(%1)
        \\})
    );
    try testZir("{[]x:x:1}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("x") token_offset:1:6 to :1:7
        \\    %3 = apply(@assign, %2, @one) node_offset:1:6 to :1:9
        \\    %4 = apply(@assign, %2, %3) node_offset:1:4 to :1:9
        \\    %5 = ret_node(%4) node_offset:1:4 to :1:9
        \\  }) (lbrace=1:1,rbrace=1:9) node_offset:1:1 to :1:10
        \\  %6 = print(%1)
        \\})
    );
    try testZir("{[](x):1}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("x") token_offset:1:5 to :1:6
        \\    %3 = apply(@assign, %2, @one) node_offset:1:4 to :1:9
        \\    %4 = ret_node(%3) node_offset:1:4 to :1:9
        \\  }) (lbrace=1:1,rbrace=1:9) node_offset:1:1 to :1:10
        \\  %5 = print(%1)
        \\})
    );
    try testZir("{[](x):(x):1}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("x") token_offset:1:9 to :1:10
        \\    %3 = apply(@assign, %2, @one) node_offset:1:8 to :1:13
        \\    %4 = apply(@assign, %2, %3) node_offset:1:4 to :1:13
        \\    %5 = ret_node(%4) node_offset:1:4 to :1:13
        \\  }) (lbrace=1:1,rbrace=1:13) node_offset:1:1 to :1:14
        \\  %6 = print(%1)
        \\})
    );

    try testZir("{[]:[x;1]}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("x") token_offset:1:6 to :1:7
        \\    %3 = apply(@assign, %2, @one) node_offset:1:4 to :1:10
        \\    %4 = ret_node(%3) node_offset:1:4 to :1:10
        \\  }) (lbrace=1:1,rbrace=1:10) node_offset:1:1 to :1:11
        \\  %5 = print(%1)
        \\})
    );
    try testZir("{[]:[x;:[x;1]]}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("x") token_offset:1:10 to :1:11
        \\    %3 = apply(@assign, %2, @one) node_offset:1:8 to :1:14
        \\    %4 = apply(@assign, %2, %3) node_offset:1:4 to :1:15
        \\    %5 = ret_node(%4) node_offset:1:4 to :1:15
        \\  }) (lbrace=1:1,rbrace=1:15) node_offset:1:1 to :1:16
        \\  %6 = print(%1)
        \\})
    );
    try testZir("{[]:[(x);1]}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("x") token_offset:1:7 to :1:8
        \\    %3 = apply(@assign, %2, @one) node_offset:1:4 to :1:12
        \\    %4 = ret_node(%3) node_offset:1:4 to :1:12
        \\  }) (lbrace=1:1,rbrace=1:12) node_offset:1:1 to :1:13
        \\  %5 = print(%1)
        \\})
    );
    try testZir("{[]:[(x);:[(x);1]]}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("x") token_offset:1:13 to :1:14
        \\    %3 = apply(@assign, %2, @one) node_offset:1:10 to :1:18
        \\    %4 = apply(@assign, %2, %3) node_offset:1:4 to :1:19
        \\    %5 = ret_node(%4) node_offset:1:4 to :1:19
        \\  }) (lbrace=1:1,rbrace=1:19) node_offset:1:1 to :1:20
        \\  %6 = print(%1)
        \\})
    );
}

test "lambda global assign" {
    try testZir("{[]x::1}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("x") token_offset:1:4 to :1:5
        \\    %3 = apply(@assign, %2, @one) node_offset:1:4 to :1:8
        \\    %4 = ret_node(%3) node_offset:1:4 to :1:8
        \\  }) (lbrace=1:1,rbrace=1:8) node_offset:1:1 to :1:9
        \\  %5 = print(%1)
        \\})
    );
    try testZir("{[]x::x::1}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("x") token_offset:1:7 to :1:8
        \\    %3 = apply(@assign, %2, @one) node_offset:1:7 to :1:11
        \\    %4 = apply(@assign, %2, %3) node_offset:1:4 to :1:11
        \\    %5 = ret_node(%4) node_offset:1:4 to :1:11
        \\  }) (lbrace=1:1,rbrace=1:11) node_offset:1:1 to :1:12
        \\  %6 = print(%1)
        \\})
    );
    try testZir("{[](x)::1}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("x") token_offset:1:5 to :1:6
        \\    %3 = apply(@assign, %2, @one) node_offset:1:4 to :1:10
        \\    %4 = ret_node(%3) node_offset:1:4 to :1:10
        \\  }) (lbrace=1:1,rbrace=1:10) node_offset:1:1 to :1:11
        \\  %5 = print(%1)
        \\})
    );
    try testZir("{[](x)::(x)::1}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("x") token_offset:1:10 to :1:11
        \\    %3 = apply(@assign, %2, @one) node_offset:1:9 to :1:15
        \\    %4 = apply(@assign, %2, %3) node_offset:1:4 to :1:15
        \\    %5 = ret_node(%4) node_offset:1:4 to :1:15
        \\  }) (lbrace=1:1,rbrace=1:15) node_offset:1:1 to :1:16
        \\  %6 = print(%1)
        \\})
    );

    try testZir("{[]::[x;1]}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("x") token_offset:1:7 to :1:8
        \\    %3 = apply(@assign, %2, @one) node_offset:1:4 to :1:11
        \\    %4 = ret_node(%3) node_offset:1:4 to :1:11
        \\  }) (lbrace=1:1,rbrace=1:11) node_offset:1:1 to :1:12
        \\  %5 = print(%1)
        \\})
    );
    try testZir("{[]::[x;::[x;1]]}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("x") token_offset:1:12 to :1:13
        \\    %3 = apply(@assign, %2, @one) node_offset:1:9 to :1:16
        \\    %4 = apply(@assign, %2, %3) node_offset:1:4 to :1:17
        \\    %5 = ret_node(%4) node_offset:1:4 to :1:17
        \\  }) (lbrace=1:1,rbrace=1:17) node_offset:1:1 to :1:18
        \\  %6 = print(%1)
        \\})
    );
    try testZir("{[]::[(x);1]}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("x") token_offset:1:8 to :1:9
        \\    %3 = apply(@assign, %2, @one) node_offset:1:4 to :1:13
        \\    %4 = ret_node(%3) node_offset:1:4 to :1:13
        \\  }) (lbrace=1:1,rbrace=1:13) node_offset:1:1 to :1:14
        \\  %5 = print(%1)
        \\})
    );
    try testZir("{[]::[(x);::[(x);1]]}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("x") token_offset:1:15 to :1:16
        \\    %3 = apply(@assign, %2, @one) node_offset:1:11 to :1:20
        \\    %4 = apply(@assign, %2, %3) node_offset:1:4 to :1:21
        \\    %5 = ret_node(%4) node_offset:1:4 to :1:21
        \\  }) (lbrace=1:1,rbrace=1:21) node_offset:1:1 to :1:22
        \\  %6 = print(%1)
        \\})
    );
}

test "lambda namespace assign" {
    try testZir("{[].ns:1}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier(".ns") token_offset:1:4 to :1:7
        \\    %3 = apply(@assign, %2, @one) node_offset:1:4 to :1:9
        \\    %4 = ret_node(%3) node_offset:1:4 to :1:9
        \\  }) (lbrace=1:1,rbrace=1:9) node_offset:1:1 to :1:10
        \\  %5 = print(%1)
        \\})
    );
    try testZir("{[].ns:.ns:1}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier(".ns") token_offset:1:8 to :1:11
        \\    %3 = apply(@assign, %2, @one) node_offset:1:8 to :1:13
        \\    %4 = apply(@assign, %2, %3) node_offset:1:4 to :1:13
        \\    %5 = ret_node(%4) node_offset:1:4 to :1:13
        \\  }) (lbrace=1:1,rbrace=1:13) node_offset:1:1 to :1:14
        \\  %6 = print(%1)
        \\})
    );
    try testZir("{[].ns.x:1}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier(".ns.x") token_offset:1:4 to :1:9
        \\    %3 = apply(@assign, %2, @one) node_offset:1:4 to :1:11
        \\    %4 = ret_node(%3) node_offset:1:4 to :1:11
        \\  }) (lbrace=1:1,rbrace=1:11) node_offset:1:1 to :1:12
        \\  %5 = print(%1)
        \\})
    );
    try testZir("{[].ns.x:.ns.x:1}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier(".ns.x") token_offset:1:10 to :1:15
        \\    %3 = apply(@assign, %2, @one) node_offset:1:10 to :1:17
        \\    %4 = apply(@assign, %2, %3) node_offset:1:4 to :1:17
        \\    %5 = ret_node(%4) node_offset:1:4 to :1:17
        \\  }) (lbrace=1:1,rbrace=1:17) node_offset:1:1 to :1:18
        \\  %6 = print(%1)
        \\})
    );
    try testZir("{[](.ns):1}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier(".ns") token_offset:1:5 to :1:8
        \\    %3 = apply(@assign, %2, @one) node_offset:1:4 to :1:11
        \\    %4 = ret_node(%3) node_offset:1:4 to :1:11
        \\  }) (lbrace=1:1,rbrace=1:11) node_offset:1:1 to :1:12
        \\  %5 = print(%1)
        \\})
    );
    try testZir("{[](.ns):(.ns):1}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier(".ns") token_offset:1:11 to :1:14
        \\    %3 = apply(@assign, %2, @one) node_offset:1:10 to :1:17
        \\    %4 = apply(@assign, %2, %3) node_offset:1:4 to :1:17
        \\    %5 = ret_node(%4) node_offset:1:4 to :1:17
        \\  }) (lbrace=1:1,rbrace=1:17) node_offset:1:1 to :1:18
        \\  %6 = print(%1)
        \\})
    );
    try testZir("{[](.ns.x):1}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier(".ns.x") token_offset:1:5 to :1:10
        \\    %3 = apply(@assign, %2, @one) node_offset:1:4 to :1:13
        \\    %4 = ret_node(%3) node_offset:1:4 to :1:13
        \\  }) (lbrace=1:1,rbrace=1:13) node_offset:1:1 to :1:14
        \\  %5 = print(%1)
        \\})
    );
    try testZir("{[](.ns.x):(.ns.x):1}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier(".ns.x") token_offset:1:13 to :1:18
        \\    %3 = apply(@assign, %2, @one) node_offset:1:12 to :1:21
        \\    %4 = apply(@assign, %2, %3) node_offset:1:4 to :1:21
        \\    %5 = ret_node(%4) node_offset:1:4 to :1:21
        \\  }) (lbrace=1:1,rbrace=1:21) node_offset:1:1 to :1:22
        \\  %6 = print(%1)
        \\})
    );

    try testZir("{[]:[.ns;1]}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier(".ns") token_offset:1:6 to :1:9
        \\    %3 = apply(@assign, %2, @one) node_offset:1:4 to :1:12
        \\    %4 = ret_node(%3) node_offset:1:4 to :1:12
        \\  }) (lbrace=1:1,rbrace=1:12) node_offset:1:1 to :1:13
        \\  %5 = print(%1)
        \\})
    );
    try testZir("{[]:[.ns;:[.ns;1]]}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier(".ns") token_offset:1:12 to :1:15
        \\    %3 = apply(@assign, %2, @one) node_offset:1:10 to :1:18
        \\    %4 = apply(@assign, %2, %3) node_offset:1:4 to :1:19
        \\    %5 = ret_node(%4) node_offset:1:4 to :1:19
        \\  }) (lbrace=1:1,rbrace=1:19) node_offset:1:1 to :1:20
        \\  %6 = print(%1)
        \\})
    );
    try testZir("{[]:[.ns.x;1]}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier(".ns.x") token_offset:1:6 to :1:11
        \\    %3 = apply(@assign, %2, @one) node_offset:1:4 to :1:14
        \\    %4 = ret_node(%3) node_offset:1:4 to :1:14
        \\  }) (lbrace=1:1,rbrace=1:14) node_offset:1:1 to :1:15
        \\  %5 = print(%1)
        \\})
    );
    try testZir("{[]:[.ns.x;:[.ns.x;1]]}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier(".ns.x") token_offset:1:14 to :1:19
        \\    %3 = apply(@assign, %2, @one) node_offset:1:12 to :1:22
        \\    %4 = apply(@assign, %2, %3) node_offset:1:4 to :1:23
        \\    %5 = ret_node(%4) node_offset:1:4 to :1:23
        \\  }) (lbrace=1:1,rbrace=1:23) node_offset:1:1 to :1:24
        \\  %6 = print(%1)
        \\})
    );
    try testZir("{[]:[(.ns);1]}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier(".ns") token_offset:1:7 to :1:10
        \\    %3 = apply(@assign, %2, @one) node_offset:1:4 to :1:14
        \\    %4 = ret_node(%3) node_offset:1:4 to :1:14
        \\  }) (lbrace=1:1,rbrace=1:14) node_offset:1:1 to :1:15
        \\  %5 = print(%1)
        \\})
    );
    try testZir("{[]:[(.ns);:[(.ns);1]]}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier(".ns") token_offset:1:15 to :1:18
        \\    %3 = apply(@assign, %2, @one) node_offset:1:12 to :1:22
        \\    %4 = apply(@assign, %2, %3) node_offset:1:4 to :1:23
        \\    %5 = ret_node(%4) node_offset:1:4 to :1:23
        \\  }) (lbrace=1:1,rbrace=1:23) node_offset:1:1 to :1:24
        \\  %6 = print(%1)
        \\})
    );
    try testZir("{[]:[(.ns.x);1]}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier(".ns.x") token_offset:1:7 to :1:12
        \\    %3 = apply(@assign, %2, @one) node_offset:1:4 to :1:16
        \\    %4 = ret_node(%3) node_offset:1:4 to :1:16
        \\  }) (lbrace=1:1,rbrace=1:16) node_offset:1:1 to :1:17
        \\  %5 = print(%1)
        \\})
    );
    try testZir("{[]:[(.ns.x);:[(.ns.x);1]]}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier(".ns.x") token_offset:1:17 to :1:22
        \\    %3 = apply(@assign, %2, @one) node_offset:1:14 to :1:26
        \\    %4 = apply(@assign, %2, %3) node_offset:1:4 to :1:27
        \\    %5 = ret_node(%4) node_offset:1:4 to :1:27
        \\  }) (lbrace=1:1,rbrace=1:27) node_offset:1:1 to :1:28
        \\  %6 = print(%1)
        \\})
    );
}

test "lambda namespace global assign" {
    try testZir("{[].ns::1}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier(".ns") token_offset:1:4 to :1:7
        \\    %3 = apply(@assign, %2, @one) node_offset:1:4 to :1:10
        \\    %4 = ret_node(%3) node_offset:1:4 to :1:10
        \\  }) (lbrace=1:1,rbrace=1:10) node_offset:1:1 to :1:11
        \\  %5 = print(%1)
        \\})
    );
    try testZir("{[].ns::.ns::1}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier(".ns") token_offset:1:9 to :1:12
        \\    %3 = apply(@assign, %2, @one) node_offset:1:9 to :1:15
        \\    %4 = apply(@assign, %2, %3) node_offset:1:4 to :1:15
        \\    %5 = ret_node(%4) node_offset:1:4 to :1:15
        \\  }) (lbrace=1:1,rbrace=1:15) node_offset:1:1 to :1:16
        \\  %6 = print(%1)
        \\})
    );
    try testZir("{[].ns.x::1}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier(".ns.x") token_offset:1:4 to :1:9
        \\    %3 = apply(@assign, %2, @one) node_offset:1:4 to :1:12
        \\    %4 = ret_node(%3) node_offset:1:4 to :1:12
        \\  }) (lbrace=1:1,rbrace=1:12) node_offset:1:1 to :1:13
        \\  %5 = print(%1)
        \\})
    );
    try testZir("{[].ns.x::.ns.x::1}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier(".ns.x") token_offset:1:11 to :1:16
        \\    %3 = apply(@assign, %2, @one) node_offset:1:11 to :1:19
        \\    %4 = apply(@assign, %2, %3) node_offset:1:4 to :1:19
        \\    %5 = ret_node(%4) node_offset:1:4 to :1:19
        \\  }) (lbrace=1:1,rbrace=1:19) node_offset:1:1 to :1:20
        \\  %6 = print(%1)
        \\})
    );
    try testZir("{[](.ns)::1}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier(".ns") token_offset:1:5 to :1:8
        \\    %3 = apply(@assign, %2, @one) node_offset:1:4 to :1:12
        \\    %4 = ret_node(%3) node_offset:1:4 to :1:12
        \\  }) (lbrace=1:1,rbrace=1:12) node_offset:1:1 to :1:13
        \\  %5 = print(%1)
        \\})
    );
    try testZir("{[](.ns)::(.ns)::1}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier(".ns") token_offset:1:12 to :1:15
        \\    %3 = apply(@assign, %2, @one) node_offset:1:11 to :1:19
        \\    %4 = apply(@assign, %2, %3) node_offset:1:4 to :1:19
        \\    %5 = ret_node(%4) node_offset:1:4 to :1:19
        \\  }) (lbrace=1:1,rbrace=1:19) node_offset:1:1 to :1:20
        \\  %6 = print(%1)
        \\})
    );
    try testZir("{[](.ns.x)::1}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier(".ns.x") token_offset:1:5 to :1:10
        \\    %3 = apply(@assign, %2, @one) node_offset:1:4 to :1:14
        \\    %4 = ret_node(%3) node_offset:1:4 to :1:14
        \\  }) (lbrace=1:1,rbrace=1:14) node_offset:1:1 to :1:15
        \\  %5 = print(%1)
        \\})
    );
    try testZir("{[](.ns.x)::(.ns.x)::1}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier(".ns.x") token_offset:1:14 to :1:19
        \\    %3 = apply(@assign, %2, @one) node_offset:1:13 to :1:23
        \\    %4 = apply(@assign, %2, %3) node_offset:1:4 to :1:23
        \\    %5 = ret_node(%4) node_offset:1:4 to :1:23
        \\  }) (lbrace=1:1,rbrace=1:23) node_offset:1:1 to :1:24
        \\  %6 = print(%1)
        \\})
    );

    try testZir("{[]::[.ns;1]}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier(".ns") token_offset:1:7 to :1:10
        \\    %3 = apply(@assign, %2, @one) node_offset:1:4 to :1:13
        \\    %4 = ret_node(%3) node_offset:1:4 to :1:13
        \\  }) (lbrace=1:1,rbrace=1:13) node_offset:1:1 to :1:14
        \\  %5 = print(%1)
        \\})
    );
    try testZir("{[]::[.ns;::[.ns;1]]}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier(".ns") token_offset:1:14 to :1:17
        \\    %3 = apply(@assign, %2, @one) node_offset:1:11 to :1:20
        \\    %4 = apply(@assign, %2, %3) node_offset:1:4 to :1:21
        \\    %5 = ret_node(%4) node_offset:1:4 to :1:21
        \\  }) (lbrace=1:1,rbrace=1:21) node_offset:1:1 to :1:22
        \\  %6 = print(%1)
        \\})
    );
    try testZir("{[]::[.ns.x;1]}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier(".ns.x") token_offset:1:7 to :1:12
        \\    %3 = apply(@assign, %2, @one) node_offset:1:4 to :1:15
        \\    %4 = ret_node(%3) node_offset:1:4 to :1:15
        \\  }) (lbrace=1:1,rbrace=1:15) node_offset:1:1 to :1:16
        \\  %5 = print(%1)
        \\})
    );
    try testZir("{[]::[.ns.x;::[.ns.x;1]]}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier(".ns.x") token_offset:1:16 to :1:21
        \\    %3 = apply(@assign, %2, @one) node_offset:1:13 to :1:24
        \\    %4 = apply(@assign, %2, %3) node_offset:1:4 to :1:25
        \\    %5 = ret_node(%4) node_offset:1:4 to :1:25
        \\  }) (lbrace=1:1,rbrace=1:25) node_offset:1:1 to :1:26
        \\  %6 = print(%1)
        \\})
    );
    try testZir("{[]::[(.ns);1]}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier(".ns") token_offset:1:8 to :1:11
        \\    %3 = apply(@assign, %2, @one) node_offset:1:4 to :1:15
        \\    %4 = ret_node(%3) node_offset:1:4 to :1:15
        \\  }) (lbrace=1:1,rbrace=1:15) node_offset:1:1 to :1:16
        \\  %5 = print(%1)
        \\})
    );
    try testZir("{[]::[(.ns);::[(.ns);1]]}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier(".ns") token_offset:1:17 to :1:20
        \\    %3 = apply(@assign, %2, @one) node_offset:1:13 to :1:24
        \\    %4 = apply(@assign, %2, %3) node_offset:1:4 to :1:25
        \\    %5 = ret_node(%4) node_offset:1:4 to :1:25
        \\  }) (lbrace=1:1,rbrace=1:25) node_offset:1:1 to :1:26
        \\  %6 = print(%1)
        \\})
    );
    try testZir("{[]::[(.ns.x);1]}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier(".ns.x") token_offset:1:8 to :1:13
        \\    %3 = apply(@assign, %2, @one) node_offset:1:4 to :1:17
        \\    %4 = ret_node(%3) node_offset:1:4 to :1:17
        \\  }) (lbrace=1:1,rbrace=1:17) node_offset:1:1 to :1:18
        \\  %5 = print(%1)
        \\})
    );
    try testZir("{[]::[(.ns.x);::[(.ns.x);1]]}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier(".ns.x") token_offset:1:19 to :1:24
        \\    %3 = apply(@assign, %2, @one) node_offset:1:15 to :1:28
        \\    %4 = apply(@assign, %2, %3) node_offset:1:4 to :1:29
        \\    %5 = ret_node(%4) node_offset:1:4 to :1:29
        \\  }) (lbrace=1:1,rbrace=1:29) node_offset:1:1 to :1:30
        \\  %6 = print(%1)
        \\})
    );
}

test "assign" {
    try testZir("x:x:1",
        \\%0 = file({
        \\  %1 = identifier("x") token_offset:1:3 to :1:4
        \\  %2 = apply(@assign, %1, @one) node_offset:1:3 to :1:6
        \\  %3 = apply(@assign, %1, %2) node_offset:1:1 to :1:6
        \\})
    );
    try testZir("x:1",
        \\%0 = file({
        \\  %1 = identifier("x") token_offset:1:1 to :1:2
        \\  %2 = apply(@assign, %1, @one) node_offset:1:1 to :1:4
        \\})
    );
    try testZir("(x):1",
        \\%0 = file({
        \\  %1 = identifier("x") token_offset:1:2 to :1:3
        \\  %2 = apply(@assign, %1, @one) node_offset:1:1 to :1:6
        \\})
    );

    // TODO: Warn setting namespace directly could have unintended consequences.
    try testZir(".ns:1",
        \\%0 = file({
        \\  %1 = identifier(".ns") token_offset:1:1 to :1:4
        \\  %2 = apply(@assign, %1, @one) node_offset:1:1 to :1:6
        \\})
    );
    try testZir("(.ns):1",
        \\%0 = file({
        \\  %1 = identifier(".ns") token_offset:1:2 to :1:5
        \\  %2 = apply(@assign, %1, @one) node_offset:1:1 to :1:8
        \\})
    );

    try testZir(".ns.x:1",
        \\%0 = file({
        \\  %1 = identifier(".ns.x") token_offset:1:1 to :1:6
        \\  %2 = apply(@assign, %1, @one) node_offset:1:1 to :1:8
        \\})
    );
    try testZir("(.ns.x):1",
        \\%0 = file({
        \\  %1 = identifier(".ns.x") token_offset:1:2 to :1:7
        \\  %2 = apply(@assign, %1, @one) node_offset:1:1 to :1:10
        \\})
    );

    try testZir(":[x;1]",
        \\%0 = file({
        \\  %1 = identifier("x") token_offset:1:3 to :1:4
        \\  %2 = apply(@assign, %1, @one) node_offset:1:1 to :1:7
        \\})
    );
    try testZir("(:)[x;1]",
        \\%0 = file({
        \\  %1 = identifier("x") token_offset:1:5 to :1:6
        \\  %2 = apply(@assign, %1, @one) node_offset:1:1 to :1:9
        \\})
    );
    try testZir(":[(x);1]",
        \\%0 = file({
        \\  %1 = identifier("x") token_offset:1:4 to :1:5
        \\  %2 = apply(@assign, %1, @one) node_offset:1:1 to :1:9
        \\})
    );
    try testZir("(:)[(x);1]",
        \\%0 = file({
        \\  %1 = identifier("x") token_offset:1:6 to :1:7
        \\  %2 = apply(@assign, %1, @one) node_offset:1:1 to :1:11
        \\})
    );

    try testZir("{[]x:1}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("x") token_offset:1:4 to :1:5
        \\    %3 = apply(@assign, %2, @one) node_offset:1:4 to :1:7
        \\    %4 = ret_node(%3) node_offset:1:4 to :1:7
        \\  }) (lbrace=1:1,rbrace=1:7) node_offset:1:1 to :1:8
        \\  %5 = print(%1)
        \\})
    );
    try testZir("{[](x):1}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("x") token_offset:1:5 to :1:6
        \\    %3 = apply(@assign, %2, @one) node_offset:1:4 to :1:9
        \\    %4 = ret_node(%3) node_offset:1:4 to :1:9
        \\  }) (lbrace=1:1,rbrace=1:9) node_offset:1:1 to :1:10
        \\  %5 = print(%1)
        \\})
    );
    try testZir("{[]:[x;1]}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("x") token_offset:1:6 to :1:7
        \\    %3 = apply(@assign, %2, @one) node_offset:1:4 to :1:10
        \\    %4 = ret_node(%3) node_offset:1:4 to :1:10
        \\  }) (lbrace=1:1,rbrace=1:10) node_offset:1:1 to :1:11
        \\  %5 = print(%1)
        \\})
    );
    try testZir("{[](:)[x;1]}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("x") token_offset:1:8 to :1:9
        \\    %3 = apply(@assign, %2, @one) node_offset:1:4 to :1:12
        \\    %4 = ret_node(%3) node_offset:1:4 to :1:12
        \\  }) (lbrace=1:1,rbrace=1:12) node_offset:1:1 to :1:13
        \\  %5 = print(%1)
        \\})
    );
    try testZir("{[]:[(x);1]}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("x") token_offset:1:7 to :1:8
        \\    %3 = apply(@assign, %2, @one) node_offset:1:4 to :1:12
        \\    %4 = ret_node(%3) node_offset:1:4 to :1:12
        \\  }) (lbrace=1:1,rbrace=1:12) node_offset:1:1 to :1:13
        \\  %5 = print(%1)
        \\})
    );
    try testZir("{[](:)[(x);1]}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("x") token_offset:1:9 to :1:10
        \\    %3 = apply(@assign, %2, @one) node_offset:1:4 to :1:14
        \\    %4 = ret_node(%3) node_offset:1:4 to :1:14
        \\  }) (lbrace=1:1,rbrace=1:14) node_offset:1:1 to :1:15
        \\  %5 = print(%1)
        \\})
    );
}

test "global assign" {
    try testZir("x::1",
        \\%0 = file({
        \\  %1 = identifier("x") token_offset:1:1 to :1:2
        \\  %2 = apply(@assign, %1, @one) node_offset:1:1 to :1:5
        \\})
    );
    try testZir("(x)::1",
        \\%0 = file({
        \\  %1 = identifier("x") token_offset:1:2 to :1:3
        \\  %2 = apply(@assign, %1, @one) node_offset:1:1 to :1:7
        \\})
    );

    try testZir("::[x;1]",
        \\%0 = file({
        \\  %1 = identifier("x") token_offset:1:4 to :1:5
        \\  %2 = apply(@assign, %1, @one) node_offset:1:1 to :1:8
        \\})
    );
    try testZir("(::)[x;1]",
        \\%0 = file({
        \\  %1 = identifier("x") token_offset:1:6 to :1:7
        \\  %2 = apply(@assign, %1, @one) node_offset:1:1 to :1:10
        \\})
    );
    try testZir("::[(x);1]",
        \\%0 = file({
        \\  %1 = identifier("x") token_offset:1:5 to :1:6
        \\  %2 = apply(@assign, %1, @one) node_offset:1:1 to :1:10
        \\})
    );
    try testZir("(::)[(x);1]",
        \\%0 = file({
        \\  %1 = identifier("x") token_offset:1:7 to :1:8
        \\  %2 = apply(@assign, %1, @one) node_offset:1:1 to :1:12
        \\})
    );
    try testZir("{[]x::1}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("x") token_offset:1:4 to :1:5
        \\    %3 = apply(@assign, %2, @one) node_offset:1:4 to :1:8
        \\    %4 = ret_node(%3) node_offset:1:4 to :1:8
        \\  }) (lbrace=1:1,rbrace=1:8) node_offset:1:1 to :1:9
        \\  %5 = print(%1)
        \\})
    );
    try testZir("{[](x)::1}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("x") token_offset:1:5 to :1:6
        \\    %3 = apply(@assign, %2, @one) node_offset:1:4 to :1:10
        \\    %4 = ret_node(%3) node_offset:1:4 to :1:10
        \\  }) (lbrace=1:1,rbrace=1:10) node_offset:1:1 to :1:11
        \\  %5 = print(%1)
        \\})
    );
    try testZir("{[]::[x;1]}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("x") token_offset:1:7 to :1:8
        \\    %3 = apply(@assign, %2, @one) node_offset:1:4 to :1:11
        \\    %4 = ret_node(%3) node_offset:1:4 to :1:11
        \\  }) (lbrace=1:1,rbrace=1:11) node_offset:1:1 to :1:12
        \\  %5 = print(%1)
        \\})
    );
    try testZir("{[](::)[x;1]}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("x") token_offset:1:9 to :1:10
        \\    %3 = apply(@assign, %2, @one) node_offset:1:4 to :1:13
        \\    %4 = ret_node(%3) node_offset:1:4 to :1:13
        \\  }) (lbrace=1:1,rbrace=1:13) node_offset:1:1 to :1:14
        \\  %5 = print(%1)
        \\})
    );
    try testZir("{[]::[(x);1]}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("x") token_offset:1:8 to :1:9
        \\    %3 = apply(@assign, %2, @one) node_offset:1:4 to :1:13
        \\    %4 = ret_node(%3) node_offset:1:4 to :1:13
        \\  }) (lbrace=1:1,rbrace=1:13) node_offset:1:1 to :1:14
        \\  %5 = print(%1)
        \\})
    );
    try testZir("{[](::)[(x);1]}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("x") token_offset:1:10 to :1:11
        \\    %3 = apply(@assign, %2, @one) node_offset:1:4 to :1:15
        \\    %4 = ret_node(%3) node_offset:1:4 to :1:15
        \\  }) (lbrace=1:1,rbrace=1:15) node_offset:1:1 to :1:16
        \\  %5 = print(%1)
        \\})
    );
}

test "view" {
    return error.SkipZigTest;
}

test "colon" {
    return error.SkipZigTest;
}

test "colon colon" {
    return error.SkipZigTest;
}

test "plus" {
    return error.SkipZigTest;
}

test "plus colon" {
    return error.SkipZigTest;
}

test "minus" {
    return error.SkipZigTest;
}

test "minus colon" {
    return error.SkipZigTest;
}

test "asterisk" {
    return error.SkipZigTest;
}

test "asterisk colon" {
    return error.SkipZigTest;
}

test "percent" {
    return error.SkipZigTest;
}

test "percent colon" {
    return error.SkipZigTest;
}

test "ampersand" {
    return error.SkipZigTest;
}

test "ampersand colon" {
    return error.SkipZigTest;
}

test "pipe" {
    return error.SkipZigTest;
}

test "pipe colon" {
    return error.SkipZigTest;
}

test "caret" {
    return error.SkipZigTest;
}

test "caret colon" {
    return error.SkipZigTest;
}

test "equal" {
    return error.SkipZigTest;
}

test "equal colon" {
    return error.SkipZigTest;
}

test "angle bracket left" {
    return error.SkipZigTest;
}

test "angle bracket left colon" {
    return error.SkipZigTest;
}

test "angle bracket left equal" {
    return error.SkipZigTest;
}

test "angle bracket left right" {
    return error.SkipZigTest;
}

test "angle bracket right" {
    return error.SkipZigTest;
}

test "angle bracket right colon" {
    return error.SkipZigTest;
}

test "angle bracket right equal" {
    return error.SkipZigTest;
}

test "dollar" {
    return error.SkipZigTest;
}

test "dollar colon" {
    return error.SkipZigTest;
}

test "comma" {
    return error.SkipZigTest;
}

test "comma colon" {
    return error.SkipZigTest;
}

test "hash" {
    return error.SkipZigTest;
}

test "hash colon" {
    return error.SkipZigTest;
}

test "underscore" {
    return error.SkipZigTest;
}

test "underscore colon" {
    return error.SkipZigTest;
}

test "tilde" {
    return error.SkipZigTest;
}

test "tilde colon" {
    return error.SkipZigTest;
}

test "bang" {
    return error.SkipZigTest;
}

test "bang colon" {
    return error.SkipZigTest;
}

test "question mark" {
    return error.SkipZigTest;
}

test "question mark colon" {
    return error.SkipZigTest;
}

test "at" {
    return error.SkipZigTest;
}

test "at colon" {
    return error.SkipZigTest;
}

test "period" {
    return error.SkipZigTest;
}

test "period colon" {
    return error.SkipZigTest;
}

test "zero colon" {
    return error.SkipZigTest;
}

test "zero colon colon" {
    return error.SkipZigTest;
}

test "one colon" {
    return error.SkipZigTest;
}

test "one colon colon" {
    return error.SkipZigTest;
}

test "two colon" {
    return error.SkipZigTest;
}

test "apostrophe" {
    return error.SkipZigTest;
}

test "apostrophe colon" {
    return error.SkipZigTest;
}

test "slash" {
    // TODO: a{[x]}/b
    try testZir("a+/",
        \\%0 = file({
        \\  %1 = apply(@over, @add) node_offset:1:2 to :1:4
        \\  %2 = identifier("a") token_offset:1:1 to :1:2
        \\  %3 = apply(%1, %2, .none) node_offset:1:1 to :1:4
        \\  %4 = print(%3)
        \\})
    );
    try testZir("a+/b",
        \\%0 = file({
        \\  %1 = identifier("b") token_offset:1:4 to :1:5
        \\  %2 = apply(@over, @add) node_offset:1:2 to :1:4
        \\  %3 = identifier("a") token_offset:1:1 to :1:2
        \\  %4 = apply(%2, %3, %1) node_offset:1:1 to :1:5
        \\  %5 = print(%4)
        \\})
    );
    // TODO: Need to think about this:
    // q)a:10
    // q)b:1 2 3
    // q)a:/b
    // 3
    try failZir("a:/b",
        \\test:1:2: error: cannot apply iterator to assignment
        \\a:/b
        \\ ^
    );
}

test "slash colon" {
    return error.SkipZigTest;
}

test "backslash" {
    return error.SkipZigTest;
}

test "backslash colon" {
    return error.SkipZigTest;
}

test "call" {
    try failZir(":[]",
        \\test:1:2: error: expected 2 argument(s), found 1
        \\:[]
        \\~^~
    );
    try failZir(":[x]",
        \\test:1:2: error: expected 2 argument(s), found 1
        \\:[x]
        \\~^~~
    );
    try failZir(":[x;y;z]",
        \\test:1:2: error: expected 2 argument(s), found 3
        \\:[x;y;z]
        \\~^~~~~~~
    );
    try failZir(":[1;2]",
        \\test:1:3: error: invalid left-hand side to assignment
        \\:[1;2]
        \\  ^
    );

    try testZir("f[]",
        \\%0 = file({
        \\  %1 = identifier("f") token_offset:1:1 to :1:2
        \\  %2 = apply(%1, @null) node_offset:1:1 to :1:4
        \\  %3 = print(%2)
        \\})
    );
    try testZir("f[1]",
        \\%0 = file({
        \\  %1 = identifier("f") token_offset:1:1 to :1:2
        \\  %2 = apply(%1, @one) node_offset:1:1 to :1:5
        \\  %3 = print(%2)
        \\})
    );
    try testZir("f[1;2]",
        \\%0 = file({
        \\  %1 = long(2)
        \\  %2 = identifier("f") token_offset:1:1 to :1:2
        \\  %3 = apply(%2, @one, %1) node_offset:1:1 to :1:7
        \\  %4 = print(%3)
        \\})
    );
    try testZir("f[1;2;3]",
        \\%0 = file({
        \\  %1 = long(3)
        \\  %2 = long(2)
        \\  %3 = identifier("f") token_offset:1:1 to :1:2
        \\  %4 = apply(%3, @one, %2, %1) node_offset:1:1 to :1:9
        \\  %5 = print(%4)
        \\})
    );

    try testZir("()[]",
        \\%0 = file({
        \\  %1 = apply(@empty_list, @null) node_offset:1:1 to :1:5
        \\  %2 = print(%1)
        \\})
    );
    try testZir("(first x)[]",
        \\%0 = file({
        \\  %1 = identifier("x") token_offset:1:8 to :1:9
        \\  %2 = builtin("first") token_offset:1:2 to :1:7
        \\  %3 = apply(%2, %1) node_offset:1:2 to :1:9
        \\  %4 = apply(%3, @null) node_offset:1:1 to :1:12
        \\  %5 = print(%4)
        \\})
    );
    try testZir("(` sv`.test`func)[]",
        \\%0 = file({
        \\  %1 = sym_list(".test", "func") node_offset:1:6 to :1:17
        \\  %2 = builtin("sv") token_offset:1:4 to :1:6
        \\  %3 = sym("") token_offset:1:2 to :1:3
        \\  %4 = apply(%2, %3, %1) node_offset:1:2 to :1:17
        \\  %5 = apply(%4, @null) node_offset:1:1 to :1:20
        \\  %6 = print(%5)
        \\})
    );

    try testZir("[f;f;f][]",
        \\%0 = file({
        \\  %1 = identifier("f") token_offset:1:2 to :1:3
        \\  %2 = identifier("f") token_offset:1:4 to :1:5
        \\  %3 = identifier("f") token_offset:1:6 to :1:7
        \\  %4 = apply(%3, @null) node_offset:1:1 to :1:10
        \\  %5 = print(%4)
        \\})
    );
}

test "apply unary" {
    try testZir("f x",
        \\%0 = file({
        \\  %1 = identifier("x") token_offset:1:3 to :1:4
        \\  %2 = identifier("f") token_offset:1:1 to :1:2
        \\  %3 = apply(%2, %1) node_offset:1:1 to :1:4
        \\  %4 = print(%3)
        \\})
    );
}

test "apply binary" {
    try testZir("x+1",
        \\%0 = file({
        \\  %1 = identifier("x") token_offset:1:1 to :1:2
        \\  %2 = apply(@add, %1, @one) node_offset:1:1 to :1:4
        \\  %3 = print(%2)
        \\})
    );
    try testZir("{[]x+1}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("x") token_offset:1:4 to :1:5
        \\    %3 = apply(@add, %2, @one) node_offset:1:4 to :1:7
        \\    %4 = ret_node(%3) node_offset:1:4 to :1:7
        \\  }) (lbrace=1:1,rbrace=1:7) node_offset:1:1 to :1:8
        \\  %5 = print(%1)
        \\})
    );
    try testZir("x+2",
        \\%0 = file({
        \\  %1 = long(2)
        \\  %2 = identifier("x") token_offset:1:1 to :1:2
        \\  %3 = apply(@add, %2, %1) node_offset:1:1 to :1:4
        \\  %4 = print(%3)
        \\})
    );
    try testZir("{[]x+2}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = long(2)
        \\    %3 = identifier("x") token_offset:1:4 to :1:5
        \\    %4 = apply(@add, %3, %2) node_offset:1:4 to :1:7
        \\    %5 = ret_node(%4) node_offset:1:4 to :1:7
        \\  }) (lbrace=1:1,rbrace=1:7) node_offset:1:1 to :1:8
        \\  %6 = print(%1)
        \\})
    );
}

test "number literal" {
    if (true) return error.SkipZigTest;
    try testZir("10:00",
        \\%0 = file({
        \\  %1 = minute(600)
        \\})
    );
}

test "number list literal" {
    return error.SkipZigTest;
}

test "string literal" {
    return error.SkipZigTest;
}

test "symbol literal" {
    try testZir("`symbol123",
        \\%0 = file({
        \\  %1 = sym("symbol123") token_offset:1:1 to :1:11
        \\  %2 = print(%1)
        \\})
    );
}

test "symbol list literal" {
    try testZir("`abc`def",
        \\%0 = file({
        \\  %1 = sym_list("abc", "def") node_offset:1:1 to :1:9
        \\  %2 = print(%1)
        \\})
    );
}

test "identifier" {
    try testZir("a",
        \\%0 = file({
        \\  %1 = identifier("a") token_offset:1:1 to :1:2
        \\  %2 = print(%1)
        \\})
    );
}

test "builtin" {
    try testZir("value x",
        \\%0 = file({
        \\  %1 = identifier("x") token_offset:1:7 to :1:8
        \\  %2 = builtin("value") token_offset:1:1 to :1:6
        \\  %3 = apply(%2, %1) node_offset:1:1 to :1:8
        \\  %4 = print(%3)
        \\})
    );
    try testZir("value[x]",
        \\%0 = file({
        \\  %1 = identifier("x") token_offset:1:7 to :1:8
        \\  %2 = builtin("value") token_offset:1:1 to :1:6
        \\  %3 = apply(%2, %1) node_offset:1:1 to :1:9
        \\  %4 = print(%3)
        \\})
    );
    try testZir("x sv y",
        \\%0 = file({
        \\  %1 = identifier("y") token_offset:1:6 to :1:7
        \\  %2 = builtin("sv") token_offset:1:3 to :1:5
        \\  %3 = identifier("x") token_offset:1:1 to :1:2
        \\  %4 = apply(%2, %3, %1) node_offset:1:1 to :1:7
        \\  %5 = print(%4)
        \\})
    );
    try testZir("sv[x;y]",
        \\%0 = file({
        \\  %1 = identifier("y") token_offset:1:6 to :1:7
        \\  %2 = identifier("x") token_offset:1:4 to :1:5
        \\  %3 = builtin("sv") token_offset:1:1 to :1:3
        \\  %4 = apply(%3, %2, %1) node_offset:1:1 to :1:8
        \\  %5 = print(%4)
        \\})
    );
}

test "select" {
    return error.SkipZigTest;
}

test "exec" {
    return error.SkipZigTest;
}

test "update" {
    return error.SkipZigTest;
}

test "delete rows" {
    return error.SkipZigTest;
}

test "delete cols" {
    return error.SkipZigTest;
}

test "do" {
    return error.SkipZigTest;
}

test "if" {
    return error.SkipZigTest;
}

test "while" {
    return error.SkipZigTest;
}

test "cond" {
    if (true) return error.SkipZigTest;

    try testZir("{$[x;a:1;a]}", "");
    try testZir("{$[x;a;a:1]}", "");

    try testZir("{$[x;a:1;a;a]}", "");
    try testZir("{$[x;a;a:1;a]}", "");
    try testZir("{$[x;a;a;a:1]}", "");

    try testZir("{$[x;a:1;a;a;a]}", "");
    try testZir("{$[x;a;a:1;a;a]}", "");
    try testZir("{$[x;a;a;a:1;a]}", "");
    try testZir("{$[x;a;a;a;a:1]}", "");
}

test "too many parameters" {
    try testZir("{[a;b;c;d;e;f;g;h]}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("a") node_offset:1:3 to :1:4
        \\    %3 = param_node("b") node_offset:1:5 to :1:6
        \\    %4 = param_node("c") node_offset:1:7 to :1:8
        \\    %5 = param_node("d") node_offset:1:9 to :1:10
        \\    %6 = param_node("e") node_offset:1:11 to :1:12
        \\    %7 = param_node("f") node_offset:1:13 to :1:14
        \\    %8 = param_node("g") node_offset:1:15 to :1:16
        \\    %9 = param_node("h") node_offset:1:17 to :1:18
        \\    %10 = ret_implicit(@null) token_offset:1:19 to :1:20
        \\  }) (lbrace=1:1,rbrace=1:19) node_offset:1:1 to :1:20
        \\  %11 = print(%1)
        \\})
    );
    try failZir("{[a;b;c;d;e;f;g;h;i]}",
        \\test:1:19: error: too many parameters (8 max)
        \\{[a;b;c;d;e;f;g;h;i]}
        \\                  ^
    );
    try failZir("{[a;b;c;d;e;f;g;h;i;j]}",
        \\test:1:19: error: too many parameters (8 max)
        \\{[a;b;c;d;e;f;g;h;i;j]}
        \\                  ^
    );
}

test "declared after use / use of undeclared identifier" {
    // TODO: remove multiple identifier instrs
    try testZir("{[]a::a+1}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("a") token_offset:1:7 to :1:8
        \\    %3 = apply(@add, %2, @one) node_offset:1:7 to :1:10
        \\    %4 = identifier("a") token_offset:1:4 to :1:5
        \\    %5 = apply(@assign, %4, %3) node_offset:1:4 to :1:10
        \\    %6 = ret_node(%5) node_offset:1:4 to :1:10
        \\  }) (lbrace=1:1,rbrace=1:10) node_offset:1:1 to :1:11
        \\  %7 = print(%1)
        \\})
    );
    try failZir("{[]a:a+1}",
        \\test:1:6: error: use of undeclared identifier 'a'
        \\{[]a:a+1}
        \\     ^
        \\test:1:4: note: identifier declared here
        \\{[]a:a+1}
        \\   ^
    );

    try testZir(
        \\{[]
        \\  a+1;
        \\  a::1;
        \\  }
    ,
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("a") token_offset:2:3 to :2:4
        \\    %3 = apply(@add, %2, @one) node_offset:2:3 to :2:6
        \\    %4 = identifier("a") token_offset:3:3 to :3:4
        \\    %5 = apply(@assign, %4, @one) node_offset:3:3 to :3:7
        \\    %6 = ret_implicit(@null) token_offset:4:3 to :4:4
        \\  }) (lbrace=1:1,rbrace=4:3) node_offset:1:1 to :1:2
        \\  %7 = print(%1)
        \\})
    );
    try failZir(
        \\{[]
        \\  a+1;
        \\  a:1;
        \\  }
    ,
        \\test:2:3: error: use of undeclared identifier 'a'
        \\  a+1;
        \\  ^
        \\test:3:3: note: identifier declared here
        \\  a:1;
        \\  ^
    );
}

test "unused function parameter" {
    try warnZir("{[x]}",
        \\test:1:3: warn: unused function parameter
        \\{[x]}
        \\  ^
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = ret_implicit(@null) token_offset:1:5 to :1:6
        \\  }) (lbrace=1:1,rbrace=1:5) node_offset:1:1 to :1:6
        \\  %4 = print(%1)
        \\})
    );
    try warnZir("{[x;y]}",
        \\test:1:5: warn: unused function parameter
        \\{[x;y]}
        \\    ^
        \\test:1:3: warn: unused function parameter
        \\{[x;y]}
        \\  ^
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = param_node("y") node_offset:1:5 to :1:6
        \\    %4 = ret_implicit(@null) token_offset:1:7 to :1:8
        \\  }) (lbrace=1:1,rbrace=1:7) node_offset:1:1 to :1:8
        \\  %5 = print(%1)
        \\})
    );
}

test "unused implicit function parameter" {
    try testZir("{}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = ret_implicit(@null) token_offset:1:2 to :1:3
        \\  }) (lbrace=1:1,rbrace=1:2) node_offset:1:1 to :1:3
        \\  %3 = print(%1)
        \\})
    );
    try testZir("{x}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_implicit(@x) token_offset:1:2 to :1:3
        \\    %3 = ret_node(%2) node_offset:1:2 to :1:3
        \\  }) (lbrace=1:1,rbrace=1:3) node_offset:1:1 to :1:4
        \\  %4 = print(%1)
        \\})
    );
    try warnZir("{y}",
        \\test:1:1: warn: unused function parameter 'x'
        \\{y}
        \\^
        \\test:1:2: note: consider renaming 'y' to 'x'
        \\{y}
        \\ ^
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_implicit(@x) token_offset:1:1 to :1:2
        \\    %3 = param_implicit(@y) token_offset:1:2 to :1:3
        \\    %4 = ret_node(%3) node_offset:1:2 to :1:3
        \\  }) (lbrace=1:1,rbrace=1:3) node_offset:1:1 to :1:4
        \\  %5 = print(%1)
        \\})
    );
    try testZir("{x+y}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_implicit(@x) token_offset:1:2 to :1:3
        \\    %3 = param_implicit(@y) token_offset:1:4 to :1:5
        \\    %4 = apply(@add, %2, %3) node_offset:1:2 to :1:5
        \\    %5 = ret_node(%4) node_offset:1:2 to :1:5
        \\  }) (lbrace=1:1,rbrace=1:5) node_offset:1:1 to :1:6
        \\  %6 = print(%1)
        \\})
    );
    try warnZir("{z}",
        \\test:1:1: warn: unused function parameter 'y'
        \\{z}
        \\^
        \\test:1:2: note: consider renaming 'z' to 'x'
        \\{z}
        \\ ^
        \\test:1:1: warn: unused function parameter 'x'
        \\{z}
        \\^
        \\test:1:2: note: consider renaming 'z' to 'x'
        \\{z}
        \\ ^
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_implicit(@x) token_offset:1:1 to :1:2
        \\    %3 = param_implicit(@y) token_offset:1:1 to :1:2
        \\    %4 = param_implicit(@z) token_offset:1:2 to :1:3
        \\    %5 = ret_node(%4) node_offset:1:2 to :1:3
        \\  }) (lbrace=1:1,rbrace=1:3) node_offset:1:1 to :1:4
        \\  %6 = print(%1)
        \\})
    );
    try warnZir("{x+z}",
        \\test:1:1: warn: unused function parameter 'y'
        \\{x+z}
        \\^
        \\test:1:4: note: consider renaming 'z' to 'y'
        \\{x+z}
        \\   ^
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_implicit(@x) token_offset:1:2 to :1:3
        \\    %3 = param_implicit(@y) token_offset:1:1 to :1:2
        \\    %4 = param_implicit(@z) token_offset:1:4 to :1:5
        \\    %5 = apply(@add, %2, %4) node_offset:1:2 to :1:5
        \\    %6 = ret_node(%5) node_offset:1:2 to :1:5
        \\  }) (lbrace=1:1,rbrace=1:5) node_offset:1:1 to :1:6
        \\  %7 = print(%1)
        \\})
    );
    try warnZir("{y+z}",
        \\test:1:1: warn: unused function parameter 'x'
        \\{y+z}
        \\^
        \\test:1:2: note: consider renaming 'y' to 'x'
        \\{y+z}
        \\ ^
        \\test:1:4: note: consider renaming 'z' to 'y'
        \\{y+z}
        \\   ^
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_implicit(@x) token_offset:1:1 to :1:2
        \\    %3 = param_implicit(@y) token_offset:1:2 to :1:3
        \\    %4 = param_implicit(@z) token_offset:1:4 to :1:5
        \\    %5 = apply(@add, %3, %4) node_offset:1:2 to :1:5
        \\    %6 = ret_node(%5) node_offset:1:2 to :1:5
        \\  }) (lbrace=1:1,rbrace=1:5) node_offset:1:1 to :1:6
        \\  %7 = print(%1)
        \\})
    );
    try testZir("{x+y+z}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_implicit(@x) token_offset:1:2 to :1:3
        \\    %3 = param_implicit(@y) token_offset:1:4 to :1:5
        \\    %4 = param_implicit(@z) token_offset:1:6 to :1:7
        \\    %5 = apply(@add, %3, %4) node_offset:1:4 to :1:7
        \\    %6 = apply(@add, %2, %5) node_offset:1:2 to :1:7
        \\    %7 = ret_node(%6) node_offset:1:2 to :1:7
        \\  }) (lbrace=1:1,rbrace=1:7) node_offset:1:1 to :1:8
        \\  %8 = print(%1)
        \\})
    );
}

test "unused local variable" {
    try warnZir(
        \\{[]
        \\  a:1;
        \\  }
    ,
        \\test:2:3: warn: unused local variable
        \\  a:1;
        \\  ^
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("a") token_offset:2:3 to :2:4
        \\    %3 = apply(@assign, %2, @one) node_offset:2:3 to :2:6
        \\    %4 = ret_implicit(@null) token_offset:3:3 to :3:4
        \\  }) (lbrace=1:1,rbrace=3:3) node_offset:1:1 to :1:2
        \\  %5 = print(%1)
        \\})
    );
    try warnZir(
        \\{[]
        \\  a:1;
        \\  a:2;
        \\  }
    ,
        \\test:2:3: warn: unused local variable
        \\  a:1;
        \\  ^
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("a") token_offset:2:3 to :2:4
        \\    %3 = apply(@assign, %2, @one) node_offset:2:3 to :2:6
        \\    %4 = long(2)
        \\    %5 = apply(@assign, %2, %4) node_offset:3:3 to :3:6
        \\    %6 = ret_implicit(@null) token_offset:4:3 to :4:4
        \\  }) (lbrace=1:1,rbrace=4:3) node_offset:1:1 to :1:2
        \\  %7 = print(%1)
        \\})
    );
    try warnZir(
        \\{
        \\  a:1;
        \\  a:2;
        \\  }
    ,
        \\test:2:3: warn: unused local variable
        \\  a:1;
        \\  ^
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("a") token_offset:2:3 to :2:4
        \\    %3 = apply(@assign, %2, @one) node_offset:2:3 to :2:6
        \\    %4 = long(2)
        \\    %5 = apply(@assign, %2, %4) node_offset:3:3 to :3:6
        \\    %6 = ret_implicit(@null) token_offset:4:3 to :4:4
        \\  }) (lbrace=1:1,rbrace=4:3) node_offset:1:1 to :1:2
        \\  %7 = print(%1)
        \\})
    );
}

test "redeclaration of function parameter" {
    try warnZir("{[a;a]}",
        \\test:1:5: warn: redeclaration of function parameter 'a'
        \\{[a;a]}
        \\    ^
        \\test:1:3: note: previous declaration here
        \\{[a;a]}
        \\  ^
        \\test:1:3: warn: unused function parameter
        \\{[a;a]}
        \\  ^
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("a") node_offset:1:3 to :1:4
        \\    %3 = ret_implicit(@null) token_offset:1:7 to :1:8
        \\  }) (lbrace=1:1,rbrace=1:7) node_offset:1:1 to :1:8
        \\  %4 = print(%1)
        \\})
    );
    try warnZir("{[a;a]a}",
        \\test:1:5: warn: redeclaration of function parameter 'a'
        \\{[a;a]a}
        \\    ^
        \\test:1:3: note: previous declaration here
        \\{[a;a]a}
        \\  ^
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("a") node_offset:1:3 to :1:4
        \\    %3 = ret_node(%2) node_offset:1:7 to :1:8
        \\  }) (lbrace=1:1,rbrace=1:8) node_offset:1:1 to :1:9
        \\  %4 = print(%1)
        \\})
    );
}

test "global used as local" {
    try failZir("{[]a::1;a:1}",
        \\test:1:9: error: global variable 'a' used as local
        \\{[]a::1;a:1}
        \\        ^
        \\test:1:4: note: global variable declared here
        \\{[]a::1;a:1}
        \\   ^
    );
    try failZir("{[]a:a::1}",
        \\test:1:4: error: global variable 'a' used as local
        \\{[]a:a::1}
        \\   ^
        \\test:1:6: note: global variable declared here
        \\{[]a:a::1}
        \\     ^
    );

    try failZir(
        \\{[]
        \\  a::1;
        \\  a:1;
        \\  }
    ,
        \\test:3:3: error: global variable 'a' used as local
        \\  a:1;
        \\  ^
        \\test:2:3: note: global variable declared here
        \\  a::1;
        \\  ^
    );
}

test "misleading global assign" {
    try warnZir("{[]x::x:1}",
        \\test:1:5: warn: misleading global-assign of local variable 'x'
        \\{[]x::x:1}
        \\   ~^~~~~
        \\test:1:7: note: local variable declared here
        \\{[]x::x:1}
        \\      ^
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("x") token_offset:1:7 to :1:8
        \\    %3 = apply(@assign, %2, @one) node_offset:1:7 to :1:10
        \\    %4 = apply(@assign, %2, %3) node_offset:1:4 to :1:10
        \\    %5 = ret_node(%4) node_offset:1:4 to :1:10
        \\  }) (lbrace=1:1,rbrace=1:10) node_offset:1:1 to :1:11
        \\  %6 = print(%1)
        \\})
    );
    try warnZir("{[x]x::1}",
        \\test:1:6: warn: misleading global-assign of function parameter 'x'
        \\{[x]x::1}
        \\    ~^~~
        \\test:1:3: note: function parameter declared here
        \\{[x]x::1}
        \\  ^
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = apply(@assign, %2, @one) node_offset:1:5 to :1:9
        \\    %4 = ret_node(%3) node_offset:1:5 to :1:9
        \\  }) (lbrace=1:1,rbrace=1:9) node_offset:1:1 to :1:10
        \\  %5 = print(%1)
        \\})
    );
    // TODO: unused function parameter.
    try warnZir("{x::1}",
        \\test:1:3: warn: misleading global-assign of function parameter 'x'
        \\{x::1}
        \\ ~^~~
        \\test:1:2: note: function parameter declared here
        \\{x::1}
        \\ ^
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_implicit(@x) token_offset:1:2 to :1:3
        \\    %3 = apply(@assign, %2, @one) node_offset:1:2 to :1:6
        \\    %4 = ret_node(%3) node_offset:1:2 to :1:6
        \\  }) (lbrace=1:1,rbrace=1:6) node_offset:1:1 to :1:7
        \\  %5 = print(%1)
        \\})
    );
    try warnZir(
        \\{[]
        \\  a:1;
        \\  a::1;
        \\  }
    ,
        \\test:3:4: warn: misleading global-assign of local variable 'a'
        \\  a::1;
        \\  ~^~~
        \\test:2:3: note: local variable declared here
        \\  a:1;
        \\  ^
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("a") token_offset:2:3 to :2:4
        \\    %3 = apply(@assign, %2, @one) node_offset:2:3 to :2:6
        \\    %4 = apply(@assign, %2, @one) node_offset:3:3 to :3:7
        \\    %5 = ret_implicit(@null) token_offset:4:3 to :4:4
        \\  }) (lbrace=1:1,rbrace=4:3) node_offset:1:1 to :1:2
        \\  %6 = print(%1)
        \\})
    );
}

test "unreachable code" {
    try warnZir(
        \\{[]
        \\  :1;
        \\  1}
    ,
        \\test:3:3: warn: unreachable code
        \\  1}
        \\  ^
        \\test:2:3: note: control flow is diverted here
        \\  :1;
        \\  ^~
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = ret_node(@one) node_offset:2:3 to :2:5
        \\    %3 = ret_node(@one) node_offset:3:3 to :3:4
        \\  }) (lbrace=1:1,rbrace=3:4) node_offset:1:1 to :1:2
        \\  %4 = print(%1)
        \\})
    );
    try warnZir(
        \\{[]
        \\  'break;
        \\  1}
    ,
        \\test:3:3: warn: unreachable code
        \\  1}
        \\  ^
        \\test:2:3: note: control flow is diverted here
        \\  'break;
        \\  ^~~~~~
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("break") token_offset:2:4 to :2:9
        \\    %3 = signal(%2) node_offset:2:3 to :2:9
        \\    %4 = ret_node(@one) node_offset:3:3 to :3:4
        \\  }) (lbrace=1:1,rbrace=3:4) node_offset:1:1 to :1:2
        \\  %5 = print(%1)
        \\})
    );
    try warnZir(
        \\'break;
        \\1
    ,
        \\test:2:1: warn: unreachable code
        \\1
        \\^
        \\test:1:1: note: control flow is diverted here
        \\'break;
        \\^~~~~~
        \\%0 = file({
        \\  %1 = identifier("break") token_offset:1:2 to :1:7
        \\  %2 = signal(%1) node_offset:1:1 to :1:7
        \\  %3 = print(@one)
        \\})
    );
}
