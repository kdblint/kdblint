const std = @import("std");
const assert = std.debug.assert;
const mem = std.mem;
const Allocator = std.mem.Allocator;
const meta = std.meta;
const kdb = @import("../kdb.zig");
const Ast = kdb.Ast;
const Token = kdb.Token;
const primitives = kdb.primitives;
const panic = std.debug.panic;

const indent_delta = 4;
const asm_indent_delta = 2;

const log = std.log.scoped(.kdbLint_render);

pub const Error = Ast.RenderError;

const Ais = AutoIndentingStream(std.ArrayList(u8).Writer);

pub const Fixups = struct {
    /// The key is the mut token (`var`/`const`) of the variable declaration
    /// that should have a `_ = foo;` inserted afterwards.
    unused_var_decls: std.AutoHashMapUnmanaged(Ast.TokenIndex, void) = .{},
    /// The functions in this unordered set of AST fn decl nodes will render
    /// with a function body of `@trap()` instead, with all parameters
    /// discarded.
    gut_functions: std.AutoHashMapUnmanaged(Ast.Node.Index, void) = .{},
    /// These global declarations will be omitted.
    omit_nodes: std.AutoHashMapUnmanaged(Ast.Node.Index, void) = .{},
    /// These expressions will be replaced with the string value.
    replace_nodes_with_string: std.AutoHashMapUnmanaged(Ast.Node.Index, []const u8) = .{},
    /// The string value will be inserted directly after the node.
    append_string_after_node: std.AutoHashMapUnmanaged(Ast.Node.Index, []const u8) = .{},
    /// These nodes will be replaced with a different node.
    replace_nodes_with_node: std.AutoHashMapUnmanaged(Ast.Node.Index, Ast.Node.Index) = .{},
    /// Change all identifier names matching the key to be value instead.
    rename_identifiers: std.StringArrayHashMapUnmanaged([]const u8) = .{},

    /// All `@import` builtin calls which refer to a file path will be prefixed
    /// with this path.
    rebase_imported_paths: ?[]const u8 = null,

    pub fn count(f: Fixups) usize {
        return f.unused_var_decls.count() +
            f.gut_functions.count() +
            f.omit_nodes.count() +
            f.replace_nodes_with_string.count() +
            f.append_string_after_node.count() +
            f.replace_nodes_with_node.count() +
            f.rename_identifiers.count() +
            @intFromBool(f.rebase_imported_paths != null);
    }

    pub fn clearRetainingCapacity(f: *Fixups) void {
        f.unused_var_decls.clearRetainingCapacity();
        f.gut_functions.clearRetainingCapacity();
        f.omit_nodes.clearRetainingCapacity();
        f.replace_nodes_with_string.clearRetainingCapacity();
        f.append_string_after_node.clearRetainingCapacity();
        f.replace_nodes_with_node.clearRetainingCapacity();
        f.rename_identifiers.clearRetainingCapacity();

        f.rebase_imported_paths = null;
    }

    pub fn deinit(f: *Fixups, gpa: Allocator) void {
        f.unused_var_decls.deinit(gpa);
        f.gut_functions.deinit(gpa);
        f.omit_nodes.deinit(gpa);
        f.replace_nodes_with_string.deinit(gpa);
        f.append_string_after_node.deinit(gpa);
        f.replace_nodes_with_node.deinit(gpa);
        f.rename_identifiers.deinit(gpa);
        f.* = undefined;
    }
};

const Render = struct {
    gpa: Allocator,
    ais: *Ais,
    tree: Ast,
    fixups: Fixups,
    prev_token: Ast.TokenIndex,
};

pub fn renderTree(buffer: *std.ArrayList(u8), tree: Ast, fixups: Fixups) Error!void {
    assert(tree.errors.len == 0); // Cannot render an invalid tree.
    var auto_indenting_stream = Ais{
        .indent_delta = indent_delta,
        .underlying_writer = buffer.writer(),
    };
    var r: Render = .{
        .gpa = buffer.allocator,
        .ais = &auto_indenting_stream,
        .tree = tree,
        .fixups = fixups,
        .prev_token = 0,
    };

    try renderBlocks(&r, r.tree.rootDecls());

    if (auto_indenting_stream.disabled_offset) |disabled_offset| {
        try writeFixingWhitespace(auto_indenting_stream.underlying_writer, tree.source[disabled_offset..]);
    }
}

fn renderBlocks(r: *Render, blocks: []const Ast.Node.Index) Error!void {
    try renderBlock(r, blocks[0], .newline);
    for (blocks[1..]) |block| {
        try renderExtraNewline(r, block);
        try renderBlock(r, block, .newline);
    }
}

fn renderBlock(r: *Render, decl: Ast.Node.Index, space: Space) Error!void {
    const tree = r.tree;
    const node_tags: []Ast.Node.Tag = tree.nodes.items(.tag);
    const datas = tree.nodes.items(.data);
    switch (node_tags[decl]) {
        .implicit_return => try renderExpression(r, datas[decl].lhs, space),
        else => try renderExpression(r, decl, .semicolon_newline),
    }
}

// TODO: Render comments in weird places.
fn renderExpression(r: *Render, node: Ast.Node.Index, space: Space) Error!void {
    const tree = r.tree;
    const ais = r.ais;
    const token_locs: []Token.Loc = tree.tokens.items(.loc);
    const main_tokens: []Ast.TokenIndex = tree.nodes.items(.main_token);
    const node_tags: []Ast.Node.Tag = tree.nodes.items(.tag);
    const datas: []Ast.Node.Data = tree.nodes.items(.data);
    switch (node_tags[node]) {
        .number_literal => {
            const main_token = main_tokens[node];
            const loc = token_locs[main_token];
            if (tree.source[loc.start] == '-') {
                const items = ais.underlying_writer.context.items;
                switch (items[items.len - 1]) {
                    ')', '}', ']' => try ais.writer().writeByte(' '),
                    else => {},
                }
            }
            return renderToken(r, main_token, space);
        },
        .assign,
        .add,
        .subtract,
        .multiply,
        .divide,
        .dict,
        .lesser,
        => {
            const infix = datas[node];
            try renderExpression(r, infix.lhs, .none);
            try renderToken(r, main_tokens[node], .none);
            return renderExpression(r, infix.rhs, space);
        },
        .minus => {
            try renderToken(r, node, space);
        },
        inline .lambda_one, .lambda_one_semicolon, .lambda, .lambda_semicolon => |t| {
            const top = ais.underlying_writer.context.items.len;
            try renderLambda(r, false, t, node, space);
            const len = ais.underlying_writer.context.items.len - top;
            if (len > 30) {
                ais.underlying_writer.context.shrinkRetainingCapacity(top);
                r.prev_token = main_tokens[node];
                try renderLambda(r, true, t, node, space);
            }
        },
        .implicit_apply => {
            const data = datas[node];
            try renderExpression(r, data.lhs, .none);
            try renderExpression(r, data.rhs, space);
        },
        .identifier => try renderToken(r, main_tokens[node], space),
        .select => {
            try renderToken(r, main_tokens[node], .space);

            const select = tree.extraData(datas[node].rhs, Ast.Node.Select);
            const column_exprs = tree.extra_data[select.select..select.select_end];
            if (column_exprs.len > 0) {
                const column_names = tree.table_columns[select.select_columns .. select.select_columns + column_exprs.len];
                for (column_names[0 .. column_names.len - 1], column_exprs[0 .. column_exprs.len - 1]) |column, expr| {
                    try ais.writer().writeAll(column);
                    try ais.writer().writeByte(':');
                    try renderExpression(r, expr, .comma);
                }

                try ais.writer().writeAll(column_names[column_names.len - 1]);
                try ais.writer().writeByte(':');
                try renderExpression(r, column_exprs[column_exprs.len - 1], .space);
            }

            const by_exprs = tree.extra_data[select.by..select.select];
            if (by_exprs.len > 0) {
                try ais.writer().writeAll("by ");
                const by_names = tree.table_columns[select.by_columns .. select.by_columns + by_exprs.len];
                for (by_names[0 .. by_names.len - 1], by_exprs[0 .. by_exprs.len - 1]) |column, expr| {
                    try ais.writer().writeAll(column);
                    try ais.writer().writeByte(':');
                    try renderExpression(r, expr, .comma);
                }
                try ais.writer().writeAll(by_names[by_names.len - 1]);
                try ais.writer().writeByte(':');
                try renderExpression(r, by_exprs[by_exprs.len - 1], .space);
            }

            try ais.writer().writeAll("from ");
            const where_exprs = tree.extra_data[select.where..select.by];
            if (where_exprs.len > 0) {
                try renderExpression(r, select.from, .space);

                try ais.writer().writeAll("where ");
                for (where_exprs[0 .. where_exprs.len - 1]) |expr| {
                    try renderExpression(r, expr, .comma);
                }
                try renderExpression(r, where_exprs[where_exprs.len - 1], space);
            } else {
                try renderExpression(r, select.from, space);
            }
        },
        else => |t| panic("renderExpression: {s}", .{@tagName(t)}),
    }
}

fn renderLambda(r: *Render, comptime multi_line: bool, comptime tag: Ast.Node.Tag, node: Ast.Node.Index, space: Space) !void {
    const tree = r.tree;
    const ais = r.ais;
    const main_tokens: []Ast.TokenIndex = tree.nodes.items(.main_token);
    const datas: []Ast.Node.Data = tree.nodes.items(.data);

    try renderToken(r, main_tokens[node], .none);

    const data = datas[node];
    if (nextTag(r, .l_bracket)) |l_bracket| {
        try renderToken(r, l_bracket, .none);
    } else {
        try ais.writer().writeByte('[');
    }
    if (data.lhs > 0) {
        const params = tree.extraData(data.lhs, Ast.Node.Params);
        const tokens = tree.extra_data[params.start..params.end];
        try renderToken(r, tokens[0], .none);
        for (tokens[1..]) |token| {
            try ais.writer().writeByte(';');
            try renderToken(r, token, .none);
        }
    }
    if (nextTag(r, .r_bracket)) |r_bracket| {
        try renderToken(r, r_bracket, .none);
    } else {
        try ais.writer().writeByte(']');
    }

    const has_body = if (tag == .lambda or tag == .lambda_semicolon) true else data.rhs > 0;
    if (has_body) {
        if (multi_line) {
            try ais.insertNewline();
            ais.pushIndent();
        }
        if (tag == .lambda or tag == .lambda_semicolon) {
            const sub_range = tree.extraData(data.rhs, Ast.Node.SubRange);
            const exprs = tree.extra_data[sub_range.start..sub_range.end];
            try renderExpression(r, exprs[0], .none);
            for (exprs[1..]) |expr| {
                try ais.writer().writeByte(';');
                if (multi_line) try ais.insertNewline();
                try renderExpression(r, expr, .none);
            }
        } else {
            try renderExpression(r, data.rhs, .none);
        }
    }

    if (nextTag(r, .semicolon)) |semicolon| {
        if (has_body) try renderToken(r, semicolon, if (multi_line) .newline else .none);
    }

    if (nextTag(r, .r_brace)) |r_brace| {
        try renderToken(r, r_brace, space);
    }

    if (multi_line and has_body) ais.popIndent();
}

fn nextTag(r: *Render, comptime expected_tag: Token.Tag) ?Ast.TokenIndex {
    const token_tags: []Token.Tag = r.tree.tokens.items(.tag);
    var i = r.prev_token + 1;
    while (true) : (i += 1) {
        if (token_tags[i] == expected_tag) return i;
        switch (token_tags[i]) {
            .comment, .semicolon => continue,
            else => return null,
        }
    }
}

fn renderToken(r: *Render, token_index: Ast.TokenIndex, space: Space) Error!void {
    const tree = r.tree;
    const ais = r.ais;
    const lexeme = tokenSliceForRender(tree, token_index);
    try ais.writer().writeAll(lexeme);
    try renderSpace(r, token_index, lexeme.len, space);
    r.prev_token = token_index;
}

fn renderSpace(r: *Render, token_index: Ast.TokenIndex, lexeme_len: usize, space: Space) Error!void {
    const tree = r.tree;
    const ais = r.ais;
    const token_tags = tree.tokens.items(.tag);
    const token_locs = tree.tokens.items(.loc);

    const token_start = token_locs[token_index].start;

    if (space == .skip) return;

    if (space == .comma and token_tags[token_index + 1] != .comma) {
        try ais.writer().writeByte(',');
    }

    const comment = try renderComments(r, token_start + lexeme_len, token_locs[token_index + 1].start);
    switch (space) {
        .none => {},
        .space => if (!comment) try ais.writer().writeByte(' '),
        .newline => if (!comment) try ais.insertNewline(),

        .comma => if (token_tags[token_index + 1] == .comma) {
            try renderToken(r, token_index + 1, .none);
        } else if (!comment) {
            try ais.writer().writeByte(',');
        },
        .comma_space => if (token_tags[token_index + 1] == .comma) {
            try renderToken(r, token_index + 1, .space);
        } else if (!comment) {
            try ais.writer().writeByte(' ');
        },
        .comma_newline => if (token_tags[token_index + 1] == .comma) {
            try renderToken(r, token_index + 1, .newline);
        } else if (!comment) {
            try ais.insertNewline();
        },

        .semicolon => if (token_tags[token_index + 1] == .semicolon) {
            try renderToken(r, token_index + 1, .none);
        } else if (!comment) {
            try ais.writer().writeByte(';');
        },
        .semicolon_newline => if (token_tags[token_index + 1] == .semicolon) {
            try renderToken(r, token_index + 1, .newline);
        } else if (!comment) {
            try ais.insertNewline();
        },

        .skip => unreachable,
    }
}

const Space = enum {
    /// Output the token lexeme only.
    none,
    /// Output the token lexeme followed by a single space.
    space,
    /// Output the token lexeme followed by a newline.
    newline,
    /// If the next token is a comma, render it as well. If not, insert one.
    comma,
    /// Additionally consume the next token if it is a comma.
    /// In either case, a space will be inserted afterwards.
    comma_space,
    /// If the next token is a comma, render it as well. If not, insert one.
    /// In either case, a newline will be inserted afterwards.
    comma_newline,
    /// Additionally consume the next token if it is a semicolon.
    semicolon,
    /// Additionally consume the next token if it is a semicolon.
    /// In either case, a newline will be inserted afterwards.
    semicolon_newline,
    /// Skip rendering whitespace and comments. If this is used, the caller
    /// *must* handle whitespace and comments manually.
    skip,
};

/// Assumes that start is the first byte past the previous token and
/// that end is the last byte before the next token.
fn renderComments(r: *Render, start: usize, end: usize) Error!bool {
    const tree = r.tree;
    const ais = r.ais;

    var index: usize = start;
    while (mem.indexOf(u8, tree.source[index..end], "//")) |offset| {
        const comment_start = index + offset;

        // If there is no newline, the comment ends with EOF
        const newline_index = mem.indexOfScalar(u8, tree.source[comment_start..end], '\n');
        const newline = if (newline_index) |i| comment_start + i else null;

        const untrimmed_comment = tree.source[comment_start .. newline orelse tree.source.len];
        const trimmed_comment = mem.trimRight(u8, untrimmed_comment, &std.ascii.whitespace);

        // Don't leave any whitespace at the start of the file
        if (index != 0) {
            if (index == start and mem.containsAtLeast(u8, tree.source[index..comment_start], 2, "\n")) {
                // Leave up to one empty line before the first comment
                try ais.insertNewline();
                try ais.insertNewline();
            } else if (mem.indexOfScalar(u8, tree.source[index..comment_start], '\n') != null) {
                // Respect the newline directly before the comment.
                // Note: This allows an empty line between comments
                try ais.insertNewline();
            } else if (index == start) {
                // Otherwise if the first comment is on the same line as
                // the token before it, prefix it with a single space.
                try ais.writer().writeByte(' ');
            }
        }

        index = 1 + (newline orelse end - 1);

        const comment_content = mem.trimLeft(u8, trimmed_comment["//".len..], &std.ascii.whitespace);
        if (ais.disabled_offset != null and mem.eql(u8, comment_content, "zig fmt: on")) {
            // Write the source for which formatting was disabled directly
            // to the underlying writer, fixing up invalid whitespace.
            const disabled_source = tree.source[ais.disabled_offset.?..comment_start];
            try writeFixingWhitespace(ais.underlying_writer, disabled_source);
            // Write with the canonical single space.
            try ais.underlying_writer.writeAll("// zig fmt: on\n");
            ais.disabled_offset = null;
        } else if (ais.disabled_offset == null and mem.eql(u8, comment_content, "zig fmt: off")) {
            // Write with the canonical single space.
            try ais.writer().writeAll("// zig fmt: off\n");
            ais.disabled_offset = index;
        } else {
            // Write the comment minus trailing whitespace.
            try ais.writer().print("{s}\n", .{trimmed_comment});
        }
    }

    if (index != start and mem.containsAtLeast(u8, tree.source[index - 1 .. end], 2, "\n")) {
        // Don't leave any whitespace at the end of the file
        if (end != tree.source.len) {
            try ais.insertNewline();
        }
    }

    return index != start;
}

fn renderExtraNewline(r: *Render, node: Ast.Node.Index) Error!void {
    return renderExtraNewlineToken(r, r.tree.firstToken(node));
}

/// Check if there is an empty line immediately before the given token. If so, render it.
fn renderExtraNewlineToken(r: *Render, token_index: Ast.TokenIndex) Error!void {
    const tree = r.tree;
    const ais = r.ais;
    const token_locs = tree.tokens.items(.loc);
    const token_loc = token_locs[token_index];
    if (token_loc.start == 0) return;
    const prev_token_end = if (token_index == 0)
        0
    else
        token_locs[token_index - 1].end;

    // If there is a immediately preceding comment,
    // skip it because required extra newline has already been rendered.
    if (token_index > 0 and tree.tokens.items(.tag)[token_index - 1] == .comment) return;

    // Iterate backwards to the end of the previous token, stopping if a
    // non-whitespace character is encountered or two newlines have been found.
    var i = token_loc.start - 1;
    var newlines: u2 = 0;
    while (std.ascii.isWhitespace(tree.source[i])) : (i -= 1) {
        if (tree.source[i] == '\n') newlines += 1;
        if (newlines == 2) return ais.insertNewline();
        if (i == prev_token_end) break;
    }
}

fn tokenSliceForRender(tree: Ast, token_index: Ast.TokenIndex) []const u8 {
    return tree.tokenSlice(token_index);
}

fn writeFixingWhitespace(writer: std.ArrayList(u8).Writer, slice: []const u8) Error!void {
    for (slice) |byte| switch (byte) {
        '\t' => try writer.writeAll(" " ** 4),
        '\r' => {},
        else => try writer.writeByte(byte),
    };
}

/// Automatically inserts indentation of written data by keeping
/// track of the current indentation level
fn AutoIndentingStream(comptime UnderlyingWriter: type) type {
    return struct {
        const Self = @This();
        pub const WriteError = UnderlyingWriter.Error;
        pub const Writer = std.io.Writer(*Self, WriteError, write);

        underlying_writer: UnderlyingWriter,

        /// Offset into the source at which formatting has been disabled with
        /// a `zig fmt: off` comment.
        ///
        /// If non-null, the AutoIndentingStream will not write any bytes
        /// to the underlying writer. It will however continue to track the
        /// indentation level.
        disabled_offset: ?usize = null,

        indent_count: usize = 0,
        indent_delta: usize,
        current_line_empty: bool = true,
        /// automatically popped when applied
        indent_one_shot_count: usize = 0,
        /// the most recently applied indent
        applied_indent: usize = 0,
        /// not used until the next line
        indent_next_line: usize = 0,

        pub fn writer(self: *Self) Writer {
            return .{ .context = self };
        }

        pub fn write(self: *Self, bytes: []const u8) WriteError!usize {
            if (bytes.len == 0)
                return @as(usize, 0);

            try self.applyIndent();
            return self.writeNoIndent(bytes);
        }

        // Change the indent delta without changing the final indentation level
        pub fn setIndentDelta(self: *Self, new_indent_delta: usize) void {
            if (self.indent_delta == new_indent_delta) {
                return;
            } else if (self.indent_delta > new_indent_delta) {
                assert(self.indent_delta % new_indent_delta == 0);
                self.indent_count = self.indent_count * (self.indent_delta / new_indent_delta);
            } else {
                // assert that the current indentation (in spaces) in a multiple of the new delta
                assert((self.indent_count * self.indent_delta) % new_indent_delta == 0);
                self.indent_count = self.indent_count / (new_indent_delta / self.indent_delta);
            }
            self.indent_delta = new_indent_delta;
        }

        fn writeNoIndent(self: *Self, bytes: []const u8) WriteError!usize {
            if (bytes.len == 0)
                return @as(usize, 0);

            if (self.disabled_offset == null) try self.underlying_writer.writeAll(bytes);
            if (bytes[bytes.len - 1] == '\n')
                self.resetLine();
            return bytes.len;
        }

        pub fn insertNewline(self: *Self) WriteError!void {
            _ = try self.writeNoIndent("\n");
        }

        fn resetLine(self: *Self) void {
            self.current_line_empty = true;
            self.indent_next_line = 0;
        }

        /// Insert a newline unless the current line is blank
        pub fn maybeInsertNewline(self: *Self) WriteError!void {
            if (!self.current_line_empty)
                try self.insertNewline();
        }

        /// Push default indentation
        /// Doesn't actually write any indentation.
        /// Just primes the stream to be able to write the correct indentation if it needs to.
        pub fn pushIndent(self: *Self) void {
            self.indent_count += 1;
        }

        /// Push an indent that is automatically popped after being applied
        pub fn pushIndentOneShot(self: *Self) void {
            self.indent_one_shot_count += 1;
            self.pushIndent();
        }

        /// Turns all one-shot indents into regular indents
        /// Returns number of indents that must now be manually popped
        pub fn lockOneShotIndent(self: *Self) usize {
            const locked_count = self.indent_one_shot_count;
            self.indent_one_shot_count = 0;
            return locked_count;
        }

        /// Push an indent that should not take effect until the next line
        pub fn pushIndentNextLine(self: *Self) void {
            self.indent_next_line += 1;
            self.pushIndent();
        }

        pub fn popIndent(self: *Self) void {
            assert(self.indent_count != 0);
            self.indent_count -= 1;

            if (self.indent_next_line > 0)
                self.indent_next_line -= 1;
        }

        /// Writes ' ' bytes if the current line is empty
        fn applyIndent(self: *Self) WriteError!void {
            const current_indent = self.currentIndent();
            if (self.current_line_empty and current_indent > 0) {
                if (self.disabled_offset == null) {
                    try self.underlying_writer.writeByteNTimes(' ', current_indent);
                }
                self.applied_indent = current_indent;
            }

            self.indent_count -= self.indent_one_shot_count;
            self.indent_one_shot_count = 0;
            self.current_line_empty = false;
        }

        /// Checks to see if the most recent indentation exceeds the currently pushed indents
        pub fn isLineOverIndented(self: *Self) bool {
            if (self.current_line_empty) return false;
            return self.applied_indent > self.currentIndent();
        }

        fn currentIndent(self: *Self) usize {
            var indent_current: usize = 0;
            if (self.indent_count > 0) {
                const indent_count = self.indent_count - self.indent_next_line;
                indent_current = indent_count * self.indent_delta;
            }
            return indent_current;
        }
    };
}

test {
    @import("std").testing.refAllDecls(@This());
}
