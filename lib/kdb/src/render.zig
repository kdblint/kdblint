const std = @import("std");
const assert = std.debug.assert;
const mem = std.mem;
const Allocator = std.mem.Allocator;
const meta = std.meta;
const kdb = @import("root.zig");
const Ast = kdb.Ast;
const Token = kdb.Token;

const indent_delta = 2;

pub const Error = Ast.RenderError;

const Ais = AutoIndentingStream(std.ArrayList(u8).Writer);

pub const Fixups = struct {};

const Render = struct {
    gpa: Allocator,
    ais: *Ais,
    tree: Ast,
    fixups: Fixups,
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
    };

    const blocks = tree.rootDecls();

    // Render everything up until the first token
    const end = r.tree.tokens.items(.loc)[0].start;
    const untrimmed_comment = r.tree.source[0..end];
    const trimmed_comment = mem.trimRight(u8, untrimmed_comment, &std.ascii.whitespace);
    if (trimmed_comment.len > 0) {
        try r.ais.writer().print("{s}\n", .{trimmed_comment});
        if (mem.containsAtLeast(u8, r.tree.source[trimmed_comment.len..end], 2, "\n")) {
            try r.ais.insertNewline();
        }
    }

    try renderBlocks(&r, blocks);

    if (auto_indenting_stream.disabled_offset) |disabled_offset| {
        try writeFixingWhitespace(auto_indenting_stream.underlying_writer, tree.source[disabled_offset..]);
    }
}

/// Render all expressions in the slice, keeping empty lines where appropriate
fn renderBlocks(r: *Render, blocks: []const Ast.Node.Index) Error!void {
    if (blocks.len == 0) return;
    if (blocks.len == 1) return renderBlock(r, blocks[0], .semicolon_newline);

    if (r.tree.tokensOnSameLine(r.tree.lastToken(blocks[0]), r.tree.firstToken(blocks[1]))) {
        try renderBlock(r, blocks[0], .semicolon);
    } else {
        try renderBlock(r, blocks[0], .semicolon_newline);
    }

    for (blocks[1 .. blocks.len - 1], blocks[2..]) |block, next_block| {
        if (r.tree.tokensOnSameLine(r.tree.lastToken(block), r.tree.firstToken(next_block))) {
            try renderBlock(r, block, .semicolon);
        } else {
            try renderExtraNewline(r, block);
            try renderBlock(r, block, .semicolon_newline);
        }
    }
    if (!r.tree.tokensOnSameLine(r.tree.lastToken(blocks[blocks.len - 2]), r.tree.firstToken(blocks[blocks.len - 1]))) {
        try r.ais.maybeInsertNewline();
    }
    try renderBlock(r, blocks[blocks.len - 1], .semicolon_newline);
}

fn renderBlock(r: *Render, block: Ast.Node.Index, space: Space) Error!void {
    r.ais.pushIndentNextLine();
    defer r.ais.popIndent();

    return renderExpression(r, block, space);
}

fn renderExpression(r: *Render, node: Ast.Node.Index, space: Space) Error!void {
    const tree = r.tree;
    const ais = r.ais;
    const main_tokens: []Token.Index = tree.nodes.items(.main_token);
    const node_tags: []Ast.Node.Tag = tree.nodes.items(.tag);
    const datas: []Ast.Node.Data = tree.nodes.items(.data);
    switch (node_tags[node]) {
        .root,
        => unreachable,

        .grouped_expression,
        => {
            try renderToken(r, main_tokens[node], .none);
            try renderExpression(r, datas[node].lhs, .none);
            return renderToken(r, datas[node].rhs, space);
        },

        .empty_list => {
            try renderToken(r, main_tokens[node], .none);
            return renderToken(r, datas[node].rhs, space);
        },

        // TODO: Complex formatting.
        .list => {
            try renderToken(r, main_tokens[node], .none);
            const extra = tree.extraData(datas[node].lhs, Ast.Node.SubRange);
            const exprs = tree.extra_data[extra.start..extra.end];
            if (exprs[0] > 0) try renderExpression(r, exprs[0], .none);
            for (exprs[1..]) |expr| {
                try ais.writer().writeByte(';');
                if (expr > 0) try renderExpression(r, expr, .none);
            }
            return renderToken(r, datas[node].rhs, space);
        },

        .lambda,
        .lambda_semicolon,
        => return renderLambda(r, node, space),

        .expr_block,
        => {
            try renderToken(r, main_tokens[node], .none);
            if (datas[node].lhs > 0) {
                const sub_range = tree.extraData(datas[node].lhs, Ast.Node.SubRange);
                const exprs = tree.extra_data[sub_range.start..sub_range.end];
                if (exprs.len > 1) {
                    if (tree.tokensOnSameLine(tree.firstToken(exprs[0]), tree.firstToken(exprs[1]))) {
                        for (exprs) |expr| {
                            try renderExpression(r, expr, .semicolon);
                        }
                    } else {
                        for (exprs) |expr| {
                            try ais.maybeInsertNewline();
                            try renderExpression(r, expr, .semicolon);
                        }
                    }
                } else {
                    for (exprs) |expr| {
                        try renderExpression(r, expr, .semicolon);
                    }
                }
            }
            try renderToken(r, datas[node].rhs, space);
        },

        .assign,
        => {
            const args = datas[node];
            try renderExpression(r, args.lhs, .none);
            try renderTokenSpace(r, main_tokens[node]);
            return renderExpression(r, args.rhs, space);
        },

        .plus,
        .minus,
        .asterisk,
        .percent,
        .ampersand,
        .pipe,
        .caret,
        .equal,
        .angle_bracket_left,
        .angle_bracket_left_equal,
        .angle_bracket_left_right,
        .angle_bracket_right,
        .angle_bracket_right_equal,
        .dollar,
        .comma,
        .hash,
        .underscore,
        .tilde,
        .bang,
        .question_mark,
        .at,
        .period,
        .zero_colon,
        .one_colon,
        .two_colon,
        => return renderToken(r, main_tokens[node], space),

        .apostrophe,
        .apostrophe_colon,
        .slash,
        .slash_colon,
        .backslash,
        .backslash_colon,
        => {
            const op = datas[node].lhs;
            if (op > 0) try renderExpression(r, op, .none);
            return renderToken(r, main_tokens[node], space);
        },

        .apply_unary,
        => {
            const infix = datas[node];
            try renderExpressionSpace(r, infix.lhs);
            return renderExpression(r, infix.rhs, space);
        },

        .apply_binary,
        => {
            const op_node: Ast.Node.Index = main_tokens[node];
            const args = datas[node];
            try renderExpressionSpace(r, args.lhs);
            if (args.rhs == 0) {
                return renderExpression(r, op_node, space);
            } else {
                try renderExpressionSpace(r, op_node);
                return renderExpression(r, args.rhs, space);
            }
        },

        .number_literal,
        .string_literal,
        .symbol_literal,
        .identifier,
        => return renderToken(r, main_tokens[node], space),

        .number_list_literal,
        => {
            for (main_tokens[node]..datas[node].lhs) |token| {
                try renderToken(r, @intCast(token), .space);
            }
            return renderToken(r, datas[node].lhs, space);
        },

        .symbol_list_literal,
        => {
            for (main_tokens[node]..datas[node].lhs) |token| {
                try renderToken(r, @intCast(token), .none);
            }
            return renderToken(r, datas[node].lhs, space);
        },
    }
}

fn renderLambda(r: *Render, node: Ast.Node.Index, space: Space) Error!void {
    const tree = r.tree;
    const ais = r.ais;
    _ = ais; // autofix
    const main_tokens: []Token.Index = tree.nodes.items(.main_token);
    const node_tags: []Ast.Node.Tag = tree.nodes.items(.tag);
    const datas: []Ast.Node.Data = tree.nodes.items(.data);
    const token_tags: []Token.Tag = tree.tokens.items(.tag);

    const lambda = tree.extraData(datas[node].lhs, Ast.Node.Lambda);
    const l_brace = main_tokens[node];
    const r_brace = datas[node].rhs;
    const l_bracket = lambda.l_bracket;
    const r_bracket = lambda.r_bracket;
    const has_params = l_bracket > 0;
    assert((r_bracket > 0) == has_params);
    const has_semicolon = node_tags[node] == .lambda_semicolon;
    const body = tree.extra_data[lambda.body_start..lambda.body_end];

    if (has_params) {
        // lambda has params.
        try renderToken(r, l_brace, .none);
        try renderToken(r, l_bracket, .none);

        const params = tree.extra_data[lambda.params_start..lambda.params_end];
        switch (params.len) {
            0 => {},
            1 => try renderExpression(r, params[0], .none),
            else => if (tree.tokensOnSameLine(tree.firstToken(params[0]), tree.firstToken(params[1]))) {
                // Params are on the same line.
                for (params) |expr| {
                    try renderExpression(r, expr, .semicolon);
                }
            } else {
                // Params are not on the same line.
                for (params[0 .. params.len - 1]) |expr| {
                    try renderExpression(r, expr, .semicolon_newline);
                }
                try renderExpression(r, params[params.len - 1], .none);
            },
        }

        if (tree.tokensOnSameLine(r_bracket, r_brace)) {
            // r_bracket ']' and r_brace '}' are on the same line.
            const params_space: Space = if (tree.mode == .q and
                body.len > 0 and
                token_tags[tree.firstToken(body[0])] == .number_literal and
                tree.tokensOnSameLine(r_bracket, tree.firstToken(body[0])) and
                tree.tokenSlice(tree.firstToken(body[0]))[0] == '-') .space else .none;
            try renderToken(r, r_bracket, params_space);

            switch (body.len) {
                0 => {},
                1 => try renderExpression(r, body[0], .semicolon),
                else => {
                    for (body) |expr| {
                        try renderExpression(r, expr, .semicolon);
                    }
                },
            }
            return renderToken(r, r_brace, space);
        } else {
            // r_bracket ']' and r_brace '}' are not on the same line.
            try renderToken(r, r_bracket, .newline);

            switch (body.len) {
                0 => {},
                1 => try renderExpression(
                    r,
                    body[0],
                    if (has_semicolon) .semicolon_newline else .semicolon,
                ),
                else => if (tree.tokensOnSameLine(tree.firstToken(body[0]), tree.firstToken(body[1]))) {
                    // Body is on the same line.
                    for (body) |expr| {
                        try renderExpression(r, expr, .semicolon);
                    }
                } else {
                    // Body is not on the same line.
                    for (body[0 .. body.len - 1]) |expr| {
                        try renderExpression(r, expr, .semicolon_newline);
                    }
                    try renderExpression(
                        r,
                        body[body.len - 1],
                        if (has_semicolon) .semicolon_newline else .none,
                    );
                },
            }
            return renderToken(r, r_brace, space);
        }
    } else {
        // Lambda does not have params.
        try renderToken(r, l_brace, .none);

        switch (body.len) {
            0 => {},
            1 => try renderExpression(r, body[0], .semicolon),
            else => {
                for (body) |expr| {
                    try renderExpression(r, expr, .semicolon);
                }
            },
        }
        return renderToken(r, r_brace, space);
    }
}

fn renderTokenSpace(r: *Render, token: Token.Index) Error!void {
    const space = getSpace(r, token, token + 1);
    return renderToken(r, token, space);
}

fn renderExpressionSpace(r: *Render, node: Ast.Node.Index) Error!void {
    const token = r.tree.lastToken(node);
    const space = getSpace(r, token, token + 1);
    return renderExpression(r, node, space);
}

fn getSpace(r: *Render, token1: Token.Index, token2: Token.Index) Space {
    return switch (r.tree.tokensOnSameLine(token1, token2)) {
        true => switch (needsSpace(r, token1, token2)) {
            true => .space,
            false => .none,
        },
        false => .newline,
    };
}

const Space = enum {
    /// Output the token lexeme only.
    none,
    /// Output the token lexeme followed by a single space.
    space,
    /// Output the token lexeme followed by a newline.
    newline,
    /// If the next token is a comma, render it as well. If not, insert one.
    /// In either case, a newline will be inserted afterwards.
    comma,
    /// Additionally consume the next token if it is a comma.
    /// In either case, a space will be inserted afterwards.
    comma_space,
    /// Additionally consume the next token if it is a semicolon.
    semicolon,
    /// Additionally consume the next token if it is a semicolon.
    /// In either case, a newline will be inserted afterwards.
    semicolon_newline,
    /// Skip rendering whitespace and comments. If this is used, the caller
    /// *must* handle whitespace and comments manually.
    skip,
};

fn renderToken(r: *Render, token_index: Token.Index, space: Space) Error!void {
    const tree = r.tree;
    const ais = r.ais;
    const lexeme = tokenSliceForRender(tree, token_index);
    try ais.writer().writeAll(lexeme);
    try renderSpace(r, token_index, lexeme.len, space);
}

fn renderSpace(r: *Render, token_index: Token.Index, lexeme_len: usize, space: Space) Error!void {
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
            try renderToken(r, token_index + 1, .newline);
        } else if (!comment) {
            try ais.insertNewline();
        },

        .comma_space => if (token_tags[token_index + 1] == .comma) {
            try renderToken(r, token_index + 1, .space);
        } else if (!comment) {
            try ais.writer().writeByte(' ');
        },

        .semicolon => if (token_tags[token_index + 1] == .semicolon) {
            try renderToken(r, token_index + 1, .none);
        },

        .semicolon_newline => if (token_tags[token_index + 1] == .semicolon) {
            try renderToken(r, token_index + 1, .newline);
        } else if (!comment) {
            try ais.insertNewline();
        },

        .skip => unreachable,
    }
}

const State = enum {
    start,
    line_comment,
    block_comment_start,
    block_comment,
    block_comment_end,
};

const Comment = struct {
    start: usize,
    end: usize,
    is_line: bool,
};

/// Assumes that start is the first byte past the previous token and
/// that end is the last byte before the next token.
fn renderComments(r: *Render, start: usize, end: usize) Error!bool {
    if (start == end) return false;

    const tree = r.tree;
    const ais = r.ais;

    var comments = std.ArrayList(Comment).init(r.gpa);
    defer comments.deinit();

    // TODO: Capture unterminated block comments
    var index = start;
    var comment_start: usize = 0;
    state: switch (State.start) {
        .start => {
            switch (tree.source[index]) {
                '/' => {
                    comment_start = index;
                    if (tree.source[index - 1] == '\n') {
                        continue :state .block_comment_start;
                    } else continue :state .line_comment;
                },
                '\\' => {
                    try comments.append(.{
                        .start = index,
                        .end = tree.source.len,
                        .is_line = false,
                    });
                },
                else => if (index < end) {
                    index += 1;
                    continue :state .start;
                },
            }
        },

        .line_comment => {
            if (index < end) {
                index += 1;
                switch (tree.source[index]) {
                    0, '\n' => {
                        try comments.append(.{
                            .start = comment_start,
                            .end = index,
                            .is_line = true,
                        });
                        continue :state .start;
                    },
                    else => continue :state .line_comment,
                }
            }
        },

        .block_comment_start => {
            if (index < end) {
                index += 1;
                switch (tree.source[index]) {
                    ' ', '\t', '\r' => continue :state .block_comment_start,
                    '\n' => continue :state .block_comment,
                    else => continue :state .line_comment,
                }
            }
        },

        .block_comment => {
            if (index < end) {
                index += 1;
                switch (tree.source[index]) {
                    '\\' => if (tree.source[index - 1] == '\n') {
                        continue :state .block_comment_end;
                    } else continue :state .block_comment,
                    else => continue :state .block_comment,
                }
            }
        },

        .block_comment_end => {
            if (index < end) {
                index += 1;
                switch (tree.source[index]) {
                    ' ', '\t', '\r' => continue :state .block_comment_end,
                    0, '\n' => {
                        try comments.append(.{
                            .start = comment_start,
                            .end = index,
                            .is_line = false,
                        });
                        continue :state .start;
                    },
                    else => continue :state .block_comment,
                }
            }
        },
    }

    if (comments.items.len == 0) return false;

    if (mem.containsAtLeast(u8, tree.source[start..comments.items[0].start], 2, "\n")) {
        // Leave up to one empty line before the first comment
        try ais.insertNewline();
        try ais.insertNewline();
    } else if (mem.indexOfScalar(u8, tree.source[start..comments.items[0].start], '\n') != null) {
        // Respect the newline directly before the comment.
        // Note: This allows an empty line between comments
        try ais.insertNewline();
    } else {
        // Otherwise if the first comment is on the same line as
        // the token before it, prefix it with a single space.
        try ais.writer().writeByte(' ');
    }

    for (comments.items, 0..) |comment, i| {
        if (i > 0 and mem.indexOfScalar(u8, tree.source[comments.items[i - 1].end + 1 .. comment.start], '\n') != null) {
            // Respect the newline directly before the comment.
            // Note: This allows an empty line between comments
            try ais.insertNewline();
        }

        // Write the comment minus any trailing whitespace.
        const trimmed_comment = mem.trimRight(u8, tree.source[comment.start..comment.end], &std.ascii.whitespace);
        if (comment.is_line) {
            try ais.writer().print("{s}\n", .{trimmed_comment});
        } else {
            _ = try ais.writeNoIndent(trimmed_comment);
            _ = try ais.writeNoIndent("\n");
        }
    }

    if (mem.containsAtLeast(u8, tree.source[comments.items[comments.items.len - 1].end..end], 2, "\n")) {
        // Don't leave any whitespace at the end of the file
        if (end != tree.source.len) {
            try ais.insertNewline();
        }
    }

    return true;
}

fn needsSpace(r: *Render, token1: Token.Index, token2: Token.Index) bool {
    const tags: []Token.Tag = r.tree.tokens.items(.tag);
    return switch (tags[token1]) {
        .r_paren,
        .r_brace,
        .r_bracket,
        => tags[token2] == .number_literal and r.tree.tokenSlice(token2)[0] == '-',

        .number_literal,
        => tags[token2] == .identifier,

        .symbol_literal,
        => switch (tags[token2]) {
            .colon,
            .colon_colon,
            .period,
            .period_colon,
            .zero_colon,
            .zero_colon_colon,
            .one_colon,
            .one_colon_colon,
            .two_colon,
            .number_literal,
            .symbol_literal,
            .identifier,
            => true,

            // Depends on language.
            .underscore,
            .underscore_colon,
            => r.tree.mode == .q and r.tree.tokenLen(token1) > 1,

            else => false,
        },

        .identifier,
        => tags[token2] == .number_literal or tags[token2] == .identifier,

        else => false,
    };
}

fn renderExtraNewline(r: *Render, node: Ast.Node.Index) Error!void {
    return renderExtraNewlineToken(r, r.tree.firstToken(node));
}

/// Check if there is an empty line immediately before the given token. If so, render it.
fn renderExtraNewlineToken(r: *Render, token_index: Token.Index) Error!void {
    const tree = r.tree;
    const ais = r.ais;
    const token_locs = tree.tokens.items(.loc);
    const token_start = token_locs[token_index].start;
    if (token_start == 0) return;
    const prev_token_end = if (token_index == 0)
        0
    else
        token_locs[token_index - 1].start + tokenSliceForRender(tree, token_index - 1).len;

    // If there is a immediately preceding comment or doc_comment,
    // skip it because required extra newline has already been rendered.
    if (mem.indexOf(u8, tree.source[prev_token_end..token_start], "//") != null) return;

    // Iterate backwards to the end of the previous token, stopping if a
    // non-whitespace character is encountered or two newlines have been found.
    var i = token_start - 1;
    var newlines: u2 = 0;
    while (std.ascii.isWhitespace(tree.source[i])) : (i -= 1) {
        if (tree.source[i] == '\n') newlines += 1;
        if (newlines == 2) return ais.insertNewline();
        if (i == prev_token_end) break;
    }
}

fn tokenSliceForRender(tree: Ast, token_index: Token.Index) []const u8 {
    return tree.tokenSlice(token_index);
}

fn writeFixingWhitespace(writer: std.ArrayList(u8).Writer, slice: []const u8) Error!void {
    for (slice) |byte| switch (byte) {
        '\t' => try writer.writeAll(" " ** indent_delta),
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
