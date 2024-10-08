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
    assert(node > 0);

    const tree = r.tree;
    const main_tokens: []Token.Index = tree.nodes.items(.main_token);
    const node_tags: []Ast.Node.Tag = tree.nodes.items(.tag);
    const datas: []Ast.Node.Data = tree.nodes.items(.data);
    const token_tags: []Token.Tag = tree.tokens.items(.tag);

    const tag = node_tags[node];
    switch (tag) {
        .root,
        => unreachable,

        .empty,
        => {
            switch (space) {
                .none => {},
                .space => return renderOnlySpace(r, space),
                .newline => return renderOnlySpace(r, space),
                .comma => {
                    const main_token = main_tokens[node];
                    if (token_tags[main_token] == .comma) {
                        return renderToken(r, main_token, .none);
                    } else {
                        return renderOnlySpace(r, .space);
                    }
                },
                .semicolon => {
                    const main_token = main_tokens[node];
                    if (token_tags[main_token] == .semicolon) {
                        return renderToken(r, main_token, .none);
                    }
                },
                .semicolon_newline => {
                    const main_token = main_tokens[node];
                    if (token_tags[main_token] == .semicolon) {
                        try renderToken(r, main_token, .none);
                    }
                    return renderOnlySpace(r, .newline);
                },
            }
        },

        .null,
        => return renderToken(r, main_tokens[node], space),

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

        .list => return renderList(r, node, space),

        .table_literal => return renderTable(r, node, space),

        .lambda,
        .lambda_semicolon,
        => return renderLambda(r, node, space),

        .expr_block,
        => return renderExprBlock(r, node, space),

        .assign,
        .global_assign,
        => {
            const args = datas[node];
            try renderExpression(r, args.lhs, .none);
            try renderTokenSpace(r, main_tokens[node]);
            return renderExpression(r, args.rhs, space);
        },

        .colon,
        .plus,
        .plus_colon,
        .minus,
        .minus_colon,
        .asterisk,
        .asterisk_colon,
        .percent,
        .percent_colon,
        .ampersand,
        .ampersand_colon,
        .pipe,
        .pipe_colon,
        .caret,
        .caret_colon,
        .equal,
        .equal_colon,
        .angle_bracket_left,
        .angle_bracket_left_colon,
        .angle_bracket_left_equal,
        .angle_bracket_left_right,
        .angle_bracket_right,
        .angle_bracket_right_colon,
        .angle_bracket_right_equal,
        .dollar,
        .dollar_colon,
        .comma,
        .comma_colon,
        .hash,
        .hash_colon,
        .underscore,
        .underscore_colon,
        .tilde,
        .tilde_colon,
        .bang,
        .bang_colon,
        .question_mark,
        .question_mark_colon,
        .at,
        .at_colon,
        .period,
        .period_colon,
        .zero_colon,
        .zero_colon_colon,
        .one_colon,
        .one_colon_colon,
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

        .call,
        => return renderCall(r, node, space),

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

        .select,
        => return renderSelect(r, node, space),

        .exec,
        => {
            const exec = r.tree.fullExec(node);

            try renderTokenSpace(r, exec.exec_token); // exec
            return renderSqlCommon(r, exec, space);
        },

        .update,
        => {
            const update = r.tree.fullUpdate(node);

            try renderTokenSpace(r, update.update_token); // update
            return renderSqlCommon(r, update, space);
        },

        .delete_rows,
        => {
            const delete = r.tree.fullDeleteRows(node);

            try renderTokenSpace(r, delete.delete_token); // delete
            return renderSqlCommon(r, delete, space);
        },

        .delete_cols,
        => {
            const delete = r.tree.fullDeleteCols(node);

            try renderTokenSpace(r, delete.delete_token); // delete
            return renderSqlCommon(r, delete, space);
        },

        .do,
        .@"if",
        .@"while",
        => return renderStatement(r, r.tree.fullStatement(node), space),
    }
}

fn renderList(r: *Render, node: Ast.Node.Index, space: Space) Error!void {
    const tree = r.tree;
    const ais = r.ais;
    const gpa = r.gpa;
    const tags: []Ast.Node.Tag = tree.nodes.items(.tag);
    const main_tokens: []Token.Index = tree.nodes.items(.main_token);
    const datas: []Ast.Node.Data = tree.nodes.items(.data);
    const token_tags: []Token.Tag = tree.tokens.items(.tag);

    const l_paren = main_tokens[node];
    assert(token_tags[l_paren] == .l_paren);
    const r_paren = datas[node].rhs;
    assert(token_tags[r_paren] == .r_paren);
    const extra = tree.extraData(datas[node].lhs, Ast.Node.SubRange);
    const elements = tree.extra_data[extra.start..extra.end];
    assert(elements.len > 1);

    const contains_comment = hasComment(tree, l_paren, r_paren);

    if (!contains_comment and tree.tokensOnSameLine(l_paren, r_paren)) {
        // Render all on one line
        try renderToken(r, l_paren, .none);

        for (elements[0 .. elements.len - 1]) |expr| {
            try renderExpression(r, expr, .semicolon);
        }
        try renderExpression(r, elements[elements.len - 1], .none);

        return renderToken(r, r_paren, space);
    }

    const should_indent = ais.indent_count == 0;
    if (should_indent) ais.pushIndentNextLine();
    defer if (should_indent) ais.popIndent();
    try renderToken(r, l_paren, .newline);

    var expr_index: usize = 0;
    while (true) {
        const row_size = rowSize(tree, elements[expr_index..], r_paren);
        const row_exprs = elements[expr_index..];
        // A place to store the width of each expression and its column's maximum
        const widths = try gpa.alloc(usize, row_exprs.len + row_size);
        defer gpa.free(widths);
        @memset(widths, 0);

        const expr_newlines = try gpa.alloc(bool, row_exprs.len);
        defer gpa.free(expr_newlines);
        @memset(expr_newlines, false);

        const expr_widths = widths[0..row_exprs.len];
        const column_widths = widths[row_exprs.len..];

        // TODO: Test with comments.
        // Find next row with trailing comment (if any) to end the current section.
        const section_end = sec_end: {
            var this_line_first_expr: usize = 0;
            var this_line_size = rowSize(tree, row_exprs, r_paren);
            for (row_exprs, 0..) |expr, i| {
                // Ignore comment on first line of this section.
                if (i == 0) continue;
                const expr_last_token = tree.lastToken(expr);
                if (tree.tokensOnSameLine(tree.firstToken(row_exprs[0]), expr_last_token))
                    continue;
                // Track start of line containing comment.
                if (!tree.tokensOnSameLine(tree.firstToken(row_exprs[this_line_first_expr]), expr_last_token)) {
                    this_line_first_expr = i;
                    this_line_size = rowSize(tree, row_exprs[this_line_first_expr..], r_paren);
                }

                const maybe_semicolon = if (tags[expr] == .empty)
                    expr_last_token
                else
                    expr_last_token + 1;
                if (token_tags[maybe_semicolon] == .semicolon) {
                    if (hasSameLineComment(tree, maybe_semicolon))
                        break :sec_end i - this_line_size + 1;
                }
            }
            break :sec_end row_exprs.len;
        };
        expr_index += section_end;

        const section_exprs = row_exprs[0..section_end];

        var sub_expr_buffer = std.ArrayList(u8).init(gpa);
        defer sub_expr_buffer.deinit();

        const sub_expr_buffer_starts = try gpa.alloc(usize, section_exprs.len + 1);
        defer gpa.free(sub_expr_buffer_starts);

        var auto_indenting_stream = Ais{
            .indent_delta = indent_delta,
            .underlying_writer = sub_expr_buffer.writer(),
        };
        var sub_render: Render = .{
            .gpa = r.gpa,
            .ais = &auto_indenting_stream,
            .tree = r.tree,
            .fixups = r.fixups,
        };

        // Calculate size of columns in current section
        var column_counter: usize = 0;
        var single_line = true;
        var contains_newline = false;
        for (section_exprs, 0..) |expr, i| {
            const start = sub_expr_buffer.items.len;
            sub_expr_buffer_starts[i] = start;

            if (i + 1 < section_exprs.len) {
                try renderExpression(&sub_render, expr, .none);
                const width = sub_expr_buffer.items.len - start;
                const this_contains_newline = mem.indexOfScalar(u8, sub_expr_buffer.items[start..], '\n') != null;
                contains_newline = contains_newline or this_contains_newline;
                expr_widths[i] = width;
                expr_newlines[i] = this_contains_newline;

                if (!this_contains_newline) {
                    const column = column_counter % row_size;
                    column_widths[column] = @max(column_widths[column], width);

                    const expr_last_token = if (tags[expr] == .empty)
                        tree.lastToken(expr)
                    else
                        tree.lastToken(expr) + 1;
                    const next_expr = section_exprs[i + 1];
                    column_counter += 1;
                    if (!tree.tokensOnSameLine(expr_last_token, tree.firstToken(next_expr))) single_line = false;
                } else {
                    single_line = false;
                    column_counter = 0;
                }
            } else {
                try renderExpression(&sub_render, expr, .none);
                const width = sub_expr_buffer.items.len - start;
                const this_contains_newline = mem.indexOfScalar(u8, sub_expr_buffer.items[start..][0..width], '\n') != null;
                contains_newline = contains_newline or this_contains_newline;
                expr_widths[i] = width;
                expr_newlines[i] = contains_newline;

                if (!contains_newline) {
                    const column = column_counter % row_size;
                    column_widths[column] = @max(column_widths[column], width);
                }
            }
        }
        sub_expr_buffer_starts[section_exprs.len] = sub_expr_buffer.items.len;

        // Render exprs in current section.
        column_counter = 0;
        for (section_exprs, 0..) |expr, i| {
            const start = sub_expr_buffer_starts[i];
            const end = sub_expr_buffer_starts[i + 1];
            const expr_text = sub_expr_buffer.items[start..end];
            if (!expr_newlines[i]) {
                try ais.writer().writeAll(expr_text);
            } else {
                var by_line = std.mem.splitScalar(u8, expr_text, '\n');
                var last_line_was_empty = false;
                try ais.writer().writeAll(by_line.first());
                while (by_line.next()) |line| {
                    if (std.mem.startsWith(u8, line, "/") and last_line_was_empty) {
                        try ais.insertNewline();
                    } else {
                        try ais.maybeInsertNewline();
                    }
                    last_line_was_empty = (line.len == 0);
                    try ais.writer().writeAll(line);
                }
            }

            if (i + 1 < section_exprs.len) {
                const next_expr = section_exprs[i + 1];
                const semicolon = if (tags[expr] == .empty)
                    tree.lastToken(expr)
                else
                    tree.lastToken(expr) + 1;

                if (column_counter != row_size - 1) {
                    if (!expr_newlines[i] and !expr_newlines[i + 1]) {
                        // Neither the current or next expression is multiline
                        assert(column_widths[column_counter % row_size] >= expr_widths[i]);
                        const padding = column_widths[column_counter % row_size] - expr_widths[i];
                        try ais.writer().writeByteNTimes(' ', padding);
                        try renderToken(r, semicolon, .none);

                        column_counter += 1;
                        continue;
                    }
                }

                if (single_line and row_size != 1) {
                    unreachable;
                    // try renderToken(r, semicolon, .none);
                    // continue;
                }

                column_counter = 0;
                try renderToken(r, semicolon, .newline);
                try renderExtraNewline(r, next_expr);
            }
        }

        if (expr_index == elements.len)
            break;
    }

    // ais.popIndent();
    return renderToken(r, r_paren, space); // rbrace
}

// TODO: Render multiline table expression
fn renderTable(r: *Render, node: Ast.Node.Index, space: Space) Error!void {
    const tree = r.tree;
    const ais = r.ais;
    _ = ais; // autofix
    const gpa = r.gpa;
    _ = gpa; // autofix
    const tags: []Ast.Node.Tag = tree.nodes.items(.tag);
    _ = tags; // autofix
    const main_tokens: []Token.Index = tree.nodes.items(.main_token);
    const datas: []Ast.Node.Data = tree.nodes.items(.data);
    const token_tags: []Token.Tag = tree.tokens.items(.tag);

    const l_paren = main_tokens[node];
    assert(token_tags[l_paren] == .l_paren);
    const r_paren = datas[node].rhs;
    assert(token_tags[r_paren] == .r_paren);
    const table: Ast.Node.Table = tree.extraData(datas[node].lhs, Ast.Node.Table);
    const keys = tree.extra_data[table.keys_start..table.keys_end];
    const columns = tree.extra_data[table.columns_start..table.columns_end];
    assert(columns.len > 0);

    const contains_comment = hasComment(tree, l_paren, r_paren);

    if (!contains_comment and tree.tokensOnSameLine(l_paren, r_paren)) {
        // Render all on one line
        try renderToken(r, l_paren, .none);

        try renderToken(r, l_paren + 1, .none);
        for (keys) |key| {
            try renderExpression(r, key, .semicolon);
        }
        const last_key_token = if (keys.len > 0) tree.lastToken(keys[keys.len - 1]) else l_paren + 1;
        try renderToken(r, last_key_token + 1, .none);

        for (columns) |column| {
            try renderExpression(r, column, .semicolon);
        }

        return renderToken(r, r_paren, space);
    }

    unreachable;
}

fn renderLambda(r: *Render, node: Ast.Node.Index, space: Space) Error!void {
    const tree = r.tree;
    const main_tokens: []Token.Index = tree.nodes.items(.main_token);
    const node_tags: []Ast.Node.Tag = tree.nodes.items(.tag);
    const datas: []Ast.Node.Data = tree.nodes.items(.data);
    const token_tags: []Token.Tag = tree.tokens.items(.tag);

    const lambda: Ast.Node.Lambda = tree.extraData(datas[node].lhs, Ast.Node.Lambda);
    const l_brace = main_tokens[node];
    assert(token_tags[l_brace] == .l_brace);
    const r_brace = datas[node].rhs;
    assert(token_tags[r_brace] == .r_brace);
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

fn renderExprBlock(r: *Render, node: Ast.Node.Index, space: Space) Error!void {
    const tree = r.tree;
    const ais = r.ais;
    const main_tokens: []Token.Index = tree.nodes.items(.main_token);
    const datas: []Ast.Node.Data = tree.nodes.items(.data);
    const token_tags: []Token.Tag = tree.tokens.items(.tag);

    const l_bracket = main_tokens[node];
    assert(token_tags[l_bracket] == .l_bracket);
    const r_bracket = datas[node].rhs;
    assert(token_tags[r_bracket] == .r_bracket);

    if (datas[node].lhs == 0) {
        try renderToken(r, l_bracket, .none);
        return renderToken(r, r_bracket, space);
    }

    const extra = tree.extraData(datas[node].lhs, Ast.Node.SubRange);
    const params = tree.extra_data[extra.start..extra.end];
    assert(params.len > 0);

    if (tree.tokensOnSameLine(l_bracket, r_bracket)) {
        try renderToken(r, l_bracket, .none);
        for (params, 0..) |param_node, i| {
            try renderExpression(r, param_node, if (i + 1 < params.len) .semicolon else .none);
        }
        return renderToken(r, r_bracket, space);
    }

    const should_indent = ais.indent_count == 0;
    if (should_indent) ais.pushIndentNextLine();
    defer if (should_indent) ais.popIndent();

    try renderToken(r, l_bracket, .newline);
    for (params, 0..) |param_node, i| {
        try renderExpression(r, param_node, if (i + 1 < params.len) .semicolon_newline else .none);
    }
    return renderToken(r, r_bracket, space);
}

fn renderCall(r: *Render, node: Ast.Node.Index, space: Space) Error!void {
    const tree = r.tree;
    const ais = r.ais;
    const tags: []Ast.Node.Tag = tree.nodes.items(.tag);

    const call = tree.fullCall(node);

    try renderExpression(r, call.func, .none);

    if (tree.tokensOnSameLine(call.l_bracket, call.r_bracket)) {
        try renderToken(r, call.l_bracket, .none); // [

        for (call.args) |arg_node| {
            try renderExpression(r, arg_node, if (tags[arg_node] == .empty) .none else .semicolon);
        }

        return renderToken(r, call.r_bracket, space); // ]
    }

    const should_indent = ais.indent_count == 0;
    if (should_indent) ais.pushIndentNextLine();
    defer if (should_indent) ais.popIndent();

    try renderToken(r, call.l_bracket, .newline); // [

    for (call.args) |arg_node| {
        const first_param_token = tree.firstToken(arg_node);
        if (hasSameLineComment(tree, first_param_token - 1)) {
            ais.pushIndentOneShot();
        }
        try renderExpression(r, arg_node, .semicolon_newline);
    }

    return renderToken(r, call.r_bracket, space); // ]
}

fn renderSelect(r: *Render, node: Ast.Node.Index, space: Space) Error!void {
    const select = r.tree.fullSelect(node);

    try renderTokenSpace(r, select.select_token); // select
    if (select.limit) |limit| {
        try renderToken(r, limit.l_bracket, .none); // [
        if (limit.expr) |expr| try renderExpression(r, expr, .semicolon);
        if (limit.order_token) |tok| {
            try renderToken(r, tok - 1, .none); // < / >
            try renderToken(r, tok, .none);
        }
        try renderTokenSpace(r, limit.r_bracket); // ]
    }

    if (select.distinct_token) |tok| try renderTokenSpace(r, tok); // distinct

    return renderSqlCommon(r, select, space);
}

fn renderSqlCommon(r: *Render, data: anytype, space: Space) Error!void {
    if (@hasField(@TypeOf(data), "select")) {
        for (data.select) |expr| {
            try renderExpression(r, expr, .comma);
        }
    }

    if (@hasField(@TypeOf(data), "by")) {
        if (data.by) |by| {
            try renderTokenSpace(r, by.by_token); // by
            for (by.exprs) |expr| {
                try renderExpression(r, expr, .comma);
            }
        }
    }

    try renderToken(r, data.from_token, .space); // from

    if (@hasField(@TypeOf(data), "where")) {
        if (data.where) |where| {
            try renderExpression(r, data.from, .space);

            try renderTokenSpace(r, where.where_token); // where
            for (where.exprs[0 .. where.exprs.len - 1]) |expr| {
                try renderExpression(r, expr, .comma);
            }

            return renderExpression(r, where.exprs[where.exprs.len - 1], space);
        }
    }

    return renderExpression(r, data.from, space);
}

fn renderStatement(r: *Render, data: Ast.full.Statement, space: Space) Error!void {
    try renderTokenSpace(r, data.main_token); // do/if/while

    try renderToken(r, data.l_bracket, .none); // [

    try renderExpression(r, data.condition, .semicolon);

    for (data.body) |expr| {
        try renderExpression(r, expr, .semicolon);
    }

    return renderToken(r, data.r_bracket, space); // ]
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
    /// If the next token is a comma, render it as well. If not, insert a space.
    comma,
    /// Additionally consume the next token if it is a semicolon.
    semicolon,
    /// Additionally consume the next token if it is a semicolon.
    /// In either case, a newline will be inserted afterwards.
    semicolon_newline,
};

fn renderToken(r: *Render, token_index: Token.Index, space: Space) Error!void {
    const tree = r.tree;
    const ais = r.ais;
    const lexeme = tree.tokenSlice(token_index);
    try ais.writer().writeAll(lexeme);
    try renderSpace(r, token_index, lexeme.len, space);
}

fn renderSpace(r: *Render, token_index: Token.Index, lexeme_len: usize, space: Space) Error!void {
    const tree = r.tree;
    const ais = r.ais;
    const token_tags = tree.tokens.items(.tag);
    const token_locs = tree.tokens.items(.loc);

    const token_start = token_locs[token_index].start;

    // Ensure comments before the start of a new block aren't indented.
    const is_eob = isEob(r, token_index);
    if (is_eob) ais.popIndent();
    defer if (is_eob) ais.pushIndent();

    const comment = try renderComments(r, token_start + lexeme_len, token_locs[token_index + 1].start);
    switch (space) {
        .none => {},
        .space => if (!comment) try ais.writer().writeByte(' '),
        .newline => if (!comment) try ais.insertNewline(),

        .comma => if (token_tags[token_index + 1] == .comma) {
            try renderToken(r, token_index + 1, .none);
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
    }
}

fn isEob(r: *Render, token_index: Token.Index) bool {
    const tree = r.tree;
    const locs: []Token.Loc = tree.tokens.items(.loc);

    const next_token_start = locs[token_index + 1].start;
    return next_token_start == tree.source.len or tree.source[next_token_start - 1] == '\n';
}

fn renderOnlySpace(r: *Render, space: Space) Error!void {
    const ais = r.ais;
    switch (space) {
        .none => {},
        .space => try ais.writer().writeByte(' '),
        .newline => try ais.insertNewline(),
        .comma => try ais.writer().writeAll(","),
        .semicolon => try ais.writer().writeByte(';'),
        .semicolon_newline => try ais.writer().writeAll(";\n"),
    }
}

/// Returns true if there exists a line comment between any of the tokens from
/// `start_token` to `end_token`. This is used to determine if e.g. a
/// fn_proto should be wrapped and have a trailing comma inserted even if
/// there is none in the source.
fn hasComment(tree: Ast, start_token: Ast.Token.Index, end_token: Ast.Token.Index) bool {
    const token_locs = tree.tokens.items(.loc);

    var i = start_token;
    while (i < end_token) : (i += 1) {
        const start = token_locs[i].start + tree.tokenSlice(i).len;
        const end = token_locs[i + 1].start;
        if (mem.indexOf(u8, tree.source[start..end], "/") != null) return true;
    }

    return false;
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
        => tags[token2] == .number_literal or tags[token2] == .identifier or tags[token2].isKeyword(),

        inline else => |t| t.isKeyword() and
            (tags[token2] == .number_literal or tags[token2] == .identifier or tags[token2].isKeyword()),
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
        token_locs[token_index - 1].start + tree.tokenSlice(token_index - 1).len;

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

fn hasSameLineComment(tree: Ast, token_index: Ast.Token.Index) bool {
    const token_locs = tree.tokens.items(.loc);
    const between_source = tree.source[token_locs[token_index].start..token_locs[token_index + 1].start];
    for (between_source) |byte| switch (byte) {
        '\n' => return false,
        '/' => return true,
        else => continue,
    };
    return false;
}

fn writeFixingWhitespace(writer: std.ArrayList(u8).Writer, slice: []const u8) Error!void {
    for (slice) |byte| switch (byte) {
        '\t' => try writer.writeAll(" " ** indent_delta),
        '\r' => {},
        else => try writer.writeByte(byte),
    };
}

// Returns the number of nodes in `exprs` that are on the same line as `rtoken`.
fn rowSize(tree: Ast, exprs: []const Ast.Node.Index, rtoken: Ast.Token.Index) usize {
    const first_token = tree.firstToken(exprs[0]);
    if (tree.tokensOnSameLine(first_token, rtoken)) {
        return exprs.len; // no newlines
    }

    var count: usize = 1;
    for (exprs, 0..) |expr, i| {
        if (i + 1 < exprs.len) {
            const expr_last_token = tree.lastToken(expr);
            const next_token = tree.firstToken(exprs[i + 1]);
            if (next_token != rtoken and
                !tree.tokensOnSameLine(expr_last_token, next_token)) return count;
            count += 1;
        } else {
            return count;
        }
    }
    unreachable;
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
