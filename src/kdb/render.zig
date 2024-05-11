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

const log = std.log.scoped(.kdblint_render);

pub const Error = Ast.RenderError;

const Ais = AutoIndentingStream(std.ArrayList(u8).Writer);

const Render = struct {
    gpa: Allocator,
    ais: *Ais,
    tree: Ast,
    prev_token: Ast.TokenIndex,
    settings: RenderSettings,
};

pub const RenderSettings = struct {
    explicit_function_args: bool = true,
};

pub fn renderTree(buffer: *std.ArrayList(u8), tree: Ast, settings: RenderSettings) Error!void {
    assert(tree.errors.len == 0); // Cannot render an invalid tree.
    var auto_indenting_stream = Ais{
        .indent_delta = indent_delta,
        .underlying_writer = buffer.writer(),
    };
    var r: Render = .{
        .gpa = buffer.allocator,
        .ais = &auto_indenting_stream,
        .tree = tree,
        .prev_token = 0,
        .settings = settings,
    };

    if (tree.tokens.items(.tag)[0] == .comment) {
        try r.ais.writer().writeAll(tree.tokenSlice(0));
    }

    try renderBlocks(&r, r.tree.rootDecls());

    if (auto_indenting_stream.disabled_offset) |disabled_offset| {
        try writeFixingWhitespace(auto_indenting_stream.underlying_writer, tree.source[disabled_offset..]);
    }
}

fn renderBlocks(r: *Render, blocks: []const Ast.Node.Index) Error!void {
    const tree = r.tree;
    const locs = tree.tokens.items(.loc);

    try renderBlock(r, blocks[0], .newline);
    for (blocks[1..]) |block| {
        const first_token = tree.firstToken(block);
        const source = tree.source[locs[first_token - 1].end..locs[first_token].start];
        if (std.mem.containsAtLeast(u8, source, 2, "\n")) {
            try r.ais.insertNewline();
        }

        try renderBlock(r, block, .newline);
    }
}

fn renderBlock(r: *Render, decl: Ast.Node.Index, space: Space) Error!void {
    const tree = r.tree;
    const ais = r.ais;
    const node_tags: []Ast.Node.Tag = tree.nodes.items(.tag);
    const datas = tree.nodes.items(.data);

    ais.indent_count = 0;
    ais.indent_next_line = 0;

    switch (node_tags[decl]) {
        .implicit_return => try renderExpression(r, datas[decl].lhs, space),
        else => try renderExpression(r, decl, .semicolon_newline),
    }
}

// TODO: Render comments in weird places.
fn renderExpression(r: *Render, node: Ast.Node.Index, space: Space) Error!void {
    const tree = r.tree;
    const ais = r.ais;
    const token_tags: []Token.Tag = tree.tokens.items(.tag);
    const main_tokens: []Ast.TokenIndex = tree.nodes.items(.main_token);
    const node_tags: []Ast.Node.Tag = tree.nodes.items(.tag);
    const datas: []Ast.Node.Data = tree.nodes.items(.data);
    switch (node_tags[node]) {
        .root => unreachable,

        .grouped_expression => {
            const data = datas[node];

            try renderToken(r, main_tokens[node], .none);
            try renderExpression(r, data.lhs, .none);
            try renderToken(r, data.rhs, space);
        },
        .empty_list => {
            const data = datas[node];

            try renderToken(r, main_tokens[node], .none);
            try renderToken(r, data.rhs, space);
        },
        .list => {
            const data = datas[node];

            try renderToken(r, main_tokens[node], .none);
            const sub_range = tree.extraData(data.lhs, Ast.Node.SubRange);
            const exprs = tree.extra_data[sub_range.start..sub_range.end];
            try renderExpression(r, exprs[0], .none);
            for (exprs[1..]) |expr| {
                try ais.writer().writeByte(';');
                if (expr > 0) try renderExpression(r, expr, .none);
            }
            try renderToken(r, data.rhs, space);
        },
        .table_literal => {
            const data = datas[node];
            const table = tree.extraData(data.lhs, Ast.Node.Table);

            try renderToken(r, main_tokens[node], .none);
            const key_columns = tree.table_columns[table.column_start .. table.column_start + table.key_len];
            const key_exprs = tree.extra_data[table.expr_start .. table.expr_start + table.key_len];
            try ais.writer().writeByte('[');
            if (key_columns.len > 0) {
                try ais.writer().writeAll(key_columns[0]);
                try ais.writer().writeByte(':');
                try renderExpression(r, key_exprs[0], .none);
                for (key_columns[1..], key_exprs[1..]) |column, expr| {
                    try ais.writer().writeByte(';');
                    try ais.writer().writeAll(column);
                    try ais.writer().writeByte(':');
                    try renderExpression(r, expr, .none);
                }
            }
            try ais.writer().writeByte(']');
            const columns = tree.table_columns[table.column_start + table.key_len .. table.column_start + table.key_len + table.len];
            const exprs = tree.extra_data[table.expr_start + table.key_len .. table.expr_start + table.key_len + table.len];
            try ais.writer().writeAll(columns[0]);
            try ais.writer().writeByte(':');
            try renderExpression(r, exprs[0], .none);
            for (columns[1..], exprs[1..]) |column, expr| {
                try ais.writer().writeByte(';');
                try ais.writer().writeAll(column);
                try ais.writer().writeByte(':');
                try renderExpression(r, expr, .none);
            }
            try renderToken(r, data.rhs, space);
        },

        .number_literal => try renderToken(r, main_tokens[node], space),
        .number_list_literal => {
            const data = datas[node];
            const sub_range = tree.extraData(data.lhs, Ast.Node.SubRange);
            const tokens = tree.extra_data[sub_range.start .. sub_range.end - 1];
            for (tokens) |token| {
                try renderToken(r, token, .space);
            }
            try renderToken(r, data.rhs, space);
        },
        .string_literal,
        .symbol_literal,
        .symbol_list_literal,
        .identifier,
        => try renderToken(r, main_tokens[node], space),

        .colon,
        .colon_colon,
        .plus,
        .plus_colon,
        .minus,
        .minus_colon,
        .asterisk,
        .asterisk_colon,
        .percent,
        .percent_colon,
        .bang,
        .bang_colon,
        .ampersand,
        .ampersand_colon,
        .pipe,
        .pipe_colon,
        .angle_bracket_left,
        .angle_bracket_left_colon,
        .angle_bracket_left_equal,
        .angle_bracket_left_right,
        .angle_bracket_right,
        .angle_bracket_right_colon,
        .angle_bracket_right_equal,
        .equal,
        .equal_colon,
        .tilde,
        .tilde_colon,
        .comma,
        .comma_colon,
        .caret,
        .caret_colon,
        .hash,
        .hash_colon,
        .underscore,
        .underscore_colon,
        .dollar,
        .dollar_colon,
        .question_mark,
        .question_mark_colon,
        .at,
        .at_colon,
        .dot,
        .dot_colon,
        .zero_colon,
        .zero_colon_colon,
        .one_colon,
        .one_colon_colon,
        .two_colon,
        => try renderToken(r, main_tokens[node], space),

        inline .assign,
        .global_assign,
        .add,
        .plus_assign,
        .subtract,
        .minus_assign,
        .multiply,
        .asterisk_assign,
        .divide,
        .percent_assign,
        .dict,
        .bang_assign,
        .lesser,
        .ampersand_assign,
        .greater,
        .pipe_assign,
        .less_than,
        .angle_bracket_left_assign,
        .less_than_equal,
        .not_equal,
        .greater_than,
        .angle_bracket_right_assign,
        .greater_than_equal,
        .equals,
        .equal_assign,
        .match,
        .tilde_assign,
        .join,
        .comma_assign,
        .fill,
        .caret_assign,
        .take,
        .hash_assign,
        .drop,
        .underscore_assign,
        .cast,
        .dollar_assign,
        .find,
        .question_mark_assign,
        .apply,
        .at_assign,
        .apply_n,
        .dot_assign,
        .file_text,
        .zero_colon_assign,
        .file_binary,
        .one_colon_assign,
        .dynamic_load,
        => |tag| {
            const data = datas[node];

            switch (tag) {
                .drop, .underscore_assign => {
                    try renderExpression(r, data.lhs, switch (node_tags[data.lhs]) {
                        .symbol_literal, .symbol_list_literal, .identifier => .space,
                        else => .none,
                    });
                    try renderToken(r, main_tokens[node], if (data.rhs > 0) .none else space);
                    if (data.rhs > 0) try renderExpression(r, data.rhs, space);
                },
                .apply_n, .dot_assign => {
                    try renderExpression(r, data.lhs, switch (node_tags[data.lhs]) {
                        .number_literal, .number_list_literal, .symbol_literal, .symbol_list_literal, .identifier => .space,
                        else => .none,
                    });
                    try renderToken(r, main_tokens[node], if (data.rhs > 0) switch (token_tags[Ast.firstToken(tree, data.rhs)]) {
                        .number_literal, .identifier => .space,
                        else => .none,
                    } else space);
                    if (data.rhs > 0) try renderExpression(r, data.rhs, space);
                },
                else => {
                    if (data.lhs > 0) try renderExpression(r, data.lhs, .none);
                    try renderToken(r, main_tokens[node], if (data.rhs > 0) .none else space);
                    if (data.rhs > 0) try renderExpression(r, data.rhs, space);
                },
            }
        },

        .implicit_apply => {
            const data = datas[node];

            const space_between: Space = switch (token_tags[Ast.lastToken(tree, data.lhs)]) {
                .r_paren, .r_brace, .r_bracket, .string_literal => .none,
                .number_literal,
                .identifier,
                .keyword_abs,
                .keyword_acos,
                .keyword_asin,
                .keyword_atan,
                .keyword_avg,
                .keyword_bin,
                .keyword_binr,
                .keyword_cor,
                .keyword_cos,
                .keyword_cov,
                .keyword_dev,
                .keyword_div,
                .keyword_enlist,
                .keyword_exit,
                .keyword_exp,
                .keyword_getenv,
                .keyword_hopen,
                .keyword_in,
                .keyword_insert,
                .keyword_last,
                .keyword_like,
                .keyword_log,
                .keyword_max,
                .keyword_min,
                .keyword_prd,
                .keyword_setenv,
                .keyword_sin,
                .keyword_sqrt,
                .keyword_ss,
                .keyword_sum,
                .keyword_tan,
                .keyword_var,
                .keyword_wavg,
                .keyword_within,
                .keyword_wsum,
                .keyword_xexp,
                => switch (token_tags[Ast.firstToken(tree, data.rhs)]) {
                    .l_paren, .l_brace, .string_literal, .symbol_literal, .symbol_list_literal => .none,
                    else => .space,
                },
                .symbol_literal, .symbol_list_literal => switch (token_tags[Ast.firstToken(tree, data.rhs)]) {
                    .l_paren, .l_brace, .string_literal => .none,
                    else => .space,
                },
                else => .space,
            };
            try renderExpression(r, data.lhs, space_between);
            try renderExpression(r, data.rhs, space);
        },

        .apostrophe,
        .apostrophe_colon,
        .slash,
        .slash_colon,
        .backslash,
        .backslash_colon,
        => {
            const data = datas[node];

            try renderExpression(r, data.lhs, .none);
            try renderToken(r, main_tokens[node], space);
        },

        .apostrophe_infix,
        .apostrophe_colon_infix,
        .slash_infix,
        .slash_colon_infix,
        .backslash_infix,
        .backslash_colon_infix,
        => {
            const data = datas[node];
            const iterator = tree.extraData(data.rhs, Ast.Node.Iterator);

            try renderExpression(r, iterator.lhs, .none);
            try renderExpression(r, data.lhs, .none);
            try renderToken(r, main_tokens[node], .none);
            try renderExpression(r, iterator.rhs, space);
        },

        inline .lambda,
        .lambda_semicolon,
        => |t| {
            const top = ais.underlying_writer.context.items.len;
            try renderLambda(r, false, t, node, space);
            const len = ais.underlying_writer.context.items.len - top;
            // TODO: Add config setting.
            if (len > 30) {
                ais.underlying_writer.context.shrinkRetainingCapacity(top);
                r.prev_token = main_tokens[node];
                try renderLambda(r, true, t, node, space);
            }
        },

        .block => {
            const data = datas[node];

            try renderToken(r, main_tokens[node], .none);
            const sub_range = tree.extraData(data.lhs, Ast.Node.SubRange);
            const exprs = tree.extra_data[sub_range.start..sub_range.end];
            if (exprs.len > 0) {
                if (exprs[0] > 0) try renderExpression(r, exprs[0], .none);
                for (exprs[1..]) |expr| {
                    try ais.writer().writeByte(';');
                    if (expr > 0) try renderExpression(r, expr, .none);
                }
            }
            try renderToken(r, data.rhs, space);
        },

        .call_one => {
            const data = datas[node];

            try renderExpression(r, data.lhs, .none);
            try renderToken(r, main_tokens[node], .none);
            if (data.rhs > 0) try renderExpression(r, data.rhs, .none);
            if (nextTag(r, .r_bracket)) |r_bracket| {
                try renderToken(r, r_bracket, space);
            }
        },
        .call => {
            const data = datas[node];
            const sub_range = tree.extraData(data.rhs, Ast.Node.SubRange);
            const exprs = tree.extra_data[sub_range.start..sub_range.end];

            try renderExpression(r, data.lhs, .none);
            try renderToken(r, main_tokens[node], .none);
            if (exprs[0] > 0) try renderExpression(r, exprs[0], .none);
            for (exprs[1..]) |expr| {
                try ais.writer().writeByte(';');
                if (expr > 0) try renderExpression(r, expr, .none);
            }
            if (nextTag(r, .r_bracket)) |r_bracket| {
                try renderToken(r, r_bracket, space);
            }
        },

        .implicit_return => {
            const data = datas[node];

            try renderExpression(r, data.lhs, space);
        },
        .@"return" => {
            const data = datas[node];

            try ais.writer().writeByte(':');
            try renderExpression(r, data.lhs, space);
        },

        .abs,
        .acos,
        .asin,
        .atan,
        .avg,
        .cos,
        .dev,
        .enlist,
        .exit,
        .exp,
        .getenv,
        .hopen,
        .last,
        .log,
        .max,
        .min,
        .prd,
        .sin,
        .sqrt,
        .sum,
        .tan,
        .@"var",
        => try renderToken(r, main_tokens[node], space),

        .bin,
        .binr,
        .cor,
        .cov,
        .div,
        .in,
        .insert,
        .like,
        .setenv,
        .ss,
        .wavg,
        .within,
        .wsum,
        .xexp,
        => try renderToken(r, main_tokens[node], space),
        .bin_infix,
        .binr_infix,
        .cor_infix,
        .cov_infix,
        .div_infix,
        .in_infix,
        .insert_infix,
        .like_infix,
        .setenv_infix,
        .ss_infix,
        .wavg_infix,
        .within_infix,
        .wsum_infix,
        .xexp_infix,
        => {
            const data = datas[node];

            const space_before: Space = switch (token_tags[Ast.lastToken(tree, data.lhs)]) {
                .r_paren, .r_brace, .r_bracket, .string_literal => .none,
                else => .space,
            };
            try renderExpression(r, data.lhs, space_before);
            const space_after: Space = if (data.rhs > 0) switch (token_tags[Ast.firstToken(tree, data.rhs)]) {
                .l_paren, .l_brace, .string_literal, .symbol_literal, .symbol_list_literal => .none,
                else => .space,
            } else space;
            try renderToken(r, main_tokens[node], space_after);
            if (data.rhs > 0) try renderExpression(r, data.rhs, space);
        },

        .do_one,
        .if_one,
        .while_one,
        => {
            const data = datas[node];

            try renderToken(r, main_tokens[node], .none);
            try ais.writer().writeByte('[');
            try renderExpression(r, data.lhs, .semicolon);
            if (data.rhs > 0) try renderExpression(r, data.rhs, .none);
            if (nextTag(r, .r_bracket)) |r_bracket| {
                try renderToken(r, r_bracket, space);
            }
        },
        .do,
        .@"if",
        .@"while",
        => {
            const data = datas[node];

            try renderToken(r, main_tokens[node], .none);
            try ais.writer().writeByte('[');
            try renderExpression(r, data.lhs, .semicolon);
            const sub_range = tree.extraData(data.rhs, Ast.Node.SubRange);
            const exprs = tree.extra_data[sub_range.start..sub_range.end];
            try renderExpression(r, exprs[0], .none);
            for (exprs[1..]) |expr| {
                try ais.writer().writeByte(';');
                if (expr > 0) try renderExpression(r, expr, .none);
            }
            if (nextTag(r, .r_bracket)) |r_bracket| {
                try renderToken(r, r_bracket, space);
            }
        },

        .select => {
            const data = datas[node];
            const select = tree.extraData(data.lhs, Ast.Node.Select);

            try renderToken(r, main_tokens[node], if (select.limit > 0 or select.order > 0) .none else .space);

            if (select.limit > 0 or select.order > 0) {
                try ais.writer().writeByte('[');
                if (select.limit > 0) {
                    try renderExpression(r, select.limit, if (select.order > 0) .semicolon else .none);
                }
                if (select.order > 0) {
                    if (select.data.ascending) {
                        try ais.writer().writeByte('<');
                    } else {
                        try ais.writer().writeByte('>');
                    }
                    try renderToken(r, select.order, .none);
                }
                try ais.writer().writeByte(']');
            }

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
        .exec => {
            const data = datas[node];
            const exec = tree.extraData(data.lhs, Ast.Node.Exec);

            try renderToken(r, main_tokens[node], .space);

            const column_exprs = tree.extra_data[exec.select..exec.select_end];
            if (column_exprs.len > 0) {
                if (exec.data.has_select_columns) {
                    const column_names = tree.table_columns[exec.select_columns .. exec.select_columns + column_exprs.len];
                    for (column_names[0 .. column_names.len - 1], column_exprs[0 .. column_exprs.len - 1]) |column, expr| {
                        try ais.writer().writeAll(column);
                        try ais.writer().writeByte(':');
                        try renderExpression(r, expr, .comma);
                    }

                    try ais.writer().writeAll(column_names[column_names.len - 1]);
                    try ais.writer().writeByte(':');
                    try renderExpression(r, column_exprs[column_exprs.len - 1], .space);
                } else {
                    for (column_exprs[0 .. column_exprs.len - 1]) |expr| {
                        try renderExpression(r, expr, .comma);
                    }

                    try renderExpression(r, column_exprs[column_exprs.len - 1], .space);
                }
            }

            const by_exprs = tree.extra_data[exec.by..exec.select];
            if (by_exprs.len > 0) {
                try ais.writer().writeAll("by ");
                if (exec.data.has_by_columns) {
                    const by_names = tree.table_columns[exec.by_columns .. exec.by_columns + by_exprs.len];
                    for (by_names[0 .. by_names.len - 1], by_exprs[0 .. by_exprs.len - 1]) |column, expr| {
                        try ais.writer().writeAll(column);
                        try ais.writer().writeByte(':');
                        try renderExpression(r, expr, .comma);
                    }
                    try ais.writer().writeAll(by_names[by_names.len - 1]);
                    try ais.writer().writeByte(':');
                    try renderExpression(r, by_exprs[by_exprs.len - 1], .space);
                } else {
                    for (by_exprs[0 .. by_exprs.len - 1]) |expr| {
                        try renderExpression(r, expr, .comma);
                    }
                    try renderExpression(r, by_exprs[by_exprs.len - 1], .space);
                }
            }

            try ais.writer().writeAll("from ");
            const where_exprs = tree.extra_data[exec.where..exec.by];
            if (where_exprs.len > 0) {
                try renderExpression(r, exec.from, .space);

                try ais.writer().writeAll("where ");
                for (where_exprs[0 .. where_exprs.len - 1]) |expr| {
                    try renderExpression(r, expr, .comma);
                }
                try renderExpression(r, where_exprs[where_exprs.len - 1], space);
            } else {
                try renderExpression(r, exec.from, space);
            }
        },
        .update => {
            const data = datas[node];
            const update = tree.extraData(data.lhs, Ast.Node.Update);

            try renderToken(r, main_tokens[node], .space);

            const column_exprs = tree.extra_data[update.select..update.select_end];
            if (column_exprs.len > 0) {
                const column_names = tree.table_columns[update.select_columns .. update.select_columns + column_exprs.len];
                for (column_names[0 .. column_names.len - 1], column_exprs[0 .. column_exprs.len - 1]) |column, expr| {
                    try ais.writer().writeAll(column);
                    try ais.writer().writeByte(':');
                    try renderExpression(r, expr, .comma);
                }

                try ais.writer().writeAll(column_names[column_names.len - 1]);
                try ais.writer().writeByte(':');
                try renderExpression(r, column_exprs[column_exprs.len - 1], .space);
            }

            const by_exprs = tree.extra_data[update.by..update.select];
            if (by_exprs.len > 0) {
                try ais.writer().writeAll("by ");
                const by_names = tree.table_columns[update.by_columns .. update.by_columns + by_exprs.len];
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
            const where_exprs = tree.extra_data[update.where..update.by];
            if (where_exprs.len > 0) {
                try renderExpression(r, update.from, .space);

                try ais.writer().writeAll("where ");
                for (where_exprs[0 .. where_exprs.len - 1]) |expr| {
                    try renderExpression(r, expr, .comma);
                }
                try renderExpression(r, where_exprs[where_exprs.len - 1], space);
            } else {
                try renderExpression(r, update.from, space);
            }
        },
        .delete_rows => {
            const data = datas[node];
            const delete = tree.extraData(data.lhs, Ast.Node.DeleteRows);

            try renderToken(r, main_tokens[node], .space);

            try ais.writer().writeAll("from ");
            const where_exprs = tree.extra_data[delete.where..delete.where_end];
            if (where_exprs.len > 0) {
                try renderExpression(r, delete.from, .space);

                try ais.writer().writeAll("where ");
                for (where_exprs[0 .. where_exprs.len - 1]) |expr| {
                    try renderExpression(r, expr, .comma);
                }
                try renderExpression(r, where_exprs[where_exprs.len - 1], space);
            } else {
                try renderExpression(r, delete.from, space);
            }
        },
        .delete_cols => {
            const data = datas[node];
            const delete = tree.extraData(data.lhs, Ast.Node.DeleteColumns);

            try renderToken(r, main_tokens[node], .space);

            const columns = tree.table_columns[delete.select_columns..delete.select_columns_end];
            try ais.writer().writeAll(columns[0]);
            for (columns[1..]) |column| {
                try ais.writer().writeByte(',');
                try ais.writer().writeAll(column);
            }

            try ais.writer().writeAll(" from ");
            try renderExpression(r, delete.from, space);
        },
    }
}

fn renderLambda(r: *Render, comptime multi_line: bool, comptime tag: Ast.Node.Tag, node: Ast.Node.Index, space: Space) !void {
    const tree = r.tree;
    const main_tokens: []Ast.TokenIndex = tree.nodes.items(.main_token);
    const datas: []Ast.Node.Data = tree.nodes.items(.data);

    try renderToken(r, main_tokens[node], .none);

    const data = datas[node];
    const lambda = tree.extraData(data.lhs, Ast.Node.Lambda);
    const params = tree.extra_data[lambda.params_start..lambda.params_end];
    switch (params.len) {
        0 => {},
        1 => unreachable,
        2 => {
            try renderToken(r, params[0], .none);
            try renderToken(r, params[1], .none);
        },
        else => {
            try renderToken(r, params[0], .none); // l_bracket
            for (params[1 .. params.len - 2]) |param| {
                try renderToken(r, param, .semicolon);
            }
            try renderToken(r, params[params.len - 2], .none);
            try renderToken(r, params[params.len - 1], .none); // r_bracket
        },
    }

    const body = tree.extra_data[lambda.body_start..lambda.body_end];
    if (body.len > 0) {
        for (body[0 .. body.len - 1]) |expr| {
            try renderExpression(r, expr, if (multi_line) .semicolon_newline else .semicolon);
        }
        if (tag == .lambda_semicolon) {
            try renderExpression(r, body[body.len - 1], if (multi_line) .semicolon_newline else .semicolon);
        } else {
            try renderExpression(r, body[body.len - 1], if (multi_line) .newline else .none);
        }
    }

    try renderToken(r, data.rhs, space);
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
    const lexeme = tree.tokenSlice(token_index);
    try ais.writer().writeAll(lexeme);
    try renderSpace(r, token_index, space);
    r.prev_token = token_index;
}

fn renderSpace(r: *Render, token_index: Ast.TokenIndex, space: Space) Error!void {
    const tree = r.tree;
    const ais = r.ais;
    const token_tags = tree.tokens.items(.tag);

    if (space == .skip) return;

    if (space == .comma and token_tags[token_index + 1] != .comma) {
        try ais.writer().writeByte(',');
    }

    const comment = try renderComments(r, token_index + 1);
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
fn renderComments(r: *Render, token: Ast.TokenIndex) Error!bool {
    const tree = r.tree;
    const ais = r.ais;
    const tags = tree.tokens.items(.tag);
    const locs = tree.tokens.items(.loc);
    const eobs = tree.tokens.items(.eob);

    if (tags[token] == .comment) {
        if (mem.containsAtLeast(u8, tree.source[locs[token - 1].start..locs[token].start], 2, "\n")) {
            // Respect empty lines
            try ais.insertNewline();
            try ais.insertNewline();
        } else if (mem.containsAtLeast(u8, tree.tokenSlice(token), 1, "\n")) {
            // Block comments need a new line
            try ais.insertNewline();
        } else {
            // Otherwise if the first comment is on the same line as
            // the token before it, prefix it with a single space.
            try ais.writer().writeByte(' ');
        }
    }

    var i = token;
    while (tags[i] == .comment) : (i += 1) {
        const untrimmed_comment = tree.tokenSlice(i);
        const trimmed_comment = mem.trimRight(u8, untrimmed_comment, &std.ascii.whitespace);
        try ais.writer().print("{s}\n", .{trimmed_comment});
    }

    if (i > token and !eobs[token - 1] and ais.currentIndent() == 0) {
        ais.pushIndent();
    }

    return i > token;
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

fn testRender(comptime source: [:0]const u8) !void {
    var tree = try Ast.parse(std.testing.allocator, source, .{
        .version = .@"4.0",
        .language = .q,
    });
    defer tree.deinit(std.testing.allocator);
    const actual = try Ast.render(tree, std.testing.allocator, .{
        .explicit_function_args = false,
    });
    defer std.testing.allocator.free(actual);
    try std.testing.expectEqualSlices(u8, source ++ "\n", actual);
}

test "number literals whitespace" {
    try testRender("1(1;)1"); // l_paren/r_paren
    try testRender("1{1}1"); // l_brace/r_brace
    try testRender("[1;]1"); // r_bracket
    try testRender("\"string\"1"); // string_literal
    try testRender("`symbol 1"); // symbol_literal
    try testRender("`symbol`symbol 1"); // symbol_list_literal
    try testRender("x 1"); // identifier
    const kvs = Token.keywords.kvs;
    inline for (kvs.keys[0..kvs.len], kvs.values[0..kvs.len]) |key, value| {
        switch (value) {
            .keyword_delete, .keyword_do, .keyword_exec, .keyword_if, .keyword_select, .keyword_update, .keyword_while => {},
            else => try testRender(key ++ " 1"), // keyword
        }
    }
}

test "number list literals whitespace" {
    try testRender("1 2 3(1 2 3;)1 2 3"); // l_paren/r_paren
    try testRender("1 2 3{1 2 3}1 2 3"); // l_brace/r_brace
    try testRender("[1 2 3;]1 2 3"); // r_bracket
    try testRender("\"string\"1 2 3"); // string_literal
    try testRender("`symbol 1 2 3"); // symbol_literal
    try testRender("`symbol`symbol 1 2 3"); // symbol_list_literal
    try testRender("x 1 2 3"); // identifier
    const kvs = Token.keywords.kvs;
    inline for (kvs.keys[0..kvs.len], kvs.values[0..kvs.len]) |key, value| {
        switch (value) {
            .keyword_delete, .keyword_do, .keyword_exec, .keyword_if, .keyword_select, .keyword_update, .keyword_while => {},
            else => try testRender(key ++ " 1 2 3"), // keyword
        }
    }
}

test "string literals whitespace" {
    try testRender("\"string\"(\"string\";)\"string\""); // l_paren/r_paren
    try testRender("\"string\"{\"string\"}\"string\""); // l_brace/r_brace
    try testRender("[\"string\";]\"string\""); // r_bracket
    try testRender("1\"string\""); // number_literal
    try testRender("\"string\"\"string\""); // string_literal
    try testRender("`symbol\"string\""); // symbol_literal
    try testRender("`symbol`symbol\"string\""); // symbol_list_literal
    try testRender("x\"string\""); // identifier
    const kvs = Token.keywords.kvs;
    inline for (kvs.keys[0..kvs.len], kvs.values[0..kvs.len]) |key, value| {
        switch (value) {
            .keyword_delete, .keyword_do, .keyword_exec, .keyword_if, .keyword_select, .keyword_update, .keyword_while => {},
            else => try testRender(key ++ "\"string\""), // keyword
        }
    }
}

test "symbol literals whitespace" {
    try testRender("`symbol(`symbol;)`symbol"); // l_paren/r_paren
    try testRender("`symbol{`symbol}`symbol"); // r_paren/r_brace
    try testRender("[`symbol;]`symbol"); // r_bracket
    try testRender("1`symbol"); // number_literal
    try testRender("\"string\"`symbol"); // string_literal
    try testRender("`symbol `symbol"); // symbol_literal
    try testRender("`symbol`symbol `symbol"); // symbol_list_literal
    try testRender("x`symbol"); // identifier
    const kvs = Token.keywords.kvs;
    inline for (kvs.keys[0..kvs.len], kvs.values[0..kvs.len]) |key, value| {
        switch (value) {
            .keyword_delete, .keyword_do, .keyword_exec, .keyword_if, .keyword_select, .keyword_update, .keyword_while => {},
            else => try testRender(key ++ "`symbol"), // keyword
        }
    }
}

test "symbol list literals whitespace" {
    try testRender("`symbol`symbol(`symbol`symbol;)`symbol`symbol"); // l_paren/r_paren
    try testRender("`symbol`symbol{`symbol`symbol}`symbol`symbol"); // l_brace/r_brace
    try testRender("[`symbol`symbol;]`symbol`symbol"); // r_bracket
    try testRender("1`symbol`symbol"); // number_literal
    try testRender("\"string\"`symbol"); // string_literal
    try testRender("`symbol `symbol`symbol"); // symbol_literal
    try testRender("`symbol`symbol `symbol`symbol"); // symbol_list_literal
    try testRender("x`symbol`symbol"); // identifier
    const kvs = Token.keywords.kvs;
    inline for (kvs.keys[0..kvs.len], kvs.values[0..kvs.len]) |key, value| {
        switch (value) {
            .keyword_delete, .keyword_do, .keyword_exec, .keyword_if, .keyword_select, .keyword_update, .keyword_while => {},
            else => try testRender(key ++ "`symbol`symbol"), // keyword
        }
    }
}

test "identifiers whitespace" {
    try testRender("x(x;)x"); // l_paren/r_paren
    try testRender("x{x}x"); // l_brace/r_brace
    try testRender("[x;]x"); // r_bracket
    try testRender("1 x"); // number_literal
    try testRender("\"string\"x"); // string_literal
    try testRender("`symbol x"); // symbol_literal
    try testRender("`symbol`symbol x"); // symbol_list_literal
    try testRender("x x"); // identifier
    const kvs = Token.keywords.kvs;
    inline for (kvs.keys[0..kvs.len], kvs.values[0..kvs.len]) |key, value| {
        switch (value) {
            .keyword_delete, .keyword_do, .keyword_exec, .keyword_if, .keyword_select, .keyword_update, .keyword_while => {},
            else => try testRender(key ++ " x"), // keyword
        }
    }
}

test "keywords whitespace" {
    try testRender("abs(abs;)abs"); // l_paren/r_paren
    try testRender("abs{abs}abs"); // l_brace/r_brace
    try testRender("[abs;]abs"); // r_bracket
    try testRender("1 abs"); // number_literal
    try testRender("\"string\"abs"); // string_literal
    try testRender("`symbol abs"); // symbol_literal
    try testRender("`symbol`symbol abs"); // symbol_list_literal
    try testRender("x abs"); // identifier
    @setEvalBranchQuota(2000);
    const kvs = Token.keywords.kvs;
    inline for (kvs.keys[0..kvs.len], kvs.values[0..kvs.len]) |outer_key, outer_value| {
        switch (outer_value) {
            .keyword_delete, .keyword_do, .keyword_exec, .keyword_if, .keyword_select, .keyword_update, .keyword_while => {},
            else => inline for (kvs.keys[0..kvs.len], kvs.values[0..kvs.len]) |inner_key, inner_value| {
                switch (inner_value) {
                    .keyword_delete, .keyword_do, .keyword_exec, .keyword_if, .keyword_select, .keyword_update, .keyword_while => {},
                    else => try testRender(outer_key ++ " " ++ inner_key), // keyword
                }
            },
        }
    }
}

test {
    @import("std").testing.refAllDecls(@This());
}
