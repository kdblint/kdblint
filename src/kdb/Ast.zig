//! Abstract Syntax Tree for kdb+ source code.

/// Reference to externally-owned data.
source: [:0]const u8,

tokens: TokenList.Slice,
/// The root AST node is assumed to be index 0. Since there can be no
/// references to the root node, this means 0 is available to indicate null.
nodes: NodeList.Slice,
extra_data: []Node.Index,
table_columns: [][]const u8,
values: []Value,

errors: []const Error,

tokenize_duration: u64,
parse_duration: u64,

pub const TokenIndex = u32;
pub const ByteOffset = u32;

pub const TokenList = std.MultiArrayList(Token);
pub const NodeList = std.MultiArrayList(Node);

pub const Location = struct {
    line: usize,
    column: usize,
    line_start: usize,
    line_end: usize,
};

pub fn deinit(tree: *Ast, gpa: Allocator) void {
    tree.tokens.deinit(gpa);
    tree.nodes.deinit(gpa);
    gpa.free(tree.extra_data);
    for (tree.table_columns) |slice| gpa.free(slice);
    gpa.free(tree.table_columns);
    for (tree.values) |value| value.deinit(gpa);
    gpa.free(tree.values);
    gpa.free(tree.errors);
    tree.* = undefined;
}

pub const RenderError = error{
    /// Ran out of memory allocating call stack frames to complete rendering, or
    /// ran out of memory allocating space in the output buffer.
    OutOfMemory,
};

pub const Version = enum {
    @"4.0",
};

pub const Language = enum {
    k,
    q,
};

pub const ParseSettings = struct {
    version: Version,
    language: Language,
};

/// Result should be freed with tree.deinit() when there are
/// no more references to any of the tokens or nodes.
pub fn parse(gpa: Allocator, source: [:0]const u8, settings: ParseSettings) Allocator.Error!Ast {
    const tokenize_start = diagnostics.now();
    var tokens = Ast.TokenList{};
    defer tokens.deinit(gpa);

    // Empirically, the zig std lib has an 8:1 ratio of source bytes to token count.
    const estimated_token_count = source.len / 8;
    try tokens.ensureTotalCapacity(gpa, estimated_token_count);

    var tokenizer = Tokenizer.init(source, settings.language);
    while (true) {
        const token = tokenizer.next();
        try tokens.append(gpa, token);
        if (token.tag == .eof) break;
    }
    const tokenize_duration = diagnostics.now().since(tokenize_start);

    const parse_start = diagnostics.now();
    var parser: Parse = .{
        .source = source,
        .gpa = gpa,
        .token_tags = tokens.items(.tag),
        .token_locs = tokens.items(.loc),
        .token_eobs = tokens.items(.eob),
        .version = settings.version,
        .language = settings.language,
    };
    defer parser.deinit();

    // Empirically, Zig source code has a 2:1 ratio of tokens to AST nodes.
    // Make sure at least 1 so we can use appendAssumeCapacity on the root node below.
    const estimated_node_count = (tokens.len + 2) / 2;
    try parser.nodes.ensureTotalCapacity(gpa, estimated_node_count);

    try parser.parseRoot();
    const parse_duration = diagnostics.now().since(parse_start);

    // TODO experiment with compacting the MultiArrayList slices here
    return Ast{
        .source = source,
        .tokens = tokens.toOwnedSlice(),
        .nodes = parser.nodes.toOwnedSlice(),
        .extra_data = try parser.extra_data.toOwnedSlice(gpa),
        .table_columns = try parser.table_columns.toOwnedSlice(gpa),
        .values = try parser.values.toOwnedSlice(gpa),
        .errors = try parser.errors.toOwnedSlice(gpa),
        .tokenize_duration = tokenize_duration,
        .parse_duration = parse_duration,
    };
}

/// `gpa` is used for allocating the resulting formatted source code, as well as
/// for allocating extra stack memory if needed, because this function utilizes recursion.
/// Note: that's not actually true yet, see https://github.com/ziglang/zig/issues/1006.
/// Caller owns the returned slice of bytes, allocated with `gpa`.
pub fn render(tree: Ast, gpa: Allocator, settings: RenderSettings) RenderError![]u8 {
    var buffer = std.ArrayList(u8).init(gpa);
    defer buffer.deinit();

    try tree.renderToArrayList(&buffer, settings);
    return buffer.toOwnedSlice();
}

pub fn renderToArrayList(tree: Ast, buffer: *std.ArrayList(u8), settings: RenderSettings) RenderError!void {
    return private_render.renderTree(buffer, tree, settings);
}

pub fn tokenSlice(tree: Ast, token_index: TokenIndex) []const u8 {
    const tags: []Token.Tag = tree.tokens.items(.tag);
    const locs: []Token.Loc = tree.tokens.items(.loc);

    // Many tokens can be determined entirely by their tag.
    if (tags[token_index].lexeme()) |lexeme| {
        return lexeme;
    }

    const loc = locs[token_index];
    return tree.source[loc.start..loc.end];
}

pub fn extraData(tree: Ast, index: usize, comptime T: type) T {
    const fields = std.meta.fields(T);
    var result: T = undefined;
    inline for (fields, 0..) |field, i| {
        comptime assert(switch (field.type) {
            Node.Index, Node.SelectData, Node.ExecData => true,
            else => false,
        });
        @field(result, field.name) = @bitCast(tree.extra_data[index + i]);
    }
    return result;
}

pub fn rootDecls(tree: Ast) []const Node.Index {
    // Root is always index 0.
    const nodes_data = tree.nodes.items(.data);
    return tree.extra_data[nodes_data[0].lhs..nodes_data[0].rhs];
}

pub fn renderError(tree: Ast, parse_error: Error, writer: std.io.AnyWriter) !void {
    const token_tags: []Token.Tag = tree.tokens.items(.tag);
    switch (parse_error.tag) {
        .expected_token => {
            const found_tag = token_tags[parse_error.token];
            const expected_symbol = parse_error.extra.expected_tag.symbol();
            switch (found_tag) {
                .invalid => return writer.print("expected '{s}', found invalid bytes", .{expected_symbol}),
                else => return writer.print("expected '{s}', found '{s}'", .{ expected_symbol, found_tag.symbol() }),
            }
        },

        .expected_semi_after_arg => {
            return writer.writeAll("TODO");
        },
        .expected_expr => {
            return writer.writeAll("TODO");
        },
        .expected_end_of_block => {
            return writer.writeAll("TODO");
        },
        .expected_prefix_expr => {
            return writer.writeAll("TODO");
        },
        .expected_infix_expr => {
            return writer.writeAll("TODO");
        },
        .expected_whitespace => {
            return writer.writeAll("TODO");
        },
        .not_yet_implemented => {
            return writer.writeAll("TODO");
        },
        .parse_error => {
            return writer.writeAll("TODO");
        },
        .expected_qsql_token => {
            return writer.writeAll("TODO");
        },
        .expected_from => {
            return writer.writeAll("TODO");
        },
        .expected_select_phrase => {
            return writer.writeAll("TODO");
        },
        .expected_by_phrase => {
            return writer.writeAll("TODO");
        },
        .os_expects_all_tokens_on_same_line => {
            const tag = token_tags[parse_error.token];
            return writer.print("{s} should not span multiple lines.", .{tag.symbol()});
        },
    }
}

pub const Error = struct {
    tag: Tag,
    token: TokenIndex,
    extra: union {
        none: void,
        expected_tag: Token.Tag,
        expected_string: []const u8,
    } = .{ .none = {} },

    pub const Tag = enum {
        /// `expected_tag` is populated.
        expected_token,

        expected_semi_after_arg,
        expected_expr,
        expected_end_of_block,
        expected_prefix_expr,
        expected_infix_expr,
        expected_whitespace,

        not_yet_implemented,
        parse_error,

        /// `expected_string` is populated.
        expected_qsql_token,

        expected_from,
        expected_select_phrase,
        expected_by_phrase,

        // os command errors
        os_expects_all_tokens_on_same_line,
    };
};

pub fn firstToken(tree: Ast, i: Node.Index) TokenIndex {
    const tags: []Node.Tag = tree.nodes.items(.tag);
    const main_tokens: []TokenIndex = tree.nodes.items(.main_token);
    const datas: []Node.Data = tree.nodes.items(.data);

    switch (tags[i]) {
        .root => unreachable,

        .grouped_expression,
        .empty_list,
        .list,
        .table_literal,
        .boolean_literal,
        .boolean_list_literal,
        .guid_literal,
        .guid_list_literal,
        .byte_literal,
        .byte_list_literal,
        .short_literal,
        .short_list_literal,
        .int_literal,
        .int_list_literal,
        .long_literal,
        .long_list_literal,
        .real_literal,
        .real_list_literal,
        .float_literal,
        .float_list_literal,
        .char_number_literal,
        .char_number_list_literal,
        .char_literal,
        .char_list_literal,
        .symbol_literal,
        .symbol_list_literal,
        .timestamp_literal,
        .timestamp_list_literal,
        .month_literal,
        .month_list_literal,
        .date_literal,
        .date_list_literal,
        .datetime_literal,
        .datetime_list_literal,
        .timespan_literal,
        .timespan_list_literal,
        .minute_literal,
        .minute_list_literal,
        .second_literal,
        .second_list_literal,
        .time_literal,
        .time_list_literal,
        .identifier,
        .os,
        .current_directory,
        .change_directory,
        .load_file_or_directory,
        => return main_tokens[i],

        .assign,
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

        .and_infix,
        .asof_infix,
        .bin_infix,
        .binr_infix,
        .cor_infix,
        .cov_infix,
        .cross_infix,
        .cut_infix,
        .div_infix,
        .dsave_infix,
        .each_infix,
        .ema_infix,
        .except_infix,
        .fby_infix,
        .ij_infix,
        .ijf_infix,
        .in_infix,
        .insert_infix,
        .inter_infix,
        .like_infix,
        .lj_infix,
        .ljf_infix,
        .lsq_infix,
        .mavg_infix,
        .mcount_infix,
        .mdev_infix,
        .mmax_infix,
        .mmin_infix,
        .mmu_infix,
        .mod_infix,
        .msum_infix,
        .or_infix,
        .over_infix,
        .peach_infix,
        .pj_infix,
        .prior_infix,
        .rotate_infix,
        .scan_infix,
        .scov_infix,
        .set_infix,
        .setenv_infix,
        .ss_infix,
        .sublist_infix,
        .sv_infix,
        .uj_infix,
        .ujf_infix,
        .upsert_infix,
        .vs_infix,
        .wavg_infix,
        .within_infix,
        .wsum_infix,
        .xasc_infix,
        .xbar_infix,
        .xcol_infix,
        .xcols_infix,
        .xdesc_infix,
        .xexp_infix,
        .xgroup_infix,
        .xkey_infix,
        .xlog_infix,
        .xprev_infix,
        .xrank_infix,
        => return tree.firstToken(datas[i].lhs),

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
        => return main_tokens[i],

        .apostrophe,
        .apostrophe_colon,
        .slash,
        .slash_colon,
        .backslash,
        .backslash_colon,
        => return tree.firstToken(datas[i].lhs),

        .apostrophe_infix,
        .apostrophe_colon_infix,
        .slash_infix,
        .slash_colon_infix,
        .backslash_infix,
        .backslash_colon_infix,
        => {
            const iterator = tree.extraData(datas[i].rhs, Node.Iterator);
            return tree.firstToken(iterator.lhs);
        },

        .implicit_apply => return tree.firstToken(datas[i].lhs),

        .lambda,
        .lambda_semicolon,
        .block,
        => return main_tokens[i],

        .call_one,
        .call,
        .do,
        .@"if",
        .@"while",
        .implicit_return,
        => return tree.firstToken(datas[i].lhs),

        .@"return" => unreachable,

        .abs,
        .acos,
        .aj,
        .aj0,
        .ajf,
        .ajf0,
        .all,
        .any,
        .asc,
        .asin,
        .atan,
        .attr,
        .avg,
        .avgs,
        .ceiling,
        .cols,
        .cos,
        .count,
        .csv,
        .deltas,
        .desc,
        .dev,
        .differ,
        .distinct,
        .ej,
        .enlist,
        .eval,
        .exit,
        .exp,
        .fills,
        .first,
        .fkeys,
        .flip,
        .floor,
        .get,
        .getenv,
        .group,
        .gtime,
        .hclose,
        .hcount,
        .hdel,
        .hopen,
        .hsym,
        .iasc,
        .idesc,
        .inv,
        .key,
        .keys,
        .last,
        .load,
        .log,
        .lower,
        .ltime,
        .ltrim,
        .max,
        .maxs,
        .md5,
        .med,
        .meta,
        .min,
        .mins,
        .neg,
        .next,
        .not,
        .null,
        .parse,
        .prd,
        .prds,
        .prev,
        .rand,
        .rank,
        .ratios,
        .raze,
        .read0,
        .read1,
        .reciprocal,
        .reval,
        .reverse,
        .rload,
        .rsave,
        .rtrim,
        .save,
        .sdev,
        .show,
        .signum,
        .sin,
        .sqrt,
        .ssr,
        .string,
        .sum,
        .sums,
        .svar,
        .system,
        .tables,
        .tan,
        .til,
        .trim,
        .type,
        .ungroup,
        .@"union",
        .upper,
        .value,
        .@"var",
        .view,
        .views,
        .where,
        .wj,
        .wj1,
        .ww,

        .@"and",
        .asof,
        .bin,
        .binr,
        .cor,
        .cov,
        .cross,
        .cut,
        .div,
        .dsave,
        .each,
        .ema,
        .except,
        .fby,
        .ij,
        .ijf,
        .in,
        .insert,
        .inter,
        .like,
        .lj,
        .ljf,
        .lsq,
        .mavg,
        .mcount,
        .mdev,
        .mmax,
        .mmin,
        .mmu,
        .mod,
        .msum,
        .@"or",
        .over,
        .peach,
        .pj,
        .prior,
        .rotate,
        .scan,
        .scov,
        .set,
        .setenv,
        .ss,
        .sublist,
        .sv,
        .uj,
        .ujf,
        .upsert,
        .vs,
        .wavg,
        .within,
        .wsum,
        .xasc,
        .xbar,
        .xcol,
        .xcols,
        .xdesc,
        .xexp,
        .xgroup,
        .xkey,
        .xlog,
        .xprev,
        .xrank,

        .do_one,
        .if_one,
        .while_one,

        .select,
        .exec,
        .update,
        .delete_rows,
        .delete_cols,
        => return main_tokens[i],
    }
}

pub fn lastToken(tree: Ast, i: Node.Index) TokenIndex {
    const tags: []Node.Tag = tree.nodes.items(.tag);
    const main_tokens: []TokenIndex = tree.nodes.items(.main_token);
    const datas: []Node.Data = tree.nodes.items(.data);
    const extra_datas = tree.extra_data;

    switch (tags[i]) {
        .root => unreachable,

        .grouped_expression,
        .empty_list,
        .list,
        .table_literal,
        => return datas[i].rhs,

        .boolean_literal,
        .boolean_list_literal,
        .guid_literal,
        .byte_literal,
        .byte_list_literal,
        .short_literal,
        .int_literal,
        .long_literal,
        .real_literal,
        .float_literal,
        .char_number_literal,
        .char_number_list_literal,
        .char_literal,
        .char_list_literal,
        .timestamp_literal,
        .month_literal,
        .date_literal,
        .datetime_literal,
        .timespan_literal,
        .minute_literal,
        .second_literal,
        .time_literal,
        => return main_tokens[i],
        .guid_list_literal,
        .short_list_literal,
        .int_list_literal,
        .long_list_literal,
        .real_list_literal,
        .float_list_literal,
        .timestamp_list_literal,
        .month_list_literal,
        .date_list_literal,
        .datetime_list_literal,
        .timespan_list_literal,
        .minute_list_literal,
        .second_list_literal,
        .time_list_literal,
        => return datas[i].rhs,
        .symbol_literal,
        .symbol_list_literal,
        .identifier,
        .os,
        .current_directory,
        => return main_tokens[i],

        .assign,
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

        .and_infix,
        .asof_infix,
        .bin_infix,
        .binr_infix,
        .cor_infix,
        .cov_infix,
        .cross_infix,
        .cut_infix,
        .div_infix,
        .dsave_infix,
        .each_infix,
        .ema_infix,
        .except_infix,
        .fby_infix,
        .ij_infix,
        .ijf_infix,
        .in_infix,
        .insert_infix,
        .inter_infix,
        .like_infix,
        .lj_infix,
        .ljf_infix,
        .lsq_infix,
        .mavg_infix,
        .mcount_infix,
        .mdev_infix,
        .mmax_infix,
        .mmin_infix,
        .mmu_infix,
        .mod_infix,
        .msum_infix,
        .or_infix,
        .over_infix,
        .peach_infix,
        .pj_infix,
        .prior_infix,
        .rotate_infix,
        .scan_infix,
        .scov_infix,
        .set_infix,
        .setenv_infix,
        .ss_infix,
        .sublist_infix,
        .sv_infix,
        .uj_infix,
        .ujf_infix,
        .upsert_infix,
        .vs_infix,
        .wavg_infix,
        .within_infix,
        .wsum_infix,
        .xasc_infix,
        .xbar_infix,
        .xcol_infix,
        .xcols_infix,
        .xdesc_infix,
        .xexp_infix,
        .xgroup_infix,
        .xkey_infix,
        .xlog_infix,
        .xprev_infix,
        .xrank_infix,
        => {
            const data = datas[i];
            if (data.rhs > 0) {
                return tree.lastToken(data.rhs);
            }
            return main_tokens[i];
        },

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
        .apostrophe,
        .apostrophe_colon,
        .slash,
        .slash_colon,
        .backslash,
        .backslash_colon,
        => return main_tokens[i],

        .apostrophe_infix,
        .apostrophe_colon_infix,
        .slash_infix,
        .slash_colon_infix,
        .backslash_infix,
        .backslash_colon_infix,
        => {
            const iterator = tree.extraData(datas[i].rhs, Node.Iterator);
            return tree.lastToken(iterator.rhs);
        },

        .implicit_apply => return tree.lastToken(datas[i].rhs),

        .lambda,
        .lambda_semicolon,
        .block,
        => return datas[i].rhs,

        .call_one => {
            const data = datas[i];
            var last_token = (if (data.rhs > 0) blk: {
                break :blk tree.lastToken(data.rhs);
            } else blk: {
                break :blk main_tokens[i];
            }) + 1;
            while (true) : (last_token += 1) {
                switch (tree.tokens.items(.tag)[last_token]) {
                    .comment => {},
                    else => break,
                }
            }
            return last_token;
        },

        .call,
        .do,
        .@"if",
        .@"while",
        => {
            const sub_range = tree.extraData(datas[i].rhs, Node.SubRange);
            var last_token = (if (sub_range.start == sub_range.end) blk: {
                break :blk main_tokens[i];
            } else blk: {
                var extra_data_i = sub_range.end - 1;
                var node_i = extra_datas[extra_data_i];
                while (node_i == 0 and extra_data_i > sub_range.start) {
                    extra_data_i -= 1;
                    node_i = extra_datas[extra_data_i];
                }
                break :blk if (node_i == 0) main_tokens[i] else tree.lastToken(node_i);
            }) + 1;
            while (true) : (last_token += 1) {
                switch (tree.tokens.items(.tag)[last_token]) {
                    .semicolon => {},
                    .comment => {},
                    else => break,
                }
            }
            return last_token;
        },

        .implicit_return => return tree.lastToken(datas[i].lhs),

        .@"return" => unreachable,

        .abs,
        .acos,
        .aj,
        .aj0,
        .ajf,
        .ajf0,
        .all,
        .any,
        .asc,
        .asin,
        .atan,
        .attr,
        .avg,
        .avgs,
        .ceiling,
        .cols,
        .cos,
        .count,
        .csv,
        .deltas,
        .desc,
        .dev,
        .differ,
        .distinct,
        .ej,
        .enlist,
        .eval,
        .exit,
        .exp,
        .fills,
        .first,
        .fkeys,
        .flip,
        .floor,
        .get,
        .getenv,
        .group,
        .gtime,
        .hclose,
        .hcount,
        .hdel,
        .hopen,
        .hsym,
        .iasc,
        .idesc,
        .inv,
        .key,
        .keys,
        .last,
        .load,
        .log,
        .lower,
        .ltime,
        .ltrim,
        .max,
        .maxs,
        .md5,
        .med,
        .meta,
        .min,
        .mins,
        .neg,
        .next,
        .not,
        .null,
        .parse,
        .prd,
        .prds,
        .prev,
        .rand,
        .rank,
        .ratios,
        .raze,
        .read0,
        .read1,
        .reciprocal,
        .reval,
        .reverse,
        .rload,
        .rsave,
        .rtrim,
        .save,
        .sdev,
        .show,
        .signum,
        .sin,
        .sqrt,
        .ssr,
        .string,
        .sum,
        .sums,
        .svar,
        .system,
        .tables,
        .tan,
        .til,
        .trim,
        .type,
        .ungroup,
        .@"union",
        .upper,
        .value,
        .@"var",
        .view,
        .views,
        .where,
        .wj,
        .wj1,
        .ww,

        .@"and",
        .asof,
        .bin,
        .binr,
        .cor,
        .cov,
        .cross,
        .cut,
        .div,
        .dsave,
        .each,
        .ema,
        .except,
        .fby,
        .ij,
        .ijf,
        .in,
        .insert,
        .inter,
        .like,
        .lj,
        .ljf,
        .lsq,
        .mavg,
        .mcount,
        .mdev,
        .mmax,
        .mmin,
        .mmu,
        .mod,
        .msum,
        .@"or",
        .over,
        .peach,
        .pj,
        .prior,
        .rotate,
        .scan,
        .scov,
        .set,
        .setenv,
        .ss,
        .sublist,
        .sv,
        .uj,
        .ujf,
        .upsert,
        .vs,
        .wavg,
        .within,
        .wsum,
        .xasc,
        .xbar,
        .xcol,
        .xcols,
        .xdesc,
        .xexp,
        .xgroup,
        .xkey,
        .xlog,
        .xprev,
        .xrank,
        => return main_tokens[i],

        .do_one,
        .if_one,
        .while_one,
        => {
            const data = datas[i];
            var last_token = (if (data.rhs > 0) blk: {
                break :blk tree.lastToken(data.rhs);
            } else blk: {
                break :blk tree.lastToken(data.lhs);
            }) + 1;
            while (true) : (last_token += 1) {
                switch (tree.tokens.items(.tag)[last_token]) {
                    .comment => {},
                    .semicolon => {},
                    else => break,
                }
            }
            return last_token;
        },

        .select => {
            const select_node = tree.extraData(datas[i].lhs, Node.Select);

            if (select_node.by > select_node.where) {
                const node = extra_datas[select_node.by - 1];
                return tree.lastToken(node);
            }

            return tree.lastToken(select_node.from);
        },

        .exec => {
            const exec_node = tree.extraData(datas[i].lhs, Node.Exec);

            if (exec_node.by > exec_node.where) {
                const node = extra_datas[exec_node.by - 1];
                return tree.lastToken(node);
            }

            return tree.lastToken(exec_node.from);
        },

        .update => {
            const update_node = tree.extraData(datas[i].lhs, Node.Update);

            if (update_node.by > update_node.where) {
                const node = extra_datas[update_node.by - 1];
                return tree.lastToken(node);
            }

            return tree.lastToken(update_node.from);
        },

        .delete_rows => {
            const delete_node = tree.extraData(datas[i].lhs, Node.DeleteRows);

            if (delete_node.where_end > delete_node.where) {
                const node = extra_datas[delete_node.where_end - 1];
                return tree.lastToken(node);
            }

            return tree.lastToken(delete_node.from);
        },

        .delete_cols => {
            const delete_node = tree.extraData(datas[i].lhs, Node.DeleteColumns);
            return tree.lastToken(delete_node.from);
        },

        .change_directory => return datas[i].lhs,
        .load_file_or_directory => {
            const data = datas[i];
            return if (data.rhs > 0) data.rhs else data.lhs;
        },
    }
}

pub fn print(tree: Ast, i: Node.Index, stream: anytype, gpa: Allocator) Allocator.Error!void {
    const token_tags: []Token.Tag = tree.tokens.items(.tag);
    const tags: []Node.Tag = tree.nodes.items(.tag);
    const main_tokens: []TokenIndex = tree.nodes.items(.main_token);
    const datas: []Node.Data = tree.nodes.items(.data);
    const extra_datas = tree.extra_data;
    const values = tree.values;

    switch (tags[i]) {
        .root => unreachable,
        .grouped_expression,
        .implicit_return,
        => try tree.print(datas[i].lhs, stream, gpa),
        .empty_list => try stream.writeAll("()"),
        .list => {
            try stream.writeAll("(enlist;");
            const sub_range = tree.extraData(datas[i].lhs, Node.SubRange);
            for (sub_range.start..sub_range.end) |extra_data_i| {
                const node_i = extra_datas[extra_data_i];
                if (node_i == 0) {
                    try stream.writeAll("::");
                } else {
                    try tree.print(node_i, stream, gpa);
                }
                if (extra_data_i < sub_range.end - 1) {
                    try stream.writeAll(";");
                }
            }
            try stream.writeAll(")");
        },
        .table_literal => {
            const table = tree.extraData(datas[i].lhs, Node.Table);

            const columns = tree.table_columns[table.column_start + table.key_len .. table.column_start + table.key_len + table.len];
            const exprs = tree.extra_data[table.expr_start + table.key_len .. table.expr_start + table.key_len + table.len];

            if (table.key_len > 0) {
                var flip = std.ArrayList(u8).init(gpa);
                defer flip.deinit();

                try printTable(tree, columns, exprs, flip.writer(), gpa);

                var key_flip = std.ArrayList(u8).init(gpa);
                defer key_flip.deinit();

                const key_columns = tree.table_columns[table.column_start .. table.column_start + table.key_len];
                const key_exprs = tree.extra_data[table.expr_start .. table.expr_start + table.key_len];
                try printTable(tree, key_columns, key_exprs, key_flip.writer(), gpa);

                try stream.print("(!;{s};{s})", .{ key_flip.items, flip.items });
            } else {
                try printTable(tree, columns, exprs, stream, gpa);
            }
        },
        .boolean_literal,
        .boolean_list_literal,
        .guid_literal,
        .guid_list_literal,
        .byte_literal,
        .byte_list_literal,
        .short_literal,
        .short_list_literal,
        .int_literal,
        .int_list_literal,
        .long_literal,
        .long_list_literal,
        .real_literal,
        .real_list_literal,
        .float_literal,
        .float_list_literal,
        .char_number_literal,
        .char_number_list_literal,
        .timestamp_literal,
        .timestamp_list_literal,
        .month_literal,
        .month_list_literal,
        .date_literal,
        .date_list_literal,
        .datetime_literal,
        .datetime_list_literal,
        .timespan_literal,
        .timespan_list_literal,
        .minute_literal,
        .minute_list_literal,
        .second_literal,
        .second_list_literal,
        .time_literal,
        .time_list_literal,
        => {
            const data = datas[i];
            const value = values[data.lhs];
            try stream.print("{}", .{value});
        },
        .char_literal,
        .char_list_literal,
        => try stream.writeAll(tree.tokenSlice(main_tokens[i])),
        .symbol_literal,
        .symbol_list_literal,
        => try stream.print(",{s}", .{tree.tokenSlice(main_tokens[i])}),
        .identifier => try stream.print("`{s}", .{tree.tokenSlice(main_tokens[i])}),
        .assign,
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

        .and_infix,
        .asof_infix,
        .bin_infix,
        .binr_infix,
        .cor_infix,
        .cov_infix,
        .cross_infix,
        .cut_infix,
        .div_infix,
        .dsave_infix,
        .each_infix,
        .ema_infix,
        .except_infix,
        .fby_infix,
        .ij_infix,
        .ijf_infix,
        .in_infix,
        .insert_infix,
        .inter_infix,
        .like_infix,
        .lj_infix,
        .ljf_infix,
        .lsq_infix,
        .mavg_infix,
        .mcount_infix,
        .mdev_infix,
        .mmax_infix,
        .mmin_infix,
        .mmu_infix,
        .mod_infix,
        .msum_infix,
        .or_infix,
        .over_infix,
        .peach_infix,
        .pj_infix,
        .prior_infix,
        .rotate_infix,
        .scan_infix,
        .scov_infix,
        .set_infix,
        .setenv_infix,
        .ss_infix,
        .sublist_infix,
        .sv_infix,
        .uj_infix,
        .ujf_infix,
        .upsert_infix,
        .vs_infix,
        .wavg_infix,
        .within_infix,
        .wsum_infix,
        .xasc_infix,
        .xbar_infix,
        .xcol_infix,
        .xcols_infix,
        .xdesc_infix,
        .xexp_infix,
        .xgroup_infix,
        .xkey_infix,
        .xlog_infix,
        .xprev_infix,
        .xrank_infix,
        => {
            const data = datas[i];
            const symbol = token_tags[main_tokens[i]].symbol();
            if (data.rhs > 0) {
                var rhs = std.ArrayList(u8).init(gpa);
                defer rhs.deinit();
                try tree.print(data.rhs, rhs.writer(), gpa);

                var lhs = std.ArrayList(u8).init(gpa);
                defer lhs.deinit();
                try tree.print(data.lhs, lhs.writer(), gpa);

                try stream.print("({s};{s};{s})", .{ symbol, lhs.items, rhs.items });
            } else if (data.lhs > 0) {
                var lhs = std.ArrayList(u8).init(gpa);
                defer lhs.deinit();
                try tree.print(data.lhs, lhs.writer(), gpa);

                try stream.print("({s};{s})", .{ symbol, lhs.items });
            } else {
                try stream.writeAll(symbol);
            }
        },

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
        => try stream.writeAll(token_tags[main_tokens[i]].symbol()),
        .implicit_apply => {
            const data = datas[i];

            var rhs = std.ArrayList(u8).init(gpa);
            defer rhs.deinit();
            try tree.print(data.rhs, rhs.writer(), gpa);

            var lhs = std.ArrayList(u8).init(gpa);
            defer lhs.deinit();
            try tree.print(data.lhs, lhs.writer(), gpa);

            try stream.print("({s};{s})", .{ lhs.items, rhs.items });
        },

        .apostrophe,
        .apostrophe_colon,
        .slash,
        .slash_colon,
        .backslash,
        .backslash_colon,
        => {
            const data = datas[i];

            if (data.lhs > 0) {
                var lhs = std.ArrayList(u8).init(gpa);
                defer lhs.deinit();
                try tree.print(data.lhs, lhs.writer(), gpa);

                try stream.print("({s};{s})", .{ tree.tokenSlice(main_tokens[i]), lhs.items });
            } else {
                try stream.writeAll(tree.tokenSlice(main_tokens[i]));
            }
        },

        .apostrophe_infix,
        .apostrophe_colon_infix,
        .slash_infix,
        .slash_colon_infix,
        .backslash_infix,
        .backslash_colon_infix,
        => {
            const data = datas[i];
            const iterator = tree.extraData(data.rhs, Node.Iterator);

            var lhs = std.ArrayList(u8).init(gpa);
            defer lhs.deinit();
            try tree.print(data.lhs, lhs.writer(), gpa);

            var x = std.ArrayList(u8).init(gpa);
            defer x.deinit();
            try tree.print(iterator.lhs, x.writer(), gpa);

            var y = std.ArrayList(u8).init(gpa);
            defer y.deinit();
            try tree.print(iterator.rhs, y.writer(), gpa);

            try stream.print("(({s};{s});{s};{s})", .{ tree.tokenSlice(main_tokens[i]), lhs.items, x.items, y.items });
        },

        .lambda,
        .lambda_semicolon,
        => {
            const start_token = main_tokens[i];
            const start = tree.tokens.items(.loc)[start_token].start;

            const last_token = tree.lastToken(i);
            const end = tree.tokens.items(.loc)[last_token].end;

            const source = tree.source[start..end];
            try stream.print("{s}", .{source});
        },

        .block => {
            const sub_range = tree.extraData(datas[i].lhs, Node.SubRange);
            const exprs = tree.extra_data[sub_range.start..sub_range.end];

            if (exprs.len > 0) {
                var block = std.ArrayList(u8).init(gpa);
                defer block.deinit();
                if (exprs[0] == 0) {
                    try block.appendSlice("::");
                } else {
                    try tree.print(exprs[0], block.writer(), gpa);
                }
                switch (exprs.len) {
                    1 => try stream.writeAll(block.items),
                    else => {
                        for (exprs[1..]) |expr| {
                            try block.append(';');
                            if (expr == 0) {
                                try block.appendSlice("::");
                            } else {
                                try tree.print(expr, block.writer(), gpa);
                            }
                        }

                        try stream.print("(\";\";{s})", .{block.items});
                    },
                }
            } else {
                try stream.writeAll("::");
            }
        },
        .call_one => {
            const data = datas[i];

            var rhs = std.ArrayList(u8).init(gpa);
            defer rhs.deinit();
            if (data.rhs == 0) {
                try rhs.appendSlice("::");
            } else {
                try tree.print(data.rhs, rhs.writer(), gpa);
            }

            var lhs = std.ArrayList(u8).init(gpa);
            defer lhs.deinit();
            try tree.print(data.lhs, lhs.writer(), gpa);

            try stream.print("({s};{s})", .{ lhs.items, rhs.items });
        },
        .call => {
            const data = datas[i];

            var rhs = std.ArrayList(u8).init(gpa);
            defer rhs.deinit();
            const sub_range = tree.extraData(data.rhs, Node.SubRange);
            for (sub_range.start..sub_range.end) |extra_data_i| {
                const node_i = extra_datas[extra_data_i];
                if (node_i == 0) {
                    try rhs.appendSlice("::");
                } else {
                    try tree.print(node_i, rhs.writer(), gpa);
                }
                if (extra_data_i < sub_range.end - 1) {
                    try rhs.append(';');
                }
            }

            var lhs = std.ArrayList(u8).init(gpa);
            defer lhs.deinit();
            try tree.print(data.lhs, lhs.writer(), gpa);

            try stream.print("({s};{s})", .{ lhs.items, rhs.items });
        },
        .@"return" => {
            var lhs = std.ArrayList(u8).init(gpa);
            defer lhs.deinit();
            try tree.print(datas[i].lhs, lhs.writer(), gpa);

            try stream.print("(\":\";{s})", .{lhs.items});
        },

        .abs,
        .acos,
        .aj,
        .aj0,
        .ajf,
        .ajf0,
        .all,
        .any,
        .asc,
        .asin,
        .atan,
        .attr,
        .avg,
        .avgs,
        .ceiling,
        .cols,
        .cos,
        .count,
        .csv,
        .deltas,
        .desc,
        .dev,
        .differ,
        .distinct,
        .ej,
        .enlist,
        .eval,
        .exit,
        .exp,
        .fills,
        .first,
        .fkeys,
        .flip,
        .floor,
        .get,
        .getenv,
        .group,
        .gtime,
        .hclose,
        .hcount,
        .hdel,
        .hopen,
        .hsym,
        .iasc,
        .idesc,
        .inv,
        .key,
        .keys,
        .last,
        .load,
        .log,
        .lower,
        .ltime,
        .ltrim,
        .max,
        .maxs,
        .md5,
        .med,
        .meta,
        .min,
        .mins,
        .neg,
        .next,
        .not,
        .null,
        .parse,
        .prd,
        .prds,
        .prev,
        .rand,
        .rank,
        .ratios,
        .raze,
        .read0,
        .read1,
        .reciprocal,
        .reval,
        .reverse,
        .rload,
        .rsave,
        .rtrim,
        .save,
        .sdev,
        .show,
        .signum,
        .sin,
        .sqrt,
        .ssr,
        .string,
        .sum,
        .sums,
        .svar,
        .system,
        .tables,
        .tan,
        .til,
        .trim,
        .type,
        .ungroup,
        .@"union",
        .upper,
        .value,
        .@"var",
        .view,
        .views,
        .where,
        .wj,
        .wj1,
        .ww,

        .@"and",
        .asof,
        .bin,
        .binr,
        .cor,
        .cov,
        .cross,
        .cut,
        .div,
        .dsave,
        .each,
        .ema,
        .except,
        .fby,
        .ij,
        .ijf,
        .in,
        .insert,
        .inter,
        .like,
        .lj,
        .ljf,
        .lsq,
        .mavg,
        .mcount,
        .mdev,
        .mmax,
        .mmin,
        .mmu,
        .mod,
        .msum,
        .@"or",
        .over,
        .peach,
        .pj,
        .prior,
        .rotate,
        .scan,
        .scov,
        .set,
        .setenv,
        .ss,
        .sublist,
        .sv,
        .uj,
        .ujf,
        .upsert,
        .vs,
        .wavg,
        .within,
        .wsum,
        .xasc,
        .xbar,
        .xcol,
        .xcols,
        .xdesc,
        .xexp,
        .xgroup,
        .xkey,
        .xlog,
        .xprev,
        .xrank,
        => try stream.writeAll(tree.tokenSlice(main_tokens[i])),

        .do_one,
        .if_one,
        .while_one,
        => {
            const data = datas[i];

            var lhs = std.ArrayList(u8).init(gpa);
            defer lhs.deinit();
            try tree.print(data.lhs, lhs.writer(), gpa);

            var rhs = std.ArrayList(u8).init(gpa);
            defer rhs.deinit();
            if (data.rhs == 0) {
                try rhs.appendSlice("::");
            } else {
                try tree.print(data.rhs, rhs.writer(), gpa);
            }

            try stream.print("(`{s};{s};{s})", .{ tree.tokenSlice(main_tokens[i]), lhs.items, rhs.items });
        },
        .do,
        .@"if",
        .@"while",
        => {
            const data = datas[i];

            var lhs = std.ArrayList(u8).init(gpa);
            defer lhs.deinit();
            try tree.print(data.lhs, lhs.writer(), gpa);

            var rhs = std.ArrayList(u8).init(gpa);
            defer rhs.deinit();
            const sub_range = tree.extraData(data.rhs, Node.SubRange);
            for (sub_range.start..sub_range.end) |extra_data_i| {
                const node_i = extra_datas[extra_data_i];
                if (node_i == 0) {
                    try rhs.appendSlice("::");
                } else {
                    try tree.print(node_i, rhs.writer(), gpa);
                }
                if (extra_data_i < sub_range.end - 1) {
                    try rhs.append(';');
                }
            }

            try stream.print("(`{s};{s};{s})", .{ tree.tokenSlice(main_tokens[i]), lhs.items, rhs.items });
        },

        .select => {
            const select_node = tree.extraData(datas[i].lhs, Node.Select);

            var from = std.ArrayList(u8).init(gpa);
            defer from.deinit();
            try tree.print(select_node.from, from.writer(), gpa);

            var where = std.ArrayList(u8).init(gpa);
            defer where.deinit();
            const where_slice = tree.extra_data[select_node.where..select_node.by];
            if (where_slice.len == 0) {
                try where.appendSlice("()");
            } else {
                var temp = std.ArrayList(u8).init(gpa);
                defer temp.deinit();
                try where.append(',');
                if (where_slice.len > 1) try temp.append('(');
                for (where_slice, 0..) |where_i, temp_i| {
                    try tree.print(where_i, temp.writer(), gpa);
                    if (temp_i < where_slice.len - 1) try temp.append(';');
                }
                if (where_slice.len > 1) try temp.append(')');
                if (temp.items[0] != '(' or temp.items[1] == ')') try where.append(',');
                try where.appendSlice(temp.items);
            }

            var by = std.ArrayList(u8).init(gpa);
            defer by.deinit();
            if (select_node.data.has_by) {
                const by_slice = tree.extra_data[select_node.by..select_node.select];
                if (by_slice.len == 0) {
                    try by.appendSlice("()!()");
                } else {
                    try tree.printColumns(select_node.by_columns, by_slice.len, by.writer());
                    try by.append('!');
                    try tree.printList(by_slice, by.writer(), gpa);
                }
            } else if (select_node.data.distinct) {
                try by.appendSlice("1b");
            } else {
                try by.appendSlice("0b");
            }

            var select = std.ArrayList(u8).init(gpa);
            defer select.deinit();
            const select_slice = tree.extra_data[select_node.select..select_node.select_end];
            if (select_slice.len == 0) {
                try select.appendSlice("()");
            } else {
                try tree.printColumns(select_node.select_columns, select_slice.len, select.writer());
                try select.append('!');
                try tree.printList(select_slice, select.writer(), gpa);
            }

            if (select_node.order > 0) {
                var limit = std.ArrayList(u8).init(gpa);
                defer limit.deinit();
                if (select_node.limit > 0) {
                    try tree.print(select_node.limit, limit.writer(), gpa);
                } else {
                    try limit.appendSlice("0W");
                }

                var order = std.ArrayList(u8).init(gpa);
                defer order.deinit();
                const c: u8 = if (select_node.data.ascending) '<' else '>';
                try order.writer().print(",({c}:;`{s})", .{ c, tree.tokenSlice(select_node.order) });

                try stream.print("(?;{s};{s};{s};{s};{s};{s})", .{ from.items, where.items, by.items, select.items, limit.items, order.items });
            } else if (select_node.limit > 0) {
                var limit = std.ArrayList(u8).init(gpa);
                defer limit.deinit();
                try tree.print(select_node.limit, limit.writer(), gpa);

                try stream.print("(?;{s};{s};{s};{s};{s})", .{ from.items, where.items, by.items, select.items, limit.items });
            } else {
                try stream.print("(?;{s};{s};{s};{s})", .{ from.items, where.items, by.items, select.items });
            }
        },
        .exec => {
            const exec_node = tree.extraData(datas[i].lhs, Node.Exec);

            var from = std.ArrayList(u8).init(gpa);
            defer from.deinit();
            try tree.print(exec_node.from, from.writer(), gpa);

            var where = std.ArrayList(u8).init(gpa);
            defer where.deinit();
            const where_slice = tree.extra_data[exec_node.where..exec_node.by];
            if (where_slice.len == 0) {
                try where.appendSlice("()");
            } else {
                var temp = std.ArrayList(u8).init(gpa);
                defer temp.deinit();
                try where.append(',');
                if (where_slice.len > 1) try temp.append('(');
                for (where_slice, 0..) |where_i, temp_i| {
                    try tree.print(where_i, temp.writer(), gpa);
                    if (temp_i < where_slice.len - 1) try temp.append(';');
                }
                if (where_slice.len > 1) try temp.append(')');
                if (temp.items[0] != '(' or temp.items[1] == ')') try where.append(',');
                try where.appendSlice(temp.items);
            }

            var by = std.ArrayList(u8).init(gpa);
            defer by.deinit();
            const by_slice = tree.extra_data[exec_node.by..exec_node.select];
            if (by_slice.len == 0) {
                try by.appendSlice("()");
            } else if (exec_node.data.has_by_columns) {
                try tree.printColumns(exec_node.by_columns, by_slice.len, by.writer());
                try by.append('!');
                try tree.printList(by_slice, by.writer(), gpa);
            } else {
                try tree.printList(by_slice, by.writer(), gpa);
            }

            var select = std.ArrayList(u8).init(gpa);
            defer select.deinit();
            const select_slice = tree.extra_data[exec_node.select..exec_node.select_end];
            if (select_slice.len == 0) {
                try select.appendSlice("()");
            } else if (exec_node.data.has_select_columns) {
                try tree.printColumns(exec_node.select_columns, select_slice.len, select.writer());
                try select.append('!');
                try tree.printList(select_slice, select.writer(), gpa);
            } else {
                try tree.printList(select_slice, select.writer(), gpa);
            }

            try stream.print("(?;{s};{s};{s};{s})", .{ from.items, where.items, by.items, select.items });
        },
        .update => {
            const update_node = tree.extraData(datas[i].lhs, Node.Update);

            var from = std.ArrayList(u8).init(gpa);
            defer from.deinit();
            try tree.print(update_node.from, from.writer(), gpa);

            var where = std.ArrayList(u8).init(gpa);
            defer where.deinit();
            const where_slice = tree.extra_data[update_node.where..update_node.by];
            if (where_slice.len == 0) {
                try where.appendSlice("()");
            } else {
                var temp = std.ArrayList(u8).init(gpa);
                defer temp.deinit();
                try where.append(',');
                if (where_slice.len > 1) try temp.append('(');
                for (where_slice, 0..) |where_i, temp_i| {
                    try tree.print(where_i, temp.writer(), gpa);
                    if (temp_i < where_slice.len - 1) try temp.append(';');
                }
                if (where_slice.len > 1) try temp.append(')');
                if (temp.items[0] != '(' or temp.items[1] == ')') try where.append(',');
                try where.appendSlice(temp.items);
            }

            var by = std.ArrayList(u8).init(gpa);
            defer by.deinit();
            const by_slice = tree.extra_data[update_node.by..update_node.select];
            if (by_slice.len == 0) {
                try by.appendSlice("0b");
            } else {
                try tree.printColumns(update_node.by_columns, by_slice.len, by.writer());
                try by.append('!');
                try tree.printList(by_slice, by.writer(), gpa);
            }

            var select = std.ArrayList(u8).init(gpa);
            defer select.deinit();
            const select_slice = tree.extra_data[update_node.select..update_node.select_end];
            if (select_slice.len == 0) {
                try select.appendSlice("()");
            } else {
                try tree.printColumns(update_node.select_columns, select_slice.len, select.writer());
                try select.append('!');
                try tree.printList(select_slice, select.writer(), gpa);
            }

            try stream.print("(!;{s};{s};{s};{s})", .{ from.items, where.items, by.items, select.items });
        },
        .delete_rows => {
            const delete_node = tree.extraData(datas[i].lhs, Node.DeleteRows);

            var from = std.ArrayList(u8).init(gpa);
            defer from.deinit();
            try tree.print(delete_node.from, from.writer(), gpa);

            var where = std.ArrayList(u8).init(gpa);
            defer where.deinit();
            const where_slice = tree.extra_data[delete_node.where..delete_node.where_end];
            if (where_slice.len == 0) {
                try where.appendSlice("()");
            } else {
                var temp = std.ArrayList(u8).init(gpa);
                defer temp.deinit();
                try where.append(',');
                if (where_slice.len > 1) try temp.append('(');
                for (where_slice, 0..) |where_i, temp_i| {
                    try tree.print(where_i, temp.writer(), gpa);
                    if (temp_i < where_slice.len - 1) try temp.append(';');
                }
                if (where_slice.len > 1) try temp.append(')');
                if (temp.items[0] != '(' or temp.items[1] == ')') try where.append(',');
                try where.appendSlice(temp.items);
            }

            try stream.print("(!;{s};{s};0b;`symbol$())", .{ from.items, where.items });
        },
        .delete_cols => {
            const delete_node = tree.extraData(datas[i].lhs, Node.DeleteColumns);

            var from = std.ArrayList(u8).init(gpa);
            defer from.deinit();
            try tree.print(delete_node.from, from.writer(), gpa);

            var select = std.ArrayList(u8).init(gpa);
            defer select.deinit();
            const select_slice = tree.table_columns[delete_node.select_columns..delete_node.select_columns_end];
            if (select_slice.len == 0) {
                try select.appendSlice("()");
            } else {
                if (select_slice.len == 1) {
                    try select.append(',');
                }
                try select.append(',');
                for (select_slice) |column| {
                    try select.writer().print("`{s}", .{column});
                }
            }

            try stream.print("(!;{s};();0b;{s})", .{ from.items, select.items });
        },

        .os,
        .current_directory,
        .change_directory,
        .load_file_or_directory,
        => {
            const data = datas[i];
            const main_token_source = tree.tokenSlice(main_tokens[i]);

            if (data.lhs > 0) {
                const sub_range = tree.extraData(datas[i].lhs, Node.SubRange);
                const params = tree.extra_data[sub_range.start..sub_range.end];

                var params_list = std.ArrayList(u8).init(gpa);
                defer params_list.deinit();
                for (params, 0..) |param_i, temp_i| {
                    try params_list.appendSlice(tree.tokenSlice(param_i));
                    if (temp_i < params.len - 1) {
                        try params_list.append(' ');
                    }
                }
                try stream.print("(.,[\"\\\\\"];\"{s} {s}\")", .{ main_token_source[1..], params_list.items });
            } else {
                try stream.print("(.,[\"\\\\\"];\"{s}\")", .{main_token_source[1..]});
            }
        },
    }
}

fn printTable(tree: Ast, columns: [][]const u8, exprs: []Node.Index, stream: anytype, gpa: Allocator) Allocator.Error!void {
    var keys = std.ArrayList(u8).init(gpa);
    defer keys.deinit();
    try keys.append(',');
    if (columns.len == 1) try keys.append(',');
    for (columns) |column| {
        try keys.writer().print("`{s}", .{column});
    }

    var values = std.ArrayList(u8).init(gpa);
    defer values.deinit();
    try tree.print(exprs[0], values.writer(), gpa);
    for (exprs[1..]) |expr| {
        try values.append(';');
        try tree.print(expr, values.writer(), gpa);
    }

    try stream.print("(+:;(!;{s};(enlist;{s})))", .{ keys.items, values.items });
}

fn printColumns(tree: Ast, start: Node.Index, len: usize, stream: anytype) Allocator.Error!void {
    const columns = tree.table_columns[start .. start + len];
    assert(columns.len > 0);
    if (columns.len == 1) {
        try stream.print("(,`{s})", .{columns[0]});
    } else {
        for (columns) |column| {
            try stream.print("`{s}", .{column});
        }
    }
}

fn printList(tree: Ast, list: []Node.Index, stream: anytype, gpa: Allocator) Allocator.Error!void {
    const tags: []Node.Tag = tree.nodes.items(.tag);

    switch (list.len) {
        0 => {
            try stream.writeAll("()");
        },
        1 => {
            try stream.writeAll(",");
            for (list) |i| {
                try tree.print(i, stream, gpa);
            }
        },
        else => {
            var hash_set = std.AutoHashMap(Node.Tag, void).init(gpa);
            defer hash_set.deinit();

            try hash_set.put(tags[list[0]], {});
            for (list[1..]) |i| {
                const result = try hash_set.getOrPut(tags[i]);
                if (!result.found_existing) break;
            } else {
                var iter = hash_set.keyIterator();
                const tag = iter.next().?.*;
                switch (tag) {
                    .identifier => {
                        for (list) |i| {
                            try tree.print(i, stream, gpa);
                        }
                        return;
                    },
                    else => {},
                }
            }

            try stream.writeAll("(");
            for (list, 0..) |i, temp_i| {
                try tree.print(i, stream, gpa);
                if (temp_i < list.len - 1) {
                    try stream.writeAll(";");
                }
            }
            try stream.writeAll(")");
        },
    }
}

pub fn debug(tree: Ast, gpa: Allocator) !void {
    const data = tree.nodes.items(.data)[0];
    for (data.lhs..data.rhs) |extra_data_i| {
        var list = std.ArrayList(u8).init(gpa);
        defer list.deinit();
        try tree.print(tree.extra_data[extra_data_i], list.writer(), gpa);
        log.debug("{s}", .{list.items});
    }
}

pub const Node = @import("ast/Node.zig");

const std = @import("std");
const kdb = @import("../kdb.zig");
const Token = kdb.Token;
const Tokenizer = kdb.Tokenizer;
const Ast = @This();
const Allocator = std.mem.Allocator;
const Parse = @import("Parse.zig");
const panic = std.debug.panic;
const assert = std.debug.assert;
const diagnostics = @import("../features/diagnostics.zig");
const private_render = @import("./render.zig");
const RenderSettings = private_render.RenderSettings;
const Value = @import("parser/number_parser.zig").Value;

const log = std.log.scoped(.kdblint_ast);

fn testLastToken(source: [:0]const u8) !void {
    inline for (&.{.@"4.0"}) |version| {
        inline for (&.{ .k, .q }) |language| {
            try testLastTokenSettings(.{
                .version = version,
                .language = language,
            }, source);
        }
    }
}

fn testLastTokenVersion(version: Ast.Version, source: [:0]const u8) !void {
    inline for (&.{ .k, .q }) |language| {
        try testLastTokenSettings(.{
            .version = version,
            .language = language,
        }, source);
    }
}

fn testLastTokenLanguage(language: Ast.Language, source: [:0]const u8) !void {
    inline for (&.{.@"4.0"}) |version| {
        try testLastTokenSettings(.{
            .version = version,
            .language = language,
        }, source);
    }
}

fn testLastTokenSettings(settings: Ast.ParseSettings, source: [:0]const u8) !void {
    var tree = try parse(std.testing.allocator, source, settings);
    defer tree.deinit(std.testing.allocator);

    const data = tree.nodes.items(.data)[0];
    const i = tree.extra_data[data.lhs];

    var actual = std.ArrayList(u8).init(std.testing.allocator);
    defer actual.deinit();
    try tree.print(i, actual.writer(), std.testing.allocator);
    try std.testing.expectEqualSlices(u8, source[0..source.len], actual.items);
}

test "getLastToken" {
    try testLastToken("{f[]}");
    try testLastToken("{f[];}");
    try testLastToken("{f[1]}");
    try testLastToken("{f[1];}");
    try testLastToken("{f[;]}");
    try testLastToken("{f[;];}");
    try testLastToken("{f[1;]}");
    try testLastToken("{f[1;];}");
    try testLastToken("{f[;2]}");
    try testLastToken("{f[;2];}");
    try testLastToken("{f[1;2]}");
    try testLastToken("{f[1;2];}");

    try testLastToken("{do[0;]}");
    try testLastToken("{do[0;];}");
    try testLastToken("{do[0;1]}");
    try testLastToken("{do[0;1];}");
    try testLastToken("{do[0;1;]}");
    try testLastToken("{do[0;1;];}");
    try testLastToken("{do[0;1;2]}");
    try testLastToken("{do[0;1;2];}");

    try testLastToken("{if[0;]}");
    try testLastToken("{if[0;];}");
    try testLastToken("{if[0;1]}");
    try testLastToken("{if[0;1];}");
    try testLastToken("{if[0;1;]}");
    try testLastToken("{if[0;1;];}");
    try testLastToken("{if[0;1;2]}");
    try testLastToken("{if[0;1;2];}");

    try testLastToken("{while[0;]}");
    try testLastToken("{while[0;];}");
    try testLastToken("{while[0;1]}");
    try testLastToken("{while[0;1];}");
    try testLastToken("{while[0;1;]}");
    try testLastToken("{while[0;1;];}");
    try testLastToken("{while[0;1;2]}");
    try testLastToken("{while[0;1;2];}");

    try testLastToken("{[]}");
    try testLastToken("{[x]}");
    try testLastToken("{[x;y]}");
    try testLastToken("{[x;y;z]}");

    try testLastToken("{[][]}");
    try testLastToken("{[][0]}");
    try testLastToken("{[][0;]}");
    try testLastToken("{[][;1]}");
    try testLastToken("{[][0;1]}");
    try testLastToken("{[][0;1;]}");
    try testLastToken("{[][0;1;2]}");

    try testLastToken("{[]()}");
    try testLastToken("{[](0)}");
    try testLastToken("{[](0;)}");
    try testLastToken("{[](;1)}");
    try testLastToken("{[](0;1)}");
    try testLastToken("{[](0;1;)}");
    try testLastToken("{[](0;1;2)}");
}

test {
    @import("std").testing.refAllDecls(@This());
}
