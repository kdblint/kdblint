//! Represents in-progress parsing, will be converted to an Ast after completion.

pub const Error = error{ParseError} || Allocator.Error;

gpa: Allocator,
source: []const u8,
token_tags: []const Token.Tag,
token_locs: []const Token.Loc,
token_eobs: []const bool,
version: Ast.Version,
language: Ast.Language,
tok_i: TokenIndex = 0,
eob: bool = false,
ends_expr_tags: std.ArrayListUnmanaged(Token.Tag) = .{},
ends_sql_expr_identifiers: std.ArrayListUnmanaged(?SqlIdentifier) = .{},
errors: std.ArrayListUnmanaged(AstError) = .{},
nodes: Ast.NodeList = .{},
extra_data: std.ArrayListUnmanaged(Node.Index) = .{},
table_columns: std.ArrayListUnmanaged([]const u8) = .{},
values: std.ArrayListUnmanaged(Value) = .{},
scratch: std.ArrayListUnmanaged(Node.Index) = .{},
table_scratch: std.ArrayListUnmanaged([]const u8) = .{},

pub fn deinit(p: *Parse) void {
    p.ends_expr_tags.deinit(p.gpa);
    p.ends_sql_expr_identifiers.deinit(p.gpa);
    p.errors.deinit(p.gpa);
    p.nodes.deinit(p.gpa);
    p.extra_data.deinit(p.gpa);
    p.table_columns.deinit(p.gpa);
    for (p.values.items) |value| value.deinit(p.gpa);
    p.values.deinit(p.gpa);
    p.scratch.deinit(p.gpa);
    p.table_scratch.deinit(p.gpa);
}

const Blocks = struct {
    len: usize,
    lhs: Node.Index,
    rhs: Node.Index,

    fn toSpan(self: Blocks, p: *Parse) !Node.SubRange {
        if (self.len <= 2) {
            const nodes = [2]Node.Index{ self.lhs, self.rhs };
            return p.listToSpan(nodes[0..self.len]);
        } else {
            return Node.SubRange{ .start = self.lhs, .end = self.rhs };
        }
    }
};

const Precedence = enum {
    none,
    secondary,
    primary,
};

const OperInfo = struct {
    prefix: ?*const fn (*Parse) Error!Node.Index,
    infix: ?*const fn (*Parse, Node.Index) Error!Node.Index,
    prec: Precedence,
};

const SqlIdentifier = packed struct(u8) {
    by: bool = false,
    from: bool = false,
    where: bool = false,
    distinct: bool = false,
    _: u4 = 0,
};

pub fn listToSpan(p: *Parse, list: []const Node.Index) !Node.SubRange {
    try p.extra_data.appendSlice(p.gpa, list);
    return Node.SubRange{
        .start = @intCast(p.extra_data.items.len - list.len),
        .end = @intCast(p.extra_data.items.len),
    };
}

fn listToTable(p: *Parse, columns: [][]const u8, list: []const Node.Index, key_len: u32) !Node.Table {
    assert(columns.len == list.len);
    try p.table_columns.appendSlice(p.gpa, columns);
    try p.extra_data.appendSlice(p.gpa, list);
    return Node.Table{
        .column_start = @intCast(p.table_columns.items.len - columns.len),
        .expr_start = @intCast(p.extra_data.items.len - list.len),
        .key_len = key_len,
        .len = @intCast(list.len - key_len),
    };
}

pub fn addNode(p: *Parse, elem: Ast.Node) Allocator.Error!Node.Index {
    const result: Node.Index = @intCast(p.nodes.len);
    try p.nodes.append(p.gpa, elem);
    return result;
}

pub fn addExtra(p: *Parse, extra: anytype) Allocator.Error!Node.Index {
    const fields = std.meta.fields(@TypeOf(extra));
    try p.extra_data.ensureUnusedCapacity(p.gpa, fields.len);
    const result = @as(u32, @intCast(p.extra_data.items.len));
    inline for (fields) |field| {
        comptime assert(switch (field.type) {
            Node.Index, Node.SelectData, Node.ExecData => true,
            else => false,
        });
        p.extra_data.appendAssumeCapacity(@bitCast(@field(extra, field.name)));
    }
    return result;
}

fn addList(p: *Parse, list: []Node.Index) Allocator.Error!Node.Index {
    try p.extra_data.appendSlice(p.gpa, list);
    return @intCast(p.extra_data.items.len - list.len);
}

fn addColumnList(p: *Parse, list: [][]const u8) Allocator.Error!Node.Index {
    try p.table_columns.appendSlice(p.gpa, list);
    return @intCast(p.table_columns.items.len - list.len);
}

pub fn addValue(p: *Parse, value: Value) Allocator.Error!Node.Index {
    try p.values.append(p.gpa, value);
    return @intCast(p.values.items.len - 1);
}

fn warnExpected(p: *Parse, tag: Token.Tag) Allocator.Error!void {
    @setCold(true);
    try p.warnMsg(.{
        .tag = .expected_token,
        .token = p.tok_i,
        .extra = .{ .expected_tag = tag },
    });
}

fn warn(p: *Parse, error_tag: AstError.Tag) Allocator.Error!void {
    @setCold(true);
    try p.warnMsg(.{ .tag = error_tag, .token = p.tok_i });
}

fn warnMsg(p: *Parse, msg: Ast.Error) Allocator.Error!void {
    @setCold(true);
    try p.errors.append(p.gpa, msg);
}

pub fn fail(p: *Parse, tag: Ast.Error.Tag) Error {
    @setCold(true);
    return p.failMsg(.{ .tag = tag, .token = p.tok_i });
}

fn failExpected(p: *Parse, tag: Token.Tag) Error {
    @setCold(true);
    return p.failMsg(.{
        .tag = .expected_token,
        .token = p.tok_i,
        .extra = .{ .expected_tag = tag },
    });
}

pub fn failMsg(p: *Parse, msg: Ast.Error) Error {
    @setCold(true);
    try p.warnMsg(msg);
    return error.ParseError;
}

pub fn parseRoot(p: *Parse) Allocator.Error!void {
    // Root node must be index 0.
    p.nodes.appendAssumeCapacity(.{
        .tag = .root,
        .main_token = 0,
        .data = undefined,
    });

    const blocks = try p.parseBlocks();
    const root_decls = try blocks.toSpan(p);
    p.nodes.items(.data)[0] = .{
        .lhs = root_decls.start,
        .rhs = root_decls.end,
    };
}

fn parseBlocks(p: *Parse) Allocator.Error!Blocks {
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    while (true) {
        if (p.peekTag() == .eof) break;

        const expr = p.parseBlock() catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.ParseError,
            => blk: {
                try p.nextBlock(false);
                break :blk null_node;
            },
        };
        if (expr != 0) {
            try p.scratch.append(p.gpa, expr);
        }
    }

    const items = p.scratch.items[scratch_top..];
    switch (items.len) {
        0 => return Blocks{
            .len = 0,
            .rhs = 0,
            .lhs = 0,
        },
        1 => return Blocks{
            .len = 1,
            .lhs = items[0],
            .rhs = 0,
        },
        2 => return Blocks{
            .len = 2,
            .lhs = items[0],
            .rhs = items[1],
        },
        else => {
            const span = try p.listToSpan(items);
            return Blocks{
                .len = items.len,
                .lhs = span.start,
                .rhs = span.end,
            };
        },
    }
}

fn parseBlock(p: *Parse) Error!Node.Index {
    var node = try p.expectExpr(.semicolon);
    if (p.eatToken(.semicolon)) |_| {} else {
        node = try p.addNode(.{
            .tag = .implicit_return,
            .main_token = undefined,
            .data = .{
                .lhs = node,
                .rhs = undefined,
            },
        });
    }
    try p.nextBlock(true);
    return node;
}

fn parseExpr(p: *Parse, comptime tag: Token.Tag) Error!Node.Index {
    try p.ends_expr_tags.append(p.gpa, tag);
    defer _ = p.ends_expr_tags.pop();

    try p.ends_sql_expr_identifiers.append(p.gpa, null);
    defer _ = p.ends_sql_expr_identifiers.pop();

    return p.parsePrecedence(.secondary);
}

fn expectExpr(p: *Parse, comptime tag: Token.Tag) Error!Node.Index {
    const node = try p.parseExpr(tag);
    if (node == 0) {
        return p.fail(.expected_expr);
    } else {
        return node;
    }
}

fn parseSqlExpr(p: *Parse, comptime identifier: SqlIdentifier) Error!Node.Index {
    try p.ends_expr_tags.append(p.gpa, .semicolon);
    defer _ = p.ends_expr_tags.pop();

    try p.ends_sql_expr_identifiers.append(p.gpa, identifier);
    defer _ = p.ends_sql_expr_identifiers.pop();

    return p.parsePrecedence(.secondary);
}

fn expectSqlExpr(p: *Parse, comptime identifier: SqlIdentifier) Error!Node.Index {
    const node = try p.parseSqlExpr(identifier);
    if (node == 0) {
        return p.fail(.expected_expr);
    } else {
        return node;
    }
}

fn NoOp(comptime eat_token: bool) *const fn (*Parse) Error!Node.Index {
    return struct {
        fn impl(p: *Parse) Error!Node.Index {
            if (eat_token) _ = p.nextToken();
            return null_node;
        }
    }.impl;
}

fn Prefix(comptime tag: Node.Tag) *const fn (*Parse) Error!Node.Index {
    return struct {
        fn impl(p: *Parse) Error!Node.Index {
            const lhs = try p.addNode(.{
                .tag = tag,
                .main_token = p.nextToken(),
                .data = .{
                    .lhs = undefined,
                    .rhs = undefined,
                },
            });
            const iterator_tag: Node.Tag = switch (p.peekTag()) {
                .apostrophe => .apostrophe,
                .apostrophe_colon => .apostrophe_colon,
                .slash => .slash,
                .slash_colon => .slash_colon,
                .backslash => .backslash,
                .backslash_colon => .backslash_colon,
                else => return lhs,
            };

            return p.addNode(.{
                .tag = iterator_tag,
                .main_token = p.nextToken(),
                .data = .{
                    .lhs = lhs,
                    .rhs = undefined,
                },
            });
        }
    }.impl;
}

fn QPrefix(comptime tag: Node.Tag) *const fn (*Parse) Error!Node.Index {
    return struct {
        fn impl(p: *Parse) Error!Node.Index {
            return switch (p.language) {
                .k => return operTable[@intFromEnum(Token.Tag.identifier)].prefix.?(p),
                .q => return Prefix(tag)(p),
            };
        }
    }.impl;
}

fn Infix(comptime tag: Node.Tag) *const fn (*Parse, Node.Index) Error!Node.Index {
    return struct {
        fn impl(p: *Parse, lhs: Node.Index) Error!Node.Index {
            switch (tag) {
                .assign => {
                    const prev_tag = p.nodes.items(.tag)[lhs];
                    if (prev_tag != .identifier) {
                        return p.failMsg(.{
                            .tag = .expected_token,
                            .token = p.nodes.items(.main_token)[lhs],
                            .extra = .{ .expected_tag = .identifier },
                        });
                    }
                },
                else => {},
            }

            const main_token = p.nextToken();
            const iterator_tag: Node.Tag = switch (p.peekTag()) {
                .apostrophe => .apostrophe_infix,
                .apostrophe_colon => .apostrophe_colon_infix,
                .slash => .slash_infix,
                .slash_colon => .slash_colon_infix,
                .backslash => .backslash_infix,
                .backslash_colon => .backslash_colon_infix,
                else => return p.addNode(.{
                    .tag = tag,
                    .main_token = main_token,
                    .data = .{
                        .lhs = lhs,
                        .rhs = try p.parsePrecedence(.secondary),
                    },
                }),
            };

            const token = p.nextToken();
            const iterator = Node.Iterator{
                .lhs = lhs,
                .rhs = try p.parsePrecedence(.secondary),
            };
            return p.addNode(.{
                .tag = iterator_tag,
                .main_token = token,
                .data = .{
                    .lhs = try p.addNode(.{
                        .tag = tag,
                        .main_token = main_token,
                        .data = .{
                            .lhs = undefined,
                            .rhs = undefined,
                        },
                    }),
                    .rhs = try p.addExtra(iterator),
                },
            });
        }
    }.impl;
}

fn grouping(p: *Parse) Error!Node.Index {
    const l_paren = p.assertToken(.l_paren);
    if (p.eatToken(.r_paren)) |r_paren| {
        return p.addNode(.{
            .tag = .empty_list,
            .main_token = l_paren,
            .data = .{
                .lhs = undefined,
                .rhs = r_paren,
            },
        });
    }

    if (p.eatToken(.l_bracket)) |_| {
        return p.addTable(l_paren);
    }

    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    var r_paren: Node.Index = undefined;
    while (true) {
        if (p.eob) return p.failExpected(.r_paren);
        const expr = try p.parseExpr(.r_paren);
        try p.scratch.append(p.gpa, expr);
        switch (p.peekTag()) {
            .semicolon => _ = p.nextToken(),
            .r_paren => {
                r_paren = p.nextToken();
                break;
            },
            else => {},
        }
    }

    const expressions = p.scratch.items[scratch_top..];
    switch (expressions.len) {
        1 => return p.addNode(.{
            .tag = .grouped_expression,
            .main_token = l_paren,
            .data = .{
                .lhs = expressions[0],
                .rhs = r_paren,
            },
        }),
        else => {
            return p.addNode(.{
                .tag = .list,
                .main_token = l_paren,
                .data = .{
                    .lhs = try p.addExtra(try p.listToSpan(expressions)),
                    .rhs = r_paren,
                },
            });
        },
    }
}

fn addTable(p: *Parse, l_paren: TokenIndex) Error!Node.Index {
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    const table_scratch_top = p.table_scratch.items.len;
    defer p.table_scratch.shrinkRetainingCapacity(table_scratch_top);

    if (p.peekTag() != .r_bracket) {
        var key_hash_map = std.StringHashMap(u32).init(p.gpa);
        defer key_hash_map.deinit();

        while (true) {
            if (p.eob) return p.failExpected(.r_bracket);
            const identifier = if (p.peekTag() == .identifier and p.peekNextTag() == .colon) blk: {
                const identifier = p.assertToken(.identifier);
                _ = p.assertToken(.colon);
                break :blk identifier;
            } else 0;
            const expr = try p.expectExpr(.r_bracket);
            try p.scratch.append(p.gpa, expr);
            try p.addTableColumn(identifier, expr, &key_hash_map);
            switch (p.peekTag()) {
                .semicolon => _ = p.nextToken(),
                .r_bracket => break,
                else => {},
            }
        }
    }

    _ = try p.expectToken(.r_bracket);

    const key_len = p.scratch.items.len - scratch_top;

    var hash_map = std.StringHashMap(u32).init(p.gpa);
    defer hash_map.deinit();

    while (true) {
        if (p.eob) return p.failExpected(.r_paren);
        const identifier = if (p.peekTag() == .identifier and p.peekNextTag() == .colon) blk: {
            const identifier = p.assertToken(.identifier);
            _ = p.assertToken(.colon);
            break :blk identifier;
        } else 0;
        const expr = try p.expectExpr(.r_paren);
        try p.scratch.append(p.gpa, expr);
        try p.addTableColumn(identifier, expr, &hash_map);
        switch (p.peekTag()) {
            .semicolon => _ = p.nextToken(),
            .r_paren => break,
            else => {},
        }
    }
    const r_paren = p.assertToken(.r_paren);

    const columns = p.table_scratch.items[table_scratch_top..];
    const expressions = p.scratch.items[scratch_top..];
    return p.addNode(.{
        .tag = .table_literal,
        .main_token = l_paren,
        .data = .{
            .lhs = try p.addExtra(try p.listToTable(columns, expressions, @intCast(key_len))),
            .rhs = r_paren,
        },
    });
}

fn addTableColumn(p: *Parse, identifier: TokenIndex, expr: Node.Index, existing_columns: *std.StringHashMap(u32)) Error!void {
    var source = if (identifier == 0) blk: {
        if (p.findFirstIdentifier(expr)) |token_i| {
            const loc = p.token_locs[token_i];
            break :blk p.source[loc.start..loc.end];
        }
        break :blk "x";
    } else blk: {
        const loc = p.token_locs[identifier];
        break :blk p.source[loc.start..loc.end];
    };

    const a = try existing_columns.getOrPut(source);
    if (a.found_existing) {
        source = try std.fmt.allocPrint(p.gpa, "{s}{d}", .{ source, a.value_ptr.* });
        a.value_ptr.* += 1;
    } else {
        source = try p.gpa.dupe(u8, source);
        a.value_ptr.* = 1;
    }
    errdefer p.gpa.free(source); // TODO: leak

    try p.table_scratch.append(p.gpa, source);
}

fn extraData(p: Parse, index: usize, comptime T: type) T {
    const fields = std.meta.fields(T);
    var result: T = undefined;
    inline for (fields, 0..) |field, i| {
        comptime assert(field.type == Node.Index);
        @field(result, field.name) = p.extra_data.items[index + i];
    }
    return result;
}

fn findFirstIdentifier(p: *Parse, i: Node.Index) ?TokenIndex {
    const tag: Node.Tag = p.nodes.items(.tag)[i];
    switch (tag) {
        .grouped_expression,
        .implicit_return,
        .@"return",
        => return p.findFirstIdentifier(p.nodes.items(.data)[i].lhs),
        .list => {
            const data = p.nodes.items(.data)[i];
            const sub_range = p.extraData(data.lhs, Node.SubRange);
            for (sub_range.start..sub_range.end, 1..) |_, temp_i| {
                const node_i = p.extra_data.items[sub_range.end - temp_i];
                if (p.findFirstIdentifier(node_i)) |token_i| return token_i;
            }
            return null;
        },
        .identifier => return p.nodes.items(.main_token)[i],
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
        .implicit_apply,
        .call_one,
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
        .do_one,
        .if_one,
        .while_one,
        => return p.findFirstIdentifier(p.nodes.items(.data)[i].rhs),

        .apostrophe,
        .apostrophe_colon,
        .slash,
        .slash_colon,
        .backslash,
        .backslash_colon,
        => {
            const data = p.nodes.items(.data)[i];
            return p.findFirstIdentifier(data.rhs);
        },

        .apostrophe_infix,
        .apostrophe_colon_infix,
        .slash_infix,
        .slash_colon_infix,
        .backslash_infix,
        .backslash_colon_infix,
        => {
            const data = p.nodes.items(.data)[i];
            const iterator = p.extraData(data.rhs, Node.Iterator);
            return p.findFirstIdentifier(iterator.lhs);
        },

        .block => {
            const data = p.nodes.items(.data)[i];
            const sub_range = p.extraData(data.lhs, Node.SubRange);
            for (sub_range.start..sub_range.end, 1..) |_, temp_i| {
                const node_i = p.extra_data.items[sub_range.end - temp_i];
                if (p.findFirstIdentifier(node_i)) |token_i| return token_i;
            }
            return null;
        },
        .call,
        .do,
        .@"if",
        .@"while",
        => {
            const data = p.nodes.items(.data)[i];
            const sub_range = p.extraData(data.rhs, Node.SubRange);
            for (sub_range.start..sub_range.end, 1..) |_, temp_i| {
                const node_i = p.extra_data.items[sub_range.end - temp_i];
                if (p.findFirstIdentifier(node_i)) |token_i| return token_i;
            }
            return null;
        },

        .root,
        .empty_list,
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
        .lambda,
        .lambda_semicolon,
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
        .select,
        .exec,
        .update,
        .delete_rows,
        .delete_cols,
        .os,
        .current_directory,
        .change_directory,
        .load_file_or_directory,
        => return null,
    }
}

fn lambda(p: *Parse) Error!Node.Index {
    const l_brace = p.assertToken(.l_brace);

    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    if (p.eatToken(.l_bracket)) |l_bracket| {
        try p.scratch.append(p.gpa, l_bracket);
        if (p.peekTag() != .r_bracket) {
            while (true) {
                const identifier = try p.expectToken(.identifier);
                try p.scratch.append(p.gpa, identifier);
                if (p.eatToken(.semicolon)) |_| continue;
                break;
            }
        }
        const r_bracket = try p.expectToken(.r_bracket);
        try p.scratch.append(p.gpa, r_bracket);

        // Ensure there is whitespace between r_bracket and negative number.
        if (p.language == .q and p.peekTag() == .number_literal) {
            const loc = p.token_locs[p.tok_i];
            if (p.source[loc.start] == '-' and !std.ascii.isWhitespace(p.source[loc.start - 1])) {
                return p.fail(.expected_whitespace);
            }
        }
    }

    const body_top = p.scratch.items.len;

    while (true) {
        if (p.eob) return p.failExpected(.r_brace);
        if (p.peekTag() == .r_brace) break;
        if (p.eatToken(.semicolon)) |_| continue;
        const expr = try p.parseExpr(.r_brace);
        if (expr > 0) {
            try p.scratch.append(p.gpa, expr);
        }
    }
    const r_brace = try p.expectToken(.r_brace);

    var i = p.tok_i - 2;
    while (true) : (i -= 1) {
        if (p.token_tags[i] != .comment) break;
    }
    const semicolon = p.token_tags[i] == .semicolon;

    const params = try p.listToSpan(p.scratch.items[scratch_top..body_top]);
    const body = try p.listToSpan(p.scratch.items[body_top..]);
    const lambda_node = Node.Lambda{
        .params_start = params.start,
        .params_end = params.end,
        .body_start = body.start,
        .body_end = body.end,
    };
    return p.addNode(.{
        .tag = if (semicolon) .lambda_semicolon else .lambda,
        .main_token = l_brace,
        .data = .{
            .lhs = try p.addExtra(lambda_node),
            .rhs = r_brace,
        },
    });
}

fn block(p: *Parse) Error!Node.Index {
    const l_bracket = p.assertToken(.l_bracket);

    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    if (p.peekTag() != .r_bracket) {
        while (true) {
            if (p.eob) return p.failExpected(.r_bracket);
            const expr = try p.parseExpr(.r_bracket);
            try p.scratch.append(p.gpa, expr);
            switch (p.peekTag()) {
                .semicolon => _ = p.nextToken(),
                .r_bracket => break,
                else => {},
            }
        }
    }
    const r_bracket = try p.expectToken(.r_bracket);

    const expressions = p.scratch.items[scratch_top..];
    return p.addNode(.{
        .tag = .block,
        .main_token = l_bracket,
        .data = .{
            .lhs = try p.addExtra(try p.listToSpan(expressions)),
            .rhs = r_bracket,
        },
    });
}

fn string(p: *Parse) Error!Node.Index {
    const token: TokenIndex = p.peekToken();
    const loc = p.token_locs[token];
    const len = loc.end - loc.start;

    return (switch (len) {
        3 => Prefix(.char_literal),
        6 => blk: {
            const source = p.source[loc.start + 1 .. loc.end - 1];
            break :blk if (source[0] == '\\' and
                std.ascii.isDigit(source[1]) and
                std.ascii.isDigit(source[2]) and
                std.ascii.isDigit(source[3]))
                Prefix(.char_literal)
            else
                Prefix(.char_list_literal);
        },
        else => Prefix(.char_list_literal),
    })(p);
}

fn os(p: *Parse) Error!Node.Index {
    const main_token = p.assertToken(.os);

    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    while (!p.eob) {
        const param = try p.expectToken(.os_param);
        try p.scratch.append(p.gpa, param);
    }

    const params = p.scratch.items[scratch_top..];
    if (params.len > 0 and !p.tokensOnSameLine(main_token, params[params.len - 1])) {
        return p.fail(.os_expects_all_tokens_on_same_line);
    }

    const loc = p.token_locs[main_token];
    const source = p.source[loc.start..loc.end][1..];
    return switch (source.len) {
        0 => null,
        1 => switch (source[0]) {
            'l' => try p.loadFileOrDirectory(main_token, params),
            else => null,
        },
        2 => switch (source[0]) {
            'c' => switch (source[1]) {
                'd' => try if (params.len == 0) p.osInternal(.current_directory, main_token, params) else p.changeDirectory(main_token, params),
                else => null,
            },
            else => null,
        },
        else => null,
    } orelse p.osInternal(.os, main_token, params);
}

fn changeDirectory(p: *Parse, main_token: TokenIndex, params: []TokenIndex) Error!Node.Index {
    assert(params.len > 0);
    return p.osInternal(.change_directory, main_token, params);
}

fn loadFileOrDirectory(p: *Parse, main_token: TokenIndex, params: []TokenIndex) Error!Node.Index {
    if (params.len == 0) return p.failExpected(.os_param);

    return p.osInternal(.load_file_or_directory, main_token, params);
}

fn osInternal(p: *Parse, comptime tag: Node.Tag, main_token: TokenIndex, params: []TokenIndex) Error!Node.Index {
    return p.addNode(.{
        .tag = tag,
        .main_token = main_token,
        .data = .{
            .lhs = if (params.len > 0) try p.addExtra(try p.listToSpan(params)) else 0,
            .rhs = undefined,
        },
    });
}

fn do(p: *Parse) Error!Node.Index {
    const do_token = p.assertToken(.keyword_do);
    _ = try p.expectToken(.l_bracket);
    const iter = try p.expectExpr(.semicolon);
    _ = try p.expectToken(.semicolon);

    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    while (true) {
        if (p.eob) return p.failExpected(.r_bracket);
        const expr = try p.parseExpr(.r_bracket);
        try p.scratch.append(p.gpa, expr);
        switch (p.peekTag()) {
            .semicolon => _ = p.nextToken(),
            .r_bracket => {
                _ = p.nextToken();
                break;
            },
            else => {},
        }
    }

    const expressions = p.scratch.items[scratch_top..];
    switch (expressions.len) {
        0 => return p.addNode(.{
            .tag = .do_one,
            .main_token = do_token,
            .data = .{
                .lhs = iter,
                .rhs = 0,
            },
        }),
        1 => return p.addNode(.{
            .tag = .do_one,
            .main_token = do_token,
            .data = .{
                .lhs = iter,
                .rhs = expressions[0],
            },
        }),
        else => {
            return p.addNode(.{
                .tag = .do,
                .main_token = do_token,
                .data = .{
                    .lhs = iter,
                    .rhs = try p.addExtra(try p.listToSpan(expressions)),
                },
            });
        },
    }
}

fn @"if"(p: *Parse) Error!Node.Index {
    const if_token = p.assertToken(.keyword_if);
    _ = try p.expectToken(.l_bracket);
    const condition = try p.expectExpr(.semicolon);
    _ = try p.expectToken(.semicolon);

    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    while (true) {
        if (p.eob) return p.failExpected(.r_bracket);
        const expr = try p.parseExpr(.r_bracket);
        try p.scratch.append(p.gpa, expr);
        switch (p.peekTag()) {
            .semicolon => _ = p.nextToken(),
            .r_bracket => {
                _ = p.nextToken();
                break;
            },
            else => {},
        }
    }

    const expressions = p.scratch.items[scratch_top..];
    switch (expressions.len) {
        0 => return p.addNode(.{
            .tag = .if_one,
            .main_token = if_token,
            .data = .{
                .lhs = condition,
                .rhs = 0,
            },
        }),
        1 => return p.addNode(.{
            .tag = .if_one,
            .main_token = if_token,
            .data = .{
                .lhs = condition,
                .rhs = expressions[0],
            },
        }),
        else => {
            return p.addNode(.{
                .tag = .@"if",
                .main_token = if_token,
                .data = .{
                    .lhs = condition,
                    .rhs = try p.addExtra(try p.listToSpan(expressions)),
                },
            });
        },
    }
}

fn @"while"(p: *Parse) Error!Node.Index {
    const while_token = p.assertToken(.keyword_while);
    _ = try p.expectToken(.l_bracket);
    const condition = try p.expectExpr(.semicolon);
    _ = try p.expectToken(.semicolon);

    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    while (true) {
        if (p.eob) return p.failExpected(.r_bracket);
        const expr = try p.parseExpr(.r_bracket);
        try p.scratch.append(p.gpa, expr);
        switch (p.peekTag()) {
            .semicolon => _ = p.nextToken(),
            .r_bracket => {
                _ = p.nextToken();
                break;
            },
            else => {},
        }
    }

    const expressions = p.scratch.items[scratch_top..];
    switch (expressions.len) {
        0 => return p.addNode(.{
            .tag = .while_one,
            .main_token = while_token,
            .data = .{
                .lhs = condition,
                .rhs = 0,
            },
        }),
        1 => return p.addNode(.{
            .tag = .while_one,
            .main_token = while_token,
            .data = .{
                .lhs = condition,
                .rhs = expressions[0],
            },
        }),
        else => {
            return p.addNode(.{
                .tag = .@"while",
                .main_token = while_token,
                .data = .{
                    .lhs = condition,
                    .rhs = try p.addExtra(try p.listToSpan(expressions)),
                },
            });
        },
    }
}

// TODO: select a,by from([]a:0 1;by:2 3)
//               ^
// TODO: select from x where
//                         ^
fn select(p: *Parse) Error!Node.Index {
    const select_token = p.assertToken(.keyword_select);

    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    const table_scratch_top = p.table_scratch.items.len;
    defer p.table_scratch.shrinkRetainingCapacity(table_scratch_top);

    var hash_map = std.StringHashMap(u32).init(p.gpa);
    defer hash_map.deinit();

    // Limit expression
    var distinct = false;
    var ascending = false;
    var limit_expr: Node.Index = 0;
    var order_tok: TokenIndex = 0;
    if (p.peekTag() == .keyword_q_distinct) {
        var found_by = false;
        var counter = struct {
            paren: u32 = 0,
            brace: u32 = 0,
            bracket: u32 = 0,
        }{};
        // Skip ahead to find by clause.
        var i = p.tok_i + 1;
        var skipped_tokens: u32 = 0;
        while (true) : (i += 1) {
            switch (p.token_tags[i]) {
                .comment => continue,
                .l_paren => counter.paren += 1,
                .r_paren => counter.paren = @max(0, @as(i32, @intCast(counter.paren)) - 1),
                .l_brace => counter.brace += 1,
                .r_brace => counter.brace = @max(0, @as(i32, @intCast(counter.brace)) - 1),
                .l_bracket => counter.bracket += 1,
                .r_bracket => counter.bracket = @max(0, @as(i32, @intCast(counter.bracket)) - 1),
                .identifier => {
                    if (counter.paren == 0 and counter.brace == 0 and counter.bracket == 0) {
                        if (p.identifierEql("by", i)) {
                            i += 1;
                            while (true) : (i += 1) {
                                if (p.token_tags[i] != .comment) break;
                                if (p.token_eobs[i]) break;
                            }
                            if (!p.identifierEql("from", i)) {
                                found_by = true;
                            }
                            break;
                        } else if (p.identifierEql("from", i)) {
                            break;
                        }
                    }
                },
                else => {},
            }
            if (p.token_eobs[i]) break;
            skipped_tokens += 1;
        }

        if (!found_by or skipped_tokens == 0) {
            _ = p.assertToken(.keyword_q_distinct);
            distinct = true;
        }
    } else if (p.eatToken(.l_bracket)) |_| {
        switch (p.peekTag()) {
            .angle_bracket_left, .angle_bracket_left_colon, .angle_bracket_left_equal, .angle_bracket_left_right => {
                _ = p.nextToken();
                ascending = true;
                order_tok = try p.expectToken(.identifier);
            },
            .angle_bracket_right, .angle_bracket_right_colon, .angle_bracket_right_equal => {
                _ = p.nextToken();
                order_tok = try p.expectToken(.identifier);
            },
            else => {
                limit_expr = try p.expectExpr(.r_bracket);
                if (p.eatToken(.semicolon)) |_| {
                    switch (p.peekTag()) {
                        .angle_bracket_left, .angle_bracket_left_colon, .angle_bracket_left_equal, .angle_bracket_left_right => {
                            _ = p.nextToken();
                            ascending = true;
                            order_tok = try p.expectToken(.identifier);
                        },
                        .angle_bracket_right, .angle_bracket_right_colon, .angle_bracket_right_equal => {
                            _ = p.nextToken();
                            order_tok = try p.expectToken(.identifier);
                        },
                        else => {},
                    }
                }
            },
        }
        _ = try p.expectToken(.r_bracket);
    }

    // Select phrase
    const select_top = p.scratch.items.len;
    const select_columns_top = p.table_scratch.items.len;
    if (p.peekIdentifier(.{ .by = true, .from = true }) == null) {
        while (true) {
            if (p.eob) return p.fail(.expected_from);
            const identifier = if (p.peekTag() == .identifier and p.peekNextTag() == .colon) blk: {
                if (p.peekIdentifier(.{ .by = true, .from = true })) |_| return p.fail(.expected_select_phrase);
                const identifier = p.assertToken(.identifier);
                _ = p.assertToken(.colon);
                break :blk identifier;
            } else 0;
            const expr = try p.expectSqlExpr(.{ .by = true, .from = true });
            try p.scratch.append(p.gpa, expr);
            try p.addTableColumn(identifier, expr, &hash_map);
            if (p.eatToken(.comma)) |_| continue;
            break;
        }
    }

    // By phrase
    const by_top = p.scratch.items.len;
    const by_columns_top = p.table_scratch.items.len;
    const has_by = p.eatIdentifier(.{ .by = true }) != null;
    if (has_by) {
        const identifier = if (p.peekTag() == .identifier and p.peekNextTag() == .colon) blk: {
            if (p.peekIdentifier(.{ .from = true })) |_| return p.fail(.expected_by_phrase);
            const identifier = p.assertToken(.identifier);
            _ = p.assertToken(.colon);
            break :blk identifier;
        } else 0;
        const expr = try if (identifier > 0) p.expectSqlExpr(.{ .from = true }) else p.parseSqlExpr(.{ .from = true });
        if (expr > 0) {
            try p.scratch.append(p.gpa, expr);
            // TODO: Handle duplicates.
            try p.addTableColumn(identifier, expr, &hash_map);
            while (p.eatToken(.comma) != null) {
                if (p.eob) return p.fail(.expected_from);
                const sql_identifier = if (p.peekTag() == .identifier and p.peekNextTag() == .colon) blk: {
                    const sql_identifier = p.assertToken(.identifier);
                    _ = p.assertToken(.colon);
                    break :blk sql_identifier;
                } else 0;
                const sql_expr = try p.expectSqlExpr(.{ .from = true });
                try p.scratch.append(p.gpa, sql_expr);
                // TODO: Handle duplicates.
                try p.addTableColumn(sql_identifier, sql_expr, &hash_map);
            }
        }
    }

    // From phrase
    _ = try p.expectIdentifier(.{ .from = true });
    const from_expr = try p.expectSqlExpr(.{ .where = true });

    // Where phrase
    const where_top = p.scratch.items.len;
    if (p.eatToken(.keyword_q_where)) |_| {
        while (true) {
            const expr = try p.expectSqlExpr(.{});
            try p.scratch.append(p.gpa, expr);
            if (p.eatToken(.comma)) |_| continue;
            break;
        }
    }

    const select_node = Node.Select{
        .from = from_expr,
        .where = try p.addList(p.scratch.items[where_top..]),
        .by = try p.addList(p.scratch.items[by_top..where_top]),
        .by_columns = try p.addColumnList(p.table_scratch.items[by_columns_top..]),
        .select = try p.addList(p.scratch.items[select_top..by_top]),
        .select_end = @intCast(p.extra_data.items.len),
        .select_columns = try p.addColumnList(p.table_scratch.items[select_columns_top..by_columns_top]),
        .limit = limit_expr,
        .order = order_tok,
        .data = .{
            .has_by = has_by,
            .distinct = distinct and !has_by,
            .ascending = ascending,
        },
    };
    return p.addNode(.{
        .tag = .select,
        .main_token = select_token,
        .data = .{
            .lhs = try p.addExtra(select_node),
            .rhs = undefined,
        },
    });
}

fn exec(p: *Parse) Error!Node.Index {
    const exec_token = p.assertToken(.keyword_exec);

    if (p.peekTag() == .l_bracket) {
        return p.fail(.expected_qsql_token);
    }

    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    const table_scratch_top = p.table_scratch.items.len;
    defer p.table_scratch.shrinkRetainingCapacity(table_scratch_top);

    var hash_map = std.StringHashMap(u32).init(p.gpa);
    defer hash_map.deinit();

    const ColumnData = struct {
        identifier: TokenIndex,
        expr: Node.Index,
    };

    // Select phrase
    const select_top = p.scratch.items.len;
    const select_columns_top = p.table_scratch.items.len;
    var select_columns = std.ArrayList(ColumnData).init(p.gpa);
    defer select_columns.deinit();
    if (p.peekIdentifier(.{ .by = true, .from = true }) == null) {
        while (true) {
            if (p.eob) return p.fail(.expected_from);
            const identifier = if (p.peekTag() == .identifier and p.peekNextTag() == .colon) blk: {
                if (p.peekIdentifier(.{ .by = true, .from = true })) |_| return p.fail(.expected_select_phrase);
                const identifier = p.assertToken(.identifier);
                _ = p.assertToken(.colon);
                break :blk identifier;
            } else 0;
            const expr = try p.expectSqlExpr(.{ .by = true, .from = true });
            try p.scratch.append(p.gpa, expr);
            try select_columns.append(.{
                .identifier = identifier,
                .expr = expr,
            });
            if (p.eatToken(.comma)) |_| continue;
            break;
        }
    }
    var has_select_columns = select_columns.items.len > 1;
    switch (select_columns.items.len) {
        0 => {},
        1 => {
            const column = select_columns.items[0];
            if (column.identifier > 0) {
                has_select_columns = true;
                const loc = p.token_locs[column.identifier];
                const source = try p.gpa.dupe(u8, p.source[loc.start..loc.end]);
                errdefer p.gpa.free(source); // TODO: leak
                try p.table_scratch.append(p.gpa, source);
            }
        },
        else => {
            for (select_columns.items) |column| {
                try p.addTableColumn(column.identifier, column.expr, &hash_map);
            }
        },
    }

    // By phrase
    const by_top = p.scratch.items.len;
    const by_columns_top = p.table_scratch.items.len;
    var by_columns = std.ArrayList(ColumnData).init(p.gpa);
    defer by_columns.deinit();
    if (p.eatIdentifier(.{ .by = true })) |_| {
        while (true) {
            if (p.eob) return p.fail(.expected_from);
            const identifier = if (p.peekTag() == .identifier and p.peekNextTag() == .colon) blk: {
                if (p.peekIdentifier(.{ .from = true })) |_| return p.fail(.expected_by_phrase);
                const identifier = p.assertToken(.identifier);
                _ = p.assertToken(.colon);
                break :blk identifier;
            } else 0;
            const expr = try p.expectSqlExpr(.{ .from = true });
            try p.scratch.append(p.gpa, expr);
            try by_columns.append(.{
                .identifier = identifier,
                .expr = expr,
            });
            if (p.eatToken(.comma)) |_| continue;
            break;
        }
    }
    var has_by_columns = by_columns.items.len > 1;
    switch (by_columns.items.len) {
        0 => {},
        1 => {
            const column = by_columns.items[0];
            if (column.identifier > 0) {
                has_by_columns = true;
                const loc = p.token_locs[column.identifier];
                const source = try p.gpa.dupe(u8, p.source[loc.start..loc.end]);
                errdefer p.gpa.free(source); // TODO: leak
                try p.table_scratch.append(p.gpa, source);
            }
        },
        else => {
            for (by_columns.items) |column| {
                try p.addTableColumn(column.identifier, column.expr, &hash_map);
            }
        },
    }

    // From phrase
    _ = try p.expectIdentifier(.{ .from = true });
    const from_expr = try p.expectSqlExpr(.{ .where = true });

    // Where phrase
    const where_top = p.scratch.items.len;
    if (p.eatToken(.keyword_q_where)) |_| {
        while (true) {
            const expr = try p.expectSqlExpr(.{});
            try p.scratch.append(p.gpa, expr);
            if (p.eatToken(.comma)) |_| continue;
            break;
        }
    }

    const exec_node = Node.Exec{
        .from = from_expr,
        .where = try p.addList(p.scratch.items[where_top..]),
        .by = try p.addList(p.scratch.items[by_top..where_top]),
        .by_columns = try p.addColumnList(p.table_scratch.items[by_columns_top..]),
        .select = try p.addList(p.scratch.items[select_top..by_top]),
        .select_end = @intCast(p.extra_data.items.len),
        .select_columns = try p.addColumnList(p.table_scratch.items[select_columns_top..by_columns_top]),
        .data = .{
            .has_by_columns = has_by_columns,
            .has_select_columns = has_select_columns,
        },
    };
    return p.addNode(.{
        .tag = .exec,
        .main_token = exec_token,
        .data = .{
            .lhs = try p.addExtra(exec_node),
            .rhs = undefined,
        },
    });
}

fn update(p: *Parse) Error!Node.Index {
    const update_token = p.assertToken(.keyword_update);

    if (p.peekTag() == .l_bracket) {
        return p.fail(.expected_qsql_token);
    }

    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    const table_scratch_top = p.table_scratch.items.len;
    defer p.table_scratch.shrinkRetainingCapacity(table_scratch_top);

    var hash_map = std.StringHashMap(u32).init(p.gpa);
    defer hash_map.deinit();

    // Select phrase
    const select_top = p.scratch.items.len;
    const select_columns_top = p.table_scratch.items.len;
    while (true) {
        if (p.eob) return p.fail(.expected_from);
        const identifier = if (p.peekTag() == .identifier and p.peekNextTag() == .colon) blk: {
            if (p.peekIdentifier(.{ .by = true, .from = true })) |_| return p.fail(.expected_select_phrase);
            const identifier = p.assertToken(.identifier);
            _ = p.assertToken(.colon);
            break :blk identifier;
        } else 0;
        const expr = try p.expectSqlExpr(.{ .by = true, .from = true });
        try p.scratch.append(p.gpa, expr);
        try p.addTableColumn(identifier, expr, &hash_map);
        if (p.eatToken(.comma)) |_| continue;
        break;
    }

    // By phrase
    const by_top = p.scratch.items.len;
    const by_columns_top = p.table_scratch.items.len;
    if (p.eatIdentifier(.{ .by = true })) |_| {
        while (true) {
            if (p.eob) return p.fail(.expected_from);
            const identifier = if (p.peekTag() == .identifier and p.peekNextTag() == .colon) blk: {
                if (p.peekIdentifier(.{ .from = true })) |_| return p.fail(.expected_by_phrase);
                const identifier = p.assertToken(.identifier);
                _ = p.assertToken(.colon);
                break :blk identifier;
            } else 0;
            const expr = try p.expectSqlExpr(.{ .from = true });
            try p.scratch.append(p.gpa, expr);
            try p.addTableColumn(identifier, expr, &hash_map);
            if (p.eatToken(.comma)) |_| continue;
            break;
        }
    }

    // From phrase
    _ = try p.expectIdentifier(.{ .from = true });
    const from_expr = try p.expectSqlExpr(.{ .where = true });

    // Where phrase
    const where_top = p.scratch.items.len;
    if (p.eatToken(.keyword_q_where)) |_| {
        while (true) {
            const expr = try p.expectSqlExpr(.{});
            try p.scratch.append(p.gpa, expr);
            if (p.eatToken(.comma)) |_| continue;
            break;
        }
    }

    const exec_node = Node.Update{
        .from = from_expr,
        .where = try p.addList(p.scratch.items[where_top..]),
        .by = try p.addList(p.scratch.items[by_top..where_top]),
        .by_columns = try p.addColumnList(p.table_scratch.items[by_columns_top..]),
        .select = try p.addList(p.scratch.items[select_top..by_top]),
        .select_end = @intCast(p.extra_data.items.len),
        .select_columns = try p.addColumnList(p.table_scratch.items[select_columns_top..by_columns_top]),
    };
    return p.addNode(.{
        .tag = .update,
        .main_token = update_token,
        .data = .{
            .lhs = try p.addExtra(exec_node),
            .rhs = undefined,
        },
    });
}

// TODO: delete a from x where 1b
//                       ^
fn delete(p: *Parse) Error!Node.Index {
    const delete_token = p.assertToken(.keyword_delete);

    if (p.peekTag() == .l_bracket) {
        return p.fail(.expected_qsql_token);
    }

    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    const table_scratch_top = p.table_scratch.items.len;
    defer p.table_scratch.shrinkRetainingCapacity(table_scratch_top);

    if (p.eatIdentifier(.{ .from = true })) |_| {
        // From phrase
        const from_expr = try p.expectSqlExpr(.{ .where = true });

        // Where phrase
        const where_top = p.scratch.items.len;
        if (p.eatToken(.keyword_q_where)) |_| {
            while (true) {
                const expr = try p.expectSqlExpr(.{});
                try p.scratch.append(p.gpa, expr);
                if (p.eatToken(.comma)) |_| continue;
                break;
            }
        }

        const delete_node = Node.DeleteRows{
            .from = from_expr,
            .where = try p.addList(p.scratch.items[where_top..]),
            .where_end = @intCast(p.extra_data.items.len),
        };
        return p.addNode(.{
            .tag = .delete_rows,
            .main_token = delete_token,
            .data = .{
                .lhs = try p.addExtra(delete_node),
                .rhs = undefined,
            },
        });
    } else {
        // Select phrase
        const select_columns_top = p.table_scratch.items.len;
        while (true) {
            if (p.eob) return p.fail(.expected_from);
            if (p.peekIdentifier(.{ .from = true })) |_| return p.fail(.expected_from);
            const identifier = try p.expectToken(.identifier);
            const loc = p.token_locs[identifier];
            const source = try p.gpa.dupe(u8, p.source[loc.start..loc.end]);
            errdefer p.gpa.free(source); // TODO: leak
            try p.table_scratch.append(p.gpa, source);
            if (p.eatToken(.comma)) |_| continue;
            break;
        }

        // From phrase
        _ = try p.expectIdentifier(.{ .from = true });
        const from_expr = try p.expectSqlExpr(.{ .where = true });

        const delete_node = Node.DeleteColumns{
            .from = from_expr,
            .select_columns = try p.addColumnList(p.table_scratch.items[select_columns_top..]),
            .select_columns_end = @intCast(p.table_columns.items.len),
        };
        return p.addNode(.{
            .tag = .delete_cols,
            .main_token = delete_token,
            .data = .{
                .lhs = try p.addExtra(delete_node),
                .rhs = undefined,
            },
        });
    }
}

fn apply(p: *Parse, lhs: Node.Index) Error!Node.Index {
    return p.addNode(.{
        .tag = .implicit_apply,
        .main_token = undefined,
        .data = .{
            .lhs = lhs,
            .rhs = try p.parsePrecedence(.secondary),
        },
    });
}

fn qApply(p: *Parse, lhs: Node.Index) Error!Node.Index {
    return switch (p.language) {
        .k => p.applyIdentifier(lhs),
        .q => p.apply(lhs),
    };
}

fn applyIdentifier(p: *Parse, lhs: Node.Index) Error!Node.Index {
    const rhs = try p.addNode(.{
        .tag = .identifier,
        .main_token = p.nextToken(),
        .data = .{
            .lhs = undefined,
            .rhs = undefined,
        },
    });
    const iterator_tag: Node.Tag = switch (p.peekTag()) {
        .apostrophe => .apostrophe_infix,
        .apostrophe_colon => .apostrophe_colon_infix,
        .slash => .slash_infix,
        .slash_colon => .slash_colon_infix,
        .backslash => .backslash_infix,
        .backslash_colon => .backslash_colon_infix,
        else => return p.addNode(.{
            .tag = .implicit_apply,
            .main_token = undefined,
            .data = .{
                .lhs = lhs,
                .rhs = rhs,
            },
        }),
    };

    const token = p.nextToken();
    const iterator = Node.Iterator{
        .lhs = lhs,
        .rhs = try p.parsePrecedence(.secondary),
    };
    return p.addNode(.{
        .tag = iterator_tag,
        .main_token = token,
        .data = .{
            .lhs = rhs,
            .rhs = try p.addExtra(iterator),
        },
    });
}

fn call(p: *Parse, lhs: Node.Index) Error!Node.Index {
    const l_paren = p.assertToken(.l_bracket);

    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    while (true) {
        if (p.eob) return p.failExpected(.r_bracket);
        const expr = try p.parseExpr(.r_bracket);
        try p.scratch.append(p.gpa, expr);
        switch (p.peekTag()) {
            .semicolon => _ = p.nextToken(),
            .r_bracket => {
                _ = p.nextToken();
                break;
            },
            else => {},
        }
    }

    const params = p.scratch.items[scratch_top..];

    const expressions = p.scratch.items[scratch_top..];
    switch (expressions.len) {
        0 => unreachable,
        1 => return p.addNode(.{
            .tag = .call_one,
            .main_token = l_paren,
            .data = .{
                .lhs = lhs,
                .rhs = params[0],
            },
        }),
        else => {
            return p.addNode(.{
                .tag = .call,
                .main_token = l_paren,
                .data = .{
                    .lhs = lhs,
                    .rhs = try p.addExtra(try p.listToSpan(params)),
                },
            });
        },
    }
}

fn tokensOnSameLine(p: *Parse, token1: TokenIndex, token2: TokenIndex) bool {
    return std.mem.indexOfScalar(u8, p.source[p.token_locs[token1].start..p.token_locs[token2].end], '\n') == null;
}

fn skipComments(p: *Parse) void {
    while (p.token_tags[p.tok_i] == .comment) : (p.tok_i += 1) {}
}

pub fn peekTag(p: *Parse) Token.Tag {
    p.skipComments();
    // TODO: What if skipComments brought us to eof?
    return p.token_tags[p.tok_i];
}

fn peekNextTag(p: *Parse) Token.Tag {
    p.skipComments();
    var temp_i = p.tok_i + 1;
    while (p.token_tags[temp_i] == .comment) : (temp_i += 1) {}
    // TODO: What if skipComments brought us to eof?
    return p.token_tags[temp_i];
}

pub fn eatToken(p: *Parse, tag: Token.Tag) ?TokenIndex {
    return if (p.peekTag() == tag) p.nextToken() else null;
}

fn assertToken(p: *Parse, tag: Token.Tag) TokenIndex {
    const token = p.nextToken();
    assert(p.token_tags[token] == tag);
    return token;
}

fn expectToken(p: *Parse, tag: Token.Tag) Error!TokenIndex {
    if (p.peekTag() != tag) {
        return p.failMsg(.{
            .tag = .expected_token,
            .token = p.tok_i,
            .extra = .{ .expected_tag = tag },
        });
    }
    return p.nextToken();
}

fn peekIdentifier(p: *Parse, comptime identifier: SqlIdentifier) ?TokenIndex {
    switch (p.language) {
        .k => {
            if (p.peekTag() != .identifier) return null;
            if (identifier.by) if (p.identifierEql("by", p.tok_i)) return p.tok_i;
            if (identifier.from) if (p.identifierEql("from", p.tok_i)) return p.tok_i;
            if (identifier.where) if (p.identifierEql("where", p.tok_i)) return p.tok_i;
            if (identifier.distinct) if (p.identifierEql("distinct", p.tok_i)) return p.tok_i;
            return null;
        },
        .q => {
            if (identifier.where) if (p.peekTag() == .keyword_q_where) return p.tok_i;
            if (identifier.distinct) if (p.peekTag() == .keyword_q_distinct) return p.tok_i;

            if (p.peekTag() != .identifier) return null;
            if (identifier.by) if (p.identifierEql("by", p.tok_i)) return p.tok_i;
            if (identifier.from) if (p.identifierEql("from", p.tok_i)) return p.tok_i;
            return null;
        },
    }
}

fn eatIdentifier(p: *Parse, comptime identifier: SqlIdentifier) ?TokenIndex {
    if (p.peekIdentifier(identifier)) |token_i| {
        _ = p.nextToken();
        return token_i;
    }
    return null;
}

fn expectIdentifier(p: *Parse, comptime identifier: SqlIdentifier) Error!TokenIndex {
    if (p.eatIdentifier(identifier)) |token_i| return token_i;
    return p.failMsg(.{
        .tag = .expected_qsql_token,
        .token = p.tok_i,
        .extra = .{
            .expected_string = if (identifier.by)
                "by"
            else if (identifier.from)
                "from"
            else if (identifier.where)
                "where"
            else
                unreachable,
        },
    });
}

fn identifierEql(p: *Parse, comptime slice: []const u8, i: Node.Index) bool {
    if (p.token_tags[i] == .identifier) {
        const loc = p.token_locs[i];
        const source = p.source[loc.start..loc.end];
        return std.mem.eql(u8, source, slice);
    }
    return false;
}

pub fn nextToken(p: *Parse) TokenIndex {
    if (p.eob) return null_node;

    p.skipComments();
    const result = p.tok_i;
    p.eob = p.token_eobs[result];
    p.tok_i += 1;
    return result;
}

fn peekToken(p: *Parse) TokenIndex {
    if (p.eob) return null_node;

    p.skipComments();
    return p.tok_i;
}

fn nextBlock(p: *Parse, comptime should_warn: bool) Allocator.Error!void {
    if (!p.eob) {
        if (should_warn) try p.warn(.expected_end_of_block);
        while (!p.eob) {
            _ = p.nextToken();
        }
    }
    p.eob = false;
}

fn getRule(p: *Parse) OperInfo {
    if (p.eob) {
        return .{ .prefix = NoOp(true), .infix = null, .prec = .none };
    }
    const tag = p.peekTag();
    if (p.ends_expr_tags.getLastOrNull()) |t| if (tag == t or tag == .semicolon) {
        return .{ .prefix = NoOp(false), .infix = null, .prec = .none };
    };
    if (p.ends_sql_expr_identifiers.getLastOrNull()) |i| if (i) |sql_identifier| {
        switch (tag) {
            .identifier => {
                const loc = p.token_locs[p.tok_i];
                const source = p.source[loc.start..loc.end];
                if (sql_identifier.by and std.mem.eql(u8, source, "by")) return .{ .prefix = NoOp(false), .infix = null, .prec = .none };
                if (sql_identifier.from and std.mem.eql(u8, source, "from")) return .{ .prefix = NoOp(false), .infix = null, .prec = .none };
            },
            .keyword_q_where => {
                if (sql_identifier.where) return .{ .prefix = NoOp(false), .infix = null, .prec = .none };
            },
            .comma => return .{ .prefix = NoOp(false), .infix = null, .prec = .none },
            else => {},
        }
    };
    if (p.ends_sql_expr_identifiers.getLastOrNull()) |i| if (i != null) {
        switch (tag) {
            .identifier => {
                const loc = p.token_locs[p.tok_i];
                const source = p.source[loc.start..loc.end];
                if (i.?.by and std.mem.eql(u8, source, "by")) return .{ .prefix = NoOp(false), .infix = null, .prec = .none };
                if (i.?.from and std.mem.eql(u8, source, "from")) return .{ .prefix = NoOp(false), .infix = null, .prec = .none };
                if (i.?.where and std.mem.eql(u8, source, "where")) return .{ .prefix = NoOp(false), .infix = null, .prec = .none };
            },
            .comma => return .{ .prefix = NoOp(false), .infix = null, .prec = .none },
            else => {},
        }
    };
    return operTable[@intFromEnum(tag)];
}

fn parsePrecedence(p: *Parse, precedence: Precedence) Error!Node.Index {
    const prefix = p.getRule().prefix orelse {
        try p.warn(.expected_prefix_expr);
        _ = p.nextToken();
        return null_node;
    };
    var node = try prefix(p);

    while (@intFromEnum(precedence) <= @intFromEnum(p.getRule().prec)) {
        const infix = p.getRule().infix orelse {
            try p.warn(.expected_infix_expr);
            _ = p.nextToken();
            break;
        };
        node = try infix(p, node);
    }

    return node;
}

const operTable = operTable: {
    @setEvalBranchQuota(1045);
    break :operTable std.enums.directEnumArray(Token.Tag, OperInfo, 0, .{
        // Punctuation
        .l_paren = .{ .prefix = grouping, .infix = apply, .prec = .secondary },
        .r_paren = .{ .prefix = null, .infix = null, .prec = .none },
        .l_brace = .{ .prefix = lambda, .infix = apply, .prec = .secondary },
        .r_brace = .{ .prefix = null, .infix = null, .prec = .none },
        .l_bracket = .{ .prefix = block, .infix = call, .prec = .secondary },
        .r_bracket = .{ .prefix = null, .infix = null, .prec = .none },
        .semicolon = .{ .prefix = null, .infix = null, .prec = .none },

        // Verbs
        .colon = .{ .prefix = Prefix(.colon), .infix = Infix(.assign), .prec = .secondary },
        .colon_colon = .{ .prefix = Prefix(.colon_colon), .infix = Infix(.global_assign), .prec = .secondary },
        .plus = .{ .prefix = Prefix(.plus), .infix = Infix(.add), .prec = .secondary },
        .plus_colon = .{ .prefix = Prefix(.plus_colon), .infix = Infix(.plus_assign), .prec = .secondary },
        .minus = .{ .prefix = Prefix(.minus), .infix = Infix(.subtract), .prec = .secondary },
        .minus_colon = .{ .prefix = Prefix(.minus_colon), .infix = Infix(.minus_assign), .prec = .secondary },
        .asterisk = .{ .prefix = Prefix(.asterisk), .infix = Infix(.multiply), .prec = .secondary },
        .asterisk_colon = .{ .prefix = Prefix(.asterisk_colon), .infix = Infix(.asterisk_assign), .prec = .secondary },
        .percent = .{ .prefix = Prefix(.percent), .infix = Infix(.divide), .prec = .secondary },
        .percent_colon = .{ .prefix = Prefix(.percent_colon), .infix = Infix(.percent_assign), .prec = .secondary },
        .bang = .{ .prefix = Prefix(.bang), .infix = Infix(.dict), .prec = .secondary },
        .bang_colon = .{ .prefix = Prefix(.bang_colon), .infix = Infix(.bang_assign), .prec = .secondary },
        .ampersand = .{ .prefix = Prefix(.ampersand), .infix = Infix(.lesser), .prec = .secondary },
        .ampersand_colon = .{ .prefix = Prefix(.ampersand_colon), .infix = Infix(.ampersand_assign), .prec = .secondary },
        .pipe = .{ .prefix = Prefix(.pipe), .infix = Infix(.greater), .prec = .secondary },
        .pipe_colon = .{ .prefix = Prefix(.pipe_colon), .infix = Infix(.pipe_assign), .prec = .secondary },
        .angle_bracket_left = .{ .prefix = Prefix(.angle_bracket_left), .infix = Infix(.less_than), .prec = .secondary },
        .angle_bracket_left_colon = .{ .prefix = Prefix(.angle_bracket_left_colon), .infix = Infix(.angle_bracket_left_assign), .prec = .secondary },
        .angle_bracket_left_equal = .{ .prefix = Prefix(.angle_bracket_left_equal), .infix = Infix(.less_than_equal), .prec = .secondary },
        .angle_bracket_left_right = .{ .prefix = Prefix(.angle_bracket_left_right), .infix = Infix(.not_equal), .prec = .secondary },
        .angle_bracket_right = .{ .prefix = Prefix(.angle_bracket_right), .infix = Infix(.greater_than), .prec = .secondary },
        .angle_bracket_right_colon = .{ .prefix = Prefix(.angle_bracket_right_colon), .infix = Infix(.angle_bracket_right_assign), .prec = .secondary },
        .angle_bracket_right_equal = .{ .prefix = Prefix(.angle_bracket_right_equal), .infix = Infix(.greater_than_equal), .prec = .secondary },
        .equal = .{ .prefix = Prefix(.equal), .infix = Infix(.equals), .prec = .secondary },
        .equal_colon = .{ .prefix = Prefix(.equal_colon), .infix = Infix(.equal_assign), .prec = .secondary },
        .tilde = .{ .prefix = Prefix(.tilde), .infix = Infix(.match), .prec = .secondary },
        .tilde_colon = .{ .prefix = Prefix(.tilde_colon), .infix = Infix(.tilde_assign), .prec = .secondary },
        .comma = .{ .prefix = Prefix(.comma), .infix = Infix(.join), .prec = .secondary },
        .comma_colon = .{ .prefix = Prefix(.comma_colon), .infix = Infix(.comma_assign), .prec = .secondary },
        .caret = .{ .prefix = Prefix(.caret), .infix = Infix(.fill), .prec = .secondary },
        .caret_colon = .{ .prefix = Prefix(.caret_colon), .infix = Infix(.caret_assign), .prec = .secondary },
        .hash = .{ .prefix = Prefix(.hash), .infix = Infix(.take), .prec = .secondary },
        .hash_colon = .{ .prefix = Prefix(.hash_colon), .infix = Infix(.hash_assign), .prec = .secondary },
        .underscore = .{ .prefix = Prefix(.underscore), .infix = Infix(.drop), .prec = .secondary },
        .underscore_colon = .{ .prefix = Prefix(.underscore_colon), .infix = Infix(.underscore_assign), .prec = .secondary },
        .dollar = .{ .prefix = Prefix(.dollar), .infix = Infix(.cast), .prec = .secondary },
        .dollar_colon = .{ .prefix = Prefix(.dollar_colon), .infix = Infix(.dollar_assign), .prec = .secondary },
        .question_mark = .{ .prefix = Prefix(.question_mark), .infix = Infix(.find), .prec = .secondary },
        .question_mark_colon = .{ .prefix = Prefix(.question_mark_colon), .infix = Infix(.question_mark_assign), .prec = .secondary },
        .at = .{ .prefix = Prefix(.at), .infix = Infix(.apply), .prec = .secondary },
        .at_colon = .{ .prefix = Prefix(.at_colon), .infix = Infix(.at_assign), .prec = .secondary },
        .dot = .{ .prefix = Prefix(.dot), .infix = Infix(.apply_n), .prec = .secondary },
        .dot_colon = .{ .prefix = Prefix(.dot_colon), .infix = Infix(.dot_assign), .prec = .secondary },
        .zero_colon = .{ .prefix = Prefix(.zero_colon), .infix = Infix(.file_text), .prec = .secondary },
        .zero_colon_colon = .{ .prefix = Prefix(.zero_colon_colon), .infix = Infix(.zero_colon_assign), .prec = .secondary },
        .one_colon = .{ .prefix = Prefix(.one_colon), .infix = Infix(.file_binary), .prec = .secondary },
        .one_colon_colon = .{ .prefix = Prefix(.one_colon_colon), .infix = Infix(.one_colon_assign), .prec = .secondary },
        .two_colon = .{ .prefix = Prefix(.two_colon), .infix = Infix(.dynamic_load), .prec = .secondary },

        // Adverbs
        .apostrophe = .{ .prefix = Prefix(.apostrophe), .infix = null, .prec = .none },
        .apostrophe_colon = .{ .prefix = Prefix(.apostrophe_colon), .infix = null, .prec = .none },
        .slash = .{ .prefix = Prefix(.slash), .infix = null, .prec = .none },
        .slash_colon = .{ .prefix = Prefix(.slash_colon), .infix = null, .prec = .none },
        .backslash = .{ .prefix = Prefix(.backslash), .infix = null, .prec = .none },
        .backslash_colon = .{ .prefix = Prefix(.backslash_colon), .infix = null, .prec = .none },

        // Literals
        .number_literal = .{ .prefix = number_parser.parse, .infix = apply, .prec = .primary },
        .string_literal = .{ .prefix = string, .infix = apply, .prec = .secondary },
        .symbol_literal = .{ .prefix = Prefix(.symbol_literal), .infix = apply, .prec = .secondary },
        .symbol_list_literal = .{ .prefix = Prefix(.symbol_list_literal), .infix = apply, .prec = .secondary },
        .identifier = .{ .prefix = Prefix(.identifier), .infix = applyIdentifier, .prec = .secondary },

        // Misc.
        .comment = .{ .prefix = null, .infix = null, .prec = .none },
        .invalid = .{ .prefix = null, .infix = null, .prec = .none },
        .os = .{ .prefix = os, .infix = null, .prec = .none },
        .os_param = .{ .prefix = null, .infix = null, .prec = .none },
        .eof = .{ .prefix = null, .infix = null, .prec = .none },

        // Keywords
        .keyword_abs = .{ .prefix = Prefix(.abs), .infix = apply, .prec = .secondary },
        .keyword_acos = .{ .prefix = Prefix(.acos), .infix = apply, .prec = .secondary },
        .keyword_asin = .{ .prefix = Prefix(.asin), .infix = apply, .prec = .secondary },
        .keyword_atan = .{ .prefix = Prefix(.atan), .infix = apply, .prec = .secondary },
        .keyword_avg = .{ .prefix = Prefix(.avg), .infix = apply, .prec = .secondary },
        .keyword_bin = .{ .prefix = Prefix(.bin), .infix = Infix(.bin_infix), .prec = .secondary },
        .keyword_binr = .{ .prefix = Prefix(.binr), .infix = Infix(.binr_infix), .prec = .secondary },
        .keyword_cor = .{ .prefix = Prefix(.cor), .infix = Infix(.cor_infix), .prec = .secondary },
        .keyword_cos = .{ .prefix = Prefix(.cos), .infix = apply, .prec = .secondary },
        .keyword_cov = .{ .prefix = Prefix(.cov), .infix = Infix(.cov_infix), .prec = .secondary },
        .keyword_delete = .{ .prefix = delete, .infix = apply, .prec = .secondary },
        .keyword_dev = .{ .prefix = Prefix(.dev), .infix = apply, .prec = .secondary },
        .keyword_div = .{ .prefix = Prefix(.div), .infix = Infix(.div_infix), .prec = .secondary },
        .keyword_do = .{ .prefix = do, .infix = apply, .prec = .secondary },
        .keyword_enlist = .{ .prefix = Prefix(.enlist), .infix = apply, .prec = .secondary },
        .keyword_exec = .{ .prefix = exec, .infix = apply, .prec = .secondary },
        .keyword_exit = .{ .prefix = Prefix(.exit), .infix = apply, .prec = .secondary },
        .keyword_exp = .{ .prefix = Prefix(.exp), .infix = apply, .prec = .secondary },
        .keyword_getenv = .{ .prefix = Prefix(.getenv), .infix = apply, .prec = .secondary },
        .keyword_hopen = .{ .prefix = Prefix(.hopen), .infix = apply, .prec = .secondary },
        .keyword_if = .{ .prefix = @"if", .infix = apply, .prec = .secondary },
        .keyword_in = .{ .prefix = Prefix(.in), .infix = Infix(.in_infix), .prec = .secondary },
        .keyword_insert = .{ .prefix = Prefix(.insert), .infix = Infix(.insert_infix), .prec = .secondary },
        .keyword_last = .{ .prefix = Prefix(.last), .infix = apply, .prec = .secondary },
        .keyword_like = .{ .prefix = Prefix(.like), .infix = Infix(.like_infix), .prec = .secondary },
        .keyword_log = .{ .prefix = Prefix(.log), .infix = apply, .prec = .secondary },
        .keyword_max = .{ .prefix = Prefix(.max), .infix = apply, .prec = .secondary },
        .keyword_min = .{ .prefix = Prefix(.min), .infix = apply, .prec = .secondary },
        .keyword_prd = .{ .prefix = Prefix(.prd), .infix = apply, .prec = .secondary },
        .keyword_select = .{ .prefix = select, .infix = apply, .prec = .secondary },
        .keyword_setenv = .{ .prefix = Prefix(.setenv), .infix = Infix(.setenv_infix), .prec = .secondary },
        .keyword_sin = .{ .prefix = Prefix(.sin), .infix = apply, .prec = .secondary },
        .keyword_sqrt = .{ .prefix = Prefix(.sqrt), .infix = apply, .prec = .secondary },
        .keyword_ss = .{ .prefix = Prefix(.ss), .infix = Infix(.ss_infix), .prec = .secondary },
        .keyword_sum = .{ .prefix = Prefix(.sum), .infix = apply, .prec = .secondary },
        .keyword_tan = .{ .prefix = Prefix(.tan), .infix = apply, .prec = .secondary },
        .keyword_update = .{ .prefix = update, .infix = apply, .prec = .secondary },
        .keyword_var = .{ .prefix = Prefix(.@"var"), .infix = apply, .prec = .secondary },
        .keyword_wavg = .{ .prefix = Prefix(.wavg), .infix = Infix(.wavg_infix), .prec = .secondary },
        .keyword_while = .{ .prefix = @"while", .infix = apply, .prec = .secondary },
        .keyword_within = .{ .prefix = Prefix(.within), .infix = Infix(.within_infix), .prec = .secondary },
        .keyword_wsum = .{ .prefix = Prefix(.wsum), .infix = Infix(.wsum_infix), .prec = .secondary },
        .keyword_xexp = .{ .prefix = Prefix(.xexp), .infix = Infix(.xexp_infix), .prec = .secondary },

        // Q Keywords
        .keyword_q_aj = .{ .prefix = QPrefix(.aj), .infix = qApply, .prec = .secondary },
        .keyword_q_aj0 = .{ .prefix = QPrefix(.aj0), .infix = qApply, .prec = .secondary },
        .keyword_q_ajf = .{ .prefix = QPrefix(.ajf), .infix = qApply, .prec = .secondary },
        .keyword_q_ajf0 = .{ .prefix = QPrefix(.ajf0), .infix = qApply, .prec = .secondary },
        .keyword_q_all = .{ .prefix = QPrefix(.all), .infix = qApply, .prec = .secondary },
        .keyword_q_and = .{ .prefix = QPrefix(.@"and"), .infix = qApply, .prec = .secondary },
        .keyword_q_any = .{ .prefix = QPrefix(.any), .infix = qApply, .prec = .secondary },
        .keyword_q_asc = .{ .prefix = QPrefix(.asc), .infix = qApply, .prec = .secondary },
        .keyword_q_asof = .{ .prefix = QPrefix(.asof), .infix = qApply, .prec = .secondary },
        .keyword_q_attr = .{ .prefix = QPrefix(.attr), .infix = qApply, .prec = .secondary },
        .keyword_q_avgs = .{ .prefix = QPrefix(.avgs), .infix = qApply, .prec = .secondary },
        .keyword_q_ceiling = .{ .prefix = QPrefix(.ceiling), .infix = qApply, .prec = .secondary },
        .keyword_q_cols = .{ .prefix = QPrefix(.cols), .infix = qApply, .prec = .secondary },
        .keyword_q_count = .{ .prefix = QPrefix(.count), .infix = qApply, .prec = .secondary },
        .keyword_q_cross = .{ .prefix = QPrefix(.cross), .infix = qApply, .prec = .secondary },
        .keyword_q_csv = .{ .prefix = QPrefix(.csv), .infix = qApply, .prec = .secondary },
        .keyword_q_cut = .{ .prefix = QPrefix(.cut), .infix = qApply, .prec = .secondary },
        .keyword_q_deltas = .{ .prefix = QPrefix(.deltas), .infix = qApply, .prec = .secondary },
        .keyword_q_desc = .{ .prefix = QPrefix(.desc), .infix = qApply, .prec = .secondary },
        .keyword_q_differ = .{ .prefix = QPrefix(.differ), .infix = qApply, .prec = .secondary },
        .keyword_q_distinct = .{ .prefix = QPrefix(.distinct), .infix = qApply, .prec = .secondary },
        .keyword_q_dsave = .{ .prefix = QPrefix(.dsave), .infix = qApply, .prec = .secondary },
        .keyword_q_each = .{ .prefix = QPrefix(.each), .infix = qApply, .prec = .secondary },
        .keyword_q_ej = .{ .prefix = QPrefix(.ej), .infix = qApply, .prec = .secondary },
        .keyword_q_ema = .{ .prefix = QPrefix(.ema), .infix = qApply, .prec = .secondary },
        .keyword_q_eval = .{ .prefix = QPrefix(.eval), .infix = qApply, .prec = .secondary },
        .keyword_q_except = .{ .prefix = QPrefix(.except), .infix = qApply, .prec = .secondary },
        .keyword_q_fby = .{ .prefix = QPrefix(.fby), .infix = qApply, .prec = .secondary },
        .keyword_q_fills = .{ .prefix = QPrefix(.fills), .infix = qApply, .prec = .secondary },
        .keyword_q_first = .{ .prefix = QPrefix(.first), .infix = qApply, .prec = .secondary },
        .keyword_q_fkeys = .{ .prefix = QPrefix(.fkeys), .infix = qApply, .prec = .secondary },
        .keyword_q_flip = .{ .prefix = QPrefix(.flip), .infix = qApply, .prec = .secondary },
        .keyword_q_floor = .{ .prefix = QPrefix(.floor), .infix = qApply, .prec = .secondary },
        .keyword_q_get = .{ .prefix = QPrefix(.get), .infix = qApply, .prec = .secondary },
        .keyword_q_group = .{ .prefix = QPrefix(.group), .infix = qApply, .prec = .secondary },
        .keyword_q_gtime = .{ .prefix = QPrefix(.gtime), .infix = qApply, .prec = .secondary },
        .keyword_q_hclose = .{ .prefix = QPrefix(.hclose), .infix = qApply, .prec = .secondary },
        .keyword_q_hcount = .{ .prefix = QPrefix(.hcount), .infix = qApply, .prec = .secondary },
        .keyword_q_hdel = .{ .prefix = QPrefix(.hdel), .infix = qApply, .prec = .secondary },
        .keyword_q_hsym = .{ .prefix = QPrefix(.hsym), .infix = qApply, .prec = .secondary },
        .keyword_q_iasc = .{ .prefix = QPrefix(.iasc), .infix = qApply, .prec = .secondary },
        .keyword_q_idesc = .{ .prefix = QPrefix(.idesc), .infix = qApply, .prec = .secondary },
        .keyword_q_ij = .{ .prefix = QPrefix(.ij), .infix = qApply, .prec = .secondary },
        .keyword_q_ijf = .{ .prefix = QPrefix(.ijf), .infix = qApply, .prec = .secondary },
        .keyword_q_inter = .{ .prefix = QPrefix(.inter), .infix = qApply, .prec = .secondary },
        .keyword_q_inv = .{ .prefix = QPrefix(.inv), .infix = qApply, .prec = .secondary },
        .keyword_q_key = .{ .prefix = QPrefix(.key), .infix = qApply, .prec = .secondary },
        .keyword_q_keys = .{ .prefix = QPrefix(.keys), .infix = qApply, .prec = .secondary },
        .keyword_q_lj = .{ .prefix = QPrefix(.lj), .infix = qApply, .prec = .secondary },
        .keyword_q_ljf = .{ .prefix = QPrefix(.ljf), .infix = qApply, .prec = .secondary },
        .keyword_q_load = .{ .prefix = QPrefix(.load), .infix = qApply, .prec = .secondary },
        .keyword_q_lower = .{ .prefix = QPrefix(.lower), .infix = qApply, .prec = .secondary },
        .keyword_q_lsq = .{ .prefix = QPrefix(.lsq), .infix = qApply, .prec = .secondary },
        .keyword_q_ltime = .{ .prefix = QPrefix(.ltime), .infix = qApply, .prec = .secondary },
        .keyword_q_ltrim = .{ .prefix = QPrefix(.ltrim), .infix = qApply, .prec = .secondary },
        .keyword_q_mavg = .{ .prefix = QPrefix(.mavg), .infix = qApply, .prec = .secondary },
        .keyword_q_maxs = .{ .prefix = QPrefix(.maxs), .infix = qApply, .prec = .secondary },
        .keyword_q_mcount = .{ .prefix = QPrefix(.mcount), .infix = qApply, .prec = .secondary },
        .keyword_q_md5 = .{ .prefix = QPrefix(.md5), .infix = qApply, .prec = .secondary },
        .keyword_q_mdev = .{ .prefix = QPrefix(.mdev), .infix = qApply, .prec = .secondary },
        .keyword_q_med = .{ .prefix = QPrefix(.med), .infix = qApply, .prec = .secondary },
        .keyword_q_meta = .{ .prefix = QPrefix(.meta), .infix = qApply, .prec = .secondary },
        .keyword_q_mins = .{ .prefix = QPrefix(.mins), .infix = qApply, .prec = .secondary },
        .keyword_q_mmax = .{ .prefix = QPrefix(.mmax), .infix = qApply, .prec = .secondary },
        .keyword_q_mmin = .{ .prefix = QPrefix(.mmin), .infix = qApply, .prec = .secondary },
        .keyword_q_mmu = .{ .prefix = QPrefix(.mmu), .infix = qApply, .prec = .secondary },
        .keyword_q_mod = .{ .prefix = QPrefix(.mod), .infix = qApply, .prec = .secondary },
        .keyword_q_msum = .{ .prefix = QPrefix(.msum), .infix = qApply, .prec = .secondary },
        .keyword_q_neg = .{ .prefix = QPrefix(.neg), .infix = qApply, .prec = .secondary },
        .keyword_q_next = .{ .prefix = QPrefix(.next), .infix = qApply, .prec = .secondary },
        .keyword_q_not = .{ .prefix = QPrefix(.not), .infix = qApply, .prec = .secondary },
        .keyword_q_null = .{ .prefix = QPrefix(.null), .infix = qApply, .prec = .secondary },
        .keyword_q_or = .{ .prefix = QPrefix(.@"or"), .infix = qApply, .prec = .secondary },
        .keyword_q_over = .{ .prefix = QPrefix(.over), .infix = qApply, .prec = .secondary },
        .keyword_q_parse = .{ .prefix = QPrefix(.parse), .infix = qApply, .prec = .secondary },
        .keyword_q_peach = .{ .prefix = QPrefix(.peach), .infix = qApply, .prec = .secondary },
        .keyword_q_pj = .{ .prefix = QPrefix(.pj), .infix = qApply, .prec = .secondary },
        .keyword_q_prds = .{ .prefix = QPrefix(.prds), .infix = qApply, .prec = .secondary },
        .keyword_q_prev = .{ .prefix = QPrefix(.prev), .infix = qApply, .prec = .secondary },
        .keyword_q_prior = .{ .prefix = QPrefix(.prior), .infix = qApply, .prec = .secondary },
        .keyword_q_rand = .{ .prefix = QPrefix(.rand), .infix = qApply, .prec = .secondary },
        .keyword_q_rank = .{ .prefix = QPrefix(.rank), .infix = qApply, .prec = .secondary },
        .keyword_q_ratios = .{ .prefix = QPrefix(.ratios), .infix = qApply, .prec = .secondary },
        .keyword_q_raze = .{ .prefix = QPrefix(.raze), .infix = qApply, .prec = .secondary },
        .keyword_q_read0 = .{ .prefix = QPrefix(.read0), .infix = qApply, .prec = .secondary },
        .keyword_q_read1 = .{ .prefix = QPrefix(.read1), .infix = qApply, .prec = .secondary },
        .keyword_q_reciprocal = .{ .prefix = QPrefix(.reciprocal), .infix = qApply, .prec = .secondary },
        .keyword_q_reval = .{ .prefix = QPrefix(.reval), .infix = qApply, .prec = .secondary },
        .keyword_q_reverse = .{ .prefix = QPrefix(.reverse), .infix = qApply, .prec = .secondary },
        .keyword_q_rload = .{ .prefix = QPrefix(.rload), .infix = qApply, .prec = .secondary },
        .keyword_q_rotate = .{ .prefix = QPrefix(.rotate), .infix = qApply, .prec = .secondary },
        .keyword_q_rsave = .{ .prefix = QPrefix(.rsave), .infix = qApply, .prec = .secondary },
        .keyword_q_rtrim = .{ .prefix = QPrefix(.rtrim), .infix = qApply, .prec = .secondary },
        .keyword_q_save = .{ .prefix = QPrefix(.save), .infix = qApply, .prec = .secondary },
        .keyword_q_scan = .{ .prefix = QPrefix(.scan), .infix = qApply, .prec = .secondary },
        .keyword_q_scov = .{ .prefix = QPrefix(.scov), .infix = qApply, .prec = .secondary },
        .keyword_q_sdev = .{ .prefix = QPrefix(.sdev), .infix = qApply, .prec = .secondary },
        .keyword_q_set = .{ .prefix = QPrefix(.set), .infix = qApply, .prec = .secondary },
        .keyword_q_show = .{ .prefix = QPrefix(.show), .infix = qApply, .prec = .secondary },
        .keyword_q_signum = .{ .prefix = QPrefix(.signum), .infix = qApply, .prec = .secondary },
        .keyword_q_ssr = .{ .prefix = QPrefix(.ssr), .infix = qApply, .prec = .secondary },
        .keyword_q_string = .{ .prefix = QPrefix(.string), .infix = qApply, .prec = .secondary },
        .keyword_q_sublist = .{ .prefix = QPrefix(.sublist), .infix = qApply, .prec = .secondary },
        .keyword_q_sums = .{ .prefix = QPrefix(.sums), .infix = qApply, .prec = .secondary },
        .keyword_q_sv = .{ .prefix = QPrefix(.sv), .infix = qApply, .prec = .secondary },
        .keyword_q_svar = .{ .prefix = QPrefix(.svar), .infix = qApply, .prec = .secondary },
        .keyword_q_system = .{ .prefix = QPrefix(.system), .infix = qApply, .prec = .secondary },
        .keyword_q_tables = .{ .prefix = QPrefix(.tables), .infix = qApply, .prec = .secondary },
        .keyword_q_til = .{ .prefix = QPrefix(.til), .infix = qApply, .prec = .secondary },
        .keyword_q_trim = .{ .prefix = QPrefix(.trim), .infix = qApply, .prec = .secondary },
        .keyword_q_type = .{ .prefix = QPrefix(.type), .infix = qApply, .prec = .secondary },
        .keyword_q_uj = .{ .prefix = QPrefix(.uj), .infix = qApply, .prec = .secondary },
        .keyword_q_ujf = .{ .prefix = QPrefix(.ujf), .infix = qApply, .prec = .secondary },
        .keyword_q_ungroup = .{ .prefix = QPrefix(.ungroup), .infix = qApply, .prec = .secondary },
        .keyword_q_union = .{ .prefix = QPrefix(.@"union"), .infix = qApply, .prec = .secondary },
        .keyword_q_upper = .{ .prefix = QPrefix(.upper), .infix = qApply, .prec = .secondary },
        .keyword_q_upsert = .{ .prefix = QPrefix(.upsert), .infix = qApply, .prec = .secondary },
        .keyword_q_value = .{ .prefix = QPrefix(.value), .infix = qApply, .prec = .secondary },
        .keyword_q_view = .{ .prefix = QPrefix(.view), .infix = qApply, .prec = .secondary },
        .keyword_q_views = .{ .prefix = QPrefix(.views), .infix = qApply, .prec = .secondary },
        .keyword_q_vs = .{ .prefix = QPrefix(.vs), .infix = qApply, .prec = .secondary },
        .keyword_q_where = .{ .prefix = QPrefix(.where), .infix = qApply, .prec = .secondary },
        .keyword_q_wj = .{ .prefix = QPrefix(.wj), .infix = qApply, .prec = .secondary },
        .keyword_q_wj1 = .{ .prefix = QPrefix(.wj1), .infix = qApply, .prec = .secondary },
        .keyword_q_ww = .{ .prefix = QPrefix(.ww), .infix = qApply, .prec = .secondary },
        .keyword_q_xasc = .{ .prefix = QPrefix(.xasc), .infix = qApply, .prec = .secondary },
        .keyword_q_xbar = .{ .prefix = QPrefix(.xbar), .infix = qApply, .prec = .secondary },
        .keyword_q_xcol = .{ .prefix = QPrefix(.xcol), .infix = qApply, .prec = .secondary },
        .keyword_q_xcols = .{ .prefix = QPrefix(.xcols), .infix = qApply, .prec = .secondary },
        .keyword_q_xdesc = .{ .prefix = QPrefix(.xdesc), .infix = qApply, .prec = .secondary },
        .keyword_q_xgroup = .{ .prefix = QPrefix(.xgroup), .infix = qApply, .prec = .secondary },
        .keyword_q_xkey = .{ .prefix = QPrefix(.xkey), .infix = qApply, .prec = .secondary },
        .keyword_q_xlog = .{ .prefix = QPrefix(.xlog), .infix = qApply, .prec = .secondary },
        .keyword_q_xprev = .{ .prefix = QPrefix(.xprev), .infix = qApply, .prec = .secondary },
        .keyword_q_xrank = .{ .prefix = QPrefix(.xrank), .infix = qApply, .prec = .secondary },
    });
};

const null_node: Node.Index = 0;

const Parse = @This();
const std = @import("std");
const assert = std.debug.assert;
const panic = std.debug.panic;
const Allocator = std.mem.Allocator;
const kdb = @import("../kdb.zig");
const Ast = kdb.Ast;
const Node = Ast.Node;
const AstError = Ast.Error;
const TokenIndex = Ast.TokenIndex;
const Token = kdb.Token;
const number_parser = @import("parser/number_parser.zig");
const Value = number_parser.Value;

const log = std.log.scoped(.kdblint_parse);

fn appendTags(tree: Ast, i: Node.Index, tags: *std.ArrayList(Node.Tag)) !void {
    const tag: Node.Tag = tree.nodes.items(.tag)[i];
    try tags.append(tag);

    switch (tag) {
        .implicit_return,
        .grouped_expression,
        => {
            const data = tree.nodes.items(.data)[i];
            try appendTags(tree, data.lhs, tags);
        },
        .call_one,
        .add,
        .subtract,
        .global_assign,
        .apply,
        => {
            const data = tree.nodes.items(.data)[i];
            if (data.rhs > 0) try appendTags(tree, data.rhs, tags);
            if (data.lhs > 0) try appendTags(tree, data.lhs, tags);
        },

        .backslash_colon => {
            const data = tree.nodes.items(.data)[i];
            if (data.lhs > 0) try appendTags(tree, data.lhs, tags);
        },
        .backslash_colon_infix => {
            const data = tree.nodes.items(.data)[i];
            const iterator = tree.extraData(data.rhs, Node.Iterator);
            try appendTags(tree, iterator.rhs, tags);
            try appendTags(tree, data.lhs, tags);
            try appendTags(tree, iterator.lhs, tags);
        },

        .lambda,
        .lambda_semicolon,
        => {
            const data = tree.nodes.items(.data)[i];
            const lambda_node = tree.extraData(data.lhs, Node.Lambda);
            const body = tree.extra_data[lambda_node.body_start..lambda_node.body_end];
            for (body) |expr| {
                try appendTags(tree, expr, tags);
            }
        },

        .identifier,
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
        .empty_list,
        .distinct,
        .sum,
        .til,
        .minus,
        .at,
        .os,
        .current_directory,
        .change_directory,
        .load_file_or_directory,
        => {},
        .call => {
            const data = tree.nodes.items(.data)[i];
            const sub_range = tree.extraData(data.rhs, Node.SubRange);
            for (sub_range.start..sub_range.end, 1..) |_, temp_i| {
                const extra_data_i = sub_range.end - temp_i;
                const node_i = tree.extra_data[extra_data_i];
                if (node_i > 0) {
                    try appendTags(tree, node_i, tags);
                }
            }
            try appendTags(tree, data.lhs, tags);
        },
        .block => {
            const data = tree.nodes.items(.data)[i];
            const sub_range = tree.extraData(data.lhs, Node.SubRange);
            const exprs = tree.extra_data[sub_range.start..sub_range.end];
            for (exprs) |expr| {
                if (expr > 0) try appendTags(tree, expr, tags);
            }
        },
        .list => {
            const data = tree.nodes.items(.data)[i];
            const sub_range = tree.extraData(data.lhs, Node.SubRange);
            for (sub_range.start..sub_range.end, 1..) |_, temp_i| {
                const extra_data_i = sub_range.end - temp_i;
                const node_i = tree.extra_data[extra_data_i];
                if (node_i > 0) {
                    try appendTags(tree, node_i, tags);
                }
            }
        },
        .table_literal => {
            const data = tree.nodes.items(.data)[i];
            const table = tree.extraData(data.lhs, Node.Table);
            for (table.expr_start + table.key_len..table.expr_start + table.key_len + table.len, 1..) |_, temp_i| {
                const node_i = tree.extra_data[table.expr_start + table.key_len + table.len - temp_i];
                try appendTags(tree, node_i, tags);
            }
            for (table.expr_start..table.expr_start + table.key_len, 1..) |_, temp_i| {
                const node_i = tree.extra_data[table.expr_start + table.key_len - temp_i];
                try appendTags(tree, node_i, tags);
            }
        },
        .implicit_apply => {
            const data = tree.nodes.items(.data)[i];
            try appendTags(tree, data.rhs, tags);
            try appendTags(tree, data.lhs, tags);
        },
        .select => {
            const data = tree.nodes.items(.data)[i];
            const select_node = tree.extraData(data.lhs, Node.Select);

            try appendTags(tree, select_node.from, tags);

            for (tree.extra_data[select_node.where..select_node.by]) |where_i| {
                try appendTags(tree, where_i, tags);
            }

            if (select_node.data.has_by) {
                for (tree.extra_data[select_node.by..select_node.select]) |by_i| {
                    try appendTags(tree, by_i, tags);
                }
            }

            for (tree.extra_data[select_node.select..select_node.select_end]) |select_i| {
                try appendTags(tree, select_i, tags);
            }

            if (select_node.limit > 0) {
                try appendTags(tree, select_node.limit, tags);
            }

            if (select_node.order > 0) {
                try tags.append(.identifier);
            }
        },
        .exec => {
            const data = tree.nodes.items(.data)[i];
            const exec_node = tree.extraData(data.lhs, Node.Exec);

            try appendTags(tree, exec_node.from, tags);

            for (tree.extra_data[exec_node.where..exec_node.by]) |where_i| {
                try appendTags(tree, where_i, tags);
            }

            for (tree.extra_data[exec_node.by..exec_node.select]) |by_i| {
                try appendTags(tree, by_i, tags);
            }

            for (tree.extra_data[exec_node.select..exec_node.select_end]) |select_i| {
                try appendTags(tree, select_i, tags);
            }
        },
        .update => {
            const data = tree.nodes.items(.data)[i];
            const update_node = tree.extraData(data.lhs, Node.Update);

            try appendTags(tree, update_node.from, tags);

            for (tree.extra_data[update_node.where..update_node.by]) |where_i| {
                try appendTags(tree, where_i, tags);
            }

            for (tree.extra_data[update_node.by..update_node.select]) |by_i| {
                try appendTags(tree, by_i, tags);
            }

            for (tree.extra_data[update_node.select..update_node.select_end]) |select_i| {
                try appendTags(tree, select_i, tags);
            }
        },
        .delete_rows => {
            const data = tree.nodes.items(.data)[i];
            const delete_node = tree.extraData(data.lhs, Node.DeleteRows);

            try appendTags(tree, delete_node.from, tags);

            for (tree.extra_data[delete_node.where..delete_node.where_end]) |where_i| {
                try appendTags(tree, where_i, tags);
            }
        },
        .delete_cols => {
            const data = tree.nodes.items(.data)[i];
            const delete_node = tree.extraData(data.lhs, Node.DeleteColumns);

            try appendTags(tree, delete_node.from, tags);

            for (tree.extra_data[delete_node.select_columns..delete_node.select_columns_end]) |_| {
                try tags.append(.identifier);
            }
        },
        else => |t| panic("{s}", .{@tagName(t)}),
    }
}

pub fn testParse(source: [:0]const u8, expected_tags: []const Node.Tag, expected_parse_tree: []const u8) !void {
    inline for (&.{.@"4.0"}) |version| {
        inline for (&.{ .k, .q }) |language| {
            try testParseSettings(.{
                .version = version,
                .language = language,
            }, source, expected_tags, expected_parse_tree);
        }
    }
}

fn testParseVersion(version: Ast.Version, source: [:0]const u8, expected_tags: []const Node.Tag, expected_parse_tree: []const u8) !void {
    inline for (&.{ .k, .q }) |language| {
        try testParseSettings(.{
            .version = version,
            .language = language,
        }, source, expected_tags, expected_parse_tree);
    }
}

fn testParseLanguage(language: Ast.Language, source: [:0]const u8, expected_tags: []const Node.Tag, expected_parse_tree: []const u8) !void {
    inline for (&.{.@"4.0"}) |version| {
        try testParseSettings(.{
            .version = version,
            .language = language,
        }, source, expected_tags, expected_parse_tree);
    }
}

fn testParseSettings(settings: Ast.ParseSettings, source: [:0]const u8, expected_tags: []const Node.Tag, expected_parse_tree: []const u8) !void {
    var tree = try Ast.parse(std.testing.allocator, source, settings);
    defer tree.deinit(std.testing.allocator);

    const errors = try std.testing.allocator.alloc(Ast.Error.Tag, tree.errors.len);
    defer std.testing.allocator.free(errors);
    for (tree.errors, 0..) |err, i| {
        errors[i] = err.tag;
    }
    try std.testing.expectEqualSlices(Ast.Error.Tag, &.{}, errors);

    const data = tree.nodes.items(.data)[0];
    const i = tree.extra_data[data.lhs];

    var tags = std.ArrayList(Node.Tag).init(std.testing.allocator);
    defer tags.deinit();
    try appendTags(tree, i, &tags);

    try std.testing.expectEqualSlices(Node.Tag, expected_tags, tags.items);

    var parse_tree = std.ArrayList(u8).init(std.testing.allocator);
    defer parse_tree.deinit();
    try tree.print(i, parse_tree.writer(), std.testing.allocator);

    try std.testing.expectEqualSlices(u8, expected_parse_tree, parse_tree.items);
}

pub fn testParseError(source: [:0]const u8, expected_tags: []const Ast.Error.Tag) !void {
    inline for (&.{.@"4.0"}) |version| {
        inline for (&.{ .k, .q }) |language| {
            try testParseErrorSettings(.{
                .version = version,
                .language = language,
            }, source, expected_tags);
        }
    }
}

fn testParseErrorVersion(version: Ast.Version, source: [:0]const u8, expected_tags: []const Ast.Error.Tag) !void {
    inline for (&.{ .k, .q }) |language| {
        try testParseErrorSettings(.{
            .version = version,
            .language = language,
        }, source, expected_tags);
    }
}

fn testParseErrorLanguage(language: Ast.Language, source: [:0]const u8, expected_tags: []const Ast.Error.Tag) !void {
    inline for (&.{.@"4.0"}) |version| {
        try testParseErrorSettings(.{
            .version = version,
            .language = language,
        }, source, expected_tags);
    }
}

fn testParseErrorSettings(settings: Ast.ParseSettings, source: [:0]const u8, expected_tags: []const Ast.Error.Tag) !void {
    var tree = try Ast.parse(std.testing.allocator, source, settings);
    defer tree.deinit(std.testing.allocator);

    const errors = try std.testing.allocator.alloc(Ast.Error.Tag, tree.errors.len);
    defer std.testing.allocator.free(errors);
    for (tree.errors, 0..) |err, i| {
        errors[i] = err.tag;
    }
    try std.testing.expectEqualSlices(Ast.Error.Tag, expected_tags, errors);
}

test "expressions" {
    try testParse("1", &.{ .implicit_return, .long_literal }, "1");
    try testParse("1;", &.{.long_literal}, "1");
}

test "lambda" {
    try testParse("{x}", &.{
        .implicit_return,
        .lambda,
        .identifier,
    }, "{x}");
    try testParse("{x;}", &.{
        .implicit_return,
        .lambda_semicolon,
        .identifier,
    }, "{x;}");
    try testParse("{x;y}", &.{
        .implicit_return,
        .lambda,
        .identifier,
        .identifier,
    }, "{x;y}");
    try testParse("{x;y;}", &.{
        .implicit_return,
        .lambda_semicolon,
        .identifier,
        .identifier,
    }, "{x;y;}");
    try testParse("{-1}", &.{ .implicit_return, .lambda, .long_literal }, "{-1}");
    try testParse("{-1;}", &.{ .implicit_return, .lambda_semicolon, .long_literal }, "{-1;}");
    try testParseErrorLanguage(.q, "{[]-1}", &.{.expected_whitespace});
    try testParseErrorLanguage(.q, "{[]-1;}", &.{.expected_whitespace});
    try testParseLanguage(.k, "{[]-1}", &.{ .implicit_return, .lambda, .long_literal }, "{[]-1}");
    try testParseLanguage(.k, "{[]-1;}", &.{ .implicit_return, .lambda_semicolon, .long_literal }, "{[]-1;}");
    try testParse("{[] -1}", &.{ .implicit_return, .lambda, .long_literal }, "{[] -1}");
    try testParse("{[] -1;}", &.{ .implicit_return, .lambda_semicolon, .long_literal }, "{[] -1;}");
}

test "implicit apply" {
    try testParse("{x}a", &.{
        .implicit_return,
        .implicit_apply,
        .identifier,
        .lambda,
        .identifier,
    }, "({x};`a)");
    try testParse("{x}1", &.{
        .implicit_return,
        .implicit_apply,
        .long_literal,
        .lambda,
        .identifier,
    }, "({x};1)");
}

test "call" {
    try testParse("{x}[]", &.{
        .implicit_return,
        .call_one,
        .lambda,
        .identifier,
    }, "({x};::)");
    try testParse("{x}[1]", &.{
        .implicit_return,
        .call_one,
        .long_literal,
        .lambda,
        .identifier,
    }, "({x};1)");
    try testParse("{x+y}[1;a]", &.{
        .implicit_return,
        .call,
        .identifier,
        .long_literal,
        .lambda,
        .add,
        .identifier,
        .identifier,
    }, "({x+y};1;`a)");
    try testParse("{1}[]-1", &.{
        .implicit_return,
        .subtract,
        .long_literal,
        .call_one,
        .lambda,
        .long_literal,
    }, "(-;({1};::);1)");
}

test "call - explicit projection" {
    try testParse("{x+y}[;]", &.{
        .implicit_return,
        .call,
        .lambda,
        .add,
        .identifier,
        .identifier,
    }, "({x+y};::;::)");
    try testParse("{x+y}[1;]", &.{
        .implicit_return,
        .call,
        .long_literal,
        .lambda,
        .add,
        .identifier,
        .identifier,
    }, "({x+y};1;::)");
    try testParse("{x+y}[;2]", &.{
        .implicit_return,
        .call,
        .long_literal,
        .lambda,
        .add,
        .identifier,
        .identifier,
    }, "({x+y};::;2)");
}

test "call - implicit projection" {
    try testParse("{x+y}[1]", &.{
        .implicit_return,
        .call_one,
        .long_literal,
        .lambda,
        .add,
        .identifier,
        .identifier,
    }, "({x+y};1)");
}

test "block" {
    try testParse("[]", &.{ .implicit_return, .block }, "::");
    try testParse("[0]", &.{ .implicit_return, .block, .long_literal }, "0");
    try testParse("[0;]", &.{ .implicit_return, .block, .long_literal }, "(\";\";0;::)");
    try testParse("[;1]", &.{ .implicit_return, .block, .long_literal }, "(\";\";::;1)");
    try testParse("[0;1]", &.{ .implicit_return, .block, .long_literal, .long_literal }, "(\";\";0;1)");
    try testParse("[0;1;]", &.{ .implicit_return, .block, .long_literal, .long_literal }, "(\";\";0;1;::)");
    try testParse("[0;1;2]", &.{ .implicit_return, .block, .long_literal, .long_literal, .long_literal }, "(\";\";0;1;2)");
}

test "list" {
    try testParse("()", &.{ .implicit_return, .empty_list }, "()");
    try testParse("(0)", &.{ .implicit_return, .grouped_expression, .long_literal }, "0");
    try testParse("(0;)", &.{ .implicit_return, .list, .long_literal }, "(enlist;0;::)");
    try testParse("(;1)", &.{ .implicit_return, .list, .long_literal }, "(enlist;::;1)");
    try testParse("(0;1)", &.{ .implicit_return, .list, .long_literal, .long_literal }, "(enlist;0;1)");
    try testParse("(0;1;)", &.{ .implicit_return, .list, .long_literal, .long_literal }, "(enlist;0;1;::)");
    try testParse("(0;1;2)", &.{ .implicit_return, .list, .long_literal, .long_literal, .long_literal }, "(enlist;0;1;2)");
}

test "table" {
    try testParse("([]())", &.{ .implicit_return, .table_literal, .empty_list }, "(+:;(!;,,`x;(enlist;())))");
    try testParse("([]1 2)", &.{ .implicit_return, .table_literal, .long_list_literal }, "(+:;(!;,,`x;(enlist;1 2)))");
    try testParse("([]a:1 2)", &.{ .implicit_return, .table_literal, .long_list_literal }, "(+:;(!;,,`a;(enlist;1 2)))");
    try testParse("([]a::1 2)", &.{ .implicit_return, .table_literal, .global_assign, .long_list_literal, .identifier }, "(+:;(!;,,`x;(enlist;(::;`a;1 2))))");
    try testParse("([]a:1 2;b:2)", &.{ .implicit_return, .table_literal, .long_literal, .long_list_literal }, "(+:;(!;,`a`b;(enlist;1 2;2)))");
    try testParse("([]a)", &.{ .implicit_return, .table_literal, .identifier }, "(+:;(!;,,`a;(enlist;`a)))");
    try testParse("([]b+sum a)", &.{ .implicit_return, .table_literal, .add, .implicit_apply, .identifier, .sum, .identifier }, "(+:;(!;,,`a;(enlist;(+;`b;(sum;`a)))))");
    try testParse("([]sum[a]+b)", &.{ .implicit_return, .table_literal, .add, .identifier, .call_one, .identifier, .sum }, "(+:;(!;,,`b;(enlist;(+;(sum;`a);`b))))");
    try testParse("([](a;b;c))", &.{ .implicit_return, .table_literal, .list, .identifier, .identifier, .identifier }, "(+:;(!;,,`c;(enlist;(enlist;`a;`b;`c))))");
    try testParseLanguage(.k, "([]til 10;x1:1;2)", &.{ .implicit_return, .table_literal, .long_literal, .long_literal, .implicit_apply, .long_literal, .identifier }, "(+:;(!;,`x`x1`x1;(enlist;(`til;10);1;2)))");
    try testParseLanguage(.q, "([]til 10;x1:1;2)", &.{ .implicit_return, .table_literal, .long_literal, .long_literal, .implicit_apply, .long_literal, .til }, "(+:;(!;,`x`x1`x1;(enlist;(til;10);1;2)))");
    try testParseLanguage(.k, "([]a;a::til 10)", &.{ .implicit_return, .table_literal, .global_assign, .implicit_apply, .long_literal, .identifier, .identifier, .identifier }, "(+:;(!;,`a`x;(enlist;`a;(::;`a;(`til;10)))))");
    try testParseLanguage(.q, "([]a;a::til 10)", &.{ .implicit_return, .table_literal, .global_assign, .implicit_apply, .long_literal, .til, .identifier, .identifier }, "(+:;(!;,`a`x;(enlist;`a;(::;`a;(til;10)))))");

    try testParse("([()]())", &.{ .implicit_return, .table_literal, .empty_list, .empty_list }, "(!;(+:;(!;,,`x;(enlist;())));(+:;(!;,,`x;(enlist;()))))");
    try testParse("([1 2]1 2)", &.{ .implicit_return, .table_literal, .long_list_literal, .long_list_literal }, "(!;(+:;(!;,,`x;(enlist;1 2)));(+:;(!;,,`x;(enlist;1 2))))");
    try testParse("([a:1 2]a:1 2)", &.{ .implicit_return, .table_literal, .long_list_literal, .long_list_literal }, "(!;(+:;(!;,,`a;(enlist;1 2)));(+:;(!;,,`a;(enlist;1 2))))");
    try testParse("([a::1 2]a::1 2)", &.{ .implicit_return, .table_literal, .global_assign, .long_list_literal, .identifier, .global_assign, .long_list_literal, .identifier }, "(!;(+:;(!;,,`x;(enlist;(::;`a;1 2))));(+:;(!;,,`x;(enlist;(::;`a;1 2)))))");
    try testParse("([a:1 2;b:2]a:1 2;b:2)", &.{ .implicit_return, .table_literal, .long_literal, .long_list_literal, .long_literal, .long_list_literal }, "(!;(+:;(!;,`a`b;(enlist;1 2;2)));(+:;(!;,`a`b;(enlist;1 2;2))))");
    try testParse("([a]a)", &.{ .implicit_return, .table_literal, .identifier, .identifier }, "(!;(+:;(!;,,`a;(enlist;`a)));(+:;(!;,,`a;(enlist;`a))))");
    try testParse("([b+sum a]b+sum a)", &.{ .implicit_return, .table_literal, .add, .implicit_apply, .identifier, .sum, .identifier, .add, .implicit_apply, .identifier, .sum, .identifier }, "(!;(+:;(!;,,`a;(enlist;(+;`b;(sum;`a)))));(+:;(!;,,`a;(enlist;(+;`b;(sum;`a))))))");
    try testParse("([sum[a]+b]sum[a]+b)", &.{ .implicit_return, .table_literal, .add, .identifier, .call_one, .identifier, .sum, .add, .identifier, .call_one, .identifier, .sum }, "(!;(+:;(!;,,`b;(enlist;(+;(sum;`a);`b))));(+:;(!;,,`b;(enlist;(+;(sum;`a);`b)))))");
    try testParse("([(a;b;c)](a;b;c))", &.{ .implicit_return, .table_literal, .list, .identifier, .identifier, .identifier, .list, .identifier, .identifier, .identifier }, "(!;(+:;(!;,,`c;(enlist;(enlist;`a;`b;`c))));(+:;(!;,,`c;(enlist;(enlist;`a;`b;`c)))))");
    try testParseLanguage(.k, "([til 10;x1:1;2]til 10;x1:1;2)", &.{ .implicit_return, .table_literal, .long_literal, .long_literal, .implicit_apply, .long_literal, .identifier, .long_literal, .long_literal, .implicit_apply, .long_literal, .identifier }, "(!;(+:;(!;,`x`x1`x1;(enlist;(`til;10);1;2)));(+:;(!;,`x`x1`x1;(enlist;(`til;10);1;2))))");
    try testParseLanguage(.q, "([til 10;x1:1;2]til 10;x1:1;2)", &.{ .implicit_return, .table_literal, .long_literal, .long_literal, .implicit_apply, .long_literal, .til, .long_literal, .long_literal, .implicit_apply, .long_literal, .til }, "(!;(+:;(!;,`x`x1`x1;(enlist;(til;10);1;2)));(+:;(!;,`x`x1`x1;(enlist;(til;10);1;2))))");
    try testParseLanguage(.k, "([a;a::til 10]a;a::til 10)", &.{ .implicit_return, .table_literal, .global_assign, .implicit_apply, .long_literal, .identifier, .identifier, .identifier, .global_assign, .implicit_apply, .long_literal, .identifier, .identifier, .identifier }, "(!;(+:;(!;,`a`x;(enlist;`a;(::;`a;(`til;10)))));(+:;(!;,`a`x;(enlist;`a;(::;`a;(`til;10))))))");
    try testParseLanguage(.q, "([a;a::til 10]a;a::til 10)", &.{ .implicit_return, .table_literal, .global_assign, .implicit_apply, .long_literal, .til, .identifier, .identifier, .global_assign, .implicit_apply, .long_literal, .til, .identifier, .identifier }, "(!;(+:;(!;,`a`x;(enlist;`a;(::;`a;(til;10)))));(+:;(!;,`a`x;(enlist;`a;(::;`a;(til;10))))))");
}

test "select" {
    try testParse("select from x", &.{ .implicit_return, .select, .identifier }, "(?;`x;();0b;())");
    try testParse("select a from x", &.{ .implicit_return, .select, .identifier, .identifier }, "(?;`x;();0b;(,`a)!,`a)");
    try testParse("select a,b from x", &.{ .implicit_return, .select, .identifier, .identifier, .identifier }, "(?;`x;();0b;`a`b!`a`b)");
    try testParse("select by from x", &.{ .implicit_return, .select, .identifier }, "(?;`x;();()!();())");
    try testParse("select a by from x", &.{ .implicit_return, .select, .identifier, .identifier }, "(?;`x;();()!();(,`a)!,`a)");
    try testParse("select a,b by from x", &.{ .implicit_return, .select, .identifier, .identifier, .identifier }, "(?;`x;();()!();`a`b!`a`b)");
    try testParse("select by c from x", &.{ .implicit_return, .select, .identifier, .identifier }, "(?;`x;();(,`c)!,`c;())");
    try testParse("select by c,d from x", &.{ .implicit_return, .select, .identifier, .identifier, .identifier }, "(?;`x;();`c`d!`c`d;())");
    try testParse("select a by c from x", &.{ .implicit_return, .select, .identifier, .identifier, .identifier }, "(?;`x;();(,`c)!,`c;(,`a)!,`a)");
    try testParse("select a,b by c,d from x", &.{ .implicit_return, .select, .identifier, .identifier, .identifier, .identifier, .identifier }, "(?;`x;();`c`d!`c`d;`a`b!`a`b)");
    try testParse("select from x where e", &.{ .implicit_return, .select, .identifier, .identifier }, "(?;`x;,,`e;0b;())");
    try testParse("select from x where e,f", &.{ .implicit_return, .select, .identifier, .identifier, .identifier }, "(?;`x;,(`e;`f);0b;())");
    try testParse("select a from x where e", &.{ .implicit_return, .select, .identifier, .identifier, .identifier }, "(?;`x;,,`e;0b;(,`a)!,`a)");
    try testParse("select a,b from x where e,f", &.{ .implicit_return, .select, .identifier, .identifier, .identifier, .identifier, .identifier }, "(?;`x;,(`e;`f);0b;`a`b!`a`b)");
    try testParse("select by from x where e,f", &.{ .implicit_return, .select, .identifier, .identifier, .identifier }, "(?;`x;,(`e;`f);()!();())");
    try testParse("select a by from x where e", &.{ .implicit_return, .select, .identifier, .identifier, .identifier }, "(?;`x;,,`e;()!();(,`a)!,`a)");
    try testParse("select a,b by from x where e,f", &.{ .implicit_return, .select, .identifier, .identifier, .identifier, .identifier, .identifier }, "(?;`x;,(`e;`f);()!();`a`b!`a`b)");
    try testParse("select by c from x where e", &.{ .implicit_return, .select, .identifier, .identifier, .identifier }, "(?;`x;,,`e;(,`c)!,`c;())");
    try testParse("select by c,d from x where e,f", &.{ .implicit_return, .select, .identifier, .identifier, .identifier, .identifier, .identifier }, "(?;`x;,(`e;`f);`c`d!`c`d;())");
    try testParse("select a by c from x where e", &.{ .implicit_return, .select, .identifier, .identifier, .identifier, .identifier }, "(?;`x;,,`e;(,`c)!,`c;(,`a)!,`a)");
    try testParse("select a,b by c,d from x where e,f", &.{ .implicit_return, .select, .identifier, .identifier, .identifier, .identifier, .identifier, .identifier, .identifier }, "(?;`x;,(`e;`f);`c`d!`c`d;`a`b!`a`b)");

    try testParse("select`a from x", &.{ .implicit_return, .select, .identifier, .symbol_literal }, "(?;`x;();0b;(,`x)!,,`a)");
    try testParse("select`a`b from x", &.{ .implicit_return, .select, .identifier, .symbol_list_literal }, "(?;`x;();0b;(,`x)!,,`a`b)");
    try testParse("select`a,`c from x", &.{ .implicit_return, .select, .identifier, .symbol_literal, .symbol_literal }, "(?;`x;();0b;`x`x1!(,`a;,`c))");
    try testParse("select`a,`c`d from x", &.{ .implicit_return, .select, .identifier, .symbol_literal, .symbol_list_literal }, "(?;`x;();0b;`x`x1!(,`a;,`c`d))");
    try testParse("select`a`b,`c from x", &.{ .implicit_return, .select, .identifier, .symbol_list_literal, .symbol_literal }, "(?;`x;();0b;`x`x1!(,`a`b;,`c))");
    try testParse("select`a`b,`c`d from x", &.{ .implicit_return, .select, .identifier, .symbol_list_literal, .symbol_list_literal }, "(?;`x;();0b;`x`x1!(,`a`b;,`c`d))");
    try testParse("select by`a from x", &.{ .implicit_return, .select, .identifier, .symbol_literal }, "(?;`x;();(,`x)!,,`a;())");
    try testParse("select by`a`b from x", &.{ .implicit_return, .select, .identifier, .symbol_list_literal }, "(?;`x;();(,`x)!,,`a`b;())");
    try testParse("select by`a,`c from x", &.{ .implicit_return, .select, .identifier, .symbol_literal, .symbol_literal }, "(?;`x;();`x`x1!(,`a;,`c);())");
    try testParse("select by`a,`c`d from x", &.{ .implicit_return, .select, .identifier, .symbol_literal, .symbol_list_literal }, "(?;`x;();`x`x1!(,`a;,`c`d);())");
    try testParse("select by`a`b,`c from x", &.{ .implicit_return, .select, .identifier, .symbol_list_literal, .symbol_literal }, "(?;`x;();`x`x1!(,`a`b;,`c);())");
    try testParse("select by`a`b,`c`d from x", &.{ .implicit_return, .select, .identifier, .symbol_list_literal, .symbol_list_literal }, "(?;`x;();`x`x1!(,`a`b;,`c`d);())");

    try testParse("select distinct from x", &.{ .implicit_return, .select, .identifier }, "(?;`x;();1b;())");
    try testParse("select distinct a from x", &.{ .implicit_return, .select, .identifier, .identifier }, "(?;`x;();1b;(,`a)!,`a)");
    try testParse("select distinct by from x", &.{ .implicit_return, .select, .identifier }, "(?;`x;();()!();())");
    try testParse("select distinct a by from x", &.{ .implicit_return, .select, .identifier, .identifier }, "(?;`x;();()!();(,`a)!,`a)");
    try testParse("select distinct by b from x", &.{ .implicit_return, .select, .identifier, .identifier }, "(?;`x;();(,`b)!,`b;())");
    try testParseLanguage(.k, "select distinct a by b from x", &.{ .implicit_return, .select, .identifier, .identifier, .implicit_apply, .identifier, .identifier }, "(?;`x;();(,`b)!,`b;(,`a)!,(`distinct;`a))");
    try testParseLanguage(.q, "select distinct a by b from x", &.{ .implicit_return, .select, .identifier, .identifier, .implicit_apply, .identifier, .distinct }, "(?;`x;();(,`b)!,`b;(,`a)!,(distinct;`a))");

    try testParse("select[1]from x", &.{ .implicit_return, .select, .identifier, .long_literal }, "(?;`x;();0b;();1)");
    try testParse("select[a]from x", &.{ .implicit_return, .select, .identifier, .identifier }, "(?;`x;();0b;();`a)");

    try testParse("select[1;<a]from x", &.{ .implicit_return, .select, .identifier, .long_literal, .identifier }, "(?;`x;();0b;();1;,(<:;`a))");
    try testParse("select[a;<b]from x", &.{ .implicit_return, .select, .identifier, .identifier, .identifier }, "(?;`x;();0b;();`a;,(<:;`b))");

    try testParse("select[<a]from x", &.{ .implicit_return, .select, .identifier, .identifier }, "(?;`x;();0b;();0W;,(<:;`a))");
    try testParse("select[<:a]from x", &.{ .implicit_return, .select, .identifier, .identifier }, "(?;`x;();0b;();0W;,(<:;`a))");
    try testParse("select[<=a]from x", &.{ .implicit_return, .select, .identifier, .identifier }, "(?;`x;();0b;();0W;,(<:;`a))");
    try testParse("select[<>a]from x", &.{ .implicit_return, .select, .identifier, .identifier }, "(?;`x;();0b;();0W;,(<:;`a))");
    try testParse("select[>a]from x", &.{ .implicit_return, .select, .identifier, .identifier }, "(?;`x;();0b;();0W;,(>:;`a))");
    try testParse("select[>:a]from x", &.{ .implicit_return, .select, .identifier, .identifier }, "(?;`x;();0b;();0W;,(>:;`a))");
    try testParse("select[>=a]from x", &.{ .implicit_return, .select, .identifier, .identifier }, "(?;`x;();0b;();0W;,(>:;`a))");
}

test "exec" {
    try testParse("exec from x", &.{ .implicit_return, .exec, .identifier }, "(?;`x;();();())");
    try testParse("exec a from x", &.{ .implicit_return, .exec, .identifier, .identifier }, "(?;`x;();();,`a)");
    try testParse("exec a:a from x", &.{ .implicit_return, .exec, .identifier, .identifier }, "(?;`x;();();(,`a)!,`a)");
    try testParse("exec a,b from x", &.{ .implicit_return, .exec, .identifier, .identifier, .identifier }, "(?;`x;();();`a`b!`a`b)");
    try testParse("exec by c from x", &.{ .implicit_return, .exec, .identifier, .identifier }, "(?;`x;();,`c;())");
    try testParse("exec by c:c from x", &.{ .implicit_return, .exec, .identifier, .identifier }, "(?;`x;();(,`c)!,`c;())");
    try testParse("exec by c,d from x", &.{ .implicit_return, .exec, .identifier, .identifier, .identifier }, "(?;`x;();`c`d!`c`d;())");
    try testParse("exec a by c from x", &.{ .implicit_return, .exec, .identifier, .identifier, .identifier }, "(?;`x;();,`c;,`a)");
    try testParse("exec a:a by c from x", &.{ .implicit_return, .exec, .identifier, .identifier, .identifier }, "(?;`x;();,`c;(,`a)!,`a)");
    try testParse("exec a by c:c from x", &.{ .implicit_return, .exec, .identifier, .identifier, .identifier }, "(?;`x;();(,`c)!,`c;,`a)");
    try testParse("exec a:a by c:c from x", &.{ .implicit_return, .exec, .identifier, .identifier, .identifier }, "(?;`x;();(,`c)!,`c;(,`a)!,`a)");
    try testParse("exec a,b by c,d from x", &.{ .implicit_return, .exec, .identifier, .identifier, .identifier, .identifier, .identifier }, "(?;`x;();`c`d!`c`d;`a`b!`a`b)");
    try testParse("exec from x where e", &.{ .implicit_return, .exec, .identifier, .identifier }, "(?;`x;,,`e;();())");
    try testParse("exec from x where e,f", &.{ .implicit_return, .exec, .identifier, .identifier, .identifier }, "(?;`x;,(`e;`f);();())");
    try testParse("exec a from x where e", &.{ .implicit_return, .exec, .identifier, .identifier, .identifier }, "(?;`x;,,`e;();,`a)");
    try testParse("exec a:a from x where e", &.{ .implicit_return, .exec, .identifier, .identifier, .identifier }, "(?;`x;,,`e;();(,`a)!,`a)");
    try testParse("exec a,b from x where e,f", &.{ .implicit_return, .exec, .identifier, .identifier, .identifier, .identifier, .identifier }, "(?;`x;,(`e;`f);();`a`b!`a`b)");
    try testParse("exec by c from x where e", &.{ .implicit_return, .exec, .identifier, .identifier, .identifier }, "(?;`x;,,`e;,`c;())");
    try testParse("exec by c:c from x where e", &.{ .implicit_return, .exec, .identifier, .identifier, .identifier }, "(?;`x;,,`e;(,`c)!,`c;())");
    try testParse("exec by c,d from x where e,f", &.{ .implicit_return, .exec, .identifier, .identifier, .identifier, .identifier, .identifier }, "(?;`x;,(`e;`f);`c`d!`c`d;())");
    try testParse("exec a by c from x where e", &.{ .implicit_return, .exec, .identifier, .identifier, .identifier, .identifier }, "(?;`x;,,`e;,`c;,`a)");
    try testParse("exec a:a by c from x where e", &.{ .implicit_return, .exec, .identifier, .identifier, .identifier, .identifier }, "(?;`x;,,`e;,`c;(,`a)!,`a)");
    try testParse("exec a by c:c from x where e", &.{ .implicit_return, .exec, .identifier, .identifier, .identifier, .identifier }, "(?;`x;,,`e;(,`c)!,`c;,`a)");
    try testParse("exec a:a by c:c from x where e", &.{ .implicit_return, .exec, .identifier, .identifier, .identifier, .identifier }, "(?;`x;,,`e;(,`c)!,`c;(,`a)!,`a)");
    try testParse("exec a,b by c,d from x where e,f", &.{ .implicit_return, .exec, .identifier, .identifier, .identifier, .identifier, .identifier, .identifier, .identifier }, "(?;`x;,(`e;`f);`c`d!`c`d;`a`b!`a`b)");

    try testParse("exec`a from x", &.{ .implicit_return, .exec, .identifier, .symbol_literal }, "(?;`x;();();,,`a)");
    try testParse("exec`a`b from x", &.{ .implicit_return, .exec, .identifier, .symbol_list_literal }, "(?;`x;();();,,`a`b)");
    try testParse("exec`a,`c from x", &.{ .implicit_return, .exec, .identifier, .symbol_literal, .symbol_literal }, "(?;`x;();();`x`x1!(,`a;,`c))");
    try testParse("exec`a,`c`d from x", &.{ .implicit_return, .exec, .identifier, .symbol_literal, .symbol_list_literal }, "(?;`x;();();`x`x1!(,`a;,`c`d))");
    try testParse("exec`a`b,`c from x", &.{ .implicit_return, .exec, .identifier, .symbol_list_literal, .symbol_literal }, "(?;`x;();();`x`x1!(,`a`b;,`c))");
    try testParse("exec`a`b,`c`d from x", &.{ .implicit_return, .exec, .identifier, .symbol_list_literal, .symbol_list_literal }, "(?;`x;();();`x`x1!(,`a`b;,`c`d))");
    try testParse("exec by`a from x", &.{ .implicit_return, .exec, .identifier, .symbol_literal }, "(?;`x;();,,`a;())");
    try testParse("exec by`a`b from x", &.{ .implicit_return, .exec, .identifier, .symbol_list_literal }, "(?;`x;();,,`a`b;())");
    try testParse("exec by`a,`c from x", &.{ .implicit_return, .exec, .identifier, .symbol_literal, .symbol_literal }, "(?;`x;();`x`x1!(,`a;,`c);())");
    try testParse("exec by`a,`c`d from x", &.{ .implicit_return, .exec, .identifier, .symbol_literal, .symbol_list_literal }, "(?;`x;();`x`x1!(,`a;,`c`d);())");
    try testParse("exec by`a`b,`c from x", &.{ .implicit_return, .exec, .identifier, .symbol_list_literal, .symbol_literal }, "(?;`x;();`x`x1!(,`a`b;,`c);())");
    try testParse("exec by`a`b,`c`d from x", &.{ .implicit_return, .exec, .identifier, .symbol_list_literal, .symbol_list_literal }, "(?;`x;();`x`x1!(,`a`b;,`c`d);())");
}

test "update" {
    try testParse("update a from x", &.{ .implicit_return, .update, .identifier, .identifier }, "(!;`x;();0b;(,`a)!,`a)");
    try testParse("update a,b from x", &.{ .implicit_return, .update, .identifier, .identifier, .identifier }, "(!;`x;();0b;`a`b!`a`b)");
    try testParse("update a by c from x", &.{ .implicit_return, .update, .identifier, .identifier, .identifier }, "(!;`x;();(,`c)!,`c;(,`a)!,`a)");
    try testParse("update a,b by c,d from x", &.{ .implicit_return, .update, .identifier, .identifier, .identifier, .identifier, .identifier }, "(!;`x;();`c`d!`c`d;`a`b!`a`b)");
    try testParse("update a from x where e", &.{ .implicit_return, .update, .identifier, .identifier, .identifier }, "(!;`x;,,`e;0b;(,`a)!,`a)");
    try testParse("update a,b from x where e,f", &.{ .implicit_return, .update, .identifier, .identifier, .identifier, .identifier, .identifier }, "(!;`x;,(`e;`f);0b;`a`b!`a`b)");
    try testParse("update a by c from x where e", &.{ .implicit_return, .update, .identifier, .identifier, .identifier, .identifier }, "(!;`x;,,`e;(,`c)!,`c;(,`a)!,`a)");
    try testParse("update a,b by c,d from x where e,f", &.{ .implicit_return, .update, .identifier, .identifier, .identifier, .identifier, .identifier, .identifier, .identifier }, "(!;`x;,(`e;`f);`c`d!`c`d;`a`b!`a`b)");

    try testParse("update`a from x", &.{ .implicit_return, .update, .identifier, .symbol_literal }, "(!;`x;();0b;(,`x)!,,`a)");
    try testParse("update`a`b from x", &.{ .implicit_return, .update, .identifier, .symbol_list_literal }, "(!;`x;();0b;(,`x)!,,`a`b)");
    try testParse("update`a,`c from x", &.{ .implicit_return, .update, .identifier, .symbol_literal, .symbol_literal }, "(!;`x;();0b;`x`x1!(,`a;,`c))");
    try testParse("update`a,`c`d from x", &.{ .implicit_return, .update, .identifier, .symbol_literal, .symbol_list_literal }, "(!;`x;();0b;`x`x1!(,`a;,`c`d))");
    try testParse("update`a`b,`c from x", &.{ .implicit_return, .update, .identifier, .symbol_list_literal, .symbol_literal }, "(!;`x;();0b;`x`x1!(,`a`b;,`c))");
    try testParse("update`a`b,`c`d from x", &.{ .implicit_return, .update, .identifier, .symbol_list_literal, .symbol_list_literal }, "(!;`x;();0b;`x`x1!(,`a`b;,`c`d))");
}

test "delete rows" {
    try testParse("delete from x", &.{ .implicit_return, .delete_rows, .identifier }, "(!;`x;();0b;`symbol$())");
    try testParse("delete from x where a", &.{ .implicit_return, .delete_rows, .identifier, .identifier }, "(!;`x;,,`a;0b;`symbol$())");
    try testParse("delete from x where a,b", &.{ .implicit_return, .delete_rows, .identifier, .identifier, .identifier }, "(!;`x;,(`a;`b);0b;`symbol$())");
}

test "delete columns" {
    try testParse("delete a from x", &.{ .implicit_return, .delete_cols, .identifier, .identifier }, "(!;`x;();0b;,,`a)");
    try testParse("delete a,b from x", &.{ .implicit_return, .delete_cols, .identifier, .identifier, .identifier }, "(!;`x;();0b;,`a`b)");
}

test "iterators" {
    try testParse("(\\:)", &.{ .implicit_return, .grouped_expression, .backslash_colon }, "\\:");
    try testParse("@\\:", &.{ .implicit_return, .backslash_colon, .at }, "(\\:;@)");
    try testParse("f\\:", &.{ .implicit_return, .backslash_colon, .identifier }, "(\\:;`f)");
    try testParse("@\\:[x;y]", &.{ .implicit_return, .call, .identifier, .identifier, .backslash_colon, .at }, "((\\:;@);`x;`y)");
    try testParse("f\\:[x;y]", &.{ .implicit_return, .call, .identifier, .identifier, .backslash_colon, .identifier }, "((\\:;`f);`x;`y)");
    try testParse("x@\\:y", &.{ .implicit_return, .backslash_colon_infix, .identifier, .apply, .identifier }, "((\\:;@);`x;`y)");
    try testParse("x f\\:y", &.{ .implicit_return, .backslash_colon_infix, .identifier, .identifier, .identifier }, "((\\:;`f);`x;`y)");
}

test "os" {
    try testParse("\\ls", &.{ .implicit_return, .os }, "(.,[\"\\\\\"];\"ls\")");
    try testParse("\\ls arg", &.{ .implicit_return, .os }, "(.,[\"\\\\\"];\"ls arg\")");
    try testParse("\\ls arg1 arg2", &.{ .implicit_return, .os }, "(.,[\"\\\\\"];\"ls arg1 arg2\")");
}

test "load_file_or_directory" {
    try testParse("\\l file", &.{ .implicit_return, .load_file_or_directory }, "(.,[\"\\\\\"];\"l file\")");
    try testParse("\\l file extra", &.{ .implicit_return, .load_file_or_directory }, "(.,[\"\\\\\"];\"l file extra\")");
    try testParseError("\\l", &.{.expected_token});
    try testParseError(
        \\\l file
        \\ l file
    , &.{.os_expects_all_tokens_on_same_line});
}

test "current_directory" {
    try testParse("\\cd", &.{ .implicit_return, .current_directory }, "(.,[\"\\\\\"];\"cd\")");
}

test "change_directory" {
    try testParse("\\cd path", &.{ .implicit_return, .change_directory }, "(.,[\"\\\\\"];\"cd path\")");
    try testParseError(
        \\\cd path
        \\ cd path
    , &.{.os_expects_all_tokens_on_same_line});
}

test {
    @import("std").testing.refAllDecls(@This());
}
