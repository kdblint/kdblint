//! Represents in-progress parsing, will be converted to an Ast after completion.

pub const Error = error{ParseError} || Allocator.Error;

gpa: Allocator,
mode: Ast.Mode,
source: []const u8,
tokens: Ast.TokenList.Slice,
tok_i: TokenIndex,
eob: bool,
ends_expr: std.ArrayListUnmanaged(Token.Tag),
errors: std.ArrayListUnmanaged(AstError),
nodes: Ast.NodeList,
extra_data: std.ArrayListUnmanaged(u32),
scratch: std.ArrayListUnmanaged(Node.Index),

fn tokenTag(p: *const Parse, token_index: TokenIndex) Token.Tag {
    return p.tokens.items(.tag)[token_index];
}

fn tokenStart(p: *const Parse, token_index: TokenIndex) Ast.ByteOffset {
    return p.tokens.items(.start)[token_index];
}

fn nodeTag(p: *const Parse, node: Node.Index) Node.Tag {
    return p.nodes.items(.tag)[@intFromEnum(node)];
}

fn nodeMainToken(p: *const Parse, node: Node.Index) TokenIndex {
    return p.nodes.items(.main_token)[@intFromEnum(node)];
}

fn nodeData(p: *const Parse, node: Node.Index) Node.Data {
    return p.nodes.items(.data)[@intFromEnum(node)];
}

const Blocks = struct {
    len: usize,
    /// Must be either `.opt_node_and_opt_node` if `len <= 2` or `.extra_range` otherwise.
    data: Node.Data,

    fn toSpan(self: Blocks, p: *Parse) !Node.SubRange {
        return switch (self.len) {
            0 => p.listToSpan(&.{}),
            1 => p.listToSpan(&.{self.data.opt_node_and_opt_node[0].unwrap().?}),
            2 => p.listToSpan(&.{ self.data.opt_node_and_opt_node[0].unwrap().?, self.data.opt_node_and_opt_node[1].unwrap().? }),
            else => self.data.extra_range,
        };
    }
};

fn listToSpan(p: *Parse, list: []const Node.Index) Allocator.Error!Node.SubRange {
    try p.extra_data.appendSlice(p.gpa, @ptrCast(list));
    return Node.SubRange{
        .start = @enumFromInt(p.extra_data.items.len - list.len),
        .end = @enumFromInt(p.extra_data.items.len),
    };
}

fn addNode(p: *Parse, elem: Ast.Node) Allocator.Error!Node.Index {
    const result: Node.Index = @enumFromInt(p.nodes.len);
    try p.nodes.append(p.gpa, elem);
    return result;
}

fn setNode(p: *Parse, i: usize, elem: Ast.Node) Node.Index {
    p.nodes.set(i, elem);
    return @enumFromInt(i);
}

fn reserveNode(p: *Parse, tag: Ast.Node.Tag) !usize {
    try p.nodes.resize(p.gpa, p.nodes.len + 1);
    p.nodes.items(.tag)[p.nodes.len - 1] = tag;
    return p.nodes.len - 1;
}

fn unreserveNode(p: *Parse, node_index: usize) void {
    if (p.nodes.len == node_index) {
        p.nodes.resize(p.gpa, p.nodes.len - 1) catch unreachable;
    } else {
        // There is zombie node left in the tree, let's make it as inoffensive as possible
        // (sadly there's no no-op node)
        p.nodes.items(.tag)[node_index] = .root; // TODO: It's probably a bad idea to use root here...
        p.nodes.items(.main_token)[node_index] = p.tok_i;
    }
}

fn addExtra(p: *Parse, extra: anytype) Allocator.Error!ExtraIndex {
    const fields = std.meta.fields(@TypeOf(extra));
    try p.extra_data.ensureUnusedCapacity(p.gpa, fields.len);
    const result: ExtraIndex = @enumFromInt(p.extra_data.items.len);
    inline for (fields) |field| {
        const data: u32 = switch (field.type) {
            Node.Index,
            Node.OptionalIndex,
            OptionalTokenIndex,
            ExtraIndex,
            => @intFromEnum(@field(extra, field.name)),
            TokenIndex,
            => @field(extra, field.name),
            else => |t| @compileError("bad field type: " ++ @typeName(t)),
        };
        p.extra_data.appendAssumeCapacity(data);
    }
    return result;
}

fn warnExpected(p: *Parse, expected_token: Token.Tag) !void {
    @branchHint(.cold);
    try p.warnMsg(.{
        .tag = .expected_token,
        .token = p.tok_i,
        .extra = .{ .expected_tag = expected_token },
    });
}

fn warn(p: *Parse, error_tag: Ast.Error.Tag) !void {
    @branchHint(.cold);
    try p.warnMsg(.{ .tag = error_tag, .token = p.tok_i });
}

fn warnMsg(p: *Parse, msg: Ast.Error) !void {
    @branchHint(.cold);
    try p.errors.append(p.gpa, msg);
}

fn fail(p: *Parse, tag: Ast.Error.Tag) error{ ParseError, OutOfMemory } {
    @branchHint(.cold);
    return p.failMsg(.{ .tag = tag, .token = p.tok_i });
}

fn failExpected(p: *Parse, expected_token: Token.Tag) error{ ParseError, OutOfMemory } {
    @branchHint(.cold);
    return p.failMsg(.{
        .tag = .expected_token,
        .token = p.tok_i,
        .extra = .{ .expected_tag = expected_token },
    });
}

fn failMsg(p: *Parse, msg: Ast.Error) error{ ParseError, OutOfMemory } {
    @branchHint(.cold);
    try p.warnMsg(msg);
    return error.ParseError;
}

fn tokenSlice(p: *Parse, token_index: TokenIndex) []const u8 {
    const loc: Token.Loc = p.tokens.items(.loc)[token_index];
    return p.source[loc.start..loc.end];
}

// TODO: Add tests
fn validateUnaryApplication(p: *Parse, lhs: Node.Index, rhs: Node.Index) !void {
    assert(lhs != .root);
    assert(rhs != .root);

    const tag = p.nodeTag(lhs);
    switch (tag.getType()) {
        // Fail if we are applying a unary operator directly in q.
        .unary_operator => if (p.mode == .q and
            (tag != .colon or (p.nodeMainToken(lhs) > 0 and switch (p.tokenTag(p.nodeMainToken(lhs) - 1)) {
                .l_paren,
                .l_brace,
                .l_bracket,
                .r_bracket,
                .semicolon,
                => false,
                else => true,
            })))
        {
            return p.warnMsg(.{
                .tag = .cannot_apply_operator_directly,
                .token = p.nodeMainToken(lhs),
            });
        },

        // Fail if we are applying an iterator directly in q.
        .iterator => if (p.mode == .q and (tag != .apostrophe or p.nodeData(lhs).opt_node != .none)) {
            return p.warnMsg(.{
                .tag = .cannot_apply_iterator_directly,
                .token = p.nodeMainToken(lhs),
            });
        },

        .other => {},
    }
}

/// Root <- Blocks EOF
pub fn parseRoot(p: *Parse) !void {
    // Root node must be index 0.
    p.nodes.appendAssumeCapacity(.{
        .tag = .root,
        .main_token = 0,
        .data = undefined,
    });
    const blocks = try p.parseBlocks();
    const root_decls = try blocks.toSpan(p);
    if (p.tokenTag(p.tok_i) != .eof) {
        try p.warnExpected(.eof);
    }
    p.nodes.items(.data)[0] = .{ .extra_range = root_decls };
}

/// Blocks <- Block*
fn parseBlocks(p: *Parse) !Blocks {
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    while (p.peekTag() != .eof) {
        const block = try p.parseBlock();
        if (block.unwrap()) |block_node| try p.scratch.append(p.gpa, block_node);
        if (!p.eob) {
            try p.warn(.expected_expr);
            p.skipBlock();
        }
        assert(p.eob);
        p.eob = false;
    }

    const items = p.scratch.items[scratch_top..];
    if (items.len <= 2) {
        return .{
            .len = items.len,
            .data = .{ .opt_node_and_opt_node = .{
                if (items.len >= 1) items[0].toOptional() else .none,
                if (items.len >= 2) items[1].toOptional() else .none,
            } },
        };
    } else {
        return .{
            .len = items.len,
            .data = .{ .extra_range = try p.listToSpan(items) },
        };
    }
}

/// Block <- Expr SEMICOLON?
fn parseBlock(p: *Parse) !Node.OptionalIndex {
    const expr = p.parseExpr(null) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => blk: {
            p.skipBlock();
            break :blk .none;
        },
    };

    // TODO: Use trailing.
    _ = p.eatToken(.semicolon);

    return expr;
}

/// Expr <- Noun Verb*
fn parseExpr(p: *Parse, comptime sql_identifier: ?SqlIdentifier) Error!Node.OptionalIndex {
    if (sql_identifier) |sql_id| if (p.peekIdentifier(sql_id)) |_| return .none;
    var node = (try p.parseNoun()).unwrap() orelse return .none;

    while (true) {
        if (sql_identifier) |sql_id| {
            if (p.peekIdentifier(sql_id)) |_| break;
            if (p.peekTag() == .comma) break;
        }
        switch (p.peekTag()) {
            .r_paren, .r_brace, .r_bracket, .semicolon, .eob, .eof => break,
            else => {},
        }
        node = try p.parseVerb(node, sql_identifier);
    }

    return node.toOptional();
}

fn expectExpr(p: *Parse, comptime sql_identifier: ?SqlIdentifier) !Node.Index {
    const expr = try p.parseExpr(sql_identifier);
    return expr.unwrap() orelse p.fail(.expected_expr);
}

/// Noun
///     <- (Group
///      / Lambda
///      / ExprBlock
///      / Operator
///      / NumberLiteral
///      / STRING_LITERAL
///      / SymbolLiteral
///      / IDENTIFIER) Iterator*
fn parseNoun(p: *Parse) !Node.OptionalIndex {
    const noun: Node.Index = switch (p.peekTag()) {
        .l_paren,
        => try p.parseGroup(),

        .l_brace,
        => try p.parseLambda(),

        .l_bracket,
        => try p.parseExprBlock(),

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
        => try p.parseOperator(),

        .apostrophe,
        .apostrophe_colon,
        .slash,
        .slash_colon,
        .backslash,
        .backslash_colon,
        => try p.parseIterator(.none),

        .number_literal,
        => try p.parseNumberLiteral(),

        .string_literal,
        => try p.parseToken(.string_literal, .string_literal),

        .symbol_literal,
        => try p.parseSymbolLiteral(),

        .identifier,
        => try p.parseToken(.identifier, .identifier),

        .prefix_builtin,
        => try p.parseToken(.prefix_builtin, .builtin),

        .infix_builtin,
        => try p.parseToken(.infix_builtin, .builtin),

        .keyword_select,
        => try p.parseSelect(),

        .keyword_exec,
        => try p.parseExec(),

        .keyword_update,
        => try p.parseUpdate(),

        .keyword_delete,
        => try p.parseDelete(),

        else => return .none,
    };
    return .fromIndex(try p.parseCall(noun));
}

fn expectNoun(p: *Parse) !Node.Index {
    const expr = try p.parseNoun();
    return expr.unwrap() orelse p.fail(.expected_expr);
}

/// Verb <- Expr*
fn parseVerb(p: *Parse, lhs: Node.Index, comptime sql_identifier: ?SqlIdentifier) !Node.Index {
    if (sql_identifier) |sql_id| if (p.peekIdentifier(sql_id)) |_| return lhs;

    const tag = p.peekTag();
    if (p.ends_expr.getLastOrNull()) |ends_expr| if (tag == ends_expr) return lhs;

    switch (tag) {
        .l_paren,
        .l_brace,
        .number_literal,
        .string_literal,
        .symbol_literal,
        .identifier,
        .prefix_builtin,
        .keyword_select,
        .keyword_exec,
        .keyword_update,
        .keyword_delete,
        => {
            const apply_index = try p.reserveNode(.apply_unary);
            errdefer p.unreserveNode(apply_index);

            const op = try p.expectNoun();

            const node_tags: []Node.Tag = p.nodes.items(.tag);
            if (node_tags[@intFromEnum(op)].getType() == .iterator) {
                const rhs = try p.parseExpr(sql_identifier);
                return p.setNode(apply_index, .{
                    .tag = .apply_binary,
                    .main_token = @intFromEnum(op),
                    .data = .{ .node_and_opt_node = .{
                        lhs,
                        rhs,
                    } },
                });
            } else {
                const rhs = try p.parseVerb(op, sql_identifier);
                try p.validateUnaryApplication(lhs, rhs);
                return p.setNode(apply_index, .{
                    .tag = .apply_unary,
                    .main_token = undefined,
                    .data = .{ .node_and_node = .{
                        lhs,
                        rhs,
                    } },
                });
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
        .infix_builtin,
        => {
            const apply_index = try p.reserveNode(.apply_binary);
            errdefer p.unreserveNode(apply_index);

            const op = try p.expectNoun();

            const node_tags: []Node.Tag = p.nodes.items(.tag);
            if (node_tags[@intFromEnum(op)] == .call) {
                const rhs = try p.parseVerb(op, sql_identifier);
                try p.validateUnaryApplication(lhs, rhs);
                return p.setNode(apply_index, .{
                    .tag = .apply_unary,
                    .main_token = undefined,
                    .data = .{ .node_and_node = .{
                        lhs,
                        rhs,
                    } },
                });
            } else {
                const rhs = try p.parseExpr(sql_identifier);
                return p.setNode(apply_index, .{
                    .tag = .apply_binary,
                    .main_token = @intFromEnum(op),
                    .data = .{ .node_and_opt_node = .{
                        lhs,
                        rhs,
                    } },
                });
            }
        },

        .semicolon,
        .eob,
        .eof,
        => return lhs,

        else => return p.fail(.expected_expr),
    }
}

fn parseToken(p: *Parse, token_tag: Token.Tag, node_tag: Node.Tag) !Node.Index {
    const main_token = try p.expectToken(token_tag);
    return p.addNode(.{
        .tag = node_tag,
        .main_token = main_token,
        .data = undefined,
    });
}

fn parseEmpty(p: *Parse) !Node.Index {
    const token_tags: []Token.Tag = p.tokens.items(.tag);
    switch (token_tags[p.tok_i]) {
        .semicolon, .r_paren, .r_brace, .r_bracket => return p.addNode(.{
            .tag = .empty,
            .main_token = p.tok_i,
            .data = undefined,
        }),
        else => return p.failExpected(p.ends_expr.getLast()),
    }
}

/// Group
///     <- Table
///      | LPAREN Expr* RPAREN
fn parseGroup(p: *Parse) !Node.Index {
    const l_paren = p.assertToken(.l_paren);
    if (p.eatToken(.r_paren)) |r_paren| {
        return p.addNode(.{
            .tag = .empty_list,
            .main_token = l_paren,
            .data = .{ .token = r_paren },
        });
    }

    const table = try p.parseTable(l_paren);
    if (table.unwrap()) |n| return n;

    const list_index = try p.reserveNode(.list);
    errdefer p.unreserveNode(list_index);

    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    try p.ends_expr.append(p.gpa, .r_paren);

    while (true) {
        const expr = try p.parseExpr(null);
        try p.scratch.append(p.gpa, expr.unwrap() orelse try p.parseEmpty());
        _ = p.eatToken(.semicolon) orelse break;
    }
    const r_paren = try p.expectToken(.r_paren);
    assert(p.ends_expr.pop() == .r_paren);

    const list = p.scratch.items[scratch_top..];
    switch (list.len) {
        0 => unreachable,
        1 => return p.setNode(list_index, .{
            .tag = .grouped_expression,
            .main_token = l_paren,
            .data = .{ .node_and_token = .{
                list[0],
                r_paren,
            } },
        }),
        else => return p.setNode(list_index, .{
            .tag = .list,
            .main_token = l_paren,
            .data = .{ .extra_range = try p.listToSpan(list) },
        }),
    }
}

/// Table <- LPAREN LBRACKET (Expr (SEMICOLON Expr)*)* RBRACKET Expr (SEMICOLON Expr)* RPAREN
fn parseTable(p: *Parse, l_paren: TokenIndex) !Node.OptionalIndex {
    _ = p.eatToken(.l_bracket) orelse return .none;

    try p.ends_expr.append(p.gpa, .r_paren);

    const table_index = try p.reserveNode(.table_literal);
    errdefer p.unreserveNode(table_index);

    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    const keys_top = p.scratch.items.len;
    try p.ends_expr.append(p.gpa, .r_bracket);

    if (p.peekTag() != .r_bracket) {
        while (true) {
            const expr = try p.expectExpr(null);
            try p.scratch.append(p.gpa, expr);
            _ = p.eatToken(.semicolon) orelse break;
        }
    }
    _ = try p.expectToken(.r_bracket);
    assert(p.ends_expr.pop() == .r_bracket);

    const columns_top = p.scratch.items.len;

    while (true) {
        const expr = try p.expectExpr(null);
        try p.scratch.append(p.gpa, expr);
        _ = p.eatToken(.semicolon) orelse break;
    }
    const r_paren = try p.expectToken(.r_paren);
    assert(p.ends_expr.pop() == .r_paren);

    const keys = try p.listToSpan(p.scratch.items[keys_top..columns_top]);
    const columns = try p.listToSpan(p.scratch.items[columns_top..]);
    const table: Node.Table = .{
        .keys_start = keys.start,
        .keys_end = keys.end,
        .columns_start = columns.start,
        .columns_end = columns.end,
    };
    return p.setNode(table_index, .{
        .tag = .table_literal,
        .main_token = l_paren,
        .data = .{ .extra_and_token = .{
            try p.addExtra(table),
            r_paren,
        } },
    }).toOptional();
}

/// Lambda <- LBRACE ParamList? (Expr (SEMICOLON Expr)*)? RBRACE
///
/// ParamList <- LBRACKET (IDENTIFIER (SEMICOLON IDENTIFIER)*)? RBRACKET
fn parseLambda(p: *Parse) !Node.Index {
    const l_brace = p.assertToken(.l_brace);

    try p.ends_expr.append(p.gpa, .r_brace);

    const lambda_index = try p.reserveNode(.lambda);
    errdefer p.unreserveNode(lambda_index);

    const params_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(params_top);

    if (p.eatToken(.l_bracket)) |_| {
        try p.ends_expr.append(p.gpa, .r_bracket);

        if (p.peekTag() == .r_bracket) {
            try p.scratch.append(p.gpa, try p.parseEmpty());
        } else {
            while (true) {
                const node = try p.parseToken(.identifier, .identifier);
                try p.scratch.append(p.gpa, node);
                _ = p.eatToken(.semicolon) orelse break;
            }
        }

        _ = try p.expectToken(.r_bracket);
        assert(p.ends_expr.pop() == .r_bracket);
    }

    const body_top = p.scratch.items.len;
    while (true) {
        const expr = try p.parseExpr(null);
        try p.scratch.append(p.gpa, expr.unwrap() orelse try p.parseEmpty());
        _ = p.eatToken(.semicolon) orelse break;
        if (p.peekTag() == .r_brace) {
            try p.scratch.append(p.gpa, try p.parseEmpty());
            break;
        }
    }

    const r_brace = try p.expectToken(.r_brace);
    assert(p.ends_expr.pop() == .r_brace);

    const params = try p.listToSpan(p.scratch.items[params_top..body_top]);
    const body = try p.listToSpan(p.scratch.items[body_top..]);
    const lambda: Node.Lambda = .{
        .params_start = params.start,
        .params_end = params.end,
        .body_start = body.start,
        .body_end = body.end,
    };
    return p.setNode(lambda_index, .{
        .tag = .lambda,
        .main_token = l_brace,
        .data = .{ .extra_and_token = .{
            try p.addExtra(lambda),
            r_brace,
        } },
    });
}

/// ExprBlock <- LBRACKET Exprs? RBRACKET
fn parseExprBlock(p: *Parse) !Node.Index {
    const l_bracket = p.assertToken(.l_bracket);
    if (p.eatToken(.r_bracket)) |_| {
        // TODO: Should we just do the rest of this function and have a single empty node for `[]`?
        return p.addNode(.{
            .tag = .expr_block,
            .main_token = l_bracket,
            .data = .{ .extra_range = .{
                .start = @enumFromInt(0),
                .end = @enumFromInt(0),
            } },
        });
    }

    try p.ends_expr.append(p.gpa, .r_bracket);

    const expr_block_index = try p.reserveNode(.expr_block);
    errdefer p.unreserveNode(expr_block_index);

    const exprs: Node.SubRange = exprs: {
        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);

        while (true) {
            const expr = try p.parseExpr(null);
            try p.scratch.append(p.gpa, expr.unwrap() orelse try p.parseEmpty());
            _ = p.eatToken(.semicolon) orelse break;
        }

        break :exprs try p.listToSpan(p.scratch.items[scratch_top..]);
    };
    _ = try p.expectToken(.r_bracket);
    assert(p.ends_expr.pop() == .r_bracket);

    return p.setNode(expr_block_index, .{
        .tag = .expr_block,
        .main_token = l_bracket,
        .data = .{ .extra_range = exprs },
    });
}

/// Call <- (LBRACKET Expr (SEMICOLON Expr)* RBRACKET)*
pub fn parseCall(p: *Parse, lhs: Node.Index) !Node.Index {
    if (p.peekTag() != .l_bracket) return p.parseIterator(lhs.toOptional());

    const l_bracket = p.assertToken(.l_bracket);

    try p.ends_expr.append(p.gpa, .r_bracket);

    const node_index = try p.reserveNode(.call);
    errdefer p.unreserveNode(node_index);

    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    try p.scratch.append(p.gpa, lhs);

    while (true) {
        const expr = try p.parseExpr(null);
        try p.scratch.append(p.gpa, expr.unwrap() orelse try p.parseEmpty());
        _ = p.eatToken(.semicolon) orelse break;
    }

    _ = try p.expectToken(.r_bracket);
    assert(p.ends_expr.pop() == .r_bracket);

    const args = p.scratch.items[scratch_top..];

    return p.parseCall(p.setNode(node_index, .{
        .tag = .call,
        .main_token = l_bracket,
        .data = .{ .extra_range = try p.listToSpan(args) },
    }));
}

fn parseSelect(p: *Parse) !Node.Index {
    const select_token = p.assertToken(.keyword_select);

    const select_index = try p.reserveNode(.select);
    errdefer p.unreserveNode(select_index);

    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    // Limit expression
    var distinct: OptionalTokenIndex = .none;
    var ascending = false;
    var limit_expr: Node.OptionalIndex = .none;
    var order_tok: OptionalTokenIndex = .none;
    if (p.eatBuiltin("distinct")) |distinct_token| {
        distinct = .fromToken(distinct_token);
    } else if (p.eatToken(.l_bracket)) |_| {
        try p.ends_expr.append(p.gpa, .r_bracket);

        // TODO: Warn only first character is checked for ascending condition.
        // TODO: Should this be switched to consume expressions instead of tokens?
        switch (p.peekTag()) {
            .angle_bracket_left,
            .angle_bracket_left_colon,
            .angle_bracket_left_equal,
            .angle_bracket_left_right,
            => {
                _ = p.nextToken();
                ascending = true;
                order_tok = .fromToken(try p.expectToken(.identifier));
            },

            .angle_bracket_right,
            .angle_bracket_right_colon,
            .angle_bracket_right_equal,
            => {
                _ = p.nextToken();
                order_tok = .fromToken(try p.expectToken(.identifier));
            },

            else => {
                limit_expr = .fromIndex(try p.expectExpr(null));
                if (p.eatToken(.semicolon)) |_| {
                    switch (p.peekTag()) {
                        .angle_bracket_left,
                        .angle_bracket_left_colon,
                        .angle_bracket_left_equal,
                        .angle_bracket_left_right,
                        => {
                            _ = p.nextToken();
                            ascending = true;
                            order_tok = .fromToken(try p.expectToken(.identifier));
                        },

                        .angle_bracket_right,
                        .angle_bracket_right_colon,
                        .angle_bracket_right_equal,
                        => {
                            _ = p.nextToken();
                            order_tok = .fromToken(try p.expectToken(.identifier));
                        },

                        else => {},
                    }
                }
            },
        }
        _ = try p.expectToken(.r_bracket);
        assert(p.ends_expr.pop() == .r_bracket);

        if (p.peekBuiltin("distinct")) |_| {
            try p.warn(.cannot_combine_limit_expression_and_distinct);
        }
    }

    // Select phrase
    const select_top = p.scratch.items.len;
    if (p.peekIdentifier(.{ .by = true, .from = true }) == null) {
        while (true) {
            const expr = try p.expectExpr(.{ .by = true, .from = true });
            try p.scratch.append(p.gpa, expr);
            _ = p.eatToken(.comma) orelse break;
        }
    }

    // By phrase
    const by_top = p.scratch.items.len;
    const by_token: OptionalTokenIndex = .fromOptional(p.eatIdentifier(.{ .by = true }));
    if (by_token != .none) {
        const maybe_first_expr = try p.parseExpr(.{ .from = true });
        if (maybe_first_expr.unwrap()) |first_expr| {
            try p.scratch.append(p.gpa, first_expr);
            while (p.eatToken(.comma)) |_| {
                const expr = try p.expectExpr(.{ .from = true });
                try p.scratch.append(p.gpa, expr);
            }
        }
    }

    // From phrase
    _ = try p.expectIdentifier(.{ .from = true });
    const from_expr = try p.expectExpr(.{ .where = true });

    // Where phrase
    const where_top = p.scratch.items.len;
    if (p.eatBuiltin("where")) |_| {
        while (true) {
            const expr = try p.expectExpr(.{});
            try p.scratch.append(p.gpa, expr);
            _ = p.eatToken(.comma) orelse break;
        }
    }

    const select = try p.listToSpan(p.scratch.items[select_top..by_top]);
    const by = try p.listToSpan(p.scratch.items[by_top..where_top]);
    const where = try p.listToSpan(p.scratch.items[where_top..]);
    const select_node: Node.Select = .{
        .limit = limit_expr,
        .order = order_tok,
        .distinct = distinct,
        .select_start = select.start,
        .by_token = by_token,
        .by_start = by.start,
        .from = from_expr,
        .where_start = where.start,
        .where_end = where.end,
    };
    return p.setNode(select_index, .{
        .tag = .select,
        .main_token = select_token,
        .data = .{ .extra = try p.addExtra(select_node) },
    });
}

fn parseExec(p: *Parse) !Node.Index {
    const exec_token = p.assertToken(.keyword_exec);

    const exec_index = try p.reserveNode(.exec);
    errdefer p.unreserveNode(exec_index);

    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    // Select phrase
    const select_top = p.scratch.items.len;
    if (p.peekIdentifier(.{ .by = true, .from = true }) == null) {
        while (true) {
            const expr = try p.expectExpr(.{ .by = true, .from = true });
            try p.scratch.append(p.gpa, expr);
            _ = p.eatToken(.comma) orelse break;
        }
    }

    // By phrase
    const by_top = p.scratch.items.len;
    if (p.eatIdentifier(.{ .by = true })) |_| {
        while (true) {
            const expr = try p.expectExpr(.{ .from = true });
            try p.scratch.append(p.gpa, expr);
            _ = p.eatToken(.comma) orelse break;
        }
    }

    // From phrase
    _ = try p.expectIdentifier(.{ .from = true });
    const from_expr = try p.expectExpr(.{ .where = true });

    // Where phrase
    const where_top = p.scratch.items.len;
    if (p.eatBuiltin("where")) |_| {
        while (true) {
            const expr = try p.expectExpr(.{});
            try p.scratch.append(p.gpa, expr);
            _ = p.eatToken(.comma) orelse break;
        }
    }

    const select = try p.listToSpan(p.scratch.items[select_top..by_top]);
    const by = try p.listToSpan(p.scratch.items[by_top..where_top]);
    const where = try p.listToSpan(p.scratch.items[where_top..]);
    const exec_node: Node.Exec = .{
        .select_start = select.start,
        .by_start = by.start,
        .from = from_expr,
        .where_start = where.start,
        .where_end = where.end,
    };
    return p.setNode(exec_index, .{
        .tag = .exec,
        .main_token = exec_token,
        .data = .{ .extra = try p.addExtra(exec_node) },
    });
}

fn parseUpdate(p: *Parse) !Node.Index {
    const update_token = p.assertToken(.keyword_update);

    const update_index = try p.reserveNode(.update);
    errdefer p.unreserveNode(update_index);

    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    // Select phrase
    const select_top = p.scratch.items.len;
    if (p.peekIdentifier(.{ .by = true, .from = true }) == null) {
        while (true) {
            const expr = try p.expectExpr(.{ .by = true, .from = true });
            try p.scratch.append(p.gpa, expr);
            _ = p.eatToken(.comma) orelse break;
        }
    }

    // By phrase
    const by_top = p.scratch.items.len;
    if (p.eatIdentifier(.{ .by = true })) |_| {
        while (true) {
            const expr = try p.expectExpr(.{ .from = true });
            try p.scratch.append(p.gpa, expr);
            _ = p.eatToken(.comma) orelse break;
        }
    }

    // From phrase
    _ = try p.expectIdentifier(.{ .from = true });
    const from_expr = try p.expectExpr(.{ .where = true });

    // Where phrase
    const where_top = p.scratch.items.len;
    if (p.eatBuiltin("where")) |_| {
        while (true) {
            const expr = try p.expectExpr(.{});
            try p.scratch.append(p.gpa, expr);
            _ = p.eatToken(.comma) orelse break;
        }
    }

    const select = try p.listToSpan(p.scratch.items[select_top..by_top]);
    const by = try p.listToSpan(p.scratch.items[by_top..where_top]);
    const where = try p.listToSpan(p.scratch.items[where_top..]);
    const update_node: Node.Update = .{
        .select_start = select.start,
        .by_start = by.start,
        .from = from_expr,
        .where_start = where.start,
        .where_end = where.end,
    };
    return p.setNode(update_index, .{
        .tag = .update,
        .main_token = update_token,
        .data = .{ .extra = try p.addExtra(update_node) },
    });
}

fn parseDelete(p: *Parse) !Node.Index {
    const delete_token = p.assertToken(.keyword_delete);

    const delete_index = try p.reserveNode(.delete_rows);
    errdefer p.unreserveNode(delete_index);

    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    if (p.eatIdentifier(.{ .from = true })) |_| {
        // From phrase
        const from_expr = try p.expectExpr(.{ .where = true });

        // Where phrase
        const where_top = p.scratch.items.len;
        if (p.eatBuiltin("where")) |_| {
            while (true) {
                const expr = try p.expectExpr(.{});
                try p.scratch.append(p.gpa, expr);
                _ = p.eatToken(.comma) orelse break;
            }
        }

        const where = try p.listToSpan(p.scratch.items[where_top..]);
        const delete_node: Node.DeleteRows = .{
            .from = from_expr,
            .where_start = where.start,
            .where_end = where.end,
        };
        return p.setNode(delete_index, .{
            .tag = .delete_rows,
            .main_token = delete_token,
            .data = .{ .extra = try p.addExtra(delete_node) },
        });
    }

    // Select phrase
    const select_top = p.scratch.items.len;
    while (true) {
        const identifier = try p.expectToken(.identifier);
        try p.scratch.append(p.gpa, @enumFromInt(identifier));
        _ = p.eatToken(.comma) orelse break;
    }

    // From phrase
    _ = try p.expectIdentifier(.{ .from = true });
    const from_expr = try p.expectExpr(.{ .where = true });

    if (p.peekBuiltin("where")) |_| {
        return p.fail(.cannot_define_where_cond_in_delete_cols);
    }

    const select = try p.listToSpan(p.scratch.items[select_top..]);
    const delete_node: Node.DeleteCols = .{
        .select_token_start = select.start,
        .select_token_end = select.end,
        .from = from_expr,
    };
    return p.setNode(delete_index, .{
        .tag = .delete_cols,
        .main_token = delete_token,
        .data = .{ .extra = try p.addExtra(delete_node) },
    });
}

/// Operator
///     <- PLUS
///      / MINUS
///      / ASTERISK
///      / PERCENT
///      / AMPERSAND
///      / PIPE
///      / CARET
///      / EQUAL
///      / ANGLE_BRACKET_LEFT
///      / ANGLE_BRACKET_LEFT_EQUAL
///      / ANGLE_BRACKET_LEFT_RIGHT
///      / ANGLE_BRACKET_RIGHT
///      / ANGLE_BRACKET_RIGHT_EQUAL
///      / DOLLAR
///      / COMMA
///      / HASH
///      / UNDERSCORE
///      / TILDE
///      / BANG
///      / QUESTION_MARK
///      / AT
///      / PERIOD
///      / ZERO_COLON
///      / ONE_COLON
///      / TWO_COLON
fn parseOperator(p: *Parse) !Node.Index {
    const token_tag: Token.Tag = p.peekTag();
    const node_tag: Node.Tag = switch (token_tag) {
        .colon => .colon,
        .colon_colon => .colon_colon,
        .plus => .plus,
        .plus_colon => .plus_colon,
        .minus => .minus,
        .minus_colon => .minus_colon,
        .asterisk => .asterisk,
        .asterisk_colon => .asterisk_colon,
        .percent => .percent,
        .percent_colon => .percent_colon,
        .ampersand => .ampersand,
        .ampersand_colon => .ampersand_colon,
        .pipe => .pipe,
        .pipe_colon => .pipe_colon,
        .caret => .caret,
        .caret_colon => .caret_colon,
        .equal => .equal,
        .equal_colon => .equal_colon,
        .angle_bracket_left => .angle_bracket_left,
        .angle_bracket_left_colon => .angle_bracket_left_colon,
        .angle_bracket_left_equal => .angle_bracket_left_equal,
        .angle_bracket_left_right => .angle_bracket_left_right,
        .angle_bracket_right => .angle_bracket_right,
        .angle_bracket_right_colon => .angle_bracket_right_colon,
        .angle_bracket_right_equal => .angle_bracket_right_equal,
        .dollar => .dollar,
        .dollar_colon => .dollar_colon,
        .comma => .comma,
        .comma_colon => .comma_colon,
        .hash => .hash,
        .hash_colon => .hash_colon,
        .underscore => .underscore,
        .underscore_colon => .underscore_colon,
        .tilde => .tilde,
        .tilde_colon => .tilde_colon,
        .bang => .bang,
        .bang_colon => .bang_colon,
        .question_mark => .question_mark,
        .question_mark_colon => .question_mark_colon,
        .at => .at,
        .at_colon => .at_colon,
        .period => .period,
        .period_colon => .period_colon,
        .zero_colon => .zero_colon,
        .zero_colon_colon => .zero_colon_colon,
        .one_colon => .one_colon,
        .one_colon_colon => .one_colon_colon,
        .two_colon => .two_colon,
        else => unreachable,
    };
    return p.parseToken(token_tag, node_tag);
}

/// Iterator
///     <- (APOSTROPHE
///      / APOSTROPHE_COLON
///      / SLASH
///      / SLASH_COLON
///      / BACKSLASH
///      / BACKSLASH_COLON)*
fn parseIterator(p: *Parse, lhs: Node.OptionalIndex) Error!Node.Index {
    const token_tag = p.peekTag();
    const node_tag: Node.Tag = switch (token_tag) {
        .apostrophe => .apostrophe,
        .apostrophe_colon => .apostrophe_colon,
        .slash => .slash,
        .slash_colon => .slash_colon,
        .backslash => .backslash,
        .backslash_colon => .backslash_colon,
        else => return lhs.unwrap().?,
    };
    const iterator = try p.addNode(.{
        .tag = node_tag,
        .main_token = p.assertToken(token_tag),
        .data = .{ .opt_node = lhs },
    });
    return p.parseCall(iterator);
}

/// NumberLiteral <- NUMBER_LITERAL+
/// TODO: Should we validate number suffixes here?
fn parseNumberLiteral(p: *Parse) !Node.Index {
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    while (p.peekTag() == .number_literal) {
        const number_literal = p.assertToken(.number_literal);
        try p.scratch.append(p.gpa, @enumFromInt(number_literal));
    }

    const items = p.scratch.items[scratch_top..];
    switch (items.len) {
        0 => unreachable,
        1 => return p.addNode(.{
            .tag = .number_literal,
            .main_token = @intFromEnum(items[0]),
            .data = undefined,
        }),
        else => return p.addNode(.{
            .tag = .number_list_literal,
            .main_token = @intFromEnum(items[0]),
            .data = .{ .token = @intFromEnum(items[items.len - 1]) },
        }),
    }
}

/// SymbolLiteral <- SYMBOL_LITERAL+
fn parseSymbolLiteral(p: *Parse) !Node.Index {
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    while (p.peekTag() == .symbol_literal) {
        const symbol_literal = p.assertToken(.symbol_literal);
        try p.scratch.append(p.gpa, @enumFromInt(symbol_literal));
        if (p.prevLoc().end != p.peekLoc().start) break;
    }

    const items = p.scratch.items[scratch_top..];
    switch (items.len) {
        0 => unreachable,
        1 => return p.addNode(.{
            .tag = .symbol_literal,
            .main_token = @intFromEnum(items[0]),
            .data = undefined,
        }),
        else => return p.addNode(.{
            .tag = .symbol_list_literal,
            .main_token = @intFromEnum(items[0]),
            .data = .{ .token = @intFromEnum(items[items.len - 1]) },
        }),
    }
}

fn eatToken(p: *Parse, tag: Token.Tag) ?TokenIndex {
    return if (p.peekTag() == tag) p.nextToken() else null;
}

const SqlIdentifier = packed struct(u8) {
    by: bool = false,
    from: bool = false,
    where: bool = false,
    _: u5 = 0,
};

fn peekIdentifier(p: *Parse, comptime sql_identifier: SqlIdentifier) ?TokenIndex {
    const tag = p.peekTag();
    switch (tag) {
        .identifier => {
            const slice = p.tokenSlice(p.tok_i);
            if (sql_identifier.by and std.mem.eql(u8, slice, "by")) return p.tok_i;
            if (sql_identifier.from and std.mem.eql(u8, slice, "from")) return p.tok_i;
        },
        .prefix_builtin => {
            const slice = p.tokenSlice(p.tok_i);
            if (sql_identifier.where and std.mem.eql(u8, slice, "where")) return p.tok_i;
        },
        else => {},
    }
    return null;
}

fn identifierEql(p: *Parse, identifier: []const u8, token_index: TokenIndex) bool {
    return p.peekTag() == .identifier and std.mem.eql(u8, p.tokenSlice(token_index), identifier);
}

fn eatIdentifier(p: *Parse, comptime sql_identifier: SqlIdentifier) ?TokenIndex {
    if (p.peekIdentifier(sql_identifier)) |i| {
        _ = p.nextToken();
        return i;
    }
    return null;
}

fn expectIdentifier(p: *Parse, comptime sql_identifier: SqlIdentifier) !TokenIndex {
    return p.eatIdentifier(sql_identifier) orelse p.failMsg(.{
        .tag = .expected_qsql_token,
        .token = p.tok_i,
        .extra = .{
            .expected_string = comptime if (sql_identifier.by)
                "by"
            else if (sql_identifier.from)
                "from"
            else if (sql_identifier.where)
                "where",
        },
    });
}

fn peekBuiltin(p: *Parse, comptime builtin: []const u8) ?TokenIndex {
    if (p.peekTag() == .prefix_builtin) {
        const slice = p.tokenSlice(p.tok_i);
        if (std.mem.eql(u8, slice, builtin)) return p.tok_i;
    }
    return null;
}

fn eatBuiltin(p: *Parse, comptime builtin: []const u8) ?TokenIndex {
    if (p.peekBuiltin(builtin)) |i| {
        _ = p.nextToken();
        return i;
    }
    return null;
}

fn assertToken(p: *Parse, tag: Token.Tag) TokenIndex {
    assert(p.peekTag() == tag);
    return p.nextToken();
}

fn expectToken(p: *Parse, tag: Token.Tag) !TokenIndex {
    return p.eatToken(tag) orelse p.failExpected(tag);
}

fn prevTag(p: *Parse) Token.Tag {
    const tag: Token.Tag = p.tokens.items(.tag)[p.tok_i - 1];
    return tag;
}

fn prevLoc(p: *Parse) Token.Loc {
    const loc: Token.Loc = p.tokens.items(.loc)[p.tok_i - 1];
    return loc;
}

fn peekTag(p: *Parse) Token.Tag {
    if (p.eob) return .eob;
    const tag: Token.Tag = p.tokens.items(.tag)[p.tok_i];
    return tag;
}

fn peekLoc(p: *Parse) Token.Loc {
    const loc: Token.Loc = p.tokens.items(.loc)[p.tok_i];
    return loc;
}

fn nextToken(p: *Parse) TokenIndex {
    if (p.tok_i == p.tokens.len - 1) {
        p.eob = true;
        return p.tok_i;
    }

    const result = p.tok_i;
    p.eob = p.isEob(result);
    p.tok_i += 1;
    return result;
}

// TODO: Test
fn skipBlock(p: *Parse) void {
    if (p.eob) {
        _ = p.nextToken();
    } else {
        while (!p.eob) {
            _ = p.nextToken();
        }
    }

    p.ends_expr.clearRetainingCapacity();
}

fn isEob(p: *Parse, token_index: TokenIndex) bool {
    const token_tags: []Token.Tag = p.tokens.items(.tag);
    const token_locs: []Token.Loc = p.tokens.items(.loc);

    const tag = token_tags[token_index];
    switch (tag) {
        .semicolon => return p.ends_expr.items.len == 0,
        .eof => return true,
        else => {
            if (token_tags[token_index + 1] == .eof) return true;
            const next_token_start = token_locs[token_index + 1].start;
            return next_token_start == p.source.len or p.source[next_token_start - 1] == '\n';
        },
    }
}

const Parse = @This();
const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const kdb = @import("root.zig");
const Ast = kdb.Ast;
const Node = Ast.Node;
const AstError = Ast.Error;
const TokenIndex = Ast.TokenIndex;
const OptionalTokenIndex = Ast.OptionalTokenIndex;
const ExtraIndex = Ast.ExtraIndex;
const Token = kdb.Token;
