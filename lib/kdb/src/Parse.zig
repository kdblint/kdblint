//! Represents in-progress parsing, will be converted to an Ast after completion.
const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;

const kdb = @import("root.zig");
const Token = kdb.Token;
const Ast = kdb.Ast;
const Node = Ast.Node;

const Parse = @This();

pub const Error = error{ParseError} || Allocator.Error;

gpa: Allocator,
mode: Ast.Mode,
source: []const u8,
tokens: std.MultiArrayList(Token) = .{},
tok_i: Token.Index = 0,
eob: bool = false,
within_fn: bool = false,
ends_expr: std.ArrayListUnmanaged(Token.Tag) = .{},
errors: std.ArrayListUnmanaged(Ast.Error) = .{},
nodes: std.MultiArrayList(Node) = .{},
extra_data: std.ArrayListUnmanaged(Node.Index) = .{},
scratch: std.ArrayListUnmanaged(Node.Index) = .{},

pub fn deinit(p: *Parse) void {
    p.ends_expr.deinit(p.gpa);
    p.errors.deinit(p.gpa);
    p.nodes.deinit(p.gpa);
    p.extra_data.deinit(p.gpa);
    p.scratch.deinit(p.gpa);
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

fn listToSpan(p: *Parse, list: []const Node.Index) !Node.SubRange {
    try p.extra_data.appendSlice(p.gpa, list);
    return Node.SubRange{
        .start = @intCast(p.extra_data.items.len - list.len),
        .end = @intCast(p.extra_data.items.len),
    };
}

fn addNode(p: *Parse, elem: Ast.Node) !Node.Index {
    const result = @as(Node.Index, @intCast(p.nodes.len));
    try p.nodes.append(p.gpa, elem);
    return result;
}

fn setNode(p: *Parse, i: usize, elem: Ast.Node) Node.Index {
    p.nodes.set(i, elem);
    return @as(Node.Index, @intCast(i));
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

fn addExtra(p: *Parse, extra: anytype) !Node.Index {
    const fields = std.meta.fields(@TypeOf(extra));
    try p.extra_data.ensureUnusedCapacity(p.gpa, fields.len);
    const result = @as(u32, @intCast(p.extra_data.items.len));
    inline for (fields) |field| {
        switch (@typeInfo(field.type)) {
            .int => comptime assert(field.type == Node.Index),
            .@"struct" => |ti| comptime assert(ti.layout == .@"packed" and ti.backing_integer.? == Node.Index),
            inline else => |tag| @compileError("Expected Node.Index or packed struct, found '" ++ @tagName(tag) ++ "'"),
        }
        p.extra_data.appendAssumeCapacity(@bitCast(@field(extra, field.name)));
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

fn tokenSlice(p: *Parse, token_index: Token.Index) []const u8 {
    const loc: Token.Loc = p.tokens.items(.loc)[token_index];
    return p.source[loc.start..loc.end];
}

// TODO: Add tests
fn validateUnaryApplication(p: *Parse, lhs: Node.Index, rhs: Node.Index) !void {
    assert(lhs != null_node);
    assert(rhs != null_node);
    const node_tags: []Node.Tag = p.nodes.items(.tag);
    const node_datas: []Node.Data = p.nodes.items(.data);
    const main_tokens: []Token.Index = p.nodes.items(.main_token);
    const token_tags: []Token.Tag = p.tokens.items(.tag);

    const tag = node_tags[lhs];
    switch (tag.getType()) {
        // Fail if we are applying a unary operator directly in q.
        .unary_operator => if (p.mode == .q and
            (!p.within_fn or tag != .colon or switch (token_tags[main_tokens[lhs] - 1]) {
            .l_paren,
            .l_brace,
            .l_bracket,
            .r_bracket,
            .semicolon,
            => false,
            else => true,
        })) {
            return p.warnMsg(.{
                .tag = .cannot_apply_operator_directly,
                .token = main_tokens[lhs],
            });
        },

        // Fail if we are applying an iterator directly in q.
        .iterator => if (p.mode == .q and (tag != .apostrophe or node_datas[lhs].lhs != 0)) {
            return p.warnMsg(.{
                .tag = .cannot_apply_iterator_directly,
                .token = main_tokens[lhs],
            });
        },

        .other => {},
    }
}

/// Root <- Blocks EOF
pub fn parseRoot(p: *Parse) Allocator.Error!void {
    // Root node must be index 0.
    const root_index = try p.reserveNode(.root);
    errdefer p.unreserveNode(root_index);

    const blocks = try p.parseBlocks();
    if (p.peekTag() != .eof) {
        try p.warnExpected(.eof);
    }

    const root_decls = try blocks.toSpan(p);
    _ = p.setNode(root_index, .{
        .tag = .root,
        .main_token = undefined,
        .data = .{
            .lhs = root_decls.start,
            .rhs = root_decls.end,
        },
    });
}

/// Blocks <- Block*
fn parseBlocks(p: *Parse) !Blocks {
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    while (p.peekTag() != .eof) {
        const block_node = try p.parseBlock();
        if (block_node != null_node) {
            try p.scratch.append(p.gpa, block_node);
        }
        if (!p.eob) {
            try p.warn(.expected_expr);
            p.skipBlock();
        }
        assert(p.eob);
        p.eob = false;
    }

    const items = p.scratch.items[scratch_top..];
    switch (items.len) {
        0 => return Blocks{
            .len = 0,
            .lhs = null_node,
            .rhs = null_node,
        },
        1 => return Blocks{
            .len = 1,
            .lhs = items[0],
            .rhs = null_node,
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

/// Block <- Expr SEMICOLON?
fn parseBlock(p: *Parse) !Node.Index {
    const expr = p.parseExpr(null) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => blk: {
            p.skipBlock();
            break :blk null_node;
        },
    };

    // TODO: Use trailing.
    _ = p.eatToken(.semicolon);

    return expr;
}

/// Expr <- Noun Verb*
fn parseExpr(p: *Parse, comptime sql_identifier: ?SqlIdentifier) Error!Node.Index {
    if (sql_identifier) |sql_id| if (p.peekIdentifier(sql_id)) |_| return null_node;
    var node = try p.parseNoun();
    if (node == null_node) {
        return null_node;
    }

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

    return node;
}

fn expectExpr(p: *Parse, comptime sql_identifier: ?SqlIdentifier) !Token.Index {
    const expr = try p.parseExpr(sql_identifier);
    if (expr != null_node) return expr;
    return p.fail(.expected_expr);
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
fn parseNoun(p: *Parse) !Node.Index {
    const tag = p.peekTag();
    const noun = switch (tag) {
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
        => try p.parseIterator(null_node),

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

        else => return null_node,
    };
    return p.parseCall(noun);
}

/// Verb <- Expr*
fn parseVerb(p: *Parse, lhs: Node.Index, comptime sql_identifier: ?SqlIdentifier) !Node.Index {
    assert(lhs != null_node);

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

            const op = try p.parseNoun();
            assert(op != null_node);

            const node_tags: []Node.Tag = p.nodes.items(.tag);
            if (node_tags[op].getType() == .iterator) {
                const rhs = try p.parseExpr(sql_identifier);
                return p.setNode(apply_index, .{
                    .tag = .apply_binary,
                    .main_token = op,
                    .data = .{
                        .lhs = lhs,
                        .rhs = rhs,
                    },
                });
            } else {
                const rhs = try p.parseVerb(op, sql_identifier);
                try p.validateUnaryApplication(lhs, rhs);
                return p.setNode(apply_index, .{
                    .tag = .apply_unary,
                    .main_token = undefined,
                    .data = .{
                        .lhs = lhs,
                        .rhs = rhs,
                    },
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

            const op = try p.parseNoun();
            assert(op != null_node);

            const node_tags: []Node.Tag = p.nodes.items(.tag);
            if (node_tags[op] == .call) {
                const rhs = try p.parseVerb(op, sql_identifier);
                try p.validateUnaryApplication(lhs, rhs);
                return p.setNode(apply_index, .{
                    .tag = .apply_unary,
                    .main_token = undefined,
                    .data = .{
                        .lhs = lhs,
                        .rhs = rhs,
                    },
                });
            } else {
                const rhs = try p.parseExpr(sql_identifier);
                return p.setNode(apply_index, .{
                    .tag = .apply_binary,
                    .main_token = op,
                    .data = .{
                        .lhs = lhs,
                        .rhs = rhs,
                    },
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
        .data = .{
            .lhs = undefined,
            .rhs = undefined,
        },
    });
}

fn parseEmpty(p: *Parse) !Node.Index {
    const token_tags: []Token.Tag = p.tokens.items(.tag);
    switch (token_tags[p.tok_i]) {
        .semicolon, .r_paren, .r_brace, .r_bracket => return p.addNode(.{
            .tag = .empty,
            .main_token = p.tok_i,
            .data = .{
                .lhs = undefined,
                .rhs = undefined,
            },
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
            .data = .{
                .lhs = undefined,
                .rhs = r_paren,
            },
        });
    }

    const table = try p.parseTable(l_paren);
    if (table != null_node) {
        return table;
    }

    const list_index = try p.reserveNode(.list);
    errdefer p.unreserveNode(list_index);

    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    try p.ends_expr.append(p.gpa, .r_paren);

    while (true) {
        const expr = try p.parseExpr(null);
        try p.scratch.append(p.gpa, if (expr == null_node)
            try p.parseEmpty()
        else
            expr);
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
            .data = .{
                .lhs = list[0],
                .rhs = r_paren,
            },
        }),
        else => return p.setNode(list_index, .{
            .tag = .list,
            .main_token = l_paren,
            .data = .{
                .lhs = try p.addExtra(try p.listToSpan(list)),
                .rhs = r_paren,
            },
        }),
    }
}

/// Table <- LPAREN LBRACKET (Expr (SEMICOLON Expr)*)* RBRACKET Expr (SEMICOLON Expr)* RPAREN
fn parseTable(p: *Parse, l_paren: Token.Index) !Node.Index {
    _ = p.eatToken(.l_bracket) orelse return null_node;

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
        .data = .{
            .lhs = try p.addExtra(table),
            .rhs = r_paren,
        },
    });
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

    const prev_within_fn = p.within_fn;
    defer p.within_fn = prev_within_fn;
    p.within_fn = true;

    const body_top = p.scratch.items.len;
    while (true) {
        const expr = try p.parseExpr(null);
        try p.scratch.append(p.gpa, if (expr == null_node) try p.parseEmpty() else expr);
        _ = p.eatToken(.semicolon) orelse break;
        if (p.peekTag() == .r_brace) break;
    }

    const tag: Node.Tag = if (p.prevTag() == .semicolon) .lambda_semicolon else .lambda;
    _ = try p.expectToken(.r_brace);
    assert(p.ends_expr.pop() == .r_brace);

    const params = try p.listToSpan(p.scratch.items[params_top..body_top]);
    const body = try p.listToSpan(p.scratch.items[body_top..]);
    return p.setNode(lambda_index, .{
        .tag = tag,
        .main_token = l_brace,
        .data = .{
            .lhs = try p.addExtra(params),
            .rhs = try p.addExtra(body),
        },
    });
}

/// ExprBlock <- LBRACKET Exprs? RBRACKET
fn parseExprBlock(p: *Parse) !Node.Index {
    const l_bracket = p.assertToken(.l_bracket);
    if (p.eatToken(.r_bracket)) |r_bracket| {
        return p.addNode(.{
            .tag = .expr_block,
            .main_token = l_bracket,
            .data = .{
                .lhs = null_node,
                .rhs = r_bracket,
            },
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
            try p.scratch.append(p.gpa, if (expr == null_node)
                try p.parseEmpty()
            else
                expr);
            _ = p.eatToken(.semicolon) orelse break;
        }

        break :exprs try p.listToSpan(p.scratch.items[scratch_top..]);
    };
    const r_bracket = try p.expectToken(.r_bracket);
    assert(p.ends_expr.pop() == .r_bracket);

    return p.setNode(expr_block_index, .{
        .tag = .expr_block,
        .main_token = l_bracket,
        .data = .{
            .lhs = try p.addExtra(exprs),
            .rhs = r_bracket,
        },
    });
}

/// Call <- (LBRACKET Expr (SEMICOLON Expr)* RBRACKET)*
pub fn parseCall(p: *Parse, lhs: Node.Index) !Node.Index {
    assert(lhs != null_node);
    if (p.peekTag() != .l_bracket) return p.parseIterator(lhs);

    const l_bracket = p.assertToken(.l_bracket);

    try p.ends_expr.append(p.gpa, .r_bracket);

    const node_index = try p.reserveNode(.call);
    errdefer p.unreserveNode(node_index);

    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    while (true) {
        const expr = try p.parseExpr(null);
        try p.scratch.append(p.gpa, if (expr == null_node) try p.parseEmpty() else expr);
        _ = p.eatToken(.semicolon) orelse break;
    }

    _ = try p.expectToken(.r_bracket);
    assert(p.ends_expr.pop() == .r_bracket);

    const args = p.scratch.items[scratch_top..];

    return p.parseCall(p.setNode(node_index, .{
        .tag = .call,
        .main_token = l_bracket,
        .data = .{
            .lhs = lhs,
            .rhs = try p.addExtra(try p.listToSpan(args)),
        },
    }));
}

fn parseSelect(p: *Parse) !Node.Index {
    const select_token = p.assertToken(.keyword_select);

    const select_index = try p.reserveNode(.select);
    errdefer p.unreserveNode(select_index);

    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    // Limit expression
    var distinct = false;
    var ascending = false;
    var limit_expr: Node.Index = 0;
    var order_tok: Token.Index = 0;
    if (p.eatBuiltin("distinct")) |_| {
        distinct = true;
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
                order_tok = try p.expectToken(.identifier);
            },

            .angle_bracket_right,
            .angle_bracket_right_colon,
            .angle_bracket_right_equal,
            => {
                _ = p.nextToken();
                order_tok = try p.expectToken(.identifier);
            },

            else => {
                limit_expr = try p.expectExpr(null);
                if (p.eatToken(.semicolon)) |_| {
                    switch (p.peekTag()) {
                        .angle_bracket_left,
                        .angle_bracket_left_colon,
                        .angle_bracket_left_equal,
                        .angle_bracket_left_right,
                        => {
                            _ = p.nextToken();
                            ascending = true;
                            order_tok = try p.expectToken(.identifier);
                        },

                        .angle_bracket_right,
                        .angle_bracket_right_colon,
                        .angle_bracket_right_equal,
                        => {
                            _ = p.nextToken();
                            order_tok = try p.expectToken(.identifier);
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
    const has_by = p.eatIdentifier(.{ .by = true }) != null;
    if (has_by) {
        const first_expr = try p.parseExpr(.{ .from = true });
        if (first_expr > 0) {
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
        .select_start = select.start,
        .by_start = by.start,
        .from = from_expr,
        .where_start = where.start,
        .where_end = where.end,
        .data = .{
            .has_by = has_by,
            .distinct = distinct,
            .ascending = ascending,
        },
    };
    return p.setNode(select_index, .{
        .tag = .select,
        .main_token = select_token,
        .data = .{
            .lhs = try p.addExtra(select_node),
            .rhs = undefined,
        },
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
        .data = .{
            .lhs = try p.addExtra(exec_node),
            .rhs = undefined,
        },
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
        .data = .{
            .lhs = try p.addExtra(update_node),
            .rhs = undefined,
        },
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
            .data = .{
                .lhs = try p.addExtra(delete_node),
                .rhs = undefined,
            },
        });
    }

    // Select phrase
    const select_top = p.scratch.items.len;
    while (true) {
        const identifier = try p.expectToken(.identifier);
        try p.scratch.append(p.gpa, identifier);
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
        .data = .{
            .lhs = try p.addExtra(delete_node),
            .rhs = undefined,
        },
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
fn parseIterator(p: *Parse, lhs: Node.Index) Error!Node.Index {
    const token_tag = p.peekTag();
    const node_tag: Node.Tag = switch (token_tag) {
        .apostrophe => .apostrophe,
        .apostrophe_colon => .apostrophe_colon,
        .slash => .slash,
        .slash_colon => .slash_colon,
        .backslash => .backslash,
        .backslash_colon => .backslash_colon,
        else => return lhs,
    };
    const iterator = try p.addNode(.{
        .tag = node_tag,
        .main_token = p.assertToken(token_tag),
        .data = .{
            .lhs = lhs,
            .rhs = undefined,
        },
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
        try p.scratch.append(p.gpa, number_literal);
    }

    const items = p.scratch.items[scratch_top..];
    switch (items.len) {
        0 => unreachable,
        1 => return p.addNode(.{
            .tag = .number_literal,
            .main_token = items[0],
            .data = .{
                .lhs = undefined,
                .rhs = undefined,
            },
        }),
        else => return p.addNode(.{
            .tag = .number_list_literal,
            .main_token = items[0],
            .data = .{
                .lhs = items[items.len - 1],
                .rhs = undefined,
            },
        }),
    }
}

/// SymbolLiteral <- SYMBOL_LITERAL+
fn parseSymbolLiteral(p: *Parse) !Node.Index {
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    while (p.peekTag() == .symbol_literal) {
        const symbol_literal = p.assertToken(.symbol_literal);
        try p.scratch.append(p.gpa, symbol_literal);
        if (p.prevLoc().end != p.peekLoc().start) break;
    }

    const items = p.scratch.items[scratch_top..];
    switch (items.len) {
        0 => unreachable,
        1 => return p.addNode(.{
            .tag = .symbol_literal,
            .main_token = items[0],
            .data = .{
                .lhs = undefined,
                .rhs = undefined,
            },
        }),
        else => return p.addNode(.{
            .tag = .symbol_list_literal,
            .main_token = items[0],
            .data = .{
                .lhs = items[items.len - 1],
                .rhs = undefined,
            },
        }),
    }
}

fn eatToken(p: *Parse, tag: Token.Tag) ?Token.Index {
    return if (p.peekTag() == tag) p.nextToken() else null;
}

const SqlIdentifier = packed struct(u8) {
    by: bool = false,
    from: bool = false,
    where: bool = false,
    _: u5 = 0,
};

fn peekIdentifier(p: *Parse, comptime sql_identifier: SqlIdentifier) ?Token.Index {
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

fn identifierEql(p: *Parse, identifier: []const u8, token_index: Token.Index) bool {
    return p.peekTag() == .identifier and std.mem.eql(u8, p.tokenSlice(token_index), identifier);
}

fn eatIdentifier(p: *Parse, comptime sql_identifier: SqlIdentifier) ?Token.Index {
    if (p.peekIdentifier(sql_identifier)) |i| {
        _ = p.nextToken();
        return i;
    }
    return null;
}

fn expectIdentifier(p: *Parse, comptime sql_identifier: SqlIdentifier) !Token.Index {
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

fn peekBuiltin(p: *Parse, comptime builtin: []const u8) ?Token.Index {
    comptime assert(Token.getBuiltin(builtin) == .prefix);
    if (p.peekTag() == .prefix_builtin) {
        const slice = p.tokenSlice(p.tok_i);
        if (std.mem.eql(u8, slice, builtin)) return p.tok_i;
    }
    return null;
}

fn eatBuiltin(p: *Parse, comptime builtin: []const u8) ?Token.Index {
    comptime assert(Token.getBuiltin(builtin) == .prefix);
    if (p.peekBuiltin(builtin)) |i| {
        _ = p.nextToken();
        return i;
    }
    return null;
}

fn assertToken(p: *Parse, tag: Token.Tag) Token.Index {
    assert(p.peekTag() == tag);
    return p.nextToken();
}

fn expectToken(p: *Parse, tag: Token.Tag) !Token.Index {
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

fn nextToken(p: *Parse) Token.Index {
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

fn isEob(p: *Parse, token_index: Token.Index) bool {
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

const null_node: Node.Index = 0;
