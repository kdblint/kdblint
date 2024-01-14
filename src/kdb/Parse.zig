//! Represents in-progress parsing, will be converted to an Ast after completion.

pub const Error = error{ParseError} || Allocator.Error;

gpa: Allocator,
source: []const u8,
token_tags: []const Token.Tag,
token_locs: []const Token.Loc,
tok_i: TokenIndex,
errors: std.ArrayListUnmanaged(AstError),
nodes: Ast.NodeList,
extra_data: std.ArrayListUnmanaged(Node.Index),
scratch: std.ArrayListUnmanaged(Node.Index),

const Blocks = struct {
    len: usize,
    lhs: Node.Index,
    rhs: Node.Index,
    trailing: bool,

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
        .start = @as(Node.Index, @intCast(p.extra_data.items.len - list.len)),
        .end = @as(Node.Index, @intCast(p.extra_data.items.len)),
    };
}

fn addNode(p: *Parse, elem: Ast.Node) Allocator.Error!Node.Index {
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
        p.nodes.items(.tag)[node_index] = .unreachable_literal;
        p.nodes.items(.main_token)[node_index] = p.tok_i;
    }
}

fn addExtra(p: *Parse, extra: anytype) Allocator.Error!Node.Index {
    const fields = std.meta.fields(@TypeOf(extra));
    try p.extra_data.ensureUnusedCapacity(p.gpa, fields.len);
    const result = @as(u32, @intCast(p.extra_data.items.len));
    inline for (fields) |field| {
        comptime assert(field.type == Node.Index);
        p.extra_data.appendAssumeCapacity(@field(extra, field.name));
    }
    return result;
}

fn warnExpected(p: *Parse, expected_token: Token.Tag) error{OutOfMemory}!void {
    @setCold(true);
    try p.warnMsg(.{
        .tag = .expected_token,
        .token = p.tok_i,
        .extra = .{ .expected_tag = expected_token },
    });
}

fn warn(p: *Parse, error_tag: AstError.Tag) error{OutOfMemory}!void {
    @setCold(true);
    try p.warnMsg(.{ .tag = error_tag, .token = p.tok_i });
}

fn warnMsg(p: *Parse, msg: Ast.Error) error{OutOfMemory}!void {
    @setCold(true);
    switch (msg.tag) {
        .expected_semi_after_decl,
        .expected_semi_after_stmt,
        .expected_comma_after_field,
        .expected_comma_after_arg,
        .expected_comma_after_param,
        .expected_comma_after_initializer,
        .expected_comma_after_switch_prong,
        .expected_comma_after_for_operand,
        .expected_comma_after_capture,
        .expected_semi_or_else,
        .expected_semi_or_lbrace,
        .expected_token,
        .expected_block,
        .expected_block_or_assignment,
        .expected_block_or_expr,
        .expected_block_or_field,
        .expected_expr,
        .expected_expr_or_assignment,
        .expected_fn,
        .expected_inlinable,
        .expected_labelable,
        .expected_param_list,
        .expected_prefix_expr,
        .expected_primary_type_expr,
        .expected_pub_item,
        .expected_return_type,
        .expected_suffix_op,
        .expected_type_expr,
        .expected_var_decl,
        .expected_var_decl_or_fn,
        .expected_loop_payload,
        .expected_container,
        => if (msg.token != 0 and !p.tokensOnSameLine(msg.token - 1, msg.token)) {
            var copy = msg;
            copy.token_is_prev = true;
            copy.token -= 1;
            return p.errors.append(p.gpa, copy);
        },
        else => {},
    }
    try p.errors.append(p.gpa, msg);
}

fn fail(p: *Parse, tag: Ast.Error.Tag) error{ ParseError, OutOfMemory } {
    @setCold(true);
    return p.failMsg(.{ .tag = tag, .token = p.tok_i });
}

fn failExpected(p: *Parse, expected_token: Token.Tag) error{ ParseError, OutOfMemory } {
    @setCold(true);
    return p.failMsg(.{
        .tag = .expected_token,
        .token = p.tok_i,
        .extra = .{ .expected_tag = expected_token },
    });
}

fn failMsg(p: *Parse, msg: Ast.Error) error{ ParseError, OutOfMemory } {
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
    _ = p.eatToken(.comment);

    const blocks = p.parseBlocks() catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.ParseError => unreachable,
    };
    const root_decls = try blocks.toSpan(p);
    if (p.token_tags[p.tok_i] != .eof) {
        try p.warnExpected(.eof);
    }
    p.nodes.items(.data)[0] = .{
        .lhs = root_decls.start,
        .rhs = root_decls.end,
    };
}

fn parseBlocks(p: *Parse) Error!Blocks {
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    const field_state: union(enum) {
        /// No fields have been seen.
        none,
        /// Currently parsing fields.
        seen,
        /// Saw fields and then a declaration after them.
        /// Payload is first token of previous declaration.
        end: Node.Index,
        /// There was a declaration between fields, don't report more errors.
        err,
    } = .none;
    _ = field_state; // autofix

    const last_field: TokenIndex = undefined;
    _ = last_field; // autofix

    // Skip comments.
    while (p.eatToken(.comment)) |_| {}

    var trailing = false;
    while (true) {
        _ = p.eatComments();

        switch (p.token_tags[p.tok_i]) {
            .eof => break,
            else => {},
        }

        const expr = try p.parseExpr();
        if (expr != 0) {
            try p.scratch.append(p.gpa, expr);
        }
        trailing = p.token_tags[p.tok_i - 1] == .semicolon;
    }

    const items = p.scratch.items[scratch_top..];
    switch (items.len) {
        0 => return Blocks{
            .len = 0,
            .rhs = 0,
            .lhs = 0,
            .trailing = trailing,
        },
        1 => return Blocks{
            .len = 1,
            .lhs = items[0],
            .rhs = 0,
            .trailing = trailing,
        },
        2 => return Blocks{
            .len = 2,
            .lhs = items[0],
            .rhs = items[1],
            .trailing = trailing,
        },
        else => {
            const span = try p.listToSpan(items);
            return Blocks{
                .len = items.len,
                .lhs = span.start,
                .rhs = span.end,
                .trailing = trailing,
            };
        },
    }
}

fn parseExpr(p: *Parse) Error!Node.Index {
    return p.parseExprPrecedence(0);
}

fn expectExpr(p: *Parse) Error!Node.Index {
    const node = try p.parseExpr();
    if (node == 0) {
        return p.fail(.expected_expr);
    } else {
        return node;
    }
}

const Assoc = enum {
    left,
    none,
};

const OperInfo = struct {
    prec: i8,
    tag: Node.Tag,
    assoc: Assoc = Assoc.left,
};

const operTable = std.enums.directEnumArrayDefault(Token.Tag, OperInfo, .{ .prec = -1, .tag = Node.Tag.root }, 0, .{});

fn parseExprPrecedence(p: *Parse, min_prec: i32) Error!Node.Index {
    assert(min_prec >= 0);
    var node = try p.parsePrefixExpr();
    if (node == 0) {
        return null_node;
    }

    var banned_prec: i8 = -1;

    while (true) {
        const tok_tag = p.token_tags[p.tok_i];
        const info = operTable[@as(usize, @intCast(@intFromEnum(tok_tag)))];
        if (info.prec < min_prec) {
            break;
        }
        if (info.prec == banned_prec) {
            return p.fail(.chained_comparison_operators);
        }

        const oper_token = p.nextToken();
        const rhs = try p.parseExprPrecedence(info.prec + 1);
        if (rhs == 0) {
            try p.warn(.expected_expr);
            return node;
        }

        {
            const tok_len = tok_tag.lexeme().?.len;
            _ = tok_len; // autofix
            const char_before = p.source[p.token_locs[oper_token].start - 1];
            const char_after = p.source[p.token_locs[oper_token].end];
            if (tok_tag == .ampersand and char_after == '&') {
                // without types we don't know if '&&' was intended as 'bitwise_and address_of', or a c-style logical_and
                // The best the parser can do is recommend changing it to 'and' or ' & &'
                try p.warnMsg(.{ .tag = .invalid_ampersand_ampersand, .token = oper_token });
            } else if (std.ascii.isWhitespace(char_before) != std.ascii.isWhitespace(char_after)) {
                try p.warnMsg(.{ .tag = .mismatched_binary_op_whitespace, .token = oper_token });
            }
        }

        node = try p.addNode(.{
            .tag = info.tag,
            .main_token = oper_token,
            .data = .{
                .lhs = node,
                .rhs = rhs,
            },
        });

        if (info.assoc == Assoc.none) {
            banned_prec = info.prec;
        }
    }

    return node;
}

fn parsePrefixExpr(p: *Parse) Error!Node.Index {
    const tag: Node.Tag = switch (p.token_tags[p.tok_i]) {
        .bang => .bool_not,
        .minus => .negation,
        .tilde => .bit_not,
        // .minus_percent => .negation_wrap,
        .ampersand => .address_of,
        // .keyword_try => .@"try",
        // .keyword_await => .@"await",
        else => return p.parsePrimaryExpr(),
    };
    return p.addNode(.{
        .tag = tag,
        .main_token = p.nextToken(),
        .data = .{
            .lhs = try p.expectPrefixExpr(),
            .rhs = undefined,
        },
    });
}

fn expectPrefixExpr(p: *Parse) Error!Node.Index {
    const node = try p.parsePrefixExpr();
    if (node == 0) {
        return p.fail(.expected_prefix_expr);
    }
    return node;
}

fn parsePrimaryExpr(p: *Parse) !Node.Index {
    switch (p.token_tags[p.tok_i]) {
        .number_literal => unreachable,
        else => unreachable,
    }
}

fn eatComments(p: *Parse) ?TokenIndex {
    if (p.eatToken(.comment)) |tok| {
        while (p.eatToken(.comment)) |_| {}
        return tok;
    }
    return null;
}

fn tokensOnSameLine(p: *Parse, token1: TokenIndex, token2: TokenIndex) bool {
    return std.mem.indexOfScalar(u8, p.source[p.token_locs[token1].start..p.token_locs[token2].start], '\n') == null;
}

fn eatToken(p: *Parse, tag: Token.Tag) ?TokenIndex {
    return if (p.token_tags[p.tok_i] == tag) p.nextToken() else null;
}

fn assertToken(p: *Parse, tag: Token.Tag) TokenIndex {
    const token = p.nextToken();
    assert(p.token_tags[token] == tag);
    return token;
}

fn expectToken(p: *Parse, tag: Token.Tag) Error!TokenIndex {
    if (p.token_tags[p.tok_i] != tag) {
        return p.failMsg(.{
            .tag = .expected_token,
            .token = p.tok_i,
            .extra = .{ .expected_tag = tag },
        });
    }
    return p.nextToken();
}

fn expectSemicolon(p: *Parse, error_tag: AstError.Tag, recoverable: bool) Error!void {
    if (p.token_tags[p.tok_i] == .semicolon) {
        _ = p.nextToken();
        return;
    }
    try p.warn(error_tag);
    if (!recoverable) return error.ParseError;
}

fn nextToken(p: *Parse) TokenIndex {
    const result = p.tok_i;
    p.tok_i += 1;
    return result;
}

const null_node: Node.Index = 0;

const Parse = @This();
const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const kdb = @import("../kdb.zig");
const Ast = kdb.Ast;
const Node = Ast.Node;
const AstError = Ast.Error;
const TokenIndex = Ast.TokenIndex;
const Token = kdb.Token;

test {
    @import("std").testing.refAllDecls(@This());
}
