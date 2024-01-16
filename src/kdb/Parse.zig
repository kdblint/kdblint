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

    const blocks = p.parseBlocks() catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.ParseError => unreachable,
    };
    const root_decls = try blocks.toSpan(p);
    p.nodes.items(.data)[0] = .{
        .lhs = root_decls.start,
        .rhs = root_decls.end,
    };
}

fn parseBlocks(p: *Parse) Error!Blocks {
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    while (p.tok_i < p.token_tags.len) {
        p.eatComments();

        const expr = try p.parseExpr();
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

fn parseExpr(p: *Parse) Error!Node.Index {
    return p.parseExprPrecedence(Precedence.secondary);
}

fn expectExpr(p: *Parse) Error!Node.Index {
    const node = try p.parseExpr();
    if (node == 0) {
        return p.fail(.expected_expr);
    } else {
        return node;
    }
}

fn plus(p: *Parse) Error!Node.Index {
    return p.addNode(.{
        .tag = .plus,
        .main_token = p.nextToken(),
        .data = .{
            .lhs = undefined,
            .rhs = undefined,
        },
    });
}

fn number(p: *Parse) Error!Node.Index {
    const tag = p.token_tags[p.tok_i];
    _ = tag; // autofix
    const loc = p.token_locs[p.tok_i];
    const source = p.source[loc.start..loc.end];
    _ = source; // autofix
    return try p.addNode(.{
        .tag = .number_literal,
        .main_token = p.nextToken(),
        .data = .{
            .lhs = undefined,
            .rhs = undefined,
        },
    });
}

fn binary(p: *Parse, lhs: Node.Index) Error!Node.Index {
    const rhs = try p.parseExpr();
    return p.addNode(.{
        .tag = .apply, // TODO: binary_op
        .main_token = p.nextToken(),
        .data = .{
            .lhs = lhs,
            .rhs = rhs,
        },
    });
}

fn apply(p: *Parse, lhs: Node.Index) Error!Node.Index {
    const rhs = try p.parseExpr();
    return p.addNode(.{
        .tag = .apply,
        .main_token = p.nextToken(),
        .data = .{
            .lhs = lhs,
            .rhs = rhs,
        },
    });
}

fn parseExprPrecedence(p: *Parse, precedence: Precedence) Error!Node.Index {
    const prefix = operTable[@intFromEnum(p.token_tags[p.tok_i])].prefix orelse @panic(@tagName(p.token_tags[p.tok_i]));
    var node = try prefix(p);

    while (p.tok_i < p.token_tags.len and @intFromEnum(precedence) <= @intFromEnum(operTable[@intFromEnum(p.token_tags[p.tok_i])].prec)) {
        const infix = operTable[@intFromEnum(p.token_tags[p.tok_i])].infix orelse @panic(@tagName(p.token_tags[p.tok_i]));
        node = try infix(p, node);
    }

    return node;
}

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

const operTable = std.enums.directEnumArray(Token.Tag, OperInfo, 0, .{
    // Punctuation
    .l_paren = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .r_paren = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .l_brace = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .r_brace = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .l_bracket = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .r_bracket = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .semicolon = .{ .prefix = null, .infix = null, .prec = Precedence.none },

    // Verbs
    .colon = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .colon_colon = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .plus = .{ .prefix = plus, .infix = binary, .prec = Precedence.secondary },
    .plus_colon = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .minus = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .minus_colon = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .asterisk = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .asterisk_colon = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .percent = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .percent_colon = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .bang = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .bang_colon = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .ampersand = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .ampersand_colon = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .pipe = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .pipe_colon = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .angle_bracket_left = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .angle_bracket_left_colon = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .angle_bracket_left_equal = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .angle_bracket_left_right = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .angle_bracket_right = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .angle_bracket_right_colon = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .angle_bracket_right_equal = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .equal = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .equal_colon = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .tilde = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .tilde_colon = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .comma = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .comma_colon = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .caret = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .caret_colon = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .hash = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .hash_colon = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .underscore = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .underscore_colon = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .dollar = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .dollar_colon = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .question_mark = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .question_mark_colon = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .at = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .at_colon = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .dot = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .dot_colon = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .zero_colon = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .zero_colon_colon = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .one_colon = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .one_colon_colon = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .two_colon = .{ .prefix = null, .infix = null, .prec = Precedence.none },

    // Adverbs
    .apostrophe = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .apostrophe_colon = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .slash = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .slash_colon = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .backslash = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .backslash_colon = .{ .prefix = null, .infix = null, .prec = Precedence.none },

    // Literals
    .number_literal = .{ .prefix = number, .infix = apply, .prec = Precedence.secondary },
    .string_literal = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .symbol_literal = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .symbol_list_literal = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .identifier = .{ .prefix = null, .infix = null, .prec = Precedence.none },

    // Misc.
    .comment = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .system = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .invalid = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .eof = .{ .prefix = null, .infix = null, .prec = Precedence.none },

    // Keywords
    .keyword_abs = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_acos = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_asin = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_atan = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_avg = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_bin = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_binr = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_cor = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_cos = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_cov = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_delete = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_dev = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_div = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_do = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_enlist = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_exec = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_exit = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_exp = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_getenv = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_hopen = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_if = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_in = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_insert = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_last = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_like = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_log = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_max = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_min = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_prd = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_select = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_setenv = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_sin = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_sqrt = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_ss = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_sum = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_tan = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_update = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_var = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_wavg = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_while = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_within = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_wsum = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_xexp = .{ .prefix = null, .infix = null, .prec = Precedence.none },
});

fn eatComments(p: *Parse) void {
    while (p.eatToken(.comment)) |_| {}
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
