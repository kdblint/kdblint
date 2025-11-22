const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const lsp = @import("lsp");
const types = lsp.types;
const offsets = lsp.offsets;

const kdb = @import("../../kdb/root.zig");
const Ast = kdb.Ast;
const DocumentStore = @import("../DocumentStore.zig");

pub const TokenType = enum(u32) {
    keyword,
    comment,
    string,
    number,
    operator,
    /// non-standard token type
    symbol,
    /// non-standard token type
    local,
    /// non-standard token type
    global,
};

const Builder = struct {
    arena: Allocator,
    handle: *DocumentStore.Handle,
    previous_source_index: usize = 0,
    source_index: usize = 0,
    encoding: offsets.Encoding,
    overlapping_token_support: bool,
    token_buffer: std.ArrayList(u32) = .empty,

    fn add(builder: *Builder, token: Ast.TokenIndex, token_type: TokenType) !void {
        try builder.addDirect(token_type, builder.handle.tree.tokenLoc(token));
    }

    fn addDirect(self: *Builder, token_type: TokenType, loc: Ast.Token.Loc) !void {
        assert(loc.start <= loc.end);
        assert(self.previous_source_index <= self.source_index);
        if (loc.start < self.previous_source_index) return;
        if (!self.overlapping_token_support and loc.start < self.source_index) return;

        const source = self.handle.tree.source;
        const delta_text = source[self.previous_source_index..loc.start];
        const delta = offsets.indexToPosition(delta_text, delta_text.len, self.encoding);
        const length: u32 = @intCast(offsets.locLength(source, .{ .start = loc.start, .end = loc.end }, self.encoding));

        try self.token_buffer.appendSlice(self.arena, &.{
            delta.line,
            delta.character,
            length,
            @intFromEnum(token_type),
            0,
        });
        self.previous_source_index = loc.start;
        self.source_index = loc.end;
    }

    fn writeToken(self: *Builder, token: Ast.TokenIndex, token_type: TokenType) !void {
        try self.add(token, token_type);
    }

    fn writeNode(self: *Builder, node: Ast.Node.Index) !void {
        const handle = self.handle;
        const tree = handle.tree;

        switch (tree.nodeTag(node)) {
            .root => unreachable,
            .empty => {},

            .grouped_expression => try self.writeNode(tree.nodeData(node).node_and_token[0]),
            .empty_list => {},
            .list => {
                const nodes = tree.extraDataSlice(tree.nodeData(node).extra_range, Ast.Node.Index);
                for (nodes) |n| try self.writeNode(n);
            },
            .table_literal => std.log.debug("NYI: {t}", .{tree.nodeTag(node)}),

            .lambda => {
                const lambda = tree.extraData(tree.nodeData(node).extra_and_token[0], Ast.Node.Lambda);
                const params = tree.extraDataSlice(.{
                    .start = lambda.params_start,
                    .end = lambda.params_end,
                }, Ast.Node.Index);
                const body = tree.extraDataSlice(.{
                    .start = lambda.body_start,
                    .end = lambda.body_end,
                }, Ast.Node.Index);

                for (params) |n| try self.writeNode(n);
                for (body) |n| try self.writeNode(n);
            },

            .expr_block => {
                const nodes = tree.extraDataSlice(tree.nodeData(node).extra_range, Ast.Node.Index);
                for (nodes) |n| try self.writeNode(n);
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
            => try self.writeToken(tree.nodeMainToken(node), .operator),

            .apostrophe,
            .apostrophe_colon,
            .slash,
            .slash_colon,
            .backslash,
            .backslash_colon,
            => try self.writeToken(tree.nodeMainToken(node), .operator),

            .call => {
                const nodes = tree.extraDataSlice(tree.nodeData(node).extra_range, Ast.Node.Index);
                for (nodes) |n| try self.writeNode(n);
            },
            .apply_unary => {
                const lhs, const rhs = tree.nodeData(node).node_and_node;
                try self.writeNode(lhs);
                try self.writeNode(rhs);
            },
            .apply_binary => {
                const lhs, const maybe_rhs = tree.nodeData(node).node_and_opt_node;
                try self.writeNode(lhs);
                if (maybe_rhs.unwrap()) |rhs| try self.writeNode(rhs);
            },

            .number_literal => try self.writeToken(tree.nodeMainToken(node), .number),
            .number_list_literal => {
                const first_token = tree.nodeMainToken(node);
                const last_token = tree.nodeData(node).token;
                for (first_token..last_token + 1) |token| try self.writeToken(@intCast(token), .number);
            },
            .string_literal => try self.writeToken(tree.nodeMainToken(node), .string),
            .symbol_literal => try self.writeToken(tree.nodeMainToken(node), .symbol),
            .symbol_list_literal => {
                const first_token = tree.nodeMainToken(node);
                const last_token = tree.nodeData(node).token;
                for (first_token..last_token + 1) |token| try self.writeToken(@intCast(token), .symbol);
            },
            .identifier => {
                const main_token = tree.nodeMainToken(node);
                if (tree.tokenSlice(main_token)[0] == '.') {
                    try self.writeToken(tree.nodeMainToken(node), .global);
                } else {
                    // TODO: Use ZIR to determine local/global
                    const zir = try self.handle.getZir(self.arena);
                    _ = zir; // autofix
                    try self.writeToken(tree.nodeMainToken(node), .local);
                }
            },
            .builtin => try self.writeToken(tree.nodeMainToken(node), .keyword),
            .system => std.log.debug("NYI: {t}", .{tree.nodeTag(node)}),
            .dsl => std.log.debug("NYI: {t}", .{tree.nodeTag(node)}),

            .select => std.log.debug("NYI: {t}", .{tree.nodeTag(node)}),
            .exec => std.log.debug("NYI: {t}", .{tree.nodeTag(node)}),
            .update => std.log.debug("NYI: {t}", .{tree.nodeTag(node)}),
            .delete_rows => std.log.debug("NYI: {t}", .{tree.nodeTag(node)}),
            .delete_cols => std.log.debug("NYI: {t}", .{tree.nodeTag(node)}),
        }
    }

    fn finish(self: *Builder) !types.semantic_tokens.Result {
        return .{ .data = try self.token_buffer.toOwnedSlice(self.arena) };
    }
};

pub fn writeSemanticTokens(
    arena: Allocator,
    handle: *DocumentStore.Handle,
    encoding: offsets.Encoding,
    overlapping_token_support: bool,
) !types.semantic_tokens.Result {
    var builder: Builder = .{
        .arena = arena,
        .handle = handle,
        .encoding = encoding,
        .overlapping_token_support = overlapping_token_support,
    };

    for (handle.tree.getBlocks()) |block| {
        try builder.writeNode(block);
    }

    return try builder.finish();
}
