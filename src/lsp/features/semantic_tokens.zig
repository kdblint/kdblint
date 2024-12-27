const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;

const zls = @import("zls");
const types = zls.types;

const kdb = @import("kdb");
const Ast = kdb.Ast;

const Analyser = @import("../analysis.zig");
const DocumentStore = @import("../DocumentStore.zig");
const offsets = @import("../offsets.zig");
const ast = @import("../ast.zig");

pub const TokenType = enum(u32) {
    namespace,
    type,
    class,
    @"enum",
    interface,
    @"struct",
    typeParameter,
    parameter,
    variable,
    property,
    enumMember,
    event,
    function,
    method,
    macro,
    keyword,
    modifier,
    comment,
    string,
    number,
    regexp,
    operator,
    decorator,
    /// non standard token type
    symbol,
};

pub const TokenModifiers = packed struct(u16) {
    declaration: bool = false,
    definition: bool = false,
    readonly: bool = false,
    static: bool = false,
    deprecated: bool = false,
    abstract: bool = false,
    @"async": bool = false,
    modification: bool = false,
    documentation: bool = false,
    defaultLibrary: bool = false,
    _: u6 = 0,
};

const Builder = struct {
    arena: std.mem.Allocator,
    analyser: *Analyser,
    handle: *DocumentStore.Handle,
    previous_source_index: usize = 0,
    source_index: usize = 0,
    token_buffer: std.ArrayListUnmanaged(u32) = .{},
    encoding: offsets.Encoding,
    limited: bool,

    fn add(
        self: *Builder,
        token: Ast.Token.Index,
        token_type: TokenType,
        token_modifiers: TokenModifiers,
    ) !void {
        const tree = self.handle.tree;
        const token_locs: []Ast.Token.Loc = tree.tokens.items(.loc);

        try self.handleComments(self.previous_source_index, token_locs[token].start);
        try self.addDirect(
            token_type,
            token_modifiers,
            offsets.tokenToLoc(tree, token),
        );
    }

    fn finish(self: *Builder) !types.SemanticTokens {
        try self.handleComments(self.previous_source_index, self.handle.tree.source.len);
        return .{ .data = try self.token_buffer.toOwnedSlice(self.arena) };
    }

    /// Highlight normal comments and doc comments.
    fn handleComments(self: *Builder, from: usize, to: usize) !void {
        if (from >= to) return;

        const source = self.handle.tree.source;

        var i: usize = from;
        while (i < to) : (i += 1) {
            // Skip multi-line string literals
            if (source[i] == '\\' and source[i + 1] == '\\') {
                while (i < to and source[i] != '\n') : (i += 1) {}
                continue;
            }
            // Skip normal string literals
            if (source[i] == '"') {
                i += 1;
                while (i < to and
                    source[i] != '\n' and
                    !(source[i] == '"' and source[i - 1] != '\\')) : (i += 1)
                {}
                continue;
            }
            // Skip char literals
            if (source[i] == '\'') {
                i += 1;
                while (i < to and
                    source[i] != '\n' and
                    !(source[i] == '\'' and source[i - 1] != '\\')) : (i += 1)
                {}
                continue;
            }

            if (source[i] != '/' or source[i + 1] != '/')
                continue;

            const comment_start = i;
            var mods = TokenModifiers{};
            if (i + 2 < to and (source[i + 2] == '!' or source[i + 2] == '/'))
                mods.documentation = true;

            while (i < to and source[i] != '\n') : (i += 1) {}

            try self.addDirect(.comment, mods, .{ .start = comment_start, .end = i });
        }
    }

    fn addDirect(self: *Builder, token_type: TokenType, token_modifiers: TokenModifiers, loc: offsets.Loc) !void {
        assert(loc.start <= loc.end);
        assert(self.previous_source_index <= self.source_index);
        if (loc.start < self.previous_source_index) return;
        if (loc.start < self.source_index) return;
        switch (token_type) {
            .namespace,
            .type,
            .class,
            .@"enum",
            .interface,
            .@"struct",
            .typeParameter,
            .parameter,
            .variable,
            .property,
            .enumMember,
            .event,
            .function,
            .method,
            .macro,
            .modifier,
            .regexp,
            .decorator,
            => {},

            .keyword,
            .comment,
            .string,
            .number,
            .operator,
            .symbol,
            => if (self.limited) return,
        }

        const source = self.handle.tree.source;
        const delta_text = source[self.previous_source_index..loc.start];
        const delta = offsets.indexToPosition(delta_text, delta_text.len, self.encoding);
        const length = offsets.locLength(source, loc, self.encoding);

        // assert that the `@intCast(length)` below is safe
        comptime assert(DocumentStore.max_document_size == std.math.maxInt(u32));

        try self.token_buffer.appendSlice(self.arena, &.{
            delta.line,
            delta.character,
            @intCast(length),
            @intFromEnum(token_type),
            @as(u16, @bitCast(token_modifiers)),
        });
        self.previous_source_index = loc.start;
        self.source_index = loc.end;
    }

    fn writeToken(builder: *Builder, token_idx: ?Ast.Token.Index, tok_type: TokenType) !void {
        return try writeTokenMod(builder, token_idx, tok_type, .{});
    }

    fn writeTokenMod(
        builder: *Builder,
        token_idx: ?Ast.Token.Index,
        tok_type: TokenType,
        tok_mod: TokenModifiers,
    ) !void {
        if (token_idx) |ti| {
            try builder.add(ti, tok_type, tok_mod);
        }
    }

    fn writeNodeTokens(builder: *Builder, node: Ast.Node.Index) !void {
        if (node == 0) return;

        const handle = builder.handle;
        const tree = handle.tree;
        const node_tags: []Ast.Node.Tag = tree.nodes.items(.tag);
        const node_datas: []Ast.Node.Data = tree.nodes.items(.data);
        const main_tokens: []Ast.Token.Index = tree.nodes.items(.main_token);

        const tag = node_tags[node];
        const main_token = main_tokens[node];

        switch (tag) {
            .root => unreachable,

            .empty => {},

            .grouped_expression => try builder.writeNodeTokens(node_datas[node].lhs),

            .empty_list => {},

            .list => {
                const data = node_datas[node];
                const sub_range = tree.extraData(data.lhs, Ast.Node.SubRange);
                const elems = tree.extra_data[sub_range.start..sub_range.end];
                for (elems) |elem_node| try builder.writeNodeTokens(elem_node);
            },

            .table_literal => {
                const data = node_datas[node];
                const table = tree.extraData(data.lhs, Ast.Node.Table);
                const keys = tree.extra_data[table.keys_start..table.keys_end];
                for (keys) |key_node| try builder.writeNodeTokens(key_node);
                const columns = tree.extra_data[table.columns_start..table.columns_end];
                for (columns) |column_node| try builder.writeNodeTokens(column_node);
            },

            .lambda,
            .lambda_semicolon,
            => {
                const lambda = tree.fullLambda(node);
                if (lambda.params) |p| {
                    for (p.params) |param| try builder.writeNodeTokens(param);
                }
                for (lambda.body) |body_node| try builder.writeNodeTokens(body_node);
            },

            .expr_block => {
                const data = node_datas[node];
                if (data.lhs != 0) {
                    const sub_range = tree.extraData(data.lhs, Ast.Node.SubRange);
                    const nodes = tree.extra_data[sub_range.start..sub_range.end];
                    for (nodes) |block_node| try builder.writeNodeTokens(block_node);
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
            => try builder.writeToken(main_token, .operator),

            .apostrophe,
            .apostrophe_colon,
            .slash,
            .slash_colon,
            .backslash,
            .backslash_colon,
            => {
                try builder.writeNodeTokens(node_datas[node].lhs);
                try builder.writeToken(main_token, .operator);
            },

            .call => {
                const call = tree.fullCall(node);
                switch (node_tags[call.func]) {
                    .identifier => {
                        const ident_token = main_tokens[call.func];
                        const ident_bytes = tree.tokenSlice(ident_token);
                        if (std.mem.eql(u8, ident_bytes, "do") or
                            std.mem.eql(u8, ident_bytes, "if") or
                            std.mem.eql(u8, ident_bytes, "while"))
                        {
                            try builder.writeToken(ident_token, .keyword);
                        } else {
                            try builder.writeNodeTokens(call.func);
                        }
                    },
                    else => try builder.writeNodeTokens(call.func),
                }
                for (call.args) |arg_node| try builder.writeNodeTokens(arg_node);
            },

            .apply_unary => {
                const data = node_datas[node];
                try builder.writeNodeTokens(data.lhs);
                try builder.writeNodeTokens(data.rhs);
            },

            .apply_binary => {
                const data = node_datas[node];
                try builder.writeNodeTokens(data.lhs);
                try builder.writeNodeTokens(main_token);
                if (data.rhs != 0) try builder.writeNodeTokens(data.rhs);
            },

            .number_literal => try builder.writeToken(main_token, .number),

            .number_list_literal => for (main_token..node_datas[node].lhs + 1) |literal_token| {
                try builder.writeToken(@intCast(literal_token), .number);
            },

            .string_literal => try builder.writeToken(main_token, .string),

            .symbol_literal => try builder.writeToken(main_token, .symbol),

            .symbol_list_literal => for (main_token..node_datas[node].lhs + 1) |literal_token| {
                try builder.writeToken(@intCast(literal_token), .symbol);
            },

            .identifier => try builder.writeIdentifier(main_token),

            .builtin => try builder.writeToken(main_token, .keyword),

            .select => {
                const select = tree.fullSelect(node);

                try builder.writeToken(select.select_token, .keyword);

                if (select.limit) |limit| {
                    if (limit.expr) |expr_node| try builder.writeNodeTokens(expr_node);
                    if (limit.order_column) |order_column| {
                        try builder.writeToken(order_column - 1, .operator);
                        try builder.writeToken(order_column, .variable);
                    }
                } else if (select.distinct_token) |token| try builder.writeToken(
                    token,
                    .keyword,
                );

                for (select.select) |select_node| try builder.writeNodeTokens(select_node);

                if (select.by) |by| {
                    try builder.writeToken(by.by_token, .keyword);
                    for (by.exprs) |by_node| try builder.writeNodeTokens(by_node);
                }

                try builder.writeToken(select.from_token, .keyword);
                try builder.writeNodeTokens(select.from);

                if (select.where) |where| {
                    try builder.writeToken(where.where_token, .keyword);
                    for (where.exprs) |where_node| try builder.writeNodeTokens(where_node);
                }
            },

            .exec => {
                const exec = tree.fullExec(node);

                try builder.writeToken(exec.exec_token, .keyword);

                for (exec.select) |select_node| try builder.writeNodeTokens(select_node);

                if (exec.by) |by| {
                    try builder.writeToken(by.by_token, .keyword);
                    for (by.exprs) |by_node| try builder.writeNodeTokens(by_node);
                }

                try builder.writeToken(exec.from_token, .keyword);
                try builder.writeNodeTokens(exec.from);

                if (exec.where) |where| {
                    try builder.writeToken(where.where_token, .keyword);
                    for (where.exprs) |where_node| try builder.writeNodeTokens(where_node);
                }
            },

            .update => {
                const update = tree.fullUpdate(node);

                try builder.writeToken(update.update_token, .keyword);

                for (update.select) |select_node| try builder.writeNodeTokens(select_node);

                if (update.by) |by| {
                    try builder.writeToken(by.by_token, .keyword);
                    for (by.exprs) |by_node| try builder.writeNodeTokens(by_node);
                }

                try builder.writeToken(update.from_token, .keyword);
                try builder.writeNodeTokens(update.from);

                if (update.where) |where| {
                    try builder.writeToken(where.where_token, .keyword);
                    for (where.exprs) |where_node| try builder.writeNodeTokens(where_node);
                }
            },

            .delete_rows => {
                const delete = tree.fullDeleteRows(node);

                try builder.writeToken(delete.delete_token, .keyword);

                try builder.writeToken(delete.from_token, .keyword);
                try builder.writeNodeTokens(delete.from);

                if (delete.where) |where| {
                    try builder.writeToken(where.where_token, .keyword);
                    for (where.exprs) |where_node| try builder.writeNodeTokens(where_node);
                }
            },

            .delete_cols => {
                const delete = tree.fullDeleteCols(node);

                try builder.writeToken(delete.delete_token, .keyword);

                for (delete.select_tokens) |token| try builder.writeToken(token, .variable);

                try builder.writeToken(delete.from_token, .keyword);
                try builder.writeNodeTokens(delete.from);
            },
        }
    }

    fn writeIdentifier(builder: *Builder, token: Ast.Token.Index) !void {
        const handle = builder.handle;
        const tree = handle.tree;

        const name = tree.tokenSlice(token);
        if (name[0] == '.') {
            if (std.mem.startsWith(u8, name, ".z.")) {
                const callback = name[".z.".len..];
                if (switch (callback.len) {
                    2 => std.mem.eql(u8, callback, "bm") or
                        std.mem.eql(u8, callback, "pc") or
                        std.mem.eql(u8, callback, "pd") or
                        std.mem.eql(u8, callback, "pg") or
                        std.mem.eql(u8, callback, "pg") or
                        std.mem.eql(u8, callback, "pi") or
                        std.mem.eql(u8, callback, "po") or
                        std.mem.eql(u8, callback, "pq") or
                        std.mem.eql(u8, callback, "ps") or
                        std.mem.eql(u8, callback, "pw") or
                        std.mem.eql(u8, callback, "ts") or
                        std.mem.eql(u8, callback, "vs") or
                        std.mem.eql(u8, callback, "ac") or
                        std.mem.eql(u8, callback, "ph") or
                        std.mem.eql(u8, callback, "pm") or
                        std.mem.eql(u8, callback, "pp") or
                        std.mem.eql(u8, callback, "wc") or
                        std.mem.eql(u8, callback, "wo") or
                        std.mem.eql(u8, callback, "ws"),
                    4 => std.mem.eql(u8, callback, "exit"),
                    else => false,
                }) return builder.writeToken(token, .macro);
            }

            return builder.writeToken(token, .enumMember);
        }

        if (try builder.analyser.lookupSymbolLocal(
            handle,
            name,
            tree.tokens.items(.loc)[token].start,
        )) |_| {
            try builder.writeToken(token, .variable);
        }

        try builder.writeToken(token, .enumMember);
    }
};

/// If `loc` is `null`, semantic tokens will be computed for the entire source range
/// Otherwise only tokens in the give source range will be returned
/// TODO edit version.
pub fn writeSemanticTokens(
    arena: Allocator,
    analyser: *Analyser,
    handle: *DocumentStore.Handle,
    loc: ?offsets.Loc,
    encoding: offsets.Encoding,
    limited: bool,
) error{OutOfMemory}!types.SemanticTokens {
    var builder = Builder{
        .arena = arena,
        .analyser = analyser,
        .handle = handle,
        .encoding = encoding,
        .limited = limited,
    };

    const nodes = if (loc) |l|
        try ast.nodesAtLoc(arena, handle.tree, l)
    else
        handle.tree.getBlocks();

    // reverse the ast from the root declarations
    for (nodes) |child| {
        try builder.writeNodeTokens(child);
    }

    return builder.finish();
}

test {
    @import("std").testing.refAllDecls(@This());
}
