const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const StringIndexAdapter = std.hash_map.StringIndexAdapter;
const StringIndexContext = std.hash_map.StringIndexContext;
const Timer = std.time.Timer;

const kdb = @import("root.zig");
const Ast = kdb.Ast;
const Zir = kdb.Zir;

const AstGen = @This();

gpa: Allocator,
arena: Allocator,
tree: *const Ast,
instructions: std.MultiArrayList(Zir.Inst) = .empty,
extra: std.ArrayListUnmanaged(u32) = .empty,
string_bytes: std.ArrayListUnmanaged(u8) = .empty,
/// Tracks the current byte offset within the source file.
/// Used to populate line deltas in the ZIR. AstGen maintains
/// this "cursor" throughout the entire AST lowering process in order
/// to avoid starting over the line/column scan for every declaration, which
/// would be O(N^2).
source_offset: u32 = 0,
/// Tracks the corresponding line of `source_offset`.
/// This value is absolute.
source_line: u32 = 0,
/// Tracks the corresponding column of `source_offset`.
/// This value is absolute.
source_column: u32 = 0,
string_table: std.HashMapUnmanaged(
    u32,
    void,
    StringIndexContext,
    std.hash_map.default_max_load_percentage,
) = .empty,
compile_errors: std.ArrayListUnmanaged(Zir.Inst.CompileErrors.Item) = .empty,
compile_warnings: std.ArrayListUnmanaged(Zir.Inst.CompileErrors.Item) = .empty,
/// Maps string table indexes to the first `@import` ZIR instruction
/// that uses this string as the operand.
imports: std.AutoArrayHashMapUnmanaged(Zir.NullTerminatedString, Ast.Token.Index) = .empty,
/// Used for temporary storage when building payloads.
scratch: std.ArrayListUnmanaged(u32) = .empty,
/// Whenever a `ref` instruction is needed, it is created and saved in this
/// table instead of being immediately appended to the current block body.
/// Then, when the instruction is being added to the parent block (typically from
/// setBlockBody), if it has a ref_table entry, then the ref instruction is added
/// there. This makes sure two properties are upheld:
/// 1. All pointers to the same locals return the same address. This is required
///    to be compliant with the language specification.
/// 2. `ref` instructions will dominate their uses. This is a required property
///    of ZIR.
/// The key is the ref operand; the value is the ref instruction.
ref_table: std.AutoHashMapUnmanaged(Zir.Inst.Index, Zir.Inst.Index) = .empty,

const InnerError = error{ OutOfMemory, AnalysisFail };

fn addExtra(astgen: *AstGen, extra: anytype) Allocator.Error!u32 {
    const fields = std.meta.fields(@TypeOf(extra));
    try astgen.extra.ensureUnusedCapacity(astgen.gpa, fields.len);
    return addExtraAssumeCapacity(astgen, extra);
}

fn addExtraAssumeCapacity(astgen: *AstGen, extra: anytype) u32 {
    const fields = std.meta.fields(@TypeOf(extra));
    const extra_index: u32 = @intCast(astgen.extra.items.len);
    astgen.extra.items.len += fields.len;
    setExtra(astgen, extra_index, extra);
    return extra_index;
}

fn setExtra(astgen: *AstGen, index: usize, extra: anytype) void {
    const fields = std.meta.fields(@TypeOf(extra));
    var i = index;
    inline for (fields) |field| {
        astgen.extra.items[i] = switch (field.type) {
            u32 => @field(extra, field.name),

            Zir.Inst.Ref,
            Zir.Inst.Index,
            Zir.NullTerminatedString,
            Zir.Inst.CompileErrors.Kind,
            => @intFromEnum(@field(extra, field.name)),

            i32,
            => @bitCast(@field(extra, field.name)),

            else => |t| @compileError("bad field type: " ++ @typeName(t)),
        };
        i += 1;
    }
}

pub fn generate(gpa: Allocator, tree: Ast) !Zir {
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();

    var astgen: AstGen = .{
        .gpa = gpa,
        .arena = arena.allocator(),
        .tree = &tree,
    };
    defer astgen.deinit(gpa);

    // String table index 0 is reserved for `NullTerminatedString.empty`.
    try astgen.string_bytes.append(gpa, 0);

    // We expect at least as many ZIR instructions and extra data items
    // as AST nodes.
    try astgen.instructions.ensureTotalCapacity(gpa, tree.nodes.len);

    // First few indexes of extra are reserved and set at the end.
    const reserved_count = @typeInfo(Zir.ExtraIndex).@"enum".fields.len;
    try astgen.extra.ensureTotalCapacity(gpa, tree.nodes.len + reserved_count);
    astgen.extra.items.len += reserved_count;

    var top_scope: Scope.Top = .{};
    defer top_scope.deinit(gpa);

    var gz_instructions: std.ArrayListUnmanaged(Zir.Inst.Index) = .empty;
    defer gz_instructions.deinit(gpa);
    var gen_scope: GenZir = .{
        .decl_node_index = 0,
        .decl_line = 0,
        .parent = &top_scope.base,
        .astgen = &astgen,
        .instructions = &gz_instructions,
        .instructions_top = 0,
    };

    const compile_duration: u64 = compile: {
        var timer = Timer.start() catch null;
        // The AST -> ZIR lowering process assumes an AST that does not have any
        // parse errors.
        if (tree.errors.len == 0) {
            if (file(&gen_scope, &gen_scope.base)) |file_inst| {
                assert(file_inst.toIndex().? == .file_inst);
            } else |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                error.AnalysisFail => {}, // Handled via compile_errors below.
            }
        } else {
            try astgen.lowerAstErrors();
        }
        break :compile if (timer) |*t| t.read() else 0;
    };

    const err_index = @intFromEnum(Zir.ExtraIndex.compile_errors);
    if (astgen.compile_errors.items.len == 0) {
        astgen.extra.items[err_index] = 0;
    } else {
        try astgen.extra.ensureUnusedCapacity(gpa, 1 + astgen.compile_errors.items.len *
            @typeInfo(Zir.Inst.CompileErrors.Item).@"struct".fields.len);

        astgen.extra.items[err_index] = astgen.addExtraAssumeCapacity(Zir.Inst.CompileErrors{
            .items_len = @intCast(astgen.compile_errors.items.len),
        });

        for (astgen.compile_errors.items) |item| {
            _ = astgen.addExtraAssumeCapacity(item);
        }
    }

    const warn_index = @intFromEnum(Zir.ExtraIndex.compile_warnings);
    if (astgen.compile_warnings.items.len == 0) {
        astgen.extra.items[warn_index] = 0;
    } else {
        try astgen.extra.ensureUnusedCapacity(gpa, 1 + astgen.compile_warnings.items.len *
            @typeInfo(Zir.Inst.CompileErrors.Item).@"struct".fields.len);

        astgen.extra.items[warn_index] = astgen.addExtraAssumeCapacity(Zir.Inst.CompileErrors{
            .items_len = @intCast(astgen.compile_warnings.items.len),
        });

        for (astgen.compile_warnings.items) |item| {
            _ = astgen.addExtraAssumeCapacity(item);
        }
    }

    const imports_index = @intFromEnum(Zir.ExtraIndex.imports);
    if (astgen.imports.count() == 0) {
        astgen.extra.items[imports_index] = 0;
    } else {
        try astgen.extra.ensureUnusedCapacity(gpa, @typeInfo(Zir.Inst.Imports).@"struct".fields.len +
            astgen.imports.count() * @typeInfo(Zir.Inst.Imports.Item).@"struct".fields.len);

        astgen.extra.items[imports_index] = astgen.addExtraAssumeCapacity(Zir.Inst.Imports{
            .imports_len = @intCast(astgen.imports.count()),
        });

        var it = astgen.imports.iterator();
        while (it.next()) |entry| {
            _ = astgen.addExtraAssumeCapacity(Zir.Inst.Imports.Item{
                .name = entry.key_ptr.*,
                .token = entry.value_ptr.*,
            });
        }
    }

    return Zir{
        .instructions = astgen.instructions.toOwnedSlice(),
        .string_bytes = try astgen.string_bytes.toOwnedSlice(gpa),
        .extra = try astgen.extra.toOwnedSlice(gpa),
        .compile_duration = compile_duration,
    };
}

fn deinit(astgen: *AstGen, gpa: Allocator) void {
    astgen.instructions.deinit(gpa);
    astgen.extra.deinit(gpa);
    astgen.string_table.deinit(gpa);
    astgen.string_bytes.deinit(gpa);
    astgen.compile_errors.deinit(gpa);
    astgen.compile_warnings.deinit(gpa);
    astgen.imports.deinit(gpa);
    astgen.scratch.deinit(gpa);
    astgen.ref_table.deinit(gpa);
}

fn file(gz: *GenZir, parent_scope: *Scope) InnerError!Zir.Inst.Ref {
    const astgen = gz.astgen;
    const tree = astgen.tree;
    var scope = parent_scope;

    assert(parent_scope.isGlobal());

    const file_inst = try gz.makeFile();

    const blocks = tree.getBlocks();
    try astgen.scanExprs(blocks, .{
        .global_decls = &parent_scope.top().global_decls,
    });
    for (blocks, 0..) |node, i| {
        if (gz.endsWithNoReturn()) {
            assert(i > 0);
            try gz.astgen.appendErrorNodeNotes(node, "unreachable code", .{}, &.{
                try gz.astgen.errNoteNode(blocks[i - 1], "control flow is diverted here", .{}),
            }, .warn);
        }

        // TODO: Namespace block.
        // TODO: Something should wrap expr to return an index to show or discard value.
        const expr_ref, scope = expr(gz, scope, node) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.AnalysisFail => continue,
        };
        _ = expr_ref;
    }
    try gz.setFile(file_inst);

    return file_inst.toRef();
}

const Result = struct { Zir.Inst.Ref, *Scope };

fn expr(gz: *GenZir, scope: *Scope, src_node: Ast.Node.Index) InnerError!Result {
    const astgen = gz.astgen;
    const tree = astgen.tree;
    const node_tags: []Ast.Node.Tag = tree.nodes.items(.tag);

    assert(src_node != 0);

    const node = tree.unwrapGroupedExpr(src_node);
    switch (node_tags[node]) {
        .root => unreachable,
        .empty => return .{ .null, scope },

        .grouped_expression => unreachable,
        .empty_list => return .{ .empty_list, scope },
        .list => return listExpr(gz, scope, node),

        .lambda,
        .lambda_semicolon,
        => return lambda(gz, scope, node),

        .expr_block => return exprBlock(gz, scope, node),

        .colon, .colon_colon => return .{ .assign, scope },
        .plus, .plus_colon => return .{ .add, scope },
        .minus, .minus_colon => return .{ .subtract, scope },
        .asterisk, .asterisk_colon => return .{ .multiply, scope },
        .percent, .percent_colon => return .{ .divide, scope },
        .ampersand, .ampersand_colon => return .{ .lesser, scope },
        .pipe, .pipe_colon => return .{ .greater, scope },
        .caret, .caret_colon => return .{ .fill, scope },
        .equal, .equal_colon => return .{ .equal, scope },
        .angle_bracket_left, .angle_bracket_left_colon => return .{ .less_than, scope },
        .angle_bracket_left_equal => return .{ .less_than_or_equal, scope },
        .angle_bracket_left_right => return .{ .not_equal, scope },
        .angle_bracket_right, .angle_bracket_right_colon => return .{ .greater_than, scope },
        .angle_bracket_right_equal => return .{ .greater_than_or_equal, scope },
        .dollar, .dollar_colon => return .{ .cast, scope },
        .comma, .comma_colon => return .{ .join, scope },
        .hash, .hash_colon => return .{ .take, scope },
        .underscore, .underscore_colon => return .{ .drop, scope },
        .tilde, .tilde_colon => return .{ .match, scope },
        .bang, .bang_colon => return .{ .dict, scope },
        .question_mark, .question_mark_colon => return .{ .find, scope },
        .at, .at_colon => return .{ .apply_at, scope },
        .period, .period_colon => return .{ .apply_dot, scope },
        .zero_colon, .zero_colon_colon => return .{ .file_text, scope },
        .one_colon, .one_colon_colon => return .{ .file_binary, scope },
        .two_colon => return .{ .dynamic_load, scope },

        .apostrophe,
        .apostrophe_colon,
        .slash,
        .slash_colon,
        .backslash,
        .backslash_colon,
        => return iterator(gz, scope, node),

        .call => return call(gz, scope, node),
        .apply_unary => return applyUnary(gz, scope, node),
        .apply_binary => return applyBinary(gz, scope, node),

        .number_literal => return .{ try numberLiteral(gz, node), scope },
        .number_list_literal => return .{ try numberListLiteral(gz, node), scope },
        .string_literal => return .{ try stringLiteral(gz, node), scope },
        .symbol_literal => return .{ try symbolLiteral(gz, node), scope },
        .symbol_list_literal => return .{ try symbolListLiteral(gz, node), scope },
        .identifier => return identifier(gz, scope, node),
        .builtin => return .{ try builtin(gz, node), scope },

        .cond => return cond(gz, scope, node),

        else => |t| return astgen.failNode(node, "expr NYI: {s}", .{@tagName(t)}),
    }
}

fn listExpr(gz: *GenZir, parent_scope: *Scope, src_node: Ast.Node.Index) InnerError!Result {
    const astgen = gz.astgen;
    const gpa = astgen.gpa;
    const tree = astgen.tree;
    const node_tags: []Ast.Node.Tag = tree.nodes.items(.tag);
    const node_datas: []Ast.Node.Data = tree.nodes.items(.data);
    var scope = parent_scope;

    assert(node_tags[src_node] == .list);

    const data = node_datas[src_node];
    const sub_range = tree.extraData(data.lhs, Ast.Node.SubRange);
    const list_nodes = tree.extra_data[sub_range.start..sub_range.end];

    const list = try gpa.alloc(Zir.Inst.Ref, list_nodes.len);
    defer gpa.free(list);

    var i: usize = 1;
    var it = std.mem.reverseIterator(list_nodes);
    while (it.next()) |node| : (i += 1) {
        list[list_nodes.len - i], scope = try expr(gz, scope, node);
    }

    const result = try gz.addPlNode(.list, src_node, Zir.Inst.List{
        .len = @intCast(list_nodes.len),
    });
    try gz.astgen.extra.appendSlice(gpa, @ptrCast(list));
    return .{ result, scope };
}

fn lambda(gz: *GenZir, scope: *Scope, node: Ast.Node.Index) InnerError!Result {
    const astgen = gz.astgen;
    const gpa = astgen.gpa;
    const tree = astgen.tree;
    const node_tags: []Ast.Node.Tag = tree.nodes.items(.tag);
    const main_tokens: []Ast.Token.Index = tree.nodes.items(.main_token);

    astgen.advanceSourceCursorToNode(node);
    const lbrace_line = astgen.source_line - gz.decl_line;
    const lbrace_column = astgen.source_column;

    const lambda_inst = try gz.makeLambda(node);

    const full_lambda = tree.fullLambda(node);

    var lambda_scope: Scope.Lambda = .{
        .parent = scope,
        .node = node,
        .inst = lambda_inst,
    };
    defer lambda_scope.deinit(gpa);

    var identifiers: std.StringHashMapUnmanaged(Ast.Node.Index) = .empty;
    defer identifiers.deinit(gpa);

    try astgen.scanExprs(full_lambda.body, .{
        .identifiers = &identifiers,
        .local_decls = &lambda_scope.local_decls,
        .global_decls = &lambda_scope.global_decls,
    });

    var fn_gz: GenZir = .{
        .decl_node_index = node,
        .decl_line = astgen.source_line,
        .parent = &lambda_scope.base,
        .astgen = astgen,
        .instructions = gz.instructions,
        .instructions_top = gz.instructions.items.len,
    };
    defer fn_gz.unstack();

    var params_scope = &fn_gz.base;
    const params_len: u32 = if (full_lambda.params) |p| params_len: {
        if (p.params.len > 8) return astgen.failNode(p.params[8], "too many parameters (8 max)", .{});

        for (p.params) |param_node| {
            if (node_tags[param_node] == .identifier) {
                const param_token = main_tokens[param_node];
                const param_bytes = tree.tokenSlice(main_tokens[param_node]);
                const param_name = try astgen.bytesAsString(param_bytes);

                if (params_scope.findLocal(param_name)) |local_val| {
                    try astgen.appendErrorNodeNotes(
                        param_node,
                        "redeclaration of function parameter '{s}'",
                        .{param_bytes},
                        &.{
                            try astgen.errNoteTok(
                                local_val.token_src,
                                "previous declaration here",
                                .{},
                            ),
                        },
                        .warn,
                    );
                } else {
                    try lambda_scope.local_decls.put(gpa, param_name, param_node);

                    const param_inst = try fn_gz.addStrNode(.param_node, param_name, param_node);

                    const sub_scope = try astgen.arena.create(Scope.LocalVal);
                    sub_scope.* = .{
                        .parent = params_scope,
                        .name = param_name,
                        .inst = param_inst,
                        .token_src = param_token,
                        .id_cat = .@"function parameter",
                    };
                    params_scope = &sub_scope.base;
                }
            } else {
                assert(node_tags[param_node] == .empty);
                assert(p.params.len == 1);
            }
        }

        break :params_len @intCast(p.params.len);
    } else params_len: {
        var found_x = false;
        var found_y = false;
        var found_z = false;

        // TODO: Unreferenced implicit parameter.

        const ident_x = identifiers.get("x");
        const ident_y = identifiers.get("y");
        const ident_z = identifiers.get("z");

        if (ident_x) |ident_node| {
            const ident_token = main_tokens[ident_node];
            const ident_name = try astgen.tokenAsString(ident_token);
            try lambda_scope.local_decls.put(gpa, ident_name, ident_node);
            found_x = true;

            const param_inst = try fn_gz.addUnTok(.param_implicit, .x, ident_token);
            const sub_scope = try astgen.arena.create(Scope.LocalVal);
            sub_scope.* = .{
                .parent = params_scope,
                .name = ident_name,
                .inst = param_inst,
                .token_src = ident_token,
                .used = ident_token,
                .id_cat = .@"function parameter",
            };
            params_scope = &sub_scope.base;
        } else if (ident_y != null or ident_z != null) {
            const param_inst = try fn_gz.addUnTok(
                .param_implicit,
                .x,
                full_lambda.l_brace,
            );
            const sub_scope = try astgen.arena.create(Scope.LocalVal);
            sub_scope.* = .{
                .parent = params_scope,
                .name = undefined, // TODO
                .inst = param_inst,
                .token_src = full_lambda.l_brace,
                .id_cat = .@"function parameter",
            };
            params_scope = &sub_scope.base;
        }

        if (ident_y) |ident_node| {
            const ident_token = main_tokens[ident_node];
            const ident_name = try astgen.tokenAsString(ident_token);
            try lambda_scope.local_decls.put(gpa, ident_name, ident_node);
            found_y = true;

            const param_inst = try fn_gz.addUnTok(.param_implicit, .y, ident_token);
            const sub_scope = try astgen.arena.create(Scope.LocalVal);
            sub_scope.* = .{
                .parent = params_scope,
                .name = ident_name,
                .inst = param_inst,
                .token_src = ident_token,
                .used = ident_token,
                .id_cat = .@"function parameter",
            };
            params_scope = &sub_scope.base;
        } else if (ident_z != null) {
            const param_inst = try fn_gz.addUnTok(
                .param_implicit,
                .y,
                full_lambda.l_brace,
            );
            const sub_scope = try astgen.arena.create(Scope.LocalVal);
            sub_scope.* = .{
                .parent = params_scope,
                .name = undefined, // TODO
                .inst = param_inst,
                .token_src = full_lambda.l_brace,
                .id_cat = .@"function parameter",
            };
            params_scope = &sub_scope.base;
        }

        if (identifiers.get("z")) |ident_node| {
            const ident_token = main_tokens[ident_node];
            const ident_name = try astgen.tokenAsString(ident_token);
            try lambda_scope.local_decls.put(gpa, ident_name, ident_node);
            found_z = true;

            const param_inst = try fn_gz.addUnTok(.param_implicit, .z, ident_token);
            const sub_scope = try astgen.arena.create(Scope.LocalVal);
            sub_scope.* = .{
                .parent = params_scope,
                .name = ident_name,
                .inst = param_inst,
                .token_src = ident_token,
                .used = ident_token,
                .id_cat = .@"function parameter",
            };
            params_scope = &sub_scope.base;
        }

        break :params_len if (found_z) 3 else if (found_y) 2 else 1;
    };

    assert(full_lambda.body.len > 0);

    for (full_lambda.body, 0..) |body_node, i| {
        if (fn_gz.endsWithNoReturn()) {
            assert(i > 0);
            try gz.astgen.appendErrorNodeNotes(body_node, "unreachable code", .{}, &.{
                try gz.astgen.errNoteNode(full_lambda.body[i - 1], "control flow is diverted here", .{}),
            }, .warn);
        }

        if (i < full_lambda.body.len - 1) {
            _, params_scope = try expr(&fn_gz, params_scope, body_node);
        } else if (node_tags[body_node] == .empty) {
            _ = try fn_gz.addUnTok(.ret_implicit, .null, full_lambda.r_brace);
        } else {
            const ref, params_scope = try expr(&fn_gz, params_scope, body_node);
            if (node_tags[node] == .lambda) {
                _ = try fn_gz.addUnNode(.ret_node, ref, body_node);
            } else {
                _ = try fn_gz.addUnTok(.ret_implicit, .null, full_lambda.r_brace);
            }
        }
    }

    try checkUsed(
        gz,
        &fn_gz.base,
        params_scope,
        full_lambda.params == null,
    );

    try gz.setLambda(lambda_inst, .{
        .lbrace_line = lbrace_line,
        .lbrace_column = lbrace_column,
        .rbrace = full_lambda.r_brace,
        .params_len = params_len,
        .body_gz = &fn_gz,
    });

    return .{ lambda_inst.toRef(), scope };
}

fn checkUsed(gz: *GenZir, outer_scope: *Scope, inner_scope: *Scope, implicit_params: bool) InnerError!void {
    const astgen = gz.astgen;
    const tree = astgen.tree;
    const token_tags: []Ast.Token.Tag = tree.tokens.items(.tag);

    var buf: [2]*Scope.LocalVal = undefined;
    var unused_params = std.ArrayListUnmanaged(*Scope.LocalVal).initBuffer(&buf);

    var last_param: ?*Scope.LocalVal = null;

    var scope = inner_scope;
    while (scope != outer_scope) {
        switch (scope.tag) {
            .gen_zir => scope = scope.cast(GenZir).?.parent,
            .local_val => {
                const s = scope.cast(Scope.LocalVal).?;
                if (s.id_cat == .@"function parameter" and last_param == null) last_param = s;

                if (s.used == 0) {
                    if (s.id_cat == .@"function parameter" and token_tags[s.token_src] == .l_brace) {
                        unused_params.appendAssumeCapacity(s);
                    } else if (s.id_cat != .@"global variable") {
                        try astgen.appendErrorTok(
                            s.token_src,
                            "unused {s}",
                            .{@tagName(s.id_cat)},
                            .warn,
                        );
                    }
                }

                scope = s.parent;
            },
            .lambda => break,
            .top => unreachable,
        }
    }

    if (implicit_params and unused_params.items.len > 0) {
        assert(last_param != null);
        if (last_param) |param_scope| {
            const last_param_tok = param_scope.token_src;
            const last_param_bytes = tree.tokenSlice(last_param_tok);
            if (std.mem.eql(u8, last_param_bytes, "z")) {
                const z_scope = param_scope;
                switch (unused_params.items.len) {
                    1 => {
                        const y_scope = z_scope.parent.cast(Scope.LocalVal).?;
                        if (unused_params.items[0] == y_scope) {
                            try astgen.appendErrorTokNotes(
                                y_scope.token_src,
                                "unused {s} 'y'",
                                .{@tagName(y_scope.id_cat)},
                                &.{
                                    try astgen.errNoteTok(
                                        z_scope.token_src,
                                        "consider renaming 'z' to 'y'",
                                        .{},
                                    ),
                                },
                                .warn,
                            );
                        } else {
                            const x_scope = unused_params.items[0];
                            try astgen.appendErrorTokNotes(
                                x_scope.token_src,
                                "unused {s} 'x'",
                                .{@tagName(x_scope.id_cat)},
                                &.{
                                    try astgen.errNoteTok(
                                        y_scope.token_src,
                                        "consider renaming 'y' to 'x'",
                                        .{},
                                    ),
                                    try astgen.errNoteTok(
                                        z_scope.token_src,
                                        "consider renaming 'z' to 'y'",
                                        .{},
                                    ),
                                },
                                .warn,
                            );
                        }
                    },
                    2 => {
                        const y_scope = z_scope.parent.cast(Scope.LocalVal).?;
                        const x_scope = y_scope.parent.cast(Scope.LocalVal).?;

                        try astgen.appendErrorTokNotes(
                            y_scope.token_src,
                            "unused {s} 'y'",
                            .{@tagName(y_scope.id_cat)},
                            &.{
                                try astgen.errNoteTok(
                                    z_scope.token_src,
                                    "consider renaming 'z' to 'x'",
                                    .{},
                                ),
                            },
                            .warn,
                        );
                        try astgen.appendErrorTokNotes(
                            x_scope.token_src,
                            "unused {s} 'x'",
                            .{@tagName(x_scope.id_cat)},
                            &.{
                                try astgen.errNoteTok(
                                    z_scope.token_src,
                                    "consider renaming 'z' to 'x'",
                                    .{},
                                ),
                            },
                            .warn,
                        );
                    },
                    else => unreachable,
                }
            } else if (std.mem.eql(u8, last_param_bytes, "y")) {
                const y_scope = param_scope;
                switch (unused_params.items.len) {
                    1 => {
                        const x_scope = unused_params.items[0];
                        try astgen.appendErrorTokNotes(
                            x_scope.token_src,
                            "unused {s} 'x'",
                            .{@tagName(x_scope.id_cat)},
                            &.{
                                try astgen.errNoteTok(
                                    y_scope.token_src,
                                    "consider renaming 'y' to 'x'",
                                    .{},
                                ),
                            },
                            .warn,
                        );
                    },
                    else => unreachable,
                }
            }
        }
    }
}

/// Given an index into `string_bytes` returns the null-terminated string found there.
pub fn nullTerminatedString(astgen: *AstGen, index: Zir.NullTerminatedString) [:0]const u8 {
    const slice = astgen.string_bytes.items[@intFromEnum(index)..];
    return slice[0..std.mem.indexOfScalar(u8, slice, 0).? :0];
}

fn exprBlock(gz: *GenZir, parent_scope: *Scope, node: Ast.Node.Index) InnerError!Result {
    const astgen = gz.astgen;
    const tree = astgen.tree;
    const node_datas: []Ast.Node.Data = tree.nodes.items(.data);

    const data = node_datas[node];
    var scope = parent_scope;
    if (data.lhs != 0) {
        const sub_range = tree.extraData(data.lhs, Ast.Node.SubRange);
        const slice = tree.extra_data[sub_range.start..sub_range.end];
        for (slice[0 .. slice.len - 1]) |n| {
            _, scope = try expr(gz, scope, n);
        }

        const n = slice[slice.len - 1];
        const inst, scope = try expr(gz, scope, n);
        if (tree.isCompoundAssignment(n)) return .{ .null, scope };
        return .{ inst, scope };
    }

    return .{ .null, scope };
}

fn signal(gz: *GenZir, parent_scope: *Scope, node: Ast.Node.Index) InnerError!Result {
    const astgen = gz.astgen;
    const tree = astgen.tree;
    const node_datas: []Ast.Node.Data = tree.nodes.items(.data);

    const data = node_datas[node];
    var scope = parent_scope;
    const rhs, scope = try expr(gz, scope, data.rhs);

    return .{ try gz.addUnNode(.signal, rhs, node), scope };
}

fn assign(
    gz: *GenZir,
    parent_scope: *Scope,
    src_node: Ast.Node.Index,
    op_node: Ast.Node.Index,
    ident_node: Ast.Node.Index,
    expr_node: Ast.Node.Index,
    is_global_assign: bool,
) InnerError!Result {
    const astgen = gz.astgen;
    const tree = astgen.tree;
    const node_tags: []Ast.Node.Tag = tree.nodes.items(.tag);
    const node_datas: []Ast.Node.Data = tree.nodes.items(.data);
    const main_tokens: []Ast.Token.Index = tree.nodes.items(.main_token);
    var scope = parent_scope;

    assert(src_node != 0);
    assert(ident_node != 0);
    assert(node_tags[ident_node] == .identifier);
    assert(expr_node != 0);

    const rhs, scope = try expr(gz, scope, expr_node);
    const op, scope = try expr(gz, scope, op_node);

    const ident_token = main_tokens[ident_node];
    const ident_bytes = tree.tokenSlice(ident_token);
    const ident_name = try astgen.bytesAsString(ident_bytes);

    if (parent_scope.isGlobal()) {
        const node = parent_scope.top().global_decls.get(ident_name).?;
        if (src_node == node) { // declare global
            const lhs = try gz.addStrTok(.identifier, ident_name, ident_token);

            const sub_scope = try astgen.arena.create(Scope.LocalVal);
            sub_scope.* = .{
                .parent = scope,
                .inst = lhs,
                .token_src = ident_token,
                .name = ident_name,
                .id_cat = .@"global variable",
            };
            scope = &sub_scope.base;

            return .{ try gz.addApply(src_node, op, &.{ lhs, rhs }), scope };
        } else if (scope.findGlobal(ident_name)) |lhs| { // assign global
            return .{ try gz.addApply(src_node, op, &.{ lhs.inst, rhs }), scope };
        } else {
            return astgen.failNode(src_node, "undefined global variable", .{});
            // unreachable; // undefined global
        }
    }

    const lambda_scope = parent_scope.lambda().?;

    if (is_global_assign) {
        if (lambda_scope.local_decls.get(ident_name)) |local_decl| {
            const local_ident = switch (node_tags[local_decl]) {
                .identifier => local_decl,
                .apply_binary => tree.unwrapGroupedExpr(node_datas[local_decl].lhs),
                .call => blk: {
                    const sub_range = tree.extraData(node_datas[local_decl].rhs, Ast.Node.SubRange);
                    break :blk tree.unwrapGroupedExpr(tree.extra_data[sub_range.start]);
                },
                else => unreachable,
            };
            assert(node_tags[local_ident] == .identifier);

            if (scope.findLocal(ident_name)) |local_val| { // local before global
                std.log.debug("found local", .{});
                try astgen.appendErrorNodeNotes(src_node, "misleading global-assign of {s} '{s}'", .{
                    @tagName(local_val.id_cat),
                    ident_bytes,
                }, &.{
                    try astgen.errNoteNode(
                        local_ident,
                        "{s} declared here",
                        .{@tagName(local_val.id_cat)},
                    ),
                }, .warn);

                if (local_val.used == 0) local_val.used = ident_token;

                return .{ try gz.addApply(src_node, op, &.{ local_val.inst, rhs }), scope };
            } else { // global before local
                std.log.debug("not found local", .{});
                return astgen.failNodeNotes(
                    local_ident,
                    "global variable '{s}' used as local",
                    .{ident_bytes},
                    &.{
                        try astgen.errNoteTok(
                            ident_token,
                            "global variable declared here",
                            .{},
                        ),
                    },
                );
            }
        } else {
            const node = lambda_scope.global_decls.get(ident_name).?;
            if (src_node == node) { // declare global
                const lhs = try gz.addStrTok(.identifier, ident_name, ident_token);

                const sub_scope = try astgen.arena.create(Scope.LocalVal);
                sub_scope.* = .{
                    .parent = scope,
                    .inst = lhs,
                    .token_src = ident_token,
                    .name = ident_name,
                    .id_cat = .@"global variable",
                };
                scope = &sub_scope.base;

                return .{ try gz.addApply(src_node, op, &.{ lhs, rhs }), scope };
            } else if (scope.findGlobal(ident_name)) |lhs| { // assign global
                return .{ try gz.addApply(src_node, op, &.{ lhs.inst, rhs }), scope };
            } else {
                return astgen.failNode(src_node, "undefined global variable", .{});
                // unreachable; // undefined global
            }
        }
    }

    if (std.mem.indexOfScalar(u8, ident_bytes, '.')) |_| {
        const node = lambda_scope.global_decls.get(ident_name).?;
        if (src_node == node) { // declare global
            const lhs = try gz.addStrTok(.identifier, ident_name, ident_token);

            const sub_scope = try astgen.arena.create(Scope.LocalVal);
            sub_scope.* = .{
                .parent = scope,
                .inst = lhs,
                .token_src = ident_token,
                .name = ident_name,
                .id_cat = .@"global variable",
            };
            scope = &sub_scope.base;

            return .{ try gz.addApply(src_node, op, &.{ lhs, rhs }), scope };
        } else { // assign global
            if (scope.findGlobal(ident_name)) |lhs| {
                return .{ try gz.addApply(src_node, op, &.{ lhs.inst, rhs }), scope };
            } else {
                return astgen.failNode(src_node, "undefined global variable", .{});
                // unreachable; // undefined global
            }
        }
    } else {
        const node = scope.lambda().?.local_decls.get(ident_name).?;
        if (src_node == node) { // declare local
            const lhs = try gz.addStrTok(.identifier, ident_name, ident_token);

            const sub_scope = try astgen.arena.create(Scope.LocalVal);
            sub_scope.* = .{
                .parent = scope,
                .inst = lhs,
                .token_src = ident_token,
                .name = ident_name,
                .id_cat = .@"local variable",
            };
            scope = &sub_scope.base;

            return .{ try gz.addApply(src_node, op, &.{ lhs, rhs }), scope };
        } else { // assign local
            if (scope.findLocal(ident_name)) |lhs| {
                return .{ try gz.addApply(src_node, op, &.{ lhs.inst, rhs }), scope };
            } else {
                return astgen.failNode(src_node, "undefined local variable", .{});
                // unreachable; // undefined local
            }
        }
    }
}

// TODO: a:+/[x]
// TODO: a:/[+]x - error, cannot apply iterator to ':'
// TODO: a:(/[+]x)
// TODO: a:(/[+;x]) - error, rank
fn iterator(gz: *GenZir, parent_scope: *Scope, src_node: Ast.Node.Index) InnerError!Result {
    const astgen = gz.astgen;
    const tree = astgen.tree;
    const node_tags: []Ast.Node.Tag = tree.nodes.items(.tag);
    const node_datas: []Ast.Node.Data = tree.nodes.items(.data);
    var scope = parent_scope;

    const tag: Zir.Inst.Ref = switch (node_tags[src_node]) {
        .apostrophe => .each,
        .apostrophe_colon => .each_prior,
        .slash => .over,
        .slash_colon => .each_right,
        .backslash => .scan,
        .backslash_colon => .each_left,
        else => unreachable,
    };

    const data = node_datas[src_node];
    if (data.lhs != 0) {
        const lhs, scope = try expr(gz, scope, data.lhs);
        if (lhs == .assign) {
            return astgen.failNode(data.lhs, "cannot apply iterator to assignment", .{});
        }

        return .{ try gz.addApply(src_node, tag, &.{lhs}), scope };
    } else {
        return .{ tag, scope };
    }
}

fn call(gz: *GenZir, parent_scope: *Scope, src_node: Ast.Node.Index) InnerError!Result {
    const astgen = gz.astgen;
    const tree = astgen.tree;
    const node_tags: []Ast.Node.Tag = tree.nodes.items(.tag);
    const node_datas: []Ast.Node.Data = tree.nodes.items(.data);
    var scope = parent_scope;

    assert(node_tags[src_node] == .call);

    const full_call = tree.fullCall(src_node);

    const func_node = tree.unwrapGroupedExpr(full_call.func);
    switch (node_tags[func_node]) {
        .lambda,
        .lambda_semicolon,
        => {},

        inline .colon, .colon_colon => |t| {
            switch (full_call.args.len) {
                0 => unreachable,
                2 => {},
                else => return astgen.failNode(
                    src_node,
                    "expected 2 argument(s), found {d}",
                    .{full_call.args.len},
                ),
            }

            const ident_node = tree.unwrapGroupedExpr(full_call.args[0]);
            if (node_tags[ident_node] != .identifier) return astgen.failNode(
                full_call.args[0],
                "invalid left-hand side to assignment",
                .{},
            );

            return assign(
                gz,
                scope,
                src_node,
                full_call.func,
                tree.unwrapGroupedExpr(full_call.args[0]),
                full_call.args[1],
                t == .colon_colon,
            );
        },

        .at => switch (full_call.args.len) {
            0 => unreachable,
            2, 3, 4 => {},
            else => return astgen.failNode(
                src_node,
                "expected 2, 3, or 4 argument(s), found {d}",
                .{full_call.args.len},
            ),
        },

        .apostrophe,
        .apostrophe_colon,
        .slash,
        .slash_colon,
        .backslash,
        .backslash_colon,
        => if (node_datas[func_node].lhs == 0) switch (full_call.args.len) {
            0 => unreachable,
            1 => {},
            else => return astgen.failNode(
                func_node,
                "expected 1 argument(s), found {d}",
                .{full_call.args.len},
            ),
        },

        .identifier,
        .builtin,
        => {},

        else => |t| {
            try astgen.appendErrorNode(src_node, "call NYI: {s}", .{@tagName(t)}, .@"error");
            return .{ .nyi, scope };
        },
    }

    var args_array: [8]Zir.Inst.Ref = undefined;
    assert(full_call.args.len <= 8);
    const args = args_array[0..full_call.args.len];

    var i: usize = 1;
    var it = std.mem.reverseIterator(full_call.args);
    while (it.next()) |param_node| : (i += 1) {
        args[full_call.args.len - i], scope = try expr(gz, scope, param_node);
    }

    const callee, scope = try expr(gz, scope, full_call.func);

    const ref = try gz.addApply(src_node, callee, args);
    return .{ ref, scope };
}

fn applyUnary(gz: *GenZir, parent_scope: *Scope, src_node: Ast.Node.Index) InnerError!Result {
    const astgen = gz.astgen;
    const tree = astgen.tree;
    const node_tags: []Ast.Node.Tag = tree.nodes.items(.tag);
    const node_datas: []Ast.Node.Data = tree.nodes.items(.data);
    var scope = parent_scope;

    const data = node_datas[src_node];
    switch (node_tags[data.lhs]) {
        .grouped_expression => {},

        .lambda,
        .lambda_semicolon,
        => {},

        .colon => {
            const rhs, scope = try expr(gz, scope, data.rhs);
            const ref = try gz.addUnNode(.ret_node, rhs, src_node);
            return .{ ref, scope };
        },

        .apostrophe => {
            const rhs, scope = try expr(gz, scope, data.rhs);
            const ref = try gz.addUnNode(.signal, rhs, src_node);
            return .{ ref, scope };
        },

        .call => {},

        .identifier,
        .builtin, // TODO: https://kdblint.atlassian.net/browse/KLS-311
        => {},

        else => |t| {
            try astgen.appendErrorNode(src_node, "unary NYI: {s}", .{@tagName(t)}, .@"error");
            return .{ .nyi, scope };
        },
    }

    const rhs, scope = try expr(gz, scope, data.rhs);
    const lhs, scope = try expr(gz, scope, data.lhs);
    return .{ try gz.addApply(src_node, lhs, &.{rhs}), scope };
}

fn applyBinary(gz: *GenZir, parent_scope: *Scope, src_node: Ast.Node.Index) InnerError!Result {
    const astgen = gz.astgen;
    const tree = astgen.tree;
    const node_tags: []Ast.Node.Tag = tree.nodes.items(.tag);
    const node_datas: []Ast.Node.Data = tree.nodes.items(.data);
    const main_tokens: []Ast.Token.Index = tree.nodes.items(.main_token);
    var scope = parent_scope;

    const op_node: Ast.Node.Index = main_tokens[src_node];
    switch (node_tags[op_node]) {
        inline .colon, .colon_colon => |t| {
            if (node_datas[src_node].rhs == 0) return astgen.failNode(src_node, "binary TODO", .{});
            const ident_node = tree.unwrapGroupedExpr(node_datas[src_node].lhs);
            if (node_tags[ident_node] != .identifier) return astgen.failNode(src_node, "{}", .{node_tags[ident_node]});
            return assign(
                gz,
                scope,
                src_node,
                op_node,
                ident_node,
                node_datas[src_node].rhs,
                t == .colon_colon,
            );
        },

        .plus, .plus_colon => {},
        .minus, .minus_colon => {},
        .asterisk, .asterisk_colon => {},
        .percent, .percent_colon => {},
        .ampersand, .ampersand_colon => {},
        .pipe, .pipe_colon => {},
        .caret, .caret_colon => {},
        .equal, .equal_colon => {},
        .angle_bracket_left, .angle_bracket_left_colon => {},
        .angle_bracket_left_equal => {},
        .angle_bracket_left_right => {},
        .angle_bracket_right, .angle_bracket_right_colon => {},
        .angle_bracket_right_equal => {},
        .dollar, .dollar_colon => {},
        .comma, .comma_colon => {},
        .hash, .hash_colon => {},
        .underscore, .underscore_colon => {},
        .tilde, .tilde_colon => {},
        .bang, .bang_colon => {},
        .question_mark, .question_mark_colon => {},
        .at, .at_colon => {},
        .period, .period_colon => {},
        .zero_colon, .zero_colon_colon => {},
        .one_colon, .one_colon_colon => {},
        .two_colon => {},

        .apostrophe,
        .apostrophe_colon,
        .slash,
        .slash_colon,
        .backslash,
        .backslash_colon,
        => {},

        .builtin => {},

        else => |t| {
            try astgen.appendErrorNode(src_node, "binary NYI: {s}", .{@tagName(t)}, .@"error");
            return .{ .nyi, scope };
        },
    }

    const data = node_datas[src_node];
    const rhs: Zir.Inst.Ref = if (data.rhs != 0) rhs: {
        const rhs, scope = try expr(gz, scope, data.rhs);
        break :rhs rhs;
    } else .none;
    const op, scope = try expr(gz, scope, op_node);
    const lhs, scope = try expr(gz, scope, data.lhs);

    var ref = try gz.addApply(src_node, op, &.{ lhs, rhs });
    if (node_tags[op_node].isCompoundAssignment()) {
        ref = try gz.addApply(src_node, .assign, &.{ lhs, ref });
    }
    return .{ ref, scope };
}

fn numberLiteral(gz: *GenZir, node: Ast.Node.Index) InnerError!Zir.Inst.Ref {
    const astgen = gz.astgen;
    const tree = astgen.tree;
    const node_tags: []Ast.Node.Tag = tree.nodes.items(.tag);
    const main_tokens: []Ast.Token.Index = tree.nodes.items(.main_token);
    const num_token = main_tokens[node];

    assert(node_tags[node] == .number_literal);

    return parseNumberLiteral(gz, num_token);
}

fn parseNumberLiteral(gz: *GenZir, token: Ast.Token.Index) InnerError!Zir.Inst.Ref {
    const astgen = gz.astgen;
    const tree = astgen.tree;
    const bytes = tree.tokenSlice(token);

    return switch (bytes[0]) {
        '-' => switch (kdb.parseNumberLiteral(bytes[1..])) {
            .long => |num| switch (num) {
                1 => .negative_one,
                else => gz.addLong(-num),
            },
            .failure => |err| return astgen.failWithNumberError(err, token, bytes),
        },
        else => switch (kdb.parseNumberLiteral(bytes)) {
            .long => |num| switch (num) {
                0 => .zero,
                1 => .one,
                else => gz.addLong(num),
            },
            .failure => |err| return astgen.failWithNumberError(err, token, bytes),
        },
    };
}

fn failWithNumberError(
    astgen: *AstGen,
    err: kdb.number_literal.Error,
    token: Ast.Token.Index,
    bytes: []const u8,
) InnerError {
    switch (err) {
        .nyi => return astgen.failTok(token, "nyi: '{s}'", .{bytes}),
    }
}

fn numberListLiteral(gz: *GenZir, node: Ast.Node.Index) InnerError!Zir.Inst.Ref {
    const astgen = gz.astgen;
    const gpa = astgen.gpa;
    const tree = astgen.tree;
    const node_tags: []Ast.Node.Tag = tree.nodes.items(.tag);
    const node_datas: []Ast.Node.Data = tree.nodes.items(.data);
    const main_tokens: []Ast.Token.Index = tree.nodes.items(.main_token);

    assert(node_tags[node] == .number_list_literal);

    const scratch_top = astgen.scratch.items.len;
    defer astgen.scratch.shrinkRetainingCapacity(scratch_top);

    const first_token = main_tokens[node];
    const last_token = node_datas[node].lhs;
    const len = last_token - first_token + 1;
    try astgen.scratch.ensureUnusedCapacity(gpa, len);

    var i: u32 = first_token;
    while (i <= last_token) : (i += 1) {
        const ref = try parseNumberLiteral(gz, i);
        astgen.scratch.appendAssumeCapacity(@intFromEnum(ref));
    }

    const list = astgen.scratch.items[scratch_top..];
    assert(list.len == len);
    const sym_list = try gz.addPlNode(.long_list, node, Zir.Inst.List{
        .len = @intCast(list.len),
    });
    try astgen.extra.appendSlice(gpa, list);
    return sym_list;
}

fn stringLiteral(gz: *GenZir, node: Ast.Node.Index) InnerError!Zir.Inst.Ref {
    const astgen = gz.astgen;
    const tree = astgen.tree;
    const node_tags: []Ast.Node.Tag = tree.nodes.items(.tag);
    const main_tokens: []Ast.Token.Index = tree.nodes.items(.main_token);

    assert(node_tags[node] == .string_literal);

    const str_token = main_tokens[node];
    const str = try astgen.strLitAsString(str_token);
    return gz.addStrTok(.str, str, str_token);
}

fn symbolLiteral(gz: *GenZir, node: Ast.Node.Index) InnerError!Zir.Inst.Ref {
    const astgen = gz.astgen;
    const tree = astgen.tree;
    const node_tags: []Ast.Node.Tag = tree.nodes.items(.tag);
    const main_tokens: []Ast.Token.Index = tree.nodes.items(.main_token);

    assert(node_tags[node] == .symbol_literal);

    const sym_token = main_tokens[node];
    const sym = try astgen.symLitAsString(sym_token);
    return gz.addStrTok(.sym, sym, sym_token);
}

fn symbolListLiteral(gz: *GenZir, node: Ast.Node.Index) InnerError!Zir.Inst.Ref {
    const astgen = gz.astgen;
    const gpa = astgen.gpa;
    const tree = astgen.tree;
    const node_tags: []Ast.Node.Tag = tree.nodes.items(.tag);
    const node_datas: []Ast.Node.Data = tree.nodes.items(.data);
    const main_tokens: []Ast.Token.Index = tree.nodes.items(.main_token);

    assert(node_tags[node] == .symbol_list_literal);

    const scratch_top = astgen.scratch.items.len;
    defer astgen.scratch.shrinkRetainingCapacity(scratch_top);

    const first_token = main_tokens[node];
    const last_token = node_datas[node].lhs;
    const len = last_token - first_token + 1;
    try astgen.scratch.ensureUnusedCapacity(gpa, len);

    var i: u32 = first_token;
    while (i <= last_token) : (i += 1) {
        const sym = try astgen.symLitAsString(i);
        astgen.scratch.appendAssumeCapacity(@intFromEnum(sym));
    }

    const syms = astgen.scratch.items[scratch_top..];
    assert(syms.len == len);
    const sym_list = try gz.addPlNode(.sym_list, node, Zir.Inst.StrList{
        .len = @intCast(syms.len),
    });
    try astgen.extra.appendSlice(gpa, syms);
    return sym_list;
}

fn identifier(gz: *GenZir, scope: *Scope, node: Ast.Node.Index) InnerError!Result {
    const astgen = gz.astgen;
    const tree = astgen.tree;
    const node_tags: []Ast.Node.Tag = tree.nodes.items(.tag);
    const node_datas: []Ast.Node.Data = tree.nodes.items(.data);
    const main_tokens: []Ast.Token.Index = tree.nodes.items(.main_token);

    const ident_token = main_tokens[node];
    const ident_bytes = tree.tokenSlice(ident_token);
    const ident_name = try astgen.bytesAsString(ident_bytes);

    if (scope.lambda()) |lambda_scope| if (lambda_scope.local_decls.get(ident_name)) |local_decl| {
        if (scope.findLocal(ident_name)) |local_val| {
            if (local_val.used == 0) local_val.used = ident_token;
            return .{ local_val.inst, scope };
        }

        const local_ident = switch (node_tags[local_decl]) {
            .identifier => local_decl,
            .apply_binary => tree.unwrapGroupedExpr(node_datas[local_decl].lhs),
            .call => blk: {
                const sub_range = tree.extraData(node_datas[local_decl].rhs, Ast.Node.SubRange);
                break :blk tree.unwrapGroupedExpr(tree.extra_data[sub_range.start]);
            },
            else => unreachable,
        };
        assert(node_tags[local_ident] == .identifier);

        return astgen.failNodeNotes(
            node,
            "use of undeclared identifier '{s}'",
            .{ident_bytes},
            &.{
                try astgen.errNoteNode(local_ident, "identifier declared here", .{}),
            },
        );
    };

    if (scope.findGlobal(ident_name)) |local_val| {
        if (local_val.used == 0) local_val.used = ident_token;
        return .{ local_val.inst, scope };
    }

    return .{ try gz.addStrTok(.identifier, ident_name, ident_token), scope };
}

fn builtin(gz: *GenZir, node: Ast.Node.Index) InnerError!Zir.Inst.Ref {
    const astgen = gz.astgen;
    const tree = astgen.tree;
    const node_tags: []Ast.Node.Tag = tree.nodes.items(.tag);
    const main_tokens: []Ast.Token.Index = tree.nodes.items(.main_token);

    assert(node_tags[node] == .builtin);

    const builtin_token = main_tokens[node];
    const builtin_name = try astgen.tokenAsString(builtin_token);

    return gz.addStrTok(.builtin, builtin_name, builtin_token);
}

fn cond(gz: *GenZir, parent_scope: *Scope, src_node: Ast.Node.Index) InnerError!Result {
    const astgen = gz.astgen;
    const tree = astgen.tree;
    const node_tags: []Ast.Node.Tag = tree.nodes.items(.tag);
    const node_datas: []Ast.Node.Data = tree.nodes.items(.data);
    var scope = parent_scope;

    assert(node_tags[src_node] == .cond);

    const data = node_datas[src_node];

    const cond_expr, scope = try expr(gz, scope, data.lhs);
    _ = cond_expr; // autofix

    // q)$[a;a:1b;a]
    // 'a
    //   [0]  $[a;a:1b;a]
    //          ^

    // q)$[a;a;a:1b]
    // 'a
    //   [0]  $[a;a;a:1b]
    //          ^

    // q)$[1b;a:1b;a]
    // 1b

    // q)$[1b;a;a:1b]
    // 'a
    //   [0]  $[1b;a;a:1b]
    //             ^

    // q)$[0b;a:1b;a]
    // 'a
    //   [0]  $[0b;a:1b;a]
    //                  ^

    // q)$[0b;a;a:1b]
    // 1b

    // TODO: https://kdblint.atlassian.net/browse/KLS-310

    const prev_scope = scope;

    const sub_range = tree.extraData(data.rhs, Ast.Node.SubRange);
    const extra_data = tree.extra_data[sub_range.start..sub_range.end];
    assert(extra_data.len > 0);
    const inst: Zir.Inst.Ref = for (extra_data, 0..) |node, i| {
        const inst, scope = try expr(gz, scope, node);
        if (i == extra_data.len - 1) break inst;
    } else unreachable;

    var temp_scope = scope;
    while (temp_scope != prev_scope) switch (temp_scope.tag) {
        .local_val => {
            const local_val = scope.cast(Scope.LocalVal).?;
            if (local_val.id_cat == .@"local variable") {
                try astgen.appendErrorTokNotes(
                    local_val.token_src,
                    "conditionally declared {s}",
                    .{@tagName(local_val.id_cat)},
                    &.{},
                    .warn,
                );
            }
            temp_scope = local_val.parent;
        },
        else => temp_scope = temp_scope.parent().?,
    };

    return .{ inst, scope };
}

const ScanArgs = struct {
    identifiers: ?*std.StringHashMapUnmanaged(Ast.Node.Index) = null,
    local_decls: ?*std.AutoHashMapUnmanaged(Zir.NullTerminatedString, Ast.Node.Index) = null,
    global_decls: *std.AutoHashMapUnmanaged(Zir.NullTerminatedString, Ast.Node.Index),
};

fn scanExprs(
    astgen: *AstGen,
    exprs: []const Ast.Node.Index,
    args: ScanArgs,
) !void {
    var it = std.mem.reverseIterator(exprs);
    while (it.next()) |node| try astgen.scanNode(node, args);
}

fn scanNode(
    astgen: *AstGen,
    node: Ast.Node.Index,
    args: ScanArgs,
) !void {
    if (node == 0) return;

    const gpa = astgen.gpa;
    const tree = astgen.tree;
    const node_tags: []Ast.Node.Tag = tree.nodes.items(.tag);
    const node_datas: []Ast.Node.Data = tree.nodes.items(.data);
    const main_tokens: []Ast.Token.Index = tree.nodes.items(.main_token);

    switch (node_tags[node]) {
        .root => unreachable,

        .grouped_expression => try astgen.scanNode(node_datas[node].lhs, args),
        .list => {
            const sub_range = tree.extraData(node_datas[node].lhs, Ast.Node.SubRange);
            const slice = tree.extra_data[sub_range.start..sub_range.end];
            for (slice) |n| try astgen.scanNode(n, args);
        },
        .table_literal => {
            const table = tree.extraData(node_datas[node].lhs, Ast.Node.Table);
            const keys_slice = tree.extra_data[table.keys_start..table.keys_end];
            const columns_slice = tree.extra_data[table.columns_start..table.columns_end];
            for (&[_][]Ast.Node.Index{ keys_slice, columns_slice }) |table_slice| {
                for (table_slice) |n| {
                    var next = tree.unwrapGroupedExpr(n);
                    if (node_tags[next] == .apply_binary) {
                        const data = node_datas[next];
                        const op_node: Ast.Node.Index = main_tokens[next];
                        if (node_tags[op_node] == .colon or node_tags[op_node] == .colon_colon) {
                            next = tree.unwrapGroupedExpr(data.lhs);
                            try astgen.scanNode(next, args);
                        } else {
                            try astgen.scanNode(data.lhs, args);
                            try astgen.scanNode(op_node, args);
                        }
                        try astgen.scanNode(data.rhs, args);
                    } else if (node_tags[next] == .call) {
                        const data = node_datas[next];
                        const sub_range = tree.extraData(data.rhs, Ast.Node.SubRange);
                        const slice = tree.extra_data[sub_range.start..sub_range.end];

                        next = tree.unwrapGroupedExpr(data.lhs);

                        try astgen.scanNode(next, args);
                        for (slice) |nn| try astgen.scanNode(nn, args);
                    } else {
                        try astgen.scanNode(next, args);
                    }
                }
            }
        },

        .expr_block => {
            const data = node_datas[node];
            if (data.lhs != 0) {
                const sub_range = tree.extraData(node_datas[node].lhs, Ast.Node.SubRange);
                const slice = tree.extra_data[sub_range.start..sub_range.end];
                var it = std.mem.reverseIterator(slice);
                while (it.next()) |n| try astgen.scanNode(n, args);
            }
        },

        .apostrophe,
        .apostrophe_colon,
        .slash,
        .slash_colon,
        .backslash,
        .backslash_colon,
        => try astgen.scanNode(node_datas[node].lhs, args),

        .call,
        => {
            const data = node_datas[node];
            const sub_range = tree.extraData(data.rhs, Ast.Node.SubRange);
            const slice = tree.extra_data[sub_range.start..sub_range.end];

            var next = tree.unwrapGroupedExpr(data.lhs);
            if (node_tags[next] == .colon and slice.len == 2) {
                next = tree.unwrapGroupedExpr(slice[0]);
                if (node_tags[next] == .identifier) {
                    const ident_token = main_tokens[next];
                    const ident_bytes = tree.tokenSlice(ident_token);
                    const ident_name = try astgen.bytesAsString(ident_bytes);

                    if (args.identifiers) |identifiers| try identifiers.put(
                        gpa,
                        ident_bytes,
                        next,
                    );

                    if (std.mem.indexOfScalar(u8, ident_bytes, '.') != null or args.local_decls == null) {
                        try args.global_decls.put(gpa, ident_name, node);
                    } else {
                        try args.local_decls.?.put(gpa, ident_name, node);
                    }
                    try astgen.scanNode(slice[1], args);
                } else {
                    for (slice) |n| try astgen.scanNode(n, args);
                }
            } else if (node_tags[next] == .colon_colon and slice.len == 2) {
                next = tree.unwrapGroupedExpr(slice[0]);
                if (node_tags[next] == .identifier) {
                    const ident_token = main_tokens[next];
                    const ident_bytes = tree.tokenSlice(ident_token);
                    const ident_name = try astgen.bytesAsString(ident_bytes);

                    if (args.identifiers) |identifiers| try identifiers.put(
                        gpa,
                        ident_bytes,
                        next,
                    );

                    try args.global_decls.put(gpa, ident_name, node);
                    try astgen.scanNode(slice[1], args);
                } else {
                    for (slice) |n| try astgen.scanNode(n, args);
                }
            } else {
                try astgen.scanNode(data.lhs, args);
                for (slice) |n| try astgen.scanNode(n, args);
            }
        },
        .apply_unary,
        => {
            const data = node_datas[node];
            try astgen.scanNode(data.lhs, args);
            try astgen.scanNode(data.rhs, args);
        },
        .apply_binary => {
            const data = node_datas[node];
            const op_node: Ast.Node.Index = main_tokens[node];
            if (node_tags[op_node] == .colon) {
                const next = tree.unwrapGroupedExpr(data.lhs);
                if (node_tags[next] == .identifier) {
                    const ident_token = main_tokens[next];
                    const ident_bytes = tree.tokenSlice(ident_token);
                    const ident_name = try astgen.bytesAsString(ident_bytes);

                    if (args.identifiers) |identifiers| try identifiers.put(
                        gpa,
                        ident_bytes,
                        next,
                    );

                    if (std.mem.indexOfScalar(u8, ident_bytes, '.') != null or args.local_decls == null) {
                        try args.global_decls.put(gpa, ident_name, node);
                    } else {
                        try args.local_decls.?.put(gpa, ident_name, node);
                    }
                } else {
                    try astgen.scanNode(next, args);
                }
            } else if (node_tags[op_node] == .colon_colon) {
                const next = tree.unwrapGroupedExpr(data.lhs);
                if (node_tags[next] == .identifier) {
                    const ident_token = main_tokens[next];
                    const ident_bytes = tree.tokenSlice(ident_token);
                    const ident_name = try astgen.bytesAsString(ident_bytes);

                    if (args.identifiers) |identifiers| try identifiers.put(
                        gpa,
                        ident_bytes,
                        next,
                    );

                    try args.global_decls.put(gpa, ident_name, node);
                } else {
                    try astgen.scanNode(next, args);
                }
            } else {
                try astgen.scanNode(data.lhs, args);
                try astgen.scanNode(op_node, args);
            }
            try astgen.scanNode(data.rhs, args);
        },

        inline .select, .exec, .update, .delete_rows, .delete_cols => |t| {
            const sql = tree.extraData(node_datas[node].lhs, switch (t) {
                .select => Ast.Node.Select,
                .exec => Ast.Node.Exec,
                .update => Ast.Node.Update,
                .delete_rows => Ast.Node.DeleteRows,
                .delete_cols => Ast.Node.DeleteCols,
                else => comptime unreachable,
            });
            try astgen.scanNode(sql.from, args);
        },

        .do,
        .@"if",
        .@"while",
        .cond,
        => {
            const data = node_datas[node];
            const sub_range = tree.extraData(data.rhs, Ast.Node.SubRange);
            const slice = tree.extra_data[sub_range.start..sub_range.end];
            var it = std.mem.reverseIterator(slice);
            while (it.next()) |n| try astgen.scanNode(n, args);
            try astgen.scanNode(data.lhs, args);
        },

        .identifier => if (args.identifiers) |identifiers| {
            const ident_token = main_tokens[node];
            const ident_bytes = tree.tokenSlice(ident_token);

            try identifiers.put(gpa, ident_bytes, node);
        },

        else => {},
    }
}

fn testScanExprs(source: [:0]const u8, expected: []const []const u8) !void {
    const gpa = std.testing.allocator;
    var tree = try Ast.parse(gpa, source, .{
        .mode = .q,
        .version = .@"4.0",
    });
    defer tree.deinit(gpa);

    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();

    var astgen: AstGen = .{
        .gpa = gpa,
        .arena = arena.allocator(),
        .tree = &tree,
    };
    defer astgen.deinit(gpa);

    var lambda_scope: Scope.Lambda = .{
        .parent = undefined,
        .node = 0,
        .inst = undefined,
    };
    defer lambda_scope.deinit(gpa);

    try astgen.scanExprs(tree.getBlocks(), .{
        .local_decls = &lambda_scope.local_decls,
        .global_decls = &lambda_scope.global_decls,
    });

    try std.testing.expectEqual(expected.len, lambda_scope.local_decls.size);
    var it = lambda_scope.local_decls.valueIterator();
    var i: u32 = 0;
    while (it.next()) |entry| : (i += 1) {
        const span = tree.nodeToSpan(entry.*);
        try std.testing.expectEqualStrings(expected[i], tree.source[span.start..span.end]);
    }
}

test "scan decls" {
    try testScanExprs("a:1", &.{"a:1"});
    try testScanExprs("a:1;a:2", &.{"a:1"});
    try testScanExprs("a:1+a:2", &.{"a:2"});
    try testScanExprs("(a):1", &.{"(a):1"});
    try testScanExprs("(a):1;(a):2", &.{"(a):1"});
    try testScanExprs("(a):1+(a):2", &.{"(a):2"});
    try testScanExprs("((a)):1", &.{"((a)):1"});
    try testScanExprs("((a)):1;((a)):2", &.{"((a)):1"});
    try testScanExprs("((a)):1+((a)):2", &.{"((a)):2"});

    try testScanExprs(":[a;1]", &.{":[a;1]"});
    try testScanExprs(":[a;1];:[a;2]", &.{":[a;1]"});
    try testScanExprs(":[a;1+a:2]", &.{"a:2"});
    try testScanExprs(":[a;1+:[a;2]]", &.{":[a;1+:[a;2]]"});
    try testScanExprs(":[a;1+ :[a;2]]", &.{":[a;2]"});
    try testScanExprs("(:)[a;1]", &.{"(:)[a;1]"});
    try testScanExprs("(:)[a;1];(:)[a;2]", &.{"(:)[a;1]"});
    try testScanExprs("(:)[a;1+(a):2]", &.{"(a):2"});
    try testScanExprs("(:)[a;1+(:)[a;2]]", &.{"(:)[a;2]"});
    try testScanExprs("((:))[a;1]", &.{"((:))[a;1]"});
    try testScanExprs("((:))[a;1];((:))[a;2]", &.{"((:))[a;1]"});
    try testScanExprs("((:))[a;1+((a)):2]", &.{"((a)):2"});
    try testScanExprs("((:))[a;1+((:))[a;2]]", &.{"((:))[a;2]"});

    try testScanExprs("([]x:(a:1),1)", &.{"a:1"});
    try testScanExprs("([](x:(a:1),1))", &.{"a:1"});
    try testScanExprs("([]((x:(a:1),1)))", &.{"a:1"});
    try testScanExprs("([](x):(a:1),1)", &.{"a:1"});
    try testScanExprs("([]((x):(a:1),1))", &.{"a:1"});
    try testScanExprs("([](((x):(a:1),1)))", &.{"a:1"});
    try testScanExprs("([]((x)):(a:1),1)", &.{"a:1"});
    try testScanExprs("([](((x)):(a:1),1))", &.{"a:1"});
    try testScanExprs("([]((((x)):(a:1),1)))", &.{"a:1"});
    try testScanExprs("([]:[x;(a:1),1])", &.{"a:1"});
    try testScanExprs("([](:[x;(a:1),1]))", &.{"a:1"});
    try testScanExprs("([]((:[x;(a:1),1])))", &.{"a:1"});
    try testScanExprs("([]:[(x);(a:1),1])", &.{"a:1"});
    try testScanExprs("([](:[(x);(a:1),1]))", &.{"a:1"});
    try testScanExprs("([]((:[(x);(a:1),1])))", &.{"a:1"});
    try testScanExprs("([]:[((x));(a:1),1])", &.{"a:1"});
    try testScanExprs("([](:[((x));(a:1),1]))", &.{"a:1"});
    try testScanExprs("([]((:[((x));(a:1),1])))", &.{"a:1"});
    try testScanExprs("([](:)[x;(a:1),1])", &.{"a:1"});
    try testScanExprs("([]((:)[x;(a:1),1]))", &.{"a:1"});
    try testScanExprs("([](((:)[x;(a:1),1])))", &.{"a:1"});
    try testScanExprs("([](:)[(x);(a:1),1])", &.{"a:1"});
    try testScanExprs("([]((:)[(x);(a:1),1]))", &.{"a:1"});
    try testScanExprs("([](((:)[(x);(a:1),1])))", &.{"a:1"});
    try testScanExprs("([](:)[((x));(a:1),1])", &.{"a:1"});
    try testScanExprs("([]((:)[((x));(a:1),1]))", &.{"a:1"});
    try testScanExprs("([](((:)[((x));(a:1),1])))", &.{"a:1"});
    try testScanExprs("([]((:))[x;(a:1),1])", &.{"a:1"});
    try testScanExprs("([](((:))[x;(a:1),1]))", &.{"a:1"});
    try testScanExprs("([]((((:))[x;(a:1),1])))", &.{"a:1"});
}

fn lowerAstErrors(astgen: *AstGen) !void {
    const tree = astgen.tree;
    assert(tree.errors.len > 0);

    const gpa = astgen.gpa;
    const parse_err = tree.errors[0];

    var msg: std.ArrayListUnmanaged(u8) = .empty;
    defer msg.deinit(gpa);

    var notes: std.ArrayListUnmanaged(u32) = .empty;
    defer notes.deinit(gpa);

    for (tree.errors[1..]) |note| {
        if (!note.is_note) break;

        msg.clearRetainingCapacity();
        try tree.renderError(note, msg.writer(gpa));
        try notes.append(
            gpa,
            try astgen.errNoteTok(note.token, "{s}", .{msg.items}),
        );
    }

    const extra_offset = tree.errorOffset(parse_err);
    msg.clearRetainingCapacity();
    try tree.renderError(parse_err, msg.writer(gpa));
    try astgen.appendErrorTokNotesOff(
        parse_err.token,
        extra_offset,
        "{s}",
        .{msg.items},
        notes.items,
        .@"error",
    );
}

const Scope = struct {
    tag: Tag,

    fn cast(base: *Scope, comptime T: type) ?*T {
        if (base.tag != T.base_tag) return null;
        return @alignCast(@fieldParentPtr("base", base));
    }

    fn parent(base: *Scope) ?*Scope {
        return switch (base.tag) {
            .gen_zir => base.cast(GenZir).?.parent,
            .local_val => base.cast(LocalVal).?.parent,
            .lambda => base.cast(Lambda).?.parent,
            .top => null,
        };
    }

    fn isGlobal(base: *Scope) bool {
        return base.lambda() == null;
    }

    fn top(base: *Scope) *Top {
        var scope = base;
        while (true) switch (scope.tag) {
            .gen_zir => scope = scope.cast(GenZir).?.parent,
            .local_val => scope = scope.cast(LocalVal).?.parent,
            .lambda => scope = scope.cast(Lambda).?.parent,
            .top => return scope.cast(Top).?,
        };
        unreachable;
    }

    fn lambda(base: *Scope) ?*Lambda {
        var scope = base;
        while (true) switch (scope.tag) {
            .gen_zir => scope = scope.cast(GenZir).?.parent,
            .local_val => scope = scope.cast(LocalVal).?.parent,
            .lambda => return scope.cast(Lambda).?,
            .top => break,
        };
        return null;
    }

    fn findLocal(base: *Scope, name: Zir.NullTerminatedString) ?*LocalVal {
        var scope = base;
        while (true) switch (scope.tag) {
            .gen_zir => scope = scope.cast(GenZir).?.parent,
            .local_val => {
                const local_val = scope.cast(LocalVal).?;
                if (local_val.name == name and local_val.id_cat != .@"global variable") {
                    return local_val;
                }
                scope = local_val.parent;
            },
            .lambda => break,
            .top => break,
        };
        return null;
    }

    fn findGlobal(base: *Scope, name: Zir.NullTerminatedString) ?*LocalVal {
        var scope = base;
        while (true) switch (scope.tag) {
            .gen_zir => scope = scope.cast(GenZir).?.parent,
            .local_val => {
                const local_val = scope.cast(LocalVal).?;
                if (local_val.name == name and local_val.id_cat == .@"global variable") {
                    return local_val;
                }
                scope = local_val.parent;
            },
            .lambda => scope = scope.cast(Lambda).?.parent,
            .top => break,
        };
        return null;
    }

    const Tag = enum {
        gen_zir,
        local_val,
        lambda,
        top,
    };

    /// The category of identifier. These tag names are user-visible in compile errors.
    const IdCat = enum {
        @"function parameter",
        @"local variable",
        @"global variable",
    };

    /// This is always a `const` local and importantly the `inst` is a value type, not a pointer.
    /// This structure lives as long as the AST generation of the Block
    /// node that contains the variable.
    const LocalVal = struct {
        const base_tag: Tag = .local_val;
        base: Scope = .{ .tag = base_tag },
        /// Parents can be: `LocalVal`, `GenZir`, `Namespace`.
        parent: *Scope,
        inst: Zir.Inst.Ref,
        /// Source location of the corresponding variable declaration.
        token_src: Ast.Token.Index,
        /// Track the first identifier where it is referenced.
        /// 0 means never referenced.
        used: Ast.Token.Index = 0,
        /// String table index.
        name: Zir.NullTerminatedString,
        id_cat: IdCat,
    };

    /// Represents a global scope that has any number of declarations in it.
    /// Each declaration has this as the parent scope.
    const Lambda = struct {
        const base_tag: Tag = .lambda;
        base: Scope = .{ .tag = base_tag },

        /// Parents can be: `LocalVal`, `GenZir`, `Namespace`, `Top`.
        parent: *Scope,
        local_decls: std.AutoHashMapUnmanaged(Zir.NullTerminatedString, Ast.Node.Index) = .empty,
        global_decls: std.AutoHashMapUnmanaged(Zir.NullTerminatedString, Ast.Node.Index) = .empty,
        node: Ast.Node.Index,
        inst: Zir.Inst.Index,

        fn deinit(self: *Lambda, gpa: Allocator) void {
            self.local_decls.deinit(gpa);
            self.global_decls.deinit(gpa);
            self.* = undefined;
        }
    };

    const Top = struct {
        const base_tag: Scope.Tag = .top;
        base: Scope = .{ .tag = base_tag },

        global_decls: std.AutoHashMapUnmanaged(Zir.NullTerminatedString, Ast.Node.Index) = .empty,

        fn deinit(self: *Top, gpa: Allocator) void {
            self.global_decls.deinit(gpa);
            self.* = undefined;
        }
    };
};

const GenZir = struct {
    const base_tag: Scope.Tag = .gen_zir;
    base: Scope = .{ .tag = base_tag },
    /// The containing decl AST node.
    decl_node_index: Ast.Node.Index,
    /// The containing decl line index, absolute.
    decl_line: u32,
    /// Parents can be: `LocalVal`, `GenZir`, `Namespace`.
    parent: *Scope,
    /// All `GenZir` scopes for the same ZIR share this.
    astgen: *AstGen,
    /// Keeps track of the list of instructions in this scope. Possibly shared.
    /// Indexes to instructions in `astgen`.
    instructions: *std.ArrayListUnmanaged(Zir.Inst.Index),
    /// A sub-block may share its instructions ArrayList with containing GenZir,
    /// if use is strictly nested. This saves prior size of list for unstacking.
    instructions_top: usize,

    const unstacked_top = std.math.maxInt(usize);
    /// Call unstack before adding any new instructions to containing GenZir.
    fn unstack(self: *GenZir) void {
        if (self.instructions_top != unstacked_top) {
            self.instructions.items.len = self.instructions_top;
            self.instructions_top = unstacked_top;
        }
    }

    fn isEmpty(self: *const GenZir) bool {
        return (self.instructions_top == unstacked_top) or (self.instructions.items.len == self.instructions_top);
    }

    fn makeSubBlock(gz: *GenZir, scope: *Scope) GenZir {
        return .{
            .decl_node_index = gz.decl_node_index,
            .decl_line = gz.decl_line,
            .parent = scope,
            .astgen = gz.astgen,
            .instructions = gz.instructions,
            .instructions_top = gz.instructions.items.len,
        };
    }

    /// Assumes nothing stacked on `gz`.
    fn endsWithNoReturn(gz: GenZir) bool {
        if (gz.isEmpty()) return false;
        const tags: []Zir.Inst.Tag = gz.astgen.instructions.items(.tag);
        const last_inst = gz.instructions.items[gz.instructions.items.len - 1];
        return tags[@intFromEnum(last_inst)].isNoReturn();
    }

    fn instructionsSlice(self: *const GenZir) []Zir.Inst.Index {
        return if (self.instructions_top == unstacked_top)
            &[0]Zir.Inst.Index{}
        else
            self.instructions.items[self.instructions_top..];
    }

    fn nodeIndexToRelative(gz: GenZir, node_index: Ast.Node.Index) i32 {
        return @as(i32, @bitCast(node_index)) - @as(i32, @bitCast(gz.decl_node_index));
    }

    fn tokenIndexToRelative(gz: GenZir, token: Ast.Token.Index) u32 {
        return token - gz.srcToken();
    }

    fn srcToken(gz: GenZir) Ast.Token.Index {
        return gz.astgen.tree.firstToken(gz.decl_node_index);
    }

    fn makeFile(gz: *GenZir) !Zir.Inst.Index {
        return gz.makePlNode(.file, 0);
    }

    /// Assumes nothing stacked on `gz`. Unstacks `gz`.
    fn setFile(gz: *GenZir, inst: Zir.Inst.Index) !void {
        const astgen = gz.astgen;
        const gpa = astgen.gpa;
        const body = gz.instructionsSlice();
        const body_len = astgen.countBodyLenAfterFixups(body);
        try astgen.extra.ensureUnusedCapacity(
            gpa,
            @typeInfo(Zir.Inst.Block).@"struct".fields.len + body_len,
        );
        const zir_datas: []Zir.Inst.Data = astgen.instructions.items(.data);
        zir_datas[@intFromEnum(inst)].pl_node.payload_index = astgen.addExtraAssumeCapacity(
            Zir.Inst.Block{ .body_len = body_len },
        );
        astgen.appendBodyWithFixups(body);
        gz.unstack();
    }

    fn makeLambda(gz: *GenZir, node: Ast.Node.Index) !Zir.Inst.Index {
        const new_index: Zir.Inst.Index = @enumFromInt(gz.astgen.instructions.len);
        try gz.astgen.instructions.append(gz.astgen.gpa, .{
            .tag = .lambda,
            .data = .{ .lambda = .{
                .src_node = node,
                .payload_index = undefined,
            } },
        });
        return new_index;
    }

    /// Must be called with the following stack set up:
    ///  * gz (bottom)
    ///  * body_gz (top)
    /// Unstacks all of those except for `gz`.
    fn setLambda(gz: *GenZir, inst: Zir.Inst.Index, args: struct {
        lbrace_line: u32,
        lbrace_column: u32,
        rbrace: Ast.Token.Index,
        params_len: u32,
        body_gz: *GenZir,
    }) !void {
        const astgen = gz.astgen;
        const gpa = astgen.gpa;
        const tree = astgen.tree;
        const token_locs: []Ast.Token.Loc = tree.tokens.items(.loc);
        const zir_datas: []Zir.Inst.Data = astgen.instructions.items(.data);

        const rbrace_loc = token_locs[args.rbrace];
        astgen.advanceSourceCursor(rbrace_loc.start);
        const rbrace_line = astgen.source_line - gz.decl_line;
        const rbrace_column = astgen.source_column;

        const columns = args.lbrace_column | (rbrace_column << 16);

        const src_locs: Zir.Inst.Lambda.SrcLocs = .{
            .lbrace_line = args.lbrace_line,
            .rbrace_line = rbrace_line,
            .columns = columns,
        };

        const body = args.body_gz.instructionsSlice();
        const body_len = astgen.countBodyLenAfterFixups(body);
        args.body_gz.unstack();

        try astgen.extra.ensureUnusedCapacity(
            gpa,
            @typeInfo(Zir.Inst.Lambda).@"struct".fields.len + body_len +
                @typeInfo(Zir.Inst.Lambda.SrcLocs).@"struct".fields.len,
        );

        zir_datas[@intFromEnum(inst)].lambda.payload_index = astgen.addExtraAssumeCapacity(Zir.Inst.Lambda{
            .params_len = args.params_len,
            .body_len = body_len,
        });
        // astgen.appendBodyWithFixups(params);
        astgen.appendBodyWithFixups(body);
        _ = astgen.addExtraAssumeCapacity(src_locs);

        try gz.instructions.ensureUnusedCapacity(gpa, 1);
        gz.instructions.appendAssumeCapacity(inst);
    }

    fn addStrNode(
        gz: *GenZir,
        tag: Zir.Inst.Tag,
        str_index: Zir.NullTerminatedString,
        /// Absolute node index. This function does the conversion to offset from Decl.
        src_node: Ast.Node.Index,
    ) !Zir.Inst.Ref {
        return gz.add(.{
            .tag = tag,
            .data = .{ .str_node = .{
                .start = str_index,
                .src_node = gz.nodeIndexToRelative(src_node),
            } },
        });
    }

    fn addStrTok(
        gz: *GenZir,
        tag: Zir.Inst.Tag,
        str_index: Zir.NullTerminatedString,
        /// Absolute token index. This function does the conversion to Decl offset.
        abs_tok_index: Ast.Token.Index,
    ) !Zir.Inst.Ref {
        return gz.add(.{
            .tag = tag,
            .data = .{ .str_tok = .{
                .start = str_index,
                .src_tok = gz.tokenIndexToRelative(abs_tok_index),
            } },
        });
    }

    fn addLong(gz: *GenZir, long: i64) !Zir.Inst.Ref {
        return gz.add(.{
            .tag = .long,
            .data = .{ .long = long },
        });
    }

    fn makePlNode(
        gz: *GenZir,
        tag: Zir.Inst.Tag,
        /// Absolute node index. This function does the conversion to offset from Decl.
        src_node: Ast.Node.Index,
    ) !Zir.Inst.Index {
        const new_index: Zir.Inst.Index = @enumFromInt(gz.astgen.instructions.len);
        try gz.astgen.instructions.append(gz.astgen.gpa, .{
            .tag = tag,
            .data = .{ .pl_node = .{
                .src_node = gz.nodeIndexToRelative(src_node),
                .payload_index = undefined,
            } },
        });
        return new_index;
    }

    fn addPlNode(
        gz: *GenZir,
        tag: Zir.Inst.Tag,
        /// Absolute node index. This function does the conversion to offset from Decl.
        src_node: Ast.Node.Index,
        extra: anytype,
    ) !Zir.Inst.Ref {
        const gpa = gz.astgen.gpa;
        try gz.instructions.ensureUnusedCapacity(gpa, 1);
        try gz.astgen.instructions.ensureUnusedCapacity(gpa, 1);

        const payload_index = try gz.astgen.addExtra(extra);
        const new_index: Zir.Inst.Index = @enumFromInt(gz.astgen.instructions.len);
        gz.astgen.instructions.appendAssumeCapacity(.{
            .tag = tag,
            .data = .{ .pl_node = .{
                .src_node = gz.nodeIndexToRelative(src_node),
                .payload_index = payload_index,
            } },
        });
        gz.instructions.appendAssumeCapacity(new_index);
        return new_index.toRef();
    }

    fn addUnNode(
        gz: *GenZir,
        tag: Zir.Inst.Tag,
        operand: Zir.Inst.Ref,
        /// Absolute node index. This function does the conversion to offset from Decl.
        src_node: Ast.Node.Index,
    ) !Zir.Inst.Ref {
        assert(operand != .none);
        return gz.add(.{
            .tag = tag,
            .data = .{ .un_node = .{
                .operand = operand,
                .src_node = gz.nodeIndexToRelative(src_node),
            } },
        });
    }

    fn addUnTok(
        gz: *GenZir,
        tag: Zir.Inst.Tag,
        operand: Zir.Inst.Ref,
        /// Absolute token index. This function does the conversion to Decl offset.
        abs_tok_index: Ast.Token.Index,
    ) !Zir.Inst.Ref {
        assert(operand != .none);
        return gz.add(.{
            .tag = tag,
            .data = .{ .un_tok = .{
                .operand = operand,
                .src_tok = gz.tokenIndexToRelative(abs_tok_index),
            } },
        });
    }

    fn addApply(gz: *GenZir, src_node: Ast.Node.Index, callee: Zir.Inst.Ref, args: []const Zir.Inst.Ref) !Zir.Inst.Ref {
        const result = try gz.addPlNode(.apply, src_node, Zir.Inst.Apply{
            .callee = callee,
            .len = @intCast(args.len),
        });
        try gz.astgen.extra.appendSlice(gz.astgen.gpa, @ptrCast(args));
        return result;
    }

    fn add(gz: *GenZir, inst: Zir.Inst) !Zir.Inst.Ref {
        return (try gz.addAsIndex(inst)).toRef();
    }

    fn addNode(
        gz: *GenZir,
        tag: Zir.Inst.Tag,
        /// Absolute node index. This function does the conversion to offset from Decl.
        src_node: Ast.Node.Index,
    ) !Zir.Inst.Ref {
        return gz.add(.{
            .tag = tag,
            .data = .{ .node = gz.nodeIndexToRelative(src_node) },
        });
    }

    fn addAsIndex(gz: *GenZir, inst: Zir.Inst) !Zir.Inst.Index {
        const astgen = gz.astgen;
        const gpa = astgen.gpa;
        try gz.instructions.ensureUnusedCapacity(gpa, 1);
        try astgen.instructions.ensureUnusedCapacity(gpa, 1);

        const new_index: Zir.Inst.Index = @enumFromInt(astgen.instructions.len);
        astgen.instructions.appendAssumeCapacity(inst);
        gz.instructions.appendAssumeCapacity(new_index);

        return new_index;
    }
};

/// Advances the source cursor to the beginning of `node`.
fn advanceSourceCursorToNode(astgen: *AstGen, node: Ast.Node.Index) void {
    const tree = astgen.tree;
    const token_locs: []Ast.Token.Loc = tree.tokens.items(.loc);
    const node_start = token_locs[tree.firstToken(node)].start;
    astgen.advanceSourceCursor(node_start);
}

/// Advances the source cursor to an absolute byte offset `end` in the file.
fn advanceSourceCursor(astgen: *AstGen, end: usize) void {
    const source = astgen.tree.source;
    var i = astgen.source_offset;
    var line = astgen.source_line;
    var column = astgen.source_column;
    assert(i <= end);
    while (i < end) : (i += 1) {
        if (source[i] == '\n') {
            line += 1;
            column = 0;
        } else {
            column += 1;
        }
    }
    astgen.source_offset = i;
    astgen.source_line = line;
    astgen.source_column = column;
}

/// Assumes capacity for body has already been added. Needed capacity taking into
/// account fixups can be found with `countBodyLenAfterFixups`.
fn appendBodyWithFixups(astgen: *AstGen, body: []const Zir.Inst.Index) void {
    return appendBodyWithFixupsArrayList(astgen, &astgen.extra, body);
}

fn appendBodyWithFixupsArrayList(
    astgen: *AstGen,
    list: *std.ArrayListUnmanaged(u32),
    body: []const Zir.Inst.Index,
) void {
    for (body) |body_inst| {
        appendPossiblyRefdBodyInst(astgen, list, body_inst);
    }
}

fn appendPossiblyRefdBodyInst(
    astgen: *AstGen,
    list: *std.ArrayListUnmanaged(u32),
    body_inst: Zir.Inst.Index,
) void {
    list.appendAssumeCapacity(@intFromEnum(body_inst));
    const kv = astgen.ref_table.fetchRemove(body_inst) orelse return;
    const ref_inst = kv.value;
    return appendPossiblyRefdBodyInst(astgen, list, ref_inst);
}

fn countBodyLenAfterFixups(astgen: *AstGen, body: []const Zir.Inst.Index) u32 {
    var count = body.len;
    for (body) |body_inst| {
        var check_inst = body_inst;
        while (astgen.ref_table.get(check_inst)) |ref_inst| {
            count += 1;
            check_inst = ref_inst;
        }
    }
    return @intCast(count);
}

fn failNode(
    astgen: *AstGen,
    node: Ast.Node.Index,
    comptime format: []const u8,
    args: anytype,
) InnerError {
    return astgen.failNodeNotes(node, format, args, &[0]u32{});
}

fn appendErrorNode(
    astgen: *AstGen,
    node: Ast.Node.Index,
    comptime format: []const u8,
    args: anytype,
    kind: Zir.Inst.CompileErrors.Kind,
) Allocator.Error!void {
    try astgen.appendErrorNodeNotes(node, format, args, &[0]u32{}, kind);
}

fn appendErrorNodeNotes(
    astgen: *AstGen,
    node: Ast.Node.Index,
    comptime format: []const u8,
    args: anytype,
    notes: []const u32,
    kind: Zir.Inst.CompileErrors.Kind,
) Allocator.Error!void {
    @branchHint(.cold);
    const string_bytes = &astgen.string_bytes;
    const msg: Zir.NullTerminatedString = @enumFromInt(string_bytes.items.len);
    try string_bytes.writer(astgen.gpa).print(format ++ "\x00", args);
    const notes_index: u32 = if (notes.len != 0) blk: {
        const notes_start = astgen.extra.items.len;
        try astgen.extra.ensureTotalCapacity(astgen.gpa, notes_start + 1 + notes.len);
        astgen.extra.appendAssumeCapacity(@intCast(notes.len));
        astgen.extra.appendSliceAssumeCapacity(notes);
        break :blk @intCast(notes_start);
    } else 0;
    var list = switch (kind) {
        .@"error" => &astgen.compile_errors,
        .warn => &astgen.compile_warnings,
        else => unreachable,
    };
    try list.append(astgen.gpa, .{
        .msg = msg,
        .node = node,
        .token = 0,
        .byte_offset = 0,
        .notes = notes_index,
        .kind = kind,
    });
}

fn failNodeNotes(
    astgen: *AstGen,
    node: Ast.Node.Index,
    comptime format: []const u8,
    args: anytype,
    notes: []const u32,
) InnerError {
    try appendErrorNodeNotes(astgen, node, format, args, notes, .@"error");
    return error.AnalysisFail;
}

fn failTok(
    astgen: *AstGen,
    token: Ast.Token.Index,
    comptime format: []const u8,
    args: anytype,
) InnerError {
    return astgen.failTokNotes(token, format, args, &[0]u32{});
}

fn appendErrorTok(
    astgen: *AstGen,
    token: Ast.Token.Index,
    comptime format: []const u8,
    args: anytype,
    kind: Zir.Inst.CompileErrors.Kind,
) !void {
    try astgen.appendErrorTokNotesOff(
        token,
        0,
        format,
        args,
        &[0]u32{},
        kind,
    );
}

fn failTokNotes(
    astgen: *AstGen,
    token: Ast.Token.Index,
    comptime format: []const u8,
    args: anytype,
    notes: []const u32,
) InnerError {
    try appendErrorTokNotesOff(
        astgen,
        token,
        0,
        format,
        args,
        notes,
        .@"error",
    );
    return error.AnalysisFail;
}

fn appendErrorTokNotes(
    astgen: *AstGen,
    token: Ast.Token.Index,
    comptime format: []const u8,
    args: anytype,
    notes: []const u32,
    kind: Zir.Inst.CompileErrors.Kind,
) !void {
    return appendErrorTokNotesOff(
        astgen,
        token,
        0,
        format,
        args,
        notes,
        kind,
    );
}

/// Same as `fail`, except given a token plus an offset from its starting byte
/// offset.
fn failOff(
    astgen: *AstGen,
    token: Ast.Token.Index,
    byte_offset: u32,
    comptime format: []const u8,
    args: anytype,
) InnerError {
    try appendErrorTokNotesOff(
        astgen,
        token,
        byte_offset,
        format,
        args,
        &.{},
    );
    return error.AnalysisFail;
}

fn appendErrorTokNotesOff(
    astgen: *AstGen,
    token: Ast.Token.Index,
    byte_offset: u32,
    comptime format: []const u8,
    args: anytype,
    notes: []const u32,
    kind: Zir.Inst.CompileErrors.Kind,
) !void {
    @branchHint(.cold);
    const gpa = astgen.gpa;
    const string_bytes = &astgen.string_bytes;
    const msg: Zir.NullTerminatedString = @enumFromInt(string_bytes.items.len);
    try string_bytes.writer(gpa).print(format ++ "\x00", args);
    const notes_index: u32 = if (notes.len != 0) blk: {
        const notes_start = astgen.extra.items.len;
        try astgen.extra.ensureTotalCapacity(gpa, notes_start + 1 + notes.len);
        astgen.extra.appendAssumeCapacity(@intCast(notes.len));
        astgen.extra.appendSliceAssumeCapacity(notes);
        break :blk @intCast(notes_start);
    } else 0;
    var list = switch (kind) {
        .@"error" => &astgen.compile_errors,
        .warn => &astgen.compile_warnings,
        else => unreachable,
    };
    try list.append(gpa, .{
        .msg = msg,
        .node = 0,
        .token = token,
        .byte_offset = byte_offset,
        .notes = notes_index,
        .kind = kind,
    });
}

fn errNoteTok(
    astgen: *AstGen,
    token: Ast.Token.Index,
    comptime format: []const u8,
    args: anytype,
) Allocator.Error!u32 {
    return errNoteTokOff(astgen, token, 0, format, args);
}

fn errNoteTokOff(
    astgen: *AstGen,
    token: Ast.Token.Index,
    byte_offset: u32,
    comptime format: []const u8,
    args: anytype,
) Allocator.Error!u32 {
    @branchHint(.cold);
    const string_bytes = &astgen.string_bytes;
    const msg: Zir.NullTerminatedString = @enumFromInt(string_bytes.items.len);
    try string_bytes.writer(astgen.gpa).print(format ++ "\x00", args);
    return astgen.addExtra(Zir.Inst.CompileErrors.Item{
        .msg = msg,
        .node = 0,
        .token = token,
        .byte_offset = byte_offset,
        .notes = 0,
        .kind = .note,
    });
}

fn errNoteNode(
    astgen: *AstGen,
    node: Ast.Node.Index,
    comptime format: []const u8,
    args: anytype,
) Allocator.Error!u32 {
    @branchHint(.cold);
    const string_bytes = &astgen.string_bytes;
    const msg: Zir.NullTerminatedString = @enumFromInt(string_bytes.items.len);
    try string_bytes.writer(astgen.gpa).print(format ++ "\x00", args);
    return astgen.addExtra(Zir.Inst.CompileErrors.Item{
        .msg = msg,
        .node = node,
        .token = 0,
        .byte_offset = 0,
        .notes = 0,
        .kind = .note,
    });
}

fn tokenAsString(astgen: *AstGen, token: Ast.Token.Index) !Zir.NullTerminatedString {
    const token_bytes = astgen.tree.tokenSlice(token);
    return astgen.bytesAsString(token_bytes);
}

fn strLitAsString(astgen: *AstGen, str_lit_token: Ast.Token.Index) !Zir.NullTerminatedString {
    const token_bytes = astgen.tree.tokenSlice(str_lit_token);
    return astgen.bytesAsString(token_bytes[1 .. token_bytes.len - 1]);
}

fn symLitAsString(astgen: *AstGen, sym_lit_token: Ast.Token.Index) !Zir.NullTerminatedString {
    const token_bytes = astgen.tree.tokenSlice(sym_lit_token);
    return astgen.bytesAsString(token_bytes[1..]);
}

fn bytesAsString(astgen: *AstGen, bytes: []const u8) !Zir.NullTerminatedString {
    const gpa = astgen.gpa;
    const string_bytes = &astgen.string_bytes;
    const str_index: u32 = @intCast(string_bytes.items.len);
    try string_bytes.appendSlice(gpa, bytes);
    const key: []const u8 = string_bytes.items[str_index..];
    const gop = try astgen.string_table.getOrPutContextAdapted(gpa, key, StringIndexAdapter{
        .bytes = string_bytes,
    }, StringIndexContext{
        .bytes = string_bytes,
    });
    if (gop.found_existing) {
        string_bytes.shrinkRetainingCapacity(str_index);
        return @enumFromInt(gop.key_ptr.*);
    } else {
        gop.key_ptr.* = str_index;
        try string_bytes.append(gpa, 0);
        return @enumFromInt(str_index);
    }
}
