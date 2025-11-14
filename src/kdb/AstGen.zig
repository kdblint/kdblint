const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const StringIndexAdapter = std.hash_map.StringIndexAdapter;
const StringIndexContext = std.hash_map.StringIndexContext;
const Timer = std.time.Timer;

const kdb = @import("root.zig");
const Ast = kdb.Ast;
const Zir = kdb.Zir;
const DocumentScope = kdb.DocumentScope;
const number_parser = kdb.number_parser;

const AstGen = @This();

gpa: Allocator,
arena: Allocator,
context: *DocumentScope.ScopeContext,
scope: DocumentScope.ScopeContext.PushedScope,
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
imports: std.AutoArrayHashMapUnmanaged(Zir.NullTerminatedString, Ast.TokenIndex) = .empty,
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
            // Ast.TokenIndex is missing because it is a u32.
            Ast.OptionalTokenIndex,
            Ast.Node.Index,
            Ast.Node.OptionalIndex,
            => @intFromEnum(@field(extra, field.name)),

            Ast.TokenOffset,
            Ast.OptionalTokenOffset,
            Ast.Node.Offset,
            Ast.Node.OptionalOffset,
            => @bitCast(@intFromEnum(@field(extra, field.name))),

            i32,
            => @bitCast(@field(extra, field.name)),

            else => |t| @compileError("bad field type: " ++ @typeName(t)),
        };
        i += 1;
    }
}

pub fn generate(gpa: Allocator, context: *DocumentScope.ScopeContext) Allocator.Error!Zir {
    const tree = context.tree;

    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();

    var astgen: AstGen = .{
        .gpa = gpa,
        .arena = arena.allocator(),
        .context = context,
        .scope = try context.startScope(.container, .root, .{
            .start = 0,
            .end = @intCast(tree.source.len),
        }),
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
        .decl_node_index = .root,
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

        try astgen.scope.finalize();

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
    const tree = astgen.context.tree;
    var scope = parent_scope;

    assert(parent_scope.isGlobal());

    const file_inst = try gz.makeFile();

    const blocks = tree.getBlocks();
    try astgen.scanExprs(blocks, .{
        .global_decls = &parent_scope.top().global_decls,
    });
    for (blocks, 0..) |node, i| {
        // TODO: Namespace block.
        // TODO: Something should wrap expr to return an index to show or discard value.

        astgen.advanceSourceCursorToNode(node);
        const ref, scope = expr(gz, scope, node) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.AnalysisFail => continue,
        };

        if (i < blocks.len - 1 and gz.endsWithNoReturn()) {
            try gz.astgen.appendErrorNodeNotes(blocks[i + 1], "unreachable code", .{}, &.{
                try gz.astgen.errNoteNode(node, "control flow is diverted here", .{}),
            }, .warn);
        }

        if (ref.toIndex()) |inst| {
            if (astgen.instructions.items(.tag)[@intFromEnum(inst)] == .apply) {
                const data: Zir.Inst.Data = astgen.instructions.items(.data)[@intFromEnum(inst)];
                const callee: Zir.Inst.Ref = @enumFromInt(astgen.extra.items[data.pl_node.payload_index]);
                if (callee == .assign) continue;
            }

            if (tree.tokens.items(.tag)[tree.lastToken(node) + 1] != .semicolon) {
                _ = try gz.addUnNode(.print, ref, node);
            }
        } else if (tree.tokens.items(.tag)[tree.lastToken(node) + 1] == .semicolon) {
            _ = try gz.addUnNode(.discard, ref, node);
        } else {
            _ = try gz.addUnNode(.print, ref, node);
        }
    }
    try gz.setFile(file_inst);

    return file_inst.toRef();
}

const Result = struct { Zir.Inst.Ref, *Scope };

fn expr(gz: *GenZir, scope: *Scope, src_node: Ast.Node.Index) InnerError!Result {
    const astgen = gz.astgen;
    const tree = astgen.context.tree;

    assert(src_node != .root);

    const node = tree.unwrapGroupedExpr(src_node);
    switch (tree.nodeTag(node)) {
        .root => unreachable,
        .empty => return .{ .null, scope },

        .grouped_expression => unreachable,
        .empty_list => return .{ .empty_list, scope },
        .list => return listExpr(gz, scope, node),
        .table_literal => return tableLiteral(gz, scope, node),

        .lambda,
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
        .system => return .{ try system(gz, node), scope },

        .select => return select(gz, scope, node),
        .exec => return exec(gz, scope, node),
        .update => return update(gz, scope, node),
        .delete_rows => return deleteRows(gz, scope, node),
        .delete_cols => return deleteCols(gz, scope, node),
    }
}

fn listExpr(gz: *GenZir, parent_scope: *Scope, src_node: Ast.Node.Index) InnerError!Result {
    const astgen = gz.astgen;
    const gpa = astgen.gpa;
    const tree = astgen.context.tree;
    var scope = parent_scope;

    assert(tree.nodeTag(src_node) == .list);

    const nodes = tree.extraDataSlice(tree.nodeData(src_node).extra_range, Ast.Node.Index);

    const list = try gpa.alloc(Zir.Inst.Ref, nodes.len);
    defer gpa.free(list);

    var i: usize = 1;
    var it = std.mem.reverseIterator(nodes);
    while (it.next()) |node| : (i += 1) {
        list[nodes.len - i], scope = try expr(gz, scope, node);
    }

    const result = try gz.addPlNode(.list, src_node, Zir.Inst.List{
        .len = @intCast(nodes.len),
    });
    try gz.astgen.extra.appendSlice(gpa, @ptrCast(list));
    return .{ result, scope };
}

fn tableLiteral(gz: *GenZir, parent_scope: *Scope, src_node: Ast.Node.Index) InnerError!Result {
    const astgen = gz.astgen;
    const gpa = astgen.gpa;
    const tree = astgen.context.tree;
    var scope = parent_scope;

    const table = tree.fullTable(src_node);

    var column_items: std.ArrayListUnmanaged(Zir.Inst.Table.Item) = try .initCapacity(gpa, table.columns.len);
    defer column_items.deinit(gpa);

    var columns_it = std.mem.reverseIterator(table.columns);
    while (columns_it.next()) |column_node| {
        const ident_name: Zir.NullTerminatedString, const node = switch (tree.nodeTag(column_node)) {
            .apply_binary => blk: {
                const lhs_node, const maybe_rhs = tree.nodeData(column_node).node_and_opt_node;
                const lhs = tree.unwrapGroupedExpr(lhs_node);
                const op: Ast.Node.Index = @enumFromInt(tree.nodeMainToken(column_node));

                if (maybe_rhs.unwrap()) |rhs| {
                    if (tree.nodeTag(op) == .colon and tree.nodeTag(lhs) == .identifier) {
                        const ident_token = tree.nodeMainToken(lhs);
                        const ident_bytes = tree.tokenSlice(ident_token);
                        const ident_name = try astgen.bytesAsString(ident_bytes);

                        break :blk .{ ident_name, rhs };
                    }
                }

                break :blk .{ .empty, column_node };
            },
            .call => blk: {
                const full_call = tree.fullCall(column_node);
                if (full_call.args.len == 2) {
                    const lhs = tree.unwrapGroupedExpr(full_call.args[0]);
                    const op = tree.unwrapGroupedExpr(full_call.func);
                    const rhs = full_call.args[1];
                    if (tree.nodeTag(op) == .colon and tree.nodeTag(lhs) == .identifier and tree.nodeTag(rhs) != .empty) {
                        const ident_token = tree.nodeMainToken(lhs);
                        const ident_bytes = tree.tokenSlice(ident_token);
                        const ident_name = try astgen.bytesAsString(ident_bytes);

                        break :blk .{ ident_name, rhs };
                    }
                }

                break :blk .{ .empty, column_node };
            },
            else => .{ .empty, column_node },
        };

        const ref, scope = try expr(gz, scope, node);
        column_items.appendAssumeCapacity(.{
            .name = ident_name,
            .ref = ref,
        });
    }

    var key_items: std.ArrayListUnmanaged(Zir.Inst.Table.Item) = try .initCapacity(gpa, table.keys.len);
    defer key_items.deinit(gpa);

    var keys_it = std.mem.reverseIterator(table.keys);
    while (keys_it.next()) |key_node| {
        const ident_name: Zir.NullTerminatedString, const node = switch (tree.nodeTag(key_node)) {
            .apply_binary => blk: {
                var lhs, const maybe_rhs = tree.nodeData(key_node).node_and_opt_node;
                lhs = tree.unwrapGroupedExpr(lhs);
                const op: Ast.Node.Index = @enumFromInt(tree.nodeMainToken(key_node));

                if (maybe_rhs.unwrap()) |rhs| {
                    if (tree.nodeTag(op) == .colon and tree.nodeTag(lhs) == .identifier) {
                        const ident_token = tree.nodeMainToken(lhs);
                        const ident_bytes = tree.tokenSlice(ident_token);
                        const ident_name = try astgen.bytesAsString(ident_bytes);

                        break :blk .{ ident_name, rhs };
                    }
                }

                break :blk .{ .empty, key_node };
            },
            .call => blk: {
                const nodes = tree.extraDataSlice(tree.nodeData(key_node).extra_range, Ast.Node.Index);
                if (nodes.len == 3) {
                    const lhs = tree.unwrapGroupedExpr(nodes[1]);
                    const op = tree.unwrapGroupedExpr(nodes[0]);
                    const rhs = nodes[2];
                    if (tree.nodeTag(op) == .colon and tree.nodeTag(lhs) == .identifier) {
                        const ident_token = tree.nodeMainToken(lhs);
                        const ident_bytes = tree.tokenSlice(ident_token);
                        const ident_name = try astgen.bytesAsString(ident_bytes);

                        break :blk .{ ident_name, rhs };
                    }
                }

                break :blk .{ .empty, key_node };
            },
            else => .{ .empty, key_node },
        };

        const ref, scope = try expr(gz, scope, node);
        key_items.appendAssumeCapacity(.{
            .name = ident_name,
            .ref = ref,
        });
    }

    return .{
        try gz.addTable(src_node, .{
            .keys = key_items.items,
            .columns = column_items.items,
        }),
        scope,
    };
}

fn lambda(gz: *GenZir, scope: *Scope, node: Ast.Node.Index) InnerError!Result {
    const astgen = gz.astgen;
    const gpa = astgen.gpa;
    const tree = astgen.context.tree;

    const prev_offset = astgen.source_offset;
    const prev_line = astgen.source_line;
    const prev_column = astgen.source_column;
    const prev_scope = astgen.scope;
    defer {
        astgen.source_offset = prev_offset;
        astgen.source_line = prev_line;
        astgen.source_column = prev_column;
        astgen.scope = prev_scope;
    }

    astgen.advanceSourceCursorToNode(node);
    const lbrace_line = astgen.source_line - gz.decl_line;
    const lbrace_column = astgen.source_column;

    const lambda_inst = try gz.makeLambda(node);

    const full_lambda = tree.fullLambda(node);

    astgen.scope = try astgen.context.startScope(.function, node, .{
        .start = tree.tokenStart(full_lambda.l_brace),
        .end = tree.tokenStart(full_lambda.r_brace) + 1,
    });
    errdefer astgen.scope.finalize() catch |err| switch (err) {
        error.OutOfMemory => @panic("OOM"),
    };

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

        for (p.params, 0..) |param_node, i| {
            if (tree.nodeTag(param_node) == .identifier) {
                const param_token = tree.nodeMainToken(param_node);
                const param_bytes = tree.tokenSlice(param_token);
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
                    try astgen.scope.pushDeclaration(param_token, .{
                        .function_parameter = .{ .param_index = @intCast(i), .func = node },
                    });
                }
            } else {
                assert(tree.nodeTag(param_node) == .empty);
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
            const ident_token = tree.nodeMainToken(ident_node);
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
            try astgen.scope.pushDeclaration(ident_token, .{
                .function_parameter = .{ .param_index = 0, .func = node },
            });
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
            try astgen.scope.pushDeclaration(full_lambda.l_brace, .{
                .function_parameter = .{ .param_index = 0, .func = node },
            });
        }

        if (ident_y) |ident_node| {
            const ident_token = tree.nodeMainToken(ident_node);
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
            try astgen.scope.pushDeclaration(ident_token, .{
                .function_parameter = .{ .param_index = 1, .func = node },
            });
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
            try astgen.scope.pushDeclaration(full_lambda.l_brace, .{
                .function_parameter = .{ .param_index = 1, .func = node },
            });
        }

        if (identifiers.get("z")) |ident_node| {
            const ident_token = tree.nodeMainToken(ident_node);
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
            try astgen.scope.pushDeclaration(ident_token, .{
                .function_parameter = .{ .param_index = 2, .func = node },
            });
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

        if (i + 1 < full_lambda.body.len) {
            _, params_scope = try expr(&fn_gz, params_scope, body_node);
        } else if (tree.nodeTag(body_node) == .empty) {
            _ = try fn_gz.addUnTok(.ret_implicit, .null, full_lambda.r_brace);
        } else {
            const ref, params_scope = try expr(&fn_gz, params_scope, body_node);
            _ = try fn_gz.addUnNode(.ret_node, ref, body_node);
        }
    }

    try astgen.scope.finalize();

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
    const tree = astgen.context.tree;
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
    const tree = astgen.context.tree;

    const nodes = tree.extraDataSlice(tree.nodeData(node).extra_range, Ast.Node.Index);

    var scope = parent_scope;
    if (nodes.len > 0) {
        for (nodes[0 .. nodes.len - 1]) |n| {
            _, scope = try expr(gz, scope, n);
        }

        const n = nodes[nodes.len - 1];
        const inst, scope = try expr(gz, scope, n);
        if (tree.isCompoundAssignment(n)) return .{ .null, scope };
        return .{ inst, scope };
    }

    return .{ .null, scope };
}

fn findOrCreateGlobal(
    gz: *GenZir,
    parent_scope: *Scope,
    src_node: Ast.Node.Index,
    op: Zir.Inst.Ref,
    rhs: Zir.Inst.Ref,
    ident_name: Zir.NullTerminatedString,
    ident_token: Ast.TokenIndex,
) InnerError!Result {
    const astgen = gz.astgen;
    var scope = parent_scope;

    if (scope.findGlobal(ident_name)) |lhs| {
        return .{ try gz.addApply(src_node, op, &.{ lhs.inst, rhs }), scope };
    }

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
    // try astgen.scope.pushDeclaration(ident_token, .{ .ast_node = src_node });

    return .{ try gz.addApply(src_node, op, &.{ lhs, rhs }), scope };
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
    const tree = astgen.context.tree;
    var scope = parent_scope;

    assert(src_node != .root);
    assert(ident_node != .root);
    assert(tree.nodeTag(ident_node) == .identifier);
    assert(expr_node != .root);

    const rhs, scope = try expr(gz, scope, expr_node);
    const op, scope = try expr(gz, scope, op_node);

    const ident_token = tree.nodeMainToken(ident_node);
    const ident_bytes = tree.tokenSlice(ident_token);
    const ident_name = try astgen.bytesAsString(ident_bytes);

    if (parent_scope.isGlobal()) {
        return findOrCreateGlobal(
            gz,
            scope,
            src_node,
            op,
            rhs,
            ident_name,
            ident_token,
        );
    }

    if (is_global_assign) {
        if (parent_scope.lambda().?.local_decls.get(ident_name)) |local_decl| {
            const local_ident = switch (tree.nodeTag(local_decl)) {
                .identifier => local_decl,
                .apply_binary => tree.unwrapGroupedExpr(tree.nodeData(local_decl).node_and_opt_node[0]),
                .call => blk: {
                    const nodes = tree.extraDataSlice(tree.nodeData(local_decl).extra_range, Ast.Node.Index);
                    break :blk tree.unwrapGroupedExpr(nodes[0]);
                },
                else => unreachable,
            };
            assert(tree.nodeTag(local_ident) == .identifier);

            if (scope.findLocal(ident_name)) |local_val| { // local before global
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
            return findOrCreateGlobal(
                gz,
                scope,
                src_node,
                op,
                rhs,
                ident_name,
                ident_token,
            );
        }
    }

    if (std.mem.indexOfScalar(u8, ident_bytes, '.')) |_| {
        return findOrCreateGlobal(
            gz,
            scope,
            src_node,
            op,
            rhs,
            ident_name,
            ident_token,
        );
    }

    if (scope.findLocal(ident_name)) |lhs| {
        return .{ try gz.addApply(src_node, op, &.{ lhs.inst, rhs }), scope };
    } else {
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
        try astgen.scope.pushDeclaration(ident_token, .{ .ast_node = src_node });

        return .{ try gz.addApply(src_node, op, &.{ lhs, rhs }), scope };
    }
}

// TODO: a:+/[x]
// TODO: a:/[+]x - error, cannot apply iterator to ':'
// TODO: a:(/[+]x)
// TODO: a:(/[+;x]) - error, rank
fn iterator(gz: *GenZir, parent_scope: *Scope, src_node: Ast.Node.Index) InnerError!Result {
    const astgen = gz.astgen;
    const tree = astgen.context.tree;
    var scope = parent_scope;

    const tag: Zir.Inst.Ref = switch (tree.nodeTag(src_node)) {
        .apostrophe => .each,
        .apostrophe_colon => .each_prior,
        .slash => .over,
        .slash_colon => .each_right,
        .backslash => .scan,
        .backslash_colon => .each_left,
        else => unreachable,
    };

    if (tree.nodeData(src_node).opt_node.unwrap()) |lhs| {
        const lhs_ref, scope = try expr(gz, scope, lhs);
        if (lhs_ref == .assign) {
            return astgen.failNode(lhs, "cannot apply iterator to assignment", .{});
        }

        return .{ try gz.addApply(src_node, tag, &.{lhs_ref}), scope };
    } else return .{ tag, scope };
}

fn call(gz: *GenZir, parent_scope: *Scope, src_node: Ast.Node.Index) InnerError!Result {
    const astgen = gz.astgen;
    const tree = astgen.context.tree;
    var scope = parent_scope;

    assert(tree.nodeTag(src_node) == .call);

    const full_call = tree.fullCall(src_node);
    switch (tree.nodeTag(full_call.func)) {
        .root,
        .empty,
        => unreachable,

        .grouped_expression => unreachable,
        .empty_list,
        .list,
        .table_literal,
        => {},

        .lambda,
        => {},

        .expr_block => {},

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
            if (tree.nodeTag(ident_node) != .identifier) return astgen.failNode(
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

        .dollar => switch (full_call.args.len) {
            0 => unreachable,
            1 => return astgen.failNode(
                src_node,
                "expected 2 or more argument(s), found {d}",
                .{full_call.args.len},
            ),
            2 => {},
            else => return cond(gz, scope, full_call),
        },

        .bang => switch (full_call.args.len) {
            0 => unreachable,
            2, 4 => {},
            else => return astgen.failNode(
                src_node,
                "expected 2 or 4 argument(s), found {d}",
                .{full_call.args.len},
            ),
        },

        .question_mark => switch (full_call.args.len) {
            0 => unreachable,
            2, 3, 4, 5, 6 => {},
            else => return astgen.failNode(
                src_node,
                "expected 2, 3, 4, 5, or 6 argument(s), found {d}",
                .{full_call.args.len},
            ),
        },

        .at,
        .period,
        => switch (full_call.args.len) {
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
        => if (tree.nodeData(full_call.func).opt_node == .none) switch (full_call.args.len) {
            0 => unreachable,
            1 => {},
            else => return astgen.failNode(
                full_call.func,
                "expected 1 argument(s), found {d}",
                .{full_call.args.len},
            ),
        },

        .call,
        .apply_unary,
        .apply_binary,
        => {},

        .number_literal,
        .number_list_literal,
        .string_literal,
        .symbol_literal,
        .symbol_list_literal,
        => {},

        .identifier,
        => {
            const slice = tree.tokenSlice(tree.nodeMainToken(full_call.func));
            if (std.mem.eql(u8, slice, "do")) {
                return doStatement(gz, scope, src_node, full_call);
            } else if (std.mem.eql(u8, slice, "if")) {
                return ifStatement(gz, scope, src_node, full_call);
            } else if (std.mem.eql(u8, slice, "while")) {
                return whileStatement(gz, scope, src_node, full_call);
            }
        },

        .builtin,
        => {},

        .select,
        .exec,
        .update,
        .delete_rows,
        .delete_cols,
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

fn cond(gz: *GenZir, parent_scope: *Scope, full_call: Ast.full.Call) InnerError!Result {
    const astgen = gz.astgen;
    var scope = parent_scope;

    assert(full_call.args.len > 2);

    const condition, scope = try expr(gz, scope, full_call.args[0]);
    _ = condition; // autofix

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

    const args = full_call.args[1..];
    for (args[0 .. args.len - 1]) |arg| {
        _, scope = try expr(gz, scope, arg);
    }
    const inst, scope = try expr(gz, scope, args[args.len - 1]);

    var temp_scope = scope;
    while (temp_scope != prev_scope) switch (temp_scope.tag) {
        .local_val => {
            const local_val = temp_scope.cast(Scope.LocalVal).?;
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

fn doStatement(
    gz: *GenZir,
    parent_scope: *Scope,
    src_node: Ast.Node.Index,
    full_call: Ast.full.Call,
) InnerError!Result {
    const astgen = gz.astgen;
    const tree = astgen.context.tree;
    var scope = parent_scope;

    assert(full_call.args.len > 0);

    const prev_offset = astgen.source_offset;
    const prev_line = astgen.source_line;
    const prev_column = astgen.source_column;
    defer {
        astgen.source_offset = prev_offset;
        astgen.source_line = prev_line;
        astgen.source_column = prev_column;
    }

    astgen.advanceSourceCursorToNode(src_node);
    const inst = try gz.makePlNode(.do, src_node);

    const condition, scope = try expr(gz, scope, full_call.args[0]);

    const prev_scope = scope;

    var body_gz = gz.makeSubBlock(scope);
    defer body_gz.unstack();

    const args = full_call.args[1..];
    for (args, 0..) |body_node, i| {
        if (body_gz.endsWithNoReturn() and tree.nodeTag(body_node) != .empty) {
            assert(i > 0);
            try gz.astgen.appendErrorNodeNotes(body_node, "unreachable code", .{}, &.{
                try gz.astgen.errNoteNode(args[i - 1], "control flow is diverted here", .{}),
            }, .warn);
        }

        _, scope = try expr(&body_gz, scope, body_node);
    }

    var temp_scope = scope;
    while (temp_scope != prev_scope) switch (temp_scope.tag) {
        .local_val => {
            const local_val = temp_scope.cast(Scope.LocalVal).?;
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

    try gz.setDo(inst, .{
        .condition = condition,
        .body_gz = &body_gz,
    });

    return .{ inst.toRef(), scope };
}

fn ifStatement(
    gz: *GenZir,
    parent_scope: *Scope,
    src_node: Ast.Node.Index,
    full_call: Ast.full.Call,
) InnerError!Result {
    const astgen = gz.astgen;
    const tree = astgen.context.tree;
    var scope = parent_scope;

    assert(full_call.args.len > 0);

    const prev_offset = astgen.source_offset;
    const prev_line = astgen.source_line;
    const prev_column = astgen.source_column;
    defer {
        astgen.source_offset = prev_offset;
        astgen.source_line = prev_line;
        astgen.source_column = prev_column;
    }

    astgen.advanceSourceCursorToNode(src_node);
    const inst = try gz.makePlNode(.@"if", src_node);

    const condition, scope = try expr(gz, scope, full_call.args[0]);

    const prev_scope = scope;

    var body_gz = gz.makeSubBlock(scope);
    defer body_gz.unstack();

    const args = full_call.args[1..];
    for (args, 0..) |body_node, i| {
        if (body_gz.endsWithNoReturn() and tree.nodeTag(body_node) != .empty) {
            assert(i > 0);
            try gz.astgen.appendErrorNodeNotes(body_node, "unreachable code", .{}, &.{
                try gz.astgen.errNoteNode(args[i - 1], "control flow is diverted here", .{}),
            }, .warn);
        }

        _, scope = try expr(&body_gz, scope, body_node);
    }

    var temp_scope = scope;
    while (temp_scope != prev_scope) switch (temp_scope.tag) {
        .local_val => {
            const local_val = temp_scope.cast(Scope.LocalVal).?;
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

    try gz.setIf(inst, .{
        .condition = condition,
        .body_gz = &body_gz,
    });

    return .{ inst.toRef(), scope };
}

fn whileStatement(
    gz: *GenZir,
    parent_scope: *Scope,
    src_node: Ast.Node.Index,
    full_call: Ast.full.Call,
) InnerError!Result {
    const astgen = gz.astgen;
    const tree = astgen.context.tree;
    var scope = parent_scope;

    assert(full_call.args.len > 0);

    const prev_offset = astgen.source_offset;
    const prev_line = astgen.source_line;
    const prev_column = astgen.source_column;
    defer {
        astgen.source_offset = prev_offset;
        astgen.source_line = prev_line;
        astgen.source_column = prev_column;
    }

    astgen.advanceSourceCursorToNode(src_node);
    const inst = try gz.makePlNode(.@"while", src_node);

    const condition, scope = try expr(gz, scope, full_call.args[0]);

    const prev_scope = scope;

    var body_gz = gz.makeSubBlock(scope);
    defer body_gz.unstack();

    const args = full_call.args[1..];
    for (args, 0..) |body_node, i| {
        if (body_gz.endsWithNoReturn() and tree.nodeTag(body_node) != .empty) {
            assert(i > 0);
            try gz.astgen.appendErrorNodeNotes(body_node, "unreachable code", .{}, &.{
                try gz.astgen.errNoteNode(args[i - 1], "control flow is diverted here", .{}),
            }, .warn);
        }

        _, scope = try expr(&body_gz, scope, body_node);
    }

    var temp_scope = scope;
    while (temp_scope != prev_scope) switch (temp_scope.tag) {
        .local_val => {
            const local_val = temp_scope.cast(Scope.LocalVal).?;
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

    try gz.setWhile(inst, .{
        .condition = condition,
        .body_gz = &body_gz,
    });

    return .{ inst.toRef(), scope };
}

fn applyUnary(gz: *GenZir, parent_scope: *Scope, src_node: Ast.Node.Index) InnerError!Result {
    const astgen = gz.astgen;
    const tree = astgen.context.tree;
    var scope = parent_scope;

    assert(tree.nodeTag(src_node) == .apply_unary);

    const lhs, const rhs = tree.nodeData(src_node).node_and_node;

    switch (tree.nodeTag(lhs)) {
        .grouped_expression => switch (tree.nodeTag(tree.unwrapGroupedExpr(lhs))) {
            .colon => return astgen.failNode(
                lhs,
                "return should not be surrounded by parentheses",
                .{},
            ),
            else => {},
        },
        .empty_list => {},
        .list => {},
        .table_literal => {},

        .lambda,
        => {},

        .colon => if (parent_scope.lambda()) |_| {
            const rhs_ref, scope = try expr(gz, scope, rhs);
            const ref = try gz.addUnNode(.ret_node, rhs_ref, src_node);
            return .{ ref, scope };
        } else return astgen.failNode(
            lhs,
            "return outside function scope",
            .{},
        ),

        .apostrophe => {
            const rhs_ref, scope = try expr(gz, scope, rhs);
            const ref = try gz.addUnNode(.signal, rhs_ref, src_node);
            return .{ ref, scope };
        },

        .call => {},

        .number_literal,
        .symbol_list_literal,
        .identifier,
        .builtin, // TODO: https://kdblint.atlassian.net/browse/KLS-311
        => {},

        else => |t| {
            try astgen.appendErrorNode(src_node, "unary NYI: {s}", .{@tagName(t)}, .@"error");
            return .{ .nyi, scope };
        },
    }

    const rhs_ref, scope = try expr(gz, scope, rhs);
    const lhs_ref, scope = try expr(gz, scope, lhs);
    return .{ try gz.addApply(src_node, lhs_ref, &.{rhs_ref}), scope };
}

fn applyBinary(gz: *GenZir, parent_scope: *Scope, src_node: Ast.Node.Index) InnerError!Result {
    const astgen = gz.astgen;
    const tree = astgen.context.tree;
    var scope = parent_scope;

    assert(tree.nodeTag(src_node) == .apply_binary);

    const lhs, const maybe_rhs = tree.nodeData(src_node).node_and_opt_node;

    const op_node: Ast.Node.Index = @enumFromInt(tree.nodeMainToken(src_node));
    switch (tree.nodeTag(op_node)) {
        inline .colon, .colon_colon => |t| {
            const rhs = maybe_rhs.unwrap() orelse return astgen.failNode(src_node, "binary TODO", .{});
            const ident_node = tree.unwrapGroupedExpr(lhs);
            switch (tree.nodeTag(ident_node)) {
                .identifier => return assign(
                    gz,
                    scope,
                    src_node,
                    op_node,
                    ident_node,
                    rhs,
                    t == .colon_colon,
                ),
                // TODO: Needs more work - result location?
                .call => {
                    const full_call = tree.fullCall(ident_node);
                    switch (tree.nodeTag(full_call.func)) {
                        .identifier => return assign(
                            gz,
                            scope,
                            src_node,
                            op_node,
                            full_call.func,
                            rhs,
                            t == .colon_colon,
                        ),
                        else => return astgen.failNode(src_node, "{}", .{tree.nodeTag(full_call.func)}),
                    }
                },
                else => return astgen.failNode(src_node, "{}", .{tree.nodeTag(ident_node)}),
            }
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

    const rhs_ref: Zir.Inst.Ref = if (maybe_rhs.unwrap()) |rhs| rhs_ref: {
        const rhs_ref, scope = try expr(gz, scope, rhs);
        break :rhs_ref rhs_ref;
    } else .none;
    const op, scope = try expr(gz, scope, op_node);
    const lhs_ref, scope = try expr(gz, scope, lhs);

    var ref = try gz.addApply(src_node, op, &.{ lhs_ref, rhs_ref });
    if (tree.nodeTag(op_node).isCompoundAssignment()) {
        ref = try gz.addApply(src_node, .assign, &.{ lhs_ref, ref });
    }
    return .{ ref, scope };
}

fn numberLiteral(gz: *GenZir, node: Ast.Node.Index) InnerError!Zir.Inst.Ref {
    const astgen = gz.astgen;
    const gpa = astgen.gpa;
    const tree = astgen.context.tree;
    const num_token = tree.nodeMainToken(node);

    assert(tree.nodeTag(node) == .number_literal);

    const type_hint = number_parser.TypeHint.get(tree.tokenSlice(num_token)) catch return astgen.failNode(
        node,
        "Invalid number suffix",
        .{},
    );
    switch (type_hint) {
        .bool => {
            const slice = tree.tokenSlice(num_token);
            if (slice[0] == '-') return astgen.failWithNumberError(
                .{ .invalid_character = 0 },
                num_token,
                slice,
                0,
            );

            const scratch_top = astgen.scratch.items.len;
            defer astgen.scratch.shrinkRetainingCapacity(scratch_top);

            const len = slice.len - 1;
            try astgen.scratch.ensureUnusedCapacity(gpa, len);

            for (0..len) |i| {
                const bytes = slice[i .. i + 1];
                const ref: Zir.Inst.Ref = switch (number_parser.parseBool(bytes)) {
                    .bool => |value| if (value) .true else .false,
                    .failure => |err| return astgen.failWithNumberError(
                        err,
                        num_token,
                        bytes,
                        i,
                    ),
                    else => unreachable,
                };
                astgen.scratch.appendAssumeCapacity(@intFromEnum(ref));
            }

            const list = astgen.scratch.items[scratch_top..];
            assert(list.len == len);
            const bool_list = try gz.addPlNode(.bool_list, node, Zir.Inst.List{
                .len = @intCast(list.len),
            });
            try astgen.extra.appendSlice(gpa, list);
            return bool_list;
        },
        .byte => {
            const slice = tree.tokenSlice(num_token);
            if (slice[0] == '-') return astgen.failWithNumberError(
                .{ .invalid_character = 0 },
                num_token,
                slice,
                0,
            );

            const scratch_top = astgen.scratch.items.len;
            defer astgen.scratch.shrinkRetainingCapacity(scratch_top);

            const len = ((slice.len - 2) + (slice.len - 2) % 2) / 2;
            try astgen.scratch.ensureUnusedCapacity(gpa, len);

            if ((slice.len - 2) % 2 == 0) {
                for (0..len) |i| {
                    const bytes = slice[2..][(i * 2) .. (i * 2) + 2];
                    const ref: Zir.Inst.Ref = switch (number_parser.parseByte(bytes)) {
                        .byte => |value| try gz.addByte(value),
                        .failure => |err| return astgen.failWithNumberError(
                            err,
                            num_token,
                            bytes,
                            2 + i * 2,
                        ),
                        else => unreachable,
                    };
                    astgen.scratch.appendAssumeCapacity(@intFromEnum(ref));
                }
            } else {
                {
                    const bytes = slice[2..][0..1];
                    const ref: Zir.Inst.Ref = switch (number_parser.parseByte(bytes)) {
                        .byte => |value| try gz.addByte(value),
                        .failure => |err| return astgen.failWithNumberError(
                            err,
                            num_token,
                            bytes,
                            2,
                        ),
                        else => unreachable,
                    };
                    astgen.scratch.appendAssumeCapacity(@intFromEnum(ref));
                }

                for (1..len) |i| {
                    const bytes = slice[1..][(i * 2) .. (i * 2) + 2];
                    const ref: Zir.Inst.Ref = switch (number_parser.parseByte(bytes)) {
                        .byte => |value| try gz.addByte(value),
                        .failure => |err| return astgen.failWithNumberError(
                            err,
                            num_token,
                            bytes,
                            1 + i * 2,
                        ),
                        else => unreachable,
                    };
                    astgen.scratch.appendAssumeCapacity(@intFromEnum(ref));
                }
            }

            const list = astgen.scratch.items[scratch_top..];
            assert(list.len == len);
            const byte_list = try gz.addPlNode(.byte_list, node, Zir.Inst.List{
                .len = @intCast(list.len),
            });
            try astgen.extra.appendSlice(gpa, list);
            return byte_list;
        },
        else => return parseNumberLiteral(gz, num_token, type_hint, true),
    }
}

fn parseNumberLiteral(
    gz: *GenZir,
    token: Ast.TokenIndex,
    type_hint: number_parser.TypeHint,
    allow_suffix: bool,
) InnerError!Zir.Inst.Ref {
    const astgen = gz.astgen;
    const tree = astgen.context.tree;
    const bytes = tree.tokenSlice(token);

    assert(type_hint != .bool);
    assert(type_hint != .byte);
    return switch (bytes[0]) {
        '-' => switch (number_parser.parse(bytes[1..], type_hint, allow_suffix)) {
            .bool => unreachable,
            .guid => astgen.failWithNumberError(
                .{ .invalid_character = 0 },
                token,
                bytes,
                0,
            ),
            .byte => unreachable,
            .short => |num| switch (num) {
                number_parser.null_short => astgen.failWithNumberError(
                    .{ .invalid_character = 0 },
                    token,
                    bytes,
                    0,
                ),
                number_parser.inf_short => .negative_inf_short,
                else => gz.addShort(-num),
            },
            .int => |num| switch (num) {
                number_parser.null_int => astgen.failWithNumberError(
                    .{ .invalid_character = 0 },
                    token,
                    bytes,
                    0,
                ),
                number_parser.inf_int => .negative_inf_int,
                else => gz.addInt(-num),
            },
            .long => |num| switch (num) {
                1 => .negative_one,
                number_parser.null_long => astgen.failWithNumberError(
                    .{ .invalid_character = 0 },
                    token,
                    bytes,
                    0,
                ),
                number_parser.inf_long => .negative_inf_long,
                else => gz.addLong(-num),
            },
            .real => |num| if (std.math.isNan(num))
                astgen.failWithNumberError(.{ .invalid_character = 0 }, token, bytes, 0)
            else if (std.math.isInf(num))
                .negative_inf_real
            else
                gz.addReal(-num),
            .float => |num| if (std.math.isNan(num))
                astgen.failWithNumberError(.{ .invalid_character = 0 }, token, bytes, 0)
            else if (std.math.isInf(num))
                .negative_inf_float
            else
                gz.addFloat(-num),
            .char => .null_char,
            .timestamp => |num| switch (num) {
                number_parser.null_long => astgen.failWithNumberError(
                    .{ .invalid_character = 0 },
                    token,
                    bytes,
                    0,
                ),
                number_parser.inf_long => .negative_inf_timestamp,
                else => gz.addTimestamp(-num),
            },
            .month => |num| switch (num) {
                number_parser.null_int => astgen.failWithNumberError(
                    .{ .invalid_character = 0 },
                    token,
                    bytes,
                    0,
                ),
                number_parser.inf_int => .negative_inf_month,
                else => gz.addMonth(-num),
            },
            .date => |num| switch (num) {
                number_parser.null_int => astgen.failWithNumberError(
                    .{ .invalid_character = 0 },
                    token,
                    bytes,
                    0,
                ),
                number_parser.inf_int => .negative_inf_date,
                else => gz.addDate(-num),
            },
            .datetime => |num| if (std.math.isNan(num))
                astgen.failWithNumberError(.{ .invalid_character = 0 }, token, bytes, 0)
            else if (std.math.isInf(num))
                .negative_inf_datetime
            else
                gz.addDatetime(-num),
            .timespan => |num| switch (num) {
                number_parser.null_long => if (std.mem.eql(u8, bytes[1..], "0n"))
                    gz.addTimespan(0)
                else
                    astgen.failWithNumberError(
                        .{ .invalid_character = 0 },
                        token,
                        bytes,
                        0,
                    ), // TODO: Test
                number_parser.inf_long => .negative_inf_timespan,
                else => gz.addTimespan(-num),
            },
            .minute => |num| switch (num) {
                number_parser.null_int => astgen.failWithNumberError(
                    .{ .invalid_character = 0 },
                    token,
                    bytes,
                    0,
                ),
                number_parser.inf_int => .negative_inf_minute,
                else => gz.addMinute(-num),
            },
            .second => |num| switch (num) {
                number_parser.null_int => astgen.failWithNumberError(
                    .{ .invalid_character = 0 },
                    token,
                    bytes,
                    0,
                ),
                number_parser.inf_int => .negative_inf_second,
                else => gz.addSecond(-num),
            },
            .time => |num| switch (num) {
                number_parser.null_int => astgen.failWithNumberError(
                    .{ .invalid_character = 0 },
                    token,
                    bytes,
                    0,
                ),
                number_parser.inf_int => .negative_inf_time,
                else => gz.addTime(-num),
            },
            .failure => |err| astgen.failWithNumberError(err, token, bytes, 1),
        },
        else => switch (number_parser.parse(bytes, type_hint, allow_suffix)) {
            .bool => unreachable,
            .guid => .null_guid,
            .byte => unreachable,
            .short => |num| switch (num) {
                number_parser.null_short => .null_short,
                number_parser.inf_short => .inf_short,
                else => gz.addShort(num),
            },
            .int => |num| switch (num) {
                number_parser.null_int => .null_int,
                number_parser.inf_int => .inf_int,
                else => gz.addInt(num),
            },
            .long => |num| switch (num) {
                0 => .zero,
                1 => .one,
                number_parser.null_long => .null_long,
                number_parser.inf_long => .inf_long,
                else => gz.addLong(num),
            },
            .real => |num| if (std.math.isNan(num))
                .null_real
            else if (std.math.isInf(num))
                .inf_real
            else
                gz.addReal(num),
            .float => |num| if (std.math.isNan(num))
                .null_float
            else if (std.math.isInf(num))
                .inf_float
            else
                gz.addFloat(num),
            .char => |num| switch (num) {
                '0'...'9' => gz.addChar(num),
                else => .null_char,
            },
            .timestamp => |num| switch (num) {
                number_parser.null_long => .null_timestamp,
                number_parser.inf_long => .inf_timestamp,
                else => gz.addTimestamp(num),
            },
            .month => |num| switch (num) {
                number_parser.null_int => .null_month,
                number_parser.inf_int => .inf_month,
                else => gz.addMonth(num),
            },
            .date => |num| switch (num) {
                number_parser.null_int => .null_date,
                number_parser.inf_int => .inf_date,
                else => gz.addDate(num),
            },
            .datetime => astgen.failWithNumberError(.nyi, token, bytes, 0),
            .timespan => |num| switch (num) {
                number_parser.null_long => .null_timespan,
                number_parser.inf_long => .inf_timespan,
                else => gz.addTimespan(num),
            },
            .minute => |num| switch (num) {
                number_parser.null_int => .null_minute,
                number_parser.inf_int => .inf_minute,
                else => gz.addMinute(num),
            },
            .second => |num| switch (num) {
                number_parser.null_int => .null_second,
                number_parser.inf_int => .inf_second,
                else => gz.addSecond(num),
            },
            .time => |num| switch (num) {
                number_parser.null_int => .null_time,
                number_parser.inf_int => .inf_time,
                else => gz.addTime(num),
            },
            .failure => |err| astgen.failWithNumberError(err, token, bytes, 0),
        },
    };
}

fn failWithNumberError(
    astgen: *AstGen,
    err: number_parser.Error,
    token: Ast.TokenIndex,
    bytes: []const u8,
    offset: usize,
) InnerError {
    switch (err) {
        .nyi => return astgen.failTok(token, "nyi: {s}", .{bytes}),
        .overflow => return astgen.failTok(token, "overflow", .{}),

        .upper_case_base => return astgen.failTok(token, "upper_case_base", .{}),
        .invalid_float_base => return astgen.failTok(token, "invalid_float_base", .{}),
        .repeated_underscore => return astgen.failTok(token, "repeated_underscore", .{}),
        .invalid_underscore_after_special => return astgen.failTok(token, "invalid_underscore_after_special", .{}),
        .invalid_digit => |info| return astgen.failOff(
            token,
            @intCast(info.i + offset),
            "invalid digit '{c}' for {s} base",
            .{ bytes[info.i], @tagName(info.base) },
        ),
        .invalid_digit_exponent => return astgen.failTok(token, "invalid_digit_exponent", .{}),
        .duplicate_period => return astgen.failTok(token, "duplicate_period", .{}),
        .duplicate_exponent => return astgen.failTok(token, "duplicate_exponent", .{}),
        .exponent_after_underscore => return astgen.failTok(token, "exponent_after_underscore", .{}),
        .special_after_underscore => return astgen.failTok(token, "special_after_underscore", .{}),
        .trailing_special => return astgen.failTok(token, "trailing_special", .{}),
        .trailing_underscore => return astgen.failTok(token, "trailing_underscore", .{}),
        .invalid_character => |i| return astgen.failOff(
            token,
            @intCast(i + offset),
            "invalid character '{c}'",
            .{bytes[i]},
        ),
        .invalid_exponent_sign => return astgen.failTok(token, "invalid_exponent_sign", .{}),
        .period_after_exponent => return astgen.failTok(token, "period_after_exponent", .{}),

        // TODO: These should be warnings
        .prefer_short_inf => return astgen.failTok(token, "prefer 0Wh", .{}),
        .prefer_int_inf => return astgen.failTok(token, "prefer 0Wi", .{}),
        .prefer_long_inf => return astgen.failTok(token, "prefer 0W", .{}),
        .prefer_month_inf => return astgen.failTok(token, "prefer 0Wm", .{}),
    }
}

fn numberListLiteral(gz: *GenZir, node: Ast.Node.Index) InnerError!Zir.Inst.Ref {
    const astgen = gz.astgen;
    const gpa = astgen.gpa;
    const tree = astgen.context.tree;

    assert(tree.nodeTag(node) == .number_list_literal);

    const scratch_top = astgen.scratch.items.len;
    defer astgen.scratch.shrinkRetainingCapacity(scratch_top);

    const first_token = tree.nodeMainToken(node);
    const last_token = tree.nodeData(node).token;
    const len = last_token - first_token + 1;
    try astgen.scratch.ensureUnusedCapacity(gpa, len);

    const type_hint = number_parser.TypeHint.get(tree.tokenSlice(last_token)) catch return astgen.failNode(
        node,
        "Invalid number suffix",
        .{},
    );
    switch (type_hint) {
        .bool,
        .byte,
        .float,
        => return astgen.failNode(node, "NYI: {s} '{s}'", .{
            @tagName(type_hint),
            tree.getNodeSource(node),
        }),
        else => {},
    }

    // TODO: Backtrack and change number list types.

    var i: u32 = first_token;
    while (i < last_token) : (i += 1) {
        const ref = try parseNumberLiteral(gz, i, type_hint, false);
        astgen.scratch.appendAssumeCapacity(@intFromEnum(ref));
    }
    const ref = try parseNumberLiteral(gz, i, type_hint, true);
    astgen.scratch.appendAssumeCapacity(@intFromEnum(ref));

    // TODO: long_list
    const list = astgen.scratch.items[scratch_top..];
    assert(list.len == len);
    const long_list = try gz.addPlNode(.long_list, node, Zir.Inst.List{
        .len = @intCast(list.len),
    });
    try astgen.extra.appendSlice(gpa, list);
    return long_list;
}

fn stringLiteral(gz: *GenZir, node: Ast.Node.Index) InnerError!Zir.Inst.Ref {
    const astgen = gz.astgen;
    const tree = astgen.context.tree;

    assert(tree.nodeTag(node) == .string_literal);

    const str_token = tree.nodeMainToken(node);
    const str = try astgen.strLitAsString(str_token);
    return gz.addStrTok(.str, str, str_token);
}

fn symbolLiteral(gz: *GenZir, node: Ast.Node.Index) InnerError!Zir.Inst.Ref {
    const astgen = gz.astgen;
    const tree = astgen.context.tree;

    assert(tree.nodeTag(node) == .symbol_literal);

    const sym_token = tree.nodeMainToken(node);
    const sym = try astgen.symLitAsString(sym_token);
    return gz.addStrTok(.sym, sym, sym_token);
}

fn symbolListLiteral(gz: *GenZir, node: Ast.Node.Index) InnerError!Zir.Inst.Ref {
    const astgen = gz.astgen;
    const gpa = astgen.gpa;
    const tree = astgen.context.tree;

    assert(tree.nodeTag(node) == .symbol_list_literal);

    const scratch_top = astgen.scratch.items.len;
    defer astgen.scratch.shrinkRetainingCapacity(scratch_top);

    const first_token = tree.nodeMainToken(node);
    const last_token = tree.nodeData(node).token;
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
    const tree = astgen.context.tree;

    const ident_token = tree.nodeMainToken(node);
    const ident_bytes = tree.tokenSlice(ident_token);
    const ident_name = try astgen.bytesAsString(ident_bytes);

    if (scope.lambda()) |lambda_scope| if (lambda_scope.local_decls.get(ident_name)) |local_decl| {
        if (scope.findLocal(ident_name)) |local_val| {
            if (local_val.used == 0) local_val.used = ident_token;
            return .{ local_val.inst, scope };
        }

        const local_ident = switch (tree.nodeTag(local_decl)) {
            .identifier => local_decl,
            .apply_binary => tree.unwrapGroupedExpr(tree.nodeData(local_decl).node_and_opt_node[0]),
            .call => blk: {
                const nodes = tree.extraDataSlice(tree.nodeData(local_decl).extra_range, Ast.Node.Index);
                break :blk tree.unwrapGroupedExpr(nodes[0]);
            },
            else => unreachable,
        };
        assert(tree.nodeTag(local_ident) == .identifier);

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
    const tree = astgen.context.tree;

    assert(tree.nodeTag(node) == .builtin);

    const builtin_token = tree.nodeMainToken(node);
    const builtin_name = try astgen.tokenAsString(builtin_token);

    return gz.addStrTok(.builtin, builtin_name, builtin_token);
}

fn system(gz: *GenZir, node: Ast.Node.Index) InnerError!Zir.Inst.Ref {
    _ = gz; // autofix
    _ = node; // autofix
    return .nyi;
}

fn select(gz: *GenZir, scope: *Scope, node: Ast.Node.Index) InnerError!Result {
    _ = node; // autofix
    _ = gz; // autofix
    return .{ .nyi, scope };
}

fn exec(gz: *GenZir, scope: *Scope, node: Ast.Node.Index) InnerError!Result {
    _ = node; // autofix
    _ = gz; // autofix
    return .{ .nyi, scope };
}

fn update(gz: *GenZir, scope: *Scope, node: Ast.Node.Index) InnerError!Result {
    _ = node; // autofix
    _ = gz; // autofix
    return .{ .nyi, scope };
}

fn deleteRows(gz: *GenZir, scope: *Scope, node: Ast.Node.Index) InnerError!Result {
    _ = node; // autofix
    _ = gz; // autofix
    return .{ .nyi, scope };
}

fn deleteCols(gz: *GenZir, scope: *Scope, node: Ast.Node.Index) InnerError!Result {
    _ = node; // autofix
    _ = gz; // autofix
    return .{ .nyi, scope };
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
    // TODO: Validate assertion.
    assert(node != .root);

    const gpa = astgen.gpa;
    const tree = astgen.context.tree;

    switch (tree.nodeTag(node)) {
        .root => unreachable,

        .grouped_expression => try astgen.scanNode(tree.nodeData(node).node_and_token[0], args),
        .list => {
            const nodes = tree.extraDataSlice(tree.nodeData(node).extra_range, Ast.Node.Index);
            for (nodes) |n| try astgen.scanNode(n, args);
        },
        .table_literal => {
            const table = tree.extraData(tree.nodeData(node).extra_and_token[0], Ast.Node.Table);
            const keys = tree.extraDataSlice(.{ .start = table.keys_start, .end = table.keys_end }, Ast.Node.Index);
            const columns = tree.extraDataSlice(.{ .start = table.columns_start, .end = table.columns_end }, Ast.Node.Index);
            for (&[_][]const Ast.Node.Index{ keys, columns }) |table_slice| {
                for (table_slice) |n| {
                    var next = tree.unwrapGroupedExpr(n);
                    switch (tree.nodeTag(next)) {
                        .apply_binary => {
                            const data = tree.nodeData(next).node_and_opt_node;
                            const op_node: Ast.Node.Index = @enumFromInt(tree.nodeMainToken(next));
                            switch (tree.nodeTag(op_node)) {
                                .colon, .colon_colon => {
                                    next = tree.unwrapGroupedExpr(data[0]);
                                    try astgen.scanNode(next, args);
                                },
                                else => {
                                    try astgen.scanNode(data[0], args);
                                    try astgen.scanNode(op_node, args);
                                },
                            }
                            if (data[1].unwrap()) |nn| try astgen.scanNode(nn, args);
                        },
                        .call => {
                            const nodes = tree.extraDataSlice(tree.nodeData(next).extra_range, Ast.Node.Index);

                            next = tree.unwrapGroupedExpr(nodes[0]);

                            try astgen.scanNode(next, args);
                            for (nodes[1..]) |nn| try astgen.scanNode(nn, args);
                        },
                        else => try astgen.scanNode(next, args),
                    }
                }
            }
        },

        .expr_block => {
            const nodes = tree.extraDataSlice(tree.nodeData(node).extra_range, Ast.Node.Index);
            var it = std.mem.reverseIterator(nodes);
            while (it.next()) |n| try astgen.scanNode(n, args);
        },

        .apostrophe,
        .apostrophe_colon,
        .slash,
        .slash_colon,
        .backslash,
        .backslash_colon,
        => if (tree.nodeData(node).opt_node.unwrap()) |n| try astgen.scanNode(n, args),

        .call,
        => {
            const nodes = tree.extraDataSlice(tree.nodeData(node).extra_range, Ast.Node.Index);

            var next = tree.unwrapGroupedExpr(nodes[0]);
            if (tree.nodeTag(next) == .colon and nodes.len == 3) {
                next = tree.unwrapGroupedExpr(nodes[1]);
                if (tree.nodeTag(next) == .identifier) {
                    const ident_token = tree.nodeMainToken(next);
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
                    try astgen.scanNode(nodes[2], args);
                } else {
                    for (nodes) |n| try astgen.scanNode(n, args);
                }
            } else if (tree.nodeTag(next) == .colon_colon and nodes.len == 3) {
                next = tree.unwrapGroupedExpr(nodes[1]);
                if (tree.nodeTag(next) == .identifier) {
                    const ident_token = tree.nodeMainToken(next);
                    const ident_bytes = tree.tokenSlice(ident_token);
                    const ident_name = try astgen.bytesAsString(ident_bytes);

                    if (args.identifiers) |identifiers| try identifiers.put(
                        gpa,
                        ident_bytes,
                        next,
                    );

                    try args.global_decls.put(gpa, ident_name, node);
                    try astgen.scanNode(nodes[2], args);
                } else {
                    for (nodes) |n| try astgen.scanNode(n, args);
                }
            } else {
                for (nodes) |n| try astgen.scanNode(n, args);
            }
        },
        .apply_unary,
        => {
            const data = tree.nodeData(node).node_and_node;
            try astgen.scanNode(data[0], args);
            try astgen.scanNode(data[1], args);
        },
        .apply_binary => {
            const data = tree.nodeData(node).node_and_opt_node;
            const op_node: Ast.Node.Index = @enumFromInt(tree.nodeMainToken(node));
            switch (tree.nodeTag(op_node)) {
                .colon => {
                    const next = tree.unwrapGroupedExpr(data[0]);
                    if (tree.nodeTag(next) == .identifier) {
                        const ident_token = tree.nodeMainToken(next);
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
                },
                .colon_colon => {
                    const next = tree.unwrapGroupedExpr(data[0]);
                    if (tree.nodeTag(next) == .identifier) {
                        const ident_token = tree.nodeMainToken(next);
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
                },
                else => {
                    try astgen.scanNode(data[0], args);
                    try astgen.scanNode(op_node, args);
                },
            }
            if (data[1].unwrap()) |n| try astgen.scanNode(n, args);
        },

        inline .select, .exec, .update, .delete_rows, .delete_cols => |t| {
            const sql = tree.extraData(tree.nodeData(node).extra, switch (t) {
                .select => Ast.Node.Select,
                .exec => Ast.Node.Exec,
                .update => Ast.Node.Update,
                .delete_rows => Ast.Node.DeleteRows,
                .delete_cols => Ast.Node.DeleteCols,
                else => comptime unreachable,
            });
            try astgen.scanNode(sql.from, args);
        },

        .identifier => if (args.identifiers) |identifiers| {
            const ident_token = tree.nodeMainToken(node);
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

    var doc_scope: DocumentScope = .{};
    defer doc_scope.deinit(gpa);
    var context: DocumentScope.ScopeContext = .{
        .gpa = gpa,
        .tree = tree,
        .doc_scope = &doc_scope,
    };
    defer context.deinit();

    var astgen: AstGen = .{
        .gpa = gpa,
        .arena = arena.allocator(),
        .context = &context,
        .scope = undefined,
    };
    defer astgen.deinit(gpa);

    var lambda_scope: Scope.Lambda = .{
        .parent = undefined,
        .node = .root,
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

fn lowerAstErrors(astgen: *AstGen) error{OutOfMemory}!void {
    const tree = astgen.context.tree;
    assert(tree.errors.len > 0);

    const gpa = astgen.gpa;
    const parse_err = tree.errors[0];

    var msg: std.Io.Writer.Allocating = .init(gpa);
    defer msg.deinit();
    const msg_w = &msg.writer;

    var notes: std.ArrayListUnmanaged(u32) = .empty;
    defer notes.deinit(gpa);

    for (tree.errors[1..]) |note| {
        if (!note.is_note) break;

        tree.renderError(note, msg_w) catch return error.OutOfMemory;
        try notes.append(gpa, try astgen.errNoteTok(note.token, "{s}", .{msg.written()}));
        msg.clearRetainingCapacity();
    }

    const extra_offset = tree.errorOffset(parse_err);
    tree.renderError(parse_err, msg_w) catch return error.OutOfMemory;
    try astgen.appendErrorTokNotesOff(parse_err.token, extra_offset, "{s}", .{msg.written()}, notes.items, .@"error");
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
        /// Parents can be: `LocalVal`, `GenZir`, `Lambda`.
        parent: *Scope,
        inst: Zir.Inst.Ref,
        /// Source location of the corresponding variable declaration.
        token_src: Ast.TokenIndex,
        /// Track the first identifier where it is referenced.
        /// 0 means never referenced.
        used: Ast.TokenIndex = 0,
        /// String table index.
        name: Zir.NullTerminatedString,
        id_cat: IdCat,
    };

    /// Represents a global scope that has any number of declarations in it.
    /// Each declaration has this as the parent scope.
    const Lambda = struct {
        const base_tag: Tag = .lambda;
        base: Scope = .{ .tag = base_tag },

        /// Parents can be: `LocalVal`, `GenZir`, `Lambda`, `Top`.
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

    fn nodeIndexToRelative(gz: GenZir, node_index: Ast.Node.Index) Ast.Node.Offset {
        return gz.decl_node_index.toOffset(node_index);
    }

    fn tokenIndexToRelative(gz: GenZir, token: Ast.TokenIndex) Ast.TokenOffset {
        return .init(gz.srcToken(), token);
    }

    fn srcToken(gz: GenZir) Ast.TokenIndex {
        return gz.astgen.context.tree.firstToken(gz.decl_node_index);
    }

    fn makeFile(gz: *GenZir) !Zir.Inst.Index {
        return gz.makePlNode(.file, .root);
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
        rbrace: Ast.TokenIndex,
        params_len: u32,
        body_gz: *GenZir,
    }) !void {
        const astgen = gz.astgen;
        const gpa = astgen.gpa;
        const tree = astgen.context.tree;
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

    /// Must be called with the following stack set up:
    ///  * gz (bottom)
    ///  * body_gz (top)
    /// Unstacks all of those except for `gz`.
    fn setDo(gz: *GenZir, inst: Zir.Inst.Index, args: struct { condition: Zir.Inst.Ref, body_gz: *GenZir }) !void {
        try gz.setStatement(inst, args, Zir.Inst.Do);
    }

    /// Must be called with the following stack set up:
    ///  * gz (bottom)
    ///  * body_gz (top)
    /// Unstacks all of those except for `gz`.
    fn setIf(gz: *GenZir, inst: Zir.Inst.Index, args: struct { condition: Zir.Inst.Ref, body_gz: *GenZir }) !void {
        try gz.setStatement(inst, args, Zir.Inst.If);
    }

    /// Must be called with the following stack set up:
    ///  * gz (bottom)
    ///  * body_gz (top)
    /// Unstacks all of those except for `gz`.
    fn setWhile(gz: *GenZir, inst: Zir.Inst.Index, args: struct { condition: Zir.Inst.Ref, body_gz: *GenZir }) !void {
        try gz.setStatement(inst, args, Zir.Inst.While);
    }

    /// Must be called with the following stack set up:
    ///  * gz (bottom)
    ///  * body_gz (top)
    /// Unstacks all of those except for `gz`.
    fn setStatement(gz: *GenZir, inst: Zir.Inst.Index, args: anytype, comptime T: type) !void {
        const astgen = gz.astgen;
        const gpa = astgen.gpa;
        const zir_datas: []Zir.Inst.Data = astgen.instructions.items(.data);

        const body = args.body_gz.instructionsSlice();
        const body_len = astgen.countBodyLenAfterFixups(body);
        args.body_gz.unstack();

        try astgen.extra.ensureUnusedCapacity(
            gpa,
            @typeInfo(T).@"struct".fields.len + body_len,
        );

        zir_datas[@intFromEnum(inst)].pl_node.payload_index = astgen.addExtraAssumeCapacity(T{
            .condition = args.condition,
            .body_len = body_len,
        });
        astgen.appendBodyWithFixups(body);

        try gz.instructions.ensureUnusedCapacity(gpa, 1);
        gz.instructions.appendAssumeCapacity(inst);
    }

    fn addTable(gz: *GenZir, node: Ast.Node.Index, args: struct {
        keys: []Zir.Inst.Table.Item,
        columns: []Zir.Inst.Table.Item,
    }) !Zir.Inst.Ref {
        const astgen = gz.astgen;
        const gpa = astgen.gpa;

        const ref = try gz.addPlNode(.table, node, Zir.Inst.Table{
            .keys_len = @intCast(args.keys.len),
            .columns_len = @intCast(args.columns.len),
        });

        try astgen.extra.ensureUnusedCapacity(
            gpa,
            @typeInfo(Zir.Inst.Table.Item).@"struct".fields.len * args.keys.len +
                @typeInfo(Zir.Inst.Table.Item).@"struct".fields.len * args.columns.len,
        );

        var keys_it = std.mem.reverseIterator(args.keys);
        while (keys_it.next()) |item| {
            _ = astgen.addExtraAssumeCapacity(Zir.Inst.Table.Item{
                .name = item.name,
                .ref = item.ref,
            });
        }
        var columns_it = std.mem.reverseIterator(args.columns);
        while (columns_it.next()) |item| {
            _ = astgen.addExtraAssumeCapacity(Zir.Inst.Table.Item{
                .name = item.name,
                .ref = item.ref,
            });
        }

        return ref;
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
        abs_tok_index: Ast.TokenIndex,
    ) !Zir.Inst.Ref {
        return gz.add(.{
            .tag = tag,
            .data = .{ .str_tok = .{
                .start = str_index,
                .src_tok = gz.tokenIndexToRelative(abs_tok_index),
            } },
        });
    }

    fn addByte(gz: *GenZir, value: u8) !Zir.Inst.Ref {
        return gz.add(.{
            .tag = .byte,
            .data = .{ .byte = value },
        });
    }

    fn addShort(gz: *GenZir, value: i16) !Zir.Inst.Ref {
        return gz.add(.{
            .tag = .short,
            .data = .{ .short = value },
        });
    }

    fn addInt(gz: *GenZir, value: i32) !Zir.Inst.Ref {
        return gz.add(.{
            .tag = .int,
            .data = .{ .int = value },
        });
    }

    fn addLong(gz: *GenZir, value: i64) !Zir.Inst.Ref {
        return gz.add(.{
            .tag = .long,
            .data = .{ .long = value },
        });
    }

    fn addReal(gz: *GenZir, value: f32) !Zir.Inst.Ref {
        return gz.add(.{
            .tag = .real,
            .data = .{ .real = value },
        });
    }

    fn addFloat(gz: *GenZir, value: f64) !Zir.Inst.Ref {
        return gz.add(.{
            .tag = .float,
            .data = .{ .float = value },
        });
    }

    fn addChar(gz: *GenZir, value: u8) !Zir.Inst.Ref {
        return gz.add(.{
            .tag = .char,
            .data = .{ .byte = value },
        });
    }

    fn addTimestamp(gz: *GenZir, value: i64) !Zir.Inst.Ref {
        return gz.add(.{
            .tag = .timestamp,
            .data = .{ .long = value },
        });
    }

    fn addMonth(gz: *GenZir, value: i32) !Zir.Inst.Ref {
        return gz.add(.{
            .tag = .month,
            .data = .{ .int = value },
        });
    }

    fn addDate(gz: *GenZir, value: i32) !Zir.Inst.Ref {
        return gz.add(.{
            .tag = .date,
            .data = .{ .int = value },
        });
    }

    fn addDatetime(gz: *GenZir, value: f64) !Zir.Inst.Ref {
        return gz.add(.{
            .tag = .datetime,
            .data = .{ .float = value },
        });
    }

    fn addTimespan(gz: *GenZir, value: i64) !Zir.Inst.Ref {
        return gz.add(.{
            .tag = .timespan,
            .data = .{ .long = value },
        });
    }

    fn addMinute(gz: *GenZir, value: i32) !Zir.Inst.Ref {
        return gz.add(.{
            .tag = .minute,
            .data = .{ .int = value },
        });
    }

    fn addSecond(gz: *GenZir, value: i32) !Zir.Inst.Ref {
        return gz.add(.{
            .tag = .second,
            .data = .{ .int = value },
        });
    }

    fn addTime(gz: *GenZir, value: i32) !Zir.Inst.Ref {
        return gz.add(.{
            .tag = .time,
            .data = .{ .int = value },
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
        abs_tok_index: Ast.TokenIndex,
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
    const tree = astgen.context.tree;
    const token_locs: []Ast.Token.Loc = tree.tokens.items(.loc);
    const node_start = token_locs[tree.firstToken(node)].start;
    astgen.advanceSourceCursor(node_start);
}

/// Advances the source cursor to an absolute byte offset `end` in the file.
fn advanceSourceCursor(astgen: *AstGen, end: usize) void {
    const source = astgen.context.tree.source;
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
    try string_bytes.print(astgen.gpa, format ++ "\x00", args);
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
        .node = node.toOptional(),
        .token = .none,
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
    token: Ast.TokenIndex,
    comptime format: []const u8,
    args: anytype,
) InnerError {
    return astgen.failTokNotes(token, format, args, &[0]u32{});
}

fn appendErrorTok(
    astgen: *AstGen,
    token: Ast.TokenIndex,
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
    token: Ast.TokenIndex,
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
    token: Ast.TokenIndex,
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
    token: Ast.TokenIndex,
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
        .@"error",
    );
    return error.AnalysisFail;
}

fn appendErrorTokNotesOff(
    astgen: *AstGen,
    token: Ast.TokenIndex,
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
    try string_bytes.print(astgen.gpa, format ++ "\x00", args);
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
        .node = .none,
        .token = .fromToken(token),
        .byte_offset = byte_offset,
        .notes = notes_index,
        .kind = kind,
    });
}

fn errNoteTok(
    astgen: *AstGen,
    token: Ast.TokenIndex,
    comptime format: []const u8,
    args: anytype,
) Allocator.Error!u32 {
    return errNoteTokOff(astgen, token, 0, format, args);
}

fn errNoteTokOff(
    astgen: *AstGen,
    token: Ast.TokenIndex,
    byte_offset: u32,
    comptime format: []const u8,
    args: anytype,
) Allocator.Error!u32 {
    @branchHint(.cold);
    const string_bytes = &astgen.string_bytes;
    const msg: Zir.NullTerminatedString = @enumFromInt(string_bytes.items.len);
    try string_bytes.print(astgen.gpa, format ++ "\x00", args);
    return astgen.addExtra(Zir.Inst.CompileErrors.Item{
        .msg = msg,
        .node = .none,
        .token = .fromToken(token),
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
    try string_bytes.print(astgen.gpa, format ++ "\x00", args);
    return astgen.addExtra(Zir.Inst.CompileErrors.Item{
        .msg = msg,
        .node = node.toOptional(),
        .token = .none,
        .byte_offset = 0,
        .notes = 0,
        .kind = .note,
    });
}

fn tokenAsString(astgen: *AstGen, token: Ast.TokenIndex) !Zir.NullTerminatedString {
    const token_bytes = astgen.context.tree.tokenSlice(token);
    return astgen.bytesAsString(token_bytes);
}

fn strLitAsString(astgen: *AstGen, str_lit_token: Ast.TokenIndex) !Zir.NullTerminatedString {
    const token_bytes = astgen.context.tree.tokenSlice(str_lit_token);
    return astgen.bytesAsString(token_bytes[1 .. token_bytes.len - 1]);
}

fn symLitAsString(astgen: *AstGen, sym_lit_token: Ast.TokenIndex) !Zir.NullTerminatedString {
    const token_bytes = astgen.context.tree.tokenSlice(sym_lit_token);
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
