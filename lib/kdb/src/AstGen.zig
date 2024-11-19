const std = @import("std");
const mem = std.mem;
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const StringIndexAdapter = std.hash_map.StringIndexAdapter;
const StringIndexContext = std.hash_map.StringIndexContext;

const kdb = @import("root.zig");
const Ast = kdb.Ast;
const Zir = kdb.Zir;

const AstGen = @This();

gpa: Allocator,
tree: *const Ast,
instructions: std.MultiArrayList(Zir.Inst) = .{},
extra: ArrayListUnmanaged(u32) = .empty,
string_bytes: ArrayListUnmanaged(u8) = .empty,
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
/// Used for temporary allocations; freed after AstGen is complete.
/// The resulting ZIR code has no references to anything in this arena.
arena: Allocator,
string_table: std.HashMapUnmanaged(
    u32,
    void,
    StringIndexContext,
    std.hash_map.default_max_load_percentage,
) = .empty,
compile_errors: ArrayListUnmanaged(Zir.Inst.CompileErrors.Item) = .empty,
/// Whether we are somewhere within a function. If `true`, any container decls may be
/// generic and thus must be tunneled through closure.
within_fn: bool = false,
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
            => @intFromEnum(@field(extra, field.name)),

            i32,
            Zir.Inst.Declaration.Flags,
            => @bitCast(@field(extra, field.name)),

            else => |t| @compileError("bad field type: " ++ @typeName(t)),
        };
        i += 1;
    }
}

pub fn generate(gpa: Allocator, tree: Ast) Allocator.Error!Zir {
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

    var gz_instructions: std.ArrayListUnmanaged(Zir.Inst.Index) = .empty;
    var gen_scope: GenZir = .{
        .is_comptime = true,
        .parent = &top_scope.base,
        .anon_name_strategy = .parent,
        .decl_node_index = 0,
        .decl_line = 0,
        .astgen = &astgen,
        .instructions = &gz_instructions,
        .instructions_top = 0,
    };
    defer gz_instructions.deinit(gpa);

    // The AST -> ZIR lowering process assumes an AST that does not have any
    // parse errors.
    if (tree.errors.len == 0) {
        if (AstGen.file(&gen_scope, &gen_scope.base)) |struct_decl_ref| {
            assert(struct_decl_ref.toIndex().? == .file);
        } else |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.AnalysisFail => {}, // Handled via compile_errors below.
        }
    } else {
        try lowerAstErrors(&astgen);
    }

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
    };
}

fn deinit(astgen: *AstGen, gpa: Allocator) void {
    astgen.instructions.deinit(gpa);
    astgen.extra.deinit(gpa);
    astgen.string_table.deinit(gpa);
    astgen.string_bytes.deinit(gpa);
    astgen.compile_errors.deinit(gpa);
    astgen.imports.deinit(gpa);
    astgen.scratch.deinit(gpa);
    astgen.ref_table.deinit(gpa);
}

const ResultInfo = struct {
    /// The semantics requested for the result location
    rl: Loc,

    /// The "operator" consuming the result location
    ctx: Context = .none,

    const Loc = union(enum) {
        /// The expression has an inferred type, and it will be evaluated as an rvalue.
        none,
    };

    const Context = enum {
        /// The expression is the operand to a return expression.
        @"return",
        /// The expression is the input to an error-handling operator (if-else, try, or catch).
        error_handling_expr,
        /// The expression is the right-hand side of a shift operation.
        shift_op,
        /// The expression is an argument in a function call.
        fn_arg,
        /// The expression is the right-hand side of an initializer for a `const` variable
        const_init,
        /// The expression is the right-hand side of an assignment expression.
        assignment,
        /// No specific operator in particular.
        none,
    };
};

fn file(gz: *GenZir, scope: *Scope) InnerError!Zir.Inst.Ref {
    const astgen = gz.astgen;
    const gpa = astgen.gpa;
    const tree = astgen.tree;
    const blocks = tree.rootDecls();

    const file_inst = try gz.addAsIndex(.{
        .tag = .file,
        .data = .{
            .file = .{
                .blocks_len = @intCast(blocks.len),
            },
        },
    });

    // Reserve elements to record block instruction indices.
    try gz.astgen.extra.ensureUnusedCapacity(gpa, blocks.len);
    gz.astgen.extra.items.len += blocks.len;

    var namespace: Scope.Namespace = .{
        .parent = scope,
        .node = 0,
        .inst = file_inst,
        .declaring_gz = gz,
        .maybe_generic = astgen.within_fn,
    };
    defer namespace.deinit(gpa);

    // The struct_decl instruction introduces a scope in which the decls of the struct
    // are in scope, so that field types, alignments, and default value expressions
    // can refer to decls within the struct itself.
    astgen.advanceSourceCursorToNode(0);
    var block_scope: GenZir = .{
        .parent = &namespace.base,
        .decl_node_index = 0,
        .decl_line = gz.decl_line,
        .astgen = astgen,
        .is_comptime = true,
        .instructions = gz.instructions,
        .instructions_top = gz.instructions.items.len,
    };
    defer block_scope.unstack();

    const scratch_top = astgen.scratch.items.len;
    defer astgen.scratch.shrinkRetainingCapacity(scratch_top);

    var i: u32 = 0;
    while (i < blocks.len) : (i += 1) {
        const block_node = blocks[i];
        astgen.block(&block_scope, &namespace.base, block_node, i) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.AnalysisFail => {},
        };
    }

    block_scope.unstack();
    return file_inst.toRef();
}

fn block(astgen: *AstGen, gz: *GenZir, scope: *Scope, node: Ast.Node.Index, block_index: u32) InnerError!void {
    const tree = astgen.tree;
    const token_tags: []Ast.Token.Tag = tree.tokens.items(.tag);

    astgen.advanceSourceCursorToNode(node);

    var block_scope: GenZir = .{
        .parent = scope,
        .decl_node_index = node,
        .decl_line = astgen.source_line,
        .astgen = astgen,
        .is_comptime = true,
        .instructions = gz.instructions,
        .instructions_top = gz.instructions.items.len,
    };
    defer block_scope.unstack();

    const expr_inst = try expr(&block_scope, &block_scope.base, .{ .rl = .none }, node);

    const last_token = tree.lastToken(node);
    if (token_tags[last_token + 1] == .semicolon) {
        unreachable;
    } else {
        _ = try block_scope.addShow(expr_inst, node);
    }

    const reserved_count = @typeInfo(Zir.ExtraIndex).@"enum".fields.len;
    astgen.extra.items[block_index + reserved_count] = @intCast(astgen.instructions.len);
}

fn expr(gz: *GenZir, scope: *Scope, ri: ResultInfo, node: Ast.Node.Index) InnerError!Zir.Inst.Ref {
    const astgen = gz.astgen;
    const tree = astgen.tree;
    const node_tags: []Ast.Node.Tag = tree.nodes.items(.tag);

    const prev_node = gz.decl_node_index;
    gz.decl_node_index = node;
    defer gz.decl_node_index = prev_node;

    const prev_anon_name_strategy = gz.anon_name_strategy;
    defer gz.anon_name_strategy = prev_anon_name_strategy;
    if (!nodeUsesAnonNameStrategy(tree, node)) {
        gz.anon_name_strategy = .anon;
    }

    switch (node_tags[node]) {
        .root => unreachable,
        .empty => {},

        .grouped_expression,
        .empty_list,
        .list,
        .table_literal,
        => unreachable,

        .lambda,
        .lambda_semicolon,
        => {
            try astgen.fnDecl(gz, scope, node, node, tree.fullLambda(node));
            return .void_value;
            // return lambda(gz, scope, node, tree.fullLambda(node));
        },

        .expr_block,
        => unreachable,

        .@"return",
        .signal,
        => unreachable,

        .assign,
        => return assign(gz, scope, node),

        .global_assign,
        => unreachable,

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
        => unreachable,

        .apostrophe,
        .apostrophe_colon,
        .slash,
        .slash_colon,
        .backslash,
        .backslash_colon,
        => unreachable,

        .call,
        .apply_unary,
        => unreachable,

        .apply_binary,
        => return applyBinary(gz, scope, ri, node),

        .number_literal,
        => return numberLiteral(gz, node),

        .number_list_literal,
        .string_literal,
        .symbol_literal,
        .symbol_list_literal,
        => unreachable,

        .identifier,
        => return identifier(gz, scope, ri, node),

        .builtin,
        => unreachable,

        .select,
        .exec,
        .update,
        .delete_rows,
        .delete_cols,
        => unreachable,

        .do,
        .@"if",
        .@"while",
        .cond,
        => unreachable,
    }

    unreachable;
}

fn fnDecl(
    astgen: *AstGen,
    gz: *GenZir,
    scope: *Scope,
    decl_node: Ast.Node.Index,
    body_node: Ast.Node.Index, // TODO: Remove
    fn_proto: Ast.full.Lambda,
) InnerError!void {
    const tree = astgen.tree;
    const node_tags: []Ast.Node.Tag = tree.nodes.items(.tag);
    const main_tokens: []Ast.Token.Index = tree.nodes.items(.main_token);

    // TODO: function name.

    // We insert this at the beginning so that its instruction index marks the
    // start of the top level declaration.
    // const decl_inst = try gz.makeDeclaration(decl_node);
    astgen.advanceSourceCursorToNode(decl_node);

    var decl_gz: GenZir = .{
        .is_comptime = true,
        .decl_node_index = decl_node,
        .decl_line = astgen.source_line,
        .parent = scope,
        .astgen = astgen,
        .instructions = gz.instructions,
        .instructions_top = gz.instructions.items.len,
    };
    defer decl_gz.unstack();

    var fn_gz: GenZir = .{
        .is_comptime = false,
        .decl_node_index = decl_node,
        .decl_line = decl_gz.decl_line,
        .parent = &decl_gz.base,
        .astgen = astgen,
        .instructions = gz.instructions,
        .instructions_top = GenZir.unstacked_top,
    };
    defer fn_gz.unstack();

    // Set this now, since parameter types, return type, etc may be generic.
    const prev_within_fn = astgen.within_fn;
    defer astgen.within_fn = prev_within_fn;
    astgen.within_fn = true;

    var params_scope = &fn_gz.base;
    var param_type_i: usize = 0;
    if (fn_proto.params) |p| while (param_type_i < p.params.len) : (param_type_i += 1) {
        const param = p.params[param_type_i];

        if (node_tags[param] == .empty) {
            assert(p.params.len == 1);
            break;
        }

        const name_token = main_tokens[param];
        const name_bytes = tree.tokenSlice(name_token);
        const param_name = try astgen.identAsString(name_token);
        assert(param_name != .empty);
        try astgen.detectLocalShadowing(params_scope, param_name, name_token, name_bytes, .@"function parameter");

        const param_inst = param: {
            var param_gz = decl_gz.makeSubBlock(scope);
            defer param_gz.unstack();

            const ns = scope.cast(GenZir).?.parent.cast(Scope.Namespace).?;
            try ns.decls.put(astgen.gpa, param_name, param);

            _ = try expr(&param_gz, &param_gz.base, .{ .rl = .none }, param);

            const param_inst = try decl_gz.addParam(&param_gz, name_token, param_name);
            break :param param_inst.toRef();
        };

        const sub_scope = try astgen.arena.create(Scope.LocalVal);
        sub_scope.* = .{
            .parent = params_scope,
            .gen_zir = &decl_gz,
            .name = param_name,
            .inst = param_inst,
            .token_src = name_token,
            .id_cat = .@"function parameter",
        };
        params_scope = &sub_scope.base;
    };

    var ret_gz = decl_gz.makeSubBlock(params_scope);
    defer ret_gz.unstack();

    assert(body_node != 0);
    // as a scope, fn_gz encloses ret_gz, but for instruction list, fn_gz stacks on ret_gz
    fn_gz.instructions_top = ret_gz.instructions.items.len;

    astgen.advanceSourceCursorToNode(body_node);
    const lbrace_line = astgen.source_line - decl_gz.decl_line;
    const lbrace_column = astgen.source_column;

    _ = try fullBodyExpr(&fn_gz, params_scope, fn_proto.body);
    try checkUsed(gz, &fn_gz.base, params_scope);

    // if (!fn_gz.endsWithNoReturn()) {
    //     // As our last action before the return, "pop" the error trace if needed
    //     _ = try fn_gz.addRestoreErrRetIndex(.ret, .always, decl_node);

    //     // Add implicit return at end of function.
    //     _ = try fn_gz.addUnTok(.ret_implicit, .void_value, tree.lastToken(body_node));
    // }

    const func_inst = try decl_gz.addFunc(.{
        .src_node = decl_node,
        .lbrace_line = lbrace_line,
        .lbrace_column = lbrace_column,
        .body_gz = &fn_gz,
    });
    _ = func_inst; // autofix

    // We add this at the end so that its instruction index marks the end range
    // of the top level declaration. addFunc already unstacked fn_gz and ret_gz.
    // _ = try decl_gz.addBreak(.break_inline, decl_inst, func_inst);

    // try setDeclaration(
    //     decl_inst,
    //     decl_gz.decl_line,
    //     &decl_gz,
    //     // align, linksection, and addrspace are passed in the func instruction in this case.
    //     // TODO: move them from the function instruction to the declaration instruction?
    //     null,
    // );
}

/// Local variables shadowing detection, including function parameters.
fn detectLocalShadowing(
    astgen: *AstGen,
    scope: *Scope,
    ident_name: Zir.NullTerminatedString,
    name_token: Ast.Token.Index,
    token_bytes: []const u8,
    id_cat: Scope.IdCat,
) !void {
    _ = token_bytes; // autofix
    const gpa = astgen.gpa;
    // if (token_bytes[0] != '@' and isPrimitive(token_bytes)) {
    //     return astgen.failTokNotes(name_token, "name shadows primitive '{s}'", .{
    //         token_bytes,
    //     }, &[_]u32{
    //         try astgen.errNoteTok(name_token, "consider using @\"{s}\" to disambiguate", .{
    //             token_bytes,
    //         }),
    //     });
    // }

    var s = scope;
    var outer_scope = false;
    while (true) switch (s.tag) {
        .local_val => {
            const local_val = s.cast(Scope.LocalVal).?;
            if (local_val.name == ident_name) {
                const name_slice = mem.span(astgen.nullTerminatedString(ident_name));
                const name = try gpa.dupe(u8, name_slice);
                defer gpa.free(name);
                if (outer_scope) {
                    return astgen.failTokNotes(name_token, "{s} '{s}' shadows {s} from outer scope", .{
                        @tagName(id_cat), name, @tagName(local_val.id_cat),
                    }, &[_]u32{
                        try astgen.errNoteTok(
                            local_val.token_src,
                            "previous declaration here",
                            .{},
                        ),
                    });
                }
                return astgen.failTokNotes(name_token, "redeclaration of {s} '{s}'", .{
                    @tagName(local_val.id_cat), name,
                }, &[_]u32{
                    try astgen.errNoteTok(
                        local_val.token_src,
                        "previous declaration here",
                        .{},
                    ),
                });
            }
            s = local_val.parent;
        },
        .local_ptr => {
            const local_ptr = s.cast(Scope.LocalPtr).?;
            if (local_ptr.name == ident_name) {
                const name_slice = mem.span(astgen.nullTerminatedString(ident_name));
                const name = try gpa.dupe(u8, name_slice);
                defer gpa.free(name);
                if (outer_scope) {
                    return astgen.failTokNotes(name_token, "{s} '{s}' shadows {s} from outer scope", .{
                        @tagName(id_cat), name, @tagName(local_ptr.id_cat),
                    }, &[_]u32{
                        try astgen.errNoteTok(
                            local_ptr.token_src,
                            "previous declaration here",
                            .{},
                        ),
                    });
                }
                return astgen.failTokNotes(name_token, "redeclaration of {s} '{s}'", .{
                    @tagName(local_ptr.id_cat), name,
                }, &[_]u32{
                    try astgen.errNoteTok(
                        local_ptr.token_src,
                        "previous declaration here",
                        .{},
                    ),
                });
            }
            s = local_ptr.parent;
        },
        .namespace => {
            outer_scope = true;
            const ns = s.cast(Scope.Namespace).?;
            const decl_node = ns.decls.get(ident_name) orelse {
                s = ns.parent;
                continue;
            };
            const name_slice = mem.span(astgen.nullTerminatedString(ident_name));
            const name = try gpa.dupe(u8, name_slice);
            defer gpa.free(name);
            return astgen.failTokNotes(name_token, "{s} shadows declaration of '{s}'", .{
                @tagName(id_cat), name,
            }, &[_]u32{
                try astgen.errNoteNode(decl_node, "declared here", .{}),
            });
        },
        .gen_zir => {
            s = s.cast(GenZir).?.parent;
            outer_scope = true;
        },
        .top => break,
    };
}

/// Applies `rl` semantics to `result`. Expressions which do not do their own handling of
/// result locations must call this function on their result.
/// As an example, if `ri.rl` is `.ptr`, it will write the result to the pointer.
/// If `ri.rl` is `.ty`, it will coerce the result to the type.
/// Assumes nothing stacked on `gz`.
fn rvalue(
    gz: *GenZir,
    ri: ResultInfo,
    raw_result: Zir.Inst.Ref,
    src_node: Ast.Node.Index,
) InnerError!Zir.Inst.Ref {
    return rvalueInner(gz, ri, raw_result, src_node, true);
}

fn rvalueInner(
    gz: *GenZir,
    ri: ResultInfo,
    raw_result: Zir.Inst.Ref,
    src_node: Ast.Node.Index,
    allow_coerce_pre_ref: bool,
) InnerError!Zir.Inst.Ref {
    _ = gz; // autofix
    _ = src_node; // autofix
    _ = allow_coerce_pre_ref; // autofix
    const result = r: {
        // if (raw_result.toIndex()) |result_index| {
        // const zir_tags = gz.astgen.instructions.items(.tag);
        // const data = gz.astgen.instructions.items(.data)[@intFromEnum(result_index)];
        // if (zir_tags[@intFromEnum(result_index)].isAlwaysVoid(data)) {
        //     break :r Zir.Inst.Ref.void_value;
        // }
        // }
        break :r raw_result;
    };
    // if (gz.endsWithNoReturn()) return result;
    switch (ri.rl) {
        .none => return result,
    }
}

fn blockExprStmts(gz: *GenZir, parent_scope: *Scope, statements: []const Ast.Node.Index) !void {
    const astgen = gz.astgen;
    const tree = astgen.tree;
    const node_tags = tree.nodes.items(.tag);
    const node_data = tree.nodes.items(.data);
    _ = node_data; // autofix

    if (statements.len == 0) return;

    var block_arena = std.heap.ArenaAllocator.init(gz.astgen.gpa);
    defer block_arena.deinit();
    const block_arena_allocator = block_arena.allocator();
    _ = block_arena_allocator; // autofix

    var noreturn_src_node: Ast.Node.Index = 0;
    _ = &noreturn_src_node;
    var scope = parent_scope;
    _ = &scope;
    for (statements) |statement| {
        if (noreturn_src_node != 0) {
            try astgen.appendErrorNodeNotes(
                statement,
                "unreachable code",
                .{},
                &[_]u32{
                    try astgen.errNoteNode(
                        noreturn_src_node,
                        "control flow is diverted here",
                        .{},
                    ),
                },
            );
        }
        var inner_node = statement;
        _ = &inner_node;
        while (true) {
            switch (node_tags[inner_node]) {
                .assign => _ = try assign(gz, scope, statement),
                inline else => |t| @panic(@tagName(t)),
                // zig fmt: off
                // .global_var_decl,
                // .local_var_decl,
                // .simple_var_decl,
                // .aligned_var_decl, => scope = try varDecl(gz, scope, statement, block_arena_allocator, tree.fullVarDecl(statement).?),

                // .assign_destructure => scope = try assignDestructureMaybeDecls(gz, scope, statement, block_arena_allocator),

                // .@"defer"    => scope = try deferStmt(gz, scope, statement, block_arena_allocator, .defer_normal),
                // .@"errdefer" => scope = try deferStmt(gz, scope, statement, block_arena_allocator, .defer_error),

                // .assign => try assign(gz, scope, statement),

                // .assign_shl => try assignShift(gz, scope, statement, .shl),
                // .assign_shr => try assignShift(gz, scope, statement, .shr),

                // .assign_bit_and  => try assignOp(gz, scope, statement, .bit_and),
                // .assign_bit_or   => try assignOp(gz, scope, statement, .bit_or),
                // .assign_bit_xor  => try assignOp(gz, scope, statement, .xor),
                // .assign_div      => try assignOp(gz, scope, statement, .div),
                // .assign_sub      => try assignOp(gz, scope, statement, .sub),
                // .assign_sub_wrap => try assignOp(gz, scope, statement, .subwrap),
                // .assign_mod      => try assignOp(gz, scope, statement, .mod_rem),
                // .assign_add      => try assignOp(gz, scope, statement, .add),
                // .assign_add_wrap => try assignOp(gz, scope, statement, .addwrap),
                // .assign_mul      => try assignOp(gz, scope, statement, .mul),
                // .assign_mul_wrap => try assignOp(gz, scope, statement, .mulwrap),

                // .grouped_expression => {
                //     inner_node = node_data[statement].lhs;
                //     continue;
                // },

                // .while_simple,
                // .while_cont,
                // .@"while", => _ = try whileExpr(gz, scope, .{ .rl = .none }, inner_node, tree.fullWhile(inner_node).?, true),

                // .for_simple,
                // .@"for", => _ = try forExpr(gz, scope, .{ .rl = .none }, inner_node, tree.fullFor(inner_node).?, true),

                // // These cases are here to allow branch hints.
                // .builtin_call_two, .builtin_call_two_comma => {
                //     try emitDbgNode(gz, inner_node);
                //     const ri: ResultInfo = .{ .rl = .none };
                //     const result = if (node_data[inner_node].lhs == 0) r: {
                //         break :r try builtinCall(gz, scope, ri, inner_node, &.{}, allow_branch_hint);
                //     } else if (node_data[inner_node].rhs == 0) r: {
                //         break :r try builtinCall(gz, scope, ri, inner_node, &.{node_data[inner_node].lhs}, allow_branch_hint);
                //     } else r: {
                //         break :r try builtinCall(gz, scope, ri, inner_node, &.{
                //             node_data[inner_node].lhs,
                //             node_data[inner_node].rhs,
                //         }, allow_branch_hint);
                //     };
                //     noreturn_src_node = try addEnsureResult(gz, result, inner_node);
                // },
                // .builtin_call, .builtin_call_comma => {
                //     try emitDbgNode(gz, inner_node);
                //     const ri: ResultInfo = .{ .rl = .none };
                //     const params = tree.extra_data[node_data[inner_node].lhs..node_data[inner_node].rhs];
                //     const result = try builtinCall(gz, scope, ri, inner_node, params, allow_branch_hint);
                //     noreturn_src_node = try addEnsureResult(gz, result, inner_node);
                // },

                // else => noreturn_src_node = try unusedResultExpr(gz, scope, inner_node),
                // zig fmt: on
            }
            break;
        }
    }

    try checkUsed(gz, parent_scope, scope);
}

/// Similar to `expr`, but intended for use when `gz` corresponds to a body
/// which will contain only this node's code. Differs from `expr` in that if the
/// root expression is an unlabeled block, does not emit an actual block.
/// Instead, the block contents are emitted directly into `gz`.
fn fullBodyExpr(
    gz: *GenZir,
    scope: *Scope,
    expressions: []const Ast.Node.Index,
) InnerError!Zir.Inst.Ref {
    var sub_gz = gz.makeSubBlock(scope);
    for (expressions) |node| {
        _ = try expr(&sub_gz, &sub_gz.base, .{ .rl = .none }, node);
    }
    return .void_value;
}

fn checkUsed(gz: *GenZir, outer_scope: *Scope, inner_scope: *Scope) InnerError!void {
    const astgen = gz.astgen;

    var scope = inner_scope;
    while (scope != outer_scope) {
        switch (scope.tag) {
            .gen_zir => scope = scope.cast(GenZir).?.parent,
            .local_val => {
                const s = scope.cast(Scope.LocalVal).?;
                if (s.used == 0 and s.discarded == 0) {
                    // TODO: Unused function parameters.
                    // try astgen.appendErrorTok(s.token_src, "unused {s}", .{@tagName(s.id_cat)});
                } else if (s.used != 0 and s.discarded != 0) {
                    try astgen.appendErrorTokNotes(s.discarded, "pointless discard of {s}", .{@tagName(s.id_cat)}, &[_]u32{
                        try gz.astgen.errNoteTok(s.used, "used here", .{}),
                    });
                }
                scope = s.parent;
            },
            .local_ptr => {
                const s = scope.cast(Scope.LocalPtr).?;
                if (s.used == 0 and s.discarded == 0) {
                    try astgen.appendErrorTok(s.token_src, "unused {s}", .{@tagName(s.id_cat)});
                } else {
                    if (s.used != 0 and s.discarded != 0) {
                        try astgen.appendErrorTokNotes(s.discarded, "pointless discard of {s}", .{@tagName(s.id_cat)}, &[_]u32{
                            try astgen.errNoteTok(s.used, "used here", .{}),
                        });
                    }
                    if (s.id_cat == .@"local variable" and !s.used_as_lvalue) {
                        try astgen.appendErrorTokNotes(s.token_src, "local variable is never mutated", .{}, &.{
                            try astgen.errNoteTok(s.token_src, "consider using 'const'", .{}),
                        });
                    }
                }

                scope = s.parent;
            },
            .namespace => unreachable,
            .top => unreachable,
        }
    }
}

fn assign(gz: *GenZir, scope: *Scope, node: Ast.Node.Index) InnerError!Zir.Inst.Ref {
    const astgen = gz.astgen;
    const tree = astgen.tree;
    const node_datas: []Ast.Node.Data = tree.nodes.items(.data);
    const main_tokens: []Ast.Token.Index = tree.nodes.items(.main_token);
    const node_tags: []Ast.Node.Tag = tree.nodes.items(.tag);

    const data = node_datas[node];
    const rhs = try expr(gz, scope, .{ .rl = .none }, data.rhs);
    const lhs = switch (node_tags[data.lhs]) {
        .identifier => blk: {
            switch (scope.tag) {
                .gen_zir => {
                    const s = scope.cast(GenZir).?;
                    switch (s.parent.tag) {
                        .namespace => {
                            const ss: *Scope.Namespace = s.parent.cast(Scope.Namespace).?;
                            const name = try astgen.identAsString(main_tokens[data.lhs]);
                            try ss.decls.put(astgen.gpa, name, data.lhs);
                        },
                        inline else => |t| @panic(@tagName(t)),
                    }
                },
                inline else => |t| @panic(@tagName(t)),
            }
            break :blk try expr(gz, scope, .{ .rl = .none }, data.lhs);
        },
        else => return astgen.failNode(node, "invalid left-hand side to assignment", .{}),
    };

    return gz.addPlNode(.assign, node, Zir.Inst.Bin{ .lhs = lhs, .rhs = rhs });
}

fn applyBinary(gz: *GenZir, scope: *Scope, ri: ResultInfo, node: Ast.Node.Index) InnerError!Zir.Inst.Ref {
    _ = ri;
    const astgen = gz.astgen;
    const tree = astgen.tree;
    const node_datas: []Ast.Node.Data = tree.nodes.items(.data);
    const node_tags: []Ast.Node.Tag = tree.nodes.items(.tag);
    const main_tokens: []Ast.Token.Index = tree.nodes.items(.main_token);

    const data = node_datas[node];
    assert(data.rhs != 0);
    const rhs = try expr(gz, scope, .{ .rl = .none }, data.rhs);
    const lhs = try expr(gz, scope, .{ .rl = .none }, data.lhs);

    const op: Ast.Node.Index = main_tokens[node];
    const op_inst_tag: Zir.Inst.Tag = switch (node_tags[op]) {
        .plus => .add,
        // .plus_colon => .plus_colon,
        .minus => .subtract,
        // .minus_colon => .minus_colon,
        .asterisk => .multiply,
        // .asterisk_colon => .asterisk_colon,
        .percent => .divide,
        // .percent_colon => .percent_colon,
        .ampersand => .lesser,
        // .ampersand_colon => .ampersand_colon,
        .pipe => .greater,
        // .pipe_colon => .pipe_colon,
        .caret => .fill,
        // .caret_colon => .caret_colon,
        .equal => .equal,
        // .equal_colon => .equal_colon,
        .angle_bracket_left => .less_than,
        // .angle_bracket_left_colon => .angle_bracket_left_colon,
        .angle_bracket_left_equal => .less_than_or_equal,
        .angle_bracket_left_right => .not_equal,
        .angle_bracket_right => .greater_than,
        // .angle_bracket_right_colon => .angle_bracket_right_colon,
        .angle_bracket_right_equal => .greater_than_or_equal,
        .dollar => unreachable, // https://code.kx.com/q/ref/overloads/#dollar
        // .dollar_colon => .dollar_colon,
        .comma => .join,
        // .comma_colon => .comma_colon,
        .hash => unreachable, // https://code.kx.com/q/ref/overloads/#hash
        // .hash_colon => .hash_colon,
        .underscore => unreachable, // https://code.kx.com/q/ref/cut/ / https://code.kx.com/q/ref/drop/
        // .underscore_colon => .underscore_colon,
        .tilde => .match,
        // .tilde_colon => .tilde_colon,
        .bang => unreachable, // https://code.kx.com/q/ref/overloads/#bang
        // .bang_colon => .bang_colon,
        .question_mark => unreachable, // https://code.kx.com/q/ref/overloads/#query
        // .question_mark_colon => .question_mark_colon,
        .at => unreachable, // https://code.kx.com/q/ref/overloads/#at
        // .at_colon => .at_colon,
        .period => unreachable, // https://code.kx.com/q/ref/overloads/#dot
        // .period_colon => .period_colon,
        .zero_colon => unreachable, // https://code.kx.com/q/ref/file-text/
        // .zero_colon_colon => .zero_colon_colon,
        .one_colon => unreachable, // https://code.kx.com/q/ref/file-binary/
        // .one_colon_colon => .one_colon_colon,
        .two_colon => .dynamic_load,
        inline else => |t| @panic(@tagName(t)),
    };

    return gz.addPlNode(op_inst_tag, node, Zir.Inst.Bin{ .lhs = lhs, .rhs = rhs });
}

fn numberLiteral(gz: *GenZir, node: Ast.Node.Index) InnerError!Zir.Inst.Ref {
    const astgen = gz.astgen;
    const tree = astgen.tree;
    const main_tokens = tree.nodes.items(.main_token);
    const num_token = main_tokens[node];
    const bytes = tree.tokenSlice(num_token);

    const result: Zir.Inst.Ref = switch (bytes[0]) {
        '-' => switch (kdb.parseNumberLiteral(bytes[1..])) {
            .long => |num| switch (num) {
                1 => .negative_one,
                else => try gz.addLong(-num),
            },
            .failure => unreachable,
        },
        else => switch (kdb.parseNumberLiteral(bytes)) {
            .long => |num| switch (num) {
                0 => .zero,
                1 => .one,
                else => try gz.addLong(num),
            },
            .failure => unreachable,
        },
    };

    return result;
}

fn identifier(gz: *GenZir, scope: *Scope, ri: ResultInfo, ident: Ast.Node.Index) InnerError!Zir.Inst.Ref {
    const astgen = gz.astgen;
    const tree = astgen.tree;
    const main_tokens: []Ast.Token.Index = tree.nodes.items(.main_token);

    const ident_token = main_tokens[ident];
    // const ident_name_raw = tree.tokenSlice(ident_token);

    // if (primitive_instrs.get(ident_name_raw)) |zir_const_ref| {
    //     return rvalue(gz, ri, zir_const_ref, ident);
    // }

    return localVarRef(gz, scope, ri, ident, ident_token);
}

fn localVarRef(
    gz: *GenZir,
    scope: *Scope,
    ri: ResultInfo,
    ident: Ast.Node.Index,
    ident_token: Ast.Token.Index,
) InnerError!Zir.Inst.Ref {
    const astgen = gz.astgen;
    const name_str_index = try astgen.identAsString(ident_token);
    var s = scope;
    var found_already: ?Ast.Node.Index = null; // we have found a decl with the same name already
    var found_needs_tunnel: bool = undefined; // defined when `found_already != null`
    var found_namespaces_out: u32 = undefined; // defined when `found_already != null`

    // The number of namespaces above `gz` we currently are
    var num_namespaces_out: u32 = 0;
    // defined by `num_namespaces_out != 0`
    var capturing_namespace: *Scope.Namespace = undefined;

    while (true) switch (s.tag) {
        .local_val => {
            std.log.debug("local_val", .{});
            const local_val = s.cast(Scope.LocalVal).?;

            if (local_val.name == name_str_index) {
                unreachable;
                // Locals cannot shadow anything, so we do not need to look for ambiguous
                // references in this case.
                // if (ri.rl == .discard and ri.ctx == .assignment) {
                //     local_val.discarded = ident_token;
                // } else {
                //     local_val.used = ident_token;
                // }

                // const value_inst = if (num_namespaces_out != 0) try tunnelThroughClosure(
                //     gz,
                //     ident,
                //     num_namespaces_out,
                //     .{ .ref = local_val.inst },
                //     .{ .token = local_val.token_src },
                // ) else local_val.inst;

                // return rvalueNoCoercePreRef(gz, ri, value_inst, ident);
            }
            s = local_val.parent;
        },
        .local_ptr => {
            std.log.debug("local_ptr", .{});
            const local_ptr = s.cast(Scope.LocalPtr).?;
            if (local_ptr.name == name_str_index) {
                local_ptr.used = ident_token;

                // Can't close over a runtime variable
                if (num_namespaces_out != 0 and !local_ptr.maybe_comptime and !gz.is_typeof) {
                    const ident_name = astgen.tree.tokenSlice(ident_token);
                    return astgen.failNodeNotes(ident, "mutable '{s}' not accessible from here", .{ident_name}, &.{
                        try astgen.errNoteTok(local_ptr.token_src, "declared mutable here", .{}),
                        try astgen.errNoteNode(capturing_namespace.node, "crosses namespace boundary here", .{}),
                    });
                }

                switch (ri.rl) {
                    // .ref, .ref_coerced_ty => {
                    //     const ptr_inst = if (num_namespaces_out != 0) try tunnelThroughClosure(
                    //         gz,
                    //         ident,
                    //         num_namespaces_out,
                    //         .{ .ref = local_ptr.ptr },
                    //         .{ .token = local_ptr.token_src },
                    //     ) else local_ptr.ptr;
                    //     local_ptr.used_as_lvalue = true;
                    //     return ptr_inst;
                    // },
                    else => {
                        unreachable;
                        // const val_inst = if (num_namespaces_out != 0) try tunnelThroughClosure(
                        //     gz,
                        //     ident,
                        //     num_namespaces_out,
                        //     .{ .ref_load = local_ptr.ptr },
                        //     .{ .token = local_ptr.token_src },
                        // ) else try gz.addUnNode(.load, local_ptr.ptr, ident);
                        // return rvalueNoCoercePreRef(gz, ri, val_inst, ident);
                    },
                }
            }
            s = local_ptr.parent;
        },
        .gen_zir => s = s.cast(GenZir).?.parent,
        .namespace => {
            std.log.debug("namespace", .{});
            const ns = s.cast(Scope.Namespace).?;
            if (ns.decls.get(name_str_index)) |i| {
                if (found_already) |f| {
                    return astgen.failNodeNotes(ident, "ambiguous reference", .{}, &.{
                        try astgen.errNoteNode(f, "declared here", .{}),
                        try astgen.errNoteNode(i, "also declared here", .{}),
                    });
                }
                // We found a match but must continue looking for ambiguous references to decls.
                found_already = i;
                found_needs_tunnel = ns.maybe_generic;
                found_namespaces_out = num_namespaces_out;
            }
            num_namespaces_out += 1;
            capturing_namespace = ns;
            s = ns.parent;
        },
        .top => break,
    };
    std.log.debug("found_already = {?}", .{found_already});
    if (found_already == null) {
        const ident_name = astgen.tree.tokenSlice(ident_token);
        return astgen.failNode(ident, "use of undeclared identifier '{s}'", .{ident_name});
    }

    // Decl references happen by name rather than ZIR index so that when unrelated
    // decls are modified, ZIR code containing references to them can be unmodified.

    if (found_namespaces_out > 0 and found_needs_tunnel) {
        unreachable;
        // switch (ri.rl) {
        //     .ref, .ref_coerced_ty => return tunnelThroughClosure(
        //         gz,
        //         ident,
        //         found_namespaces_out,
        //         .{ .decl_ref = name_str_index },
        //         .{ .node = found_already.? },
        //     ),
        //     else => {
        //         const result = try tunnelThroughClosure(
        //             gz,
        //             ident,
        //             found_namespaces_out,
        //             .{ .decl_val = name_str_index },
        //             .{ .node = found_already.? },
        //         );
        //         return rvalueNoCoercePreRef(gz, ri, result, ident);
        //     },
        // }
    }

    switch (ri.rl) {
        // .ref, .ref_coerced_ty => return gz.addStrTok(.decl_ref, name_str_index, ident_token),
        else => return gz.addStrTok(.decl_val, name_str_index, ident_token),
    }
}

/// Returns `true` if the node uses `gz.anon_name_strategy`.
fn nodeUsesAnonNameStrategy(tree: *const Ast, node: Ast.Node.Index) bool {
    const node_tags = tree.nodes.items(.tag);
    switch (node_tags[node]) {
        // TODO: Implement
        // .container_decl,
        // .container_decl_trailing,
        // .container_decl_two,
        // .container_decl_two_trailing,
        // .container_decl_arg,
        // .container_decl_arg_trailing,
        // .tagged_union,
        // .tagged_union_trailing,
        // .tagged_union_two,
        // .tagged_union_two_trailing,
        // .tagged_union_enum_tag,
        // .tagged_union_enum_tag_trailing,
        // => return true,
        // .builtin_call_two, .builtin_call_two_comma, .builtin_call, .builtin_call_comma => {
        //     const builtin_token = tree.nodes.items(.main_token)[node];
        //     const builtin_name = tree.tokenSlice(builtin_token);
        //     return std.mem.eql(u8, builtin_name, "@Type");
        // },
        else => return false,
    }
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
) Allocator.Error!void {
    try astgen.appendErrorNodeNotes(node, format, args, &[0]u32{});
}

fn appendErrorNodeNotes(
    astgen: *AstGen,
    node: Ast.Node.Index,
    comptime format: []const u8,
    args: anytype,
    notes: []const u32,
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
    try astgen.compile_errors.append(astgen.gpa, .{
        .msg = msg,
        .node = node,
        .token = 0,
        .byte_offset = 0,
        .notes = notes_index,
    });
}

fn failNodeNotes(
    astgen: *AstGen,
    node: Ast.Node.Index,
    comptime format: []const u8,
    args: anytype,
    notes: []const u32,
) InnerError {
    try appendErrorNodeNotes(astgen, node, format, args, notes);
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
    token: Ast.Token.Index,
    comptime format: []const u8,
    args: anytype,
) !void {
    try astgen.appendErrorTokNotesOff(token, 0, format, args, &[0]u32{});
}

fn failTokNotes(
    astgen: *AstGen,
    token: Ast.Token.Index,
    comptime format: []const u8,
    args: anytype,
    notes: []const u32,
) InnerError {
    try appendErrorTokNotesOff(astgen, token, 0, format, args, notes);
    return error.AnalysisFail;
}

fn appendErrorTokNotes(
    astgen: *AstGen,
    token: Ast.Token.Index,
    comptime format: []const u8,
    args: anytype,
    notes: []const u32,
) !void {
    return appendErrorTokNotesOff(astgen, token, 0, format, args, notes);
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
    try appendErrorTokNotesOff(astgen, token, byte_offset, format, args, &.{});
    return error.AnalysisFail;
}

fn appendErrorTokNotesOff(
    astgen: *AstGen,
    token: Ast.Token.Index,
    byte_offset: u32,
    comptime format: []const u8,
    args: anytype,
    notes: []const u32,
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
    try astgen.compile_errors.append(gpa, .{
        .msg = msg,
        .node = 0,
        .token = token,
        .byte_offset = byte_offset,
        .notes = notes_index,
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
    });
}

fn identAsString(astgen: *AstGen, ident_token: Ast.Token.Index) !Zir.NullTerminatedString {
    const gpa = astgen.gpa;
    const string_bytes = &astgen.string_bytes;
    const str_index: u32 = @intCast(string_bytes.items.len);
    try string_bytes.appendSlice(gpa, astgen.tree.tokenSlice(ident_token));
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

const Scope = struct {
    tag: Tag,

    fn cast(base: *Scope, comptime T: type) ?*T {
        if (T == Namespace) {
            switch (base.tag) {
                .namespace => return @alignCast(@fieldParentPtr("base", base)),
                else => return null,
            }
        }
        if (base.tag != T.base_tag)
            return null;

        return @alignCast(@fieldParentPtr("base", base));
    }

    const Tag = enum {
        gen_zir,
        local_val,
        local_ptr,
        namespace,
        top,
    };

    /// The category of identifier. These tag names are user-visible in compile errors.
    const IdCat = enum {
        @"function parameter",
        @"local variable",
    };

    /// This is always a `const` local and importantly the `inst` is a value type, not a pointer.
    /// This structure lives as long as the AST generation of the Block
    /// node that contains the variable.
    const LocalVal = struct {
        const base_tag: Tag = .local_val;
        base: Scope = Scope{ .tag = base_tag },
        /// Parents can be: `LocalVal`, `LocalPtr`, `GenZir`, `Defer`, `Namespace`.
        parent: *Scope,
        gen_zir: *GenZir,
        inst: Zir.Inst.Ref,
        /// Source location of the corresponding variable declaration.
        token_src: Ast.Token.Index,
        /// Track the first identifier where it is referenced.
        /// 0 means never referenced.
        used: Ast.Token.Index = 0,
        /// Track the identifier where it is discarded, like this `_ = foo;`.
        /// 0 means never discarded.
        discarded: Ast.Token.Index = 0,
        /// String table index.
        name: Zir.NullTerminatedString,
        id_cat: IdCat,
    };

    /// This could be a `const` or `var` local. It has a pointer instead of a value.
    /// This structure lives as long as the AST generation of the Block
    /// node that contains the variable.
    const LocalPtr = struct {
        const base_tag: Tag = .local_ptr;
        base: Scope = Scope{ .tag = base_tag },
        /// Parents can be: `LocalVal`, `LocalPtr`, `GenZir`, `Defer`, `Namespace`.
        parent: *Scope,
        gen_zir: *GenZir,
        ptr: Zir.Inst.Ref,
        /// Source location of the corresponding variable declaration.
        token_src: Ast.Token.Index,
        /// Track the first identifier where it is referenced.
        /// 0 means never referenced.
        used: Ast.Token.Index = 0,
        /// Track the identifier where it is discarded, like this `_ = foo;`.
        /// 0 means never discarded.
        discarded: Ast.Token.Index = 0,
        /// Whether this value is used as an lvalue after initialization.
        /// If not, we know it can be `const`, so will emit a compile error if it is `var`.
        used_as_lvalue: bool = false,
        /// String table index.
        name: Zir.NullTerminatedString,
        id_cat: IdCat,
        /// true means we find out during Sema whether the value is comptime.
        /// false means it is already known at AstGen the value is runtime-known.
        maybe_comptime: bool,
    };

    /// Represents a global scope that has any number of declarations in it.
    /// Each declaration has this as the parent scope.
    const Namespace = struct {
        const base_tag: Tag = .namespace;
        base: Scope = Scope{ .tag = base_tag },

        /// Parents can be: `LocalVal`, `LocalPtr`, `GenZir`, `Defer`, `Namespace`.
        parent: *Scope,
        /// Maps string table index to the source location of declaration,
        /// for the purposes of reporting name shadowing compile errors.
        decls: std.AutoHashMapUnmanaged(Zir.NullTerminatedString, Ast.Node.Index) = .empty,
        node: Ast.Node.Index,
        inst: Zir.Inst.Index,
        maybe_generic: bool,

        /// The astgen scope containing this namespace.
        /// Only valid during astgen.
        declaring_gz: ?*GenZir,

        /// Set of captures used by this namespace.
        captures: std.AutoArrayHashMapUnmanaged(Zir.Inst.Capture, void) = .empty,

        fn deinit(self: *Namespace, gpa: Allocator) void {
            self.decls.deinit(gpa);
            self.captures.deinit(gpa);
            self.* = undefined;
        }
    };

    const Top = struct {
        const base_tag: Scope.Tag = .top;
        base: Scope = Scope{ .tag = base_tag },
    };
};

/// This is a temporary structure; references to it are valid only
/// while constructing a `Zir`.
const GenZir = struct {
    const base_tag: Scope.Tag = .gen_zir;
    base: Scope = Scope{ .tag = base_tag },
    /// Whether we're already in a scope known to be comptime. This is set
    /// whenever we know Sema will analyze the current block with `is_comptime`,
    /// for instance when we're within a `struct_decl` or a `block_comptime`.
    is_comptime: bool,
    /// Whether we're in an expression within a `@TypeOf` operand. In this case, closure of runtime
    /// variables is permitted where it is usually not.
    is_typeof: bool = false,
    /// This is set to true for a `GenZir` of a `block_inline`, indicating that
    /// exits from this block should use `break_inline` rather than `break`.
    is_inline: bool = false,
    c_import: bool = false,
    /// How decls created in this scope should be named.
    anon_name_strategy: Zir.Inst.NameStrategy = .anon,
    /// The containing decl AST node.
    decl_node_index: Ast.Node.Index,
    /// The containing decl line index, absolute.
    decl_line: u32,
    /// Parents can be: `LocalVal`, `LocalPtr`, `GenZir`, `Defer`, `Namespace`.
    parent: *Scope,
    /// All `GenZir` scopes for the same ZIR share this.
    astgen: *AstGen,
    /// Keeps track of the list of instructions in this scope. Possibly shared.
    /// Indexes to instructions in `astgen`.
    instructions: *ArrayListUnmanaged(Zir.Inst.Index),
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

    fn addBreak(
        gz: *GenZir,
        tag: Zir.Inst.Tag,
        block_inst: Zir.Inst.Index,
        operand: Zir.Inst.Ref,
    ) !Zir.Inst.Index {
        const gpa = gz.astgen.gpa;
        try gz.instructions.ensureUnusedCapacity(gpa, 1);

        const new_index = try gz.makeBreak(tag, block_inst, operand);
        gz.instructions.appendAssumeCapacity(new_index);
        return new_index;
    }

    fn makeBreak(
        gz: *GenZir,
        tag: Zir.Inst.Tag,
        block_inst: Zir.Inst.Index,
        operand: Zir.Inst.Ref,
    ) !Zir.Inst.Index {
        return gz.makeBreakCommon(tag, block_inst, operand, null);
    }

    fn makeBreakCommon(
        gz: *GenZir,
        tag: Zir.Inst.Tag,
        block_inst: Zir.Inst.Index,
        operand: Zir.Inst.Ref,
        operand_src_node: ?Ast.Node.Index,
    ) !Zir.Inst.Index {
        const gpa = gz.astgen.gpa;
        try gz.astgen.instructions.ensureUnusedCapacity(gpa, 1);
        try gz.astgen.extra.ensureUnusedCapacity(gpa, @typeInfo(Zir.Inst.Break).@"struct".fields.len);

        const new_index: Zir.Inst.Index = @enumFromInt(gz.astgen.instructions.len);
        gz.astgen.instructions.appendAssumeCapacity(.{
            .tag = tag,
            .data = .{ .@"break" = .{
                .operand = operand,
                .payload_index = gz.astgen.addExtraAssumeCapacity(Zir.Inst.Break{
                    .operand_src_node = if (operand_src_node) |src_node|
                        gz.nodeIndexToRelative(src_node)
                    else
                        Zir.Inst.Break.no_src_node,
                    .block_inst = block_inst,
                }),
            } },
        });
        return new_index;
    }

    fn instructionsSlice(self: *const GenZir) []Zir.Inst.Index {
        return if (self.instructions_top == unstacked_top)
            &[0]Zir.Inst.Index{}
        else
            self.instructions.items[self.instructions_top..];
    }

    fn instructionsSliceUpto(self: *const GenZir, stacked_gz: *GenZir) []Zir.Inst.Index {
        return if (self.instructions_top == unstacked_top)
            &[0]Zir.Inst.Index{}
        else if (self.instructions == stacked_gz.instructions and stacked_gz.instructions_top != unstacked_top)
            self.instructions.items[self.instructions_top..stacked_gz.instructions_top]
        else
            self.instructions.items[self.instructions_top..];
    }

    fn makeSubBlock(gz: *GenZir, scope: *Scope) GenZir {
        return .{
            .is_comptime = gz.is_comptime,
            .is_typeof = gz.is_typeof,
            .c_import = gz.c_import,
            .decl_node_index = gz.decl_node_index,
            .decl_line = gz.decl_line,
            .parent = scope,
            .astgen = gz.astgen,
            .instructions = gz.instructions,
            .instructions_top = gz.instructions.items.len,
        };
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

    /// Note that this returns a `Zir.Inst.Index` not a ref.
    /// Does *not* append the block instruction to the scope.
    /// Leaves the `payload_index` field undefined. Use `setDeclaration` to finalize.
    fn makeDeclaration(gz: *GenZir, node: Ast.Node.Index) !Zir.Inst.Index {
        const new_index: Zir.Inst.Index = @enumFromInt(gz.astgen.instructions.len);
        try gz.astgen.instructions.append(gz.astgen.gpa, .{
            .tag = .declaration,
            .data = .{ .declaration = .{
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
    fn addFunc(
        gz: *GenZir,
        args: struct {
            src_node: Ast.Node.Index,
            lbrace_line: u32 = 0,
            lbrace_column: u32 = 0,

            body_gz: ?*GenZir,
        },
    ) !Zir.Inst.Ref {
        assert(args.src_node != 0);
        const astgen = gz.astgen;
        const gpa = astgen.gpa;
        const new_index: Zir.Inst.Index = @enumFromInt(astgen.instructions.len);

        try astgen.instructions.ensureUnusedCapacity(gpa, 1);

        var body: []Zir.Inst.Index = &[0]Zir.Inst.Index{};
        var src_locs_and_hash_buffer: [3]u32 = undefined;
        var src_locs_and_hash: []u32 = src_locs_and_hash_buffer[0..0];
        if (args.body_gz) |body_gz| {
            const tree = astgen.tree;
            const node_tags = tree.nodes.items(.tag);
            const token_locs = tree.tokens.items(.loc);
            const fn_decl = args.src_node;
            assert(node_tags[fn_decl] == .lambda or node_tags[fn_decl] == .lambda_semicolon);
            const rbrace_start = token_locs[tree.lastToken(fn_decl)].start;
            astgen.advanceSourceCursor(rbrace_start);
            const rbrace_line: u32 = @intCast(astgen.source_line - gz.decl_line);
            const rbrace_column: u32 = @intCast(astgen.source_column);

            const columns = args.lbrace_column | (rbrace_column << 16);

            src_locs_and_hash_buffer = .{
                args.lbrace_line,
                rbrace_line,
                columns,
            };
            src_locs_and_hash = &src_locs_and_hash_buffer;

            body = body_gz.instructionsSlice();
        }
        const body_len = astgen.countBodyLenAfterFixups(body);

        try astgen.extra.ensureUnusedCapacity(
            gpa,
            @typeInfo(Zir.Inst.Func).@"struct".fields.len + 1 + body_len + src_locs_and_hash.len,
        );

        const payload_index = astgen.addExtraAssumeCapacity(Zir.Inst.Func{
            .body_len = body_len,
        });
        astgen.appendBodyWithFixups(body);
        astgen.extra.appendSliceAssumeCapacity(src_locs_and_hash);

        // Order is important when unstacking.
        if (args.body_gz) |body_gz| body_gz.unstack();

        try gz.instructions.ensureUnusedCapacity(gpa, 1);

        astgen.instructions.appendAssumeCapacity(.{
            .tag = .func,
            .data = .{ .pl_node = .{
                .src_node = gz.nodeIndexToRelative(args.src_node),
                .payload_index = payload_index,
            } },
        });
        gz.instructions.appendAssumeCapacity(new_index);
        return new_index.toRef();
    }

    fn fancyFnExprExtraLen(astgen: *AstGen, body: []Zir.Inst.Index, ref: Zir.Inst.Ref) u32 {
        // In the case of non-empty body, there is one for the body length,
        // and then one for each instruction.
        return countBodyLenAfterFixups(astgen, body) + @intFromBool(ref != .none);
    }

    /// Must be called with the following stack set up:
    ///  * gz (bottom)
    ///  * body_gz (top)
    /// Unstacks all of those except for `gz`.
    fn addLambda(gz: *GenZir, args: struct {
        src_node: Ast.Node.Index,
        lbrace_line: u32 = 0,
        lbrace_column: u32 = 0,
        param_block: Zir.Inst.Index,

        body_gz: ?*GenZir,
    }) !Zir.Inst.Ref {
        assert(args.src_node != 0);
        const astgen = gz.astgen;
        const gpa = astgen.gpa;
        const new_index: Zir.Inst.Index = @enumFromInt(astgen.instructions.len);

        try astgen.instructions.ensureUnusedCapacity(gpa, 1);

        var body: []Zir.Inst.Index = &[0]Zir.Inst.Index{};
        var src_locs_and_hash_buffer: [3]u32 = undefined;
        var src_locs_and_hash: []u32 = src_locs_and_hash_buffer[0..0];
        if (args.body_gz) |body_gz| {
            const tree = astgen.tree;
            const node_tags: []Ast.Node.Tag = tree.nodes.items(.tag);
            const token_locs: []Ast.Token.Loc = tree.tokens.items(.loc);
            const lambda_decl = args.src_node;
            assert(node_tags[lambda_decl] == .lambda or node_tags[lambda_decl] == .lambda_semicolon);
            const rbrace_loc = token_locs[tree.lastToken(lambda_decl)];
            astgen.advanceSourceCursor(rbrace_loc.start);
            const rbrace_line: u32 = @intCast(astgen.source_line - gz.decl_line);
            const rbrace_column: u32 = @intCast(astgen.source_column);

            const columns = args.lbrace_column | (rbrace_column << 16);

            src_locs_and_hash_buffer = .{
                args.lbrace_line,
                rbrace_line,
                columns,
            };
            src_locs_and_hash = &src_locs_and_hash_buffer;

            body = body_gz.instructionsSlice();
        }
        const body_len = astgen.countBodyLenAfterFixups(body);

        try astgen.extra.ensureUnusedCapacity(
            gpa,
            @typeInfo(Zir.Inst.Lambda).@"struct".fields.len + 1 +
                // fancyFnExprExtraLen(astgen, ret_body, ret_ref) +
                body_len + src_locs_and_hash.len,
        );

        const payload_index = astgen.addExtraAssumeCapacity(Zir.Inst.Lambda{
            .param_block = args.param_block,
            .body_len = body_len,
        });
        astgen.appendBodyWithFixups(body);
        astgen.extra.appendSliceAssumeCapacity(src_locs_and_hash);

        // Order is important when unstacking.
        if (args.body_gz) |body_gz| body_gz.unstack();

        try gz.instructions.ensureUnusedCapacity(gpa, 1);

        astgen.instructions.appendAssumeCapacity(.{
            .tag = .lambda,
            .data = .{ .pl_node = .{
                .src_node = gz.nodeIndexToRelative(args.src_node),
                .payload_index = payload_index,
            } },
        });
        gz.instructions.appendAssumeCapacity(new_index);
        return new_index.toRef();
    }

    fn addShow(gz: *GenZir, operand: Zir.Inst.Ref, src_node: Ast.Node.Index) !Zir.Inst.Ref {
        return gz.addUnNode(.show, operand, src_node);
    }

    fn addLong(gz: *GenZir, long: i64) !Zir.Inst.Ref {
        return gz.add(.{
            .tag = .long,
            .data = .{ .long = long },
        });
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

    /// Supports `param_gz` stacked on `gz`. Assumes nothing stacked on `param_gz`. Unstacks `param_gz`.
    fn addParam(
        gz: *GenZir,
        param_gz: *GenZir,
        /// Absolute token index. This function does the conversion to Decl offset.
        abs_tok_index: Ast.Token.Index,
        name: Zir.NullTerminatedString,
    ) !Zir.Inst.Index {
        std.log.debug("name = {s}", .{gz.astgen.nullTerminatedString(name)});
        const gpa = gz.astgen.gpa;
        const param = param_gz.instructions.items[param_gz.instructions_top];
        try gz.astgen.instructions.ensureUnusedCapacity(gpa, 1);
        try gz.astgen.extra.ensureUnusedCapacity(
            gpa,
            @typeInfo(Zir.Inst.Param).@"struct".fields.len,
        );

        const payload_index = gz.astgen.addExtraAssumeCapacity(Zir.Inst.Param{
            .name = name,
            .inst = param,
        });
        param_gz.unstack();

        const new_index: Zir.Inst.Index = @enumFromInt(gz.astgen.instructions.len);
        gz.astgen.instructions.appendAssumeCapacity(.{
            .tag = .param,
            .data = .{ .pl_tok = .{
                .src_tok = gz.tokenIndexToRelative(abs_tok_index),
                .payload_index = payload_index,
            } },
        });
        gz.instructions.appendAssumeCapacity(new_index);
        return new_index;
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

    /// Note that this returns a `Zir.Inst.Index` not a ref.
    /// Does *not* append the block instruction to the scope.
    /// Leaves the `payload_index` field undefined. Use `setDeclaration` to finalize.
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

    fn add(gz: *GenZir, inst: Zir.Inst) !Zir.Inst.Ref {
        return (try gz.addAsIndex(inst)).toRef();
    }

    fn addAsIndex(gz: *GenZir, inst: Zir.Inst) !Zir.Inst.Index {
        const gpa = gz.astgen.gpa;
        try gz.instructions.ensureUnusedCapacity(gpa, 1);
        try gz.astgen.instructions.ensureUnusedCapacity(gpa, 1);

        const new_index: Zir.Inst.Index = @enumFromInt(gz.astgen.instructions.len);
        gz.astgen.instructions.appendAssumeCapacity(inst);
        gz.instructions.appendAssumeCapacity(new_index);
        return new_index;
    }

    fn reserveInstructionIndex(gz: *GenZir) !Zir.Inst.Index {
        const gpa = gz.astgen.gpa;
        try gz.instructions.ensureUnusedCapacity(gpa, 1);
        try gz.astgen.instructions.ensureUnusedCapacity(gpa, 1);

        const new_index: Zir.Inst.Index = @enumFromInt(gz.astgen.instructions.len);
        gz.astgen.instructions.len += 1;
        gz.instructions.appendAssumeCapacity(new_index);
        return new_index;
    }
};

/// This can only be for short-lived references; the memory becomes invalidated
/// when another string is added.
fn nullTerminatedString(astgen: AstGen, index: Zir.NullTerminatedString) [*:0]const u8 {
    return @ptrCast(astgen.string_bytes.items[@intFromEnum(index)..]);
}

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
        try notes.append(gpa, try astgen.errNoteTok(note.token, "{s}", .{msg.items}));
    }

    const extra_offset = tree.errorOffset(parse_err);
    msg.clearRetainingCapacity();
    try tree.renderError(parse_err, msg.writer(gpa));
    try astgen.appendErrorTokNotesOff(parse_err.token, extra_offset, "{s}", .{msg.items}, notes.items);
}

const DeclarationName = union(enum) {
    named: Ast.Token.Index,
    named_test: Ast.Token.Index,
    unnamed_test,
    decltest: Zir.NullTerminatedString,
    @"comptime",
    @"usingnamespace",
};

/// Sets all extra data for a `declaration` instruction.
/// Unstacks `value_gz`, `align_gz`, `linksection_gz`, and `addrspace_gz`.
fn setDeclaration(
    decl_inst: Zir.Inst.Index,
    src_line: u32,
    value_gz: *GenZir,
    /// May be `null` if all these blocks would be empty.
    /// If `null`, then `value_gz` must have nothing stacked on it.
    extra_gzs: ?struct {
        /// Must be stacked on `value_gz`.
        align_gz: *GenZir,
        /// Must be stacked on `align_gz`.
        linksection_gz: *GenZir,
        /// Must be stacked on `linksection_gz`, and have nothing stacked on it.
        addrspace_gz: *GenZir,
    },
) !void {
    const astgen = value_gz.astgen;
    const gpa = astgen.gpa;

    const empty_body: []Zir.Inst.Index = &.{};
    const value_body, const align_body, const linksection_body, const addrspace_body = if (extra_gzs) |e| .{
        value_gz.instructionsSliceUpto(e.align_gz),
        e.align_gz.instructionsSliceUpto(e.linksection_gz),
        e.linksection_gz.instructionsSliceUpto(e.addrspace_gz),
        e.addrspace_gz.instructionsSlice(),
    } else .{ value_gz.instructionsSlice(), empty_body, empty_body, empty_body };

    const value_len = astgen.countBodyLenAfterFixups(value_body);
    const align_len = astgen.countBodyLenAfterFixups(align_body);
    const linksection_len = astgen.countBodyLenAfterFixups(linksection_body);
    const addrspace_len = astgen.countBodyLenAfterFixups(addrspace_body);

    const extra: Zir.Inst.Declaration = .{
        .src_line = src_line,
        .flags = .{
            .value_body_len = @intCast(value_len),
            .has_align_linksection_addrspace = align_len != 0 or linksection_len != 0 or addrspace_len != 0,
        },
    };
    astgen.instructions.items(.data)[@intFromEnum(decl_inst)].declaration.payload_index = try astgen.addExtra(extra);
    if (extra.flags.has_align_linksection_addrspace) {
        try astgen.extra.appendSlice(gpa, &.{
            align_len,
            linksection_len,
            addrspace_len,
        });
    }
    try astgen.extra.ensureUnusedCapacity(gpa, value_len + align_len + linksection_len + addrspace_len);
    astgen.appendBodyWithFixups(value_body);
    if (extra.flags.has_align_linksection_addrspace) {
        astgen.appendBodyWithFixups(align_body);
        astgen.appendBodyWithFixups(linksection_body);
        astgen.appendBodyWithFixups(addrspace_body);
    }

    if (extra_gzs) |e| {
        e.addrspace_gz.unstack();
        e.linksection_gz.unstack();
        e.align_gz.unstack();
    }
    value_gz.unstack();
}
