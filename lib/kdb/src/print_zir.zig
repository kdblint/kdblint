const std = @import("std");
const mem = std.mem;
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const InternPool = @import("InternPool.zig");

const kdb = @import("root.zig");
const Ast = kdb.Ast;
const AstGen = kdb.AstGen;
const Zir = kdb.Zir;
const File = kdb.File;

/// Write human-readable, debug formatted ZIR code to a file.
pub fn renderAsText(
    gpa: Allocator,
    scope_file: *File,
    output: anytype,
) !void {
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();

    var writer: Writer = .{
        .gpa = gpa,
        .arena = arena.allocator(),
        .file = scope_file,
        .code = scope_file.zir,
        .indent = 0,
        .parent_decl_node = 0,
        .recurse_decls = true,
        .recurse_blocks = true,
    };

    var raw_stream = std.io.bufferedWriter(output.writer());
    const stream = raw_stream.writer();

    const inst: Zir.Inst.Index = .file_inst;
    try stream.print("%{d} ", .{@intFromEnum(inst)});
    try writer.writeInstToStream(stream, inst);
    try stream.writeAll("\n");

    const imports_index = scope_file.zir.extra[@intFromEnum(Zir.ExtraIndex.imports)];
    if (imports_index != 0) {
        try stream.writeAll("Imports:\n");

        const extra = scope_file.zir.extraData(Zir.Inst.Imports, imports_index);
        var extra_index = extra.end;

        for (0..extra.data.imports_len) |_| {
            const item = scope_file.zir.extraData(Zir.Inst.Imports.Item, extra_index);
            extra_index = item.end;

            const import_path = scope_file.zir.nullTerminatedString(item.data.name);
            try stream.print("  @import(\"{}\") ", .{
                std.zig.fmtEscapes(import_path),
            });
            try writer.writeSrcTokAbs(stream, item.data.token);
            try stream.writeAll("\n");
        }
    }

    try raw_stream.flush();
}

pub fn renderInstructionContext(
    gpa: Allocator,
    block: []const Zir.Inst.Index,
    block_index: usize,
    scope_file: *File,
    parent_decl_node: Ast.Node.Index,
    indent: u32,
    stream: anytype,
) !void {
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();

    var writer: Writer = .{
        .gpa = gpa,
        .arena = arena.allocator(),
        .file = scope_file,
        .code = scope_file.zir,
        .indent = if (indent < 2) 2 else indent,
        .parent_decl_node = parent_decl_node,
        .recurse_decls = false,
        .recurse_blocks = true,
    };

    try writer.writeBody(stream, block[0..block_index]);
    try stream.writeByteNTimes(' ', writer.indent - 2);
    try stream.print("> %{d} ", .{@intFromEnum(block[block_index])});
    try writer.writeInstToStream(stream, block[block_index]);
    try stream.writeByte('\n');
    if (block_index + 1 < block.len) {
        try writer.writeBody(stream, block[block_index + 1 ..]);
    }
}

pub fn renderSingleInstruction(
    gpa: Allocator,
    inst: Zir.Inst.Index,
    scope_file: *File,
    parent_decl_node: Ast.Node.Index,
    indent: u32,
    stream: anytype,
) !void {
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();

    var writer: Writer = .{
        .gpa = gpa,
        .arena = arena.allocator(),
        .file = scope_file,
        .code = scope_file.zir,
        .indent = indent,
        .parent_decl_node = parent_decl_node,
        .recurse_decls = false,
        .recurse_blocks = false,
    };

    try stream.print("%{d} ", .{@intFromEnum(inst)});
    try writer.writeInstToStream(stream, inst);
}

const Writer = struct {
    gpa: Allocator,
    arena: Allocator,
    file: *File,
    code: Zir,
    indent: u32,
    parent_decl_node: Ast.Node.Index,
    recurse_decls: bool,
    recurse_blocks: bool,

    /// Using `std.zig.findLineColumn` whenever we need to resolve a source location makes ZIR
    /// printing O(N^2), which can have drastic effects - taking a ZIR dump from a few seconds to
    /// many minutes. Since we're usually resolving source locations close to one another,
    /// preserving state across source location resolutions speeds things up a lot.
    line_col_cursor: struct {
        line: usize = 0,
        column: usize = 0,
        line_start: usize = 0,
        off: usize = 0,

        fn find(cur: *@This(), source: []const u8, want_offset: usize) std.zig.Loc {
            if (want_offset <= cur.off) {
                // Go back to the start of this line
                cur.off = cur.line_start;
                cur.column = 0;

                while (want_offset < cur.off) {
                    // Go back to the newline
                    cur.off -= 1;

                    // Seek to the start of the previous line
                    while (cur.off > 0 and source[cur.off - 1] != '\n') {
                        cur.off -= 1;
                    }
                    cur.line_start = cur.off;
                    cur.line -= 1;
                }
            }

            // The cursor is now positioned before `want_offset`.
            // Seek forward as in `std.zig.findLineColumn`.

            while (cur.off < want_offset) : (cur.off += 1) {
                switch (source[cur.off]) {
                    '\n' => {
                        cur.line += 1;
                        cur.column = 0;
                        cur.line_start = cur.off + 1;
                    },
                    else => {
                        cur.column += 1;
                    },
                }
            }

            while (cur.off < source.len and source[cur.off] != '\n') {
                cur.off += 1;
            }

            return .{
                .line = cur.line,
                .column = cur.column,
                .source_line = source[cur.line_start..cur.off],
            };
        }
    } = .{},

    fn relativeToNodeIndex(self: *Writer, offset: i32) Ast.Node.Index {
        return @bitCast(offset + @as(i32, @bitCast(self.parent_decl_node)));
    }

    fn writeInstToStream(
        self: *Writer,
        stream: anytype,
        inst: Zir.Inst.Index,
    ) (@TypeOf(stream).Error || error{OutOfMemory})!void {
        const tags: []Zir.Inst.Tag = self.code.instructions.items(.tag);
        const tag = tags[@intFromEnum(inst)];
        try stream.print("= {s}(", .{@tagName(tag)});
        switch (tag) {
            .file => try self.writePlNodeBlockWithoutSrc(stream, inst),

            .ret_node,
            .signal,
            => try self.writeUnNode(stream, inst),

            .ret_implicit => try self.writeUnTok(stream, inst),

            .assign,
            .global_assign,
            .view,
            .add,
            .subtract,
            .multiply,
            .divide,
            .lesser,
            .greater,
            .fill,
            .equal,
            .less_than,
            .less_than_or_equal,
            .not_equal,
            .greater_than,
            .greater_than_or_equal,
            .join,
            .match,
            .apply_at,
            .dynamic_load,
            => try self.writePlNodeBin(stream, inst),

            .lambda => try self.writeLambda(stream, inst),

            .long => try self.writeLong(stream, inst),

            .str,
            .sym,
            => try self.writeStrTok(stream, inst),

            .param_node => try self.writeStrNode(stream, inst),
            .param_implicit => try self.writeUnTok(stream, inst),

            .identifier => try self.writeStrTok(stream, inst),

            .call => unreachable,
        }
    }

    fn writeExtended(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const extended = self.code.instructions.items(.data)[@intFromEnum(inst)].extended;
        try stream.print("{s}(", .{@tagName(extended.opcode)});
        switch (extended.opcode) {
            .this,
            .ret_addr,
            .error_return_trace,
            .frame,
            .frame_address,
            .breakpoint,
            .disable_instrumentation,
            .c_va_start,
            .in_comptime,
            .value_placeholder,
            => try self.writeExtNode(stream, extended),

            .builtin_src => {
                try stream.writeAll("))");
                const inst_data = self.code.extraData(Zir.Inst.LineColumn, extended.operand).data;
                try stream.print(":{d}:{d}", .{ inst_data.line + 1, inst_data.column + 1 });
            },

            .@"asm" => try self.writeAsm(stream, extended, false),
            .asm_expr => try self.writeAsm(stream, extended, true),
            .variable => try self.writeVarExtended(stream, extended),
            .alloc => try self.writeAllocExtended(stream, extended),

            .compile_log => try self.writeNodeMultiOp(stream, extended),
            .typeof_peer => try self.writeTypeofPeer(stream, extended),
            .min_multi => try self.writeNodeMultiOp(stream, extended),
            .max_multi => try self.writeNodeMultiOp(stream, extended),

            .select => try self.writeSelect(stream, extended),

            .add_with_overflow,
            .sub_with_overflow,
            .mul_with_overflow,
            .shl_with_overflow,
            => try self.writeOverflowArithmetic(stream, extended),

            .struct_decl => try self.writeStructDecl(stream, extended),
            .union_decl => try self.writeUnionDecl(stream, extended),
            .enum_decl => try self.writeEnumDecl(stream, extended),
            .opaque_decl => try self.writeOpaqueDecl(stream, extended),

            .tuple_decl => try self.writeTupleDecl(stream, extended),

            .await_nosuspend,
            .c_undef,
            .c_include,
            .set_float_mode,
            .wasm_memory_size,
            .int_from_error,
            .error_from_int,
            .c_va_copy,
            .c_va_end,
            .work_item_id,
            .work_group_size,
            .work_group_id,
            .branch_hint,
            => {
                const inst_data = self.code.extraData(Zir.Inst.UnNode, extended.operand).data;
                try self.writeInstRef(stream, inst_data.operand);
                try stream.writeAll(")) ");
                try self.writeSrcNode(stream, inst_data.node);
            },

            .reify => {
                const inst_data = self.code.extraData(Zir.Inst.Reify, extended.operand).data;
                try stream.print("line({d}), ", .{inst_data.src_line});
                try self.writeInstRef(stream, inst_data.operand);
                try stream.writeAll(")) ");
                const prev_parent_decl_node = self.parent_decl_node;
                self.parent_decl_node = inst_data.node;
                defer self.parent_decl_node = prev_parent_decl_node;
                try self.writeSrcNode(stream, 0);
            },

            .builtin_extern,
            .c_define,
            .error_cast,
            .wasm_memory_grow,
            .prefetch,
            .c_va_arg,
            => {
                const inst_data = self.code.extraData(Zir.Inst.BinNode, extended.operand).data;
                try self.writeInstRef(stream, inst_data.lhs);
                try stream.writeAll(", ");
                try self.writeInstRef(stream, inst_data.rhs);
                try stream.writeAll(")) ");
                try self.writeSrcNode(stream, inst_data.node);
            },

            .builtin_async_call => try self.writeBuiltinAsyncCall(stream, extended),
            .cmpxchg => try self.writeCmpxchg(stream, extended),
            .ptr_cast_full => try self.writePtrCastFull(stream, extended),
            .ptr_cast_no_dest => try self.writePtrCastNoDest(stream, extended),

            .restore_err_ret_index => try self.writeRestoreErrRetIndex(stream, extended),
            .closure_get => try self.writeClosureGet(stream, extended),
            .field_parent_ptr => try self.writeFieldParentPtr(stream, extended),
            .builtin_value => try self.writeBuiltinValue(stream, extended),
            .inplace_arith_result_ty => try self.writeInplaceArithResultTy(stream, extended),
        }
    }

    fn writeExtNode(self: *Writer, stream: anytype, extended: Zir.Inst.Extended.InstData) !void {
        try stream.writeAll(")) ");
        try self.writeSrcNode(stream, @bitCast(extended.operand));
    }

    fn writeArrayInitElemType(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].bin;
        try self.writeInstRef(stream, inst_data.lhs);
        try stream.print(", {d})", .{@intFromEnum(inst_data.rhs)});
    }

    fn writeUnNode(
        self: *Writer,
        stream: anytype,
        inst: Zir.Inst.Index,
    ) (@TypeOf(stream).Error || error{OutOfMemory})!void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].un_node;
        try self.writeInstRef(stream, inst_data.operand);
        try stream.writeAll(") ");
        try self.writeSrcNode(stream, inst_data.src_node);
    }

    fn writeUnTok(
        self: *Writer,
        stream: anytype,
        inst: Zir.Inst.Index,
    ) (@TypeOf(stream).Error || error{OutOfMemory})!void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].un_tok;
        try self.writeInstRef(stream, inst_data.operand);
        try stream.writeAll(") ");
        try self.writeSrcTok(stream, inst_data.src_tok);
    }

    fn writeValidateDestructure(
        self: *Writer,
        stream: anytype,
        inst: Zir.Inst.Index,
    ) (@TypeOf(stream).Error || error{OutOfMemory})!void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].pl_node;
        const extra = self.code.extraData(Zir.Inst.ValidateDestructure, inst_data.payload_index).data;
        try self.writeInstRef(stream, extra.operand);
        try stream.print(", {d}) (destructure=", .{extra.expect_len});
        try self.writeSrcNode(stream, extra.destructure_node);
        try stream.writeAll(") ");
        try self.writeSrcNode(stream, inst_data.src_node);
    }

    fn writeValidateArrayInitTy(
        self: *Writer,
        stream: anytype,
        inst: Zir.Inst.Index,
    ) (@TypeOf(stream).Error || error{OutOfMemory})!void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].pl_node;
        const extra = self.code.extraData(Zir.Inst.ArrayInit, inst_data.payload_index).data;
        try self.writeInstRef(stream, extra.ty);
        try stream.print(", {d}) ", .{extra.init_count});
        try self.writeSrcNode(stream, inst_data.src_node);
    }

    fn writeArrayTypeSentinel(
        self: *Writer,
        stream: anytype,
        inst: Zir.Inst.Index,
    ) (@TypeOf(stream).Error || error{OutOfMemory})!void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].pl_node;
        const extra = self.code.extraData(Zir.Inst.ArrayTypeSentinel, inst_data.payload_index).data;
        try self.writeInstRef(stream, extra.len);
        try stream.writeAll(", ");
        try self.writeInstRef(stream, extra.sentinel);
        try stream.writeAll(", ");
        try self.writeInstRef(stream, extra.elem_type);
        try stream.writeAll(") ");
        try self.writeSrcNode(stream, inst_data.src_node);
    }

    fn writePtrType(
        self: *Writer,
        stream: anytype,
        inst: Zir.Inst.Index,
    ) (@TypeOf(stream).Error || error{OutOfMemory})!void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].ptr_type;
        const str_allowzero = if (inst_data.flags.is_allowzero) "allowzero, " else "";
        const str_const = if (!inst_data.flags.is_mutable) "const, " else "";
        const str_volatile = if (inst_data.flags.is_volatile) "volatile, " else "";
        const extra = self.code.extraData(Zir.Inst.PtrType, inst_data.payload_index);
        try self.writeInstRef(stream, extra.data.elem_type);
        try stream.print(", {s}{s}{s}{s}", .{
            str_allowzero,
            str_const,
            str_volatile,
            @tagName(inst_data.size),
        });
        var extra_index = extra.end;
        if (inst_data.flags.has_sentinel) {
            try stream.writeAll(", ");
            try self.writeInstRef(stream, @as(Zir.Inst.Ref, @enumFromInt(self.code.extra[extra_index])));
            extra_index += 1;
        }
        if (inst_data.flags.has_align) {
            try stream.writeAll(", align(");
            try self.writeInstRef(stream, @as(Zir.Inst.Ref, @enumFromInt(self.code.extra[extra_index])));
            extra_index += 1;
            if (inst_data.flags.has_bit_range) {
                const bit_start = extra_index + @intFromBool(inst_data.flags.has_addrspace);
                try stream.writeAll(":");
                try self.writeInstRef(stream, @as(Zir.Inst.Ref, @enumFromInt(self.code.extra[bit_start])));
                try stream.writeAll(":");
                try self.writeInstRef(stream, @as(Zir.Inst.Ref, @enumFromInt(self.code.extra[bit_start + 1])));
            }
            try stream.writeAll(")");
        }
        if (inst_data.flags.has_addrspace) {
            try stream.writeAll(", addrspace(");
            try self.writeInstRef(stream, @as(Zir.Inst.Ref, @enumFromInt(self.code.extra[extra_index])));
            try stream.writeAll(")");
        }
        try stream.writeAll(") ");
        try self.writeSrcNode(stream, extra.data.src_node);
    }

    fn writeInt(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].int;
        try stream.print("{d})", .{inst_data});
    }

    fn writeLong(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].long;
        try stream.print("{d})", .{inst_data});
    }

    fn writeIntBig(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].str;
        const byte_count = inst_data.len * @sizeOf(std.math.big.Limb);
        const limb_bytes = self.code.string_bytes[@intFromEnum(inst_data.start)..][0..byte_count];
        // limb_bytes is not aligned properly; we must allocate and copy the bytes
        // in order to accomplish this.
        const limbs = try self.gpa.alloc(std.math.big.Limb, inst_data.len);
        defer self.gpa.free(limbs);

        @memcpy(mem.sliceAsBytes(limbs), limb_bytes);
        const big_int: std.math.big.int.Const = .{
            .limbs = limbs,
            .positive = true,
        };
        const as_string = try big_int.toStringAlloc(self.gpa, 10, .lower);
        defer self.gpa.free(as_string);
        try stream.print("{s})", .{as_string});
    }

    fn writeFloat(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const number = self.code.instructions.items(.data)[@intFromEnum(inst)].float;
        try stream.print("{d})", .{number});
    }

    fn writeFloat128(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].pl_node;
        const extra = self.code.extraData(Zir.Inst.Float128, inst_data.payload_index).data;
        const number = extra.get();
        // TODO improve std.format to be able to print f128 values
        try stream.print("{d}) ", .{@as(f64, @floatCast(number))});
        try self.writeSrcNode(stream, inst_data.src_node);
    }

    fn writeStr(
        self: *Writer,
        stream: anytype,
        inst: Zir.Inst.Index,
    ) (@TypeOf(stream).Error || error{OutOfMemory})!void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].str;
        const str = inst_data.get(self.code);
        try stream.print("\"{}\")", .{std.zig.fmtEscapes(str)});
    }

    fn writeSliceStart(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].pl_node;
        const extra = self.code.extraData(Zir.Inst.SliceStart, inst_data.payload_index).data;
        try self.writeInstRef(stream, extra.lhs);
        try stream.writeAll(", ");
        try self.writeInstRef(stream, extra.start);
        try stream.writeAll(") ");
        try self.writeSrcNode(stream, inst_data.src_node);
    }

    fn writeSliceEnd(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].pl_node;
        const extra = self.code.extraData(Zir.Inst.SliceEnd, inst_data.payload_index).data;
        try self.writeInstRef(stream, extra.lhs);
        try stream.writeAll(", ");
        try self.writeInstRef(stream, extra.start);
        try stream.writeAll(", ");
        try self.writeInstRef(stream, extra.end);
        try stream.writeAll(") ");
        try self.writeSrcNode(stream, inst_data.src_node);
    }

    fn writeSliceSentinel(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].pl_node;
        const extra = self.code.extraData(Zir.Inst.SliceSentinel, inst_data.payload_index).data;
        try self.writeInstRef(stream, extra.lhs);
        try stream.writeAll(", ");
        try self.writeInstRef(stream, extra.start);
        try stream.writeAll(", ");
        try self.writeInstRef(stream, extra.end);
        try stream.writeAll(", ");
        try self.writeInstRef(stream, extra.sentinel);
        try stream.writeAll(") ");
        try self.writeSrcNode(stream, inst_data.src_node);
    }

    fn writeSliceLength(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].pl_node;
        const extra = self.code.extraData(Zir.Inst.SliceLength, inst_data.payload_index).data;
        try self.writeInstRef(stream, extra.lhs);
        try stream.writeAll(", ");
        try self.writeInstRef(stream, extra.start);
        try stream.writeAll(", ");
        try self.writeInstRef(stream, extra.len);
        if (extra.sentinel != .none) {
            try stream.writeAll(", ");
            try self.writeInstRef(stream, extra.sentinel);
        }
        try stream.writeAll(") ");
        try self.writeSrcNode(stream, inst_data.src_node);
    }

    fn writeUnionInit(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].pl_node;
        const extra = self.code.extraData(Zir.Inst.UnionInit, inst_data.payload_index).data;
        try self.writeInstRef(stream, extra.union_type);
        try stream.writeAll(", ");
        try self.writeInstRef(stream, extra.field_name);
        try stream.writeAll(", ");
        try self.writeInstRef(stream, extra.init);
        try stream.writeAll(") ");
        try self.writeSrcNode(stream, inst_data.src_node);
    }

    fn writeShuffle(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].pl_node;
        const extra = self.code.extraData(Zir.Inst.Shuffle, inst_data.payload_index).data;
        try self.writeInstRef(stream, extra.elem_type);
        try stream.writeAll(", ");
        try self.writeInstRef(stream, extra.a);
        try stream.writeAll(", ");
        try self.writeInstRef(stream, extra.b);
        try stream.writeAll(", ");
        try self.writeInstRef(stream, extra.mask);
        try stream.writeAll(") ");
        try self.writeSrcNode(stream, inst_data.src_node);
    }

    fn writeSelect(self: *Writer, stream: anytype, extended: Zir.Inst.Extended.InstData) !void {
        const extra = self.code.extraData(Zir.Inst.Select, extended.operand).data;
        try self.writeInstRef(stream, extra.elem_type);
        try stream.writeAll(", ");
        try self.writeInstRef(stream, extra.pred);
        try stream.writeAll(", ");
        try self.writeInstRef(stream, extra.a);
        try stream.writeAll(", ");
        try self.writeInstRef(stream, extra.b);
        try stream.writeAll(") ");
        try self.writeSrcNode(stream, extra.node);
    }

    fn writeMulAdd(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].pl_node;
        const extra = self.code.extraData(Zir.Inst.MulAdd, inst_data.payload_index).data;
        try self.writeInstRef(stream, extra.mulend1);
        try stream.writeAll(", ");
        try self.writeInstRef(stream, extra.mulend2);
        try stream.writeAll(", ");
        try self.writeInstRef(stream, extra.addend);
        try stream.writeAll(") ");
        try self.writeSrcNode(stream, inst_data.src_node);
    }

    fn writeBuiltinCall(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].pl_node;
        const extra = self.code.extraData(Zir.Inst.BuiltinCall, inst_data.payload_index).data;

        try self.writeFlag(stream, "nodiscard ", extra.flags.ensure_result_used);
        try self.writeFlag(stream, "nosuspend ", extra.flags.is_nosuspend);

        try self.writeInstRef(stream, extra.modifier);
        try stream.writeAll(", ");
        try self.writeInstRef(stream, extra.callee);
        try stream.writeAll(", ");
        try self.writeInstRef(stream, extra.args);
        try stream.writeAll(") ");
        try self.writeSrcNode(stream, inst_data.src_node);
    }

    fn writeFieldParentPtr(self: *Writer, stream: anytype, extended: Zir.Inst.Extended.InstData) !void {
        const extra = self.code.extraData(Zir.Inst.FieldParentPtr, extended.operand).data;
        const FlagsInt = @typeInfo(Zir.Inst.FullPtrCastFlags).@"struct".backing_integer.?;
        const flags: Zir.Inst.FullPtrCastFlags = @bitCast(@as(FlagsInt, @truncate(extended.small)));
        if (flags.align_cast) try stream.writeAll("align_cast, ");
        if (flags.addrspace_cast) try stream.writeAll("addrspace_cast, ");
        if (flags.const_cast) try stream.writeAll("const_cast, ");
        if (flags.volatile_cast) try stream.writeAll("volatile_cast, ");
        try self.writeInstRef(stream, extra.parent_ptr_type);
        try stream.writeAll(", ");
        try self.writeInstRef(stream, extra.field_name);
        try stream.writeAll(", ");
        try self.writeInstRef(stream, extra.field_ptr);
        try stream.writeAll(") ");
        try self.writeSrcNode(stream, extra.src_node);
    }

    fn writeBuiltinAsyncCall(self: *Writer, stream: anytype, extended: Zir.Inst.Extended.InstData) !void {
        const extra = self.code.extraData(Zir.Inst.AsyncCall, extended.operand).data;
        try self.writeInstRef(stream, extra.frame_buffer);
        try stream.writeAll(", ");
        try self.writeInstRef(stream, extra.result_ptr);
        try stream.writeAll(", ");
        try self.writeInstRef(stream, extra.fn_ptr);
        try stream.writeAll(", ");
        try self.writeInstRef(stream, extra.args);
        try stream.writeAll(") ");
        try self.writeSrcNode(stream, extra.node);
    }

    fn writeParam(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].pl_tok;
        const extra = self.code.extraData(Zir.Inst.Param, inst_data.payload_index);
        const body = self.code.bodySlice(extra.end, extra.data.body_len);
        try stream.print("\"{}\", ", .{
            std.zig.fmtEscapes(self.code.nullTerminatedString(extra.data.name)),
        });

        if (extra.data.doc_comment != .empty) {
            try stream.writeAll("\n");
            try self.writeDocComment(stream, extra.data.doc_comment);
            try stream.writeByteNTimes(' ', self.indent);
        }
        try self.writeBracedBody(stream, body);
        try stream.writeAll(") ");
        try self.writeSrcTok(stream, inst_data.src_tok);
    }

    fn writePlNodeBin(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].pl_node;
        const extra = self.code.extraData(Zir.Inst.Bin, inst_data.payload_index).data;
        try self.writeInstRef(stream, extra.lhs);
        try stream.writeAll(", ");
        try self.writeInstRef(stream, extra.rhs);
        try stream.writeAll(") ");
        try self.writeSrcNode(stream, inst_data.src_node);
    }

    fn writePlNodeMultiOp(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].pl_node;
        const extra = self.code.extraData(Zir.Inst.MultiOp, inst_data.payload_index);
        const args = self.code.refSlice(extra.end, extra.data.operands_len);
        try stream.writeAll("{");
        for (args, 0..) |arg, i| {
            if (i != 0) try stream.writeAll(", ");
            try self.writeInstRef(stream, arg);
        }
        try stream.writeAll("}) ");
        try self.writeSrcNode(stream, inst_data.src_node);
    }

    fn writeArrayMul(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].pl_node;
        const extra = self.code.extraData(Zir.Inst.ArrayMul, inst_data.payload_index).data;
        try self.writeInstRef(stream, extra.res_ty);
        try stream.writeAll(", ");
        try self.writeInstRef(stream, extra.lhs);
        try stream.writeAll(", ");
        try self.writeInstRef(stream, extra.rhs);
        try stream.writeAll(") ");
        try self.writeSrcNode(stream, inst_data.src_node);
    }

    fn writeElemValImm(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].elem_val_imm;
        try self.writeInstRef(stream, inst_data.operand);
        try stream.print(", {d})", .{inst_data.idx});
    }

    fn writeArrayInitElemPtr(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].pl_node;
        const extra = self.code.extraData(Zir.Inst.ElemPtrImm, inst_data.payload_index).data;

        try self.writeInstRef(stream, extra.ptr);
        try stream.print(", {d}) ", .{extra.index});
        try self.writeSrcNode(stream, inst_data.src_node);
    }

    fn writePlNodeExport(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].pl_node;
        const extra = self.code.extraData(Zir.Inst.Export, inst_data.payload_index).data;

        try self.writeInstRef(stream, extra.exported);
        try stream.writeAll(", ");
        try self.writeInstRef(stream, extra.options);
        try stream.writeAll(") ");
        try self.writeSrcNode(stream, inst_data.src_node);
    }

    fn writeValidateArrayInitRefTy(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].pl_node;
        const extra = self.code.extraData(Zir.Inst.ArrayInitRefTy, inst_data.payload_index).data;

        try self.writeInstRef(stream, extra.ptr_ty);
        try stream.writeAll(", ");
        try stream.print(", {}) ", .{extra.elem_count});
        try self.writeSrcNode(stream, inst_data.src_node);
    }

    fn writeStructInit(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].pl_node;
        const extra = self.code.extraData(Zir.Inst.StructInit, inst_data.payload_index);
        var field_i: u32 = 0;
        var extra_index = extra.end;

        while (field_i < extra.data.fields_len) : (field_i += 1) {
            const item = self.code.extraData(Zir.Inst.StructInit.Item, extra_index);
            extra_index = item.end;

            if (field_i != 0) {
                try stream.writeAll(", [");
            } else {
                try stream.writeAll("[");
            }
            try self.writeInstIndex(stream, item.data.field_type);
            try stream.writeAll(", ");
            try self.writeInstRef(stream, item.data.init);
            try stream.writeAll("]");
        }
        try stream.writeAll(") ");
        try self.writeSrcNode(stream, inst_data.src_node);
    }

    fn writeCmpxchg(self: *Writer, stream: anytype, extended: Zir.Inst.Extended.InstData) !void {
        const extra = self.code.extraData(Zir.Inst.Cmpxchg, extended.operand).data;

        try self.writeInstRef(stream, extra.ptr);
        try stream.writeAll(", ");
        try self.writeInstRef(stream, extra.expected_value);
        try stream.writeAll(", ");
        try self.writeInstRef(stream, extra.new_value);
        try stream.writeAll(", ");
        try self.writeInstRef(stream, extra.success_order);
        try stream.writeAll(", ");
        try self.writeInstRef(stream, extra.failure_order);
        try stream.writeAll(") ");
        try self.writeSrcNode(stream, extra.node);
    }

    fn writePtrCastFull(self: *Writer, stream: anytype, extended: Zir.Inst.Extended.InstData) !void {
        const FlagsInt = @typeInfo(Zir.Inst.FullPtrCastFlags).@"struct".backing_integer.?;
        const flags: Zir.Inst.FullPtrCastFlags = @bitCast(@as(FlagsInt, @truncate(extended.small)));
        const extra = self.code.extraData(Zir.Inst.BinNode, extended.operand).data;
        if (flags.ptr_cast) try stream.writeAll("ptr_cast, ");
        if (flags.align_cast) try stream.writeAll("align_cast, ");
        if (flags.addrspace_cast) try stream.writeAll("addrspace_cast, ");
        if (flags.const_cast) try stream.writeAll("const_cast, ");
        if (flags.volatile_cast) try stream.writeAll("volatile_cast, ");
        try self.writeInstRef(stream, extra.lhs);
        try stream.writeAll(", ");
        try self.writeInstRef(stream, extra.rhs);
        try stream.writeAll(")) ");
        try self.writeSrcNode(stream, extra.node);
    }

    fn writePtrCastNoDest(self: *Writer, stream: anytype, extended: Zir.Inst.Extended.InstData) !void {
        const FlagsInt = @typeInfo(Zir.Inst.FullPtrCastFlags).@"struct".backing_integer.?;
        const flags: Zir.Inst.FullPtrCastFlags = @bitCast(@as(FlagsInt, @truncate(extended.small)));
        const extra = self.code.extraData(Zir.Inst.UnNode, extended.operand).data;
        if (flags.const_cast) try stream.writeAll("const_cast, ");
        if (flags.volatile_cast) try stream.writeAll("volatile_cast, ");
        try self.writeInstRef(stream, extra.operand);
        try stream.writeAll(")) ");
        try self.writeSrcNode(stream, extra.node);
    }

    fn writeAtomicLoad(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].pl_node;
        const extra = self.code.extraData(Zir.Inst.AtomicLoad, inst_data.payload_index).data;

        try self.writeInstRef(stream, extra.elem_type);
        try stream.writeAll(", ");
        try self.writeInstRef(stream, extra.ptr);
        try stream.writeAll(", ");
        try self.writeInstRef(stream, extra.ordering);
        try stream.writeAll(") ");
        try self.writeSrcNode(stream, inst_data.src_node);
    }

    fn writeAtomicStore(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].pl_node;
        const extra = self.code.extraData(Zir.Inst.AtomicStore, inst_data.payload_index).data;

        try self.writeInstRef(stream, extra.ptr);
        try stream.writeAll(", ");
        try self.writeInstRef(stream, extra.operand);
        try stream.writeAll(", ");
        try self.writeInstRef(stream, extra.ordering);
        try stream.writeAll(") ");
        try self.writeSrcNode(stream, inst_data.src_node);
    }

    fn writeAtomicRmw(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].pl_node;
        const extra = self.code.extraData(Zir.Inst.AtomicRmw, inst_data.payload_index).data;

        try self.writeInstRef(stream, extra.ptr);
        try stream.writeAll(", ");
        try self.writeInstRef(stream, extra.operation);
        try stream.writeAll(", ");
        try self.writeInstRef(stream, extra.operand);
        try stream.writeAll(", ");
        try self.writeInstRef(stream, extra.ordering);
        try stream.writeAll(") ");
        try self.writeSrcNode(stream, inst_data.src_node);
    }

    fn writeStructInitAnon(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].pl_node;
        const extra = self.code.extraData(Zir.Inst.StructInitAnon, inst_data.payload_index);
        var field_i: u32 = 0;
        var extra_index = extra.end;

        while (field_i < extra.data.fields_len) : (field_i += 1) {
            const item = self.code.extraData(Zir.Inst.StructInitAnon.Item, extra_index);
            extra_index = item.end;

            const field_name = self.code.nullTerminatedString(item.data.field_name);

            const prefix = if (field_i != 0) ", [" else "[";
            try stream.print("{s}{s}=", .{ prefix, field_name });
            try self.writeInstRef(stream, item.data.init);
            try stream.writeAll("]");
        }
        try stream.writeAll(") ");
        try self.writeSrcNode(stream, inst_data.src_node);
    }

    fn writeStructInitFieldType(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].pl_node;
        const extra = self.code.extraData(Zir.Inst.FieldType, inst_data.payload_index).data;
        try self.writeInstRef(stream, extra.container_type);
        const field_name = self.code.nullTerminatedString(extra.name_start);
        try stream.print(", {s}) ", .{field_name});
        try self.writeSrcNode(stream, inst_data.src_node);
    }

    fn writeFieldTypeRef(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].pl_node;
        const extra = self.code.extraData(Zir.Inst.FieldTypeRef, inst_data.payload_index).data;
        try self.writeInstRef(stream, extra.container_type);
        try stream.writeAll(", ");
        try self.writeInstRef(stream, extra.field_name);
        try stream.writeAll(") ");
        try self.writeSrcNode(stream, inst_data.src_node);
    }

    fn writeNodeMultiOp(self: *Writer, stream: anytype, extended: Zir.Inst.Extended.InstData) !void {
        const extra = self.code.extraData(Zir.Inst.NodeMultiOp, extended.operand);
        const operands = self.code.refSlice(extra.end, extended.small);

        for (operands, 0..) |operand, i| {
            if (i != 0) try stream.writeAll(", ");
            try self.writeInstRef(stream, operand);
        }
        try stream.writeAll(")) ");
        try self.writeSrcNode(stream, extra.data.src_node);
    }

    fn writeInstNode(
        self: *Writer,
        stream: anytype,
        inst: Zir.Inst.Index,
    ) (@TypeOf(stream).Error || error{OutOfMemory})!void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].inst_node;
        try self.writeInstIndex(stream, inst_data.inst);
        try stream.writeAll(") ");
        try self.writeSrcNode(stream, inst_data.src_node);
    }

    fn writeAsm(
        self: *Writer,
        stream: anytype,
        extended: Zir.Inst.Extended.InstData,
        tmpl_is_expr: bool,
    ) !void {
        const extra = self.code.extraData(Zir.Inst.Asm, extended.operand);
        const outputs_len = @as(u5, @truncate(extended.small));
        const inputs_len = @as(u5, @truncate(extended.small >> 5));
        const clobbers_len = @as(u5, @truncate(extended.small >> 10));
        const is_volatile = @as(u1, @truncate(extended.small >> 15)) != 0;

        try self.writeFlag(stream, "volatile, ", is_volatile);
        if (tmpl_is_expr) {
            try self.writeInstRef(stream, @enumFromInt(@intFromEnum(extra.data.asm_source)));
            try stream.writeAll(", ");
        } else {
            const asm_source = self.code.nullTerminatedString(extra.data.asm_source);
            try stream.print("\"{}\", ", .{std.zig.fmtEscapes(asm_source)});
        }
        try stream.writeAll(", ");

        var extra_i: usize = extra.end;
        var output_type_bits = extra.data.output_type_bits;
        {
            var i: usize = 0;
            while (i < outputs_len) : (i += 1) {
                const output = self.code.extraData(Zir.Inst.Asm.Output, extra_i);
                extra_i = output.end;

                const is_type = @as(u1, @truncate(output_type_bits)) != 0;
                output_type_bits >>= 1;

                const name = self.code.nullTerminatedString(output.data.name);
                const constraint = self.code.nullTerminatedString(output.data.constraint);
                try stream.print("output({p}, \"{}\", ", .{
                    std.zig.fmtId(name), std.zig.fmtEscapes(constraint),
                });
                try self.writeFlag(stream, "->", is_type);
                try self.writeInstRef(stream, output.data.operand);
                try stream.writeAll(")");
                if (i + 1 < outputs_len) {
                    try stream.writeAll("), ");
                }
            }
        }
        {
            var i: usize = 0;
            while (i < inputs_len) : (i += 1) {
                const input = self.code.extraData(Zir.Inst.Asm.Input, extra_i);
                extra_i = input.end;

                const name = self.code.nullTerminatedString(input.data.name);
                const constraint = self.code.nullTerminatedString(input.data.constraint);
                try stream.print("input({p}, \"{}\", ", .{
                    std.zig.fmtId(name), std.zig.fmtEscapes(constraint),
                });
                try self.writeInstRef(stream, input.data.operand);
                try stream.writeAll(")");
                if (i + 1 < inputs_len) {
                    try stream.writeAll(", ");
                }
            }
        }
        {
            var i: usize = 0;
            while (i < clobbers_len) : (i += 1) {
                const str_index = self.code.extra[extra_i];
                extra_i += 1;
                const clobber = self.code.nullTerminatedString(@enumFromInt(str_index));
                try stream.print("{p}", .{std.zig.fmtId(clobber)});
                if (i + 1 < clobbers_len) {
                    try stream.writeAll(", ");
                }
            }
        }
        try stream.writeAll(")) ");
        try self.writeSrcNode(stream, extra.data.src_node);
    }

    fn writeOverflowArithmetic(self: *Writer, stream: anytype, extended: Zir.Inst.Extended.InstData) !void {
        const extra = self.code.extraData(Zir.Inst.BinNode, extended.operand).data;

        try self.writeInstRef(stream, extra.lhs);
        try stream.writeAll(", ");
        try self.writeInstRef(stream, extra.rhs);
        try stream.writeAll(")) ");
        try self.writeSrcNode(stream, extra.node);
    }

    fn writeCall(
        self: *Writer,
        stream: anytype,
        inst: Zir.Inst.Index,
        comptime kind: enum { direct, field },
    ) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].pl_node;
        const ExtraType = switch (kind) {
            .direct => Zir.Inst.Call,
            .field => Zir.Inst.FieldCall,
        };
        const extra = self.code.extraData(ExtraType, inst_data.payload_index);
        const args_len = extra.data.flags.args_len;
        const body = self.code.extra[extra.end..];

        if (extra.data.flags.ensure_result_used) {
            try stream.writeAll("nodiscard ");
        }
        try stream.print(".{s}, ", .{@tagName(@as(std.builtin.CallModifier, @enumFromInt(extra.data.flags.packed_modifier)))});
        switch (kind) {
            .direct => try self.writeInstRef(stream, extra.data.callee),
            .field => {
                const field_name = self.code.nullTerminatedString(extra.data.field_name_start);
                try self.writeInstRef(stream, extra.data.obj_ptr);
                try stream.print(", \"{}\"", .{std.zig.fmtEscapes(field_name)});
            },
        }
        try stream.writeAll(", [");

        self.indent += 2;
        if (args_len != 0) {
            try stream.writeAll("\n");
        }
        var i: usize = 0;
        var arg_start: u32 = args_len;
        while (i < args_len) : (i += 1) {
            try stream.writeByteNTimes(' ', self.indent);
            const arg_end = self.code.extra[extra.end + i];
            defer arg_start = arg_end;
            const arg_body = body[arg_start..arg_end];
            try self.writeBracedBody(stream, @ptrCast(arg_body));

            try stream.writeAll(",\n");
        }
        self.indent -= 2;
        if (args_len != 0) {
            try stream.writeByteNTimes(' ', self.indent);
        }

        try stream.writeAll("]) ");
        try self.writeSrcNode(stream, inst_data.src_node);
    }

    fn writePlNodeBlockWithoutSrc(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const zir_datas: []Zir.Inst.Data = self.code.instructions.items(.data);
        const inst_data = zir_datas[@intFromEnum(inst)].pl_node;
        const extra = self.code.extraData(Zir.Inst.Block, inst_data.payload_index);
        const body = self.code.bodySlice(extra.end, extra.data.body_len);
        try self.writeBracedBody(stream, body);
        try stream.writeAll(")");
    }

    fn writeCondBr(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].pl_node;
        const extra = self.code.extraData(Zir.Inst.CondBr, inst_data.payload_index);
        const then_body = self.code.bodySlice(extra.end, extra.data.then_body_len);
        const else_body = self.code.bodySlice(extra.end + then_body.len, extra.data.else_body_len);
        try self.writeInstRef(stream, extra.data.condition);
        try stream.writeAll(", ");
        try self.writeBracedBody(stream, then_body);
        try stream.writeAll(", ");
        try self.writeBracedBody(stream, else_body);
        try stream.writeAll(") ");
        try self.writeSrcNode(stream, inst_data.src_node);
    }

    fn writeTry(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].pl_node;
        const extra = self.code.extraData(Zir.Inst.Try, inst_data.payload_index);
        const body = self.code.bodySlice(extra.end, extra.data.body_len);
        try self.writeInstRef(stream, extra.data.operand);
        try stream.writeAll(", ");
        try self.writeBracedBody(stream, body);
        try stream.writeAll(") ");
        try self.writeSrcNode(stream, inst_data.src_node);
    }

    fn writeStructDecl(self: *Writer, stream: anytype, extended: Zir.Inst.Extended.InstData) !void {
        const small: Zir.Inst.StructDecl.Small = @bitCast(extended.small);

        const extra = self.code.extraData(Zir.Inst.StructDecl, extended.operand);

        const prev_parent_decl_node = self.parent_decl_node;
        self.parent_decl_node = extra.data.src_node;
        defer self.parent_decl_node = prev_parent_decl_node;

        const fields_hash: std.zig.SrcHash = @bitCast([4]u32{
            extra.data.fields_hash_0,
            extra.data.fields_hash_1,
            extra.data.fields_hash_2,
            extra.data.fields_hash_3,
        });

        try stream.print("hash({}) ", .{std.fmt.fmtSliceHexLower(&fields_hash)});

        var extra_index: usize = extra.end;

        const captures_len = if (small.has_captures_len) blk: {
            const captures_len = self.code.extra[extra_index];
            extra_index += 1;
            break :blk captures_len;
        } else 0;

        const fields_len = if (small.has_fields_len) blk: {
            const fields_len = self.code.extra[extra_index];
            extra_index += 1;
            break :blk fields_len;
        } else 0;

        const decls_len = if (small.has_decls_len) blk: {
            const decls_len = self.code.extra[extra_index];
            extra_index += 1;
            break :blk decls_len;
        } else 0;

        try self.writeFlag(stream, "known_non_opv, ", small.known_non_opv);
        try self.writeFlag(stream, "known_comptime_only, ", small.known_comptime_only);

        try stream.print("{s}, ", .{@tagName(small.name_strategy)});

        if (captures_len == 0) {
            try stream.writeAll("{}, ");
        } else {
            try stream.writeAll("{ ");
            try self.writeCapture(stream, @bitCast(self.code.extra[extra_index]));
            extra_index += 1;
            for (1..captures_len) |_| {
                try stream.writeAll(", ");
                try self.writeCapture(stream, @bitCast(self.code.extra[extra_index]));
                extra_index += 1;
            }
            try stream.writeAll(" }, ");
        }

        if (small.has_backing_int) {
            const backing_int_body_len = self.code.extra[extra_index];
            extra_index += 1;
            try stream.writeAll("packed(");
            if (backing_int_body_len == 0) {
                const backing_int_ref: Zir.Inst.Ref = @enumFromInt(self.code.extra[extra_index]);
                extra_index += 1;
                try self.writeInstRef(stream, backing_int_ref);
            } else {
                const body = self.code.bodySlice(extra_index, backing_int_body_len);
                extra_index += backing_int_body_len;
                self.indent += 2;
                try self.writeBracedDecl(stream, body);
                self.indent -= 2;
            }
            try stream.writeAll("), ");
        } else {
            try stream.print("{s}, ", .{@tagName(small.layout)});
        }

        if (decls_len == 0) {
            try stream.writeAll("{}, ");
        } else {
            try stream.writeAll("{\n");
            self.indent += 2;
            try self.writeBody(stream, self.code.bodySlice(extra_index, decls_len));
            self.indent -= 2;
            extra_index += decls_len;
            try stream.writeByteNTimes(' ', self.indent);
            try stream.writeAll("}, ");
        }

        if (fields_len == 0) {
            try stream.writeAll("{}, {}) ");
        } else {
            const bits_per_field = 4;
            const fields_per_u32 = 32 / bits_per_field;
            const bit_bags_count = std.math.divCeil(usize, fields_len, fields_per_u32) catch unreachable;
            const Field = struct {
                doc_comment_index: Zir.NullTerminatedString,
                type_len: u32 = 0,
                align_len: u32 = 0,
                init_len: u32 = 0,
                type: Zir.Inst.Ref = .none,
                name: Zir.NullTerminatedString,
                is_comptime: bool,
            };
            const fields = try self.arena.alloc(Field, fields_len);
            {
                var bit_bag_index: usize = extra_index;
                extra_index += bit_bags_count;
                var cur_bit_bag: u32 = undefined;
                var field_i: u32 = 0;
                while (field_i < fields_len) : (field_i += 1) {
                    if (field_i % fields_per_u32 == 0) {
                        cur_bit_bag = self.code.extra[bit_bag_index];
                        bit_bag_index += 1;
                    }
                    const has_align = @as(u1, @truncate(cur_bit_bag)) != 0;
                    cur_bit_bag >>= 1;
                    const has_default = @as(u1, @truncate(cur_bit_bag)) != 0;
                    cur_bit_bag >>= 1;
                    const is_comptime = @as(u1, @truncate(cur_bit_bag)) != 0;
                    cur_bit_bag >>= 1;
                    const has_type_body = @as(u1, @truncate(cur_bit_bag)) != 0;
                    cur_bit_bag >>= 1;

                    const field_name_index: Zir.NullTerminatedString = @enumFromInt(self.code.extra[extra_index]);
                    extra_index += 1;
                    const doc_comment_index: Zir.NullTerminatedString = @enumFromInt(self.code.extra[extra_index]);
                    extra_index += 1;

                    fields[field_i] = .{
                        .doc_comment_index = doc_comment_index,
                        .is_comptime = is_comptime,
                        .name = field_name_index,
                    };

                    if (has_type_body) {
                        fields[field_i].type_len = self.code.extra[extra_index];
                    } else {
                        fields[field_i].type = @enumFromInt(self.code.extra[extra_index]);
                    }
                    extra_index += 1;

                    if (has_align) {
                        fields[field_i].align_len = self.code.extra[extra_index];
                        extra_index += 1;
                    }

                    if (has_default) {
                        fields[field_i].init_len = self.code.extra[extra_index];
                        extra_index += 1;
                    }
                }
            }

            try stream.writeAll("{\n");
            self.indent += 2;

            for (fields, 0..) |field, i| {
                try self.writeDocComment(stream, field.doc_comment_index);
                try stream.writeByteNTimes(' ', self.indent);
                try self.writeFlag(stream, "comptime ", field.is_comptime);
                if (field.name != .empty) {
                    const field_name = self.code.nullTerminatedString(field.name);
                    try stream.print("{p}: ", .{std.zig.fmtId(field_name)});
                } else {
                    try stream.print("@\"{d}\": ", .{i});
                }
                if (field.type != .none) {
                    try self.writeInstRef(stream, field.type);
                }

                if (field.type_len > 0) {
                    const body = self.code.bodySlice(extra_index, field.type_len);
                    extra_index += body.len;
                    self.indent += 2;
                    try self.writeBracedDecl(stream, body);
                    self.indent -= 2;
                }

                if (field.align_len > 0) {
                    const body = self.code.bodySlice(extra_index, field.align_len);
                    extra_index += body.len;
                    self.indent += 2;
                    try stream.writeAll(" align(");
                    try self.writeBracedDecl(stream, body);
                    try stream.writeAll(")");
                    self.indent -= 2;
                }

                if (field.init_len > 0) {
                    const body = self.code.bodySlice(extra_index, field.init_len);
                    extra_index += body.len;
                    self.indent += 2;
                    try stream.writeAll(" = ");
                    try self.writeBracedDecl(stream, body);
                    self.indent -= 2;
                }

                try stream.writeAll(",\n");
            }

            self.indent -= 2;
            try stream.writeByteNTimes(' ', self.indent);
            try stream.writeAll("}) ");
        }
        try self.writeSrcNode(stream, 0);
    }

    fn writeUnionDecl(self: *Writer, stream: anytype, extended: Zir.Inst.Extended.InstData) !void {
        const small = @as(Zir.Inst.UnionDecl.Small, @bitCast(extended.small));

        const extra = self.code.extraData(Zir.Inst.UnionDecl, extended.operand);

        const prev_parent_decl_node = self.parent_decl_node;
        self.parent_decl_node = extra.data.src_node;
        defer self.parent_decl_node = prev_parent_decl_node;

        const fields_hash: std.zig.SrcHash = @bitCast([4]u32{
            extra.data.fields_hash_0,
            extra.data.fields_hash_1,
            extra.data.fields_hash_2,
            extra.data.fields_hash_3,
        });

        try stream.print("hash({}) ", .{std.fmt.fmtSliceHexLower(&fields_hash)});

        var extra_index: usize = extra.end;

        const tag_type_ref = if (small.has_tag_type) blk: {
            const tag_type_ref = @as(Zir.Inst.Ref, @enumFromInt(self.code.extra[extra_index]));
            extra_index += 1;
            break :blk tag_type_ref;
        } else .none;

        const captures_len = if (small.has_captures_len) blk: {
            const captures_len = self.code.extra[extra_index];
            extra_index += 1;
            break :blk captures_len;
        } else 0;

        const body_len = if (small.has_body_len) blk: {
            const body_len = self.code.extra[extra_index];
            extra_index += 1;
            break :blk body_len;
        } else 0;

        const fields_len = if (small.has_fields_len) blk: {
            const fields_len = self.code.extra[extra_index];
            extra_index += 1;
            break :blk fields_len;
        } else 0;

        const decls_len = if (small.has_decls_len) blk: {
            const decls_len = self.code.extra[extra_index];
            extra_index += 1;
            break :blk decls_len;
        } else 0;

        try stream.print("{s}, {s}, ", .{
            @tagName(small.name_strategy), @tagName(small.layout),
        });
        try self.writeFlag(stream, "autoenum, ", small.auto_enum_tag);

        if (captures_len == 0) {
            try stream.writeAll("{}, ");
        } else {
            try stream.writeAll("{ ");
            try self.writeCapture(stream, @bitCast(self.code.extra[extra_index]));
            extra_index += 1;
            for (1..captures_len) |_| {
                try stream.writeAll(", ");
                try self.writeCapture(stream, @bitCast(self.code.extra[extra_index]));
                extra_index += 1;
            }
            try stream.writeAll(" }, ");
        }

        if (decls_len == 0) {
            try stream.writeAll("{}");
        } else {
            try stream.writeAll("{\n");
            self.indent += 2;
            try self.writeBody(stream, self.code.bodySlice(extra_index, decls_len));
            self.indent -= 2;
            extra_index += decls_len;
            try stream.writeByteNTimes(' ', self.indent);
            try stream.writeAll("}");
        }

        if (tag_type_ref != .none) {
            try stream.writeAll(", ");
            try self.writeInstRef(stream, tag_type_ref);
        }

        if (fields_len == 0) {
            try stream.writeAll("}) ");
            try self.writeSrcNode(stream, 0);
            return;
        }
        try stream.writeAll(", ");

        const body = self.code.bodySlice(extra_index, body_len);
        extra_index += body.len;

        try self.writeBracedDecl(stream, body);
        try stream.writeAll(", {\n");

        self.indent += 2;
        const bits_per_field = 4;
        const fields_per_u32 = 32 / bits_per_field;
        const bit_bags_count = std.math.divCeil(usize, fields_len, fields_per_u32) catch unreachable;
        const body_end = extra_index;
        extra_index += bit_bags_count;
        var bit_bag_index: usize = body_end;
        var cur_bit_bag: u32 = undefined;
        var field_i: u32 = 0;
        while (field_i < fields_len) : (field_i += 1) {
            if (field_i % fields_per_u32 == 0) {
                cur_bit_bag = self.code.extra[bit_bag_index];
                bit_bag_index += 1;
            }
            const has_type = @as(u1, @truncate(cur_bit_bag)) != 0;
            cur_bit_bag >>= 1;
            const has_align = @as(u1, @truncate(cur_bit_bag)) != 0;
            cur_bit_bag >>= 1;
            const has_value = @as(u1, @truncate(cur_bit_bag)) != 0;
            cur_bit_bag >>= 1;
            const unused = @as(u1, @truncate(cur_bit_bag)) != 0;
            cur_bit_bag >>= 1;

            _ = unused;

            const field_name_index: Zir.NullTerminatedString = @enumFromInt(self.code.extra[extra_index]);
            const field_name = self.code.nullTerminatedString(field_name_index);
            extra_index += 1;
            const doc_comment_index: Zir.NullTerminatedString = @enumFromInt(self.code.extra[extra_index]);
            extra_index += 1;

            try self.writeDocComment(stream, doc_comment_index);
            try stream.writeByteNTimes(' ', self.indent);
            try stream.print("{p}", .{std.zig.fmtId(field_name)});

            if (has_type) {
                const field_type = @as(Zir.Inst.Ref, @enumFromInt(self.code.extra[extra_index]));
                extra_index += 1;

                try stream.writeAll(": ");
                try self.writeInstRef(stream, field_type);
            }
            if (has_align) {
                const align_ref = @as(Zir.Inst.Ref, @enumFromInt(self.code.extra[extra_index]));
                extra_index += 1;

                try stream.writeAll(" align(");
                try self.writeInstRef(stream, align_ref);
                try stream.writeAll(")");
            }
            if (has_value) {
                const default_ref = @as(Zir.Inst.Ref, @enumFromInt(self.code.extra[extra_index]));
                extra_index += 1;

                try stream.writeAll(" = ");
                try self.writeInstRef(stream, default_ref);
            }
            try stream.writeAll(",\n");
        }

        self.indent -= 2;
        try stream.writeByteNTimes(' ', self.indent);
        try stream.writeAll("}) ");
        try self.writeSrcNode(stream, 0);
    }

    fn writeEnumDecl(self: *Writer, stream: anytype, extended: Zir.Inst.Extended.InstData) !void {
        const small = @as(Zir.Inst.EnumDecl.Small, @bitCast(extended.small));

        const extra = self.code.extraData(Zir.Inst.EnumDecl, extended.operand);

        const prev_parent_decl_node = self.parent_decl_node;
        self.parent_decl_node = extra.data.src_node;
        defer self.parent_decl_node = prev_parent_decl_node;

        const fields_hash: std.zig.SrcHash = @bitCast([4]u32{
            extra.data.fields_hash_0,
            extra.data.fields_hash_1,
            extra.data.fields_hash_2,
            extra.data.fields_hash_3,
        });

        try stream.print("hash({}) ", .{std.fmt.fmtSliceHexLower(&fields_hash)});

        var extra_index: usize = extra.end;

        const tag_type_ref = if (small.has_tag_type) blk: {
            const tag_type_ref = @as(Zir.Inst.Ref, @enumFromInt(self.code.extra[extra_index]));
            extra_index += 1;
            break :blk tag_type_ref;
        } else .none;

        const captures_len = if (small.has_captures_len) blk: {
            const captures_len = self.code.extra[extra_index];
            extra_index += 1;
            break :blk captures_len;
        } else 0;

        const body_len = if (small.has_body_len) blk: {
            const body_len = self.code.extra[extra_index];
            extra_index += 1;
            break :blk body_len;
        } else 0;

        const fields_len = if (small.has_fields_len) blk: {
            const fields_len = self.code.extra[extra_index];
            extra_index += 1;
            break :blk fields_len;
        } else 0;

        const decls_len = if (small.has_decls_len) blk: {
            const decls_len = self.code.extra[extra_index];
            extra_index += 1;
            break :blk decls_len;
        } else 0;

        try stream.print("{s}, ", .{@tagName(small.name_strategy)});
        try self.writeFlag(stream, "nonexhaustive, ", small.nonexhaustive);

        if (captures_len == 0) {
            try stream.writeAll("{}, ");
        } else {
            try stream.writeAll("{ ");
            try self.writeCapture(stream, @bitCast(self.code.extra[extra_index]));
            extra_index += 1;
            for (1..captures_len) |_| {
                try stream.writeAll(", ");
                try self.writeCapture(stream, @bitCast(self.code.extra[extra_index]));
                extra_index += 1;
            }
            try stream.writeAll(" }, ");
        }

        if (decls_len == 0) {
            try stream.writeAll("{}, ");
        } else {
            try stream.writeAll("{\n");
            self.indent += 2;
            try self.writeBody(stream, self.code.bodySlice(extra_index, decls_len));
            self.indent -= 2;
            extra_index += decls_len;
            try stream.writeByteNTimes(' ', self.indent);
            try stream.writeAll("}, ");
        }

        if (tag_type_ref != .none) {
            try self.writeInstRef(stream, tag_type_ref);
            try stream.writeAll(", ");
        }

        const body = self.code.bodySlice(extra_index, body_len);
        extra_index += body.len;

        try self.writeBracedDecl(stream, body);
        if (fields_len == 0) {
            try stream.writeAll(", {}) ");
        } else {
            try stream.writeAll(", {\n");

            self.indent += 2;
            const bit_bags_count = std.math.divCeil(usize, fields_len, 32) catch unreachable;
            const body_end = extra_index;
            extra_index += bit_bags_count;
            var bit_bag_index: usize = body_end;
            var cur_bit_bag: u32 = undefined;
            var field_i: u32 = 0;
            while (field_i < fields_len) : (field_i += 1) {
                if (field_i % 32 == 0) {
                    cur_bit_bag = self.code.extra[bit_bag_index];
                    bit_bag_index += 1;
                }
                const has_tag_value = @as(u1, @truncate(cur_bit_bag)) != 0;
                cur_bit_bag >>= 1;

                const field_name = self.code.nullTerminatedString(@enumFromInt(self.code.extra[extra_index]));
                extra_index += 1;

                const doc_comment_index: Zir.NullTerminatedString = @enumFromInt(self.code.extra[extra_index]);
                extra_index += 1;

                try self.writeDocComment(stream, doc_comment_index);

                try stream.writeByteNTimes(' ', self.indent);
                try stream.print("{p}", .{std.zig.fmtId(field_name)});

                if (has_tag_value) {
                    const tag_value_ref = @as(Zir.Inst.Ref, @enumFromInt(self.code.extra[extra_index]));
                    extra_index += 1;

                    try stream.writeAll(" = ");
                    try self.writeInstRef(stream, tag_value_ref);
                }
                try stream.writeAll(",\n");
            }
            self.indent -= 2;
            try stream.writeByteNTimes(' ', self.indent);
            try stream.writeAll("}) ");
        }
        try self.writeSrcNode(stream, 0);
    }

    fn writeOpaqueDecl(
        self: *Writer,
        stream: anytype,
        extended: Zir.Inst.Extended.InstData,
    ) !void {
        const small = @as(Zir.Inst.OpaqueDecl.Small, @bitCast(extended.small));
        const extra = self.code.extraData(Zir.Inst.OpaqueDecl, extended.operand);

        const prev_parent_decl_node = self.parent_decl_node;
        self.parent_decl_node = extra.data.src_node;
        defer self.parent_decl_node = prev_parent_decl_node;

        var extra_index: usize = extra.end;

        const captures_len = if (small.has_captures_len) blk: {
            const captures_len = self.code.extra[extra_index];
            extra_index += 1;
            break :blk captures_len;
        } else 0;

        const decls_len = if (small.has_decls_len) blk: {
            const decls_len = self.code.extra[extra_index];
            extra_index += 1;
            break :blk decls_len;
        } else 0;

        try stream.print("{s}, ", .{@tagName(small.name_strategy)});

        if (captures_len == 0) {
            try stream.writeAll("{}, ");
        } else {
            try stream.writeAll("{ ");
            try self.writeCapture(stream, @bitCast(self.code.extra[extra_index]));
            extra_index += 1;
            for (1..captures_len) |_| {
                try stream.writeAll(", ");
                try self.writeCapture(stream, @bitCast(self.code.extra[extra_index]));
                extra_index += 1;
            }
            try stream.writeAll(" }, ");
        }

        if (decls_len == 0) {
            try stream.writeAll("{}) ");
        } else {
            try stream.writeAll("{\n");
            self.indent += 2;
            try self.writeBody(stream, self.code.bodySlice(extra_index, decls_len));
            self.indent -= 2;
            try stream.writeByteNTimes(' ', self.indent);
            try stream.writeAll("}) ");
        }
        try self.writeSrcNode(stream, 0);
    }

    fn writeTupleDecl(self: *Writer, stream: anytype, extended: Zir.Inst.Extended.InstData) !void {
        const fields_len = extended.small;
        assert(fields_len != 0);
        const extra = self.code.extraData(Zir.Inst.TupleDecl, extended.operand);

        var extra_index = extra.end;

        try stream.writeAll("{ ");

        for (0..fields_len) |field_idx| {
            if (field_idx != 0) try stream.writeAll(", ");

            const field_ty, const field_init = self.code.extra[extra_index..][0..2].*;
            extra_index += 2;

            try stream.print("@\"{d}\": ", .{field_idx});
            try self.writeInstRef(stream, @enumFromInt(field_ty));
            try stream.writeAll(" = ");
            try self.writeInstRef(stream, @enumFromInt(field_init));
        }

        try stream.writeAll(" }) ");

        try self.writeSrcNode(stream, extra.data.src_node);
    }

    fn writeErrorSetDecl(
        self: *Writer,
        stream: anytype,
        inst: Zir.Inst.Index,
    ) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].pl_node;
        const extra = self.code.extraData(Zir.Inst.ErrorSetDecl, inst_data.payload_index);

        try stream.writeAll("{\n");
        self.indent += 2;

        var extra_index = @as(u32, @intCast(extra.end));
        const extra_index_end = extra_index + (extra.data.fields_len * 2);
        while (extra_index < extra_index_end) : (extra_index += 2) {
            const name_index: Zir.NullTerminatedString = @enumFromInt(self.code.extra[extra_index]);
            const name = self.code.nullTerminatedString(name_index);
            const doc_comment_index: Zir.NullTerminatedString = @enumFromInt(self.code.extra[extra_index + 1]);
            try self.writeDocComment(stream, doc_comment_index);
            try stream.writeByteNTimes(' ', self.indent);
            try stream.print("{p},\n", .{std.zig.fmtId(name)});
        }

        self.indent -= 2;
        try stream.writeByteNTimes(' ', self.indent);
        try stream.writeAll("}) ");

        try self.writeSrcNode(stream, inst_data.src_node);
    }

    fn writeSwitchBlockErrUnion(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].pl_node;
        const extra = self.code.extraData(Zir.Inst.SwitchBlockErrUnion, inst_data.payload_index);

        var extra_index: usize = extra.end;

        const multi_cases_len = if (extra.data.bits.has_multi_cases) blk: {
            const multi_cases_len = self.code.extra[extra_index];
            extra_index += 1;
            break :blk multi_cases_len;
        } else 0;

        const err_capture_inst: Zir.Inst.Index = if (extra.data.bits.any_uses_err_capture) blk: {
            const tag_capture_inst = self.code.extra[extra_index];
            extra_index += 1;
            break :blk @enumFromInt(tag_capture_inst);
        } else undefined;

        try self.writeInstRef(stream, extra.data.operand);

        if (extra.data.bits.any_uses_err_capture) {
            try stream.writeAll(", err_capture=");
            try self.writeInstIndex(stream, err_capture_inst);
        }

        self.indent += 2;

        {
            const info = @as(Zir.Inst.SwitchBlock.ProngInfo, @bitCast(self.code.extra[extra_index]));
            extra_index += 1;

            assert(!info.is_inline);
            const body = self.code.bodySlice(extra_index, info.body_len);
            extra_index += body.len;

            try stream.writeAll(",\n");
            try stream.writeByteNTimes(' ', self.indent);
            try stream.writeAll("non_err => ");
            try self.writeBracedBody(stream, body);
        }

        if (extra.data.bits.has_else) {
            const info = @as(Zir.Inst.SwitchBlock.ProngInfo, @bitCast(self.code.extra[extra_index]));
            extra_index += 1;
            const capture_text = switch (info.capture) {
                .none => "",
                .by_val => "by_val ",
                .by_ref => "by_ref ",
            };
            const inline_text = if (info.is_inline) "inline " else "";
            const body = self.code.bodySlice(extra_index, info.body_len);
            extra_index += body.len;

            try stream.writeAll(",\n");
            try stream.writeByteNTimes(' ', self.indent);
            try stream.print("{s}{s}else => ", .{ capture_text, inline_text });
            try self.writeBracedBody(stream, body);
        }

        {
            const scalar_cases_len = extra.data.bits.scalar_cases_len;
            var scalar_i: usize = 0;
            while (scalar_i < scalar_cases_len) : (scalar_i += 1) {
                const item_ref = @as(Zir.Inst.Ref, @enumFromInt(self.code.extra[extra_index]));
                extra_index += 1;
                const info = @as(Zir.Inst.SwitchBlock.ProngInfo, @bitCast(self.code.extra[extra_index]));
                extra_index += 1;
                const body = self.code.bodySlice(extra_index, info.body_len);
                extra_index += info.body_len;

                try stream.writeAll(",\n");
                try stream.writeByteNTimes(' ', self.indent);
                switch (info.capture) {
                    .none => {},
                    .by_val => try stream.writeAll("by_val "),
                    .by_ref => try stream.writeAll("by_ref "),
                }
                if (info.is_inline) try stream.writeAll("inline ");
                try self.writeInstRef(stream, item_ref);
                try stream.writeAll(" => ");
                try self.writeBracedBody(stream, body);
            }
        }
        {
            var multi_i: usize = 0;
            while (multi_i < multi_cases_len) : (multi_i += 1) {
                const items_len = self.code.extra[extra_index];
                extra_index += 1;
                const ranges_len = self.code.extra[extra_index];
                extra_index += 1;
                const info = @as(Zir.Inst.SwitchBlock.ProngInfo, @bitCast(self.code.extra[extra_index]));
                extra_index += 1;
                const items = self.code.refSlice(extra_index, items_len);
                extra_index += items_len;

                try stream.writeAll(",\n");
                try stream.writeByteNTimes(' ', self.indent);
                switch (info.capture) {
                    .none => {},
                    .by_val => try stream.writeAll("by_val "),
                    .by_ref => try stream.writeAll("by_ref "),
                }
                if (info.is_inline) try stream.writeAll("inline ");

                for (items, 0..) |item_ref, item_i| {
                    if (item_i != 0) try stream.writeAll(", ");
                    try self.writeInstRef(stream, item_ref);
                }

                var range_i: usize = 0;
                while (range_i < ranges_len) : (range_i += 1) {
                    const item_first = @as(Zir.Inst.Ref, @enumFromInt(self.code.extra[extra_index]));
                    extra_index += 1;
                    const item_last = @as(Zir.Inst.Ref, @enumFromInt(self.code.extra[extra_index]));
                    extra_index += 1;

                    if (range_i != 0 or items.len != 0) {
                        try stream.writeAll(", ");
                    }
                    try self.writeInstRef(stream, item_first);
                    try stream.writeAll("...");
                    try self.writeInstRef(stream, item_last);
                }

                const body = self.code.bodySlice(extra_index, info.body_len);
                extra_index += info.body_len;
                try stream.writeAll(" => ");
                try self.writeBracedBody(stream, body);
            }
        }

        self.indent -= 2;

        try stream.writeAll(") ");
        try self.writeSrcNode(stream, inst_data.src_node);
    }

    fn writeSwitchBlock(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].pl_node;
        const extra = self.code.extraData(Zir.Inst.SwitchBlock, inst_data.payload_index);

        var extra_index: usize = extra.end;

        const multi_cases_len = if (extra.data.bits.has_multi_cases) blk: {
            const multi_cases_len = self.code.extra[extra_index];
            extra_index += 1;
            break :blk multi_cases_len;
        } else 0;

        const tag_capture_inst: Zir.Inst.Index = if (extra.data.bits.any_has_tag_capture) blk: {
            const tag_capture_inst = self.code.extra[extra_index];
            extra_index += 1;
            break :blk @enumFromInt(tag_capture_inst);
        } else undefined;

        try self.writeInstRef(stream, extra.data.operand);

        if (extra.data.bits.any_has_tag_capture) {
            try stream.writeAll(", tag_capture=");
            try self.writeInstIndex(stream, tag_capture_inst);
        }

        self.indent += 2;

        else_prong: {
            const special_prong = extra.data.bits.specialProng();
            const prong_name = switch (special_prong) {
                .@"else" => "else",
                .under => "_",
                else => break :else_prong,
            };

            const info = @as(Zir.Inst.SwitchBlock.ProngInfo, @bitCast(self.code.extra[extra_index]));
            const capture_text = switch (info.capture) {
                .none => "",
                .by_val => "by_val ",
                .by_ref => "by_ref ",
            };
            const inline_text = if (info.is_inline) "inline " else "";
            extra_index += 1;
            const body = self.code.bodySlice(extra_index, info.body_len);
            extra_index += body.len;

            try stream.writeAll(",\n");
            try stream.writeByteNTimes(' ', self.indent);
            try stream.print("{s}{s}{s} => ", .{ capture_text, inline_text, prong_name });
            try self.writeBracedBody(stream, body);
        }

        {
            const scalar_cases_len = extra.data.bits.scalar_cases_len;
            var scalar_i: usize = 0;
            while (scalar_i < scalar_cases_len) : (scalar_i += 1) {
                const item_ref = @as(Zir.Inst.Ref, @enumFromInt(self.code.extra[extra_index]));
                extra_index += 1;
                const info = @as(Zir.Inst.SwitchBlock.ProngInfo, @bitCast(self.code.extra[extra_index]));
                extra_index += 1;
                const body = self.code.bodySlice(extra_index, info.body_len);
                extra_index += info.body_len;

                try stream.writeAll(",\n");
                try stream.writeByteNTimes(' ', self.indent);
                switch (info.capture) {
                    .none => {},
                    .by_val => try stream.writeAll("by_val "),
                    .by_ref => try stream.writeAll("by_ref "),
                }
                if (info.is_inline) try stream.writeAll("inline ");
                try self.writeInstRef(stream, item_ref);
                try stream.writeAll(" => ");
                try self.writeBracedBody(stream, body);
            }
        }
        {
            var multi_i: usize = 0;
            while (multi_i < multi_cases_len) : (multi_i += 1) {
                const items_len = self.code.extra[extra_index];
                extra_index += 1;
                const ranges_len = self.code.extra[extra_index];
                extra_index += 1;
                const info = @as(Zir.Inst.SwitchBlock.ProngInfo, @bitCast(self.code.extra[extra_index]));
                extra_index += 1;
                const items = self.code.refSlice(extra_index, items_len);
                extra_index += items_len;

                try stream.writeAll(",\n");
                try stream.writeByteNTimes(' ', self.indent);
                switch (info.capture) {
                    .none => {},
                    .by_val => try stream.writeAll("by_val "),
                    .by_ref => try stream.writeAll("by_ref "),
                }
                if (info.is_inline) try stream.writeAll("inline ");

                for (items, 0..) |item_ref, item_i| {
                    if (item_i != 0) try stream.writeAll(", ");
                    try self.writeInstRef(stream, item_ref);
                }

                var range_i: usize = 0;
                while (range_i < ranges_len) : (range_i += 1) {
                    const item_first = @as(Zir.Inst.Ref, @enumFromInt(self.code.extra[extra_index]));
                    extra_index += 1;
                    const item_last = @as(Zir.Inst.Ref, @enumFromInt(self.code.extra[extra_index]));
                    extra_index += 1;

                    if (range_i != 0 or items.len != 0) {
                        try stream.writeAll(", ");
                    }
                    try self.writeInstRef(stream, item_first);
                    try stream.writeAll("...");
                    try self.writeInstRef(stream, item_last);
                }

                const body = self.code.bodySlice(extra_index, info.body_len);
                extra_index += info.body_len;
                try stream.writeAll(" => ");
                try self.writeBracedBody(stream, body);
            }
        }

        self.indent -= 2;

        try stream.writeAll(") ");
        try self.writeSrcNode(stream, inst_data.src_node);
    }

    fn writePlNodeField(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].pl_node;
        const extra = self.code.extraData(Zir.Inst.Field, inst_data.payload_index).data;
        const name = self.code.nullTerminatedString(extra.field_name_start);
        try self.writeInstRef(stream, extra.lhs);
        try stream.print(", \"{}\") ", .{std.zig.fmtEscapes(name)});
        try self.writeSrcNode(stream, inst_data.src_node);
    }

    fn writePlNodeFieldNamed(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].pl_node;
        const extra = self.code.extraData(Zir.Inst.FieldNamed, inst_data.payload_index).data;
        try self.writeInstRef(stream, extra.lhs);
        try stream.writeAll(", ");
        try self.writeInstRef(stream, extra.field_name);
        try stream.writeAll(") ");
        try self.writeSrcNode(stream, inst_data.src_node);
    }

    fn writeAs(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].pl_node;
        const extra = self.code.extraData(Zir.Inst.As, inst_data.payload_index).data;
        try self.writeInstRef(stream, extra.dest_type);
        try stream.writeAll(", ");
        try self.writeInstRef(stream, extra.operand);
        try stream.writeAll(") ");
        try self.writeSrcNode(stream, inst_data.src_node);
    }

    fn writeNode(
        self: *Writer,
        stream: anytype,
        inst: Zir.Inst.Index,
    ) (@TypeOf(stream).Error || error{OutOfMemory})!void {
        const src_node = self.code.instructions.items(.data)[@intFromEnum(inst)].node;
        try stream.writeAll(") ");
        try self.writeSrcNode(stream, src_node);
    }

    fn writeStrNode(
        self: *Writer,
        stream: anytype,
        inst: Zir.Inst.Index,
    ) (@TypeOf(stream).Error || error{OutOfMemory})!void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].str_node;
        const str = inst_data.get(self.code);
        try stream.print("\"{}\") ", .{std.zig.fmtEscapes(str)});
        try self.writeSrcNode(stream, inst_data.src_node);
    }

    fn writeStrTok(
        self: *Writer,
        stream: anytype,
        inst: Zir.Inst.Index,
    ) (@TypeOf(stream).Error || error{OutOfMemory})!void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].str_tok;
        const str = inst_data.get(self.code);
        try stream.print("\"{}\") ", .{std.zig.fmtEscapes(str)});
        try self.writeSrcTok(stream, inst_data.src_tok);
    }

    fn writeStrOp(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].str_op;
        const str = inst_data.getStr(self.code);
        try self.writeInstRef(stream, inst_data.operand);
        try stream.print(", \"{}\")", .{std.zig.fmtEscapes(str)});
    }

    fn writeLambda(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const zir_datas: []Zir.Inst.Data = self.code.instructions.items(.data);
        const inst_data = zir_datas[@intFromEnum(inst)].lambda;

        const prev_parent_decl_node = self.parent_decl_node;
        defer self.parent_decl_node = prev_parent_decl_node;
        self.parent_decl_node = inst_data.src_node;

        const extra = self.code.extraData(Zir.Inst.Lambda, inst_data.payload_index);

        var extra_index = extra.end;

        const body = self.code.bodySlice(extra_index, extra.data.body_len);
        extra_index += body.len;

        const src_locs = self.code.extraData(Zir.Inst.Lambda.SrcLocs, extra_index).data;

        try self.writeBracedBody(stream, body);
        try stream.writeAll(") ");
        if (body.len != 0) {
            try stream.print("(lbrace={d}:{d},rbrace={d}:{d}) ", .{
                src_locs.lbrace_line + 1, @as(u16, @truncate(src_locs.columns)) + 1,
                src_locs.rbrace_line + 1, @as(u16, @truncate(src_locs.columns >> 16)) + 1,
            });
        }
        try self.writeSrcNode(stream, 0);
    }

    fn writeFunc(
        self: *Writer,
        stream: anytype,
        inst: Zir.Inst.Index,
        inferred_error_set: bool,
    ) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].pl_node;
        const extra = self.code.extraData(Zir.Inst.Func, inst_data.payload_index);

        var extra_index = extra.end;
        var ret_ty_ref: Zir.Inst.Ref = .none;
        var ret_ty_body: []const Zir.Inst.Index = &.{};

        switch (extra.data.ret_body_len) {
            0 => {
                ret_ty_ref = .void_type;
            },
            1 => {
                ret_ty_ref = @as(Zir.Inst.Ref, @enumFromInt(self.code.extra[extra_index]));
                extra_index += 1;
            },
            else => {
                ret_ty_body = self.code.bodySlice(extra_index, extra.data.ret_body_len);
                extra_index += ret_ty_body.len;
            },
        }

        const body = self.code.bodySlice(extra_index, extra.data.body_len);
        extra_index += body.len;

        var src_locs: Zir.Inst.Func.SrcLocs = undefined;
        if (body.len != 0) {
            src_locs = self.code.extraData(Zir.Inst.Func.SrcLocs, extra_index).data;
        }
        return self.writeFuncCommon(
            stream,
            inferred_error_set,
            false,
            false,
            false,

            .none,
            &.{},
            .none,
            &.{},
            .none,
            &.{},
            .none,
            &.{},
            ret_ty_ref,
            ret_ty_body,

            body,
            inst_data.src_node,
            src_locs,
            0,
        );
    }

    fn writeFuncFancy(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].pl_node;
        const extra = self.code.extraData(Zir.Inst.FuncFancy, inst_data.payload_index);

        var extra_index: usize = extra.end;
        var align_ref: Zir.Inst.Ref = .none;
        var align_body: []const Zir.Inst.Index = &.{};
        var addrspace_ref: Zir.Inst.Ref = .none;
        var addrspace_body: []const Zir.Inst.Index = &.{};
        var section_ref: Zir.Inst.Ref = .none;
        var section_body: []const Zir.Inst.Index = &.{};
        var cc_ref: Zir.Inst.Ref = .none;
        var cc_body: []const Zir.Inst.Index = &.{};
        var ret_ty_ref: Zir.Inst.Ref = .none;
        var ret_ty_body: []const Zir.Inst.Index = &.{};

        if (extra.data.bits.has_lib_name) {
            const lib_name = self.code.nullTerminatedString(@enumFromInt(self.code.extra[extra_index]));
            extra_index += 1;
            try stream.print("lib_name=\"{}\", ", .{std.zig.fmtEscapes(lib_name)});
        }
        try self.writeFlag(stream, "test, ", extra.data.bits.is_test);

        if (extra.data.bits.has_align_body) {
            const body_len = self.code.extra[extra_index];
            extra_index += 1;
            align_body = self.code.bodySlice(extra_index, body_len);
            extra_index += align_body.len;
        } else if (extra.data.bits.has_align_ref) {
            align_ref = @as(Zir.Inst.Ref, @enumFromInt(self.code.extra[extra_index]));
            extra_index += 1;
        }
        if (extra.data.bits.has_addrspace_body) {
            const body_len = self.code.extra[extra_index];
            extra_index += 1;
            addrspace_body = self.code.bodySlice(extra_index, body_len);
            extra_index += addrspace_body.len;
        } else if (extra.data.bits.has_addrspace_ref) {
            addrspace_ref = @as(Zir.Inst.Ref, @enumFromInt(self.code.extra[extra_index]));
            extra_index += 1;
        }
        if (extra.data.bits.has_section_body) {
            const body_len = self.code.extra[extra_index];
            extra_index += 1;
            section_body = self.code.bodySlice(extra_index, body_len);
            extra_index += section_body.len;
        } else if (extra.data.bits.has_section_ref) {
            section_ref = @as(Zir.Inst.Ref, @enumFromInt(self.code.extra[extra_index]));
            extra_index += 1;
        }
        if (extra.data.bits.has_cc_body) {
            const body_len = self.code.extra[extra_index];
            extra_index += 1;
            cc_body = self.code.bodySlice(extra_index, body_len);
            extra_index += cc_body.len;
        } else if (extra.data.bits.has_cc_ref) {
            cc_ref = @as(Zir.Inst.Ref, @enumFromInt(self.code.extra[extra_index]));
            extra_index += 1;
        }
        if (extra.data.bits.has_ret_ty_body) {
            const body_len = self.code.extra[extra_index];
            extra_index += 1;
            ret_ty_body = self.code.bodySlice(extra_index, body_len);
            extra_index += ret_ty_body.len;
        } else if (extra.data.bits.has_ret_ty_ref) {
            ret_ty_ref = @as(Zir.Inst.Ref, @enumFromInt(self.code.extra[extra_index]));
            extra_index += 1;
        }

        const noalias_bits: u32 = if (extra.data.bits.has_any_noalias) blk: {
            const x = self.code.extra[extra_index];
            extra_index += 1;
            break :blk x;
        } else 0;

        const body = self.code.bodySlice(extra_index, extra.data.body_len);
        extra_index += body.len;

        var src_locs: Zir.Inst.Func.SrcLocs = undefined;
        if (body.len != 0) {
            src_locs = self.code.extraData(Zir.Inst.Func.SrcLocs, extra_index).data;
        }
        return self.writeFuncCommon(
            stream,
            extra.data.bits.is_inferred_error,
            extra.data.bits.is_var_args,
            extra.data.bits.is_extern,
            extra.data.bits.is_noinline,
            align_ref,
            align_body,
            addrspace_ref,
            addrspace_body,
            section_ref,
            section_body,
            cc_ref,
            cc_body,
            ret_ty_ref,
            ret_ty_body,
            body,
            inst_data.src_node,
            src_locs,
            noalias_bits,
        );
    }

    fn writeVarExtended(self: *Writer, stream: anytype, extended: Zir.Inst.Extended.InstData) !void {
        const extra = self.code.extraData(Zir.Inst.ExtendedVar, extended.operand);
        const small = @as(Zir.Inst.ExtendedVar.Small, @bitCast(extended.small));

        try self.writeInstRef(stream, extra.data.var_type);

        var extra_index: usize = extra.end;
        if (small.has_lib_name) {
            const lib_name_index: Zir.NullTerminatedString = @enumFromInt(self.code.extra[extra_index]);
            const lib_name = self.code.nullTerminatedString(lib_name_index);
            extra_index += 1;
            try stream.print(", lib_name=\"{}\"", .{std.zig.fmtEscapes(lib_name)});
        }
        const align_inst: Zir.Inst.Ref = if (!small.has_align) .none else blk: {
            const align_inst = @as(Zir.Inst.Ref, @enumFromInt(self.code.extra[extra_index]));
            extra_index += 1;
            break :blk align_inst;
        };
        const init_inst: Zir.Inst.Ref = if (!small.has_init) .none else blk: {
            const init_inst = @as(Zir.Inst.Ref, @enumFromInt(self.code.extra[extra_index]));
            extra_index += 1;
            break :blk init_inst;
        };
        try self.writeFlag(stream, ", is_extern", small.is_extern);
        try self.writeFlag(stream, ", is_threadlocal", small.is_threadlocal);
        try self.writeOptionalInstRef(stream, ", align=", align_inst);
        try self.writeOptionalInstRef(stream, ", init=", init_inst);
        try stream.writeAll("))");
    }

    fn writeAllocExtended(self: *Writer, stream: anytype, extended: Zir.Inst.Extended.InstData) !void {
        const extra = self.code.extraData(Zir.Inst.AllocExtended, extended.operand);
        const small = @as(Zir.Inst.AllocExtended.Small, @bitCast(extended.small));

        var extra_index: usize = extra.end;
        const type_inst: Zir.Inst.Ref = if (!small.has_type) .none else blk: {
            const type_inst = @as(Zir.Inst.Ref, @enumFromInt(self.code.extra[extra_index]));
            extra_index += 1;
            break :blk type_inst;
        };
        const align_inst: Zir.Inst.Ref = if (!small.has_align) .none else blk: {
            const align_inst = @as(Zir.Inst.Ref, @enumFromInt(self.code.extra[extra_index]));
            extra_index += 1;
            break :blk align_inst;
        };
        try self.writeFlag(stream, ",is_const", small.is_const);
        try self.writeFlag(stream, ",is_comptime", small.is_comptime);
        try self.writeOptionalInstRef(stream, ",ty=", type_inst);
        try self.writeOptionalInstRef(stream, ",align=", align_inst);
        try stream.writeAll(")) ");
        try self.writeSrcNode(stream, extra.data.src_node);
    }

    fn writeTypeofPeer(self: *Writer, stream: anytype, extended: Zir.Inst.Extended.InstData) !void {
        const extra = self.code.extraData(Zir.Inst.TypeOfPeer, extended.operand);
        const body = self.code.bodySlice(extra.data.body_index, extra.data.body_len);
        try self.writeBracedBody(stream, body);
        try stream.writeAll(",[");
        const args = self.code.refSlice(extra.end, extended.small);
        for (args, 0..) |arg, i| {
            if (i != 0) try stream.writeAll(", ");
            try self.writeInstRef(stream, arg);
        }
        try stream.writeAll("])");
    }

    fn writeBoolBr(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].pl_node;
        const extra = self.code.extraData(Zir.Inst.BoolBr, inst_data.payload_index);
        const body = self.code.bodySlice(extra.end, extra.data.body_len);
        try self.writeInstRef(stream, extra.data.lhs);
        try stream.writeAll(", ");
        try self.writeBracedBody(stream, body);
        try stream.writeAll(") ");
        try self.writeSrcNode(stream, inst_data.src_node);
    }

    fn writeIntType(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const int_type = self.code.instructions.items(.data)[@intFromEnum(inst)].int_type;
        const prefix: u8 = switch (int_type.signedness) {
            .signed => 'i',
            .unsigned => 'u',
        };
        try stream.print("{c}{d}) ", .{ prefix, int_type.bit_count });
        try self.writeSrcNode(stream, int_type.src_node);
    }

    fn writeSaveErrRetIndex(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].save_err_ret_index;

        try self.writeInstRef(stream, inst_data.operand);

        try stream.writeAll(")");
    }

    fn writeRestoreErrRetIndex(self: *Writer, stream: anytype, extended: Zir.Inst.Extended.InstData) !void {
        const extra = self.code.extraData(Zir.Inst.RestoreErrRetIndex, extended.operand).data;

        try self.writeInstRef(stream, extra.block);
        try self.writeInstRef(stream, extra.operand);

        try stream.writeAll(") ");
        try self.writeSrcNode(stream, extra.src_node);
    }

    fn writeBreak(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].@"break";
        const extra = self.code.extraData(Zir.Inst.Break, inst_data.payload_index).data;

        try self.writeInstIndex(stream, extra.block_inst);
        try stream.writeAll(", ");
        try self.writeInstRef(stream, inst_data.operand);
        try stream.writeAll(")");
    }

    fn writeArrayInit(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].pl_node;

        const extra = self.code.extraData(Zir.Inst.MultiOp, inst_data.payload_index);
        const args = self.code.refSlice(extra.end, extra.data.operands_len);

        try self.writeInstRef(stream, args[0]);
        try stream.writeAll("{");
        for (args[1..], 0..) |arg, i| {
            if (i != 0) try stream.writeAll(", ");
            try self.writeInstRef(stream, arg);
        }
        try stream.writeAll("}) ");
        try self.writeSrcNode(stream, inst_data.src_node);
    }

    fn writeArrayInitAnon(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].pl_node;

        const extra = self.code.extraData(Zir.Inst.MultiOp, inst_data.payload_index);
        const args = self.code.refSlice(extra.end, extra.data.operands_len);

        try stream.writeAll("{");
        for (args, 0..) |arg, i| {
            if (i != 0) try stream.writeAll(", ");
            try self.writeInstRef(stream, arg);
        }
        try stream.writeAll("}) ");
        try self.writeSrcNode(stream, inst_data.src_node);
    }

    fn writeArrayInitSent(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].pl_node;

        const extra = self.code.extraData(Zir.Inst.MultiOp, inst_data.payload_index);
        const args = self.code.refSlice(extra.end, extra.data.operands_len);
        const sent = args[args.len - 1];
        const elems = args[0 .. args.len - 1];

        try self.writeInstRef(stream, sent);
        try stream.writeAll(", ");

        try stream.writeAll(".{");
        for (elems, 0..) |elem, i| {
            if (i != 0) try stream.writeAll(", ");
            try self.writeInstRef(stream, elem);
        }
        try stream.writeAll("}) ");
        try self.writeSrcNode(stream, inst_data.src_node);
    }

    fn writeUnreachable(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].@"unreachable";
        try stream.writeAll(") ");
        try self.writeSrcNode(stream, inst_data.src_node);
    }

    fn writeFuncCommon(
        self: *Writer,
        stream: anytype,
        inferred_error_set: bool,
        var_args: bool,
        is_extern: bool,
        is_noinline: bool,
        align_ref: Zir.Inst.Ref,
        align_body: []const Zir.Inst.Index,
        addrspace_ref: Zir.Inst.Ref,
        addrspace_body: []const Zir.Inst.Index,
        section_ref: Zir.Inst.Ref,
        section_body: []const Zir.Inst.Index,
        cc_ref: Zir.Inst.Ref,
        cc_body: []const Zir.Inst.Index,
        ret_ty_ref: Zir.Inst.Ref,
        ret_ty_body: []const Zir.Inst.Index,
        body: []const Zir.Inst.Index,
        src_node: i32,
        src_locs: Zir.Inst.Func.SrcLocs,
        noalias_bits: u32,
    ) !void {
        try self.writeOptionalInstRefOrBody(stream, "align=", align_ref, align_body);
        try self.writeOptionalInstRefOrBody(stream, "addrspace=", addrspace_ref, addrspace_body);
        try self.writeOptionalInstRefOrBody(stream, "section=", section_ref, section_body);
        try self.writeOptionalInstRefOrBody(stream, "cc=", cc_ref, cc_body);
        try self.writeOptionalInstRefOrBody(stream, "ret_ty=", ret_ty_ref, ret_ty_body);
        try self.writeFlag(stream, "vargs, ", var_args);
        try self.writeFlag(stream, "extern, ", is_extern);
        try self.writeFlag(stream, "inferror, ", inferred_error_set);
        try self.writeFlag(stream, "noinline, ", is_noinline);

        if (noalias_bits != 0) {
            try stream.print("noalias=0b{b}, ", .{noalias_bits});
        }

        try stream.writeAll("body=");
        try self.writeBracedBody(stream, body);
        try stream.writeAll(") ");
        if (body.len != 0) {
            try stream.print("(lbrace={d}:{d},rbrace={d}:{d}) ", .{
                src_locs.lbrace_line + 1, @as(u16, @truncate(src_locs.columns)) + 1,
                src_locs.rbrace_line + 1, @as(u16, @truncate(src_locs.columns >> 16)) + 1,
            });
        }
        try self.writeSrcNode(stream, src_node);
    }

    fn writeDbgStmt(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].dbg_stmt;
        try stream.print("{d}, {d})", .{ inst_data.line + 1, inst_data.column + 1 });
    }

    fn writeDefer(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].@"defer";
        const body = self.code.bodySlice(inst_data.index, inst_data.len);
        try self.writeBracedBody(stream, body);
        try stream.writeByte(')');
    }

    fn writeDeferErrCode(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].defer_err_code;
        const extra = self.code.extraData(Zir.Inst.DeferErrCode, inst_data.payload_index).data;

        try self.writeInstRef(stream, extra.remapped_err_code.toRef());
        try stream.writeAll(" = ");
        try self.writeInstRef(stream, inst_data.err_code);
        try stream.writeAll(", ");
        const body = self.code.bodySlice(extra.index, extra.len);
        try self.writeBracedBody(stream, body);
        try stream.writeByte(')');
    }

    fn writeDeclaration(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        const inst_data = self.code.instructions.items(.data)[@intFromEnum(inst)].declaration;
        const extra = self.code.extraData(Zir.Inst.Declaration, inst_data.payload_index);
        const doc_comment: ?Zir.NullTerminatedString = if (extra.data.flags.has_doc_comment) dc: {
            break :dc @enumFromInt(self.code.extra[extra.end]);
        } else null;

        const prev_parent_decl_node = self.parent_decl_node;
        defer self.parent_decl_node = prev_parent_decl_node;
        self.parent_decl_node = inst_data.src_node;

        if (extra.data.flags.is_pub) try stream.writeAll("pub ");
        if (extra.data.flags.is_export) try stream.writeAll("export ");
        switch (extra.data.name) {
            .@"comptime" => try stream.writeAll("comptime"),
            .@"usingnamespace" => try stream.writeAll("usingnamespace"),
            .unnamed_test => try stream.writeAll("test"),
            .decltest => try stream.print("decltest '{s}'", .{self.code.nullTerminatedString(doc_comment.?)}),
            _ => {
                const name = extra.data.name.toString(self.code).?;
                const prefix = if (extra.data.name.isNamedTest(self.code)) "test " else "";
                try stream.print("{s}'{s}'", .{ prefix, self.code.nullTerminatedString(name) });
            },
        }
        const src_hash_arr: [4]u32 = .{
            extra.data.src_hash_0,
            extra.data.src_hash_1,
            extra.data.src_hash_2,
            extra.data.src_hash_3,
        };
        const src_hash_bytes: [16]u8 = @bitCast(src_hash_arr);
        try stream.print(" line({d}) hash({})", .{ extra.data.src_line, std.fmt.fmtSliceHexLower(&src_hash_bytes) });

        {
            const bodies = extra.data.getBodies(@intCast(extra.end), self.code);

            try stream.writeAll(" value=");
            try self.writeBracedDecl(stream, bodies.value_body);

            if (bodies.align_body) |b| {
                try stream.writeAll(" align=");
                try self.writeBracedDecl(stream, b);
            }

            if (bodies.linksection_body) |b| {
                try stream.writeAll(" linksection=");
                try self.writeBracedDecl(stream, b);
            }

            if (bodies.addrspace_body) |b| {
                try stream.writeAll(" addrspace=");
                try self.writeBracedDecl(stream, b);
            }
        }

        try stream.writeAll(") ");
        try self.writeSrcNode(stream, 0);
    }

    fn writeClosureGet(self: *Writer, stream: anytype, extended: Zir.Inst.Extended.InstData) !void {
        try stream.print("{d})) ", .{extended.small});
        try self.writeSrcNode(stream, @bitCast(extended.operand));
    }

    fn writeBuiltinValue(self: *Writer, stream: anytype, extended: Zir.Inst.Extended.InstData) !void {
        const val: Zir.Inst.BuiltinValue = @enumFromInt(extended.small);
        try stream.print("{s})) ", .{@tagName(val)});
        try self.writeSrcNode(stream, @bitCast(extended.operand));
    }

    fn writeInplaceArithResultTy(self: *Writer, stream: anytype, extended: Zir.Inst.Extended.InstData) !void {
        const op: Zir.Inst.InplaceOp = @enumFromInt(extended.small);
        try self.writeInstRef(stream, @enumFromInt(extended.operand));
        try stream.print(", {s}))", .{@tagName(op)});
    }

    fn writeInstRef(self: *Writer, stream: anytype, ref: Zir.Inst.Ref) !void {
        if (ref == .none) {
            return stream.writeAll(".none");
        } else if (ref.toIndex()) |i| {
            return self.writeInstIndex(stream, i);
        } else {
            const val: InternPool.Index = @enumFromInt(@intFromEnum(ref));
            return stream.print("@{s}", .{@tagName(val)});
        }
    }

    fn writeInstIndex(self: *Writer, stream: anytype, inst: Zir.Inst.Index) !void {
        _ = self;
        return stream.print("%{d}", .{@intFromEnum(inst)});
    }

    fn writeCapture(self: *Writer, stream: anytype, capture: Zir.Inst.Capture) !void {
        switch (capture.unwrap()) {
            .nested => |i| return stream.print("[{d}]", .{i}),
            .instruction => |inst| return self.writeInstIndex(stream, inst),
            .instruction_load => |ptr_inst| {
                try stream.writeAll("load ");
                try self.writeInstIndex(stream, ptr_inst);
            },
            .decl_val => |str| try stream.print("decl_val \"{}\"", .{
                std.zig.fmtEscapes(self.code.nullTerminatedString(str)),
            }),
            .decl_ref => |str| try stream.print("decl_ref \"{}\"", .{
                std.zig.fmtEscapes(self.code.nullTerminatedString(str)),
            }),
        }
    }

    fn writeOptionalInstRef(self: *Writer, stream: anytype, prefix: []const u8, inst: Zir.Inst.Ref) !void {
        if (inst == .none) return;
        try stream.writeAll(prefix);
        try self.writeInstRef(stream, inst);
    }

    fn writeOptionalInstRefOrBody(
        self: *Writer,
        stream: anytype,
        prefix: []const u8,
        ref: Zir.Inst.Ref,
        body: []const Zir.Inst.Index,
    ) !void {
        if (body.len != 0) {
            try stream.writeAll(prefix);
            try self.writeBracedBody(stream, body);
            try stream.writeAll(", ");
        } else if (ref != .none) {
            try stream.writeAll(prefix);
            try self.writeInstRef(stream, ref);
            try stream.writeAll(", ");
        }
    }

    fn writeOptionalBody(self: *Writer, stream: anytype, prefix: []const u8, body: []const Zir.Inst.Index) !void {
        if (body.len != 0) {
            try stream.writeAll(prefix);
            try self.writeBracedBody(stream, body);
            try stream.writeAll(", ");
        }
    }

    fn writeFlag(
        self: *Writer,
        stream: anytype,
        name: []const u8,
        flag: bool,
    ) !void {
        _ = self;
        if (!flag) return;
        try stream.writeAll(name);
    }

    fn writeSrcNode(self: *Writer, stream: anytype, src_node: i32) !void {
        if (!self.file.tree_loaded) return;
        const tree = self.file.tree;
        const abs_node = self.relativeToNodeIndex(src_node);
        const src_span = tree.nodeToSpan(abs_node);
        const start = self.line_col_cursor.find(tree.source, src_span.start);
        const end = self.line_col_cursor.find(tree.source, src_span.end);
        try stream.print("node_offset:{d}:{d} to :{d}:{d}", .{
            start.line + 1, start.column + 1,
            end.line + 1,   end.column + 1,
        });
    }

    fn writeSrcTok(self: *Writer, stream: anytype, src_tok: u32) !void {
        if (!self.file.tree_loaded) return;
        const tree = self.file.tree;
        const abs_tok = tree.firstToken(self.parent_decl_node) + src_tok;
        const span_loc = tree.tokens.items(.loc)[abs_tok];
        const start = self.line_col_cursor.find(tree.source, span_loc.start);
        const end = self.line_col_cursor.find(tree.source, span_loc.end);
        try stream.print("token_offset:{d}:{d} to :{d}:{d}", .{
            start.line + 1, start.column + 1,
            end.line + 1,   end.column + 1,
        });
    }

    fn writeSrcTokAbs(self: *Writer, stream: anytype, src_tok: u32) !void {
        if (!self.file.tree_loaded) return;
        const tree = self.file.tree;
        const span_loc = tree.tokens.items(.loc)[src_tok];
        const start = self.line_col_cursor.find(tree.source, span_loc.start);
        const end = self.line_col_cursor.find(tree.source, span_loc.end);
        try stream.print("token_abs:{d}:{d} to :{d}:{d}", .{
            start.line + 1, start.column + 1,
            end.line + 1,   end.column + 1,
        });
    }

    fn writeBracedDecl(self: *Writer, stream: anytype, body: []const Zir.Inst.Index) !void {
        try self.writeBracedBodyConditional(stream, body, self.recurse_decls);
    }

    fn writeBracedBody(self: *Writer, stream: anytype, body: []const Zir.Inst.Index) !void {
        try self.writeBracedBodyConditional(stream, body, self.recurse_blocks);
    }

    fn writeBracedBodyConditional(self: *Writer, stream: anytype, body: []const Zir.Inst.Index, enabled: bool) !void {
        if (body.len == 0) {
            try stream.writeAll("{}");
        } else if (enabled) {
            try stream.writeAll("{\n");
            self.indent += 2;
            try self.writeBody(stream, body);
            self.indent -= 2;
            try stream.writeByteNTimes(' ', self.indent);
            try stream.writeAll("}");
        } else if (body.len == 1) {
            try stream.writeByte('{');
            try self.writeInstIndex(stream, body[0]);
            try stream.writeByte('}');
        } else if (body.len == 2) {
            try stream.writeByte('{');
            try self.writeInstIndex(stream, body[0]);
            try stream.writeAll(", ");
            try self.writeInstIndex(stream, body[1]);
            try stream.writeByte('}');
        } else {
            try stream.writeByte('{');
            try self.writeInstIndex(stream, body[0]);
            try stream.writeAll("..");
            try self.writeInstIndex(stream, body[body.len - 1]);
            try stream.writeByte('}');
        }
    }

    fn writeDocComment(self: *Writer, stream: anytype, doc_comment_index: Zir.NullTerminatedString) !void {
        if (doc_comment_index != .empty) {
            const doc_comment = self.code.nullTerminatedString(doc_comment_index);
            var it = std.mem.tokenizeScalar(u8, doc_comment, '\n');
            while (it.next()) |doc_line| {
                try stream.writeByteNTimes(' ', self.indent);
                try stream.print("///{s}\n", .{doc_line});
            }
        }
    }

    fn writeBody(self: *Writer, stream: anytype, body: []const Zir.Inst.Index) !void {
        for (body) |inst| {
            try stream.writeByteNTimes(' ', self.indent);
            try stream.print("%{d} ", .{@intFromEnum(inst)});
            try self.writeInstToStream(stream, inst);
            try stream.writeByte('\n');
        }
    }
};

fn testZir(source: [:0]const u8, expected: []const u8) !void {
    try noFailZir(source);
    inline for (@typeInfo(Ast.Mode).@"enum".fields) |field| {
        try testZirMode(@enumFromInt(field.value), source, expected);
    }
}

fn testZirMode(mode: Ast.Mode, source: [:0]const u8, expected: []const u8) !void {
    const gpa = std.testing.allocator;

    var tree = try Ast.parse(gpa, source, .{
        .mode = mode,
        .version = .@"4.0",
    });
    defer tree.deinit(gpa);

    var zir = try AstGen.generate(gpa, tree);
    defer zir.deinit(gpa);

    // Errors and TODO: warnings
    if (zir.hasCompileErrors()) {
        std.debug.print("error\n", .{});
        try kdb.printZirErrorsToStderr(gpa, tree, zir, "test", .auto);
        return error.Unexpected;
    }

    var file: kdb.File = .{
        .source_loaded = true,
        .tree_loaded = true,
        .zir_loaded = true,
        .sub_file_path = "test",
        .source = source,
        .tree = tree,
        .zir = zir,
    };
    var output = std.ArrayList(u8).init(gpa);
    defer output.deinit();
    try renderAsText(gpa, &file, &output);

    try std.testing.expectEqualStrings(expected, output.items[0 .. output.items.len - 1]);
    try std.testing.expectEqual('\n', output.getLast());
}

fn warnZir(source: [:0]const u8, expected: []const u8) !void {
    try noFailZir(source);
    inline for (@typeInfo(Ast.Mode).@"enum".fields) |field| {
        try warnZirMode(@enumFromInt(field.value), source, expected);
    }
}

fn warnZirMode(mode: Ast.Mode, source: [:0]const u8, expected: []const u8) !void {
    const gpa = std.testing.allocator;

    var tree = try Ast.parse(gpa, source, .{
        .mode = mode,
        .version = .@"4.0",
    });
    defer tree.deinit(gpa);

    var zir = try AstGen.generate(gpa, tree);
    defer zir.deinit(gpa);

    // Errors
    if (zir.hasCompileErrors()) {
        std.debug.print("error\n", .{});
        try kdb.printZirErrorsToStderr(gpa, tree, zir, "test", .auto);
        return error.Unexpected;
    }

    try std.testing.expect(zir.hasCompileWarnings());

    var wip_errors: kdb.ErrorBundle.Wip = undefined;
    try wip_errors.init(gpa);
    defer wip_errors.deinit();

    try wip_errors.addZirErrorMessages(zir, tree, source, "test");

    var error_bundle = try wip_errors.toOwnedBundle("");
    defer error_bundle.deinit(gpa);

    var output = std.ArrayList(u8).init(gpa);
    defer output.deinit();
    try error_bundle.renderToWriter(.{ .ttyconf = .no_color }, output.writer());

    var file: kdb.File = .{
        .source_loaded = true,
        .tree_loaded = true,
        .zir_loaded = true,
        .sub_file_path = "test",
        .source = source,
        .tree = tree,
        .zir = zir,
    };
    try renderAsText(gpa, &file, &output);

    try std.testing.expectEqualStrings(expected, output.items[0 .. output.items.len - 1]);
    try std.testing.expectEqual('\n', output.getLast());
}

fn failZir(source: [:0]const u8, expected: []const u8) !void {
    try noFailZir(source);
    inline for (@typeInfo(Ast.Mode).@"enum".fields) |field| {
        try failZirMode(@enumFromInt(field.value), source, expected);
    }
}

fn failZirMode(mode: Ast.Mode, source: [:0]const u8, expected: []const u8) !void {
    const gpa = std.testing.allocator;

    var tree = try Ast.parse(gpa, source, .{
        .mode = mode,
        .version = .@"4.0",
    });
    defer tree.deinit(gpa);

    var zir = try AstGen.generate(gpa, tree);
    defer zir.deinit(gpa);

    try std.testing.expect(zir.hasCompileErrors());

    var wip_errors: kdb.ErrorBundle.Wip = undefined;
    try wip_errors.init(gpa);
    defer wip_errors.deinit();

    try wip_errors.addZirErrorMessages(zir, tree, source, "test");

    var error_bundle = try wip_errors.toOwnedBundle("");
    defer error_bundle.deinit(gpa);

    var output = std.ArrayList(u8).init(gpa);
    defer output.deinit();
    try error_bundle.renderToWriter(.{ .ttyconf = .no_color }, output.writer());

    try std.testing.expectEqualStrings(expected, output.items[0 .. output.items.len - 1]);
    try std.testing.expectEqual('\n', output.getLast());
}

fn noFailZir(source: [:0]const u8) !void {
    inline for (@typeInfo(Ast.Mode).@"enum".fields) |mode_field| {
        inline for (@typeInfo(Ast.Version).@"enum".fields) |version_field| {
            try noFailZirModeVersion(
                @enumFromInt(mode_field.value),
                @enumFromInt(version_field.value),
                source,
            );
        }
    }
}

fn noFailZirModeVersion(mode: Ast.Mode, version: Ast.Version, source: [:0]const u8) !void {
    const gpa = std.testing.allocator;

    const settings: Ast.ParseSettings = .{ .mode = mode, .version = version };

    for (0..source.len) |i| {
        const src = try gpa.dupeZ(u8, source[0..i]);
        defer gpa.free(src);

        var tree = try Ast.parse(gpa, src, settings);
        defer tree.deinit(gpa);

        var zir = try AstGen.generate(gpa, tree);
        defer zir.deinit(gpa);
    }

    for (0..source.len) |i| {
        const src = try gpa.allocSentinel(u8, source.len - 1, 0);
        defer gpa.free(src);

        @memcpy(src[0..i], source[0..i]);
        @memcpy(src[i..], source[i + 1 ..]);

        var tree = try Ast.parse(gpa, src, settings);
        defer tree.deinit(gpa);

        var zir = try AstGen.generate(gpa, tree);
        defer zir.deinit(gpa);
    }
}

fn testPropertiesUpheld(source: []const u8) anyerror!void {
    const gpa = std.testing.allocator;

    const source0 = try std.testing.allocator.dupeZ(u8, source);
    defer gpa.free(source0);

    inline for (@typeInfo(Ast.Mode).@"enum".fields) |mode_field| {
        inline for (@typeInfo(Ast.Version).@"enum".fields) |version_field| {
            var tree = try Ast.parse(gpa, source0, .{
                .mode = @enumFromInt(mode_field.value),
                .version = @enumFromInt(version_field.value),
            });
            defer tree.deinit(gpa);

            var zir = try AstGen.generate(gpa, tree);
            defer zir.deinit(gpa);
        }
    }
}

test "fuzzable properties upheld" {
    return std.testing.fuzz(testPropertiesUpheld, .{
        .corpus = &.{
            "{[]x:}",
        },
    });
}

test "empty" {
    return error.SkipZigTest;
}

test "grouped expression" {
    try testZir("3*4+5",
        \\%0 = file({
        \\  %1 = long(5)
        \\  %2 = long(4)
        \\  %3 = add(%2, %1) node_offset:1:3 to :1:6
        \\  %4 = long(3)
        \\  %5 = multiply(%4, %3) node_offset:1:1 to :1:6
        \\})
    );
    try testZir("(3*4)+5",
        \\%0 = file({
        \\  %1 = long(5)
        \\  %2 = long(4)
        \\  %3 = long(3)
        \\  %4 = multiply(%3, %2) node_offset:1:2 to :1:5
        \\  %5 = add(%4, %1) node_offset:1:1 to :1:8
        \\})
    );
}

test "empty list" {
    try testZir("()", "%0 = file({})");
    try testZir("a:()",
        \\%0 = file({
        \\  %1 = identifier("a") token_offset:1:1 to :1:2
        \\  %2 = assign(%1, @empty_list) node_offset:1:1 to :1:5
        \\})
    );
    try testZir("{[]()}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = ret_node(@empty_list) node_offset:1:4 to :1:6
        \\  }) (lbrace=1:1,rbrace=1:6) node_offset:1:1 to :1:7
        \\})
    );
}

test "list" {
    return error.SkipZigTest;
}

test "table literal" {
    return error.SkipZigTest;
}

test "lambda" {
    try testZir("{}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_implicit(@x) token_offset:1:1 to :1:2
        \\    %3 = ret_implicit(@null) token_offset:1:2 to :1:3
        \\  }) (lbrace=1:1,rbrace=1:2) node_offset:1:1 to :1:3
        \\})
    );
    try testZir("{[]}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = ret_implicit(@null) token_offset:1:4 to :1:5
        \\  }) (lbrace=1:1,rbrace=1:4) node_offset:1:1 to :1:5
        \\})
    );
    try testZir("{[x]}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = ret_implicit(@null) token_offset:1:5 to :1:6
        \\  }) (lbrace=1:1,rbrace=1:5) node_offset:1:1 to :1:6
        \\})
    );
    try testZir("{[x;y]}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = param_node("y") node_offset:1:5 to :1:6
        \\    %4 = ret_implicit(@null) token_offset:1:7 to :1:8
        \\  }) (lbrace=1:1,rbrace=1:7) node_offset:1:1 to :1:8
        \\})
    );

    try testZir("{1}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_implicit(@x) token_offset:1:1 to :1:2
        \\    %3 = ret_node(@one) node_offset:1:2 to :1:3
        \\  }) (lbrace=1:1,rbrace=1:3) node_offset:1:1 to :1:4
        \\})
    );
    try testZir("{[]1}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = ret_node(@one) node_offset:1:4 to :1:5
        \\  }) (lbrace=1:1,rbrace=1:5) node_offset:1:1 to :1:6
        \\})
    );
    try testZir("{[x]1}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = ret_node(@one) node_offset:1:5 to :1:6
        \\  }) (lbrace=1:1,rbrace=1:6) node_offset:1:1 to :1:7
        \\})
    );
    try testZir("{[x;y]1}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = param_node("y") node_offset:1:5 to :1:6
        \\    %4 = ret_node(@one) node_offset:1:7 to :1:8
        \\  }) (lbrace=1:1,rbrace=1:8) node_offset:1:1 to :1:9
        \\})
    );

    try testZir("{2}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_implicit(@x) token_offset:1:1 to :1:2
        \\    %3 = long(2)
        \\    %4 = ret_node(%3) node_offset:1:2 to :1:3
        \\  }) (lbrace=1:1,rbrace=1:3) node_offset:1:1 to :1:4
        \\})
    );
    try testZir("{[]2}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = long(2)
        \\    %3 = ret_node(%2) node_offset:1:4 to :1:5
        \\  }) (lbrace=1:1,rbrace=1:5) node_offset:1:1 to :1:6
        \\})
    );
    try testZir("{[x]2}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = long(2)
        \\    %4 = ret_node(%3) node_offset:1:5 to :1:6
        \\  }) (lbrace=1:1,rbrace=1:6) node_offset:1:1 to :1:7
        \\})
    );
    try testZir("{[x;y]2}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = param_node("y") node_offset:1:5 to :1:6
        \\    %4 = long(2)
        \\    %5 = ret_node(%4) node_offset:1:7 to :1:8
        \\  }) (lbrace=1:1,rbrace=1:8) node_offset:1:1 to :1:9
        \\})
    );

    try testZir("{x}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_implicit(@x) token_offset:1:1 to :1:2
        \\    %3 = identifier("x") token_offset:1:2 to :1:3
        \\    %4 = ret_node(%3) node_offset:1:2 to :1:3
        \\  }) (lbrace=1:1,rbrace=1:3) node_offset:1:1 to :1:4
        \\})
    );
    try testZir("{[]x}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("x") token_offset:1:4 to :1:5
        \\    %3 = ret_node(%2) node_offset:1:4 to :1:5
        \\  }) (lbrace=1:1,rbrace=1:5) node_offset:1:1 to :1:6
        \\})
    );
    try testZir("{[x]x}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = ret_node(%2) node_offset:1:5 to :1:6
        \\  }) (lbrace=1:1,rbrace=1:6) node_offset:1:1 to :1:7
        \\})
    );
    try testZir("{[x;y]x}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = param_node("y") node_offset:1:5 to :1:6
        \\    %4 = ret_node(%2) node_offset:1:7 to :1:8
        \\  }) (lbrace=1:1,rbrace=1:8) node_offset:1:1 to :1:9
        \\})
    );

    try testZir("{y}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_implicit(@x) token_offset:1:1 to :1:2
        \\    %3 = param_implicit(@y) token_offset:1:1 to :1:2
        \\    %4 = identifier("y") token_offset:1:2 to :1:3
        \\    %5 = ret_node(%4) node_offset:1:2 to :1:3
        \\  }) (lbrace=1:1,rbrace=1:3) node_offset:1:1 to :1:4
        \\})
    );
    try testZir("{[]y}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("y") token_offset:1:4 to :1:5
        \\    %3 = ret_node(%2) node_offset:1:4 to :1:5
        \\  }) (lbrace=1:1,rbrace=1:5) node_offset:1:1 to :1:6
        \\})
    );
    try testZir("{[x]y}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = identifier("y") token_offset:1:5 to :1:6
        \\    %4 = ret_node(%3) node_offset:1:5 to :1:6
        \\  }) (lbrace=1:1,rbrace=1:6) node_offset:1:1 to :1:7
        \\})
    );
    try testZir("{[x;y]y}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = param_node("y") node_offset:1:5 to :1:6
        \\    %4 = ret_node(%3) node_offset:1:7 to :1:8
        \\  }) (lbrace=1:1,rbrace=1:8) node_offset:1:1 to :1:9
        \\})
    );

    try testZir("{z}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_implicit(@x) token_offset:1:1 to :1:2
        \\    %3 = param_implicit(@y) token_offset:1:1 to :1:2
        \\    %4 = param_implicit(@z) token_offset:1:1 to :1:2
        \\    %5 = identifier("z") token_offset:1:2 to :1:3
        \\    %6 = ret_node(%5) node_offset:1:2 to :1:3
        \\  }) (lbrace=1:1,rbrace=1:3) node_offset:1:1 to :1:4
        \\})
    );
    try testZir("{[]z}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("z") token_offset:1:4 to :1:5
        \\    %3 = ret_node(%2) node_offset:1:4 to :1:5
        \\  }) (lbrace=1:1,rbrace=1:5) node_offset:1:1 to :1:6
        \\})
    );
    try testZir("{[x]z}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = identifier("z") token_offset:1:5 to :1:6
        \\    %4 = ret_node(%3) node_offset:1:5 to :1:6
        \\  }) (lbrace=1:1,rbrace=1:6) node_offset:1:1 to :1:7
        \\})
    );
    try testZir("{[x;y]z}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = param_node("y") node_offset:1:5 to :1:6
        \\    %4 = identifier("z") token_offset:1:7 to :1:8
        \\    %5 = ret_node(%4) node_offset:1:7 to :1:8
        \\  }) (lbrace=1:1,rbrace=1:8) node_offset:1:1 to :1:9
        \\})
    );
}

test "lambda semicolon" {
    try testZir("{;}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_implicit(@x) token_offset:1:1 to :1:2
        \\    %3 = ret_implicit(@null) token_offset:1:3 to :1:4
        \\  }) (lbrace=1:1,rbrace=1:3) node_offset:1:1 to :1:4
        \\})
    );
    try testZir("{[];}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = ret_implicit(@null) token_offset:1:5 to :1:6
        \\  }) (lbrace=1:1,rbrace=1:5) node_offset:1:1 to :1:6
        \\})
    );
    try testZir("{[x];}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = ret_implicit(@null) token_offset:1:6 to :1:7
        \\  }) (lbrace=1:1,rbrace=1:6) node_offset:1:1 to :1:7
        \\})
    );
    try testZir("{[x;y];}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = param_node("y") node_offset:1:5 to :1:6
        \\    %4 = ret_implicit(@null) token_offset:1:8 to :1:9
        \\  }) (lbrace=1:1,rbrace=1:8) node_offset:1:1 to :1:9
        \\})
    );

    try testZir("{1;}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_implicit(@x) token_offset:1:1 to :1:2
        \\    %3 = ret_implicit(@null) token_offset:1:4 to :1:5
        \\  }) (lbrace=1:1,rbrace=1:4) node_offset:1:1 to :1:5
        \\})
    );
    try testZir("{[]1;}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = ret_implicit(@null) token_offset:1:6 to :1:7
        \\  }) (lbrace=1:1,rbrace=1:6) node_offset:1:1 to :1:7
        \\})
    );
    try testZir("{[x]1;}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = ret_implicit(@null) token_offset:1:7 to :1:8
        \\  }) (lbrace=1:1,rbrace=1:7) node_offset:1:1 to :1:8
        \\})
    );
    try testZir("{[x;y]1;}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = param_node("y") node_offset:1:5 to :1:6
        \\    %4 = ret_implicit(@null) token_offset:1:9 to :1:10
        \\  }) (lbrace=1:1,rbrace=1:9) node_offset:1:1 to :1:10
        \\})
    );

    try testZir("{2;}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_implicit(@x) token_offset:1:1 to :1:2
        \\    %3 = long(2)
        \\    %4 = ret_implicit(@null) token_offset:1:4 to :1:5
        \\  }) (lbrace=1:1,rbrace=1:4) node_offset:1:1 to :1:5
        \\})
    );
    try testZir("{[]2;}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = long(2)
        \\    %3 = ret_implicit(@null) token_offset:1:6 to :1:7
        \\  }) (lbrace=1:1,rbrace=1:6) node_offset:1:1 to :1:7
        \\})
    );
    try testZir("{[x]2;}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = long(2)
        \\    %4 = ret_implicit(@null) token_offset:1:7 to :1:8
        \\  }) (lbrace=1:1,rbrace=1:7) node_offset:1:1 to :1:8
        \\})
    );
    try testZir("{[x;y]2;}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = param_node("y") node_offset:1:5 to :1:6
        \\    %4 = long(2)
        \\    %5 = ret_implicit(@null) token_offset:1:9 to :1:10
        \\  }) (lbrace=1:1,rbrace=1:9) node_offset:1:1 to :1:10
        \\})
    );

    try testZir("{x;}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_implicit(@x) token_offset:1:1 to :1:2
        \\    %3 = identifier("x") token_offset:1:2 to :1:3
        \\    %4 = ret_implicit(@null) token_offset:1:4 to :1:5
        \\  }) (lbrace=1:1,rbrace=1:4) node_offset:1:1 to :1:5
        \\})
    );
    try testZir("{[]x;}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("x") token_offset:1:4 to :1:5
        \\    %3 = ret_implicit(@null) token_offset:1:6 to :1:7
        \\  }) (lbrace=1:1,rbrace=1:6) node_offset:1:1 to :1:7
        \\})
    );
    try testZir("{[x]x;}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = ret_implicit(@null) token_offset:1:7 to :1:8
        \\  }) (lbrace=1:1,rbrace=1:7) node_offset:1:1 to :1:8
        \\})
    );
    try testZir("{[x;y]x;}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = param_node("y") node_offset:1:5 to :1:6
        \\    %4 = ret_implicit(@null) token_offset:1:9 to :1:10
        \\  }) (lbrace=1:1,rbrace=1:9) node_offset:1:1 to :1:10
        \\})
    );

    try testZir("{y;}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_implicit(@x) token_offset:1:1 to :1:2
        \\    %3 = param_implicit(@y) token_offset:1:1 to :1:2
        \\    %4 = identifier("y") token_offset:1:2 to :1:3
        \\    %5 = ret_implicit(@null) token_offset:1:4 to :1:5
        \\  }) (lbrace=1:1,rbrace=1:4) node_offset:1:1 to :1:5
        \\})
    );
    try testZir("{[]y;}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("y") token_offset:1:4 to :1:5
        \\    %3 = ret_implicit(@null) token_offset:1:6 to :1:7
        \\  }) (lbrace=1:1,rbrace=1:6) node_offset:1:1 to :1:7
        \\})
    );
    try testZir("{[x]y;}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = identifier("y") token_offset:1:5 to :1:6
        \\    %4 = ret_implicit(@null) token_offset:1:7 to :1:8
        \\  }) (lbrace=1:1,rbrace=1:7) node_offset:1:1 to :1:8
        \\})
    );
    try testZir("{[x;y]y;}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = param_node("y") node_offset:1:5 to :1:6
        \\    %4 = ret_implicit(@null) token_offset:1:9 to :1:10
        \\  }) (lbrace=1:1,rbrace=1:9) node_offset:1:1 to :1:10
        \\})
    );

    try testZir("{z;}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_implicit(@x) token_offset:1:1 to :1:2
        \\    %3 = param_implicit(@y) token_offset:1:1 to :1:2
        \\    %4 = param_implicit(@z) token_offset:1:1 to :1:2
        \\    %5 = identifier("z") token_offset:1:2 to :1:3
        \\    %6 = ret_implicit(@null) token_offset:1:4 to :1:5
        \\  }) (lbrace=1:1,rbrace=1:4) node_offset:1:1 to :1:5
        \\})
    );
    try testZir("{[]z;}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("z") token_offset:1:4 to :1:5
        \\    %3 = ret_implicit(@null) token_offset:1:6 to :1:7
        \\  }) (lbrace=1:1,rbrace=1:6) node_offset:1:1 to :1:7
        \\})
    );
    try testZir("{[x]z;}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = identifier("z") token_offset:1:5 to :1:6
        \\    %4 = ret_implicit(@null) token_offset:1:7 to :1:8
        \\  }) (lbrace=1:1,rbrace=1:7) node_offset:1:1 to :1:8
        \\})
    );
    try testZir("{[x;y]z;}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = param_node("y") node_offset:1:5 to :1:6
        \\    %4 = identifier("z") token_offset:1:7 to :1:8
        \\    %5 = ret_implicit(@null) token_offset:1:9 to :1:10
        \\  }) (lbrace=1:1,rbrace=1:9) node_offset:1:1 to :1:10
        \\})
    );
}

test "expr block" {
    return error.SkipZigTest;
}

test "return" {
    try testZir(
        \\{[]
        \\  :1
        \\  }
    ,
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = ret_node(@one) node_offset:2:3 to :2:5
        \\    %3 = ret_node(%2) node_offset:2:3 to :2:5
        \\  }) (lbrace=1:1,rbrace=3:3) node_offset:1:1 to :1:2
        \\})
    );
    try testZir(
        \\{[]
        \\  :1;
        \\  }
    ,
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = ret_node(@one) node_offset:2:3 to :2:5
        \\    %3 = ret_implicit(@null) token_offset:3:3 to :3:4
        \\  }) (lbrace=1:1,rbrace=3:3) node_offset:1:1 to :1:2
        \\})
    );
}

test "signal" {
    try testZir("'`signal",
        \\%0 = file({
        \\  %1 = sym("signal") token_offset:1:2 to :1:9
        \\  %2 = signal(%1) node_offset:1:1 to :1:9
        \\})
    );
    try testZir("'\"signal\"",
        \\%0 = file({
        \\  %1 = str("signal") token_offset:1:2 to :1:10
        \\  %2 = signal(%1) node_offset:1:1 to :1:10
        \\})
    );
    try testZir("'break",
        \\%0 = file({
        \\  %1 = identifier("break") token_offset:1:2 to :1:7
        \\  %2 = signal(%1) node_offset:1:1 to :1:7
        \\})
    );
}

test "assign" {
    try testZir("x:1",
        \\%0 = file({
        \\  %1 = identifier("x") token_offset:1:1 to :1:2
        \\  %2 = assign(%1, @one) node_offset:1:1 to :1:4
        \\})
    );
    try testZir("{[]x:1}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("x") token_offset:1:4 to :1:5
        \\    %3 = assign(%2, @one) node_offset:1:4 to :1:7
        \\    %4 = ret_node(%3) node_offset:1:4 to :1:7
        \\  }) (lbrace=1:1,rbrace=1:7) node_offset:1:1 to :1:8
        \\})
    );
    try testZir("x:2",
        \\%0 = file({
        \\  %1 = long(2)
        \\  %2 = identifier("x") token_offset:1:1 to :1:2
        \\  %3 = assign(%2, %1) node_offset:1:1 to :1:4
        \\})
    );
    try testZir("{[]x:2}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = long(2)
        \\    %3 = identifier("x") token_offset:1:4 to :1:5
        \\    %4 = assign(%3, %2) node_offset:1:4 to :1:7
        \\    %5 = ret_node(%4) node_offset:1:4 to :1:7
        \\  }) (lbrace=1:1,rbrace=1:7) node_offset:1:1 to :1:8
        \\})
    );
}

test "global assign" {
    try testZir("x::1",
        \\%0 = file({
        \\  %1 = identifier("x") token_offset:1:1 to :1:2
        \\  %2 = view(%1, @one) node_offset:1:1 to :1:5
        \\})
    );
    try testZir("{[]x::1}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("x") token_offset:1:4 to :1:5
        \\    %3 = global_assign(%2, @one) node_offset:1:4 to :1:8
        \\    %4 = ret_node(%3) node_offset:1:4 to :1:8
        \\  }) (lbrace=1:1,rbrace=1:8) node_offset:1:1 to :1:9
        \\})
    );
    try testZir("x::2",
        \\%0 = file({
        \\  %1 = long(2)
        \\  %2 = identifier("x") token_offset:1:1 to :1:2
        \\  %3 = view(%2, %1) node_offset:1:1 to :1:5
        \\})
    );
    try testZir("{[]x::2}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = long(2)
        \\    %3 = identifier("x") token_offset:1:4 to :1:5
        \\    %4 = global_assign(%3, %2) node_offset:1:4 to :1:8
        \\    %5 = ret_node(%4) node_offset:1:4 to :1:8
        \\  }) (lbrace=1:1,rbrace=1:8) node_offset:1:1 to :1:9
        \\})
    );
}

test "colon" {
    return error.SkipZigTest;
}

test "colon colon" {
    return error.SkipZigTest;
}

test "plus" {
    return error.SkipZigTest;
}

test "plus colon" {
    return error.SkipZigTest;
}

test "minus" {
    return error.SkipZigTest;
}

test "minus colon" {
    return error.SkipZigTest;
}

test "asterisk" {
    return error.SkipZigTest;
}

test "asterisk colon" {
    return error.SkipZigTest;
}

test "percent" {
    return error.SkipZigTest;
}

test "percent colon" {
    return error.SkipZigTest;
}

test "ampersand" {
    return error.SkipZigTest;
}

test "ampersand colon" {
    return error.SkipZigTest;
}

test "pipe" {
    return error.SkipZigTest;
}

test "pipe colon" {
    return error.SkipZigTest;
}

test "caret" {
    return error.SkipZigTest;
}

test "caret colon" {
    return error.SkipZigTest;
}

test "equal" {
    return error.SkipZigTest;
}

test "equal colon" {
    return error.SkipZigTest;
}

test "angle bracket left" {
    return error.SkipZigTest;
}

test "angle bracket left colon" {
    return error.SkipZigTest;
}

test "angle bracket left equal" {
    return error.SkipZigTest;
}

test "angle bracket left right" {
    return error.SkipZigTest;
}

test "angle bracket right" {
    return error.SkipZigTest;
}

test "angle bracket right colon" {
    return error.SkipZigTest;
}

test "angle bracket right equal" {
    return error.SkipZigTest;
}

test "dollar" {
    return error.SkipZigTest;
}

test "dollar colon" {
    return error.SkipZigTest;
}

test "comma" {
    return error.SkipZigTest;
}

test "comma colon" {
    return error.SkipZigTest;
}

test "hash" {
    return error.SkipZigTest;
}

test "hash colon" {
    return error.SkipZigTest;
}

test "underscore" {
    return error.SkipZigTest;
}

test "underscore colon" {
    return error.SkipZigTest;
}

test "tilde" {
    return error.SkipZigTest;
}

test "tilde colon" {
    return error.SkipZigTest;
}

test "bang" {
    return error.SkipZigTest;
}

test "bang colon" {
    return error.SkipZigTest;
}

test "question mark" {
    return error.SkipZigTest;
}

test "question mark colon" {
    return error.SkipZigTest;
}

test "at" {
    return error.SkipZigTest;
}

test "at colon" {
    return error.SkipZigTest;
}

test "period" {
    return error.SkipZigTest;
}

test "period colon" {
    return error.SkipZigTest;
}

test "zero colon" {
    return error.SkipZigTest;
}

test "zero colon colon" {
    return error.SkipZigTest;
}

test "one colon" {
    return error.SkipZigTest;
}

test "one colon colon" {
    return error.SkipZigTest;
}

test "two colon" {
    return error.SkipZigTest;
}

test "apostrophe" {
    return error.SkipZigTest;
}

test "apostrophe colon" {
    return error.SkipZigTest;
}

test "slash" {
    return error.SkipZigTest;
}

test "slash colon" {
    return error.SkipZigTest;
}

test "backslash" {
    return error.SkipZigTest;
}

test "backslash colon" {
    return error.SkipZigTest;
}

test "call" {
    return error.SkipZigTest;
}

test "apply unary" {
    try testZir("f x",
        \\%0 = file({
        \\  %1 = identifier("x") token_offset:1:3 to :1:4
        \\  %2 = identifier("f") token_offset:1:1 to :1:2
        \\  %3 = apply_at(%2, %1) node_offset:1:1 to :1:4
        \\})
    );
}

test "apply binary" {
    try testZir("x+1",
        \\%0 = file({
        \\  %1 = identifier("x") token_offset:1:1 to :1:2
        \\  %2 = add(%1, @one) node_offset:1:1 to :1:4
        \\})
    );
    try testZir("{[]x+1}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("x") token_offset:1:4 to :1:5
        \\    %3 = add(%2, @one) node_offset:1:4 to :1:7
        \\    %4 = ret_node(%3) node_offset:1:4 to :1:7
        \\  }) (lbrace=1:1,rbrace=1:7) node_offset:1:1 to :1:8
        \\})
    );
    try testZir("x+2",
        \\%0 = file({
        \\  %1 = long(2)
        \\  %2 = identifier("x") token_offset:1:1 to :1:2
        \\  %3 = add(%2, %1) node_offset:1:1 to :1:4
        \\})
    );
    try testZir("{[]x+2}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = long(2)
        \\    %3 = identifier("x") token_offset:1:4 to :1:5
        \\    %4 = add(%3, %2) node_offset:1:4 to :1:7
        \\    %5 = ret_node(%4) node_offset:1:4 to :1:7
        \\  }) (lbrace=1:1,rbrace=1:7) node_offset:1:1 to :1:8
        \\})
    );
}

test "number literal" {
    return error.SkipZigTest;
}

test "number list literal" {
    return error.SkipZigTest;
}

test "string literal" {
    return error.SkipZigTest;
}

test "symbol literal" {
    try testZir("`symbol123",
        \\%0 = file({
        \\  %1 = sym("symbol123") token_offset:1:1 to :1:11
        \\})
    );
}

test "symbol list literal" {
    return error.SkipZigTest;
}

test "identifier" {
    try testZir("a",
        \\%0 = file({
        \\  %1 = identifier("a") token_offset:1:1 to :1:2
        \\})
    );
}

test "builtin" {
    return error.SkipZigTest;
}

test "select" {
    return error.SkipZigTest;
}

test "exec" {
    return error.SkipZigTest;
}

test "update" {
    return error.SkipZigTest;
}

test "delete rows" {
    return error.SkipZigTest;
}

test "delete cols" {
    return error.SkipZigTest;
}

test "do" {
    return error.SkipZigTest;
}

test "if" {
    return error.SkipZigTest;
}

test "while" {
    return error.SkipZigTest;
}

test "cond" {
    return error.SkipZigTest;
}

test "too many parameters" {
    try testZir("{[a;b;c;d;e;f;g;h]}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("a") node_offset:1:3 to :1:4
        \\    %3 = param_node("b") node_offset:1:5 to :1:6
        \\    %4 = param_node("c") node_offset:1:7 to :1:8
        \\    %5 = param_node("d") node_offset:1:9 to :1:10
        \\    %6 = param_node("e") node_offset:1:11 to :1:12
        \\    %7 = param_node("f") node_offset:1:13 to :1:14
        \\    %8 = param_node("g") node_offset:1:15 to :1:16
        \\    %9 = param_node("h") node_offset:1:17 to :1:18
        \\    %10 = ret_implicit(@null) token_offset:1:19 to :1:20
        \\  }) (lbrace=1:1,rbrace=1:19) node_offset:1:1 to :1:20
        \\})
    );
    try failZir("{[a;b;c;d;e;f;g;h;i]}",
        \\test:1:19: error: too many parameters (8 max)
        \\{[a;b;c;d;e;f;g;h;i]}
        \\                  ^
    );
    try failZir("{[a;b;c;d;e;f;g;h;i;j]}",
        \\test:1:19: error: too many parameters (8 max)
        \\{[a;b;c;d;e;f;g;h;i;j]}
        \\                  ^
    );
}

test "declared after use / use of undeclared identifier" {
    try failZir("{[]x:x::1}",
        \\test:1:6: error: use of undeclared identifier 'x'
        \\{[]x:x::1}
        \\     ^
        \\test:1:4: note: identifier declared here
        \\{[]x:x::1}
        \\   ^
    );

    try testZir("{[]a::a+1}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("a") token_offset:1:7 to :1:8
        \\    %3 = add(%2, @one) node_offset:1:7 to :1:10
        \\    %4 = identifier("a") token_offset:1:4 to :1:5
        \\    %5 = global_assign(%4, %3) node_offset:1:4 to :1:10
        \\    %6 = ret_node(%5) node_offset:1:4 to :1:10
        \\  }) (lbrace=1:1,rbrace=1:10) node_offset:1:1 to :1:11
        \\})
    );
    try failZir("{[]a:a+1}",
        \\test:1:6: error: use of undeclared identifier 'a'
        \\{[]a:a+1}
        \\     ^
        \\test:1:4: note: identifier declared here
        \\{[]a:a+1}
        \\   ^
    );

    try testZir(
        \\{[]
        \\  a+1;
        \\  a::1;
        \\  }
    ,
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("a") token_offset:2:3 to :2:4
        \\    %3 = add(%2, @one) node_offset:2:3 to :2:6
        \\    %4 = identifier("a") token_offset:3:3 to :3:4
        \\    %5 = global_assign(%4, @one) node_offset:3:3 to :3:7
        \\    %6 = ret_implicit(@null) token_offset:4:3 to :4:4
        \\  }) (lbrace=1:1,rbrace=4:3) node_offset:1:1 to :1:2
        \\})
    );
    try failZir(
        \\{[]
        \\  a+1;
        \\  a:1;
        \\  }
    ,
        \\test:2:3: error: use of undeclared identifier 'a'
        \\  a+1;
        \\  ^
        \\test:3:3: note: identifier declared here
        \\  a:1;
        \\  ^
    );
    try failZir(
        \\{[]
        \\  a::1;
        \\  a:1;
        \\  }
    ,
        \\test:2:3: error: use of undeclared identifier 'a'
        \\  a::1;
        \\  ^
        \\test:3:3: note: identifier declared here
        \\  a:1;
        \\  ^
    );
}

test "unused function parameter" {
    try warnZir("{[x]}",
        \\test:1:3: warn: unused function parameter
        \\{[x]}
        \\  ^
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = ret_implicit(@null) token_offset:1:5 to :1:6
        \\  }) (lbrace=1:1,rbrace=1:5) node_offset:1:1 to :1:6
        \\})
    );
    try warnZir("{[x;y]}",
        \\test:1:5: warn: unused function parameter
        \\{[x;y]}
        \\    ^
        \\test:1:3: warn: unused function parameter
        \\{[x;y]}
        \\  ^
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = param_node("y") node_offset:1:5 to :1:6
        \\    %4 = ret_implicit(@null) token_offset:1:7 to :1:8
        \\  }) (lbrace=1:1,rbrace=1:7) node_offset:1:1 to :1:8
        \\})
    );
    try testZir("{y}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_implicit(@x) token_offset:1:1 to :1:2
        \\    %3 = param_implicit(@y) token_offset:1:1 to :1:2
        \\    %4 = identifier("y") token_offset:1:2 to :1:3
        \\    %5 = ret_node(%4) node_offset:1:2 to :1:3
        \\  }) (lbrace=1:1,rbrace=1:3) node_offset:1:1 to :1:4
        \\})
    );
    try testZir("{z}",
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_implicit(@x) token_offset:1:1 to :1:2
        \\    %3 = param_implicit(@y) token_offset:1:1 to :1:2
        \\    %4 = param_implicit(@z) token_offset:1:1 to :1:2
        \\    %5 = identifier("z") token_offset:1:2 to :1:3
        \\    %6 = ret_node(%5) node_offset:1:2 to :1:3
        \\  }) (lbrace=1:1,rbrace=1:3) node_offset:1:1 to :1:4
        \\})
    );
}

test "unused local variable" {
    try warnZir(
        \\{[]
        \\  a:1;
        \\  }
    ,
        \\test:2:3: warn: unused local variable
        \\  a:1;
        \\  ^
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("a") token_offset:2:3 to :2:4
        \\    %3 = assign(%2, @one) node_offset:2:3 to :2:6
        \\    %4 = ret_implicit(@null) token_offset:3:3 to :3:4
        \\  }) (lbrace=1:1,rbrace=3:3) node_offset:1:1 to :1:2
        \\})
    );
    try warnZir(
        \\{[]
        \\  a:1;
        \\  a:2;
        \\  }
    ,
        \\test:2:3: warn: unused local variable
        \\  a:1;
        \\  ^
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("a") token_offset:2:3 to :2:4
        \\    %3 = assign(%2, @one) node_offset:2:3 to :2:6
        \\    %4 = long(2)
        \\    %5 = assign(%2, %4) node_offset:3:3 to :3:6
        \\    %6 = ret_implicit(@null) token_offset:4:3 to :4:4
        \\  }) (lbrace=1:1,rbrace=4:3) node_offset:1:1 to :1:2
        \\})
    );
    try warnZir(
        \\{
        \\  a:1;
        \\  a:2;
        \\  }
    ,
        \\test:2:3: warn: unused local variable
        \\  a:1;
        \\  ^
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_implicit(@x) token_offset:1:1 to :1:2
        \\    %3 = identifier("a") token_offset:2:3 to :2:4
        \\    %4 = assign(%3, @one) node_offset:2:3 to :2:6
        \\    %5 = long(2)
        \\    %6 = assign(%3, %5) node_offset:3:3 to :3:6
        \\    %7 = ret_implicit(@null) token_offset:4:3 to :4:4
        \\  }) (lbrace=1:1,rbrace=4:3) node_offset:1:1 to :1:2
        \\})
    );
}

test "redeclaration of function parameter" {
    try warnZir("{[a;a]}",
        \\test:1:5: warn: redeclaration of function parameter 'a'
        \\{[a;a]}
        \\    ^
        \\test:1:3: note: previous declaration here
        \\{[a;a]}
        \\  ^
        \\test:1:3: warn: unused function parameter
        \\{[a;a]}
        \\  ^
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("a") node_offset:1:3 to :1:4
        \\    %3 = ret_implicit(@null) token_offset:1:7 to :1:8
        \\  }) (lbrace=1:1,rbrace=1:7) node_offset:1:1 to :1:8
        \\})
    );
}

test "misleading global assign" {
    try warnZir("{[x]x::1}",
        \\test:1:6: warn: misleading global-assign of function parameter 'x'
        \\{[x]x::1}
        \\    ~^~~
        \\test:1:3: note: function parameter declared here
        \\{[x]x::1}
        \\  ^
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = param_node("x") node_offset:1:3 to :1:4
        \\    %3 = assign(%2, @one) node_offset:1:5 to :1:9
        \\    %4 = ret_node(%3) node_offset:1:5 to :1:9
        \\  }) (lbrace=1:1,rbrace=1:9) node_offset:1:1 to :1:10
        \\})
    );
    try warnZir(
        \\{[]
        \\  a:1;
        \\  a::1;
        \\  }
    ,
        \\test:3:4: warn: misleading global-assign of local variable 'a'
        \\  a::1;
        \\  ~^~~
        \\test:2:3: note: local variable declared here
        \\  a:1;
        \\  ^
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("a") token_offset:2:3 to :2:4
        \\    %3 = assign(%2, @one) node_offset:2:3 to :2:6
        \\    %4 = assign(%2, @one) node_offset:3:3 to :3:7
        \\    %5 = ret_implicit(@null) token_offset:4:3 to :4:4
        \\  }) (lbrace=1:1,rbrace=4:3) node_offset:1:1 to :1:2
        \\})
    );
}

test "unreachable code" {
    try warnZir(
        \\{[]
        \\  :1;
        \\  1}
    ,
        \\test:3:3: warn: unreachable code
        \\  1}
        \\  ^
        \\test:2:3: note: control flow is diverted here
        \\  :1;
        \\  ^~
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = ret_node(@one) node_offset:2:3 to :2:5
        \\    %3 = ret_node(@one) node_offset:3:3 to :3:4
        \\  }) (lbrace=1:1,rbrace=3:4) node_offset:1:1 to :1:2
        \\})
    );
    try warnZir(
        \\{[]
        \\  'break;
        \\  1}
    ,
        \\test:3:3: warn: unreachable code
        \\  1}
        \\  ^
        \\test:2:3: note: control flow is diverted here
        \\  'break;
        \\  ^~~~~~
        \\%0 = file({
        \\  %1 = lambda({
        \\    %2 = identifier("break") token_offset:2:4 to :2:9
        \\    %3 = signal(%2) node_offset:2:3 to :2:9
        \\    %4 = ret_node(@one) node_offset:3:3 to :3:4
        \\  }) (lbrace=1:1,rbrace=3:4) node_offset:1:1 to :1:2
        \\})
    );
    try warnZir(
        \\'break;
        \\1
    ,
        \\test:2:1: warn: unreachable code
        \\1
        \\^
        \\test:1:1: note: control flow is diverted here
        \\'break;
        \\^~~~~~
        \\%0 = file({
        \\  %1 = identifier("break") token_offset:1:2 to :1:7
        \\  %2 = signal(%1) node_offset:1:1 to :1:7
        \\})
    );
}
