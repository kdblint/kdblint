const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;

const kdb = @import("root.zig");
const Ast = kdb.Ast;

const Zir = @This();

instructions: std.MultiArrayList(Inst).Slice,
/// In order to store references to strings in fewer bytes, we copy all
/// string bytes into here. String bytes can be null. It is up to whomever
/// is referencing the data here whether they want to store both index and length,
/// thus allowing null bytes, or store only index, and use null-termination. The
/// `string_bytes` array is agnostic to either usage.
/// Index 0 is reserved for special cases.
string_bytes: []u8,
/// The meaning of this data is determined by `Inst.Tag` value.
/// The first few indexes are reserved. See `ExtraIndex` for the values.
extra: []u32,

compile_duration: u64,

pub const ExtraIndex = enum(u32) {
    /// If this is 0, no compile errors. Otherwise there is a `CompileErrors`
    /// payload at this index.
    compile_errors,
    /// If this is 0, no compile warnings. Otherwise there is a `CompileErrors`
    /// payload at this index.
    compile_warnings,
    /// If this is 0, this file contains no imports. Otherwise there is a `Imports`
    /// payload at this index.
    imports,

    _,
};

fn ExtraData(comptime T: type) type {
    return struct { data: T, end: usize };
}

/// Returns the requested data, as well as the new index which is at the start of the
/// trailers for the object.
pub fn extraData(code: Zir, comptime T: type, index: usize) ExtraData(T) {
    const fields = @typeInfo(T).@"struct".fields;
    var i: usize = index;
    var result: T = undefined;
    inline for (fields) |field| {
        @field(result, field.name) = switch (field.type) {
            u32 => code.extra[i],

            Inst.Ref,
            Inst.Index,
            NullTerminatedString,
            Inst.CompileErrors.Kind,
            => @enumFromInt(code.extra[i]),

            i32 => @bitCast(code.extra[i]),

            else => |t| @compileError("bad field type: " ++ @typeName(t)),
        };
        i += 1;
    }
    return .{
        .data = result,
        .end = i,
    };
}

pub const NullTerminatedString = enum(u32) {
    empty = 0,
    _,
};

/// Given an index into `string_bytes` returns the null-terminated string found there.
pub fn nullTerminatedString(code: Zir, index: NullTerminatedString) [:0]const u8 {
    const slice = code.string_bytes[@intFromEnum(index)..];
    return slice[0..std.mem.indexOfScalar(u8, slice, 0).? :0];
}

pub fn bodySlice(code: Zir, start: usize, len: usize) []Inst.Index {
    return @ptrCast(code.extra[start..][0..len]);
}

pub fn hasCompileErrors(code: Zir) bool {
    return code.extra[@intFromEnum(ExtraIndex.compile_errors)] != 0;
}

pub fn hasCompileWarnings(code: Zir) bool {
    return code.extra[@intFromEnum(ExtraIndex.compile_warnings)] != 0;
}

pub fn deinit(code: *Zir, gpa: Allocator) void {
    code.instructions.deinit(gpa);
    gpa.free(code.string_bytes);
    gpa.free(code.extra);
    code.* = undefined;
}

/// These are untyped instructions generated from an Abstract Syntax Tree.
/// The data here is immutable because it is possible to have multiple
/// analyses on the same ZIR happening at the same time.
pub const Inst = struct {
    tag: Tag,
    data: Data,

    /// These names are used directly as the instruction names in the text format.
    /// See `data_field_map` for a list of which `Data` fields are used by each `Tag`.
    pub const Tag = enum(u8) {
        /// Represents a file.
        /// Uses the `pl_node` union field with payload `Block`.
        file,

        /// Variable assignment.
        /// Uses the `node` union field.
        assign,
        /// `+`.
        /// Uses the `node` union field.
        add,
        /// `-`.
        /// Uses the `node` union field.
        subtract,
        /// `*`.
        /// Uses the `node` union field.
        multiply,
        /// `%`.
        /// Uses the `node` union field.
        divide,
        /// `&`.
        /// Uses the `node` union field.
        lesser,
        /// `|`.
        /// Uses the `node` union field.
        greater,
        /// `^`.
        /// Uses the `node` union field.
        fill,
        /// `=`.
        /// Uses the `node` union field.
        equal,
        /// `<`.
        /// Uses the `node` union field.
        less_than,
        /// `<=`.
        /// Uses the `node` union field.
        less_than_or_equal,
        /// `<>`.
        /// Uses the `node` union field.
        not_equal,
        /// `>`.
        /// Uses the `node` union field.
        greater_than,
        /// `>=`.
        /// Uses the `node` union field.
        greater_than_or_equal,
        /// `$`.
        /// Uses the `node` union field.
        cast,
        /// `,`.
        /// Uses the `node` union field.
        join,
        /// `#`.
        /// Uses the `node` union field.
        take,
        /// `_`.
        /// Uses the `node` union field.
        drop,
        /// `~`.
        /// Uses the `node` union field.
        match,
        /// `!`.
        /// Uses the `node` union field.
        dict,
        /// `?`.
        /// Uses the `node` union field.
        find,
        /// `@`.
        /// Uses the `node` union field.
        apply_at,
        /// `.`.
        /// Uses the `node` union field.
        apply_dot,
        /// `0:`.
        /// Uses the `node` union field.
        file_text,
        /// `1:`.
        /// Uses the `node` union field.
        file_binary,
        /// `2:`.
        /// Uses the `node` union field.
        dynamic_load,

        /// Lambda expression.
        /// Uses the `lambda` union field. Payload is `Lambda`.
        lambda,
        /// Declares a parameter of the current lambda.
        /// Includes an AST node source location.
        /// Uses the `str_node` union field. Node is the parameter name. String is the parameter name.
        param_node,
        /// Declares a parameter of the current lambda.
        /// Includes a token source location.
        /// Uses the `un_tok` union field. Token is the `{`. Operand is the parameter name.
        param_implicit,
        /// Sends control flow back to the function's callee.
        /// Includes an operand as the return value.
        /// Includes an AST node source location.
        /// Uses the `un_node` union field.
        ret_node,
        /// Sends control flow back to the function's callee.
        /// Includes an operand as the return value.
        /// Includes a token source location.
        /// Uses the `un_tok` union field.
        ret_implicit,

        /// Do statement.
        /// Uses the `pl_node` union field with payload `Do`.
        do,
        /// If statement.
        /// Uses the `pl_node` union field with payload `If`.
        @"if",
        /// While statement.
        /// Uses the `pl_node` union field with payload `While`.
        @"while",

        /// Signals an error.
        /// Uses the `un_node` union field.
        signal,

        /// Boolean list literal.
        /// Uses the `pl_node` union field with payload `List`.
        bool_list,
        /// Integer literal that fits in a u8.
        /// Uses the `byte` union field.
        byte,
        /// Integer list literal that fits in a u8.
        /// Uses the `pl_node` union field with payload `List`.
        byte_list,
        /// Integer literal that fits in an i16.
        /// Uses the `short` union field.
        short,
        /// Integer list literal that fits in an i16.
        /// Uses the `pl_node` union field with payload `List`.
        short_list,
        /// Integer literal that fits in an i32.
        /// Uses the `int` union field.
        int,
        /// Integer list literal that fits in an i32.
        /// Uses the `pl_node` union field with payload `List`.
        int_list,
        /// Integer literal that fits in an i64.
        /// Uses the `long` union field.
        long,
        /// Integer list literal that fits in an i64.
        /// Uses the `pl_node` union field with payload `List`.
        long_list,
        /// Integer literal that fits in a u8.
        /// Uses the `byte` union field.
        char,
        /// Integer list literal that fits in a u8.
        /// Uses the `pl_node` union field with payload `List`.
        char_list,
        /// Integer literal that fits in an i32.
        /// Uses the `int` union field.
        month,
        /// Integer list literal that fits in an i32.
        /// Uses the `pl_node` union field with payload `List`.
        month_list,

        /// String literal
        /// Uses the `str_tok` union field. Token is the string literal. String is the string content.
        str,
        /// Symbol literal
        /// Uses the `str_tok` union field. Token is the symbol literal. String is the symbol content.
        sym,
        /// Symbol list literal
        /// Uses the `pl_node` union field with payload `SymList`.
        sym_list,

        /// Identifier.
        /// Uses the `str_tok` union field. Token is the identifier name. String is the identifier name.
        identifier,

        /// Builtin.
        /// Uses the `str_tok` union field. Token is the builtin name. String is the builtin name.
        builtin,

        /// Apply.
        /// Uses the `pl_node` union field with payload `Apply`.
        apply,

        /// List.
        /// Uses the `pl_node` union field with payload `List`.
        list,

        /// Table.
        /// Uses the `pl_node` union field with payload `Table`.
        table,

        /// Returns whether the instruction is one of the control flow "noreturn" types.
        /// Function calls do not count.
        pub fn isNoReturn(tag: Tag) bool {
            return switch (tag) {
                .file,
                .assign,
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
                .cast,
                .join,
                .take,
                .drop,
                .match,
                .dict,
                .find,
                .apply_at,
                .apply_dot,
                .file_text,
                .file_binary,
                .dynamic_load,
                .lambda,
                .param_node,
                .param_implicit,
                .do,
                .@"if",
                .@"while",
                .bool_list,
                .byte,
                .byte_list,
                .short,
                .short_list,
                .int,
                .int_list,
                .long,
                .long_list,
                .char,
                .char_list,
                .month,
                .month_list,
                .str,
                .sym,
                .sym_list,
                .identifier,
                .builtin,
                .apply,
                .list,
                .table,
                => false,

                .ret_node,
                .ret_implicit,
                .signal,
                => true,
            };
        }
    };

    /// The position of a ZIR instruction within the `Zir` instructions array.
    pub const Index = enum(u32) {
        /// ZIR is structured so that the outermost "main" struct of any file
        /// is always at index 0.
        file_inst = 0,
        ref_start_index = static_len,
        _,

        pub const static_len = 71;

        pub fn toRef(i: Index) Inst.Ref {
            return @enumFromInt(@intFromEnum(Index.ref_start_index) + @intFromEnum(i));
        }
    };

    /// A reference to ZIR instruction, or to an InternPool index, or neither.
    ///
    /// If the integer tag value is < InternPool.static_len, then it
    /// corresponds to an InternPool index. Otherwise, this refers to a ZIR
    /// instruction.
    ///
    /// The tag type is specified so that it is safe to bitcast between `[]u32`
    /// and `[]Ref`.
    pub const Ref = enum(u32) {
        true,
        false,
        zero,
        one,
        negative_one,
        null,
        empty_list,
        x,
        y,
        z,

        assign,
        add,
        subtract,
        multiply,
        divide,
        lesser,
        greater,
        fill,
        equal,
        less_than,
        less_than_or_equal,
        not_equal,
        greater_than,
        greater_than_or_equal,
        cast,
        join,
        take,
        drop,
        match,
        dict,
        find,
        apply_at,
        apply_dot,
        file_text,
        file_binary,
        dynamic_load,

        each,
        each_prior,
        over,
        each_right,
        scan,
        each_left,

        null_guid,
        null_short,
        null_int,
        null_long,
        null_char,
        null_month,

        inf_short,
        negative_inf_short,
        inf_int,
        negative_inf_int,
        inf_long,
        negative_inf_long,
        inf_month,
        negative_inf_month,

        nyi,

        /// This Ref does not correspond to any ZIR instruction or constant
        /// value and may instead be used as a sentinel to indicate null.
        none = std.math.maxInt(u32),

        _,

        pub fn toIndex(inst: Ref) ?Index {
            assert(inst != .none);
            const ref_int = @intFromEnum(inst);
            if (ref_int >= @intFromEnum(Index.ref_start_index)) {
                return @enumFromInt(ref_int - @intFromEnum(Index.ref_start_index));
            } else {
                return null;
            }
        }

        pub fn toIndexAllowNone(inst: Ref) ?Index {
            if (inst == .none) return null;
            return toIndex(inst);
        }
    };

    /// All instructions have an 8-byte payload, which is contained within
    /// this union. `Tag` determines which union field is active, as well as
    /// how to interpret the data within.
    pub const Data = union {
        /// Used for unary operators, with an AST node source location.
        un_node: struct {
            /// Offset from Decl AST node index.
            src_node: i32,
            /// The meaning of this operand depends on the corresponding `Tag`.
            operand: Ref,
        },
        /// Used for unary operators, with a token source location.
        un_tok: struct {
            /// Offset from Decl AST token index.
            src_tok: Ast.Token.Index,
            /// The meaning of this operand depends on the corresponding `Tag`.
            operand: Ref,
        },
        pl_node: struct {
            /// Offset from Decl AST node index.
            /// `Tag` determines which kind of AST node this points to.
            src_node: i32,
            /// index into extra.
            /// `Tag` determines what lives there.
            payload_index: u32,
        },
        str_node: struct {
            /// Offset into `string_bytes`. Null-terminated.
            start: NullTerminatedString,
            /// Offset from Decl AST node index.
            src_node: i32,

            pub fn get(self: @This(), code: Zir) [:0]const u8 {
                return code.nullTerminatedString(self.start);
            }
        },
        str_tok: struct {
            /// Offset into `string_bytes`. Null-terminated.
            start: NullTerminatedString,
            /// Offset from Decl AST token index.
            src_tok: u32,

            pub fn get(self: @This(), code: Zir) [:0]const u8 {
                return code.nullTerminatedString(self.start);
            }
        },
        /// Offset from Decl AST node index.
        node: i32,
        byte: u8,
        short: i16,
        int: i32,
        long: i64,
        lambda: struct {
            /// This node provides a new absolute baseline node for all instructions within this struct.
            src_node: Ast.Node.Index,
            /// index into extra to a `Lambda` payload.
            payload_index: u32,
        },
        @"break": struct {
            operand: Ref,
            /// Index of a `Break` payload.
            payload_index: u32,
        },

        // Make sure we don't accidentally add a field to make this union
        // bigger than expected. Note that in Debug builds, Zig is allowed
        // to insert a secret field for safety checks.
        comptime {
            if (builtin.mode != .Debug and builtin.mode != .ReleaseSafe) {
                assert(@sizeOf(Data) == 8);
            }
        }
    };

    /// Trailing:
    /// 1. elem: Ref // for each len
    pub const List = struct {
        len: u32,
    };

    /// Trailing:
    /// 1. elem: Ref // for each len
    pub const Apply = struct {
        callee: Ref,
        len: u32,
    };

    /// Trailing:
    /// 1. body: Index // for each body_len
    /// 2. src_locs: SrcLocs
    pub const Lambda = struct {
        params_len: u32,
        body_len: u32,

        pub const SrcLocs = struct {
            /// Line index in the source file relative to the parent decl.
            lbrace_line: u32,
            /// Line index in the source file relative to the parent decl.
            rbrace_line: u32,
            /// lbrace_column is least significant bits u16
            /// rbrace_column is most significant bits u16
            columns: u32,
        };
    };

    /// Trailing:
    /// 1. keys: Item // for each keys_len
    /// 2. columns: Item // for each columns_len
    pub const Table = struct {
        keys_len: u32,
        columns_len: u32,

        pub const Item = struct {
            name: NullTerminatedString,
            ref: Ref,
        };
    };

    /// Trailing:
    /// 1. body: Ref // for each body_len
    pub const Do = struct {
        condition: Ref,
        body_len: u32,
    };

    /// Trailing:
    /// 1. body: Ref // for each body_len
    pub const If = struct {
        condition: Ref,
        body_len: u32,
    };

    /// Trailing:
    /// 1. body: Ref // for each body_len
    pub const While = struct {
        condition: Ref,
        body_len: u32,
    };

    /// Trailing:
    /// 1. elem: NullTerminatedString // for each len
    pub const StrList = struct {
        len: u32,
    };

    /// This data is stored inside extra, with trailing operands according to `body_len`.
    /// Each operand is an `Index`.
    pub const Block = struct {
        body_len: u32,
    };

    /// Trailing: `CompileErrors.Item` for each `items_len`.
    pub const CompileErrors = struct {
        items_len: u32,

        /// Trailing: `note_payload_index: u32` for each `notes_len`.
        /// It's a payload index of another `Item`.
        pub const Item = struct {
            /// null terminated string index
            msg: NullTerminatedString,
            node: Ast.Node.Index,
            /// If node is 0 then this will be populated.
            token: Ast.Token.Index,
            /// Can be used in combination with `token`.
            byte_offset: u32,
            /// 0 or a payload index of a `Block`, each is a payload
            /// index of another `Item`.
            notes: u32,
            kind: Kind,

            pub fn notesLen(item: Item, zir: Zir) u32 {
                if (item.notes == 0) return 0;
                const block = zir.extraData(Block, item.notes);
                return block.data.body_len;
            }
        };

        pub const Kind = enum {
            @"error",
            warn,
            note,

            pub fn color(self: Kind) std.io.tty.Color {
                return switch (self) {
                    .@"error" => .red,
                    .warn => .yellow,
                    .note => .cyan,
                };
            }
        };
    };

    /// Trailing: for each `imports_len` there is an Item
    pub const Imports = struct {
        imports_len: u32,

        pub const Item = struct {
            /// null terminated string index
            name: NullTerminatedString,
            /// points to the import name
            token: Ast.Token.Index,
        };
    };
};
