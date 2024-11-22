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

pub const ExtraIndex = enum(u32) {
    /// If this is 0, no compile errors. Otherwise there is a `CompileErrors`
    /// payload at this index.
    compile_errors,
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
        /// Uses the `pl_node` union field. Payload is `Bin`.
        assign,
        /// `+`.
        /// Uses the `pl_node` union field. Payload is `Bin`.
        add,
        /// `-`.
        /// Uses the `pl_node` union field. Payload is `Bin`.
        subtract,
        /// `*`.
        /// Uses the `pl_node` union field. Payload is `Bin`.
        multiply,
        /// `%`.
        /// Uses the `pl_node` union field. Payload is `Bin`.
        divide,
        /// `&`.
        /// Uses the `pl_node` union field. Payload is `Bin`.
        lesser,
        /// `|`.
        /// Uses the `pl_node` union field. Payload is `Bin`.
        greater,
        /// `^`.
        /// Uses the `pl_node` union field. Payload is `Bin`.
        fill,
        /// `=`.
        /// Uses the `pl_node` union field. Payload is `Bin`.
        equal,
        /// `<`.
        /// Uses the `pl_node` union field. Payload is `Bin`.
        less_than,
        /// `<=`.
        /// Uses the `pl_node` union field. Payload is `Bin`.
        less_than_or_equal,
        /// `<>`.
        /// Uses the `pl_node` union field. Payload is `Bin`.
        not_equal,
        /// `>`.
        /// Uses the `pl_node` union field. Payload is `Bin`.
        greater_than,
        /// `>=`.
        /// Uses the `pl_node` union field. Payload is `Bin`.
        greater_than_or_equal,
        /// `,`.
        /// Uses the `pl_node` union field. Payload is `Bin`.
        join,
        /// `~`.
        /// Uses the `pl_node` union field. Payload is `Bin`.
        match,
        /// `2:`.
        /// Uses the `pl_node` union field. Payload is `Bin`.
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

        /// Integer literal that fits in an i64.
        /// Uses the `long` union field.
        long,

        /// Identifier.
        /// Uses the `str_tok` union field. Token is the identifier name. String is the identifier name.
        identifier,

        call,
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
        zero,
        one,
        negative_one,
        null,
        x,
        y,
        z,

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
        long: i64,
        lambda: struct {
            /// This node provides a new absolute baseline node for all instructions within this struct.
            src_node: Ast.Node.Index,
            /// index into extra to a `Lambda` payload.
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
    /// 1. params: Index // for each params_len
    /// 2. body: Index // for each body_len
    /// 3. src_locs: SrcLocs
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

    /// The meaning of these operands depends on the corresponding `Tag`.
    pub const Bin = struct {
        lhs: Ref,
        rhs: Ref,
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

            pub fn notesLen(item: Item, zir: Zir) u32 {
                if (item.notes == 0) return 0;
                const block = zir.extraData(Block, item.notes);
                return block.data.body_len;
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
