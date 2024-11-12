const std = @import("std");
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

            Inst.Index,
            NullTerminatedString,
            => @enumFromInt(code.extra[i]),

            i32,
            => @bitCast(code.extra[i]),

            else => @compileError("bad field type"),
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
    /// The position of a ZIR instruction within the `Zir` instructions array.
    pub const Index = enum(u32) {
        /// ZIR is structured so that the outermost "main" struct of any file
        /// is always at index 0.
        main_struct_inst = 0,
        ref_start_index = static_len,
        _,

        pub const static_len = 71;

        pub fn toRef(i: Index) Inst.Ref {
            return @enumFromInt(@intFromEnum(Index.ref_start_index) + @intFromEnum(i));
        }

        pub fn toOptional(i: Index) OptionalIndex {
            return @enumFromInt(@intFromEnum(i));
        }
    };

    pub const OptionalIndex = enum(u32) {
        /// ZIR is structured so that the outermost "main" struct of any file
        /// is always at index 0.
        main_struct_inst = 0,
        ref_start_index = Index.static_len,
        none = std.math.maxInt(u32),
        _,

        pub fn unwrap(oi: OptionalIndex) ?Index {
            return if (oi == .none) null else @enumFromInt(@intFromEnum(oi));
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

    /// This data is stored inside extra, with trailing operands according to `body_len`.
    /// Each operand is an `Index`.
    pub const Block = struct {
        body_len: u32,
    };

    /// Represents a single value being captured in a type declaration's closure.
    pub const Capture = packed struct(u32) {
        tag: enum(u3) {
            /// `data` is a `u16` index into the parent closure.
            nested,
            /// `data` is a `Zir.Inst.Index` to an instruction whose value is being captured.
            instruction,
            /// `data` is a `Zir.Inst.Index` to an instruction representing an alloc whose contents is being captured.
            instruction_load,
            /// `data` is a `NullTerminatedString` to a decl name.
            decl_val,
            /// `data` is a `NullTerminatedString` to a decl name.
            decl_ref,
        },
        data: u29,
        pub const Unwrapped = union(enum) {
            nested: u16,
            instruction: Zir.Inst.Index,
            instruction_load: Zir.Inst.Index,
            decl_val: NullTerminatedString,
            decl_ref: NullTerminatedString,
        };
        pub fn wrap(cap: Unwrapped) Capture {
            return switch (cap) {
                .nested => |idx| .{
                    .tag = .nested,
                    .data = idx,
                },
                .instruction => |inst| .{
                    .tag = .instruction,
                    .data = @intCast(@intFromEnum(inst)),
                },
                .instruction_load => |inst| .{
                    .tag = .instruction_load,
                    .data = @intCast(@intFromEnum(inst)),
                },
                .decl_val => |str| .{
                    .tag = .decl_val,
                    .data = @intCast(@intFromEnum(str)),
                },
                .decl_ref => |str| .{
                    .tag = .decl_ref,
                    .data = @intCast(@intFromEnum(str)),
                },
            };
        }
        pub fn unwrap(cap: Capture) Unwrapped {
            return switch (cap.tag) {
                .nested => .{ .nested = @intCast(cap.data) },
                .instruction => .{ .instruction = @enumFromInt(cap.data) },
                .instruction_load => .{ .instruction_load = @enumFromInt(cap.data) },
                .decl_val => .{ .decl_val = @enumFromInt(cap.data) },
                .decl_ref => .{ .decl_ref = @enumFromInt(cap.data) },
            };
        }
    };

    pub const NameStrategy = enum(u2) {
        /// Use the same name as the parent declaration name.
        /// e.g. `const Foo = struct {...};`.
        parent,
        /// Use the name of the currently executing comptime function call,
        /// with the current parameters. e.g. `ArrayList(i32)`.
        func,
        /// Create an anonymous name for this declaration.
        /// Like this: "ParentDeclName_struct_69"
        anon,
        /// Use the name specified in the next `dbg_var_{val,ptr}` instruction.
        dbg_var,
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
};
