const std = @import("std");
const Io = std.Io;
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;

const kdb = @import("root.zig");
const Ast = kdb.Ast;
const AstGen = kdb.AstGen;
const Zir = kdb.Zir;
const InternPool = kdb.InternPool;

const Vm = @This();

io: Io,
gpa: Allocator,
stdout: *Io.Writer,
code: Zir = undefined,
code_list: std.ArrayList(Zir) = .empty,
stack: std.ArrayList(*KStruct),
state: *KStruct,
string_bytes: std.ArrayList(u8) = .empty,
string_table: std.HashMapUnmanaged(
    u32,
    void,
    std.hash_map.StringIndexContext,
    std.hash_map.default_max_load_percentage,
) = .empty,
unary_primitives: [std.meta.tags(UnaryPrimitive).len]*KStruct = undefined,
operators: [std.meta.tags(Operator).len]*KStruct = undefined,
iterators: [std.meta.tags(Iterator).len]*KStruct = undefined,

pub const Error = Allocator.Error || Io.Writer.Error || error{
    undefined,
    rank,
    projection_NYI,
    domain,
};

pub fn init(io: Io, gpa: Allocator, stdout: *Io.Writer, stack_buffer: []*KStruct) !*Vm {
    const vm = try gpa.create(Vm);
    errdefer gpa.destroy(vm);
    vm.* = .{
        .io = io,
        .gpa = gpa,
        .stdout = stdout,
        .stack = .initBuffer(stack_buffer),
        .state = undefined,
    };

    var unary_primitives_created: usize = 0;
    errdefer for (0..unary_primitives_created) |i| vm.unary_primitives[i].deref(vm.gpa);
    inline for (&vm.unary_primitives, 0..) |*unary_primitive, i| {
        unary_primitive.* = try vm.createUnaryPrimitive(@enumFromInt(i));
        unary_primitives_created += 1;
    }

    var operators_created: usize = 0;
    errdefer for (0..operators_created) |i| vm.operators[i].deref(vm.gpa);
    inline for (&vm.operators, 0..) |*operator, i| {
        operator.* = try vm.createOperator(@enumFromInt(i));
        operators_created += 1;
    }

    var iterators_created: usize = 0;
    errdefer for (0..iterators_created) |i| vm.iterators[i].deref(vm.gpa);
    inline for (&vm.iterators, 0..) |*iterator, i| {
        iterator.* = try vm.createIterator(@enumFromInt(i));
        iterators_created += 1;
    }

    const keys = try vm.createSymbolList(&.{""});
    defer keys.deref(gpa);

    const values = try vm.createList(&.{vm.getUnaryPrimitive(.identity)});
    defer values.deref(gpa);

    vm.state = try vm.createDict(.{
        .keys = keys,
        .values = values,
    });
    errdefer comptime unreachable;

    return vm;
}

pub fn deinit(vm: *Vm) void {
    vm.state.deref(vm.gpa);
    vm.string_table.deinit(vm.gpa);
    vm.string_bytes.deinit(vm.gpa);
    for (vm.unary_primitives) |unary_primitive| unary_primitive.deref(vm.gpa);
    for (vm.operators) |operator| operator.deref(vm.gpa);
    for (vm.iterators) |iterator| iterator.deref(vm.gpa);
    for (vm.code_list.items) |*code| code.deinit(vm.gpa);
    vm.code_list.deinit(vm.gpa);
    vm.gpa.destroy(vm);
}

pub fn exec(vm: *Vm, zir: Zir) !void {
    const code = try zir.clone(vm.gpa);
    try vm.code_list.append(vm.gpa, code);
    vm.code = code;
    const data = vm.code.instData(.file_inst).pl_node;
    const extra = vm.code.extraData(Zir.Inst.Block, data.payload_index);
    const body = vm.code.bodySlice(extra.end, extra.data.body_len);
    if (body.len > 0) {
        for (body) |i| {
            try vm.execInst(i);
        }
    } else {
        vm.stack.appendAssumeCapacity(vm.getUnaryPrimitive(.identity));
    }
    assert(vm.stack.items.len == 1);
}

fn execInst(vm: *Vm, inst: Zir.Inst.Index) Error!void {
    const gpa = vm.gpa;

    try vm.stdout.writeAll("          ");
    for (vm.stack.items) |slot| {
        try vm.stdout.print("[ {t} ]", .{slot.type});
    }
    try vm.stdout.writeByte('\n');
    try vm.stdout.print("{t}\n", .{vm.code.instTag(inst)});
    try vm.stdout.flush();

    switch (vm.code.instTag(inst)) {
        .file => unreachable,

        .print => {
            const data = vm.code.instData(inst).un_node;

            const x = try vm.getRef(data.operand);

            try vm.print(x);
            vm.stack.appendAssumeCapacity(x);
        },

        .lambda => {
            const data = vm.code.instData(inst).lambda;

            const l = vm.code.extraData(Zir.Inst.Lambda, data.payload_index);

            const lambda = try vm.createLambda(.{
                .params_len = @intCast(l.data.params_len),
                .code_index = @intCast(vm.code_list.items.len - 1),
                .extra_index = data.payload_index,
            });
            errdefer comptime unreachable;
            vm.stack.appendAssumeCapacity(lambda);
        },

        .long => {
            const data = vm.code.instData(inst).long;
            const long = try vm.createLong(data);
            errdefer comptime unreachable;
            vm.stack.appendAssumeCapacity(long);
        },

        .sym => {
            const data = vm.code.instData(inst).str_tok;
            const string = data.get(vm.code);
            const symbol = try vm.createSymbol(string);
            errdefer comptime unreachable;
            vm.stack.appendAssumeCapacity(symbol);
        },
        .sym_list => {
            const data = vm.code.instData(inst).pl_node;
            const extra = vm.code.extraData(Zir.Inst.StrList, data.payload_index);
            const slice = vm.code.extraSlice(Zir.NullTerminatedString, extra.end, extra.data.len);
            const symbol_list = try vm.createSymbolListZir(slice);
            errdefer comptime unreachable;
            vm.stack.appendAssumeCapacity(symbol_list);
        },

        .init_identifier => {
            const data = vm.code.instData(inst).str_tok;
            const string = data.get(vm.code);
            const interned_string = try vm.intern(string);

            const items: []*KStruct = @ptrCast(@alignCast(vm.state.as.list));
            const keys = items[0];
            assert(keys.type == .symbol_list);
            const values = items[1];
            assert(values.type == .list);

            const symbol_list: []InternedString = @ptrCast(@alignCast(keys.as.list));
            if (std.mem.findScalar(InternedString, symbol_list, interned_string) == null) {
                try vm.append(keys, InternedString, interned_string);
                errdefer vm.shrink(keys, InternedString, symbol_list.len) catch @panic("OutOfMemory");
                const dummy_value = vm.getUnaryPrimitive(._unused);
                try vm.append(values, *KStruct, dummy_value.ref());
                errdefer comptime unreachable;
            }
        },

        // Handled in `getRef`.
        .identifier => {},

        .list => {
            const data = vm.code.instData(inst).pl_node;
            const extra = vm.code.extraData(Zir.Inst.List, data.payload_index);
            const args = vm.code.extraSlice(Zir.Inst.Ref, extra.end, extra.data.len);

            try vm.list(args);
        },

        .apply => {
            const data = vm.code.instData(inst).pl_node;
            const extra = vm.code.extraData(Zir.Inst.Apply, data.payload_index);
            const args = vm.code.extraSlice(Zir.Inst.Ref, extra.end, extra.data.len);

            const callee = try vm.getRef(extra.data.callee);
            defer callee.deref(gpa);

            // Assignment needs to be handled differently to other applicable values.
            if (callee.type == .operator and @as(Operator, @enumFromInt(callee.as.byte)) == .assign) {
                assert(args.len == 2);
                assert(args[0] != .none and args[1] != .none); // TODO: Requires AstGen changes.

                const string = vm.code.instData(args[0].toIndex().?).str_tok.get(vm.code);
                const interned_string = try vm.intern(string);
                const y = try vm.getRef(args[1]);
                defer y.deref(vm.gpa);
                const value = try vm.assign(interned_string, y);
                vm.stack.appendAssumeCapacity(value);
                return;
            }

            const k_args = blk: {
                var k_args: [AstGen.max_param_len]?*KStruct = undefined;
                var init_count: usize = 0;
                errdefer for (k_args[0..init_count]) |opt_arg| if (opt_arg) |a| a.deref(vm.gpa);
                for (args, 0..) |ref, i| {
                    k_args[i] = try vm.getRefAllowNone(ref);
                    init_count += 1;
                }
                break :blk k_args[0..args.len];
            };
            defer for (k_args) |opt_arg| if (opt_arg) |a| a.deref(vm.gpa);

            try vm.apply(callee, k_args);
        },

        // .param_node => {},
        .param_implicit => {
            const data = vm.code.instData(inst).un_tok;
            const operand = try vm.getRef(data.operand);
            vm.stack.appendAssumeCapacity(operand);
        },

        .ret_node => {
            const data = vm.code.instData(inst).un_node;
            const operand = try vm.getRef(data.operand);
            vm.stack.appendAssumeCapacity(operand);
        },

        .ret_implicit => {
            const data = vm.code.instData(inst).un_tok;
            const operand = try vm.getRef(data.operand);
            vm.stack.appendAssumeCapacity(operand);
        },

        inline else => |t| std.debug.panic("NYI: {t}", .{t}),
    }
}

fn apply(vm: *Vm, callee: *KStruct, args: []const ?*KStruct) Error!void {
    switch (callee.type) {
        .lambda => {
            const lambda: Lambda = @bitCast(callee.as.long);
            if (args.len > lambda.params_len) return error.rank;
            if (args.len < lambda.params_len or std.mem.findScalar(?*KStruct, args, null) != null) {
                var projection_args = std.mem.zeroes([AstGen.max_param_len]?*KStruct);
                for (args, 0..) |opt_arg, i| {
                    if (opt_arg) |a| projection_args[i] = a.ref();
                }
                const projection = try vm.createProjection(.{
                    .callee = callee,
                    .args = projection_args[0..lambda.params_len],
                });
                vm.stack.appendAssumeCapacity(projection);
            } else {
                try vm.applyLambda(lambda, @ptrCast(args));
            }
        },
        .unary_primitive => {
            assert(args.len == 1);
            assert(args[0] != null);

            const unary_primitive: UnaryPrimitive = @enumFromInt(callee.as.byte);
            try vm.applyUnaryPrimitive(unary_primitive, @ptrCast(args[0]));
        },
        .operator => {
            assert(args.len == 2);

            if (args[0] == null or args[1] == null) {
                const projection_args = &.{
                    if (args[0]) |a| a.ref() else null,
                    if (args[1]) |a| a.ref() else null,
                };
                const projection = try vm.createProjection(.{
                    .callee = callee,
                    .args = projection_args,
                });
                vm.stack.appendAssumeCapacity(projection);
            } else {
                const operator: Operator = @enumFromInt(callee.as.byte);
                try vm.applyOperator(operator, @ptrCast(args[0]), @ptrCast(args[1]));
            }
        },
        .iterator => unreachable,
        .projection => {
            const projection: *Projection = @ptrCast(@alignCast(callee.as.list));
            const missing_args = std.mem.countScalar(?*KStruct, projection.args, null);
            if (args.len > missing_args) return error.rank;
            if (args.len < missing_args or std.mem.findScalar(?*KStruct, args, null) != null) {
                var args_i: usize = 0;
                var new_args = std.mem.zeroes([AstGen.max_param_len]?*KStruct);
                for (projection.args, 0..) |opt_arg, i| {
                    if (opt_arg) |a| {
                        new_args[i] = a.ref();
                    } else if (args_i < args.len) {
                        new_args[i] = if (args[args_i]) |a| a.ref() else null;
                        args_i += 1;
                    }
                }
                errdefer for (new_args[0..projection.args.len]) |opt_arg| {
                    if (opt_arg) |a| a.deref(vm.gpa);
                };
                const new_projection = try vm.createProjection(.{
                    .callee = callee,
                    .args = new_args[0..projection.args.len],
                });
                vm.stack.appendAssumeCapacity(new_projection);
            } else {
                try vm.applyProjection(projection, @ptrCast(args));
            }
        },
        .composition => unreachable,
        inline else => |t| std.debug.panic("NYI: {t}", .{t}),
    }
}

fn applyLambda(vm: *Vm, lambda: Lambda, args: []const *KStruct) !void {
    assert(args.len == lambda.params_len);

    const prev_code = vm.code;
    defer vm.code = prev_code;
    vm.code = vm.code_list.items[lambda.code_index];

    const extra = vm.code.extraData(Zir.Inst.Lambda, lambda.extra_index);

    // TODO: init params

    const body = vm.code.bodySlice(extra.end, extra.data.body_len);
    for (body) |inst| try vm.execInst(inst);
}

fn applyUnaryPrimitive(vm: *Vm, unary_primitive: UnaryPrimitive, x: *const KStruct) !void {
    const result = switch (unary_primitive) {
        .first => try vm.firstImpl(x),
        .reverse => try vm.reverseImpl(x),
        .key => try vm.keyImpl(x),
        inline else => |t| std.debug.panic("NYI: {t}", .{t}),
    };
    errdefer comptime unreachable;
    vm.stack.appendAssumeCapacity(result);
}

fn applyOperator(vm: *Vm, operator: Operator, x: *const KStruct, y: *const KStruct) !void {
    const result = switch (operator) {
        .assign => unreachable,
        .add => try vm.add(x, y),
        .subtract => try vm.subtract(x, y),
        .multiply => try vm.multiply(x, y),
        inline else => |t| std.debug.panic("NYI: {t}", .{t}),
    };
    errdefer comptime unreachable;
    vm.stack.appendAssumeCapacity(result);
}

fn applyProjection(vm: *Vm, projection: *const Projection, args: []const *KStruct) !void {
    var args_i: usize = 0;
    var projection_args: [AstGen.max_param_len]*KStruct = undefined;
    for (projection.args, 0..) |opt_arg, i| {
        if (opt_arg) |a| {
            projection_args[i] = a;
        } else {
            projection_args[i] = args[args_i];
            args_i += 1;
        }
    }

    try vm.apply(projection.callee, projection_args[0..projection.args.len]);
}

fn write(vm: *Vm, w: *Io.Writer, x: *const KStruct) !void {
    switch (x.type) {
        .long => try w.print("{d}", .{x.as.long}),
        .long_list => {
            const longs: []i64 = @ptrCast(@alignCast(x.as.list));
            try w.print("{d}", .{longs[0]});
            for (longs[1..]) |l| {
                try w.print(" {d}", .{l});
            }
        },
        .symbol => try w.print("`{s}", .{vm.internedString(x.as.symbol)}),
        .symbol_list => {
            const symbols: []InternedString = @ptrCast(@alignCast(x.as.list));
            for (symbols) |s| try w.print("`{s}", .{vm.internedString(s)});
        },
        .lambda => {
            const lambda: Lambda = @bitCast(x.as.long);
            const code = vm.code_list.items[lambda.code_index];
            const extra = code.extraData(Zir.Inst.Lambda, lambda.extra_index);
            const src_locs = code.extraData(Zir.Inst.Lambda.SrcLocs, extra.end + extra.data.body_len);
            const source = code.nullTerminatedString(src_locs.data.source);
            try w.print("{s}", .{source});
        },
        .unary_primitive => {
            const unary_primitive: UnaryPrimitive = @enumFromInt(x.as.byte);
            try w.print("{f}", .{unary_primitive});
        },
        .operator => {
            const operator: Operator = @enumFromInt(x.as.byte);
            try w.print("{f}", .{operator});
        },
        .iterator => {
            const iterator: Iterator = @enumFromInt(x.as.byte);
            try w.print("{f}", .{iterator});
        },
        .projection => {
            const projection: *Projection = @ptrCast(@alignCast(x.as.list));
            try vm.write(vm.stdout, projection.callee);
            try vm.stdout.writeByte('[');
            for (projection.args[0 .. projection.args.len - 1]) |opt_arg| {
                if (opt_arg) |a| try vm.write(vm.stdout, a);
                try vm.stdout.writeByte(';');
            }
            if (projection.args[projection.args.len - 1]) |arg| try vm.write(vm.stdout, arg);
            try vm.stdout.writeByte(']');
        },
        inline else => |t| std.debug.panic("NYI: {t}", .{t}),
    }
}

fn print(vm: *Vm, x: *const KStruct) !void {
    switch (x.type) {
        .long => try vm.stdout.print("{d}\n", .{x.as.long}),
        .long_list => {
            const longs: []i64 = @ptrCast(@alignCast(x.as.list));
            try vm.stdout.print("{d}", .{longs[0]});
            for (longs[1..]) |l| {
                try vm.stdout.print(" {d}", .{l});
            }
            try vm.stdout.writeByte('\n');
        },
        .symbol => try vm.stdout.print("`{s}\n", .{vm.internedString(x.as.symbol)}),
        .symbol_list => {
            const symbols: []InternedString = @ptrCast(@alignCast(x.as.list));
            for (symbols) |s| try vm.stdout.print("`{s}", .{vm.internedString(s)});
            try vm.stdout.writeByte('\n');
        },
        .lambda => {
            const lambda: Lambda = @bitCast(x.as.long);
            const code = vm.code_list.items[lambda.code_index];
            const extra = code.extraData(Zir.Inst.Lambda, lambda.extra_index);
            const src_locs = code.extraData(Zir.Inst.Lambda.SrcLocs, extra.end + extra.data.body_len);
            const source = code.nullTerminatedString(src_locs.data.source);
            try vm.stdout.print("{s}\n", .{source});
        },
        .unary_primitive => {
            const unary_primitive: UnaryPrimitive = @enumFromInt(x.as.byte);
            try vm.stdout.print("{f}\n", .{unary_primitive});
        },
        .operator => {
            const operator: Operator = @enumFromInt(x.as.byte);
            try vm.stdout.print("{f}\n", .{operator});
        },
        .iterator => {
            const iterator: Iterator = @enumFromInt(x.as.byte);
            try vm.stdout.print("{f}\n", .{iterator});
        },
        .projection => {
            const projection: *Projection = @ptrCast(@alignCast(x.as.list));
            try vm.write(vm.stdout, projection.callee);
            try vm.stdout.writeByte('[');
            for (projection.args[0 .. projection.args.len - 1]) |opt_arg| {
                if (opt_arg) |a| try vm.write(vm.stdout, a);
                try vm.stdout.writeByte(';');
            }
            if (projection.args[projection.args.len - 1]) |arg| try vm.write(vm.stdout, arg);
            try vm.stdout.writeAll("]\n");
        },
        inline else => |t| std.debug.panic("NYI: {t}", .{t}),
    }
    try vm.stdout.flush();
}

fn getRefAllowNone(vm: *Vm, ref: Zir.Inst.Ref) !?*KStruct {
    if (ref == .none) return null;
    return vm.getRef(ref);
}

fn getRef(vm: *Vm, ref: Zir.Inst.Ref) !*KStruct {
    assert(ref != .none);
    if (ref.toIndex()) |inst| {
        switch (vm.code.instTag(inst)) {
            .init_identifier, .identifier => {
                const data = vm.code.instData(inst).str_tok;
                const string = data.get(vm.code);
                const interned_string = try vm.intern(string);

                const items: []*KStruct = @ptrCast(@alignCast(vm.state.as.list));
                const keys = items[0];
                assert(keys.type == .symbol_list);
                const values = items[1];
                assert(values.type == .list);

                const symbol_list: []InternedString = @ptrCast(@alignCast(keys.as.list));
                if (std.mem.findScalar(InternedString, symbol_list, interned_string)) |index| {
                    const value_items: []*KStruct = @ptrCast(@alignCast(values.as.list));
                    return value_items[index].ref();
                } else {
                    return error.undefined;
                }
            },
            else => {},
        }
        return vm.stack.pop().?;
    } else {
        const val: InternPool.Index = @enumFromInt(@intFromEnum(ref));
        return switch (val) {
            .true => try vm.createBoolean(true),
            .false => try vm.createBoolean(false),
            .zero => try vm.createLong(0),
            .one => try vm.createLong(1),
            .negative_one => try vm.createLong(-1),
            .empty_list => try vm.createList(&.{}),
            .x => unreachable,
            .y => unreachable,
            .z => unreachable,

            .identity => vm.getUnaryPrimitive(.identity).ref(),
            .flip => vm.getUnaryPrimitive(.flip).ref(),
            .neg => vm.getUnaryPrimitive(.neg).ref(),
            .first => vm.getUnaryPrimitive(.first).ref(),
            .reciprocal => vm.getUnaryPrimitive(.reciprocal).ref(),
            .where => vm.getUnaryPrimitive(.where).ref(),
            .reverse => vm.getUnaryPrimitive(.reverse).ref(),
            .null => vm.getUnaryPrimitive(.null).ref(),
            .group => vm.getUnaryPrimitive(.group).ref(),
            .asc => vm.getUnaryPrimitive(.asc).ref(),
            .desc => vm.getUnaryPrimitive(.desc).ref(),
            .string => vm.getUnaryPrimitive(.string).ref(),
            .list => vm.getUnaryPrimitive(.list).ref(),
            .count => vm.getUnaryPrimitive(.count).ref(),
            .lower => vm.getUnaryPrimitive(.lower).ref(),
            .not => vm.getUnaryPrimitive(.not).ref(),
            .key => vm.getUnaryPrimitive(.key).ref(),
            .distinct => vm.getUnaryPrimitive(.distinct).ref(),
            .type => vm.getUnaryPrimitive(.type).ref(),
            .value => vm.getUnaryPrimitive(.value).ref(),
            .read_text => vm.getUnaryPrimitive(.read_text).ref(),
            .read_binary => vm.getUnaryPrimitive(.read_binary).ref(),
            .avg => vm.getUnaryPrimitive(.avg).ref(),
            .last => vm.getUnaryPrimitive(.last).ref(),
            .sum => vm.getUnaryPrimitive(.sum).ref(),
            .prd => vm.getUnaryPrimitive(.prd).ref(),
            .min => vm.getUnaryPrimitive(.min).ref(),
            .max => vm.getUnaryPrimitive(.max).ref(),
            .exit => vm.getUnaryPrimitive(.exit).ref(),
            .getenv => vm.getUnaryPrimitive(.getenv).ref(),
            .abs => vm.getUnaryPrimitive(.abs).ref(),
            .sqrt => vm.getUnaryPrimitive(.sqrt).ref(),
            .log => vm.getUnaryPrimitive(.log).ref(),
            .exp => vm.getUnaryPrimitive(.exp).ref(),
            .sin => vm.getUnaryPrimitive(.sin).ref(),
            .asin => vm.getUnaryPrimitive(.asin).ref(),
            .cos => vm.getUnaryPrimitive(.cos).ref(),
            .acos => vm.getUnaryPrimitive(.acos).ref(),
            .tan => vm.getUnaryPrimitive(.tan).ref(),
            .atan => vm.getUnaryPrimitive(.atan).ref(),
            .enlist => vm.getUnaryPrimitive(.enlist).ref(),
            .@"var" => vm.getUnaryPrimitive(.@"var").ref(),
            .dev => vm.getUnaryPrimitive(.dev).ref(),
            .hopen => vm.getUnaryPrimitive(.hopen).ref(),

            .assign => vm.getOperator(.assign).ref(),
            .add => vm.getOperator(.add).ref(),
            .subtract => vm.getOperator(.subtract).ref(),
            .multiply => vm.getOperator(.multiply).ref(),
            .divide => vm.getOperator(.divide).ref(),
            .@"and" => vm.getOperator(.@"and").ref(),
            .@"or" => vm.getOperator(.@"or").ref(),
            .fill => vm.getOperator(.fill).ref(),
            .equals => vm.getOperator(.equals).ref(),
            .less_than => vm.getOperator(.less_than).ref(),
            .less_than_or_equal => unreachable,
            .not_equal => unreachable,
            .greater_than => vm.getOperator(.greater_than).ref(),
            .greater_than_or_equal => unreachable,
            .cast => vm.getOperator(.cast).ref(),
            .join => vm.getOperator(.join).ref(),
            .take => vm.getOperator(.take).ref(),
            .drop => vm.getOperator(.drop).ref(),
            .match => vm.getOperator(.match).ref(),
            .dict => vm.getOperator(.dict).ref(),
            .find => vm.getOperator(.find).ref(),
            .apply_at => vm.getOperator(.apply_at).ref(),
            .apply => vm.getOperator(.apply).ref(),
            .file_text => vm.getOperator(.file_text).ref(),
            .file_binary => vm.getOperator(.file_binary).ref(),
            .dynamic_load => vm.getOperator(.dynamic_load).ref(),
            .in => vm.getOperator(.in).ref(),
            .within => vm.getOperator(.within).ref(),
            .like => vm.getOperator(.like).ref(),
            .bin => vm.getOperator(.bin).ref(),
            .ss => vm.getOperator(.ss).ref(),
            .insert => vm.getOperator(.insert).ref(),
            .wsum => vm.getOperator(.wsum).ref(),
            .wavg => vm.getOperator(.wavg).ref(),
            .div => vm.getOperator(.div).ref(),
            .xexp => vm.getOperator(.xexp).ref(),
            .setenv => vm.getOperator(.setenv).ref(),
            .binr => vm.getOperator(.binr).ref(),
            .cov => vm.getOperator(.cov).ref(),
            .cor => vm.getOperator(.cor).ref(),

            .each => vm.getIterator(.each).ref(),
            .each_prior => vm.getIterator(.each_prior).ref(),
            .over => vm.getIterator(.over).ref(),
            .each_right => vm.getIterator(.each_right).ref(),
            .scan => vm.getIterator(.scan).ref(),
            .each_left => vm.getIterator(.each_left).ref(),

            inline else => |t| std.debug.panic("NYI: {t}", .{t}),
        };
    }
}

fn getUnaryPrimitive(vm: *Vm, unary_primitive: UnaryPrimitive) *KStruct {
    return vm.unary_primitives[@intFromEnum(unary_primitive)];
}

fn getOperator(vm: *Vm, operator: Operator) *KStruct {
    return vm.operators[@intFromEnum(operator)];
}

fn getIterator(vm: *Vm, iterator: Iterator) *KStruct {
    return vm.iterators[@intFromEnum(iterator)];
}

fn firstImpl(vm: *Vm, x: *const KStruct) !*KStruct {
    return switch (x.type) {
        .long_list => {
            const longs: []i64 = @ptrCast(@alignCast(x.as.list));
            return try vm.createLong(longs[0]);
        },
        inline else => |t| std.debug.panic("NYI: {t}", .{t}),
    };
}

fn reverseImpl(vm: *Vm, x: *const KStruct) !*KStruct {
    return switch (x.type) {
        .long_list => blk: {
            const long_list = try vm.createLongList(@ptrCast(@alignCast(x.as.list)));
            errdefer comptime unreachable;
            std.mem.reverse(i64, @ptrCast(@alignCast(long_list.as.list)));
            break :blk long_list;
        },
        inline else => |t| std.debug.panic("NYI: {t}", .{t}),
    };
}

fn keyImpl(vm: *Vm, x: *const KStruct) !*KStruct {
    return switch (x.type) {
        .long => blk: {
            if (x.as.long < 0) return error.domain;
            const items = try vm.gpa.alloc(i64, @intCast(x.as.long));
            defer vm.gpa.free(items);
            for (items, 0..) |*item, i| item.* = @intCast(i);
            break :blk try vm.createLongList(items);
        },
        inline else => |t| std.debug.panic("NYI: {t}", .{t}),
    };
}

fn assign(vm: *Vm, x: InternedString, y: *KStruct) !*KStruct {
    const items: []*KStruct = @ptrCast(@alignCast(vm.state.as.list));
    const keys = items[0];
    assert(keys.type == .symbol_list);
    const values = items[1];
    assert(values.type == .list);

    const symbol_list: []InternedString = @ptrCast(@alignCast(keys.as.list));
    const index = std.mem.findScalar(InternedString, symbol_list, x).?;
    const value_items: []*KStruct = @ptrCast(@alignCast(values.as.list));

    value_items[index].deref(vm.gpa);
    value_items[index] = y.ref();
    return y.ref();
}

fn add(vm: *Vm, x: *const KStruct, y: *const KStruct) !*KStruct {
    return switch (x.type) {
        .long => switch (y.type) {
            .long => try vm.createLong(x.as.long + y.as.long),
            inline else => |t| std.debug.panic("NYI: {t}", .{t}),
        },
        inline else => |t| std.debug.panic("NYI: {t}", .{t}),
    };
}

fn subtract(vm: *Vm, x: *const KStruct, y: *const KStruct) !*KStruct {
    return switch (x.type) {
        .long => switch (y.type) {
            .long => try vm.createLong(x.as.long - y.as.long),
            inline else => |t| std.debug.panic("NYI: {t}", .{t}),
        },
        inline else => |t| std.debug.panic("NYI: {t}", .{t}),
    };
}

fn multiply(vm: *Vm, x: *const KStruct, y: *const KStruct) !*KStruct {
    return switch (x.type) {
        .long => switch (y.type) {
            .long => try vm.createLong(x.as.long * y.as.long),
            inline else => |t| std.debug.panic("NYI: {t}", .{t}),
        },
        inline else => |t| std.debug.panic("NYI: {t}", .{t}),
    };
}

// TODO: list projection (1;;3)
fn list(vm: *Vm, refs: []const Zir.Inst.Ref) !void {
    var data: std.ArrayList(i64) = try .initCapacity(vm.gpa, refs.len);
    defer data.deinit(vm.gpa);

    for (refs) |ref| {
        var value = try vm.getRef(ref);
        defer value.deref(vm.gpa);
        assert(value.type == .long);
        data.appendAssumeCapacity(value.as.long);
    }

    const long_list = try vm.createLongList(data.items);
    errdefer comptime unreachable;
    vm.stack.appendAssumeCapacity(long_list);
}

const InternedString = enum(u32) {
    empty = 0,
    _,
};

fn intern(vm: *Vm, value: []const u8) !InternedString {
    const str_index: u32 = @intCast(vm.string_bytes.items.len);
    try vm.string_bytes.appendSlice(vm.gpa, value);
    const key: []const u8 = vm.string_bytes.items[str_index..];
    const gop = try vm.string_table.getOrPutContextAdapted(
        vm.gpa,
        key,
        std.hash_map.StringIndexAdapter{ .bytes = &vm.string_bytes },
        std.hash_map.StringIndexContext{ .bytes = &vm.string_bytes },
    );
    if (gop.found_existing) {
        vm.string_bytes.shrinkRetainingCapacity(str_index);
        return @enumFromInt(gop.key_ptr.*);
    } else {
        gop.key_ptr.* = str_index;
        try vm.string_bytes.append(vm.gpa, 0);
        return @enumFromInt(str_index);
    }
}

pub fn internedString(vm: *Vm, index: InternedString) [:0]const u8 {
    const slice = vm.string_bytes.items[@intFromEnum(index)..];
    return slice[0..std.mem.findScalar(u8, slice, 0).? :0];
}

pub fn createList(vm: *Vm, value: []const *KStruct) !*KStruct {
    const self = try vm.gpa.create(KStruct);
    errdefer vm.gpa.destroy(self);
    const items = try vm.gpa.dupe(u8, @ptrCast(value));
    errdefer comptime unreachable;
    for (value) |v| _ = v.ref();
    self.* = .{ .type = .list, .as = .{ .list = items } };
    return self;
}

pub fn createBoolean(vm: *Vm, value: bool) !*KStruct {
    const self = try vm.gpa.create(KStruct);
    errdefer comptime unreachable;
    self.* = .{ .type = .boolean, .as = .{ .byte = @intFromBool(value) } };
    return self;
}

pub fn createGuid(vm: *Vm, value: [16]u8) !*KStruct {
    const self = try vm.gpa.create(KStruct);
    errdefer vm.gpa.destroy(self);
    const items = try vm.gpa.dupe(u8, value);
    errdefer comptime unreachable;
    self.* = .{ .type = .guid, .as = .{ .list = items } };
    return self;
}

pub fn createByte(vm: *Vm, value: u8) !*KStruct {
    const self = try vm.gpa.create(KStruct);
    errdefer comptime unreachable;
    self.* = .{ .type = .byte, .as = .{ .byte = value } };
    return self;
}

pub fn createShort(vm: *Vm, value: i16) !*KStruct {
    const self = try vm.gpa.create(KStruct);
    errdefer comptime unreachable;
    self.* = .{ .type = .short, .as = .{ .short = value } };
    return self;
}

pub fn createInt(vm: *Vm, value: i32) !*KStruct {
    const self = try vm.gpa.create(KStruct);
    errdefer comptime unreachable;
    self.* = .{ .type = .int, .as = .{ .int = value } };
    return self;
}

pub fn createLong(vm: *Vm, value: i64) !*KStruct {
    const self = try vm.gpa.create(KStruct);
    errdefer comptime unreachable;
    self.* = .{ .type = .long, .as = .{ .long = value } };
    return self;
}

pub fn createLongList(vm: *Vm, value: []const i64) !*KStruct {
    const self = try vm.gpa.create(KStruct);
    errdefer vm.gpa.destroy(self);
    const items = try vm.gpa.dupe(u8, @ptrCast(value));
    errdefer comptime unreachable;
    self.* = .{ .type = .long_list, .as = .{ .list = items } };
    return self;
}

pub fn createReal(vm: *Vm, value: f32) !*KStruct {
    const self = try vm.gpa.create(KStruct);
    errdefer comptime unreachable;
    self.* = .{ .type = .real, .as = .{ .real = value } };
    return self;
}

pub fn createFloat(vm: *Vm, value: f64) !*KStruct {
    const self = try vm.gpa.create(KStruct);
    errdefer comptime unreachable;
    self.* = .{ .type = .float, .as = .{ .float = value } };
    return self;
}

pub fn createChar(vm: *Vm, value: u8) !*KStruct {
    const self = try vm.gpa.create(KStruct);
    errdefer comptime unreachable;
    self.* = .{ .type = .char, .as = .{ .byte = value } };
    return self;
}

pub fn createSymbol(vm: *Vm, value: []const u8) !*KStruct {
    const index = try vm.intern(value);
    const self = try vm.gpa.create(KStruct);
    errdefer comptime unreachable;
    self.* = .{ .type = .symbol, .as = .{ .symbol = index } };
    return self;
}

pub fn createSymbolList(vm: *Vm, value: []const []const u8) !*KStruct {
    const self = try vm.gpa.create(KStruct);
    errdefer vm.gpa.destroy(self);
    const items = try vm.gpa.alloc(u8, @sizeOf(InternedString) * value.len);
    errdefer vm.gpa.free(items);
    const symbol_list: []InternedString = @ptrCast(@alignCast(items));
    for (symbol_list, value) |*sym, bytes| sym.* = try vm.intern(bytes);
    self.* = .{ .type = .symbol_list, .as = .{ .list = items } };
    return self;
}

pub fn createSymbolListZir(vm: *Vm, value: []const Zir.NullTerminatedString) !*KStruct {
    const self = try vm.gpa.create(KStruct);
    errdefer vm.gpa.destroy(self);
    const items = try vm.gpa.alloc(u8, @sizeOf(InternedString) * value.len);
    errdefer vm.gpa.free(items);
    const symbol_list: []InternedString = @ptrCast(@alignCast(items));
    for (symbol_list, value) |*sym, index| {
        const bytes = vm.code.nullTerminatedString(index);
        sym.* = try vm.intern(bytes);
    }
    self.* = .{ .type = .symbol_list, .as = .{ .list = items } };
    return self;
}

pub fn createTimestamp(vm: *Vm, value: i64) !*KStruct {
    const self = try vm.gpa.create(KStruct);
    errdefer comptime unreachable;
    self.* = .{ .type = .timestamp, .as = .{ .long = value } };
    return self;
}

pub fn createMonth(vm: *Vm, value: i32) !*KStruct {
    const self = try vm.gpa.create(KStruct);
    errdefer comptime unreachable;
    self.* = .{ .type = .month, .as = .{ .int = value } };
    return self;
}

pub fn createDate(vm: *Vm, value: i32) !*KStruct {
    const self = try vm.gpa.create(KStruct);
    errdefer comptime unreachable;
    self.* = .{ .type = .date, .as = .{ .int = value } };
    return self;
}

pub fn createDatetime(vm: *Vm, value: f64) !*KStruct {
    const self = try vm.gpa.create(KStruct);
    errdefer comptime unreachable;
    self.* = .{ .type = .datetime, .as = .{ .float = value } };
    return self;
}

pub fn createTimespan(vm: *Vm, value: i64) !*KStruct {
    const self = try vm.gpa.create(KStruct);
    errdefer comptime unreachable;
    self.* = .{ .type = .timespan, .as = .{ .long = value } };
    return self;
}

pub fn createMinute(vm: *Vm, value: i32) !*KStruct {
    const self = try vm.gpa.create(KStruct);
    errdefer comptime unreachable;
    self.* = .{ .type = .minute, .as = .{ .int = value } };
    return self;
}

pub fn createSecond(vm: *Vm, value: i32) !*KStruct {
    const self = try vm.gpa.create(KStruct);
    errdefer comptime unreachable;
    self.* = .{ .type = .second, .as = .{ .int = value } };
    return self;
}

pub fn createTime(vm: *Vm, value: i32) !*KStruct {
    const self = try vm.gpa.create(KStruct);
    errdefer comptime unreachable;
    self.* = .{ .type = .time, .as = .{ .int = value } };
    return self;
}

pub fn createTable(vm: *Vm) !*KStruct {
    _ = vm; // autofix
    return error.NYI;
}

const Dict = struct {
    keys: *KStruct,
    values: *KStruct,
};

pub fn createDict(vm: *Vm, dict: Dict) !*KStruct {
    const self = try vm.gpa.create(KStruct);
    errdefer vm.gpa.destroy(self);
    var d = dict;
    _ = d.keys.ref();
    _ = d.values.ref();
    const items = try vm.gpa.dupe(u8, @ptrCast(&dict));
    errdefer comptime unreachable;
    self.* = .{ .type = .dict, .as = .{ .list = items } };
    return self;
}

const Lambda = packed struct(i64) {
    params_len: u4,
    code_index: u28,
    extra_index: u32,

    comptime {
        assert(std.math.maxInt(@FieldType(@This(), "params_len")) >= AstGen.max_param_len);
    }
};

pub fn createLambda(vm: *Vm, lambda: Lambda) !*KStruct {
    const self = try vm.gpa.create(KStruct);
    errdefer comptime unreachable;
    self.* = .{ .type = .lambda, .as = .{ .long = @bitCast(lambda) } };
    return self;
}

pub fn createUnaryPrimitive(vm: *Vm, value: UnaryPrimitive) !*KStruct {
    const self = try vm.gpa.create(KStruct);
    errdefer comptime unreachable;
    self.* = .{ .type = .unary_primitive, .as = .{ .byte = @intFromEnum(value) } };
    return self;
}

pub fn createOperator(vm: *Vm, value: Operator) !*KStruct {
    const self = try vm.gpa.create(KStruct);
    errdefer comptime unreachable;
    self.* = .{ .type = .operator, .as = .{ .byte = @intFromEnum(value) } };
    return self;
}

pub fn createIterator(vm: *Vm, value: Iterator) !*KStruct {
    const self = try vm.gpa.create(KStruct);
    errdefer comptime unreachable;
    self.* = .{ .type = .iterator, .as = .{ .byte = @intFromEnum(value) } };
    return self;
}

const Projection = struct {
    callee: *KStruct,
    args: []const ?*KStruct,
};

pub fn createProjection(vm: *Vm, projection: Projection) !*KStruct {
    const self = try vm.gpa.create(KStruct);
    errdefer vm.gpa.destroy(self);
    var p = projection;
    _ = p.callee.ref();
    p.args = try vm.gpa.dupe(?*KStruct, p.args);
    errdefer vm.gpa.free(p.args);
    const bytes = try vm.gpa.dupe(u8, @ptrCast(&p));
    errdefer comptime unreachable;
    self.* = .{ .type = .projection, .as = .{ .list = bytes } };
    return self;
}

pub fn append(vm: *Vm, k: *KStruct, comptime T: type, value: T) !void {
    if (vm.gpa.remap(k.as.list, k.as.list.len + @sizeOf(T))) |new_memory| {
        const items: []T = @ptrCast(@alignCast(new_memory));
        items[items.len - 1] = value;
        k.as.list = new_memory;
    } else {
        const new_memory = try vm.gpa.alloc(u8, k.as.list.len + @sizeOf(T));
        @memcpy(new_memory[0..k.as.list.len], k.as.list);
        vm.gpa.free(k.as.list);
        const items: []T = @ptrCast(@alignCast(new_memory));
        items[items.len - 1] = value;
        k.as.list = new_memory;
    }
}

pub fn shrink(vm: *Vm, k: *KStruct, comptime T: type, new_len: usize) !void {
    const byte_len = new_len * @sizeOf(T);
    assert(byte_len <= k.as.list.len);
    if (vm.gpa.remap(k.as.list, byte_len)) |new_memory| {
        k.as.list = new_memory;
    } else {
        const new_memory = try vm.gpa.alloc(u8, byte_len);
        @memcpy(new_memory, k.as.list[0..byte_len]);
        vm.gpa.free(k.as.list);
        k.as.list = new_memory;
    }
}

pub const KStruct = struct {
    m: i8 = undefined,
    a: i8 = undefined,
    type: Type,
    attr: Attr = .none,
    ref_count: i32 = 0,
    as: Union,

    comptime {
        const expected_size = switch (@import("builtin").mode) {
            .Debug, .ReleaseSafe => 32,
            .ReleaseFast, .ReleaseSmall => 24,
        };
        assert(@sizeOf(@This()) == expected_size);
    }

    pub fn ref(self: *KStruct) *KStruct {
        self.ref_count += 1;
        return self;
    }

    pub fn deref(self: *KStruct, gpa: Allocator) void {
        if (self.ref_count > 0) {
            self.ref_count -= 1;
        } else {
            switch (self.type) {
                .list, .dict => {
                    const items: []*KStruct = @ptrCast(@alignCast(self.as.list));
                    for (items) |k| k.deref(gpa);
                    gpa.free(self.as.list);
                },

                .boolean,
                .byte,
                .short,
                .int,
                .long,
                .real,
                .float,
                .char,
                .symbol,
                .timestamp,
                .month,
                .date,
                .datetime,
                .timespan,
                .minute,
                .second,
                .time,
                .lambda,
                => {},

                .guid,
                .boolean_list,
                .guid_list,
                .byte_list,
                .short_list,
                .int_list,
                .long_list,
                .real_list,
                .float_list,
                .char_list,
                .symbol_list,
                .timestamp_list,
                .month_list,
                .date_list,
                .datetime_list,
                .timespan_list,
                .minute_list,
                .second_list,
                .time_list,
                => gpa.free(self.as.list),

                .table => self.as.table.deref(gpa),

                .unary_primitive,
                .operator,
                .iterator,
                .composition,
                => {},

                .projection => {
                    const projection: *Projection = @ptrCast(@alignCast(self.as.list));
                    projection.callee.deref(gpa);
                    for (projection.args) |opt_arg| if (opt_arg) |a| a.deref(gpa);
                    gpa.free(projection.args);
                    gpa.free(self.as.list);
                },
            }
            gpa.destroy(self);
        }
    }
};
const Union = union {
    byte: u8,
    short: i16,
    int: i32,
    long: i64,
    real: f32,
    float: f64,
    symbol: InternedString,
    table: *KStruct,
    list: []u8,
};
pub const Type = enum(i8) {
    list = 0,
    boolean = -1,
    boolean_list = 1,
    guid = -2,
    guid_list = 2,
    byte = -4,
    byte_list = 4,
    short = -5,
    short_list = 5,
    int = -6,
    int_list = 6,
    long = -7,
    long_list = 7,
    real = -8,
    real_list = 8,
    float = -9,
    float_list = 9,
    char = -10,
    char_list = 10,
    symbol = -11,
    symbol_list = 11,
    timestamp = -12,
    timestamp_list = 12,
    month = -13,
    month_list = 13,
    date = -14,
    date_list = 14,
    datetime = -15,
    datetime_list = 15,
    timespan = -16,
    timespan_list = 16,
    minute = -17,
    minute_list = 17,
    second = -18,
    second_list = 18,
    time = -19,
    time_list = 19,
    table = 98,
    dict = 99,
    lambda = 100,
    unary_primitive = 101,
    operator = 102,
    iterator = 103,
    projection = 104,
    composition = 105,
};
pub const Attr = enum(u8) {
    none = 0,
    sorted = 1,
    unique = 2,
    parted = 3,
    grouped = 4,
};
// :: +: -: *: %: &: |: ^: =: <: >: $: ,: #: _: ~: !: ?: @: .: 0:: 1:: 2::
// avg last sum prd min max exit getenv abs sqrt log exp sin asin cos acos tan atan enlist var dev hopen
pub const UnaryPrimitive = enum(u8) {
    identity,
    flip,
    neg,
    first,
    reciprocal,
    where,
    reverse,
    null,
    group,
    asc,
    desc,
    string,
    list,
    count,
    lower,
    not,
    key,
    distinct,
    type,
    value,
    read_text,
    read_binary,
    _unused,
    avg,
    last,
    sum,
    prd,
    min,
    max,
    exit,
    getenv,
    abs,
    sqrt,
    log,
    exp,
    sin,
    asin,
    cos,
    acos,
    tan,
    atan,
    enlist,
    @"var",
    dev,
    hopen,

    pub fn format(self: UnaryPrimitive, w: *Io.Writer) !void {
        try w.writeAll(@tagName(self));
    }
};
// : + - * % & | ^ = < > $ , # _ ~ ! ? @ . 0: 1: 2:
// in within like bin ss insert wsum wavg div xexp setenv binr cov cor
pub const Operator = enum(u8) {
    assign,
    add,
    subtract,
    multiply,
    divide,
    @"and",
    @"or",
    fill,
    equals,
    less_than,
    greater_than,
    cast,
    join,
    take,
    drop,
    match,
    dict,
    find,
    apply_at,
    apply,
    file_text,
    file_binary,
    dynamic_load,
    in,
    within,
    like,
    bin,
    ss,
    insert,
    wsum,
    wavg,
    div,
    xexp,
    setenv,
    binr,
    cov,
    cor,

    pub fn format(self: Operator, w: *Io.Writer) !void {
        switch (self) {
            .assign => try w.writeAll("TODO"),
            .add => try w.writeByte('+'),
            .subtract => try w.writeByte('-'),
            .multiply => try w.writeByte('*'),
            .divide => try w.writeByte('%'),
            .@"and" => try w.writeByte('&'),
            .@"or" => try w.writeByte('|'),
            .fill => try w.writeByte('^'),
            .equals => try w.writeByte('='),
            .less_than => try w.writeByte('<'),
            .greater_than => try w.writeByte('>'),
            .cast => try w.writeByte('$'),
            .join => try w.writeByte(','),
            .take => try w.writeByte('#'),
            .drop => try w.writeByte('_'),
            .match => try w.writeByte('~'),
            .dict => try w.writeByte('!'),
            .find => try w.writeByte('?'),
            .apply_at => try w.writeByte('@'),
            .apply => try w.writeByte('.'),
            .file_text => try w.writeAll("0:"),
            .file_binary => try w.writeAll("1:"),
            .dynamic_load => try w.writeAll("2:"),
            inline else => |t| try w.writeAll(@tagName(t)),
        }
    }
};
// ' / \ ': /: \:
pub const Iterator = enum(u8) {
    each,
    over,
    scan,
    each_prior,
    each_right,
    each_left,

    pub fn format(self: Iterator, w: *Io.Writer) !void {
        switch (self) {
            .each => try w.writeByte('\''),
            .over => try w.writeByte('/'),
            .scan => try w.writeByte('\\'),
            .each_prior => try w.writeAll("':"),
            .each_right => try w.writeAll("/:"),
            .each_left => try w.writeAll("\\:"),
        }
    }
};
