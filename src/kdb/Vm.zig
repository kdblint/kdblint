const std = @import("std");
const Io = std.Io;
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;

const kdb = @import("root.zig");
const Zir = kdb.Zir;
const InternPool = kdb.InternPool;

const Vm = @This();

io: Io,
gpa: Allocator,
stdout: *Io.Writer,
code: Zir,
stack: std.ArrayList(KStruct),

pub fn init(io: Io, gpa: Allocator, stdout: *Io.Writer, stack_buffer: []KStruct) Vm {
    return .{
        .io = io,
        .gpa = gpa,
        .stdout = stdout,
        .code = undefined,
        .stack = .initBuffer(stack_buffer),
    };
}

pub fn deinit(vm: *Vm) void {
    _ = vm; // autofix
}

pub fn exec(vm: *Vm, zir: Zir) !void {
    vm.code = zir;
    try vm.execInst(.file_inst);
}

fn execInst(vm: *Vm, inst: Zir.Inst.Index) !void {
    switch (vm.code.instTag(inst)) {
        .file => {
            const data = vm.code.instData(inst).pl_node;
            const extra = vm.code.extraData(Zir.Inst.Block, data.payload_index);
            const body = vm.code.bodySlice(extra.end, extra.data.body_len);
            for (body) |i| {
                try vm.execInst(i);
            }
        },

        .print => {
            const data = vm.code.instData(inst).un_node;

            try vm.print(data.operand);
        },

        .long => {
            const data = vm.code.instData(inst).long;
            vm.stack.appendAssumeCapacity(.{ .type = .long, .as = .{ .long = data } });
        },

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

            const ref = extra.data.callee;
            if (ref == .none) {
                unreachable;
            } else if (ref.toIndex()) |i| {
                _ = i; // autofix
                unreachable;
            } else {
                const val: InternPool.Index = @enumFromInt(@intFromEnum(ref));
                switch (val) {
                    .add => try vm.add(args),
                    .subtract => try vm.subtract(args),
                    inline else => |t| std.debug.panic("NYI: {t}", .{t}),
                }
            }
        },

        inline else => |t| std.debug.panic("NYI: {t}", .{t}),
    }
}

fn print(vm: *Vm, ref: Zir.Inst.Ref) !void {
    const x = vm.getRef(ref);
    defer vm.stack.appendAssumeCapacity(x);

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
        inline else => |t| std.debug.panic("NYI: {t}", .{t}),
    }
    try vm.stdout.flush();
}

fn getRef(vm: *Vm, ref: Zir.Inst.Ref) KStruct {
    if (ref == .none) {
        unreachable;
    } else if (ref.toIndex()) |_| {
        return vm.stack.pop().?;
    } else {
        const val: InternPool.Index = @enumFromInt(@intFromEnum(ref));
        return switch (val) {
            .true => .true,
            .false => .false,
            .zero => .zero,
            .one => .one,
            .negative_one => .negative_one,
            .empty_list => .empty_list,
            inline else => |t| std.debug.panic("NYI: {t}", .{t}),
        };
    }
}

fn add(vm: *Vm, args: []const Zir.Inst.Ref) !void {
    assert(args.len == 2);
    var x = vm.getRef(args[0]);
    defer x.deref(vm.gpa);
    var y = vm.getRef(args[1]);
    defer y.deref(vm.gpa);

    switch (x.type) {
        .long => switch (y.type) {
            .long => {
                const result = x.as.long + y.as.long;
                vm.stack.appendAssumeCapacity(.{ .type = .long, .as = .{ .long = result } });
            },
            inline else => |t| std.debug.panic("NYI: {t}", .{t}),
        },
        inline else => |t| std.debug.panic("NYI: {t}", .{t}),
    }
}

fn subtract(vm: *Vm, args: []const Zir.Inst.Ref) !void {
    assert(args.len == 2);
    var x = vm.getRef(args[0]);
    defer x.deref(vm.gpa);
    var y = vm.getRef(args[1]);
    defer y.deref(vm.gpa);

    switch (x.type) {
        .long => switch (y.type) {
            .long => {
                const result = x.as.long - y.as.long;
                vm.stack.appendAssumeCapacity(.{ .type = .long, .as = .{ .long = result } });
            },
            inline else => |t| std.debug.panic("NYI: {t}", .{t}),
        },
        inline else => |t| std.debug.panic("NYI: {t}", .{t}),
    }
}

fn list(vm: *Vm, refs: []const Zir.Inst.Ref) !void {
    var data: std.ArrayList(u8) = try .initCapacity(vm.gpa, refs.len * @sizeOf(i64));
    defer data.deinit(vm.gpa);

    for (refs) |ref| {
        var value = vm.getRef(ref);
        defer value.deref(vm.gpa);
        assert(value.type == .long);
        const bytes: [8]u8 = @bitCast(value.as.long);
        data.appendSliceAssumeCapacity(&bytes);
    }

    const bytes = try data.toOwnedSlice(vm.gpa);
    errdefer comptime unreachable;
    vm.stack.appendAssumeCapacity(.{ .type = .long_list, .as = .{ .list = bytes } });
}

pub const K = *KStruct;
pub const KStruct = struct {
    m: i8 = undefined,
    a: i8 = undefined,
    type: Type,
    attr: Attr = .none,
    ref_count: i32 = 0,
    as: Union,

    pub const @"true": KStruct = .{ .type = .boolean, .as = .{ .byte = @intFromBool(true) } };
    pub const @"false": KStruct = .{ .type = .boolean, .as = .{ .byte = @intFromBool(false) } };
    pub const zero: KStruct = .{ .type = .long, .as = .{ .long = 0 } };
    pub const one: KStruct = .{ .type = .long, .as = .{ .long = 1 } };
    pub const negative_one: KStruct = .{ .type = .long, .as = .{ .long = -1 } };
    pub const empty_list: KStruct = .{ .type = .list, .as = .{ .list = &.{} } };

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
            std.log.debug("deref {t}", .{self.type});
            switch (self.type) {
                .long => {},
                .long_list => gpa.free(self.as.list),
                inline else => |t| std.debug.panic("NYI: {t}", .{t}),
            }
            self.* = undefined;
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
    symbol: [:0]u8,
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
};
pub const Attr = enum(u8) {
    none = 0,
    sorted = 1,
    unique = 2,
    parted = 3,
    grouped = 4,
};
