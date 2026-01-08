const std = @import("std");
const Allocator = std.mem.Allocator;
const Io = std.Io;

const Uri = @This();

percent_encoded: []const u8,

pub fn parse(gpa: Allocator, text: []const u8) !Uri {
    const uri: std.Uri = try .parse(text);

    var buffer: Io.Writer.Allocating = .init(gpa);
    defer buffer.deinit();

    try uri.format(&buffer.writer);

    return .{ .percent_encoded = try buffer.toOwnedSlice() };
}

pub fn deinit(uri: Uri, gpa: Allocator) void {
    gpa.free(uri.percent_encoded);
}

pub fn dupe(uri: Uri, gpa: Allocator) !Uri {
    return .{ .percent_encoded = try gpa.dupe(u8, uri.percent_encoded) };
}

pub fn eql(a: Uri, b: Uri) bool {
    return std.mem.eql(u8, a.percent_encoded, b.percent_encoded);
}

pub fn ArrayHashMap(comptime V: type) type {
    return std.ArrayHashMapUnmanaged(Uri, V, Context, true);
}

const Context = struct {
    pub fn hash(_: Context, key: Uri) u32 {
        return std.array_hash_map.hashString(key.percent_encoded);
    }

    pub fn eql(_: Context, a: Uri, b: Uri, _: usize) bool {
        return std.array_hash_map.eqlString(a.percent_encoded, b.percent_encoded);
    }
};
