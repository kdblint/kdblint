const std = @import("std");
const builtin = @import("builtin");
const zls = @import("zls");
const kdb = @import("kdb.zig");
const Ast = kdb.Ast;
const offsets = @import("offsets.zig");

const log = std.log.scoped(.kdblint_store);

const DocumentStore = @This();

allocator: std.mem.Allocator,
/// the DocumentStore ussumes that `config` is not modified while calling one of its functions.
config: Config,
lock: std.Thread.RwLock = .{},
thread_pool: if (builtin.single_threaded) void else *std.Thread.Pool,
handles: std.StringArrayHashMapUnmanaged(*Handle) = .{},

pub const Uri = zls.DocumentStore.Uri;

pub const Hasher = zls.DocumentStore.Hasher;
pub const Hash = zls.DocumentStore.Hash;

pub const max_document_size = zls.DocumentStore.max_document_size;

pub fn computeHash(bytes: []const u8) Hash {
    var hasher: Hasher = Hasher.init(&[_]u8{0} ** Hasher.key_length);
    hasher.update(bytes);
    var hash: Hash = undefined;
    hasher.final(&hash);
    return hash;
}

pub const Config = struct {
    pub fn fromMainConfig(config: @import("Config.zig")) Config {
        _ = config; // autofix
        return .{};
    }
};

pub const Handle = struct {
    uri: Uri,
    tree: Ast,

    /// private field
    impl: struct {
        /// @bitCast from/to `Status`
        status: std.atomic.Value(u32) = std.atomic.Value(u32).init(@bitCast(Status{})),
        /// TODO can we avoid storing one allocator per Handle?
        allocator: std.mem.Allocator,

        lock: std.Thread.Mutex = .{},
        condition: std.Thread.Condition = .{},
    },

    const Status = packed struct(u32) {
        /// `true` if the document has been directly opened by the client i.e. with `textDocument/didOpen`
        /// `false` indicates the document only exists because it is a dependency of another document
        /// or has been closed with `textDocument/didClose` and is awaiting cleanup through `garbageCollection`
        open: bool = false,
        _: u31 = undefined,
    };
    /// takes ownership of `text`
    pub fn init(allocator: std.mem.Allocator, uri: Uri, text: [:0]const u8) error{OutOfMemory}!Handle {
        const duped_uri = try allocator.dupe(u8, uri);
        errdefer allocator.free(duped_uri);

        const tree = try parseTree(allocator, text);
        errdefer tree.deinit(allocator);

        return .{
            .uri = duped_uri,
            .tree = tree,
            .impl = .{
                .allocator = allocator,
            },
        };
    }

    fn deinit(self: *Handle) void {
        const allocator = self.impl.allocator;

        allocator.free(self.tree.source);
        self.tree.deinit(allocator);
        allocator.free(self.uri);

        self.* = undefined;
    }

    fn getStatus(self: Handle) Status {
        return @bitCast(self.impl.status.load(.acquire));
    }

    /// returns the previous value
    fn setOpen(self: *Handle, open: bool) bool {
        if (open) {
            return self.impl.status.bitSet(@offsetOf(Handle.Status, "open"), .release) == 1;
        } else {
            return self.impl.status.bitReset(@offsetOf(Handle.Status, "open"), .release) == 1;
        }
    }

    fn parseTree(allocator: std.mem.Allocator, new_text: [:0]const u8) error{OutOfMemory}!Ast {
        // TODO: Use settings config
        var tree = try Ast.parse(allocator, new_text, .{
            .version = .v4_0,
            .language = .q,
        });
        errdefer tree.deinit(allocator);

        // remove unused capacity
        var nodes = tree.nodes.toMultiArrayList();
        try nodes.setCapacity(allocator, nodes.len);
        tree.nodes = nodes.slice();

        // remove unused capacity
        var tokens = tree.tokens.toMultiArrayList();
        try tokens.setCapacity(allocator, tokens.len);
        tree.tokens = tokens.slice();

        return tree;
    }

    fn setSource(self: *Handle, new_text: [:0]const u8) error{OutOfMemory}!void {
        const new_status = Handle.Status{
            .open = self.getStatus().open,
        };

        const new_tree = try parseTree(self.impl.allocator, new_text);

        self.impl.lock.lock();
        errdefer @compileError("");

        const old_status: Handle.Status = @bitCast(self.impl.status.swap(@bitCast(new_status), .acq_rel));
        _ = old_status;

        var old_tree = self.tree;

        self.tree = new_tree;

        self.impl.lock.unlock();

        self.impl.allocator.free(old_tree.source);
        old_tree.deinit(self.impl.allocator);
    }
};

pub fn deinit(self: *DocumentStore) void {
    for (self.handles.values()) |handle| {
        handle.deinit();
        self.allocator.destroy(handle);
    }
    self.handles.deinit(self.allocator);
}

pub fn getHandle(self: *DocumentStore, uri: Uri) ?*Handle {
    self.lock.lockShared();
    defer self.lock.unlockShared();
    return self.handles.get(uri);
}

pub fn openDocument(self: *DocumentStore, uri: Uri, text: []const u8) error{OutOfMemory}!void {
    {
        self.lock.lockShared();
        defer self.lock.unlockShared();
        if (self.handles.get(uri)) |handle| {
            if (handle.setOpen(true)) {
                log.warn("Document already open: {s}", .{uri});
            }
            return;
        }
    }

    const duped_text = try self.allocator.dupeZ(u8, text);
    _ = try self.createAndStoreDocument(uri, duped_text, true);
}

pub fn closeDocument(self: *DocumentStore, uri: Uri) void {
    {
        self.lock.lockShared();
        defer self.lock.unlockShared();

        const handle = self.handles.get(uri) orelse {
            log.warn("Document not found: {s}", .{uri});
            return;
        };
        if (!handle.setOpen(false)) {
            log.warn("Document already closed: {s}", .{uri});
        }
    }

    if (!self.lock.tryLock()) return;
    defer self.lock.unlock();

    // TODO: Garbage collection
}

pub fn refreshDocument(self: *DocumentStore, uri: Uri, new_text: [:0]const u8) !void {
    const handle = self.getHandle(uri).?;
    if (!handle.getStatus().open) {
        log.warn("Document modified without being opened: {s}", .{uri});
    }

    try handle.setSource(new_text);
}

fn createDocument(self: *DocumentStore, uri: Uri, text: [:0]const u8, open: bool) error{OutOfMemory}!Handle {
    var handle = try Handle.init(self.allocator, uri, text);
    errdefer handle.deinit();

    _ = handle.setOpen(open);
    return handle;
}

fn createAndStoreDocument(self: *DocumentStore, uri: Uri, text: [:0]const u8, open: bool) error{OutOfMemory}!*Handle {
    const handle_ptr = try self.allocator.create(Handle);
    errdefer self.allocator.destroy(handle_ptr);

    handle_ptr.* = try self.createDocument(uri, text, open);
    errdefer handle_ptr.deinit();

    const gop = blk: {
        self.lock.lock();
        defer self.lock.unlock();
        break :blk try self.handles.getOrPutValue(self.allocator, handle_ptr.uri, handle_ptr);
    };

    if (gop.found_existing) {
        handle_ptr.deinit();
        self.allocator.destroy(handle_ptr);
    }

    return gop.value_ptr.*;
}

test {
    @import("std").testing.refAllDecls(@This());
}
