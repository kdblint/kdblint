const std = @import("std");
const Allocator = std.mem.Allocator;
const Uri = std.Uri;
const assert = std.debug.assert;

const kdb = @import("../kdb/root.zig");
const Ast = kdb.Ast;
const Zir = kdb.Zir;
const AstGen = kdb.AstGen;
const DocumentScope = kdb.DocumentScope;

const DocumentStore = @This();

io: std.Io,
gpa: Allocator,
lock: std.Thread.RwLock = .{},
thread_pool: *std.Thread.Pool,
handles: std.ArrayHashMapUnmanaged(Uri, *Handle, Context, true) = .empty,

pub const Handle = struct {
    uri: Uri,
    tree: Ast,

    impl: struct {
        /// @bitCast from/to `Status`
        status: std.atomic.Value(u32),
        lock: std.Thread.Mutex = .{},
        lazy_condition: std.Thread.Condition = .{},
        zir: Zir = undefined,
    },

    const Status = packed struct(u32) {
        /// `true` if the document has been directly opened by the client i.e. with `textDocument/didOpen`
        /// `false` indicates the document only exists because it is a dependency of another document
        /// or has been closed with `textDocument/didClose`.
        lsp_synced: bool = false,
        /// true if a thread has acquired the permission to compute the `Zir`
        has_zir_lock: bool = false,
        /// all other threads will wait until the given thread has computed the `Zir` before reading it.
        /// true if `handle.impl.zir` has been set
        has_zir: bool = false,
        _: u29 = 0,
    };

    /// Takes ownership of `source` on success.
    pub fn init(gpa: Allocator, uri: Uri, source: [:0]const u8, lsp_synced: bool) !Handle {
        const mode: Ast.Mode = if (std.mem.eql(u8, std.fs.path.extension(uri.path.percent_encoded), ".k")) .k else .q;
        const tree: Ast = try .parse(gpa, source, .{
            .mode = mode,
            .version = .@"4.0",
        });
        errdefer comptime unreachable;

        return .{
            .uri = uri,
            .tree = tree,
            .impl = .{
                .status = .init(@bitCast(Status{
                    .lsp_synced = lsp_synced,
                })),
            },
        };
    }

    pub fn deinit(self: *Handle, gpa: Allocator) void {
        gpa.free(self.tree.source);
        self.tree.deinit(gpa);
        self.* = undefined;
    }

    pub fn getStatus(self: *const Handle) Status {
        return @bitCast(self.impl.status.load(.acquire));
    }

    pub fn isLspSynced(self: *const Handle) bool {
        return self.getStatus().lsp_synced;
    }

    pub fn setLspSynced(self: *Handle, lsp_synced: bool) bool {
        if (lsp_synced) {
            return self.impl.status.bitSet(@offsetOf(Status, "lsp_synced"), .release) == 1;
        } else {
            return self.impl.status.bitReset(@offsetOf(Status, "lsp_synced"), .release) == 1;
        }
    }

    pub fn getZir(self: *Handle, gpa: Allocator) !Zir {
        if (self.getStatus().has_zir) return self.impl.zir;

        self.impl.lock.lock();
        defer self.impl.lock.unlock();
        while (true) {
            const status = self.getStatus();
            if (status.has_zir) break;
            if (status.has_zir_lock or self.impl.status.bitSet(@bitOffsetOf(Status, "has_zir_lock"), .release) != 0) {
                // another thread is currently computing the data
                self.impl.lazy_condition.wait(&self.impl.lock);
                continue;
            }
            defer self.impl.lazy_condition.broadcast();

            var doc_scope: DocumentScope = .{};
            defer doc_scope.deinit(gpa);
            var context: DocumentScope.ScopeContext = .{
                .gpa = gpa,
                .tree = self.tree,
                .doc_scope = &doc_scope,
            };
            defer context.deinit();

            self.impl.zir = try AstGen.generate(gpa, &context);
            errdefer comptime unreachable;

            const old_has_data = self.impl.status.bitSet(@bitOffsetOf(Status, "has_zir"), .release);
            assert(old_has_data == 0); // race condition
        }
        return self.impl.zir;
    }
};

const Context = struct {
    pub fn hash(_: Context, key: Uri) u32 {
        return std.array_hash_map.hashString(key.path.percent_encoded);
    }

    pub fn eql(_: Context, a: Uri, b: Uri, _: usize) bool {
        return std.array_hash_map.eqlString(a.path.percent_encoded, b.path.percent_encoded);
    }
};

pub fn deinit(self: *DocumentStore) void {
    for (self.handles.values()) |handle| {
        handle.deinit(self.gpa);
        self.gpa.destroy(handle);
    }
    self.handles.deinit(self.gpa);
}

pub fn getHandle(self: *DocumentStore, uri: Uri) ?*Handle {
    self.lock.lockShared();
    defer self.lock.unlockShared();
    return self.handles.get(uri);
}

pub fn openLspSyncedDocument(self: *DocumentStore, uri: Uri, text: []const u8) !void {
    if (self.handles.contains(uri)) {
        std.log.warn("Document already open: {s}", .{uri.path.percent_encoded});
    }

    const duped_text = try self.gpa.dupeZ(u8, text);
    _ = try self.createAndStoreDocument(uri, duped_text, true);
}

pub fn closeLspSyncedDocument(self: *DocumentStore, uri: Uri) void {
    const kv = self.handles.fetchSwapRemove(uri) orelse {
        std.log.warn("Document not found: {s}", .{uri.path.percent_encoded});
        return;
    };
    if (!kv.value.isLspSynced()) {
        std.log.warn("Document already closed: {s}", .{uri.path.percent_encoded});
    }

    kv.value.deinit(self.gpa);
    self.gpa.destroy(kv.value);
}

pub fn refreshLspSyncedDocument(self: *DocumentStore, uri: Uri, new_text: [:0]const u8) !void {
    if (self.handles.get(uri)) |handle| {
        if (!handle.isLspSynced()) {
            std.log.warn("Document modified without being opened: {s}", .{uri.path.percent_encoded});
        }
    } else {
        std.log.warn("Document modified without being opened: {s}", .{uri.path.percent_encoded});
    }

    _ = try self.createAndStoreDocument(uri, new_text, true);
}

/// Takes ownership of `source`.
fn createAndStoreDocument(self: *DocumentStore, uri: Uri, source: [:0]const u8, lsp_synced: bool) !*Handle {
    var new_handle: Handle = handle: {
        errdefer self.gpa.free(source);
        break :handle try .init(self.gpa, uri, source, lsp_synced);
    };
    errdefer new_handle.deinit(self.gpa);

    self.lock.lock();
    defer self.lock.unlock();

    const gop = try self.handles.getOrPut(self.gpa, uri);
    errdefer if (!gop.found_existing) assert(self.handles.swapRemove(uri));

    if (gop.found_existing) {
        if (lsp_synced) {
            gop.value_ptr.*.deinit(self.gpa);

            new_handle.uri = gop.key_ptr.*;
            gop.value_ptr.*.* = new_handle;
        } else {
            new_handle.deinit(self.gpa);
        }
    } else {
        gop.value_ptr.* = try self.gpa.create(Handle);
        errdefer comptime unreachable;

        new_handle.uri = gop.key_ptr.*;
        gop.value_ptr.*.* = new_handle;
    }

    return gop.value_ptr.*;
}
