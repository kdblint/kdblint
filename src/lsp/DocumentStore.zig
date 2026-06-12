const std = @import("std");
const Io = std.Io;
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;

const Uri = @import("Uri.zig");

const kdb = @import("../kdb/root.zig");
const Ast = kdb.Ast;
const Zir = kdb.Zir;
const AstGen = kdb.AstGen;
const DocumentScope = kdb.DocumentScope;

const DocumentStore = @This();

io: Io,
gpa: Allocator,
mutex: Io.Mutex = .init,
handles: Uri.ArrayHashMap(*Handle) = .empty,

pub const Handle = struct {
    uri: Uri,
    tree: Ast,

    impl: struct {
        /// @bitCast from/to `Status`
        status: std.atomic.Value(u32),
        lock: Io.Mutex = .init,
        lazy_condition: Io.Condition = .init,
        zir: Zir = undefined,
        /// Computed together with `zir`; valid iff `Status.has_zir` is set.
        doc_scope: DocumentScope = undefined,
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
    pub fn init(io: Io, gpa: Allocator, uri: Uri, source: [:0]const u8, lsp_synced: bool) !Handle {
        const mode: Ast.Mode = if (std.mem.eql(u8, std.fs.path.extension(uri.percent_encoded), ".k")) .k else .q;
        const tree: Ast = try .parse(io, gpa, source, .{
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
        const status = self.getStatus();
        if (status.has_zir) {
            self.impl.zir.deinit(gpa);
            self.impl.doc_scope.deinit(gpa);
        }
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

    pub fn getZir(self: *Handle, io: Io, gpa: Allocator) !Zir {
        if (self.getStatus().has_zir) return self.impl.zir;

        self.impl.lock.lockUncancelable(io);
        defer self.impl.lock.unlock(io);
        while (true) {
            const status = self.getStatus();
            if (status.has_zir) break;
            if (status.has_zir_lock or self.impl.status.bitSet(@bitOffsetOf(Status, "has_zir_lock"), .release) != 0) {
                // another thread is currently computing the data
                self.impl.lazy_condition.waitUncancelable(io, &self.impl.lock);
                continue;
            }
            defer self.impl.lazy_condition.broadcast(io);

            var doc_scope: DocumentScope = .{};
            errdefer doc_scope.deinit(gpa);
            var context: DocumentScope.ScopeContext = .{
                .gpa = gpa,
                .tree = self.tree,
                .doc_scope = &doc_scope,
            };
            defer context.deinit();

            self.impl.zir = try AstGen.generate(io, gpa, &context);
            errdefer comptime unreachable;

            self.impl.doc_scope = doc_scope;

            const old_has_data = self.impl.status.bitSet(@bitOffsetOf(Status, "has_zir"), .release);
            assert(old_has_data == 0); // race condition
        }
        return self.impl.zir;
    }

    /// The `DocumentScope` is computed lazily alongside the `Zir`.
    /// The returned pointer is valid for the lifetime of the `Handle`.
    pub fn getDocumentScope(self: *Handle, io: Io, gpa: Allocator) !*const DocumentScope {
        _ = try self.getZir(io, gpa);
        return &self.impl.doc_scope;
    }
};

pub fn deinit(self: *DocumentStore) void {
    for (self.handles.keys(), self.handles.values()) |uri, handle| {
        handle.deinit(self.gpa);
        self.gpa.destroy(handle);
        uri.deinit(self.gpa);
    }
    self.handles.deinit(self.gpa);
    self.* = undefined;
}

pub fn getHandle(self: *DocumentStore, uri: Uri) ?*Handle {
    self.mutex.lockUncancelable(self.io);
    defer self.mutex.unlock(self.io);
    return self.handles.get(uri);
}

pub fn openLspSyncedDocument(self: *DocumentStore, uri: Uri, text: []const u8) !void {
    const duped_text = try self.gpa.dupeSentinel(u8, text, 0);
    _ = try self.createAndStoreDocument(uri, duped_text, true, .always);
}

pub fn refreshLspSyncedDocument(self: *DocumentStore, uri: Uri, new_text: [:0]const u8) !void {
    if (self.getHandle(uri)) |handle| {
        if (!handle.isLspSynced()) {
            std.log.warn("Document modified without being opened: {s}", .{uri.percent_encoded});
        }
    } else {
        std.log.warn("Document modified without being opened: {s}", .{uri.percent_encoded});
    }

    _ = try self.createAndStoreDocument(uri, new_text, true, .always);
}

/// What to do when a document already exists for the URI.
pub const ReplacePolicy = enum {
    always,
    never,
    if_not_lsp_synced,
};

/// Stores a document that is not synchronized through LSP `textDocument/did*`
/// notifications. Used by workspace indexing, watched-file events and tests.
/// Takes ownership of `source`.
pub fn loadDocumentFromSource(self: *DocumentStore, uri: Uri, source: [:0]const u8, replace: ReplacePolicy) !void {
    _ = try self.createAndStoreDocument(uri, source, false, replace);
}

/// Reads `uri` from disk and stores it as a non-lsp-synced document.
pub fn loadDocumentFromDisk(self: *DocumentStore, uri: Uri, replace: ReplacePolicy) !void {
    const path = try uri.toFsPath(self.gpa);
    defer self.gpa.free(path);

    const file = try Io.Dir.cwd().openFile(self.io, path, .{});
    defer file.close(self.io);

    var read_buffer: [1024]u8 = undefined;
    var file_reader = file.reader(self.io, &read_buffer);
    const source = std.zig.readSourceFileToEndAlloc(self.gpa, &file_reader) catch |err| switch (err) {
        error.ReadFailed => return file_reader.err.?,
        else => |e| return e,
    };

    try self.loadDocumentFromSource(uri, source, replace);
}

pub const RemovePolicy = enum { any, non_synced_only };

pub fn removeDocument(self: *DocumentStore, uri: Uri, policy: RemovePolicy) void {
    self.mutex.lockUncancelable(self.io);
    defer self.mutex.unlock(self.io);

    const handle = self.handles.get(uri) orelse return;
    if (policy == .non_synced_only and handle.isLspSynced()) return;

    const kv = self.handles.fetchSwapRemove(uri).?;
    kv.key.deinit(self.gpa);
    kv.value.deinit(self.gpa);
    self.gpa.destroy(kv.value);
}

/// `textDocument/didClose`: demote the document to a non-synced one backed by
/// its on-disk content, or remove it if the file cannot be read (deleted,
/// non-`file:` scheme, ...). Unsaved buffer changes are discarded either way.
pub fn unsyncOrRemoveDocument(self: *DocumentStore, uri: Uri) void {
    self.loadDocumentFromDisk(uri, .always) catch |err| {
        std.log.debug("removing closed document {s}: {t}", .{ uri.percent_encoded, err });
        self.removeDocument(uri, .any);
    };
}

/// Snapshot of all handles, in insertion order. Handle pointers are
/// heap-stable; the returned slice is allocated with `allocator`.
pub fn collectHandles(self: *DocumentStore, allocator: Allocator) ![]*Handle {
    self.mutex.lockUncancelable(self.io);
    defer self.mutex.unlock(self.io);
    return allocator.dupe(*Handle, self.handles.values());
}

/// Removes all non-lsp-synced documents whose URI is inside `folder_uri`.
pub fn removeDocumentsInFolder(self: *DocumentStore, folder_uri: Uri) void {
    self.mutex.lockUncancelable(self.io);
    defer self.mutex.unlock(self.io);

    const folder = folder_uri.percent_encoded;
    var i: usize = self.handles.count();
    while (i > 0) {
        i -= 1;
        const uri = self.handles.keys()[i];
        const handle = self.handles.values()[i];
        if (handle.isLspSynced()) continue;
        if (!std.mem.startsWith(u8, uri.percent_encoded, folder)) continue;
        if (uri.percent_encoded.len > folder.len and uri.percent_encoded[folder.len] != '/') continue;

        handle.deinit(self.gpa);
        self.gpa.destroy(handle);
        uri.deinit(self.gpa);
        self.handles.swapRemoveAt(i);
    }
}

/// Takes ownership of `source`.
fn createAndStoreDocument(self: *DocumentStore, uri: Uri, source: [:0]const u8, lsp_synced: bool, replace: ReplacePolicy) !*Handle {
    var new_handle: Handle = handle: {
        errdefer self.gpa.free(source);
        break :handle try .init(self.io, self.gpa, uri, source, lsp_synced);
    };
    errdefer new_handle.deinit(self.gpa);

    self.mutex.lockUncancelable(self.io);
    defer self.mutex.unlock(self.io);

    const gop = try self.handles.getOrPut(self.gpa, uri);
    errdefer if (!gop.found_existing) assert(self.handles.swapRemove(uri));

    if (gop.found_existing) {
        const replace_existing = switch (replace) {
            .always => true,
            .never => false,
            .if_not_lsp_synced => !gop.value_ptr.*.isLspSynced(),
        };
        if (replace_existing) {
            if (lsp_synced and gop.value_ptr.*.isLspSynced()) {
                std.log.warn("Document already open: {s}", .{uri.percent_encoded});
            }
            new_handle.uri = gop.key_ptr.*;
            gop.value_ptr.*.deinit(self.gpa);
            gop.value_ptr.*.* = new_handle;
        } else {
            new_handle.deinit(self.gpa);
        }
    } else {
        gop.key_ptr.* = try uri.dupe(self.gpa);
        errdefer gop.key_ptr.*.deinit(self.gpa);

        gop.value_ptr.* = try self.gpa.create(Handle);
        errdefer comptime unreachable;

        new_handle.uri = gop.key_ptr.*;
        gop.value_ptr.*.* = new_handle;
    }

    return gop.value_ptr.*;
}

fn testLoadSource(store: *DocumentStore, uri_text: []const u8, source: [:0]const u8, replace: ReplacePolicy) !void {
    const uri = try Uri.parse(store.gpa, uri_text);
    defer uri.deinit(store.gpa);
    const duped = try store.gpa.dupeSentinel(u8, source, 0);
    try store.loadDocumentFromSource(uri, duped, replace);
}

test "replace policies" {
    const gpa = std.testing.allocator;
    var store: DocumentStore = .{ .io = std.testing.io, .gpa = gpa };
    defer store.deinit();

    const uri = try Uri.parse(gpa, "file:///a.q");
    defer uri.deinit(gpa);

    try testLoadSource(&store, "file:///a.q", "a:1", .never);
    try std.testing.expectEqualStrings("a:1", store.getHandle(uri).?.tree.source);

    // .never keeps the existing document
    try testLoadSource(&store, "file:///a.q", "a:2", .never);
    try std.testing.expectEqualStrings("a:1", store.getHandle(uri).?.tree.source);

    // .if_not_lsp_synced replaces a non-synced document
    try testLoadSource(&store, "file:///a.q", "a:3", .if_not_lsp_synced);
    try std.testing.expectEqualStrings("a:3", store.getHandle(uri).?.tree.source);

    // but not an lsp-synced one
    try store.openLspSyncedDocument(uri, "a:4");
    try testLoadSource(&store, "file:///a.q", "a:5", .if_not_lsp_synced);
    try std.testing.expectEqualStrings("a:4", store.getHandle(uri).?.tree.source);

    // .always replaces anything
    try testLoadSource(&store, "file:///a.q", "a:6", .always);
    try std.testing.expectEqualStrings("a:6", store.getHandle(uri).?.tree.source);
    try std.testing.expect(!store.getHandle(uri).?.isLspSynced());
}

test "removeDocument honors the policy" {
    const gpa = std.testing.allocator;
    var store: DocumentStore = .{ .io = std.testing.io, .gpa = gpa };
    defer store.deinit();

    const uri = try Uri.parse(gpa, "file:///a.q");
    defer uri.deinit(gpa);

    try store.openLspSyncedDocument(uri, "a:1");
    store.removeDocument(uri, .non_synced_only);
    try std.testing.expect(store.getHandle(uri) != null);

    store.removeDocument(uri, .any);
    try std.testing.expectEqual(null, store.getHandle(uri));
}

test "collectHandles and removeDocumentsInFolder" {
    const gpa = std.testing.allocator;
    var store: DocumentStore = .{ .io = std.testing.io, .gpa = gpa };
    defer store.deinit();

    try testLoadSource(&store, "file:///proj/a.q", "a:1", .never);
    try testLoadSource(&store, "file:///proj/sub/b.q", "b:1", .never);
    try testLoadSource(&store, "file:///projother/c.q", "c:1", .never);

    const synced_uri = try Uri.parse(gpa, "file:///proj/open.q");
    defer synced_uri.deinit(gpa);
    try store.openLspSyncedDocument(synced_uri, "o:1");

    const handles = try store.collectHandles(gpa);
    defer gpa.free(handles);
    try std.testing.expectEqual(4, handles.len);

    const folder = try Uri.parse(gpa, "file:///proj");
    defer folder.deinit(gpa);
    store.removeDocumentsInFolder(folder);

    // non-synced docs under /proj are gone; the synced one and the
    // sibling /projother dir survive
    try std.testing.expectEqual(2, store.handles.count());
    try std.testing.expect(store.getHandle(synced_uri) != null);
}

test {
    std.testing.refAllDecls(@This());
}
