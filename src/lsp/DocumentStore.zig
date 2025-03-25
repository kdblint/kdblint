const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const builtin = @import("builtin");
const tracy = @import("tracy");
const zls = @import("zls");
const URI = zls.URI;
const kdb = @import("kdb");
const Ast = kdb.Ast;
const AstGen = kdb.AstGen;
const Zir = kdb.Zir;
const offsets = @import("offsets.zig");
const Config = @import("Config.zig");
const analysis = @import("analysis.zig");
const DocumentScope = kdb.DocumentScope;
const DiagnosticsCollection = @import("DiagnosticsCollection.zig");

const log = std.log.scoped(.kdblint_store);

const DocumentStore = @This();

allocator: Allocator,
config: Config,
lock: std.Thread.RwLock = .{},
thread_pool: if (builtin.single_threaded) void else *std.Thread.Pool,
handles: std.StringArrayHashMapUnmanaged(*Handle) = .{},
diagnostics_collection: *DiagnosticsCollection,

pub const Uri = []const u8;

pub const Hasher = std.crypto.auth.siphash.SipHash128(1, 3);
pub const Hash = [Hasher.mac_length]u8;

pub const max_document_size = std.math.maxInt(u32);

pub fn computeHash(bytes: []const u8) Hash {
    var hasher: Hasher = Hasher.init(&[_]u8{0} ** Hasher.key_length);
    hasher.update(bytes);
    var hash: Hash = undefined;
    hasher.final(&hash);
    return hash;
}

pub const Handle = struct {
    uri: Uri,
    tree: Ast,
    /// Contains one entry for every import in the document
    import_uris: std.ArrayListUnmanaged(Uri) = .{},

    /// private field
    impl: struct {
        /// @bitCast from/to `Status`
        status: std.atomic.Value(u32) = .init(@bitCast(Status{})),
        /// TODO can we avoid storing one allocator per Handle?
        gpa: Allocator,

        lock: std.Thread.Mutex = .{},
        condition: std.Thread.Condition = .{},

        document_scope: DocumentScope = .{},
        zir: Zir = undefined,
    },

    const Status = packed struct(u32) {
        /// `true` if the document has been directly opened by the client i.e. with `textDocument/didOpen`
        /// `false` indicates the document only exists because it is a dependency of another document
        /// or has been closed with `textDocument/didClose` and is awaiting cleanup through `garbageCollection`
        open: bool = false,
        /// true if a thread has acquired the permission to compute the `ZIR`
        has_zir_lock: bool = false,
        /// all other threads will wait until the given thread has computed the `ZIR` before reading it.
        /// true if `handle.impl.zir` has been set
        has_zir: bool = false,
        zir_outdated: bool = undefined,
        _: u28 = undefined,
    };

    pub const ZirStatus = enum {
        none,
        outdated,
        done,
    };

    /// takes ownership of `text`
    pub fn init(gpa: Allocator, uri: Uri, text: [:0]const u8, settings: Ast.ParseSettings) error{OutOfMemory}!Handle {
        const duped_uri = try gpa.dupe(u8, uri);
        errdefer gpa.free(duped_uri);

        const tree = try parseTree(gpa, text, settings);
        errdefer tree.deinit(gpa);

        return .{
            .uri = duped_uri,
            .tree = tree,
            .impl = .{
                .gpa = gpa,
            },
        };
    }

    pub fn getDocumentScope(self: *Handle) error{OutOfMemory}!DocumentScope {
        if (!self.getStatus().has_zir) _ = try self.getZir();
        return self.impl.document_scope;
    }

    /// Asserts that `getDocumentScope` has been previously called on `handle`.
    pub fn getDocumentScopeCached(self: *Handle) DocumentScope {
        assert(self.getStatus().has_zir);
        return self.impl.document_scope;
    }

    pub fn getZir(self: *Handle) error{OutOfMemory}!Zir {
        if (self.getStatus().has_zir) return self.impl.zir;
        return try self.getZirCold();
    }

    pub fn getZirStatus(self: *const Handle) ZirStatus {
        const status = self.getStatus();
        if (!status.has_zir) return .none;
        return if (status.zir_outdated) .outdated else .done;
    }

    fn getZirCold(self: *Handle) error{OutOfMemory}!Zir {
        @branchHint(.cold);
        const tracy_zone = tracy.trace(@src());
        defer tracy_zone.end();

        self.impl.lock.lock();
        defer self.impl.lock.unlock();
        while (true) {
            const status = self.getStatus();
            if (status.has_zir) break;
            if (status.has_zir_lock or
                self.impl.status.bitSet(@bitOffsetOf(Status, "has_zir_lock"), .release) != 0)
            {
                // another thread is currently computing the ZIR
                self.impl.condition.wait(&self.impl.lock);
                continue;
            }
            defer self.impl.condition.broadcast();

            self.impl.zir = blk: {
                const tracy_zone_inner = tracy.traceNamed(@src(), "AstGen.generate");
                defer tracy_zone_inner.end();

                var context: DocumentScope.ScopeContext = .{
                    .gpa = self.impl.gpa,
                    .tree = self.tree,
                    .doc_scope = &self.impl.document_scope,
                };
                defer context.deinit();
                var zir = try AstGen.generate(self.impl.gpa, &context);
                errdefer zir.deinit(self.impl.gpa);

                // remove unused capacity
                var instructions = zir.instructions.toMultiArrayList();
                try instructions.setCapacity(self.impl.gpa, instructions.len);
                zir.instructions = instructions.slice();

                break :blk zir;
            };
            _ = self.impl.status.bitReset(@bitOffsetOf(Status, "zir_outdated"), .release); // atomically set zir_outdated
            const old_has_zir = self.impl.status.bitSet(@bitOffsetOf(Status, "has_zir"), .release); // atomically set has_zir
            assert(old_has_zir == 0); // race condition: another thread set `has_zir` even though we hold the lock
        }
        return self.impl.zir;
    }

    fn getStatus(self: Handle) Status {
        return @bitCast(self.impl.status.load(.acquire));
    }

    pub fn isOpen(self: Handle) bool {
        return self.getStatus().open;
    }

    /// returns the previous value
    fn setOpen(self: *Handle, open: bool) bool {
        if (open) {
            return self.impl.status.bitSet(@offsetOf(Handle.Status, "open"), .release) == 1;
        } else {
            return self.impl.status.bitReset(@offsetOf(Handle.Status, "open"), .release) == 1;
        }
    }

    fn parseTree(allocator: Allocator, new_text: [:0]const u8, settings: Ast.ParseSettings) error{OutOfMemory}!Ast {
        const tracy_zone_inner = tracy.traceNamed(@src(), "Ast.parse");
        defer tracy_zone_inner.end();

        var tree = try Ast.parse(allocator, new_text, settings);
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

    fn setSource(self: *Handle, new_text: [:0]const u8, settings: Ast.ParseSettings) error{OutOfMemory}!void {
        const tracy_zone = tracy.trace(@src());
        defer tracy_zone.end();

        const new_status = Handle.Status{
            .open = self.getStatus().open,
        };

        const new_tree = try parseTree(self.impl.gpa, new_text, settings);

        self.impl.lock.lock();
        errdefer comptime unreachable;

        const old_status: Handle.Status = @bitCast(
            self.impl.status.swap(@bitCast(new_status), .acq_rel),
        );

        var old_tree = self.tree;
        var old_import_uris = self.import_uris;
        var old_document_scope = if (old_status.has_zir) self.impl.document_scope else null;
        var old_zir = if (old_status.has_zir) self.impl.zir else null;

        self.tree = new_tree;
        self.import_uris = .{};
        self.impl.document_scope = .{};
        self.impl.zir = undefined;

        self.impl.lock.unlock();

        self.impl.gpa.free(old_tree.source);
        old_tree.deinit(self.impl.gpa);

        for (old_import_uris.items) |uri| self.impl.gpa.free(uri);
        old_import_uris.deinit(self.impl.gpa);

        if (old_document_scope) |*document_scope| document_scope.deinit(self.impl.gpa);
        if (old_zir) |*zir| zir.deinit(self.impl.gpa);
    }

    fn deinit(self: *Handle) void {
        const tracy_zone = tracy.trace(@src());
        defer tracy_zone.end();

        const status = self.getStatus();

        const allocator = self.impl.gpa;

        if (status.has_zir) self.impl.zir.deinit(allocator);
        if (status.has_zir) self.impl.document_scope.deinit(allocator);
        allocator.free(self.tree.source);
        self.tree.deinit(allocator);
        allocator.free(self.uri);

        for (self.import_uris.items) |uri| allocator.free(uri);
        self.import_uris.deinit(allocator);

        self.* = undefined;
    }
};

pub const ErrorMessage = struct {
    loc: offsets.Loc,
    code: []const u8,
    message: []const u8,
};

pub fn deinit(self: *DocumentStore) void {
    for (self.handles.values()) |handle| {
        handle.deinit();
        self.allocator.destroy(handle);
    }
    self.handles.deinit(self.allocator);
    self.* = undefined;
}

/// Returns a handle to the given document
/// **Thread safe** takes a shared lock
/// This function does not protect against data races from modifying the Handle
pub fn getHandle(self: *DocumentStore, uri: Uri) ?*Handle {
    self.lock.lockShared();
    defer self.lock.unlockShared();
    return self.handles.get(uri);
}

/// Returns a handle to the given document
/// Will load the document from disk if it hasn't been already
/// **Thread safe** takes an exclusive lock
/// This function does not protect against data races from modifying the Handle
pub fn getOrLoadHandle(self: *DocumentStore, uri: Uri) ?*Handle {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    if (self.getHandle(uri)) |handle| return handle;

    const file_path = URI.parse(self.allocator, uri) catch |err| {
        log.err("failed to parse URI `{s}`: {}", .{ uri, err });
        return null;
    };
    defer self.allocator.free(file_path);

    if (!std.fs.path.isAbsolute(file_path)) {
        log.err("file path is not absolute `{s}`", .{file_path});
        return null;
    }
    const file_contents = std.fs.cwd().readFileAllocOptions(
        self.allocator,
        file_path,
        max_document_size,
        null,
        @alignOf(u8),
        0,
    ) catch |err| {
        log.err("failed to load document `{s}`: {}", .{ file_path, err });
        return null;
    };

    return self.createAndStoreDocument(uri, file_contents, false) catch return null;
}

/// **Thread safe** takes an exclusive lock
pub fn openDocument(self: *DocumentStore, uri: Uri, text: []const u8) error{OutOfMemory}!void {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    {
        self.lock.lockShared();
        defer self.lock.unlockShared();

        if (self.handles.get(uri)) |handle| {
            if (!handle.setOpen(true)) {
                log.warn("Document already open: {s}", .{uri});
            }
            return;
        }
    }

    const duped_text = try self.allocator.dupeZ(u8, text);
    _ = try self.createAndStoreDocument(uri, duped_text, true);
}

/// **Thread safe** takes a shared lock, takes an exclusive lock (with `tryLock`)
/// Assumes that no other thread is currently accessing the given document
pub fn closeDocument(self: *DocumentStore, uri: Uri) void {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    {
        self.lock.lockShared();
        defer self.lock.unlockShared();

        const handle = self.handles.get(uri) orelse {
            log.warn("Document not found: {s}", .{uri});
            return;
        };
        // instead of destroying the handle here we just mark it not open
        // and let it be destroy by the garbage collection code
        if (!handle.setOpen(false)) {
            log.warn("Document already closed: {s}", .{uri});
        }
    }

    if (!self.lock.tryLock()) return;
    defer self.lock.unlock();

    self.garbageCollectionImports() catch {};
}

/// Takes ownership of `new_text` which has to be allocated with this DocumentStore's allocator.
/// Assumes that a document with the given `uri` is in the DocumentStore.
///
/// **Thread safe** takes a shared lock when called on different documents
/// **Not thread safe** when called on the same document
pub fn refreshDocument(self: *DocumentStore, uri: Uri, new_text: [:0]const u8) !void {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    const handle = self.getHandle(uri).?;
    if (!handle.getStatus().open) {
        log.warn("Document modified without being opened: {s}", .{uri});
    }
    try handle.setSource(new_text, self.getParseSettings(uri));
    handle.import_uris = try self.collectImportUris(handle);
}

/// The `DocumentStore` represents a graph structure where every
/// handle/document is a node and every `@import` and `@cImport` represent
/// a directed edge.
/// We can remove every document which cannot be reached from
/// another document that is `open` (see `Handle.open`)
/// **Not thread safe** requires access to `DocumentStore.handles`, `DocumentStore.cimports` and `DocumentStore.build_files`
fn garbageCollectionImports(self: *DocumentStore) error{OutOfMemory}!void {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    var arena = std.heap.ArenaAllocator.init(self.allocator);
    defer arena.deinit();

    var reachable = try std.DynamicBitSetUnmanaged.initEmpty(arena.allocator(), self.handles.count());

    var queue = std.ArrayListUnmanaged(Uri){};

    for (self.handles.values(), 0..) |handle, handle_index| {
        if (!handle.getStatus().open) continue;
        reachable.set(handle_index);

        try self.collectDependenciesInternal(arena.allocator(), handle, &queue, false);
    }

    while (queue.pop()) |uri| {
        const handle_index = self.handles.getIndex(uri) orelse continue;
        if (reachable.isSet(handle_index)) continue;
        reachable.set(handle_index);

        const handle = self.handles.values()[handle_index];

        try self.collectDependenciesInternal(arena.allocator(), handle, &queue, false);
    }

    var it = reachable.iterator(.{
        .kind = .unset,
        .direction = .reverse,
    });

    while (it.next()) |handle_index| {
        const handle = self.handles.values()[handle_index];
        log.debug("Closing document {s}", .{handle.uri});
        self.handles.swapRemoveAt(handle_index);
        handle.deinit();
        self.allocator.destroy(handle);
    }
}

/// invalidates any pointers into `DocumentStore.build_files`
/// **Thread safe** takes an exclusive lock
fn uriInImports(
    self: *DocumentStore,
    checked_uris: *std.StringHashMapUnmanaged(void),
    build_file_uri: Uri,
    source_uri: Uri,
    uri: Uri,
) error{OutOfMemory}!bool {
    if (std.mem.eql(u8, uri, source_uri)) return true;

    const gop = try checked_uris.getOrPut(self.allocator, source_uri);
    if (gop.found_existing) return false;

    const handle = self.getOrLoadHandle(source_uri) orelse {
        errdefer assert(checked_uris.remove(source_uri));
        gop.key_ptr.* = try self.allocator.dupe(u8, source_uri);
        return false;
    };
    gop.key_ptr.* = handle.uri;

    if (try handle.getAssociatedBuildFileUri(self)) |associated_build_file_uri| {
        return std.mem.eql(u8, associated_build_file_uri, build_file_uri);
    }

    for (handle.import_uris.items) |import_uri| {
        if (try self.uriInImports(checked_uris, build_file_uri, import_uri, uri))
            return true;
    }

    return false;
}

/// invalidates any pointers into `DocumentStore.build_files`
/// takes ownership of the `text` passed in.
/// **Thread safe** takes an exclusive lock
fn createDocument(self: *DocumentStore, uri: Uri, text: [:0]const u8, open: bool) error{OutOfMemory}!Handle {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    var handle = try Handle.init(self.allocator, uri, text, self.getParseSettings(uri));
    errdefer handle.deinit();

    _ = handle.setOpen(open);

    handle.import_uris = try self.collectImportUris(&handle);

    return handle;
}

/// takes ownership of the `text` passed in.
/// invalidates any pointers into `DocumentStore.build_files`
/// **Thread safe** takes an exclusive lock
fn createAndStoreDocument(self: *DocumentStore, uri: Uri, text: [:0]const u8, open: bool) error{OutOfMemory}!*Handle {
    const handle_ptr: *Handle = try self.allocator.create(Handle);
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

    log.debug("Opened document `{s}`", .{gop.value_ptr.*.uri});

    return gop.value_ptr.*;
}

/// Caller owns returned memory.
/// **Thread safe** takes a shared lock
fn collectImportUris(self: *DocumentStore, handle: *Handle) error{OutOfMemory}!std.ArrayListUnmanaged(Uri) {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    var imports = try analysis.collectImports(self.allocator, handle.tree);

    var i: usize = 0;
    errdefer {
        // only free the uris
        for (imports.items[0..i]) |uri| self.allocator.free(uri);
        imports.deinit(self.allocator);
    }

    // Convert to URIs
    while (i < imports.items.len) {
        const maybe_uri = try uriFromImportStr(self.allocator, handle, imports.items[i]);

        if (maybe_uri) |uri| {
            // The raw import strings are owned by the document and do not need to be freed here.
            imports.items[i] = uri;
            i += 1;
        } else {
            _ = imports.swapRemove(i);
        }
    }

    return imports;
}

/// collects every file uri the given handle depends on
/// **Thread safe** takes a shared lock
pub fn collectDependencies(
    store: *DocumentStore,
    allocator: Allocator,
    handle: *Handle,
    dependencies: *std.ArrayListUnmanaged(Uri),
) error{OutOfMemory}!void {
    return store.collectDependenciesInternal(allocator, handle, dependencies, true);
}

fn collectDependenciesInternal(
    store: *DocumentStore,
    allocator: Allocator,
    handle: *Handle,
    dependencies: *std.ArrayListUnmanaged(Uri),
    lock: bool,
) error{OutOfMemory}!void {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    if (lock) store.lock.lockShared();
    defer if (lock) store.lock.unlockShared();

    try dependencies.ensureUnusedCapacity(allocator, handle.import_uris.items.len);
    for (handle.import_uris.items) |uri| {
        dependencies.appendAssumeCapacity(try allocator.dupe(u8, uri));
    }
}

/// returns `true` if all include paths could be collected
/// may return `false` because include paths from a build.zig may not have been resolved already
/// **Thread safe** takes a shared lock
pub fn collectIncludeDirs(
    store: *DocumentStore,
    allocator: Allocator,
    handle: *Handle,
    include_dirs: *std.ArrayListUnmanaged([]const u8),
) !bool {
    _ = store; // autofix
    _ = handle; // autofix
    var arena_allocator = std.heap.ArenaAllocator.init(allocator);
    defer arena_allocator.deinit();

    const target_info = try std.zig.system.resolveTargetQuery(.{});
    const native_paths = try std.zig.system.NativePaths.detect(arena_allocator.allocator(), target_info);

    try include_dirs.ensureUnusedCapacity(allocator, native_paths.include_dirs.items.len);
    for (native_paths.include_dirs.items) |native_include_dir| {
        include_dirs.appendAssumeCapacity(try allocator.dupe(u8, native_include_dir));
    }

    return true;
}

/// takes the string inside a @import() node (without the quotation marks)
/// and returns it's uri
/// caller owns the returned memory
/// **Thread safe** takes a shared lock
pub fn uriFromImportStr(allocator: Allocator, handle: *Handle, import_str: []const u8) error{OutOfMemory}!?Uri {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    const base_path = URI.parse(allocator, handle.uri) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => return null,
    };
    defer allocator.free(base_path);

    const joined_path = std.fs.path.resolve(allocator, &.{ base_path, "..", import_str }) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => return null,
    };
    defer allocator.free(joined_path);

    return try URI.fromPath(allocator, joined_path);
}

fn getParseSettings(self: DocumentStore, uri: Uri) Ast.ParseSettings {
    return .{
        .version = self.config.kdb_version,
        .mode = if (std.mem.endsWith(u8, uri, ".k")) .k else .q,
    };
}

test {
    @import("std").testing.refAllDecls(@This());
}
