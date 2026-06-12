const std = @import("std");
const Io = std.Io;
const Allocator = std.mem.Allocator;

const DocumentStore = @import("DocumentStore.zig");
const Uri = @import("Uri.zig");

/// Loads every `.q`/`.k` file under the given workspace folders into the
/// document store as non-lsp-synced documents. Designed to run in the
/// background: failures are logged and skipped, never propagated.
pub fn indexWorkspaceFolders(
    io: Io,
    gpa: Allocator,
    document_store: *DocumentStore,
    folder_uris: []const Uri,
) void {
    var seen: std.AutoHashMap(Io.File.INode, void) = .init(gpa);
    defer seen.deinit();

    for (folder_uris) |folder_uri| {
        const path = folder_uri.toFsPath(gpa) catch |err| {
            std.log.warn("cannot index workspace folder {s}: {t}", .{ folder_uri.percent_encoded, err });
            continue;
        };
        defer gpa.free(path);

        indexDir(io, gpa, document_store, &seen, path) catch |err| {
            std.log.warn("failed to index workspace folder {s}: {t}", .{ folder_uri.percent_encoded, err });
        };
    }
}

fn indexDir(
    io: Io,
    gpa: Allocator,
    document_store: *DocumentStore,
    seen: *std.AutoHashMap(Io.File.INode, void),
    dir_path: []const u8,
) !void {
    var dir = try Io.Dir.cwd().openDir(io, dir_path, .{ .iterate = true });
    defer dir.close(io);

    const stat = try dir.stat(io);
    if (try seen.fetchPut(stat.inode, {})) |_| return;

    var dir_it = dir.iterate();
    while (try dir_it.next(io)) |entry| {
        if (std.mem.startsWith(u8, entry.name, ".")) continue;

        switch (entry.kind) {
            .directory => {
                const child_path = try std.fs.path.join(gpa, &.{ dir_path, entry.name });
                defer gpa.free(child_path);
                indexDir(io, gpa, document_store, seen, child_path) catch |err| {
                    std.log.warn("failed to index directory {s}: {t}", .{ child_path, err });
                };
            },
            .file => {
                if (!std.mem.endsWith(u8, entry.name, ".q") and
                    !std.mem.endsWith(u8, entry.name, ".k")) continue;

                const file_path = try std.fs.path.join(gpa, &.{ dir_path, entry.name });
                defer gpa.free(file_path);

                const uri = try Uri.fromPath(gpa, file_path);
                defer uri.deinit(gpa);

                // `.never`: documents already open in the editor or already
                // indexed are left untouched.
                document_store.loadDocumentFromDisk(uri, .never) catch |err| {
                    std.log.warn("failed to index file {s}: {t}", .{ file_path, err });
                };
            },
            else => {},
        }
    }
}

test indexWorkspaceFolders {
    const gpa = std.testing.allocator;
    const io = std.testing.io;

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.writeFile(io, .{ .sub_path = "a.q", .data = "g:1" });
    try tmp.dir.createDirPath(io, "sub");
    try tmp.dir.writeFile(io, .{ .sub_path = "sub/b.q", .data = "g+1" });
    try tmp.dir.writeFile(io, .{ .sub_path = "sub/c.k", .data = "h:2" });
    try tmp.dir.writeFile(io, .{ .sub_path = "ignored.txt", .data = "nope" });
    try tmp.dir.createDirPath(io, ".hidden");
    try tmp.dir.writeFile(io, .{ .sub_path = ".hidden/d.q", .data = "nope:1" });

    var path_buffer: [std.fs.max_path_bytes]u8 = undefined;
    const path_len = try tmp.dir.realPath(io, &path_buffer);
    const tmp_path = path_buffer[0..path_len];

    const folder_uri = try Uri.fromPath(gpa, tmp_path);
    defer folder_uri.deinit(gpa);

    var store: DocumentStore = .{ .io = io, .gpa = gpa };
    defer store.deinit();

    indexWorkspaceFolders(io, gpa, &store, &.{folder_uri});

    const handles = try store.collectHandles(gpa);
    defer gpa.free(handles);
    try std.testing.expectEqual(3, handles.len);

    var file_path_buffer: [std.fs.max_path_bytes]u8 = undefined;
    const a_path = try std.fmt.bufPrint(&file_path_buffer, "{s}/a.q", .{tmp_path});
    const a_uri = try Uri.fromPath(gpa, a_path);
    defer a_uri.deinit(gpa);
    const a_handle = store.getHandle(a_uri) orelse return error.TestExpectedHandle;
    try std.testing.expectEqualStrings("g:1", a_handle.tree.source);
    try std.testing.expect(!a_handle.isLspSynced());
}

test {
    std.testing.refAllDecls(@This());
}
