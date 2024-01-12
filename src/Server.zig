const std = @import("std");
const lsp = @import("lsp");
const types = lsp.types;

const Connection = @import("main.zig").Connection;
const DocumentStore = @import("DocumentStore.zig");
const diff = @import("diff.zig");
const offsets = @import("offsets.zig");
const diagnostics_gen = @import("features/diagnostics.zig");

const log = std.log.scoped(.kdbLint_Server);

const Server = @This();

allocator: std.mem.Allocator,
document_store: DocumentStore,
position_encoding: types.PositionEncodingKind = .@"utf-16",

/// private fields
conn: *Connection = undefined,
job_queue: std.fifo.LinearFifo(Job, .Dynamic),
job_queue_lock: std.Thread.Mutex = .{},
client_capabilities: ClientCapabilities = .{},

const ClientCapabilities = struct {
    supports_publish_diagnostics: bool = false,
    supports_hover: bool = false,
    hover_supports_md: bool = false,
};

pub const Error = error{
    OutOfMemory,
    // ParseError,
    // InvalidRequest,
    // MethodNotFound,
    // InvalidParams,
    InternalError,
};

const Job = union(enum) {
    generate_diagnostics: DocumentStore.Uri,

    pub fn deinit(self: Job, allocator: std.mem.Allocator) void {
        switch (self) {
            .generate_diagnostics => |uri| allocator.free(uri),
        }
    }
};

pub fn create(allocator: std.mem.Allocator) !*Server {
    const server = try allocator.create(Server);
    errdefer server.destroy();
    server.* = .{
        .allocator = allocator,
        .document_store = .{
            .allocator = allocator,
        },
        .job_queue = std.fifo.LinearFifo(Job, .Dynamic).init(allocator),
    };
    return server;
}

pub fn destroy(server: *Server) void {
    while (server.job_queue.readItem()) |job| job.deinit(server.allocator);
    server.job_queue.deinit();
    server.document_store.deinit();
    server.allocator.destroy(server);
}

pub fn initialize(server: *Server, request: types.InitializeParams) !types.InitializeResult {
    if (request.clientInfo) |clientInfo| {
        log.info("client is '{s}-{s}'", .{ clientInfo.name, clientInfo.version orelse "<no version>" });
    }

    if (request.capabilities.general) |general| {
        var supports_utf8 = false;
        var supports_utf16 = false;
        var supports_utf32 = false;
        if (general.positionEncodings) |position_encodings| {
            for (position_encodings) |encoding| {
                switch (encoding) {
                    .@"utf-8" => supports_utf8 = true,
                    .@"utf-16" => supports_utf16 = true,
                    .@"utf-32" => supports_utf32 = true,
                }
            }
        }

        if (supports_utf8) {
            server.position_encoding = .@"utf-8";
        } else if (supports_utf32) {
            server.position_encoding = .@"utf-32";
        } else {
            server.position_encoding = .@"utf-16";
        }

        log.info("encoding is {s}", .{@tagName(server.position_encoding)});
    }

    if (request.capabilities.textDocument) |textDocument| {
        server.client_capabilities.supports_publish_diagnostics = textDocument.publishDiagnostics != null;
        if (textDocument.hover) |hover| {
            server.client_capabilities.supports_hover = true;
            if (hover.contentFormat) |content_format| {
                for (content_format) |format| {
                    if (format == .plaintext) {
                        break;
                    }
                    if (format == .markdown) {
                        server.client_capabilities.hover_supports_md = true;
                        break;
                    }
                }
            }
        }
    }

    return types.InitializeResult{
        .capabilities = .{
            .positionEncoding = server.position_encoding,
            .textDocumentSync = .{ .TextDocumentSyncKind = types.TextDocumentSyncKind.Incremental },
            .notebookDocumentSync = .{ .NotebookDocumentSyncOptions = .{ .notebookSelector = &.{} } },
        },
        .serverInfo = .{
            .name = "kdbLint Language Server",
            .version = "0.1.0",
        },
    };
}

pub fn initialized(server: *Server, conn: *Connection) !void {
    server.conn = conn;
}

pub fn shutdown(server: *Server) !?void {
    _ = server;
}

pub fn exit(server: *Server) !void {
    _ = server;
}

pub fn @"textDocument/didOpen"(server: *Server, notification: types.DidOpenTextDocumentParams) Error!void {
    if (notification.textDocument.text.len > DocumentStore.max_document_size) {
        log.err("textDocument/didOpen {s} failed: text size ({d}) is greater than maximum length ({d})", .{
            notification.textDocument.uri,
            notification.textDocument.text.len,
            DocumentStore.max_document_size,
        });
        return error.InternalError;
    }

    try server.document_store.openDocument(notification.textDocument.uri, notification.textDocument.text);

    if (server.client_capabilities.supports_publish_diagnostics) {
        try server.pushJob(.{
            .generate_diagnostics = try server.allocator.dupe(u8, notification.textDocument.uri),
        });
    }
}

pub fn @"textDocument/didChange"(server: *Server, notification: types.DidChangeTextDocumentParams) !void {
    const handle = server.document_store.getHandle(notification.textDocument.uri) orelse return;
    const new_text = try diff.applyContentChanges(server.allocator, handle.tree.source, notification.contentChanges, server.position_encoding);

    if (new_text.len > DocumentStore.max_document_size) {
        log.err("textDocument/didChange {s} failed: text size ({d}) is greater than maximum length ({d})", .{
            notification.textDocument.uri,
            new_text.len,
            DocumentStore.max_document_size,
        });
        return error.InternalError;
    }

    try server.document_store.refreshDocument(handle.uri, new_text);

    if (server.client_capabilities.supports_publish_diagnostics) {
        try server.pushJob(.{
            .generate_diagnostics = try server.allocator.dupe(u8, notification.textDocument.uri),
        });
    }
}

pub fn @"textDocument/didSave"(server: *Server, notification: types.DidSaveTextDocumentParams) !void {
    _ = server;
    _ = notification;
}

pub fn @"textDocument/didClose"(server: *Server, notification: types.DidCloseTextDocumentParams) !void {
    server.document_store.closeDocument(notification.textDocument.uri);
}

pub fn processJob(server: *Server, job: Job) void {
    defer job.deinit(server.allocator);

    switch (job) {
        .generate_diagnostics => |uri| {
            const handle = server.document_store.getHandle(uri) orelse return;
            var arena_allocator = std.heap.ArenaAllocator.init(server.allocator);
            defer arena_allocator.deinit();
            const diagnostics = diagnostics_gen.generateDiagnostics(server, arena_allocator.allocator(), handle) catch return;
            log.info("publishing {d} diagnostic(s)", .{diagnostics.diagnostics.len});
            server.conn.notify("textDocument/publishDiagnostics", diagnostics) catch return;
        },
    }
}

/// takes ownership of `job`
fn pushJob(server: *Server, job: Job) error{OutOfMemory}!void {
    server.job_queue_lock.lock();
    defer server.job_queue_lock.unlock();
    server.job_queue.writeItem(job) catch |err| {
        job.deinit(server.allocator);
        return err;
    };
}

test {
    @import("std").testing.refAllDecls(@This());
}
