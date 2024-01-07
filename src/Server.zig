const std = @import("std");
const lsp = @import("lsp");
const types = lsp.types;

const DocumentStore = @import("DocumentStore.zig");
const diff = @import("diff.zig");
const offsets = @import("offsets.zig");

const log = std.log.scoped(.kdbLint_Server);

const Server = @This();

allocator: std.mem.Allocator,
document_store: DocumentStore,
position_encoding: types.PositionEncodingKind = .@"utf-16",

pub const Error = error{
    OutOfMemory,
    // ParseError,
    // InvalidRequest,
    // MethodNotFound,
    // InvalidParams,
    InternalError,
};

pub fn create(allocator: std.mem.Allocator) !*Server {
    const server = try allocator.create(Server);
    errdefer server.destroy();
    server.* = .{
        .allocator = allocator,
        .document_store = .{
            .allocator = allocator,
        },
    };
    return server;
}

pub fn destroy(server: *Server) void {
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

pub fn initialized(server: *Server) !void {
    _ = server;
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

    // TODO: Client capabilities - publish diagnostics
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

    // TODO: Client capabilities - publish diagnostics
}

pub fn @"textDocument/didSave"(server: *Server, notification: types.DidSaveTextDocumentParams) !void {
    _ = server;
    _ = notification;
}

pub fn @"textDocument/didClose"(server: *Server, notification: types.DidCloseTextDocumentParams) !void {
    server.document_store.closeDocument(notification.textDocument.uri);
}

test {
    @import("std").testing.refAllDecls(@This());
}
