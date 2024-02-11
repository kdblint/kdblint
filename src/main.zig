const std = @import("std");
const lsp = @import("lsp");
const types = lsp.types;
const DocumentStore = @import("DocumentStore.zig");
const diff = @import("diff.zig");
const Server = @import("Server.zig");
const diagnostics = @import("features/diagnostics.zig");

const log = std.log.scoped(.kdbLint);

// TODO: Tracy instrumentation!

pub const Connection = lsp.Connection(std.fs.File.Reader, std.fs.File.Writer, Context);

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const allocator = gpa.allocator();

    const server = try Server.create(allocator);
    defer server.destroy();

    var context: Context = .{
        .server = server,
    };

    const reader = std.io.getStdIn().reader();
    const writer = std.io.getStdOut().writer();

    var conn = Connection.init(allocator, reader, writer, &context);
    defer conn.callback_map.deinit(allocator);
    defer conn.write_buffer.deinit(allocator);

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    while (true) {
        const arena_allocator = arena.allocator();
        conn.accept(arena_allocator) catch return;

        while (server.job_queue.readItem()) |job| {
            server.processJob(job);
        }
    }
}

const Context = struct {
    server: *Server,

    pub fn initialize(conn: *Connection, _: types.RequestId, value: types.InitializeParams) !types.InitializeResult {
        log.debug("initialize", .{});
        return conn.context.server.initialize(value);
    }

    pub fn initialized(conn: *Connection, _: types.InitializedParams) !void {
        log.debug("initialized", .{});
        try conn.context.server.initialized(conn);
    }

    pub fn shutdown(conn: *Connection, _: types.RequestId, _: void) !?void {
        log.debug("shutdown", .{});
        return conn.context.server.shutdown();
    }

    pub fn exit(conn: *Connection, _: void) !void {
        log.debug("exit", .{});
        try conn.context.server.exit();
    }

    pub fn @"textDocument/didOpen"(conn: *Connection, value: types.DidOpenTextDocumentParams) !void {
        log.debug("textDocument/didOpen {s}", .{value.textDocument.uri});
        conn.context.server.start = diagnostics.now();
        try conn.context.server.@"textDocument/didOpen"(value);
    }

    pub fn @"textDocument/didChange"(conn: *Connection, value: types.DidChangeTextDocumentParams) !void {
        log.debug("textDocument/didChange {s}", .{value.textDocument.uri});
        conn.context.server.start = diagnostics.now();
        try conn.context.server.@"textDocument/didChange"(value);
    }

    pub fn @"textDocument/didSave"(conn: *Connection, value: types.DidSaveTextDocumentParams) !void {
        log.debug("textDocument/didSave {s}", .{value.textDocument.uri});
        conn.context.server.start = diagnostics.now();
        try conn.context.server.@"textDocument/didSave"(value);
    }

    pub fn @"textDocument/didClose"(conn: *Connection, value: types.DidCloseTextDocumentParams) !void {
        log.debug("textDocument/didClose {s}", .{value.textDocument.uri});
        conn.context.server.start = diagnostics.now();
        try conn.context.server.@"textDocument/didClose"(value);
    }

    pub fn @"textDocument/formatting"(conn: *Connection, _: types.RequestId, value: types.DocumentFormattingParams) !?[]types.TextEdit {
        log.debug("textDocument/formatting {s}", .{value.textDocument.uri});
        conn.context.server.start = diagnostics.now();
        var arena_allocator = std.heap.ArenaAllocator.init(conn.allocator);
        defer arena_allocator.deinit();
        return conn.context.server.@"textDocument/formatting"(arena_allocator.allocator(), value);
    }
};

test {
    @import("std").testing.refAllDecls(@This());
}
