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
    arena_allocator: std.heap.ArenaAllocator = undefined,

    pub fn initialize(conn: *Connection, _: types.RequestId, value: types.InitializeParams) !types.InitializeResult {
        return conn.context.server.initialize(value);
    }

    pub fn initialized(conn: *Connection, _: types.InitializedParams) !void {
        try conn.context.server.initialized(conn);
    }

    pub fn shutdown(conn: *Connection, _: types.RequestId, _: void) !?void {
        return conn.context.server.shutdown();
    }

    pub fn exit(conn: *Connection, _: void) !void {
        try conn.context.server.exit();
    }

    pub fn @"textDocument/didOpen"(conn: *Connection, value: types.DidOpenTextDocumentParams) !void {
        try conn.context.server.@"textDocument/didOpen"(value);
    }

    pub fn @"textDocument/didChange"(conn: *Connection, value: types.DidChangeTextDocumentParams) !void {
        try conn.context.server.@"textDocument/didChange"(value);
    }

    pub fn @"textDocument/didSave"(conn: *Connection, value: types.DidSaveTextDocumentParams) !void {
        try conn.context.server.@"textDocument/didSave"(value);
    }

    pub fn @"textDocument/didClose"(conn: *Connection, value: types.DidCloseTextDocumentParams) !void {
        try conn.context.server.@"textDocument/didClose"(value);
    }

    pub fn @"textDocument/formatting"(conn: *Connection, _: types.RequestId, value: types.DocumentFormattingParams) !?[]types.TextEdit {
        return conn.context.server.@"textDocument/formatting"(conn.context.arena_allocator.allocator(), value);
    }

    pub fn lspRecvPre(conn: *Connection, comptime method: []const u8, _: lsp.MessageKind, _: ?types.RequestId, value: anytype) !void {
        switch (@TypeOf(value)) {
            void => {},
            else => |T| {
                if (@typeName(T)[0] != '?' and @hasField(T, "textDocument")) {
                    log.debug(method ++ " {s}", .{value.textDocument.uri});
                } else {
                    log.debug(method, .{});
                }
            },
        }
        conn.context.server.start = diagnostics.now();
        conn.context.arena_allocator = std.heap.ArenaAllocator.init(conn.allocator);
    }

    pub fn lspRecvPost(conn: *Connection, comptime method: []const u8, _: lsp.MessageKind, _: ?types.RequestId, _: anytype) !void {
        // TODO: Is this single-threaded?
        conn.context.arena_allocator.deinit();

        const duration: f64 = @as(f64, @floatFromInt(diagnostics.now().since(conn.context.server.start))) / std.time.ns_per_ms;
        log.debug(method ++ " duration: {d:.2}ms", .{duration});
    }
};

test {
    @import("std").testing.refAllDecls(@This());
}
