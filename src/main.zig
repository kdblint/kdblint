const std = @import("std");
const builtin = @import("builtin");
const zls = @import("zls");
const types = zls.types;
const tracy = @import("tracy");
const DocumentStore = @import("DocumentStore.zig");
const Server = @import("Server.zig");
const diagnostics = @import("features/diagnostics.zig");

const log = std.log.scoped(.kdblint);

const ParseArgsResut = struct {
    action: enum { proceed, exit },
    config_path: ?[]const u8,
    message_tracing_enabled: bool,

    kdblint_exe_path: []const u8,

    fn deinit(self: ParseArgsResut, allocator: std.mem.Allocator) void {
        defer if (self.config_path) |path| allocator.free(path);
        defer allocator.free(self.kdblint_exe_path);
    }
};

fn parseArgs(allocator: std.mem.Allocator) !ParseArgsResut {
    var result = ParseArgsResut{
        .action = .exit,
        .config_path = null,
        .message_tracing_enabled = false,
        .kdblint_exe_path = "",
    };
    errdefer result.deinit(allocator);

    result.action = .proceed;

    return result;
}

pub fn main() !void {
    var allocator_state = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(allocator_state.deinit() == .ok);

    var tracy_state = if (tracy.enable_allocation) tracy.tracyAllocator(allocator_state.allocator()) else void{};
    const inner_allocator = if (tracy.enable_allocation) tracy_state.allocator() else allocator_state.allocator();

    const enable_failing_allocator = false;
    var failing_allocator_state = if (enable_failing_allocator) zls.debug.FailingAllocator.init(inner_allocator, 256) else void{};
    const allocator: std.mem.Allocator = if (enable_failing_allocator) failing_allocator_state.allocator() else inner_allocator;

    const result = try parseArgs(allocator);
    defer result.deinit(allocator);
    switch (result.action) {
        .proceed => {},
        .exit => return,
    }

    const message_tracing_enabled = false;
    var transport = zls.Transport.init(
        std.io.getStdIn().reader(),
        std.io.getStdOut().writer(),
    );
    transport.message_tracing = message_tracing_enabled;

    const server = try Server.create(allocator);
    defer server.destroy();
    server.transport = &transport;
    server.config_path = result.config_path;

    try server.loop();

    if (server.status == .exiting_failure) {
        if (builtin.mode == .Debug) {
            return;
        } else {
            std.process.exit(1);
        }
    }
}

// const Context = struct {
//     server: *Server,
//     arena_allocator: std.heap.ArenaAllocator = undefined,

//     pub fn initialize(conn: *Connection, _: types.RequestId, value: types.InitializeParams) !types.InitializeResult {
//         return conn.context.server.initialize(value);
//     }

//     pub fn initialized(conn: *Connection, _: types.InitializedParams) !void {
//         try conn.context.server.initialized(conn);
//     }

//     pub fn shutdown(conn: *Connection, _: types.RequestId, _: void) !?void {
//         return conn.context.server.shutdown();
//     }

//     pub fn exit(conn: *Connection, _: void) !void {
//         try conn.context.server.exit();
//     }

//     pub fn @"textDocument/didOpen"(conn: *Connection, value: types.DidOpenTextDocumentParams) !void {
//         try conn.context.server.@"textDocument/didOpen"(value);
//     }

//     pub fn @"textDocument/didChange"(conn: *Connection, value: types.DidChangeTextDocumentParams) !void {
//         try conn.context.server.@"textDocument/didChange"(value);
//     }

//     pub fn @"textDocument/didSave"(conn: *Connection, value: types.DidSaveTextDocumentParams) !void {
//         try conn.context.server.@"textDocument/didSave"(value);
//     }

//     pub fn @"textDocument/didClose"(conn: *Connection, value: types.DidCloseTextDocumentParams) !void {
//         try conn.context.server.@"textDocument/didClose"(value);
//     }

//     pub fn @"textDocument/formatting"(conn: *Connection, _: types.RequestId, value: types.DocumentFormattingParams) !?[]types.TextEdit {
//         return conn.context.server.@"textDocument/formatting"(conn.context.arena_allocator.allocator(), value);
//     }

//     pub fn lspRecvPre(conn: *Connection, comptime method: []const u8, _: lsp.MessageKind, _: ?types.RequestId, value: anytype) !void {
//         switch (@TypeOf(value)) {
//             void => {},
//             else => |T| {
//                 if (@typeName(T)[0] != '?' and @hasField(T, "textDocument")) {
//                     log.debug(method ++ " {s}", .{value.textDocument.uri});
//                 } else {
//                     log.debug(method, .{});
//                 }
//             },
//         }
//         conn.context.server.start = diagnostics.now();
//         conn.context.arena_allocator = std.heap.ArenaAllocator.init(conn.allocator);
//     }

//     pub fn lspRecvPost(conn: *Connection, comptime method: []const u8, _: lsp.MessageKind, _: ?types.RequestId, _: anytype) !void {
//         // TODO: Is this single-threaded?
//         conn.context.arena_allocator.deinit();

//         const duration: f64 = @as(f64, @floatFromInt(diagnostics.now().since(conn.context.server.start))) / std.time.ns_per_ms;
//         log.debug(method ++ " duration: {d:.2}ms", .{duration});
//     }
// };

test {
    @import("std").testing.refAllDecls(@This());
}
