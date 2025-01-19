const std = @import("std");

const zls = @import("zls");
const lsp = zls.lsp;
const types = lsp.types;

const Server = @import("../Server.zig");

pub const GotoKind = enum {
    declaration,
    definition,
    type_definition,
};

pub fn gotoHandler(
    server: *Server,
    arena: std.mem.Allocator,
    kind: GotoKind,
    request: types.DefinitionParams,
) Server.Error!lsp.ResultType("textDocument/definition") {
    _ = request; // autofix
    _ = kind; // autofix
    _ = arena; // autofix
    _ = server; // autofix
    return null;
}
