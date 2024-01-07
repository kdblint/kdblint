//! Represents in-progress parsing, will be converted to an Ast after completion.

pub const Error = error{ParseError} || Allocator.Error;

gpa: Allocator,
source: []const u8,
token_tags: []const Token.Tag,
token_starts: []const Ast.ByteOffset,
tok_i: TokenIndex,
errors: std.ArrayListUnmanaged(AstError),
nodes: Ast.NodeList,
extra_data: std.ArrayListUnmanaged(Node.Index),
scratch: std.ArrayListUnmanaged(Node.Index),

/// NYI
pub fn parseRoot(p: *Parse) !void {
    _ = p;
}

const Parse = @This();
const std = @import("std");
const Allocator = std.mem.Allocator;
const kdb = @import("../kdb.zig");
const Ast = kdb.Ast;
const Node = Ast.Node;
const AstError = Ast.Error;
const TokenIndex = Ast.TokenIndex;
const Token = kdb.Token;

test {
    @import("std").testing.refAllDecls(@This());
}
