const std = @import("std");
const Allocator = std.mem.Allocator;

const kdb = @import("kdb");
const Ast = kdb.Ast;

const DocumentScope = @This();

scopes: std.MultiArrayList(Scope) = .empty,

pub fn init(gpa: Allocator, tree: Ast) !DocumentScope {
    var document_scope: DocumentScope = .{};
    errdefer document_scope.deinit(gpa);

    var context: ScopeContext = .{
        .allocator = gpa,
        .tree = tree,
        .doc_scope = &document_scope,
    };
    defer context.deinit();
    // try walkFile(&context, tree, 0);

    return document_scope;
}

pub fn deinit(scope: *DocumentScope, gpa: Allocator) void {
    scope.scopes.deinit(gpa);
}

pub const Scope = struct {};

pub const ScopeContext = struct {};
