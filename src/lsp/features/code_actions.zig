const zls = @import("zls");
const types = zls.lsp.types;

/// To report server capabilities
pub const supported_code_actions: []const types.CodeActionKind = &.{
    .quickfix,
    .refactor,
    .source,
    .@"source.organizeImports",
    .@"source.fixAll",
};
