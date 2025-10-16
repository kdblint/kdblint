const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const lib_mod = b.addModule("kdb", .{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    const test_filters = b.option(
        []const []const u8,
        "test-filter",
        "Skip tests that do not match any filter",
    ) orelse &.{};

    const lib_unit_tests = addTest(b, lib_mod, test_filters);
    const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_lib_unit_tests.step);
}

pub fn addTest(
    b: *std.Build,
    root_module: *std.Build.Module,
    test_filters: []const []const u8,
) *std.Build.Step.Compile {
    const lib_unit_tests = b.addTest(.{
        .name = "kdb",
        .root_module = root_module,
        .filters = test_filters,
    });

    const options = b.addOptions();
    options.addOptionPath("path", b.path("tests"));
    lib_unit_tests.root_module.addOptions("test_options", options);

    return lib_unit_tests;
}
