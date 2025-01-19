const std = @import("std");
const builtin = @import("builtin");

const kdblint_version: std.SemanticVersion = .{ .major = 0, .minor = 1, .patch = 0 };

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const options = try getOptions(b);

    const kdb = b.dependency("kdb", .{});
    const kdb_module = kdb.module("kdb");

    const zls = b.dependency("zls", .{});
    const zls_module = zls.module("zls");
    const tracy_module = zls.module("tracy");

    const known_folders = zls.builder.dependency("known_folders", .{});
    const known_folders_module = known_folders.module("known-folders");

    const exe_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const exe = b.addExecutable(.{
        .name = if (optimize == .Debug) "kdblint.Debug" else "kdblint",
        .root_module = exe_mod,
    });
    exe.root_module.addImport("kdb", kdb_module);
    exe.root_module.addImport("zls", zls_module);
    exe.root_module.addImport("tracy", tracy_module);
    exe.root_module.addImport("known_folders", known_folders_module);
    exe.root_module.addOptions("build_options", options);

    const install_exe = b.addInstallArtifact(exe, .{
        .dest_dir = .{
            .override = .{
                .custom = b.fmt("../dist/{s}/{s}", .{
                    @tagName(target.result.os.tag), @tagName(target.result.cpu.arch),
                }),
            },
        },
    });
    b.getInstallStep().dependOn(&install_exe.step);
    if (target.result.os.tag == builtin.target.os.tag and target.result.cpu.arch == builtin.target.cpu.arch) {
        b.installArtifact(exe);
    }

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const test_filters = b.option(
        []const []const u8,
        "test-filter",
        "Skip tests that do not match any filter",
    ) orelse &[0][]const u8{};

    const unit_tests = b.addTest(.{
        .name = "lsp",
        .root_module = exe_mod,
        .filters = test_filters,
    });
    unit_tests.root_module.addImport("kdb", kdb_module);
    unit_tests.root_module.addImport("zls", zls_module);
    unit_tests.root_module.addImport("tracy", tracy_module);
    unit_tests.root_module.addImport("known_folders", known_folders_module);
    unit_tests.root_module.addOptions("build_options", options);

    const run_unit_tests = b.addRunArtifact(unit_tests);

    const kdb_unit_tests = @import("kdb").addTest(
        kdb.builder,
        kdb_module,
        target,
        optimize,
        test_filters,
    );

    const run_kdb_unit_tests = b.addRunArtifact(kdb_unit_tests);

    const test_step = b.step("test", "Run all unit tests");
    test_step.dependOn(&run_unit_tests.step);
    test_step.dependOn(&run_kdb_unit_tests.step);

    const test_lsp_step = b.step("test-lsp", "Run lsp unit tests");
    test_lsp_step.dependOn(&run_unit_tests.step);

    const test_kdb_step = b.step("test-kdb", "Run kdb unit tests");
    test_kdb_step.dependOn(&run_kdb_unit_tests.step);
}

fn getOptions(b: *std.Build) !*std.Build.Step.Options {
    const options = b.addOptions();

    const version_slice = v: {
        if (!std.process.can_spawn) {
            std.debug.print("error: version info cannot be retrieved from git.", .{});
            std.process.exit(1);
        }
        const version_string = b.fmt("{d}.{d}.{d}", .{
            kdblint_version.major, kdblint_version.minor, kdblint_version.patch,
        });

        var code: u8 = undefined;
        const git_describe_untrimmed = b.runAllowFail(&[_][]const u8{
            "git",
            "-C",
            b.build_root.path orelse ".",
            "describe",
            "--match",
            "*.*.*",
            "--tags",
            "--abbrev=9",
        }, &code, .Ignore) catch {
            break :v version_string;
        };
        const git_describe = std.mem.trim(u8, git_describe_untrimmed, " \n\r");

        switch (std.mem.count(u8, git_describe, "-")) {
            0 => {
                // Tagged release version (e.g. 0.10.0).
                if (!std.mem.eql(u8, git_describe, version_string)) {
                    std.debug.print("kdblint version '{s}' does not match git tag '{s}'\n", .{
                        version_string, git_describe,
                    });
                    std.process.exit(1);
                }
                break :v version_string;
            },
            2 => {
                // Untagged development build (e.g. 0.10.0-dev.2025+ecf0050a9).
                var it = std.mem.splitScalar(u8, git_describe, '-');
                const tagged_ancestor = it.first();
                const commit_height = it.next().?;
                const commit_id = it.next().?;

                const ancestor_ver = try std.SemanticVersion.parse(tagged_ancestor);
                if (kdblint_version.order(ancestor_ver) != .gt) {
                    std.debug.print("kdblint version '{}' must be greater than tagged ancestor '{}'\n", .{
                        kdblint_version, ancestor_ver,
                    });
                    std.process.exit(1);
                }

                // Check that the commit hash is prefixed with a 'g' (a Git convention).
                if (commit_id.len < 1 or commit_id[0] != 'g') {
                    std.debug.print("Unexpected `git describe` output: {s}\n", .{git_describe});
                    break :v version_string;
                }

                // The version is reformatted in accordance with the https://semver.org specification.
                break :v b.fmt("{s}-dev.{s}+{s}", .{ version_string, commit_height, commit_id[1..] });
            },
            else => {
                std.debug.print("Unexpected `git describe` output: {s}\n", .{git_describe});
                break :v version_string;
            },
        }

        break :v "";
    };

    const version = try std.SemanticVersion.parse(version_slice);
    options.addOption(std.SemanticVersion, "version", version);

    const version_string = try b.allocator.dupeZ(u8, version_slice);
    options.addOption([:0]const u8, "version_string", version_string);

    return options;
}
