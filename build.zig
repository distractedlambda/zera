const std = @import("std");

const wasm_cpu_model = std.Target.Cpu.Model{
    .name = "generic",
    .llvm_name = "generic",
    .features = std.Target.wasm.featureSet(&.{
        .bulk_memory,
        .multivalue,
        .mutable_globals,
        .nontrapping_fptoint,
        .reference_types,
        .sign_ext,
        .simd128,
    }),
};

const wasm_freestanding_target = std.zig.CrossTarget{
    .cpu_arch = .wasm32,
    .cpu_model = .{ .explicit = &wasm_cpu_model },
    .os_tag = .freestanding,
};

const wasm_wasi_target = std.zig.CrossTarget{
    .cpu_arch = .wasm32,
    .cpu_model = .{ .explicit = &wasm_cpu_model },
    .os_tag = .wasi,
};

pub fn build(b: *std.Build) void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard optimization options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall. Here we do not
    // set a preferred release mode, allowing the user to decide how to optimize.
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "zera",
        // In this case the main source file is merely a path, however, in more
        // complicated build scripts, this could be a generated file.
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    const validate_utf8_module = b.addSharedLibrary(.{
        .name = "validate_utf8",
        .root_source_file = .{ .path = "src/test_modules/validate_utf8.zig" },
        .target = wasm_freestanding_target,
        .optimize = .ReleaseSmall,
    });

    const install_validate_utf8 = b.addInstallArtifact(validate_utf8_module);

    // This declares intent for the executable to be installed into the
    // standard location when the user invokes the "install" step (the default
    // step when running `zig build`).
    b.installArtifact(exe);

    // This *creates* a Run step in the build graph, to be executed when another
    // step is evaluated that depends on it. The next line below will establish
    // such a dependency.
    const run_cmd = b.addRunArtifact(exe);

    // By making the run step depend on the install step, it will be run from the
    // installation directory rather than directly from within the cache directory.
    // This is not necessary, however, if the application depends on other installed
    // files, this ensures they will be present and in the expected location.
    run_cmd.step.dependOn(b.getInstallStep());

    // This allows the user to pass arguments to the application in the build
    // command itself, like this: `zig build run -- arg1 arg2 etc`
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    // This creates a build step. It will be visible in the `zig build --help` menu,
    // and can be selected like this: `zig build run`
    // This will evaluate the `run` step rather than the default, which is "install".
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    // Creates a step for unit testing. This only builds the test executable
    // but does not run it.
    const unit_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    const run_unit_tests = b.addRunArtifact(unit_tests);
    run_unit_tests.step.dependOn(&install_validate_utf8.step);

    // Similar to creating the run step earlier, this exposes a `test` step to
    // the `zig build --help` menu, providing a way for the user to request
    // running the unit tests.
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests.step);
}
