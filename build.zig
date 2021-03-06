const Builder = @import("std").build.Builder;

pub fn build(b: *Builder) void {
    const mode = b.standardReleaseOptions();

    var main_tests = b.addTest("template.zig");
    main_tests.setBuildMode(mode);

    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&main_tests.step);
}
