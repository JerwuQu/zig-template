const std = @import("std");
const testing = std.testing;

const CommandBlock = []const Command;
const FieldPath = []const []const u8;
const Command = union(enum) {
    data: []const u8,
    template: anytype,
    prop: struct {
        htmlEscape: bool = false,
        fieldPath: FieldPath,
    },
    scope: struct {
        fieldPath: FieldPath,
        block: CommandBlock,
    },
    condition: struct {
        invert: bool,
        fieldPath: FieldPath,
        block: CommandBlock,
    },
    iterator: struct {
        name: []const u8,
        fieldPath: FieldPath,
        block: CommandBlock,
    },
};

fn hasField(comptime T: type, comptime name: []const u8) bool {
    const info = @typeInfo(T);
    return @hasField(if (info == .Optional) info.Optional.child else T, name);
}

// This is a bit of a workaround because zig doesn't have return type inference
// See https://github.com/ziglang/zig/issues/447
fn walkFieldType(comptime path: []const []const u8, comptime ctx1T: type, comptime ctx2T: type) type {
    if (path.len == 0) {
        return ctx1T;
    }

    const T = if (hasField(ctx1T, path[0])) ctx1T else ctx2T;
    const info = @typeInfo(T);
    const ctxVal: if (info == .Optional) info.Optional.child else T = undefined;
    return walkFieldType(path[1..], @TypeOf(@field(ctxVal, path[0])), void);
}

fn walkField(comptime path: []const []const u8, ctx1: anytype, ctx2: anytype) walkFieldType(path, @TypeOf(ctx1), @TypeOf(ctx2)) {
    if (path.len == 0) {
        return ctx1;
    }

    // Will prioritise ctx1 over ctx2
    const ctx = comptime if (hasField(@TypeOf(ctx1), path[0])) ctx1 else ctx2;
    const info = @typeInfo(@TypeOf(ctx));
    const ctxVal = if (info == .Optional) ctx.? else ctx;
    return walkField(path[1..], @field(ctxVal, path[0]), .{});
}

fn htmlEscape(writer: anytype, str: []const u8) !void {
    var iter: std.unicode.Utf8Iterator = .{ .bytes = str, .i = 0 };
    while (iter.nextCodepointSlice()) |cps| {
        _ = try writer.write(
            if (std.mem.eql(u8, cps, "<")) "&lt;"
            else if (std.mem.eql(u8, cps, ">")) "&gt;"
            else if (std.mem.eql(u8, cps, "&")) "&amp;"
            else cps
        );
    }
}

fn runCommandBlock(comptime block: CommandBlock, writer: anytype, ctx1: anytype, ctx2: anytype) !void {
    inline for (block) |cmd| {
        switch (cmd) {
            .data => |data| _ = try writer.write(data),
            .template => |tmpl| try tmpl.runContext(writer, ctx1, ctx2),
            .prop => |prop| {
                const val = walkField(prop.fieldPath, ctx1, ctx2);
                const valType = @TypeOf(val);
                const valTypeInfo = @typeInfo(valType);

                if (valTypeInfo == .Pointer) {
                    if (prop.htmlEscape) {
                        try htmlEscape(writer, val);
                    } else {
                        _ = try writer.write(val);
                    }
                } else if (valTypeInfo == .Int or valTypeInfo == .ComptimeInt) {
                    try std.fmt.format(writer, "{d}", .{val});
                } else {
                    @compileError("unknown type '" ++ @typeName(valType) ++ "'");
                }
            },
            .scope => |scope| {
                try runCommandBlock(scope.block, writer, walkField(scope.fieldPath, ctx1, ctx2), .{});
            },
            .condition => |cond| {
                const res = walkField(cond.fieldPath, ctx1, ctx2);
                const resInfo = @typeInfo(@TypeOf(res));
                if (resInfo == .Bool) {
                    if (res != cond.invert) {
                        try runCommandBlock(cond.block, writer, ctx1, ctx2);
                    }
                } else if (resInfo == .Optional) {
                    if ((res == null) == cond.invert) {
                        try runCommandBlock(cond.block, writer, ctx1, ctx2);
                    }
                } else {
                    @compileError("unknown conditional type '" ++ @TypeOf(res) ++ "'");
                }
            },
            .iterator => |iter| {
                const values = walkField(iter.fieldPath, ctx1, ctx2);
                for (values) |value| {
                    // This is very hacky
                    // Clones ctx2 struct and adds field named `iter.name` with value `value`
                    const fields = @typeInfo(@TypeOf(ctx2)).Struct.fields
                            ++ &[_]std.builtin.TypeInfo.StructField{
                        .{
                            .name = iter.name,
                            .field_type = @TypeOf(value),
                            .default_value = null,
                            .is_comptime = false,
                            .alignment = 0,
                        }
                    };
                    var newCtx2: @Type(std.builtin.TypeInfo {
                        .Struct = .{
                            .is_tuple = false,
                            .layout = .Auto,
                            .decls = &.{},
                            .fields = fields,
                        },
                    }) = undefined;

                    @field(newCtx2, iter.name) = value;
                    for (std.meta.fields(@TypeOf(ctx2))) |field| {
                        @field(newCtx2, field.name) = @field(ctx2, field.name);
                    }

                    try runCommandBlock(iter.block, writer, ctx1, newCtx2);
                }
            },
        }
    }
}

fn splitSpaces(comptime str: []const u8) []const []const u8 {
    var out: []const []const u8 = &.{};
    var it = std.mem.split(str, " ");
    while (it.next()) |crumb| {
        if (crumb.len > 0) {
            out = out ++ &[1][]const u8{crumb};
        }
    }
    return out;
}

fn splitFieldPath(comptime str: []const u8) FieldPath {
    var out: FieldPath = &.{};
    if (str.len == 0 or str[0] != '.') {
        @compileError("invalid field path '" ++ str ++ "'");
    } else if (str.len == 1) {
        return out;
    }
    var it = std.mem.split(str[1..], ".");
    while (it.next()) |crumb| {
        if (crumb.len == 0) {
            @compileError("empty piece in field path");
        }
        out = out ++ &[1][]const u8{crumb};
    }
    return out;
}

fn parseCommand(iter: *std.unicode.Utf8Iterator, templates: anytype, cmdParts: []const []const u8) Command {
    if (cmdParts.len == 0) {
        @compileError("empty command");
    } else if (cmdParts.len == 1) {
        if (cmdParts[0][0] == '.') {
            return .{
                .prop = .{
                    .fieldPath = splitFieldPath(cmdParts[0]),
                },
            };
        }
    } else if (cmdParts.len == 2) {
        if (std.mem.eql(u8, cmdParts[0], "template")) {
            const tmpl = @field(templates, cmdParts[1]);
            if (@TypeOf(tmpl) != Template) {
                @compileError("expected template, got '" ++ @typeName(@TypeOf(tmpl)) ++ "'");
            }
            return .{ .template = tmpl };
        } else if (std.mem.eql(u8, cmdParts[0], "escape")) {
            return .{
                .prop = .{
                    .htmlEscape = true,
                    .fieldPath = splitFieldPath(cmdParts[1]),
                },
            };
        } else if (std.mem.eql(u8, cmdParts[0], "scope")) {
            return .{
                .scope = .{
                    .fieldPath = splitFieldPath(cmdParts[1]),
                    .block = parseBlock(iter, templates, true),
                },
            };
        } else if (std.mem.eql(u8, cmdParts[0], "if")) {
            return .{
                .condition = .{
                    .invert = false,
                    .fieldPath = splitFieldPath(cmdParts[1]),
                    .block = parseBlock(iter, templates, true),
                },
            };
        } else if (std.mem.eql(u8, cmdParts[0], "unless")) {
            return .{
                .condition = .{
                    .invert = true,
                    .fieldPath = splitFieldPath(cmdParts[1]),
                    .block = parseBlock(iter, templates, true),
                },
            };
        }
    } else if (cmdParts.len == 4) {
        if (std.mem.eql(u8, cmdParts[0], "for") and std.mem.eql(u8, cmdParts[2], "in")) {
            return .{
                .iterator = .{
                    .name = cmdParts[1],
                    .fieldPath = splitFieldPath(cmdParts[3]),
                    .block = parseBlock(iter, templates, true),
                },
            };
        }
    }
    @compileError("unknown command '" ++ cmdParts[0] ++ "'");
}

fn parseBlock(comptime iter: *std.unicode.Utf8Iterator, comptime templates: anytype, isNested: bool) CommandBlock {
    var escaped = false;
    var state: enum {
        data,
        bracket,
    } = .data;

    var block: CommandBlock = &.{};
    var curStr: []const u8 = "";
    while (iter.nextCodepointSlice()) |cps| {
        if (escaped) {
            escaped = false;
        } else if (std.mem.eql(u8, cps, "\\")) {
            escaped = true;
        } else if (state == .data and std.mem.eql(u8, cps, "{")) {
            if (curStr.len > 0) {
                block = block ++ [1]Command{.{ .data = curStr }};
                curStr = "";
            }
            state = .bracket;
        } else if (state == .bracket and std.mem.eql(u8, cps, "}")) {
            const cmdParts = splitSpaces(curStr);
            state = .data;
            curStr = "";

            // Stack pop
            if (cmdParts.len == 1 and std.mem.eql(u8, cmdParts[0], "/")) {
                if (!isNested) {
                    @compileError("unexpected {/}");
                }
                return block;
            }

            const cmd = parseCommand(iter, templates, cmdParts);
            block = block ++ [1]Command{cmd};
        } else {
            curStr = curStr ++ cps;
        }
    }
    if (isNested) {
        @compileError("unexpected end of template");
    }
    if (state == .data and curStr.len > 0) {
        block = block ++ [1]Command{.{ .data = curStr }};
    }
    return block;
}

pub const Template = struct {
    block: CommandBlock,

    pub fn compile(comptime template: []const u8, comptime templates: anytype) @This() {
        var iter: std.unicode.Utf8Iterator = .{ .bytes = template, .i = 0 };
        @setEvalBranchQuota(100000); // :)
        return .{ .block = parseBlock(&iter, templates, false) };
    }

    fn runContext(comptime self: *const @This(), writer: anytype, ctx1: anytype, ctx2: anytype) !void {
        try runCommandBlock(self.block, writer, ctx1, ctx2);
    }

    pub fn run(comptime self: *const @This(), writer: anytype, arg: anytype) !void {
        try self.runContext(writer, arg, .{});
    }

    pub fn runAlloc(comptime self: *const @This(), alloc: *std.mem.Allocator, arg: anytype) ![]const u8 {
        var str = std.ArrayList(u8).init(alloc);
        errdefer str.deinit();
        try self.run(str.writer(), arg);
        return str.toOwnedSlice();
    }
};

fn testTemplate(comptime template: Template, arg: anytype, expected: []const u8) void {
    const result = template.runAlloc(testing.allocator, arg) catch unreachable;
    defer testing.allocator.free(result);
    testing.expectEqualStrings(expected, result);
}

test "basic template" {
    const template = comptime Template.compile("Hello, {escape .}!", .{});
    testTemplate(template, "World", "Hello, World!");
    testTemplate(template, "<Other Name>", "Hello, &lt;Other Name&gt;!");
}

test "nested structs" {
    const template = comptime Template.compile("{.name}, {.child.name}, {.child.child.name}", .{});
    testTemplate(template, .{
        .name = "grandpa",
        .child = .{
            .name = "dad",
            .child = .{
                .name = "me",
            },
        },
    }, "grandpa, dad, me");
}

test "scopes" {
    const template = comptime Template.compile("{scope .child}Hello, {.name}!{/}", .{});
    testTemplate(template, .{ .child = .{ .name = "World" } }, "Hello, World!");
}

test "bool conditional" {
    const template = comptime Template.compile("{if .}Yes{/}{unless .}No{/}", .{});
    testTemplate(template, true, "Yes");
    testTemplate(template, false, "No");
}

test "for loop iterator" {
    const Item = struct { name: []const u8 };
    const template = comptime Template.compile("{for item in .items}[{.item.name}]{/}", .{});
    testTemplate(template, .{
        .items = [_]Item{
            .{ .name = "A" },
            .{ .name = "B" },
            .{ .name = "C" },
        },
    }, "[A][B][C]");
}

test "nested templates" {
    const Person = struct {
        name: []const u8,
        accounts: []const struct {
            name: []const u8,
            amount: i32,
        },
    };

    const accountTemplate = comptime Template.compile("Account: {escape .name} - Amount: {.amount}", .{});
    const personTemplate = comptime Template.compile(
        \\Name: {escape .name}
        \\{for account in .accounts}{scope .account}{template Account}{/}
        \\{/}
    , .{ .Account = accountTemplate });
    const template = comptime Template.compile(
        \\{for person in .people}{scope .person}{template Person}{/}
        \\{/}
    , .{ .Person = personTemplate });

    testTemplate(template, .{
        .people = [_]Person{
            .{
                .name = "Alice",
                .accounts = &.{
                    .{ .name = "main", .amount = 321 },
                    .{ .name = "savings", .amount = 321321 },
                },
            },
            .{
                .name = "Bob",
                .accounts = &.{
                    .{ .name = "main", .amount = 123 },
                    .{ .name = "savings", .amount = 123123 },
                },
            },
        },
    },
        \\Name: Alice
        \\Account: main - Amount: 321
        \\Account: savings - Amount: 321321
        \\
        \\Name: Bob
        \\Account: main - Amount: 123
        \\Account: savings - Amount: 123123
        \\
        \\
    );
}
