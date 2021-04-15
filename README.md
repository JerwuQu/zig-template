# zig-template

comptime templates

A work in progress, but any possible errors will arise at compile-time.

## Usage

```zig
const template = comptime Template.compile("Hello, {.}!", .{});
try template.run(writer, "World");
// "Hello, World!"
```

## Commands

| Command                       | Description                                                                                                |
|-------------------------------|------------------------------------------------------------------------------------------------------------|
| `{.path}`                     | Replaced by contents of `path`. Can be `.a.nested.path`. Passed through the escape function if one exists. |
| `{bypass .path}`              | Same as above but bypasses the escape function.                                                            |
| `{template name}`             | Nest another template called `name`.                                                                       |
| `{scope .path} ... {/}`       | Scope inner content context to `path`.                                                                     |
| `{if .path} ... {/}`          | Only output inner content if `path` is true (if a bool) or not null (if an option).                        |
| `{unless .path} ... {/}`      | Only output inner content if `path` is false (if a bool) or null (if an option).                           |
| `{for name in .path} ... {/}` | For each item in `path`, output inner content. `name` will be available in the context.                    |
