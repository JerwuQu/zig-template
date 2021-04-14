# zig-template

comptime templates

A work in progress, but any possible errors will arise at compile-time.

## Usage

`const template = comptime Template.compile("Hello, {.}!", .{});`

`template.run(writer, arg);`

## Commands

- `{.path}` - replaced by contents of `path`, can be `.a.nested.path`
- `{escape .path}` - same as above but html escaped
- `{template name}` - nest another template called `name`
- `{scope .path} ... {/}` - scope current context to `path`
- `{if .path} ... {/}` - only render content if `path` is true (if a bool) or not null (if an option) 
- `{unless .path} ... {/}` - only render content if `path` is false (if a bool) or null (if an option) 
- `{for name in .path} ... {/}` - for each item in `path`, output content. `name` will be available in the context
