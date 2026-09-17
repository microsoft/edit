# LSH Definitions

This directory contains syntax highlighting definitions.
Each `.lsh` file describes how to highlight one or more file types.
The compiler turns these definitions into bytecode, and the runtime executes that bytecode against the input one line at a time.

Essentially, LSH is a small, line-oriented coroutine language for writing lexers.

## The basic idea

Most definitions follow the same pattern:
* Select a definition by file name or path
* Walk the current line from left to right
* Try regexes at the current position
* `yield` highlight kinds as tokens are recognized
* Use `await input` only when a construct needs to continue onto the next line

## A minimal definition

A definition is a `pub fn` with attributes that tell the editor when to use it:

```rs
#[display_name = "Diff"]
#[path = "**/*.diff"]
#[path = "**/*.patch"]
pub fn diff() {
    if /(?:diff|---|\+\+\+).*/ {
        yield meta.header;
    } else if /-.*/ {
        yield markup.deleted;
    } else if /\+.*/ {
        yield markup.inserted;
    }
}
```

`#[display_name]` sets the human-readable name.
`#[path]` is a glob pattern; you can have as many as you need.
Functions without `pub` are private helpers that can be called from other definitions.

## How execution works

The runtime feeds input to a definition one line at a time.
Within a line, matching is always left to right.

Each `if /regex/` tries to match at the current position:
* On success, the input position advances past the match and the block runs
* On failure, the input position does not move and the `else` branch, if any, runs

Definitions behave like coroutines:
* If execution reaches `await input`, the function suspends and resumes on the next line
* If the function returns, the next line starts again from the top of the function

## Highlighting with `yield`

`yield <kind>` emits a highlight span.
Everything between the previous `yield` and the current position is colored with `<kind>`.

> [!NOTE]
> This can be confusing in practice, because `yield` does not just color the regex it appears in.
> Long term, the goal is for `yield` to apply only to the regex it appears in, or to some other explicitly specified range.

Highlight kinds are dotted identifiers such as `comment`, `string`, `keyword.control`, `constant.numeric`, and `markup.bold`.
Kinds are interned at compile time. You can invent new ones, but the editor still needs to know what color to assign them.

`yield other` switches back to the default, unhighlighted kind.
Use it when you want to reset the current highlight between tokens. See [json.lsh](json.lsh) for a representative pattern.

## Multi-line constructs

Single-line constructs need no special handling.
For constructs that can span lines, such as block comments or fenced code blocks, combine `loop` or `until` with `await input`:

```rs
if /\/\*/ {
    loop {
        yield comment;
        await input;
        if /\*\// {
            yield comment;
            break;
        }
    }
}
```

`await input` means "advance to the next line if there is no more input to consume here."
If there is still unconsumed text on the current line, it is a no-op and execution continues immediately.

One important detail: if you want the remainder of the current line to stay highlighted, emit the appropriate `yield` before `await input`.

## Control flow

| Expression | Meaning |
|------------|---------|
| `if /pat/ { ... }` | Match `pat` at the current position and enter the block on success |
| `else if /pat/ { ... }` | Try another pattern if the previous one failed |
| `else { ... }` | Fallback branch |
| `loop { ... }` | Loop until `break`, `continue`, or `return` |
| `until /pat/ { ... }` | Repeat the body until `pat` matches, then consume the match and exit |
| `break` | Exit the innermost loop |
| `continue` | Restart the innermost loop |
| `return` | Exit the current function |

`until /$/ { ... }` is the usual way to say "keep processing until end-of-line."

## Capture groups

Regexes can have capture groups.
Use `yield $N as <kind>` when only part of the match should receive a specific highlight:

```rs
if /([\w:.-]+)\s*=/ {
    yield $1 as variable;
    yield other;
}
```

The full regex match is still consumed.
Only capture group `$1` receives the `variable` highlight; everything else falls through to the following `yield`.

## Variables and the input position

You can store the current input offset in a variable and compare against it later:

```rs
var indentation = off;
// ...later...
if off <= indentation {
    break;
}
```

`off` is the built-in register for the current position in the line.
[yaml.lsh](yaml.lsh) uses this pattern to detect when a multi-line string ends.

## Calling other definitions

Definitions can call helper functions or other definitions.
This is how [markdown.lsh](markdown.lsh) delegates the contents of fenced code blocks:

```rs
if /(?i:json)/ {
    loop {
        await input;
        if /\s*```/ { return; }
        else { json(); if /.*/ {} }
    }
}
```

The `if /.*/ {}` at the end consumes any text that the nested definition did not consume itself.
Without that final match, `await input` would see remaining input on the current line and continue immediately instead of advancing to the next line.
