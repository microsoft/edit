# LSH Definitions

This directory contains syntax highlighting definitions.
Each `.lsh` file describes how to highlight a particular file type.
The compiler turns these into bytecode that the runtime executes line-by-line against input text.

## Quick tour of the language

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
`#[path]` is a glob pattern and you can have as many as you want.
Functions without `pub` are internal helper functions.

## How execution works

The runtime feeds one line at a time.
At the start of each line your function resumes where it left off - just like a coroutine.
Returning from your function resumes it from the top on the next line.

Within a line, matching is strictly left-to-right.
Each `if /regex/` tries to match at the current position.
On success the position advances past the match; on failure nothing moves and the `else` branch (if any) runs instead.

`yield <kind>` emits a highlight span: everything between the previous yield and this one gets colored with `<kind>`.
The kinds are dotted identifiers like `comment`, `string`, `keyword.control`, `constant.numeric`, `markup.bold`, etc.
They're interned at compile time - you can invent new ones, but the editor has to know what color to assign them.

`yield other` is the default/unhighlighted kind.
Sprinkle it around your regex chains to reset the highlight state between tokens - see [json.lsh](json.lsh) for a typical pattern.

## Multi-line constructs

Most definitions process one line at a time implicitly.
For things that span lines (e.g. block comments), use `loop` (or `until`) and `await input`:

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

`await input` tells the runtime "I'm done with this line, give me the next one."
If there's still unconsumed text on the current line it's a no-op and execution continues immediately.
Note that you still need to `yield` colors before `await input` as the remaining line remains uncolored otherwise.

## Control flow

| Expression | What it does |
|------------|--------------|
| `if /pat/ { ... }` | Match regex at current position, enter block on success |
| `else if /pat/ { ... }` | Chain of alternatives |
| `else { ... }` | Alternative |
| `loop { ... }` | Loop forever (use `break`/`continue`/`return`) |
| `until /pat/ { ... }` | Loop body until the regex matches (then consume the match and exit) |
| `break` | Exit innermost loop |
| `continue` | Restart innermost loop |
| `return` | Exit the current function |

`until /$/ { ... }` is the idiomatic way to say "process tokens until end-of-line".

## Capture groups

Regexes can have capture groups.
Use `yield $N as <kind>` to highlight just the captured portion:

```rs
if /([\w:.-]+)\s*=/ {
    yield $1 as variable;
    yield other;
}
```

Everything matched by the full regex is consumed, but only group `$1` gets the `variable` highlight.
The rest falls through to whatever yield follows.

## Variables

You can capture the current input offset into a variable and compare against it later:

```rs
var indentation = off;
// ...later...
if off <= indentation {
    break;
}
```

`off` is the built-in register tracking the current position in the line.
This is how [yaml.lsh](yaml.lsh) detects when a multi-line string ends.

## Calling other definitions

Functions can call each other. This is how [markdown.lsh](markdown.lsh) delegates code blocks:

```rs
if /(?i:json)/ {
    loop {
        await input;
        if /\s*```/ { return; }
        else { json(); if /.*/ {} }
    }
}
```

Because the called function might not consume the entire line, the `if /.*/ {}` ensures to consume the remaining text before the `await input`.
