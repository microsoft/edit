# LSH

LSH is a custom syntax highlighting engine and compiler designed for `edit`.
It defines a procedural DSL for writing syntax highlighters, which are then compiled into bytecode to be executed by a minimal VM (hundreds of LOC at most).

## Architecture

The system consists of three main parts:

1. Definitions (`definitions/*.lsh`): High-level syntax rules written in the LSH DSL.
2. Compiler (`src/compiler/`): A multi-stage compiler that transforms `.lsh` source into an intermediate representation (IR), optimizes it, and prepares it for code generation.
  - Tokenizer: Lexes the DSL.
  - Frontend: Parses tokens into an IR.
  - Optimizer: Optimizes the IR (e.g., merging states).
  - Backend: Generates the final assembly/bytecode structure.
3. Generator (`src/generator.rs`): Orchestrates the compilation and emits the final Rust code used by the main `edit` crate.

### Priorities

1. Most importantly: The VM that runs the resulting bytecode should not need complex dependencies. It should only require hundreds of LOC to implement.
2. Otherwise, this crate, its language, the compiler, and their respective needs are considered authorative for how the bytecode looks like. If a new instruction needs to be added or modified to make something work well, so be it.
3. The DSL doesn't need to be universally powerful, it just needs to be powerful enough. A simple compiler is more important.

## The LSH DSL

- `/regex/` are first-class citizens used for matching input.
- `yield <kind>;` emits a syntax token of the given kind (e.g., `yield keyword;`).
- `await input;` suspends the VM if it's out of input. This can be used for parsing blocks spanning multiple lines.
- `until /regex/ { ... }` loops until the regex matches.
- `if`, `else`, `loop`, `break`, and `continue`

### Example (`json.lsh`)

```cs
pub fn json() {
    until /$/ {
        if /\/\/.*/ {
            yield comment;
        } else if /"/ {
            until /"/ {}
            yield string;
        }
    }
}
```

## Development

### Adding/Modifying Syntax Rules

1. Edit or create a file in `crates/lsh/definitions/`.
2. `cargo run -p lsh -- <path>` can be used to test the compilation.

### Directory Structure

- `definitions/`: Source `.lsh` files.
- `src/compiler/`: Compiler implementation (Tokenizer, Parser, Optimizer).
- `src/generator.rs`: Rust code generation logic.
- `src/bin/lsh-gen.rs`: CLI entry point for the generator.

## Conventions

- Like the rest of the project, `lsh` relies heavily on `stdext::arena` for memory management during compilation to ensure performance and simple lifetime management.
