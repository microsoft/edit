# lsh

`lsh` contains the compiler and runtime for Edit's syntax-highlighting system.

At a high level:
* Language definitions live in `definitions/*.lsh`
* The compiler lowers them into bytecode
* The runtime executes the bytecode on the input text line by line

To understand the definition language itself, read [definitions/README.md](definitions/README.md).

For debugging and optimizing language definitions use `lsh-bin`.
To see the generated assembly, for example:
```sh
# Show the generated assembly of a file or directory
cargo run -p lsh-bin -- assembly crates/lsh/definitions/diff.lsh

# Due to the lack of include statements, you must specify included files manually.
# Here, git_commit.lsh implicitly relies on diff() from diff.lsh.
cargo run -p lsh-bin -- assembly crates/lsh/definitions/git_commit.lsh crates/lsh/definitions/diff.lsh
```

Or to render a file:
```sh
cargo run -p lsh-bin -- render --input assets/highlighting-tests/html.html crates/lsh/definitions
```
