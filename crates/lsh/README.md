# lsh

`lsh` contains the compiler and runtime for Edit's syntax-highlighting system.

At a high level:
* Language definitions live in `definitions/*.lsh`
* The compiler lowers them into bytecode
* The runtime executes the bytecode on the input text line by line

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

## Language

See [definitions/README.md](definitions/README.md).

## Instruction Set

### Registers

The virtual machine has 16 32-bit registers, named `r0` to `r15`.
`r0` to `r2` currently have a fixed meaning:
* `r0` is `off`, which is the text input offset
* `r1` is `hs`, which describes the start of the next highlight range, emitted via a `yield` statement, corresponding to a `flush` instruction
* `r2` is `pc`, the program counter, aka instruction offset

Registers `r0` and `r1` are preserved between calls and `r2` to `r15` are caller saved.

> [!NOTE]
> `pc` is pre-incremented when processing instructions.
> For instance, `mov r15, pc` saves the address of the _next_ instruction.

### Instruction: mov, add, sub

`mov` assigns `src` to `dst`.
As one may expect, `add` and `sub` perform the corresponding `+=` and `-=` arithmetic.

Mnemonic:
```
mov dst, src
add dst, src
sub dst, src
```

Encoding:
```
 0               1
 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7
+---------------+-------+-------+
|    opcode     |  dst  |  src  |
+---------------+-------+-------+
   mov = 0x00
   add = 0x01
   sub = 0x02
```

### Instruction: movi, addi, subi

`movi`, `addi`, and `subi` are immediate variants of `mov`, `add`, and `sub`.
The `src` parameter is replaced with a fixed 32-bit constant.

Mnemonic:
```
movi dst, imm
addi dst, imm
subi dst, imm
```

Encoding:
```
 0               1               2               3
 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7
+---------------+-------+-------+-------+-------+-------+-------+
|    opcode     |  dst  |       |              imm              |
+---------------+-------+-------+-------+-------+-------+-------+
   movi = 0x03
   addi = 0x04
   subi = 0x05
```

### Instruction: call

`call` pushes `r2` to `r15` on the stack and jumps to `tgt`.

Mnemonic:
```
call tgt
```

Encoding:
```
call:
 0               1               2
 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7
+---------------+-------+-------+-------+-------+
|    opcode     |              tgt              |
+---------------+-------+-------+-------+-------+
   call = 0x06
```

### Instruction: ret

`ret` restores and pops the last bundle of registers (`r2` to `r15`).
When the call stack is empty, `ret` resets the VM to its entrypoint and clears registers `r2` to `r15`.

Mnemonic:
```
ret
```

Encoding:
```
ret:
 0               1
 0 1 2 3 4 5 6 7
+---------------+
|    opcode     |
+---------------+
   ret = 0x07
```

### Instruction: jeq, jne, jlt, jle, jgt, jge

Jumps to `tgt` if the two given registers fulfill the comparison.
* `jeq`: jump if `lhs == rhs`
* `jne`: jump if `lhs != rhs`
* `jlt`: jump if `lhs < rhs`
* `jle`: jump if `lhs <= rhs`
* `jgt`: jump if `lhs > rhs`
* `jge`: jump if `lhs >= rhs`


Mnemonic:
```
jeq lhs, rhs, tgt
jne lhs, rhs, tgt
jlt lhs, rhs, tgt
jle lhs, rhs, tgt
jgt lhs, rhs, tgt
jge lhs, rhs, tgt
```

Encoding:
```
 0               1               2               3
 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7
+---------------+-------+-------+-------+-------+-------+-------+
|    opcode     |  lhs  |  rhs  |              tgt              |
+---------------+-------+-------+-------+-------+-------+-------+
   jeq = 0x08
   jne = 0x09
   jlt = 0x0a
   jle = 0x0b
   jgt = 0x0c
   jge = 0x0d
```

### Instruction: jeol

Jumps to `tgt` if the input offset has reached the end of line.

Mnemonic:
```
jeol tgt
```

Encoding:
```
 0               1               2
 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7
+---------------+-------+-------+-------+-------+
|    opcode     |              tgt              |
+---------------+-------+-------+-------+-------+
   jeol = 0x0e
```

### Instruction: jc (JumpIfMatchCharset)

Jumps to `tgt` if the next `min` characters are found in the charset at `idx`.
Consumes no more than `max` characters.
On success the `off` register is incremented by the amount of matched characters.

Mnemonic:
```
jc idx, min, max, tgt
```

Encoding:
```
 0               1               2               3               4               5               6               7               8
 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7
+---------------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+
|    opcode     |              idx              |              min              |              max              |              tgt              |
+---------------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+
   jc = 0x0f
```

### Instruction: jp (JumpIfMatchPrefix)

Jumps to `tgt` if the next characters in the input match the given prefix string at `idx`.
On success the `off` register is incremented by the string length.

Mnemonic:
```
jp idx, tgt
```

Encoding:
```
 0               1               2               3               4
 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7
+---------------+-------+-------+-------+-------+-------+-------+-------+-------+
|    opcode     |              idx              |              tgt              |
+---------------+-------+-------+-------+-------+-------+-------+-------+-------+
   jp = 0x10
```

### Instruction: jpi (JumpIfMatchPrefixInsensitive)

Jumps to `tgt` if the next characters in the input match the given prefix string at `idx` using an ASCII-case-insensitive comparison.
On success the `off` register is incremented by the string length.

Mnemonic:
```
jpi idx, tgt
```

Encoding:
```
 0               1               2               3               4
 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7
+---------------+-------+-------+-------+-------+-------+-------+-------+-------+
|    opcode     |              idx              |              tgt              |
+---------------+-------+-------+-------+-------+-------+-------+-------+-------+
   jpi = 0x11
```

### Instruction: flush

Tells the runtime that the range between `hs` and `off` should be highlighted with the color stored in the register at index `kind`.
The runtime will then set `hs` to `off`.

> [!NOTE]
> This is a flaw in the current design, because it's not flexible enough.
> Ideally, it would be a "color the range from point A to point B with color C".

Mnemonic:
```
flush kind
```

Encoding:
```
 0               1
 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7
+---------------+-------+-------+
|    opcode     | kind  |       |
+---------------+-------+-------+
   flush = 0x12
```

### Instruction: await

Pauses execution if the input offset has reached the end of line.
The runtime will resume execution with the next line of input at the next instruction.

Mnemonic:
```
await
```

Encoding:
```
 0
 0 1 2 3 4 5 6 7
+---------------+
|    opcode     |
+---------------+
   await = 0x13
```
