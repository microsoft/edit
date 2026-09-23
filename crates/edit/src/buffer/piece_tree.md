# Arena-native persistent PieceTree

## Goal and scope

The prototype adapted a functional red-black tree to an append-only arena.
Its ancestry is [fredbuf](https://github.com/cdacamar/fredbuf), whose tree uses
reference-counted immutable nodes. An intermediate tree that loses its last
reference there can be reclaimed. An intermediate tree allocated in this arena
cannot: even nodes absent from every retained revision occupy memory until drop.

The rewrite keeps the red-black piece-tree representation and public storage
API, including revision grouping, metadata exchange, and generation restoration.
It changes how edits are assembled, rather than introducing a collector,
per-node heap allocation, a new balancing scheme, or a different text-copy limit.
`LOCAL_PIECE_LIMIT` remains 256 bytes.

## Representation and ownership

Every node contains a byte span, two children, color, left-subtree byte length,
total byte length, and black height. Null children have zero length and zero
black height. A black node adds one to its children's black height; a red node
does not. Public roots are black.

Black height occupies existing padding beside the color. Joins can compare
heights without repeatedly walking the left spine of both operands.

`Node` is a copyable value. Reading one into a local variable produces a draft,
not a newly allocated tree node. A draft may have stale cached lengths while its
children are being rearranged. `TreeEdit::store` refreshes those lengths and the
black height before publishing it in an arena allocation.

`TreeEdit` holds the arena and the current revision's address. Because the arena
is a single monotonically growing reservation, a node allocated after that
revision is pending and writable; older nodes require copying. This replaces
the prototype's per-node arena offset and per-revision offset watermark.
On the measured 64-bit build, the node layout shrinks from 64 to 56 bytes.
No pointer is reconstructed from an integer; addresses are only compared.

Reuse is an ownership operation, not a search for arbitrary spare memory:

- A draft's reuse candidate is its own node in the input tree.
- A node being split can be reused for at most one surviving fragment.
- A node removed by `pop_last` is detached before it is reused as a join pivot.
- Shared ancestors are copied before links or lengths change.
- Historical nodes and historical text are never overwritten.

One important invariant follows: if a reachable node is pending, all its
ancestors in the current tree are pending. A frozen parent could not have been
made to point to a newly allocated descendant without first being copied.

Text ownership is separate. A pending node can still reference old bytes.
`EditBuffer` identifies the one exclusively owned editable allocation by its
complete span and capacity. History transitions and structural edits discard
this capability. Ordinary piece slicing never grants text-write permission.

## Structural editing

### One balancing operation

`balance` handles insertion-style red-red violations under a black parent.
Left and right cases use the same directional code. The two shapes are:

- An outer red grandchild: promote the red child, blacken the grandchild,
  and move the old black parent below the promoted child.
- An inner red grandchild: promote that grandchild and place the two black
  children below it.

All participating node values are read before their allocations are reused.
There is no allocated unbalanced parent passed into another balancing helper.
The nodes are stored in their resulting positions, using pending allocations
where permitted.

### Join is the structural primitive

`join(left, piece, right)` connects two ordered trees with a pivot piece.

When the input black heights match, a black pivot can connect them directly,
even if either child root is red. Neither child needs recoloring or copying.

When heights differ, the shorter root is made black if necessary. The join
descends the inward spine of the taller tree until black heights match, inserts
a red pivot, and repairs red-red violations on the return path with `balance`.
Finally the result's root is blackened. A red root left at the splice point
can create a transient red-red edge, which the enclosing balancing step or
final root blackening removes.

The taller input does not need to be blackened in advance. Avoiding unnecessary
root normalization matters: recoloring a frozen root is a real allocation.

Boundary insertions retain a direct insertion path. They do not need a range
cut or a general join.

### Cut only what survives

The old general replacement assembled:

```text
split at begin -> left + rest
split rest at deletion length -> discarded middle + right
join/concatenate left and right around replacement
```

`cut(root, begin..end)` now walks both boundaries together until they diverge.
It returns the surviving left and right trees without constructing the deleted
middle. `prefix` and `suffix` retain only their respective sides of a boundary;
whole retained subtrees are returned directly. Partial pieces remain views into
existing text allocations.

Both output trees are disjoint in nodes, though their text may be shared.
Consumed pending nodes can therefore be reused during their reconstruction.

For deletion without replacement, `concat` removes the last piece from the
left tree and uses it as a pivot for `join`. `pop_last` also reconstructs with
`join`. There is no separate deletion-balancing, double-black, or fusion
implementation to maintain.

This does not reclaim all unreachable arena memory. Deleted nodes, abandoned
redo branches, and replaced text remain retained. The improvement is to avoid
allocating disposable construction scaffolding and the deleted middle at all.

## Local edits and traversal

The cursor stores a root/generation key, current piece position, and a fixed
ancestor path. Forward and backward stepping share one implementation. A
matching cached range returns immediately; adjacent reads step through the
tree; other reads seek by subtree byte counts.

The path has 64 entries. The red-black height bound and arena capacity bound the
number of reachable nodes well below what would require a taller tree. Only
the initialized prefix of the path is read.

The cursor is allocated once in the arena at construction. It is interior
mutable so document reads can take `&self`, but it does not mutate document
bytes. Each operation holds one mutable cursor borrow; helpers receive that
borrow instead of reacquiring it through the owning tree.

Edits contained within one piece avoid structural operations:

1. For insertion at a boundary, prefer the preceding piece. For replacement or
   deletion, locate the first affected byte.
2. If the nonempty result fits in 256 bytes, make the cursor path pending,
   copying shared nodes only up to the first already-pending ancestor.
3. Reuse the owned text buffer, grow it geometrically, or allocate a new buffer
   with a minimum capacity of 64 bytes.
4. Move the suffix with overlap-safe copying and write the replacement.
5. Update the piece and ancestor byte lengths by a delta. Color and black height
   do not change, and the cursor remains valid.

Nodes are made pending before allocating text, so the text remains the last
arena allocation and subsequent growth can often happen in place.

Coalescing also allows pending pieces larger than 256 bytes to append or trim
a prefix/suffix. Appending uses arena reallocation; trimming changes the span
rather than overwriting shared bytes. These paths do not retain an owned-buffer
capability. Deleting a whole piece uses the structural path.

Unlike Zipper, this implementation does not merge neighboring small pieces into
256-byte chunks. The shared numeric limit bounds local piece copying, not an
identical compaction policy.

## History

Revision creation captures the current root, metadata, generation, and previous
revision pointer. Coalescing reuses an existing undo group when it has a
predecessor. Both forms discard redo.

Undo and redo exchange the caller's metadata and the current generation with
the revision record, then switch roots. They clear text-edit ownership.
`reset_history` creates a fresh revision without a predecessor, preserving the
text but protecting all inherited nodes behind the new watermark.

Empty replacements remain no-ops. `clear` increments generation, including when
already empty. Range endpoints retain the existing clamping behavior.

## Validation and measurement

The starting point was the user's 256-byte prototype, not the earlier 1024-byte
version. Its unchanged benchmark printed:

| Storage | Arena bytes rounded to 64 KiB |
| --- | ---: |
| Zipper | 8,060,928 |
| PieceTree prototype, 256-byte limit | 11,206,656 |
| AVL | 10,616,832 |

The prototype measured `[11.750, 11.878, 12.034] ms` in the background terminal.
Foreground timings have a different scheduling baseline.

Temporary validation covers vector-model editing, metadata/generation history,
branching after undo, reset, clamping, extraction/copying, bidirectional reads,
the complete rustcode trace with transaction undo/redo, and logical editor
replay against Zipper. Private checks additionally verify red-black invariants,
cached lengths and heights, nonempty pieces, node uniqueness, and frozen roots.
These temporary harnesses are not retained in the repository, in keeping with
the planned separate test rewrite.

The final isolated run, using a ten-second Criterion measurement period, gave:

| Measurement | Prototype | Rewrite |
| --- | ---: | ---: |
| Arena bytes rounded to 64 KiB | 11,206,656 | 9,175,040 |
| Runtime estimate | 11.878 ms | 11.448 ms |
| Runtime confidence interval | 11.750-12.034 ms | 11.390-11.514 ms |

That is 18.1% less arena memory and an estimated 3.6% lower runtime against the
256-byte prototype in this terminal. This is not a comparison against the
requester's foreground timings or the original 1024-byte implementation.
Zipper and AVL memory measurements were unchanged.

Validation results before removing the temporary harnesses:

- Four private invariant tests passed in debug and release, including exhaustive
  small ranges and full trace replay with historical-root checks.
- Five public API/history/trace tests passed in debug and release.
- The release buffer suite passed all 17 tests, including the four temporary
  invariant tests.
- Production Clippy passed with `-D warnings`, allowing only the pre-existing
  storage trait's `len_without_is_empty` lint.

No benchmark workload, process priority, compiler profile, or other storage
backend was modified for these measurements.

After removing the temporary harnesses, the remaining 13 debug buffer tests
passed. A final exact `cargo bench buffer/PieceTree/rustcode` run measured
`[11.331, 11.365, 11.406] ms`, with the same 9,175,040-byte arena footprint.
