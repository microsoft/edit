# Zipper: prototype design and v2

## Scope

Zipper is the editor's persistent byte store, not a balanced rope. Its optimization
target is an editing session with spatially local changes and repeated nearby
reads. The acceptance workload is `cargo bench buffer/Zipper/rustcode`, including
the editor's logical cursor movement, selection, line scanning, and history
grouping, rather than just raw insertion throughput. Undo/redo correctness is a
separate requirement.

This document records the prototype before the v2 rewrite, including policies
that must not be mistaken for incidental implementation details. The 256-byte
coalescing limit is an empirically tuned policy from the prototype's benchmark
work, not a newly derived constant. The measurements below validate the rewrite;
they do not reproduce the original parameter sweep.

## 1. Representation: two persistent lists and a focus

A revision describes:

- A root piece, or no root for an empty document.
- The absolute byte offset of the root and the total document length.
- Undo metadata, the generation before the edit, history links, and an allocation
  watermark.

Each piece has a pointer and byte length, two optional node links, its allocation
offset, and an optional writable capacity. Pieces are nonempty. They need not have
equal sizes, and large initial text and insertions are not divided into 256-byte
leaves.

The root's left link leads toward byte zero. Its right link leads toward EOF:

```text
       outward <---                 ---> outward
    L3 <- L2 <- L1 <- [ root ] -> R1 -> R2 -> R3
                       ^
                    root_start
```

This is NOT a conventional doubly linked list. On the left, only leftward links
are authoritative; on the right, only rightward links are authoritative. Links
toward the root are scratch backlinks for the active traversal. They may be
absent or stale until an outward traversal establishes them.

Consequences:

1. Unchanged outer tails can be shared by many revisions.
2. Replacing the root does not require repairing its neighbors.
3. Returning toward the root uses backlinks established by the active read path.
4. Moving the root requires copying the nodes crossed along the way.

The concatenation of the reversed left list, root text, and right list is the
document. The sum of left lengths equals `root_start`; the sum of all lengths
equals the revision length. An empty document has no root and both lengths zero.

## 2. Arena storage and persistence

Nodes, revisions, and text live in one append-oriented arena. Small buffers reserve
128 MiB of virtual address space; large buffers reserve 4 GiB on 64-bit targets or
512 MiB on 32-bit targets. Reservation is not immediate physical commitment.
The arena commits pages as needed and releases everything when dropped.

No individual node, text span, discarded redo branch, or old revision is freed.
`reset_history` severs history links, but does not reset the arena. This makes
shared raw pointers stable and history changes cheap, at the cost of retaining
unreachable allocations. Exhaustion follows the arena's existing panic policy;
initial reservation is fallible.

Metadata is `Copy`: arena values cannot depend on destructors being run.

### Two independent permissions

The prototype distinguishes writable *nodes* from writable *text*:

- A node is pending when its recorded allocation offset is at or beyond the
  current revision's watermark. A new revision records its watermark after
  allocating the revision record, so inherited nodes are never pending.
- Nonzero capacity means a node exclusively owns that text allocation. Sharing
  text into a copied or split node sets the new node's capacity to zero.

Both conditions are necessary for an in-place interior text splice. A new node
can still point at old, shared bytes. Conversely, an old node can have nonzero
capacity but be protected by a newer revision's watermark.

Node allocation records the arena offset before allocation/alignment. Comparing
it to the watermark is sufficient; it is an age marker, not a pointer conversion.

Nodes are allocated before their new text. This ordering is intentional: the
arena can extend its last text allocation in place. Allocating another object
after the text would lose that common-case optimization.

## 3. Reanchoring and replacement

### Reanchoring

An edit offset inside the root, including either endpoint, needs no traversal.
Otherwise, walk outward toward the edit. For every crossed node, allocate a copy
on the opposite side of the new focus. The text is shared, not copied. The
untouched outer list is reused.

For example, moving right across the old root and R1 creates new leftward links:

```text
    old left tail <- copy(root) <- copy(R1) <- [ R2 ] -> old right tail
```

The prototype also allocated a copy of the destination node and installed it as
the revision root before constructing the replacement. That intermediate root
was immediately superseded by replacement assembly. V2 keeps the destination as
a temporary focus value and publishes only the final root.

### Cutting the range

Normalize a requested range as:

```text
begin = min(request.start, document_length)
end   = max(begin, min(request.end, document_length))
```

Thus reversed ranges become insertions, and `usize::MAX` works as EOF.
An empty insertion is a no-op in the ordinary replacement path.

Reanchor at `begin`. Retain the prefix of the focus before `begin`, then walk
right across the deletion, retaining the suffix after `end`. Full deleted pieces
are simply skipped. The result is two boundaries:

```text
left outer list | surviving prefix | replacement | surviving suffix | right outer list
```

The prefix and suffix are views into old bytes. Creating them does not copy text.

### Bounded coalescing: 256 bytes

Start with `prefix.len + replacement.len + suffix.len`. If this exceeds 256,
do not compact. In particular, a tiny insertion into a large piece must not copy
the large surviving prefix and suffix.

Otherwise:

1. Absorb complete left neighbors while the combined text still fits.
2. Then absorb complete right neighbors while it still fits.
3. Copy the resulting nonempty run into one new owned text allocation.
4. Use the remaining outer lists as its links.

The left-first order is part of the policy. Neighbors are absorbed whole; the
algorithm does not split them merely to fill unused space. A deletion whose
immediate result is empty can still absorb neighbors and create a new root.

Capacity is `max(64, next_power_of_two(length))`. Logical length and allocated
capacity are distinct. The 64-byte floor and geometric growth let short runs of
typing reuse space; the 256-byte ceiling limits both per-edit copying and
unchanged text retained in history. Raising the ceiling reduces piece count and
read traversal, but increases copying and retained history. Lowering it does the
opposite. These are workload-dependent tradeoffs, not asymptotic improvements.

Without compaction, materialize the surviving boundary fragments as shared
pieces and insert the replacement as an exact-size allocation. For deletion
without replacement, choose a surviving fragment or copy an outer neighbor as
the root. If nothing survives, publish an empty document.

Initial insertion into an empty document is also an exact-size allocation,
without the small-chunk capacity policy.

## 4. Pending-edit fast paths

`replace_coalescing` is called within an existing revision. It first checks that
the root node is pending. It then tries these cases, in order:

1. **Owned bounded splice.** The range lies wholly inside the root, capacity is
   nonzero, and the resulting length is in `1..=256`. Grow geometrically if
   necessary, move the suffix with overlap-safe copying, and copy the inserted
   bytes. This handles interior insertion, replacement, and deletion.
2. **Append.** Insert nonempty text at the end of the root. Reallocate using the
   current logical length, append bytes, and clear capacity. This is not bounded
   by 256. If the old span is not the final arena allocation, reallocation copies
   it; otherwise it grows in place. Previously visible bytes are not overwritten.
3. **Trim.** Delete a strict prefix or suffix without deleting the whole root.
   A prefix trim advances the text pointer and clears capacity; a suffix trim
   just shortens the view. No shared text is modified.

All other edits use persistent replacement assembly. Ordinary `replace` does not
try these fast paths. This distinction matters for loading/copying text and for
tests of persistence.

## 5. Revisions, metadata, and generation

`begin_revision(metadata, false)` snapshots the current root, offset, and length
into a new current revision, links its predecessor, records the current
generation, and establishes a new watermark. It does not copy the document.

`begin_revision(metadata, true)` reuses the current revision if it already has a
predecessor. It preserves that revision's original metadata and watermark.
Either form discards the redo stack. Coalescing after undo therefore modifies the
resumed undo group rather than necessarily creating another group.

Undo pushes the departing revision onto the redo stack and selects its
predecessor. Redo pops that revision back. In both directions:

- Return the departing/restored revision's saved metadata.
- Replace its metadata with the caller's current metadata.
- Swap the global generation with its `generation_before`.

This swap protocol restores editor state in both directions without needing
separate before/after metadata records. Cursor/selection restoration is owned by
the caller; Zipper treats metadata as opaque.

Generation increments with wrapping arithmetic on successful content edits and
on `clear`. It is a content/cache token, not a monotonic edit counter across
undo/redo. Beginning/resetting history does not increment it.
One prototype detail is preserved: an empty coalescing edit inside a pending
owned chunk can take the successful splice path and increment generation despite
leaving the bytes unchanged. Ordinary empty replacement does not.
`reset_history` allocates a fresh revision with the same text but no predecessor,
and drops redo. A fresh watermark protects inherited pieces.

Callers open a revision before undoable edits; the storage's raw write methods do
not implicitly open one or discard redo.

## 6. Cached reads

Read methods clamp offsets and return the contiguous remainder/prefix of one
piece. They do not flatten the document. The prototype is byte-oriented; it does
not discover Unicode boundaries when splitting pieces.

The cache records:

- Revision identity AND generation.
- A node and its absolute start.
- Which side of the root it occupies (or the root itself).
- The root pointer, to recognize when an inward step reaches the root.

Both keys are required: revisions may share a generation, and the same revision
can be edited repeatedly.

A read inside the cached piece is constant-time. A read exactly at its next or
previous boundary steps one node. Moving outward writes the previous node into
the destination's inward link slot. Moving inward follows that scratch backlink.
Crossing the root changes which direction is outward. Other reads seek from the
revision root, rebuilding backlinks along the path.

This makes sequential scans linear in visited pieces rather than repeatedly
walking from the root. Alternating forward/backward reads reuse those backlinks.
The prototype and initial v2 used a vector for the return path; this version
stores it intrusively in the otherwise unused inward slots, with no additional
per-node memory and no traversal-path allocation.

This is safe because a node's role never changes: root, left tail, or right
tail. Reanchoring copies crossed nodes into the opposite tail; promoting a tail
piece to the root also creates a new node. Consequently, an inward scratch
slot is never an authoritative outward link in another revision. Editing reads
only authoritative links, never scratch backlinks.

There is only one active traversal cache per Zipper. A revision/generation
change forces a seek before any backlink can be followed. Seeking overwrites
the complete return path needed by that cursor; stale links elsewhere do not
matter. Independent cursors traversing shared nodes would require coordination
or separate path storage, and are not supported by this cache design.

The cache is interior-mutable because `ReadableDocument` reads take `&self`.
The link slots use `Cell<NodePtr>` to permit scratch writes through shared node
references. Root links and outward tail links are not modified by reads.
Zipper is not `Sync`. Cache mutation must never change document bytes and must
not create overlapping mutable cache references. V2 hands one exclusive cache
borrow to traversal helpers instead of recursively reacquiring it.

## 7. Complexity and limitations

| Operation | Cost |
| --- | --- |
| Read inside cached piece / adjacent step | O(1), no path allocation |
| Uncached seek | O(pieces from root to target) |
| Reanchor | O(crossed pieces), allocating one node per crossing |
| Delete | O(crossed pieces), plus boundary construction |
| Compact | At most 256 bytes of text copied |
| Owned pending splice | At most 256 bytes moved/copied |
| Append | O(inserted bytes), plus old piece length if reallocation copies |
| Begin revision / undo / redo / reset history | O(1) |

There is no balancing, subtree length index, reclamation, or logarithmic random
access guarantee. Distant alternating edits can copy many list nodes. Large
append pieces can require large copies if they cease to be the arena tail.
The design intentionally spends memory to keep the common local-edit path simple.

## 8. V2 abstractions

The implementation separates the invariants instead of interleaving pointer
bookkeeping throughout replacement:

- **Span:** a copyable byte view; slicing does not confer mutation permission.
- **Direction:** the symmetric left/right operations on links and positions.
- **Focus:** a piece with authoritative outer links and an absolute position;
  it need not be an allocated node.
- **Fringe:** an outer list plus a surviving boundary fragment; handles bounded
  absorption, directional copying, and materialization.
- **Snapshot:** root, root offset, and document length as one revision value.
- **Traversal cache:** owns seeking and stepping with a single mutable borrow.
- **Node:** owns the pending text-splice rules; the revision watermark gates
  access to them.

The public `Zipper<T>` and storage trait integration stay unchanged. There is no
parallel legacy implementation or new storage backend to configure.

### Keeping the abstractions cheap

The project builds with `opt-level = "s"`. Consequently, factoring a block into
a helper does not guarantee that it disappears during optimization. The initial
rewrite had a small but real regression: a foreground comparison measured
8.8977 ms against the prototype's 8.5586 ms.

Inspection of generated assembly showed out-of-line helper calls in the new
allocation, splicing, and traversal paths. Several helpers also passed aggregate
values or a runtime direction across those boundaries. The retained changes are:

- Explicit inlining at hot abstraction boundaries, including allocation,
  splicing, reanchoring, compaction, and directional copying/stepping. This
  exposes aggregate fields and known directions to the optimizer.
- An inlined cache lookup that checks revision identity and generation once,
  returns immediately on a cache hit, and otherwise steps or seeks.
- Passing a revision pointer to cache repositioning rather than a by-value
  three-word snapshot. The snapshot is loaded only when a root seek is needed.
- Keeping seek loop positions in locals and publishing the cache position once,
  rather than routing every seek step through the general cursor-step operation.
- Avoiding zero-length fragment copy calls.

This is an account of the code-generation issues addressed, not a claim that
each individual change independently explains a measured fraction of the
regression. Background timings varied enough that small isolated deltas were not
reliable. Inlining hints should therefore be reconsidered only with the actual
editor benchmark, not removed as cosmetic cleanup.

Caching an extra copy of the current text span and choosing the nearer of the
cached position/root for nonadjacent seeks were also tried. They did not establish
a useful improvement and are not part of the final implementation.

### Unsafe-code boundary

All span and node pointers are internal to this module and remain within the
owning arena's lifetime. A span is a view, not proof of writable ownership.
`Node::splice` is reached only after checking the current revision's watermark
and converting a contained absolute range to a relative range.

Fresh nodes may briefly contain a dangling text pointer during construction;
they are not published until their text pointer and initialized bytes are ready.
Directional chunk copying writes into a fresh allocation, whereas an owned
interior splice deliberately uses overlap-safe suffix movement.

The uninitialized traversal cache uses a dangling revision identity. A cache
node is dereferenced only after its identity and generation match, or after a
seek has initialized it. There is one mutable cache borrow per read, and no
helper can reacquire that borrow through the owning Zipper. Returned slices
borrow the Zipper, preventing text edits while those slices remain live.
Further reads may update scratch backlinks through `Cell` without affecting
the borrowed bytes or persistent topology.

## 9. Validation

### Intrusive backlink follow-up

Compared with the immediately preceding vector-based implementation in the same
background terminal:

| Return path | Runtime estimate | 95% confidence interval |
| --- | ---: | ---: |
| Vector | 8.6109 ms | 8.5619-8.6672 ms |
| Scratch backlinks | 8.6134 ms | 8.5665-8.6729 ms |

Criterion detected no performance change. Rounded arena usage remained
8,060,928 bytes. The vector's separate heap allocation is eliminated; the
existing arena usage measurement did not include that allocation.

Temporary tests passed in debug and release, checking randomized edits,
forward/backward scans, nonadjacent seeks, repeated undo/redo, branching,
history reset, clear, and complete rustcode transaction history. They also
checked that a node address never changes its root/left-tail/right-tail role
across revisions. The 15-test release buffer run included both temporary tests.
The harness was then removed to leave the planned test rewrite independent.

### Initial v2 measurements

The prototype's exact requested benchmark in the background terminal measured
`[8.8105, 8.8534, 8.9057] ms` (Criterion lower bound, estimate, upper bound).
A second run saved as `zipper-v1` measured `[8.9998, 9.0553, 9.1150] ms`.
These are background-process measurements on the same machine, not comparable
to a separately foreground-boosted run.

Commands:

```text
cargo bench buffer/Zipper/rustcode
cargo bench --bench lib buffer/Zipper/rustcode -- --save-baseline zipper-v1
cargo test -p edit --lib buffer::
cargo bench --bench lib buffer/Zipper/rustcode -- --baseline zipper-v1
```

Existing tests cover raw trace replay, logical editor replay, vector-model
replacement, adjacent reads, capacity reuse, and scattered coalesced revisions.
The original eight tests are preserved in [tests.rs](tests.rs). Two additional
tests cover:

- 2,000 deterministic operations against an independent full-snapshot model,
  checking text, metadata, generation, undo/redo, coalescing, branches, history
  resets, clear, and bidirectional reads.
- Appending to a shared root, prefix/suffix trimming, clamped/reversed ranges,
  no-op replacement, and repeated history restoration.

Final validation:

| Check | Result |
| --- | --- |
| `cargo test -p edit --lib buffer::zipper::` | 10 passed |
| `cargo test -p edit --lib buffer::` | 32 passed |
| `cargo test -p edit --release --lib buffer::` | 32 passed |
| `cargo bench buffer/Zipper/rustcode` (background) | `[8.8828, 8.9172, 8.9563] ms` |
| Foreground prototype, measured by the requester | 8.5586 ms |
| Foreground final v2, measured by the requester | 8.5574 ms |

The final foreground comparison establishes parity at the precision of these
measurements, rather than a meaningful speedup. No benchmark workload, compiler
profile, process priority, or coalescing limit was changed to obtain it.

Formatting and diff whitespace checks pass. Strict Clippy also identifies
pre-existing `len_without_is_empty` in the storage trait and `byte_char_slices`
in piece-tree tests; these unrelated files are left unchanged. Clippy passes with
only those two pre-existing lint categories allowed:

```text
cargo clippy -p edit --lib --tests -- -D warnings -A clippy::len_without_is_empty -A clippy::byte_char_slices
```
