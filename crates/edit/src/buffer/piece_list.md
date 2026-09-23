# PieceList: mutable pieces with reversible splices

## Scope

`PieceList<T>` is an alternative `TextBufferStorage`, selected with
`TextBuffer::<PieceListStorage>::new_with_storage(...)`. Zipper remains the
default. The benchmark is `cargo bench buffer/PieceList/rustcode`.

The live document is a conventional doubly linked list. Both neighbor links are
always authoritative; reading never changes them. Unlike Zipper, there is no
root-relative topology, path copying, traversal stack, or scratch backlink.

History is an edit log, not a collection of traversable snapshots. Undo changes
the one live list; redo changes it back. Comparing arbitrary historical versions
would need reconstruction or checkpoints, neither of which is implemented here.

## Representation

Each node stores non-null predecessor/successor pointers and a byte span. One
arena-allocated sentinel closes the list into a ring. Its next/previous pointers
are the first/last pieces; both point to itself when the document is empty.
The sentinel is a fully initialized, full-sized node with a zero-length span,
not a cast from a smaller allocation.

The list stores total length, generation, a read cursor, and a tail node pointer
with its document start and available text capacity. The tail's text pointer
and length live only in the node: there is no separate `Tail` or pending-cursor
object. The sentinel denotes no editable tail or an invalidated read cursor.
Capacity is tracked once for the tail, not in every node.

Nodes, text, revision records, and splice records use the existing arena. There
is no per-node heap allocation, reference counting, free list, or balancing.
Historical allocations are not reclaimed, including after discarding redo or
resetting history. Unused capacity at the end of the active text allocation is
reclaimed before any later allocation. `committed()` reports the arena offset
rounded to 64 KiB, as in the other backends; it is not an OS commitment query.
Shrinking the tail makes space reusable but does not decommit physical pages.

## Reversible splices

A change records two fixed outside neighbors and two runs:

```text
left <-> before.first ... before.last <-> right
left <-> after.first  ... after.last  <-> right
```

Either run can be empty; document endpoints use the sentinel as their outside
neighbor. An empty run has `first = right` and `last = left`. Thus `connect`
always performs the same two links: `left <-> first` and `last <-> right`.
For an empty run, both simply establish `left <-> right`. For an empty document
those pointers all name the sentinel. Field writes are sequential to avoid
creating simultaneous mutable references to that same node.
There are no endpoint/null branches and no walk or copy of the run's interior.

Forward editing detaches the old run and connects the new one. The detached run
remains in the arena. Undo connects `before`; redo connects `after`.

Later operations may change links in nodes referenced by an earlier record.
This is intentional: undo visits records in reverse order, so those intervening
operations restore the required topology before the earlier record is applied.
Redo visits records in forward order. A record is not a standalone snapshot and
must not be replayed out of order.

Each revision owns a doubly linked chain of changes. Undo/redo work is O(number
of recorded splices in the group), rather than a persistent root swap. Document
length, generation, and caller metadata are exchanged at the revision boundary.
Coalescing preserves the group's original metadata. Empty groups are valid.

`begin_revision` discards redo. Actual text edits also discard redo, so raw
writes after undo cannot replay records against an incompatible live topology.
An empty replacement remains a no-op. `clear` is undoable within a revision and
increments generation even when already empty.

The initial state and the state after `reset_history` do not need undo records
until `begin_revision` opens a group. Resetting history keeps the live list and
allocates a fresh base revision; it does not free old allocations.

## Building a replacement

Locate the boundary pieces, preserving their unchanged prefix and suffix as
byte views. Complete deleted pieces are detached, not individually processed
into new nodes. Only the search for the end boundary walks through them.

Construct at most three pieces: shared prefix, new text, and shared suffix.
Unchanged bytes are never copied to coalesce pieces. The 256-byte compaction
policy and bounded interior-splice optimization have been removed for this
experiment.

Before allocating the log or nodes, finish the old text tail by shrinking its
allocation to the used length. Then allocate the log and all nodes, followed
by the replacement text with capacity rounded up to a multiple of 4 KiB.
Initial insertion follows the same policy. At most one text allocation retains
spare capacity.

Replacement has one prepare/apply flow:

1. Decide whether the existing tail can accept the edit: a coalesced append or
   a strict prefix/suffix trim.
2. Otherwise, `prepare_splice` records and reconnects the surviving pieces.
   When inserting text it creates an empty tail node; deletion-only splices
   do not allocate a tail. The original deleted range is handled structurally,
   leaving an empty `0..0` range in the new tail.
3. `replace_tail` applies the prepared relative edit. A new empty tail and an
   existing tail use the same allocation/growth and byte-copying path.
4. Update document length, generation, and the read cursor once.

The empty tail's text pointer is initially dangling and capacity is zero.
`replace_tail` allocates and fills its bytes before the operation returns;
no reads or intervening edits observe the incomplete node. All log, prefix,
tail, and suffix nodes have already been allocated at that point. The live
list still contains no empty text pieces between operations.

## Mutable text without losing undo

The tail pointer identifies the newly inserted node from the most recent
splice. Its document start is separate from the read cursor: scanning another
part of the document must not disable edit coalescing.

That pending piece can be changed without another log entry:

- Appending writes at the used end of the text tail, growing its capacity in
  4 KiB increments when necessary. No intervening allocations are allowed, so
  the arena can always grow it in place.
- Strict prefix/suffix deletion can shorten a span without overwriting bytes.
  A suffix trim reduces the node's length. A prefix trim advances the node's
  text pointer and subtracts the skipped bytes from available capacity, keeping
  `text + capacity` at the arena end. The skipped prefix remains allocated.
  Later growth still extends the allocation by multiples of 4 KiB, although
  available capacity after a prefix trim need not itself be a multiple of 4 KiB.

Interior insertion, replacement, and deletion use a new recorded list splice,
even for tiny pieces. They never shift the pending piece's interior bytes.

The existing splice's `after` run now describes the revised result, while its
`before` run still describes the state at the beginning of that splice.

Only the latest replacement is eligible. Another structural edit replaces the
tail pointer; a new revision, successful undo/redo, or history reset finishes
the tail and clears it. This also ensures a new revision record cannot strand
unused text capacity behind it. Coalescing into the same revision does not
finish the tail. Once a piece is needed by an earlier undo state, its text
cannot be overwritten. Shared prefix/suffix views never become appendable tails.

This avoids preserving every keystroke separately without implementing a
second byte-level undo mechanism. It does not promise that every chunk is
writable at all times: information required to undo still has to survive.

## Traversal and costs

The read cursor remembers one node and its start offset. Reads inside it are
constant-time; other reads follow ordinary predecessor/successor links until
the requested offset is reached. Sequential scans are linear in pieces. Random
access remains linear in distance from the cursor.

Every edit repositions the read cursor, so it never refers to a detached node
or a stale offset. Undo/redo invalidate it. Reads mutate only this small `Cell`
cache, not nodes or text; returned byte slices keep text edits excluded by the
normal Rust borrow rules.

Node allocation per structural edit does not depend on cursor travel distance.
There is still one splice record per non-coalesced structural edit, and a
record can retain a long deleted run. The mutable design saves copying, not
the underlying information needed for history.

## Validation and benchmark

### Unified prepare/apply follow-up

The duplicated `replace_pending` implementation has been replaced by a
nonmutating eligibility check, structural preparation, and one tail mutation
path. Append/trim eligibility, undo grouping, allocation order, and 4 KiB
growth policy remain unchanged.

Temporary tests cover all ranges in a small fragmented document with both
replacement entry points, empty replacements, new/existing tails, prefix/suffix
trims across the 4 KiB boundary, ring invariants, branching, and full rustcode
transaction undo/redo with content and generation checks.
All three temporary tests passed in debug and release; the release buffer suite
passed 16 tests including those three. The temporary harness was removed after
validation. Production Clippy passed with `-D warnings`.

The background benchmark measured 10.011 ms immediately before the change and
9.4642 ms afterward, with unchanged reported arena usage of 3,932,160 bytes.
These timings are higher than earlier runs of this workload, so the measured
improvement should not be interpreted as an isolated estimate of code speedup.

### Sentinel and direct-tail follow-up

The list now uses a full-sized sentinel node and stores the tail as a node
pointer, eliminating duplicated span state. No smaller-to-larger struct casts
or per-node capacity fields are needed. The sentinel adds one node per buffer;
rounded arena usage on rustcode remains 3,932,160 bytes.

Temporary tests verify ring reciprocity and endpoints across empty/nonempty
transitions, complete deletion, undo/redo, branching, history reset, repeated
randomized splices, and full rustcode transaction replay. They also check
prefix trimming followed by 4 KiB growth preserves the text address and exact
unused-capacity reclamation.

All three temporary tests passed in debug and release. The release buffer suite
passed 16 tests including those three. The harness was then removed. The final
comparison measured 9.0152 ms versus 9.0130 ms before the change; Criterion
detected no performance change.

### Experiment without 256-byte chunks

The same background-terminal benchmark before and after this experiment:

| Policy | Runtime estimate | Confidence interval | Rounded arena bytes |
| --- | ---: | ---: | ---: |
| 256-byte coalescing | 8.0043 ms | 7.9958-8.0131 ms | 6,225,920 |
| No coalescing, 4 KiB tails | 9.0345 ms | 9.0188-9.0504 ms | 3,932,160 |

The change saves 2,293,760 bytes (2.1875 MiB) while increasing runtime by about
12.9%. Relative to the requester's GapBuffer measurement of 4,128,768 bytes,
the new reported usage is 196,608 bytes (192 KiB) smaller. These are the net
effects of removing compaction/interior splicing, removing per-node capacity,
and changing tail allocation; they do not isolate copied text from log/node
allocation changes.

Temporary tests directly verify 4 KiB capacity boundaries, in-place growth,
exact reclamation before a revision allocation, prefix/suffix trimming followed
by append, absence of coalescing/interior rewriting, branching, and randomized
history. Full rustcode transaction history is compared with a vector model
and checked through complete undo/redo.
All four temporary tests passed in debug and release; the release buffer suite
passed 17 tests including those four. Production Clippy passed with `-D warnings`.
The temporary harness was removed after validation, as in the preceding experiments.

### Original implementation

The existing storage-parity test also exercises PieceList. Temporary additional
tests verified:

- Live-list reciprocal links, endpoints, absence of cycles, total length, and
  cursor positions after edits and history transitions.
- Vector-model editing, overlapping/disconnected splices, repeated overwrites,
  shared fragments, large appends, and full deletion.
- Metadata and generation restoration, coalesced branching, history reset,
  no-ops, range clamping, extraction, and copying.
- Full rustcode transaction replay with undo/redo hashes and logical editor
  replay/undo/redo parity against Zipper.

Both private invariant tests and all five public API tests passed in debug and
release. The release buffer suite passed 15 tests including the two temporary
invariant tests. Those temporary harnesses were removed afterward to keep the
planned test rewrite separate. Production Clippy passed with `-D warnings`.

One paired run of the same logical editor workload in the background terminal,
using ten-second Criterion measurement periods, produced:

| Backend | Runtime estimate | Confidence interval | Rounded arena bytes |
| --- | ---: | ---: | ---: |
| PieceList | 7.8966 ms | 7.8800-7.9152 ms | 6,225,920 |
| Zipper | 8.7373 ms | 8.6989-8.7811 ms | 8,060,928 |

This is approximately 9.6% lower runtime and 22.8% less arena memory for this
trace, not a guarantee for arbitrary edit distributions. No scheduling priority
or compiler-profile changes were made. The unchanged tree backends reported
9,175,040 bytes (RB) and 10,616,832 bytes (AVL).
