// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::cell::{Cell, UnsafeCell};
use std::mem::MaybeUninit;
use std::ops::Range;
use std::ptr::NonNull;
use std::{io, slice};

use crate::alloc::Allocator as _;
use crate::arena::Arena;
use crate::document::{ReadableDocument, WriteableDocument};
use crate::helpers::*;

#[cfg(target_pointer_width = "32")]
const LARGE_CAPACITY: usize = 512 * MEBI;
#[cfg(target_pointer_width = "64")]
const LARGE_CAPACITY: usize = 4 * GIBI;
const SMALL_CAPACITY: usize = 128 * MEBI;

type NodePtr = Option<NonNull<Node>>;
type RevisionPtr<T> = NonNull<Revision<T>>;
const MAX_TREE_HEIGHT: usize = 64;
// Bound copying at revision boundaries without fragmenting ordinary typing into tiny pieces.
const MAX_EDIT_LEAF: usize = 512;

#[derive(Clone, Copy)]
struct Piece {
    text: NonNull<u8>,
    len: usize,
    // Zero for slices of shared text. Otherwise only a pending leaf may modify this allocation.
    capacity: usize,
}

#[derive(Clone, Copy)]
enum NodeKind {
    Leaf(Piece),
    Branch { left: NonNull<Node>, right: NonNull<Node> },
}

struct Node {
    kind: NodeKind,
    len: usize,
    height: u8,
}

struct Revision<T> {
    root: NodePtr,
    metadata: T,
    generation_before: u32,
    previous: Option<RevisionPtr<T>>,
    redo_next: Option<RevisionPtr<T>>,
}

#[derive(Clone, Copy)]
struct TraversalPathEntry {
    branch: NonNull<Node>,
    went_right: bool,
}

struct TraversalCache {
    root: NodePtr,
    generation: u32,
    leaf: NonNull<Node>,
    text: NonNull<u8>,
    start: usize,
    end: usize,
    path: [MaybeUninit<TraversalPathEntry>; MAX_TREE_HEIGHT],
    depth: usize,
    valid: bool,
}

impl TraversalCache {
    fn new() -> Self {
        Self {
            root: None,
            generation: 0,
            leaf: NonNull::dangling(),
            text: NonNull::dangling(),
            start: 0,
            end: 0,
            path: [MaybeUninit::uninit(); MAX_TREE_HEIGHT],
            depth: 0,
            valid: false,
        }
    }
}

/// An arena-backed piece tree with persistent revisions.
///
/// Tree nodes and inserted text are never freed individually. Starting an edit creates a
/// revision whose root shares all unchanged nodes with the previous revision. Pending nodes
/// and exclusively owned text can be updated in place; older revisions remain immutable.
/// `T` is scratch space owned by that revision, intended for editor state needed by undo and redo.
pub struct PieceTreeAvl<T: Copy> {
    arena: Arena,
    current: RevisionPtr<T>,
    redo: Option<RevisionPtr<T>>,
    generation: u32,
    traversal: Cell<Option<NonNull<UnsafeCell<TraversalCache>>>>,
    #[cfg(test)]
    node_allocations: Cell<usize>,
    #[cfg(test)]
    revision_allocations: Cell<usize>,
    #[cfg(test)]
    traversal_seeks: Cell<usize>,
}

#[cfg(test)]
#[derive(Debug)]
pub struct PieceTreeDiagnostics {
    pub node_allocations: usize,
    pub revision_allocations: usize,
    pub reachable_revisions: usize,
    pub pieces: usize,
    pub height: usize,
    pub arena_bytes: usize,
    pub traversal_seeks: usize,
}

impl<T: Copy> PieceTreeAvl<T> {
    pub fn new(small: bool, metadata: T) -> io::Result<Self> {
        let arena = Arena::new(if small { SMALL_CAPACITY } else { LARGE_CAPACITY })?;
        let revision = arena.alloc_uninit().write(Revision {
            root: None,
            metadata,
            generation_before: 0,
            previous: None,
            redo_next: None,
        });
        Ok(Self {
            current: NonNull::from(revision),
            arena,
            redo: None,
            generation: 0,
            traversal: Cell::new(None),
            #[cfg(test)]
            node_allocations: Cell::new(0),
            #[cfg(test)]
            revision_allocations: Cell::new(1),
            #[cfg(test)]
            traversal_seeks: Cell::new(0),
        })
    }

    pub fn committed(&self) -> usize {
        const ALLOC_CHUNK_SIZE: usize = 64 * 1024;
        (self.arena.offset() + ALLOC_CHUNK_SIZE - 1) & !(ALLOC_CHUNK_SIZE - 1)
    }

    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        self.root().map_or(0, |root| unsafe { root.as_ref().len })
    }

    pub fn generation(&self) -> u32 {
        self.generation
    }

    /// Starts an edit revision. When `coalesce` is true, the current revision remains the
    /// pending head and subsequent replacements become part of the same undo step.
    pub fn begin_revision(&mut self, metadata: T, coalesce: bool) {
        self.redo = None;
        if coalesce && unsafe { self.current.as_ref().previous.is_some() } {
            return;
        }

        let revision = self.arena.alloc_uninit().write(Revision {
            root: self.root(),
            metadata,
            generation_before: self.generation,
            previous: Some(self.current),
            redo_next: None,
        });
        self.current = NonNull::from(revision);
        #[cfg(test)]
        {
            self.revision_allocations.set(self.revision_allocations.get() + 1);
        }
    }

    pub fn revision_metadata_mut(&mut self) -> &mut T {
        unsafe { &mut self.current.as_mut().metadata }
    }

    /// Discards all reachable history while retaining the current root and arena allocations.
    pub fn reset_history(&mut self, metadata: T) {
        let revision = self.arena.alloc_uninit().write(Revision {
            root: self.root(),
            metadata,
            generation_before: self.generation,
            previous: None,
            redo_next: None,
        });
        self.current = NonNull::from(revision);
        self.redo = None;
        #[cfg(test)]
        {
            self.revision_allocations.set(self.revision_allocations.get() + 1);
        }
    }

    pub fn undo(&mut self, current_metadata: T) -> Option<T> {
        let mut revision = self.current;
        let previous = unsafe { revision.as_ref().previous }?;
        let revision = unsafe { revision.as_mut() };

        let metadata = revision.metadata;
        revision.metadata = current_metadata;
        std::mem::swap(&mut self.generation, &mut revision.generation_before);
        revision.redo_next = self.redo;
        self.redo = Some(self.current);
        self.current = previous;
        Some(metadata)
    }

    pub fn redo(&mut self, current_metadata: T) -> Option<T> {
        let mut revision_ptr = self.redo?;
        let revision = unsafe { revision_ptr.as_mut() };

        let metadata = revision.metadata;
        revision.metadata = current_metadata;
        std::mem::swap(&mut self.generation, &mut revision.generation_before);
        self.redo = revision.redo_next.take();
        self.current = revision_ptr;
        Some(metadata)
    }

    pub fn extract_raw(&self, range: Range<usize>, out: &mut Vec<u8>, mut out_off: usize) {
        let end = range.end.min(self.len());
        let mut beg = range.start.min(end);
        out_off = out_off.min(out.len());
        out.reserve(end - beg);

        while beg < end {
            let chunk = self.read_forward(beg);
            let chunk = &chunk[..chunk.len().min(end - beg)];
            out.replace_range(out_off..out_off, chunk);
            beg += chunk.len();
            out_off += chunk.len();
        }
    }

    pub fn clear(&mut self) {
        self.set_root(None);
        self.generation = self.generation.wrapping_add(1);
    }

    pub fn copy_from(&mut self, src: &dyn ReadableDocument) -> bool {
        let mut offset = 0;
        loop {
            let dst = self.read_forward(offset);
            let src = src.read_forward(offset);
            let len = dst.len().min(src.len());
            if dst[..len] != src[..len] {
                break;
            }
            if len == 0 {
                if dst.len() == src.len() {
                    return false;
                }
                break;
            }
            offset += len;
        }

        let mut replacement = Vec::new();
        let mut src_offset = offset;
        loop {
            let chunk = src.read_forward(src_offset);
            if chunk.is_empty() {
                break;
            }
            replacement.extend_from_slice(chunk);
            src_offset += chunk.len();
        }
        self.replace(offset..usize::MAX, &replacement);
        true
    }

    pub fn copy_into(&self, dst: &mut dyn WriteableDocument) {
        let mut source_offset = 0;
        let mut destination_offset = 0;
        while {
            let chunk = self.read_forward(source_offset);
            dst.replace(destination_offset..usize::MAX, chunk);
            destination_offset = usize::MAX;
            source_offset += chunk.len();
            source_offset < self.len()
        } {}
    }

    fn root(&self) -> NodePtr {
        unsafe { self.current.as_ref().root }
    }

    fn set_root(&mut self, root: NodePtr) {
        unsafe { self.current.as_mut().root = root };
    }

    fn alloc_leaf(&self, piece: Piece) -> NonNull<Node> {
        #[cfg(test)]
        {
            self.node_allocations.set(self.node_allocations.get() + 1);
        }
        NonNull::from(self.arena.alloc_uninit().write(Node {
            kind: NodeKind::Leaf(piece),
            len: piece.len,
            height: 1,
        }))
    }

    fn branch(&self, reuse: NodePtr, left: NonNull<Node>, right: NonNull<Node>) -> NonNull<Node> {
        debug_assert!(reuse != Some(left) && reuse != Some(right));
        let len = unsafe { left.as_ref().len + right.as_ref().len };
        let height = unsafe { left.as_ref().height.max(right.as_ref().height) + 1 };

        if let Some(mut node) = reuse
            && self.is_pending(node)
        {
            unsafe {
                let node = node.as_mut();
                node.kind = NodeKind::Branch { left, right };
                node.len = len;
                node.height = height;
            }
            return node;
        }

        #[cfg(test)]
        {
            self.node_allocations.set(self.node_allocations.get() + 1);
        }
        NonNull::from(self.arena.alloc_uninit().write(Node {
            kind: NodeKind::Branch { left, right },
            len,
            height,
        }))
    }

    fn is_pending(&self, node: NonNull<Node>) -> bool {
        // This arena only grows. The revision itself is the allocation watermark; all nodes
        // allocated after it belong exclusively to the pending revision.
        node.as_ptr().addr() > self.current.as_ptr().addr()
    }

    fn join(&self, left: NodePtr, right: NodePtr) -> NodePtr {
        self.join_reusing(left, right, None)
    }

    fn join_reusing(&self, left: NodePtr, right: NodePtr, reuse: NodePtr) -> NodePtr {
        let (Some(left), Some(right)) = (left, right) else {
            return left.or(right);
        };
        let left_ref = unsafe { left.as_ref() };
        let right_ref = unsafe { right.as_ref() };

        if left_ref.height > right_ref.height + 1 {
            let NodeKind::Branch { left: ll, right: lr } = left_ref.kind else {
                unreachable!();
            };
            // The old spine is consumed by this join, so pending branches can be recycled.
            return self.balance_reusing(
                ll,
                self.join_reusing(Some(lr), Some(right), Some(left)).unwrap(),
                reuse,
            );
        }
        if right_ref.height > left_ref.height + 1 {
            let NodeKind::Branch { left: rl, right: rr } = right_ref.kind else {
                unreachable!();
            };
            return self.balance_reusing(
                self.join_reusing(Some(left), Some(rl), Some(right)).unwrap(),
                rr,
                reuse,
            );
        }

        Some(self.branch(reuse, left, right))
    }

    fn balance_reusing(
        &self,
        left: NonNull<Node>,
        right: NonNull<Node>,
        reuse: NodePtr,
    ) -> NodePtr {
        let left_height = unsafe { left.as_ref().height };
        let right_height = unsafe { right.as_ref().height };

        if left_height > right_height + 1 {
            let NodeKind::Branch { left: ll, right: lr } = (unsafe { left.as_ref().kind }) else {
                unreachable!();
            };
            if unsafe { ll.as_ref().height >= lr.as_ref().height } {
                return Some(self.branch(reuse, ll, self.branch(Some(left), lr, right)));
            }
            let NodeKind::Branch { left: lrl, right: lrr } = (unsafe { lr.as_ref().kind }) else {
                unreachable!();
            };
            return Some(self.branch(
                reuse,
                self.branch(Some(left), ll, lrl),
                self.branch(Some(lr), lrr, right),
            ));
        }
        if right_height > left_height + 1 {
            let NodeKind::Branch { left: rl, right: rr } = (unsafe { right.as_ref().kind }) else {
                unreachable!();
            };
            if unsafe { rr.as_ref().height >= rl.as_ref().height } {
                return Some(self.branch(reuse, self.branch(Some(right), left, rl), rr));
            }
            let NodeKind::Branch { left: rll, right: rlr } = (unsafe { rl.as_ref().kind }) else {
                unreachable!();
            };
            return Some(self.branch(
                reuse,
                self.branch(Some(rl), left, rll),
                self.branch(Some(right), rlr, rr),
            ));
        }

        Some(self.branch(reuse, left, right))
    }

    fn split(&self, root: NodePtr, offset: usize) -> (NodePtr, NodePtr) {
        let Some(root) = root else {
            return (None, None);
        };
        let node = unsafe { root.as_ref() };
        let offset = offset.min(node.len);
        if offset == 0 {
            return (None, Some(root));
        }
        if offset == node.len {
            return (Some(root), None);
        }

        match node.kind {
            NodeKind::Leaf(piece) => {
                let left = Piece { text: piece.text, len: offset, capacity: 0 };
                let right = Piece {
                    text: unsafe { piece.text.add(offset) },
                    len: piece.len - offset,
                    capacity: 0,
                };
                if self.is_pending(root) {
                    unsafe {
                        let node = root.as_ptr();
                        (*node).kind = NodeKind::Leaf(left);
                        (*node).len = left.len;
                        (*node).height = 1;
                    }
                    (Some(root), Some(self.alloc_leaf(right)))
                } else {
                    (Some(self.alloc_leaf(left)), Some(self.alloc_leaf(right)))
                }
            }
            NodeKind::Branch { left, right } => {
                let left_len = unsafe { left.as_ref().len };
                if offset < left_len {
                    let (beg, end) = self.split(Some(left), offset);
                    (beg, self.join_reusing(end, Some(right), Some(root)))
                } else if offset == left_len {
                    (Some(left), Some(right))
                } else {
                    let (beg, end) = self.split(Some(right), offset - left_len);
                    (self.join_reusing(Some(left), beg, Some(root)), end)
                }
            }
        }
    }

    #[allow(clippy::mut_from_ref)]
    fn traversal_cache(&self) -> &mut TraversalCache {
        let traversal = self.traversal.get().unwrap_or_else(|| {
            let traversal = NonNull::from(
                self.arena.alloc_uninit().write(UnsafeCell::new(TraversalCache::new())),
            );
            self.traversal.set(Some(traversal));
            traversal
        });
        unsafe { &mut *traversal.as_ref().get() }
    }

    fn traversal_cache_is_current(&self, cache: &TraversalCache) -> bool {
        cache.valid && cache.root == self.root() && cache.generation == self.generation
    }

    fn traversal_seek(&self, offset: usize) -> Option<Piece> {
        #[cfg(test)]
        self.traversal_seeks.set(self.traversal_seeks.get() + 1);
        let root = self.root()?;
        let generation = self.generation;
        let cache = self.traversal_cache();
        cache.root = Some(root);
        cache.generation = generation;
        cache.depth = 0;
        cache.start = 0;

        let mut node = root;
        let mut remainder = offset;
        loop {
            match unsafe { node.as_ref().kind } {
                NodeKind::Leaf(piece) => {
                    cache.leaf = node;
                    cache.text = piece.text;
                    cache.end = cache.start + piece.len;
                    cache.valid = true;
                    return Some(piece);
                }
                NodeKind::Branch { left, right } => {
                    debug_assert!(cache.depth < cache.path.len());
                    let left_len = unsafe { left.as_ref().len };
                    let went_right = remainder >= left_len;
                    cache.path[cache.depth].write(TraversalPathEntry { branch: node, went_right });
                    cache.depth += 1;
                    if went_right {
                        remainder -= left_len;
                        cache.start += left_len;
                        node = right;
                    } else {
                        node = left;
                    }
                }
            }
        }
    }

    fn traversal_next(&self) -> Option<Piece> {
        let cache = self.traversal_cache();
        let index = (0..cache.depth)
            .rev()
            .find(|&index| !unsafe { cache.path[index].assume_init_ref() }.went_right)?;
        let branch = unsafe { cache.path[index].assume_init_ref() }.branch;
        let NodeKind::Branch { right, .. } = (unsafe { branch.as_ref().kind }) else {
            unreachable!();
        };
        unsafe { cache.path[index].assume_init_mut() }.went_right = true;
        cache.depth = index + 1;
        cache.start = cache.end;

        let mut node = right;
        loop {
            match unsafe { node.as_ref().kind } {
                NodeKind::Leaf(piece) => {
                    cache.leaf = node;
                    cache.text = piece.text;
                    cache.end = cache.start + piece.len;
                    return Some(piece);
                }
                NodeKind::Branch { left, .. } => {
                    debug_assert!(cache.depth < cache.path.len());
                    cache.path[cache.depth]
                        .write(TraversalPathEntry { branch: node, went_right: false });
                    cache.depth += 1;
                    node = left;
                }
            }
        }
    }

    fn traversal_previous(&self) -> Option<Piece> {
        let cache = self.traversal_cache();
        let index = (0..cache.depth)
            .rev()
            .find(|&index| unsafe { cache.path[index].assume_init_ref() }.went_right)?;
        let branch = unsafe { cache.path[index].assume_init_ref() }.branch;
        let NodeKind::Branch { left, .. } = (unsafe { branch.as_ref().kind }) else {
            unreachable!();
        };
        unsafe { cache.path[index].assume_init_mut() }.went_right = false;
        cache.depth = index + 1;
        cache.end = cache.start;

        let mut node = left;
        loop {
            match unsafe { node.as_ref().kind } {
                NodeKind::Leaf(piece) => {
                    cache.leaf = node;
                    cache.text = piece.text;
                    cache.start = cache.end - piece.len;
                    return Some(piece);
                }
                NodeKind::Branch { right, .. } => {
                    debug_assert!(cache.depth < cache.path.len());
                    cache.path[cache.depth]
                        .write(TraversalPathEntry { branch: node, went_right: true });
                    cache.depth += 1;
                    node = right;
                }
            }
        }
    }

    fn pending_leaf_at(&self, offset: usize) -> Option<(NonNull<Node>, usize)> {
        if offset >= self.len() {
            return None;
        }

        let mut node = self.root()?;
        let mut remainder = offset;
        loop {
            if !self.is_pending(node) {
                return None;
            }
            match unsafe { node.as_ref().kind } {
                NodeKind::Leaf(_) => return Some((node, remainder)),
                NodeKind::Branch { left, right } => {
                    let left_len = unsafe { left.as_ref().len };
                    if remainder < left_len {
                        node = left;
                    } else {
                        remainder -= left_len;
                        node = right;
                    }
                }
            }
        }
    }

    fn adjust_pending_ancestors(&self, offset: usize, delta: isize) {
        let mut node = self.root().unwrap();
        let mut remainder = offset;
        loop {
            debug_assert!(self.is_pending(node));
            let NodeKind::Branch { left, right } = (unsafe { node.as_ref().kind }) else {
                return;
            };
            let left_len = unsafe { left.as_ref().len };
            unsafe {
                node.as_mut().len = node.as_ref().len.saturating_add_signed(delta);
            }
            if remainder < left_len {
                node = left;
            } else {
                remainder -= left_len;
                node = right;
            }
        }
    }

    fn try_append_pending(&mut self, offset: usize, replacement: &[u8]) -> bool {
        if replacement.is_empty() || offset == 0 {
            return false;
        }

        let Some((mut leaf, remainder)) = self.pending_leaf_at(offset - 1) else {
            return false;
        };

        let NodeKind::Leaf(mut piece) = (unsafe { leaf.as_ref().kind }) else {
            unreachable!();
        };
        if remainder + 1 != piece.len {
            return false;
        }

        unsafe {
            self.adjust_pending_ancestors(offset - 1, replacement.len() as isize);
            let len = piece.len + replacement.len();
            if len > piece.capacity {
                let capacity =
                    if len <= MAX_EDIT_LEAF { len.next_power_of_two().max(64) } else { len };
                if piece.capacity == 0 {
                    let text =
                        self.arena.alloc_uninit_slice::<u8>(capacity).as_mut_ptr().cast::<u8>();
                    piece.text.as_ptr().copy_to_nonoverlapping(text, piece.len);
                    piece.text = NonNull::new_unchecked(text);
                } else {
                    piece.text = self.arena.realloc(piece.text, piece.capacity, capacity, 1).cast();
                }
                piece.capacity = capacity;
            }
            replacement
                .as_ptr()
                .copy_to_nonoverlapping(piece.text.as_ptr().add(piece.len), replacement.len());
            piece.len += replacement.len();
            leaf.as_mut().kind = NodeKind::Leaf(piece);
            leaf.as_mut().len = piece.len;
        }
        true
    }

    fn try_trim_pending(&mut self, range: Range<usize>) -> bool {
        if range.is_empty() || range.start >= self.len() {
            return false;
        }

        let Some((mut leaf, remainder)) = self.pending_leaf_at(range.start) else {
            return false;
        };

        let NodeKind::Leaf(mut piece) = (unsafe { leaf.as_ref().kind }) else {
            unreachable!();
        };
        let count = range.end.saturating_sub(range.start);
        if count >= piece.len || remainder + count > piece.len {
            return false;
        }
        if remainder != 0 && remainder + count != piece.len {
            return false;
        }

        if remainder == 0 {
            piece.text = unsafe { piece.text.add(count) };
            piece.capacity = piece.capacity.saturating_sub(count);
        }
        piece.len -= count;
        unsafe {
            self.adjust_pending_ancestors(range.start, -(count as isize));
            leaf.as_mut().kind = NodeKind::Leaf(piece);
            leaf.as_mut().len = piece.len;
        }
        true
    }

    /// Mutates the pending revision in place where possible. The caller guarantees that this
    /// replacement belongs to the currently coalescing editor operation.
    pub fn replace_coalescing(&mut self, range: Range<usize>, replacement: &[u8]) {
        debug_assert!(
            unsafe { self.current.as_ref().previous.is_some() },
            "coalescing requires a pending revision"
        );
        let len = self.len();
        let beg = range.start.min(len);
        let end = range.end.min(len).max(beg);
        if self.replace_in_leaf(beg..end, replacement) {
            return;
        }
        let changed = if beg == end {
            self.try_append_pending(beg, replacement)
        } else if replacement.is_empty() {
            self.try_trim_pending(beg..end)
        } else {
            false
        };

        if changed {
            self.generation = self.generation.wrapping_add(1);
        } else {
            self.replace_fallback(beg..end, replacement);
        }
    }

    #[cfg(test)]
    pub fn diagnostics(&self) -> PieceTreeDiagnostics {
        let mut reachable_revisions = 1;
        let mut revision = unsafe { self.current.as_ref().previous };
        while let Some(current) = revision {
            reachable_revisions += 1;
            revision = unsafe { current.as_ref().previous };
        }

        let mut pieces = 0;
        let mut stack = Vec::new();
        if let Some(root) = self.root() {
            stack.push(root);
        }
        while let Some(node) = stack.pop() {
            match unsafe { node.as_ref().kind } {
                NodeKind::Leaf(_) => pieces += 1,
                NodeKind::Branch { left, right } => {
                    stack.push(left);
                    stack.push(right);
                }
            }
        }

        PieceTreeDiagnostics {
            node_allocations: self.node_allocations.get(),
            revision_allocations: self.revision_allocations.get(),
            reachable_revisions,
            pieces,
            height: self.root().map_or(0, |root| unsafe { root.as_ref().height as usize }),
            arena_bytes: self.arena.offset(),
            traversal_seeks: self.traversal_seeks.get(),
        }
    }

    #[cfg(test)]
    pub fn revision_allocation_count(&self) -> usize {
        self.revision_allocations.get()
    }
}

impl<T: Copy> ReadableDocument for PieceTreeAvl<T> {
    fn read_forward(&self, off: usize) -> &[u8] {
        if off >= self.len() {
            return &[];
        }

        let cache = self.traversal_cache();
        let current = self.traversal_cache_is_current(cache);
        if current && (cache.start..cache.end).contains(&off) {
            return unsafe {
                slice::from_raw_parts(cache.text.add(off - cache.start).as_ptr(), cache.end - off)
            };
        }
        if current && off == cache.end {
            self.traversal_next().unwrap();
        } else {
            self.traversal_seek(off).unwrap();
        }
        let cache = self.traversal_cache();
        unsafe {
            slice::from_raw_parts(cache.text.add(off - cache.start).as_ptr(), cache.end - off)
        }
    }

    fn read_backward(&self, off: usize) -> &[u8] {
        let off = off.min(self.len());
        if off == 0 {
            return &[];
        }

        let cache = self.traversal_cache();
        let current = self.traversal_cache_is_current(cache);
        if current && off > cache.start && off <= cache.end {
            return unsafe { slice::from_raw_parts(cache.text.as_ptr(), off - cache.start) };
        }
        if current && off == cache.start {
            self.traversal_previous().unwrap();
        } else {
            self.traversal_seek(off - 1).unwrap();
        }
        let cache = self.traversal_cache();
        unsafe { slice::from_raw_parts(cache.text.as_ptr(), off - cache.start) }
    }
}

impl<T: Copy> WriteableDocument for PieceTreeAvl<T> {
    fn replace(&mut self, range: Range<usize>, replacement: &[u8]) {
        self.replace_impl(range, replacement);
    }
}

impl<T: Copy> PieceTreeAvl<T> {
    fn replace_in_leaf(&mut self, range: Range<usize>, replacement: &[u8]) -> bool {
        if self.root().is_none() {
            return false;
        }
        // Prefer the preceding piece for insertion at a boundary, as for ordinary typing.
        if range.is_empty() && range.start > 0 {
            self.read_backward(range.start);
        } else {
            self.read_forward(range.start);
        }
        let cache = self.traversal_cache();
        if !self.traversal_cache_is_current(cache)
            || range.start < cache.start
            || range.end > cache.end
        {
            return false;
        }
        let NodeKind::Leaf(piece) = (unsafe { cache.leaf.as_ref().kind }) else {
            unreachable!();
        };
        let len = piece.len - (range.end - range.start) + replacement.len();
        if len == 0 || len > MAX_EDIT_LEAF {
            return false;
        }
        let beg = range.start - cache.start;
        let end = range.end - cache.start;
        let pending = self.is_pending(cache.leaf);
        let mut leaf = if pending { cache.leaf } else { self.alloc_leaf(piece) };
        let capacity;
        let text;
        unsafe {
            if pending && piece.capacity >= len {
                capacity = piece.capacity;
                text = piece.text.as_ptr();
            } else {
                capacity = len.next_power_of_two().max(64);
                text = self.arena.alloc_uninit_slice::<u8>(capacity).as_mut_ptr().cast::<u8>();
                piece.text.as_ptr().copy_to_nonoverlapping(text, beg);
            }
            piece
                .text
                .add(end)
                .as_ptr()
                .copy_to(text.add(beg + replacement.len()), piece.len - end);
            replacement.as_ptr().copy_to_nonoverlapping(text.add(beg), replacement.len());
            leaf.as_mut().kind =
                NodeKind::Leaf(Piece { text: NonNull::new_unchecked(text), len, capacity });
            leaf.as_mut().len = len;
        }
        let mut root = leaf;
        // The nonempty leaf keeps its height. Rebuild just this path, without split/join or
        // rotations, and keep the traversal cache valid even when frozen branches are copied.
        for entry in cache.path[..cache.depth].iter_mut().rev() {
            let entry = unsafe { entry.assume_init_mut() };
            let NodeKind::Branch { left, right } = (unsafe { entry.branch.as_ref().kind }) else {
                unreachable!();
            };
            root = if entry.went_right {
                self.branch(Some(entry.branch), left, root)
            } else {
                self.branch(Some(entry.branch), root, right)
            };
            entry.branch = root;
        }
        cache.root = Some(root);
        cache.leaf = leaf;
        cache.text = unsafe { NonNull::new_unchecked(text) };
        cache.end = cache.start + len;
        cache.generation = self.generation.wrapping_add(1);
        self.set_root(Some(root));
        self.generation = self.generation.wrapping_add(1);
        true
    }

    fn replace_impl(&mut self, range: Range<usize>, replacement: &[u8]) {
        let len = self.len();
        let beg = range.start.min(len);
        let end = range.end.min(len).max(beg);
        if self.replace_in_leaf(beg..end, replacement) {
            return;
        }
        self.replace_fallback(beg..end, replacement);
    }

    fn replace_fallback(&mut self, range: Range<usize>, replacement: &[u8]) {
        let root = self.root();
        let (left, rest) = self.split(root, range.start);
        let (_, right) = self.split(rest, range.end - range.start);

        let inserted = if replacement.is_empty() {
            None
        } else {
            Some(self.alloc_leaf(Piece {
                text: NonNull::dangling(),
                len: replacement.len(),
                capacity: replacement.len(),
            }))
        };

        let root = self.join(self.join(left, inserted), right);
        if let Some(mut inserted) = inserted {
            debug_assert!(self.is_pending(inserted));
            let NodeKind::Leaf(mut piece) = (unsafe { inserted.as_ref().kind }) else {
                unreachable!();
            };
            debug_assert_eq!(piece.text, NonNull::dangling());
            let text =
                self.arena.alloc_uninit_slice(replacement.len()).write_copy_of_slice(replacement);
            piece.text = NonNull::from(&mut text[0]);
            unsafe { inserted.as_mut().kind = NodeKind::Leaf(piece) };
        }
        self.set_root(root);
        self.generation = self.generation.wrapping_add(1);
    }
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use super::PieceTreeAvl;
    use crate::arena::scratch_arena;
    use crate::document::{ReadableDocument as _, WriteableDocument as _};
    use crate::json;

    fn contents<T: Copy>(tree: &PieceTreeAvl<T>) -> Vec<u8> {
        let mut result = Vec::new();
        tree.extract_raw(0..usize::MAX, &mut result, 0);
        result
    }

    fn validate(root: super::NodePtr) -> (usize, u8) {
        let Some(root) = root else { return (0, 0) };
        let node = unsafe { root.as_ref() };
        let (len, height) = match node.kind {
            super::NodeKind::Leaf(piece) => {
                assert!(piece.len > 0);
                assert!(piece.capacity == 0 || piece.capacity >= piece.len);
                (piece.len, 1)
            }
            super::NodeKind::Branch { left, right } => {
                let (left_len, left_height) = validate(Some(left));
                let (right_len, right_height) = validate(Some(right));
                assert!(left_height.abs_diff(right_height) <= 1);
                (left_len + right_len, 1 + left_height.max(right_height))
            }
        };
        assert_eq!(node.len, len);
        assert_eq!(node.height, height);
        (len, height)
    }

    #[test]
    fn replace_and_read_across_pieces() {
        let mut tree = PieceTreeAvl::new(true, ()).unwrap();
        tree.replace(0..0, b"abcd");
        tree.replace(2..2, b"XY");
        tree.replace(1..5, b"!");

        assert_eq!(contents(&tree), b"a!d");
        assert!(tree.read_forward(1).starts_with(b"!"));
        assert!(tree.read_backward(3).ends_with(b"d"));
    }

    #[test]
    fn linear_reads_walk_adjacent_pieces_without_seeking() {
        let mut tree = PieceTreeAvl::new(true, ()).unwrap();
        let mut expected = Vec::new();
        for byte in b"abcd" {
            let text = vec![*byte; super::MAX_EDIT_LEAF + 1];
            tree.replace(tree.len()..tree.len(), &text);
            expected.extend(text);
        }

        let seeks_before = tree.diagnostics().traversal_seeks;
        let mut offset = 0;
        let mut forward = Vec::new();
        while offset < tree.len() {
            let chunk = tree.read_forward(offset);
            forward.extend_from_slice(chunk);
            offset += chunk.len();
        }
        assert_eq!(forward, expected);
        assert_eq!(tree.diagnostics().traversal_seeks, seeks_before + 1);

        let mut offset = tree.len();
        let mut backward = Vec::<u8>::new();
        while offset > 0 {
            let chunk = tree.read_backward(offset);
            backward.extend(chunk.iter().rev());
            offset -= chunk.len();
        }
        expected.reverse();
        assert_eq!(backward, expected);
        assert_eq!(tree.diagnostics().traversal_seeks, seeks_before + 1);
    }

    #[test]
    fn revisions_restore_roots_and_metadata() {
        let mut tree = PieceTreeAvl::new(true, 0).unwrap();
        tree.begin_revision(10, false);
        tree.replace(0..0, b"a");
        tree.begin_revision(20, true);
        tree.replace(1..1, b"b");
        tree.begin_revision(20, true);
        tree.replace(2..2, b"c");

        assert_eq!(contents(&tree), b"abc");
        assert_eq!(tree.undo(30), Some(10));
        assert_eq!(contents(&tree), b"");
        assert_eq!(tree.redo(10), Some(30));
        assert_eq!(contents(&tree), b"abc");
    }

    #[test]
    fn randomized_replacements_match_vec() {
        let mut tree = PieceTreeAvl::new(true, ()).unwrap();
        let mut expected = Vec::new();
        let mut random = 0x1234_5678_u32;

        for _ in 0..2000 {
            random = random.wrapping_mul(1_664_525).wrapping_add(1_013_904_223);
            let beg = (random as usize) % (expected.len() + 1);
            random = random.wrapping_mul(1_664_525).wrapping_add(1_013_904_223);
            let end = beg + (random as usize) % (expected.len() - beg + 1);
            let replacement = [b'a' + (random % 26) as u8, b'0' + (random % 10) as u8];
            let replacement = &replacement[..(random as usize >> 8) % 3];

            expected.splice(beg..end, replacement.iter().copied());
            tree.replace(beg..end, replacement);
            assert_eq!(contents(&tree), expected);
            validate(tree.root());
        }
    }

    #[test]
    fn pending_text_preserves_revisions_and_branches() {
        let mut tree = PieceTreeAvl::new(true, 0).unwrap();
        let mut expected = vec![b'a'; super::MAX_EDIT_LEAF * 20];
        tree.replace(0..0, &expected);
        tree.reset_history(0);
        let mut snapshots = vec![(expected.clone(), tree.generation())];
        let mut random = 0x1234_5678u32;
        for revision in 1..=150 {
            tree.begin_revision(revision, false);
            for edit in 0..8 {
                random = random.wrapping_mul(1_664_525).wrapping_add(1_013_904_223);
                let beg = random as usize % (expected.len() + 1);
                let end = (beg + ((random >> 16) as usize % 40)).min(expected.len());
                let text = vec![b'a' + (random % 26) as u8; (random >> 24) as usize % 80];
                tree.begin_revision(revision + 1000, true);
                tree.replace_coalescing(beg..end, &text);
                expected.splice(beg..end, text);
                validate(tree.root());
                assert_eq!(contents(&tree), expected, "{revision}:{edit}");
            }
            snapshots.push((expected.clone(), tree.generation()));
        }
        for revision in (1..snapshots.len()).rev() {
            assert_eq!(tree.undo(revision as u32 + 1000), Some(revision as u32));
            assert_eq!(contents(&tree), snapshots[revision - 1].0);
            assert_eq!(tree.generation(), snapshots[revision - 1].1);
            validate(tree.root());
        }
        for (revision, (text, generation)) in snapshots.iter().enumerate().skip(1) {
            assert_eq!(tree.redo(revision as u32), Some(revision as u32 + 1000));
            assert_eq!(&contents(&tree), text);
            assert_eq!(tree.generation(), *generation);
            validate(tree.root());
        }
        tree.undo(0).unwrap();
        tree.begin_revision(999, true);
        tree.replace_coalescing(5..8, b"branch");
        assert_eq!(tree.redo(0), None);
        assert_eq!(tree.undo(0), Some(149));
        assert_eq!(contents(&tree), snapshots[148].0);
        tree.redo(0).unwrap();
        let branch = contents(&tree);
        tree.reset_history(0);
        tree.begin_revision(1, false);
        tree.replace_coalescing(0..tree.len(), b"new");
        tree.undo(0).unwrap();
        assert_eq!(contents(&tree), branch);
    }

    #[test]
    fn splitting_owned_text_does_not_alias_other_pieces() {
        let mut tree = PieceTreeAvl::new(true, ()).unwrap();
        let text = vec![b'a'; super::MAX_EDIT_LEAF];
        tree.replace(0..0, &text);
        tree.begin_revision((), false);
        let middle = vec![b'b'; super::MAX_EDIT_LEAF];
        let offset = text.len() / 2;
        tree.replace_coalescing(offset..offset, &middle);
        let mut expected = text.clone();
        expected.splice(offset..offset, middle);
        tree.replace_coalescing(0..2, b"xyz");
        expected.splice(0..2, *b"xyz");
        tree.replace_coalescing(expected.len() - 2..expected.len(), b"last");
        expected.splice(expected.len() - 2..expected.len(), *b"last");
        assert_eq!(contents(&tree), expected);
        validate(tree.root());
        tree.undo(()).unwrap();
        assert_eq!(contents(&tree), text);
        tree.redo(()).unwrap();
        assert_eq!(contents(&tree), expected);
    }

    #[test]
    fn local_edits_keep_cache_and_pending_allocations() {
        let mut tree = PieceTreeAvl::new(true, ()).unwrap();
        tree.replace(0..0, b"left right");
        tree.begin_revision((), false);
        tree.replace_coalescing(4..4, b"!");
        let nodes = tree.diagnostics().node_allocations;
        let seeks = tree.diagnostics().traversal_seeks;
        let bytes = tree.diagnostics().arena_bytes;
        for _ in 0..100 {
            tree.replace_coalescing(4..5, b"longer");
            assert_eq!(tree.read_forward(4), b"longer right");
            tree.replace_coalescing(4..10, b"!");
            assert_eq!(tree.read_backward(5), b"left!");
        }
        assert_eq!(tree.diagnostics().node_allocations, nodes);
        assert_eq!(tree.diagnostics().traversal_seeks, seeks);
        assert_eq!(tree.diagnostics().arena_bytes, bytes);
        tree.undo(()).unwrap();
        assert_eq!(contents(&tree), b"left right");
        tree.redo(()).unwrap();
        assert_eq!(contents(&tree), b"left! right");
    }

    #[test]
    fn rustcode_logical_replay() {
        use crate::buffer::{CursorMovement, PieceTreeAvlStorage, TextBuffer};
        use crate::helpers::CoordType;

        let compressed = include_bytes!("../../../../assets/editing-traces/rustcode.json.zst");
        let decoded = zstd::decode_all(Cursor::new(compressed)).unwrap();
        let scratch = scratch_arena(None);
        let data = json::parse(&scratch, str::from_utf8(&decoded).unwrap()).unwrap();
        let root = data.as_object().unwrap();
        let mut reference = TextBuffer::new(false).unwrap();
        let mut buffer = TextBuffer::<PieceTreeAvlStorage>::new_with_storage(false).unwrap();
        reference.set_crlf(false);
        buffer.set_crlf(false);
        let start = root.get_str("startContent").unwrap().as_bytes();
        reference.write_raw(start);
        buffer.write_raw(start);

        for transaction in root.get_array("txns").unwrap() {
            for patch in transaction.as_object().unwrap().get_array("patches").unwrap() {
                let patch = patch.as_array().unwrap();
                let offset = patch[0].as_number().unwrap() as usize;
                let delete = patch[1].as_number().unwrap() as CoordType;
                let insert = patch[2].as_str().unwrap().as_bytes();
                if offset != reference.cursor_offset() {
                    reference.cursor_move_to_offset(offset);
                    buffer.cursor_move_to_logical(reference.cursor_logical_pos());
                }
                if delete > 0 && !insert.is_empty() {
                    reference.selection_update_delta(CursorMovement::Grapheme, delete);
                    buffer.selection_update_delta(CursorMovement::Grapheme, delete);
                    reference.write_raw(insert);
                    buffer.write_raw(insert);
                } else if delete > 0 {
                    reference.delete(CursorMovement::Grapheme, delete);
                    buffer.delete(CursorMovement::Grapheme, delete);
                } else {
                    reference.write_raw(insert);
                    buffer.write_raw(insert);
                }
            }
        }
        let mut actual = String::new();
        buffer.save_as_string(&mut actual);
        assert_eq!(actual, root.get_str("endContent").unwrap());
        validate(buffer.buffer.root());
        eprintln!("rustcode logical: {:?}", buffer.buffer.diagnostics());
    }

    #[test]
    fn investigate_rustcode_node_growth() {
        let compressed = include_bytes!("../../../../assets/editing-traces/rustcode.json.zst");
        let decoded = zstd::decode_all(Cursor::new(compressed)).unwrap();
        let decoded = str::from_utf8(&decoded).unwrap();
        let scratch = scratch_arena(None);
        let data = json::parse(&scratch, decoded).unwrap();
        let root = data.as_object().unwrap();
        let transactions = root.get_array("txns").unwrap();

        let mut tree = PieceTreeAvl::new(false, ()).unwrap();
        tree.replace(0..usize::MAX, root.get_str("startContent").unwrap().as_bytes());

        let mut patches = 0usize;
        let mut max_nodes_per_patch = 0usize;
        for transaction in transactions {
            let transaction = transaction.as_object().unwrap();
            for patch in transaction.get_array("patches").unwrap() {
                let patch = patch.as_array().unwrap();
                let offset = patch[0].as_number().unwrap() as usize;
                let delete = patch[1].as_number().unwrap() as usize;
                let insert = patch[2].as_str().unwrap().as_bytes();
                let nodes_before = tree.node_allocations.get();

                tree.replace(offset..offset + delete, insert);
                max_nodes_per_patch =
                    max_nodes_per_patch.max(tree.node_allocations.get() - nodes_before);
                patches += 1;
            }
        }

        let diagnostics = tree.diagnostics();
        let nodes_per_patch = diagnostics.node_allocations as f64 / patches as f64;
        let logarithmic_baseline = patches * diagnostics.height;
        eprintln!(
            "rustcode direct: patches={patches}, nodes_per_patch={nodes_per_patch:.2}, \
             max_nodes_per_patch={max_nodes_per_patch}, n_times_height={logarithmic_baseline}, \
             allocation_ratio={:.2}, diagnostics={diagnostics:?}",
            diagnostics.node_allocations as f64 / logarithmic_baseline as f64,
        );

        assert_eq!(contents(&tree), root.get_str("endContent").unwrap().as_bytes());
    }
}
