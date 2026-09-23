// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::cell::{Cell, UnsafeCell};
use std::ops::Range;
use std::ptr::NonNull;
use std::{io, slice};

use crate::alloc::Allocator as _;
use crate::arena::Arena;
use crate::document::{ReadableDocument, WriteableDocument};
use crate::helpers::*;

type NodePtr = Option<NonNull<Node>>;
type RevisionPtr<T> = NonNull<Revision<T>>;

#[cfg(target_pointer_width = "32")]
const LARGE_CAPACITY: usize = 512 * MEBI;
#[cfg(target_pointer_width = "64")]
const LARGE_CAPACITY: usize = 4 * GIBI;
const SMALL_CAPACITY: usize = 128 * MEBI;
// Benchmark-tuned bound on copying and unchanged text retained per revision.
const CHUNK_SIZE: usize = 256;

#[derive(Clone, Copy, Eq, PartialEq)]
enum Direction {
    Left,
    Right,
}

impl Direction {
    #[inline]
    fn opposite(self) -> Self {
        match self {
            Self::Left => Self::Right,
            Self::Right => Self::Left,
        }
    }

    #[inline]
    fn advance(self, start: usize, old_len: usize, new_len: usize) -> usize {
        match self {
            Self::Left => start - new_len,
            Self::Right => start + old_len,
        }
    }
}

#[derive(Clone, Copy)]
struct Span {
    text: NonNull<u8>,
    len: usize,
}

impl Span {
    #[inline]
    fn slice(self, range: Range<usize>) -> Self {
        debug_assert!(range.start <= range.end && range.end <= self.len);
        Self { text: unsafe { self.text.add(range.start) }, len: range.end - range.start }
    }

    #[inline]
    unsafe fn copy_to(self, dst: *mut u8) {
        if self.len > 0 {
            unsafe { self.text.as_ptr().copy_to_nonoverlapping(dst, self.len) };
        }
    }
}

struct Node {
    // Tail nodes keep their outward link; the inward slot is traversal scratch.
    // Changing a node's side or promoting it to a root always creates a copy.
    links: [Cell<NodePtr>; 2],
    span: Span,
    arena_offset: usize,
    // Exclusive text ownership, independent of the revision's node watermark.
    capacity: usize,
}

impl Node {
    #[inline]
    fn link(&self, direction: Direction) -> NodePtr {
        self.links[direction as usize].get()
    }

    #[inline]
    fn follow_outward(node: NonNull<Self>, direction: Direction) -> NonNull<Self> {
        let next = unsafe { node.as_ref().link(direction) }.unwrap();
        unsafe { next.as_ref() }.links[direction.opposite() as usize].set(Some(node));
        next
    }

    #[inline]
    fn splice(&mut self, arena: &Arena, range: Range<usize>, replacement: &[u8]) -> bool {
        let old_len = self.span.len;
        let new_len = old_len - (range.end - range.start) + replacement.len();
        if self.capacity > 0 && (1..=CHUNK_SIZE).contains(&new_len) {
            if new_len > self.capacity {
                let capacity = new_len.next_power_of_two();
                self.span.text =
                    unsafe { arena.realloc(self.span.text, self.capacity, capacity, 1).cast() };
                self.capacity = capacity;
            }
            unsafe {
                self.span.text.add(range.end).as_ptr().copy_to(
                    self.span.text.add(range.start + replacement.len()).as_ptr(),
                    old_len - range.end,
                );
                replacement.as_ptr().copy_to_nonoverlapping(
                    self.span.text.add(range.start).as_ptr(),
                    replacement.len(),
                );
            }
        } else if range.start == old_len && range.end == old_len && !replacement.is_empty() {
            // Realloc copies shared spans unless they are the arena's final allocation.
            unsafe {
                self.span.text = arena.realloc(self.span.text, old_len, new_len, 1).cast();
                replacement.as_ptr().copy_to_nonoverlapping(
                    self.span.text.add(old_len).as_ptr(),
                    replacement.len(),
                );
            }
            self.capacity = 0;
        } else if replacement.is_empty() && new_len > 0 && range.start < range.end {
            if range.start == 0 {
                self.span.text = unsafe { self.span.text.add(range.end) };
                self.capacity = 0;
            } else if range.end != old_len {
                return false;
            }
        } else {
            return false;
        }
        self.span.len = new_len;
        true
    }
}

#[derive(Clone, Copy)]
struct Snapshot {
    root: NodePtr,
    start: usize,
    len: usize,
}

impl Snapshot {
    const EMPTY: Self = Self { root: None, start: 0, len: 0 };
}

struct Revision<T> {
    snapshot: Snapshot,
    metadata: T,
    generation_before: u32,
    previous: Option<RevisionPtr<T>>,
    redo_next: Option<RevisionPtr<T>>,
    node_watermark: usize,
}

impl<T> Revision<T> {
    #[inline]
    fn alloc(
        arena: &Arena,
        snapshot: Snapshot,
        metadata: T,
        generation_before: u32,
        previous: Option<RevisionPtr<T>>,
    ) -> RevisionPtr<T> {
        let revision = arena.alloc_uninit().write(Self {
            snapshot,
            metadata,
            generation_before,
            previous,
            redo_next: None,
            node_watermark: 0,
        });
        revision.node_watermark = arena.offset();
        NonNull::from(revision)
    }

    #[inline]
    fn exchange(&mut self, metadata: T, generation: &mut u32) -> T {
        std::mem::swap(generation, &mut self.generation_before);
        std::mem::replace(&mut self.metadata, metadata)
    }
}

struct Focus {
    links: [NodePtr; 2],
    span: Span,
    start: usize,
}

#[derive(Clone, Copy)]
struct Fringe {
    outer: NodePtr,
    fragment: Span,
}

impl Fringe {
    fn absorb(&mut self, direction: Direction, total: &mut usize) -> usize {
        let mut len = self.fragment.len;
        while let Some(node) = self.outer {
            let node = unsafe { node.as_ref() };
            if *total + node.span.len > CHUNK_SIZE {
                break;
            }
            len += node.span.len;
            *total += node.span.len;
            self.outer = node.link(direction);
        }
        len
    }

    // `dst` is the edit boundary: left pieces are written backwards from it.
    #[inline]
    unsafe fn copy_to(self, direction: Direction, stop: NodePtr, mut dst: *mut u8) {
        let mut span = self.fragment;
        let mut next = self.outer;
        loop {
            unsafe {
                if direction == Direction::Left {
                    dst = dst.sub(span.len);
                }
                span.copy_to(dst);
                if direction == Direction::Right {
                    dst = dst.add(span.len);
                }
            }
            if next == stop {
                break;
            }
            let node = unsafe { next.unwrap().as_ref() };
            span = node.span;
            next = node.link(direction);
        }
    }
}

struct TraversalCache {
    revision: RevisionPtr<()>,
    generation: u32,
    root: NonNull<Node>,
    node: NonNull<Node>,
    start: usize,
    side: Option<Direction>,
}

impl TraversalCache {
    fn new() -> Self {
        Self {
            revision: NonNull::dangling(),
            generation: 0,
            root: NonNull::dangling(),
            node: NonNull::dangling(),
            start: 0,
            side: None,
        }
    }

    #[inline]
    fn locate<T>(&mut self, revision: RevisionPtr<T>, generation: u32, offset: usize) {
        if self.revision == revision.cast() && self.generation == generation {
            let end = self.start + unsafe { self.node.as_ref().span.len };
            if (self.start..end).contains(&offset) {
                return;
            }
            if offset + 1 == self.start || offset == end {
                let direction =
                    if offset < self.start { Direction::Left } else { Direction::Right };
                self.step(direction);
                return;
            }
        }
        self.seek(unsafe { revision.as_ref().snapshot }, offset);
        self.revision = revision.cast();
        self.generation = generation;
    }

    fn seek(&mut self, snapshot: Snapshot, offset: usize) {
        let mut node = snapshot.root.unwrap();
        self.root = node;
        let mut start = snapshot.start;
        self.side = if offset < start {
            while offset < start {
                node = Node::follow_outward(node, Direction::Left);
                start -= unsafe { node.as_ref().span.len };
            }
            Some(Direction::Left)
        } else if offset >= start + unsafe { node.as_ref().span.len } {
            while offset >= start + unsafe { node.as_ref().span.len } {
                start += unsafe { node.as_ref().span.len };
                node = Node::follow_outward(node, Direction::Right);
            }
            Some(Direction::Right)
        } else {
            None
        };
        self.node = node;
        self.start = start;
    }

    #[inline]
    fn step(&mut self, direction: Direction) {
        let old_len = unsafe { self.node.as_ref().span.len };
        if self.side == Some(direction.opposite()) {
            self.node = unsafe { self.node.as_ref().link(direction) }.unwrap();
            if self.node == self.root {
                self.side = None;
            }
        } else {
            self.node = Node::follow_outward(self.node, direction);
            self.side = Some(direction);
        }
        self.start = direction.advance(self.start, old_len, unsafe { self.node.as_ref().span.len });
    }
}

/// A persistent piece-list zipper centered on the most recent edit.
///
/// Outward links share history; moving the focus copies crossed nodes. Nearby
/// small pieces compact into 256-byte chunks, which pending edits can splice.
/// Reads establish scratch backlinks in tail nodes instead of storing a path.
/// See `zipper.md` for the topology, ownership rules, and benchmark policy.
pub struct Zipper<T: Copy> {
    arena: Arena,
    current: RevisionPtr<T>,
    redo: Option<RevisionPtr<T>>,
    generation: u32,
    traversal: UnsafeCell<TraversalCache>,
}

impl<T: Copy> Zipper<T> {
    pub fn new(small: bool, metadata: T) -> io::Result<Self> {
        let arena = Arena::new(if small { SMALL_CAPACITY } else { LARGE_CAPACITY })?;
        Ok(Self {
            current: Revision::alloc(&arena, Snapshot::EMPTY, metadata, 0, None),
            arena,
            redo: None,
            generation: 0,
            traversal: UnsafeCell::new(TraversalCache::new()),
        })
    }

    pub fn committed(&self) -> usize {
        const ALLOC_CHUNK_SIZE: usize = 64 * 1024;
        (self.arena.offset() + ALLOC_CHUNK_SIZE - 1) & !(ALLOC_CHUNK_SIZE - 1)
    }

    fn revision(&self) -> &Revision<T> {
        unsafe { self.current.as_ref() }
    }

    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        self.revision().snapshot.len
    }

    pub fn generation(&self) -> u32 {
        self.generation
    }

    fn push_revision(&mut self, metadata: T, previous: Option<RevisionPtr<T>>) {
        self.current = Revision::alloc(
            &self.arena,
            self.revision().snapshot,
            metadata,
            self.generation,
            previous,
        );
        self.redo = None;
    }

    pub fn begin_revision(&mut self, metadata: T, coalesce: bool) {
        self.redo = None;
        if !coalesce || self.revision().previous.is_none() {
            self.push_revision(metadata, Some(self.current));
        }
    }

    pub fn revision_metadata_mut(&mut self) -> &mut T {
        unsafe { &mut self.current.as_mut().metadata }
    }

    pub fn reset_history(&mut self, metadata: T) {
        self.push_revision(metadata, None);
    }

    pub fn undo(&mut self, current_metadata: T) -> Option<T> {
        let previous = self.revision().previous?;
        let revision = unsafe { self.current.as_mut() };
        let metadata = revision.exchange(current_metadata, &mut self.generation);
        revision.redo_next = self.redo;
        self.redo = Some(self.current);
        self.current = previous;
        Some(metadata)
    }

    pub fn redo(&mut self, current_metadata: T) -> Option<T> {
        let mut current = self.redo?;
        let revision = unsafe { current.as_mut() };
        let metadata = revision.exchange(current_metadata, &mut self.generation);
        self.redo = revision.redo_next.take();
        self.current = current;
        Some(metadata)
    }

    #[inline]
    fn alloc_node(&mut self, links: [NodePtr; 2], span: Span) -> NonNull<Node> {
        debug_assert!(span.len > 0);
        let arena_offset = self.arena.offset();
        NonNull::from(self.arena.alloc_uninit().write(Node {
            links: links.map(Cell::new),
            span,
            arena_offset,
            capacity: 0,
        }))
    }

    #[inline]
    fn alloc_text(&mut self, links: [NodePtr; 2], len: usize, capacity: usize) -> NonNull<Node> {
        // Keep text last so subsequent typing can grow it without copying.
        let mut node = self.alloc_node(links, Span { text: NonNull::dangling(), len });
        let text = self.arena.alloc_uninit_slice::<u8>(capacity);
        unsafe {
            node.as_mut().span.text = NonNull::new_unchecked(text.as_mut_ptr().cast());
        }
        node
    }

    #[inline]
    fn publish(&mut self, root: NodePtr, start: usize, len: usize) {
        unsafe { self.current.as_mut().snapshot = Snapshot { root, start, len } };
        self.generation = self.generation.wrapping_add(1);
    }

    #[inline]
    fn reanchor(&mut self, offset: usize) -> Focus {
        let snapshot = self.revision().snapshot;
        let root = unsafe { snapshot.root.unwrap().as_ref() };
        let mut focus = Focus {
            links: root.links.each_ref().map(Cell::get),
            span: root.span,
            start: snapshot.start,
        };
        let direction = if offset < focus.start { Direction::Left } else { Direction::Right };
        let outward = direction as usize;
        let inward = direction.opposite() as usize;
        while !(focus.start..=focus.start + focus.span.len).contains(&offset) {
            let next = unsafe { focus.links[outward].unwrap().as_ref() };
            let mut links = [None; 2];
            links[inward] = focus.links[inward];
            focus.links[inward] = Some(self.alloc_node(links, focus.span));
            focus.links[outward] = next.link(direction);
            focus.start = direction.advance(focus.start, focus.span.len, next.span.len);
            focus.span = next.span;
        }
        focus
    }

    #[inline]
    fn materialize(&mut self, fringe: Fringe, direction: Direction) -> NodePtr {
        if fringe.fragment.len == 0 {
            fringe.outer
        } else {
            let mut links = [None; 2];
            links[direction as usize] = fringe.outer;
            Some(self.alloc_node(links, fringe.fragment))
        }
    }

    #[inline]
    fn compact(
        &mut self,
        left: &mut Fringe,
        right: &mut Fringe,
        replacement: &[u8],
    ) -> Option<(NonNull<Node>, usize)> {
        let mut len = left.fragment.len + replacement.len() + right.fragment.len;
        if len > CHUNK_SIZE {
            return None;
        }
        let original_left = *left;
        let original_right = *right;
        let left_len = left.absorb(Direction::Left, &mut len);
        right.absorb(Direction::Right, &mut len);
        if len == 0 {
            return None;
        }
        let capacity = len.next_power_of_two().max(64);
        let mut root = self.alloc_text([left.outer, right.outer], len, capacity);
        unsafe {
            let dst = root.as_ref().span.text.as_ptr();
            original_left.copy_to(Direction::Left, left.outer, dst.add(left_len));
            replacement.as_ptr().copy_to_nonoverlapping(dst.add(left_len), replacement.len());
            original_right.copy_to(
                Direction::Right,
                right.outer,
                dst.add(left_len + replacement.len()),
            );
            root.as_mut().capacity = capacity;
        }
        Some((root, left_len))
    }

    fn replace_impl(&mut self, range: Range<usize>, replacement: &[u8]) {
        let old_len = self.len();
        let beg = range.start.min(old_len);
        let end = range.end.min(old_len).max(beg);
        if beg == end && replacement.is_empty() {
            return;
        }
        if old_len == 0 {
            let root = self.alloc_text([None; 2], replacement.len(), replacement.len());
            unsafe {
                replacement
                    .as_ptr()
                    .copy_to_nonoverlapping(root.as_ref().span.text.as_ptr(), replacement.len())
            };
            self.publish(Some(root), 0, replacement.len());
            return;
        }

        let focus = self.reanchor(beg);
        let mut left =
            Fringe { outer: focus.links[0], fragment: focus.span.slice(0..beg - focus.start) };
        let mut span = focus.span;
        let mut outer = focus.links[1];
        let mut remaining = end - focus.start;
        while remaining > span.len {
            remaining -= span.len;
            let node = unsafe { outer.unwrap().as_ref() };
            span = node.span;
            outer = node.link(Direction::Right);
        }
        let mut right = Fringe { outer, fragment: span.slice(remaining..span.len) };
        let new_len = old_len - (end - beg) + replacement.len();

        let (root, start) =
            if let Some((root, left_len)) = self.compact(&mut left, &mut right, replacement) {
                (Some(root), beg - left_len)
            } else if !replacement.is_empty() {
                let links = [
                    self.materialize(left, Direction::Left),
                    self.materialize(right, Direction::Right),
                ];
                let root = self.alloc_text(links, replacement.len(), replacement.len());
                unsafe {
                    replacement
                        .as_ptr()
                        .copy_to_nonoverlapping(root.as_ref().span.text.as_ptr(), replacement.len())
                };
                (Some(root), beg)
            } else if left.fragment.len > 0 {
                let next = self.materialize(right, Direction::Right);
                (Some(self.alloc_node([left.outer, next], left.fragment)), beg - left.fragment.len)
            } else if right.fragment.len > 0 {
                (Some(self.alloc_node([left.outer, right.outer], right.fragment)), beg)
            } else if let Some(next) = right.outer {
                let next = unsafe { next.as_ref() };
                (Some(self.alloc_node([left.outer, next.link(Direction::Right)], next.span)), beg)
            } else if let Some(prev) = left.outer {
                let prev = unsafe { prev.as_ref() };
                (
                    Some(self.alloc_node([prev.link(Direction::Left), None], prev.span)),
                    beg - prev.span.len,
                )
            } else {
                (None, 0)
            };
        self.publish(root, start, new_len);
    }

    pub fn replace_coalescing(&mut self, range: Range<usize>, replacement: &[u8]) {
        let revision = self.revision();
        debug_assert!(revision.previous.is_some());
        let snapshot = revision.snapshot;
        let beg = range.start.min(snapshot.len);
        let end = range.end.min(snapshot.len).max(beg);
        if let Some(mut root) = snapshot.root {
            let node = unsafe { root.as_ref() };
            let old_len = node.span.len;
            if node.arena_offset >= revision.node_watermark
                && beg >= snapshot.start
                && end <= snapshot.start + old_len
                && unsafe { root.as_mut() }.splice(
                    &self.arena,
                    beg - snapshot.start..end - snapshot.start,
                    replacement,
                )
            {
                self.publish(
                    Some(root),
                    snapshot.start,
                    snapshot.len - old_len + unsafe { root.as_ref().span.len },
                );
                return;
            }
        }
        self.replace_impl(beg..end, replacement);
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
        self.publish(None, 0, 0);
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

    #[inline]
    fn read(&self, off: usize, direction: Direction) -> &[u8] {
        let len = self.len();
        let off = off.min(len);
        let target = match direction {
            Direction::Left if off == 0 => return &[],
            Direction::Left => off - 1,
            Direction::Right if off == len => return &[],
            Direction::Right => off,
        };
        // Reads only mutate traversal state, never nodes or bytes. This is the
        // sole cache borrow; helpers cannot reacquire it through &self.
        let cache = unsafe { &mut *self.traversal.get() };
        cache.locate(self.current, self.generation, target);
        let span = unsafe { cache.node.as_ref().span };
        let relative = off - cache.start;
        debug_assert!((cache.start..cache.start + span.len).contains(&target));
        unsafe {
            match direction {
                Direction::Left => slice::from_raw_parts(span.text.as_ptr(), relative),
                Direction::Right => {
                    slice::from_raw_parts(span.text.add(relative).as_ptr(), span.len - relative)
                }
            }
        }
    }
}

impl<T: Copy> ReadableDocument for Zipper<T> {
    #[inline]
    fn read_forward(&self, off: usize) -> &[u8] {
        self.read(off, Direction::Right)
    }

    #[inline]
    fn read_backward(&self, off: usize) -> &[u8] {
        self.read(off, Direction::Left)
    }
}

impl<T: Copy> WriteableDocument for Zipper<T> {
    fn replace(&mut self, range: Range<usize>, replacement: &[u8]) {
        self.replace_impl(range, replacement);
    }
}
