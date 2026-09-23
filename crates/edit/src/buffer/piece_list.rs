// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::cell::Cell;
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
const TAIL_GRANULARITY: usize = 4 * KIBI;

type NodePtr = NonNull<Node>;
type ChangePtr = Option<NonNull<Change>>;
type RevisionPtr<T> = NonNull<Revision<T>>;

#[derive(Clone, Copy)]
struct Span {
    text: NonNull<u8>,
    len: usize,
}

impl Span {
    fn slice(self, range: Range<usize>) -> Self {
        debug_assert!(range.start <= range.end && range.end <= self.len);
        Self { text: unsafe { self.text.add(range.start) }, len: range.end - range.start }
    }
}

struct Node {
    prev: NodePtr,
    next: NodePtr,
    span: Span,
}

#[derive(Clone, Copy)]
struct Run {
    // Empty between left/right means first = right and last = left.
    first: NodePtr,
    last: NodePtr,
}

struct Change {
    previous: ChangePtr,
    next: ChangePtr,
    left: NodePtr,
    right: NodePtr,
    before: Run,
    after: Run,
}

struct Revision<T> {
    first: ChangePtr,
    last: ChangePtr,
    previous: Option<RevisionPtr<T>>,
    redo_next: Option<RevisionPtr<T>>,
    metadata: T,
    generation_before: u32,
    len_before: usize,
}

impl<T> Revision<T> {
    fn alloc(
        arena: &Arena,
        previous: Option<RevisionPtr<T>>,
        metadata: T,
        generation: u32,
        len: usize,
    ) -> RevisionPtr<T> {
        NonNull::from(arena.alloc_uninit().write(Self {
            first: None,
            last: None,
            previous,
            redo_next: None,
            metadata,
            generation_before: generation,
            len_before: len,
        }))
    }

    fn exchange(&mut self, metadata: T, generation: &mut u32, len: &mut usize) -> T {
        std::mem::swap(generation, &mut self.generation_before);
        std::mem::swap(len, &mut self.len_before);
        std::mem::replace(&mut self.metadata, metadata)
    }
}

#[derive(Clone, Copy)]
struct Cursor {
    node: NodePtr,
    start: usize,
}

/// A mutable doubly linked piece list with reversible splices.
///
/// Undo reconnects detached runs in reverse edit order; redo reconnects their
/// replacements in forward order. Historical roots cannot be read independently.
/// Nodes, text, and edit records stay in the arena until the list is dropped.
/// See `piece_list.md` for the splice and text-ownership invariants.
pub struct PieceList<T: Copy> {
    arena: Arena,
    sentinel: NodePtr,
    len: usize,
    generation: u32,
    current: RevisionPtr<T>,
    redo: Option<RevisionPtr<T>>,
    cursor: Cell<Cursor>,
    tail: NodePtr,
    tail_start: usize,
    tail_capacity: usize,
}

impl<T: Copy> PieceList<T> {
    pub fn new(small: bool, metadata: T) -> io::Result<Self> {
        let arena = Arena::new(if small { SMALL_CAPACITY } else { LARGE_CAPACITY })?;
        let sentinel = NonNull::from(arena.alloc_uninit().write(Node {
            prev: NonNull::dangling(),
            next: NonNull::dangling(),
            span: Span { text: NonNull::dangling(), len: 0 },
        }));
        Self::link(sentinel, sentinel);
        Ok(Self {
            current: Revision::alloc(&arena, None, metadata, 0, 0),
            arena,
            sentinel,
            len: 0,
            generation: 0,
            redo: None,
            cursor: Cell::new(Cursor { node: sentinel, start: 0 }),
            tail: sentinel,
            tail_start: 0,
            tail_capacity: 0,
        })
    }

    pub fn committed(&self) -> usize {
        const ALLOC_CHUNK_SIZE: usize = 64 * 1024;
        (self.arena.offset() + ALLOC_CHUNK_SIZE - 1) & !(ALLOC_CHUNK_SIZE - 1)
    }

    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        self.len
    }

    pub fn generation(&self) -> u32 {
        self.generation
    }

    fn revision(&self) -> &Revision<T> {
        unsafe { self.current.as_ref() }
    }

    fn finish_tail(&mut self) {
        if self.tail != self.sentinel {
            let span = unsafe { self.tail.as_ref().span };
            // Must precede every subsequent allocation, including revision records.
            unsafe { self.arena.realloc(span.text, self.tail_capacity, span.len, 1) };
            self.tail = self.sentinel;
        }
    }

    pub fn begin_revision(&mut self, metadata: T, coalesce: bool) {
        self.redo = None;
        if !coalesce || self.revision().previous.is_none() {
            self.finish_tail();
            self.current = Revision::alloc(
                &self.arena,
                Some(self.current),
                metadata,
                self.generation,
                self.len,
            );
        }
    }

    pub fn revision_metadata_mut(&mut self) -> &mut T {
        unsafe { &mut self.current.as_mut().metadata }
    }

    pub fn reset_history(&mut self, metadata: T) {
        self.finish_tail();
        self.current = Revision::alloc(&self.arena, None, metadata, self.generation, self.len);
        self.redo = None;
    }

    #[inline]
    fn link(mut left: NodePtr, mut right: NodePtr) {
        // Use separate field writes: both pointers may name the empty-list sentinel.
        unsafe {
            left.as_mut().next = right;
            right.as_mut().prev = left;
        }
    }

    // Only the active run's boundary links need repair. Detached runs keep their
    // interior links; intervening edits are reversed before a run is restored.
    #[inline]
    fn connect(left: NodePtr, right: NodePtr, run: Run) {
        Self::link(left, run.first);
        Self::link(run.last, right);
    }

    pub fn undo(&mut self, current_metadata: T) -> Option<T> {
        let previous = self.revision().previous?;
        self.finish_tail();
        let mut change = self.revision().last;
        while let Some(ptr) = change {
            let record = unsafe { ptr.as_ref() };
            Self::connect(record.left, record.right, record.before);
            change = record.previous;
        }
        let revision = unsafe { self.current.as_mut() };
        let metadata = revision.exchange(current_metadata, &mut self.generation, &mut self.len);
        revision.redo_next = self.redo;
        self.redo = Some(self.current);
        self.current = previous;
        self.cursor.set(Cursor { node: self.sentinel, start: 0 });
        Some(metadata)
    }

    pub fn redo(&mut self, current_metadata: T) -> Option<T> {
        let mut current = self.redo?;
        self.finish_tail();
        let mut change = unsafe { current.as_ref().first };
        while let Some(ptr) = change {
            let record = unsafe { ptr.as_ref() };
            Self::connect(record.left, record.right, record.after);
            change = record.next;
        }
        let revision = unsafe { current.as_mut() };
        let metadata = revision.exchange(current_metadata, &mut self.generation, &mut self.len);
        self.redo = revision.redo_next.take();
        self.current = current;
        self.cursor.set(Cursor { node: self.sentinel, start: 0 });
        Some(metadata)
    }

    #[inline]
    fn locate(&self, offset: usize) -> Cursor {
        debug_assert!(offset < self.len);
        let mut cursor = self.cursor.get();
        if cursor.node == self.sentinel {
            cursor = Cursor { node: unsafe { self.sentinel.as_ref().next }, start: 0 };
        }
        let mut node = cursor.node;
        while offset < cursor.start {
            node = unsafe { node.as_ref().prev };
            cursor.start -= unsafe { node.as_ref().span.len };
        }
        while offset >= cursor.start + unsafe { node.as_ref().span.len } {
            cursor.start += unsafe { node.as_ref().span.len };
            node = unsafe { node.as_ref().next };
        }
        cursor.node = node;
        self.cursor.set(cursor);
        cursor
    }

    fn alloc_node(&self, span: Span) -> NonNull<Node> {
        NonNull::from(self.arena.alloc_uninit().write(Node {
            prev: self.sentinel,
            next: self.sentinel,
            span,
        }))
    }

    fn record(&mut self, left: NodePtr, right: NodePtr, before: Run) -> ChangePtr {
        self.revision().previous?;
        let revision = unsafe { self.current.as_mut() };
        let record = NonNull::from(self.arena.alloc_uninit().write(Change {
            previous: revision.last,
            next: None,
            left,
            right,
            before,
            after: Run { first: right, last: left },
        }));
        if let Some(mut previous) = revision.last {
            unsafe { previous.as_mut().next = Some(record) };
        } else {
            revision.first = Some(record);
        }
        revision.last = Some(record);
        Some(record)
    }

    fn can_replace_tail(&self, range: &Range<usize>, replacement: &[u8]) -> bool {
        if self.tail == self.sentinel || range.start < self.tail_start {
            return false;
        }
        let end = self.tail_start + unsafe { self.tail.as_ref().span.len };
        if replacement.is_empty() {
            range.end <= end
                && ((range.start == self.tail_start && range.end < end)
                    || (range.start > self.tail_start && range.end == end))
        } else {
            range.start == end && range.end == end
        }
    }

    fn replace_tail(&mut self, range: Range<usize>, replacement: &[u8]) {
        let span = unsafe { &mut self.tail.as_mut().span };
        if !replacement.is_empty() {
            debug_assert_eq!(range, span.len..span.len);
            let len = span.len + replacement.len();
            if len > self.tail_capacity {
                let capacity = self.tail_capacity
                    + (len - self.tail_capacity).next_multiple_of(TAIL_GRANULARITY);
                span.text = if self.tail_capacity == 0 {
                    NonNull::new(self.arena.alloc_uninit_slice::<u8>(capacity).as_mut_ptr().cast())
                        .unwrap()
                } else {
                    let text = unsafe {
                        self.arena.realloc(span.text, self.tail_capacity, capacity, 1).cast()
                    };
                    debug_assert_eq!(text, span.text);
                    text
                };
                self.tail_capacity = capacity;
            }
            unsafe {
                replacement
                    .as_ptr()
                    .copy_to_nonoverlapping(span.text.add(span.len).as_ptr(), replacement.len());
            }
            span.len = len;
        } else {
            debug_assert!(range.start == 0 || range.end == span.len);
            if range.start == 0 {
                span.text = unsafe { span.text.add(range.end) };
                // Keep text + capacity at the arena end after advancing the view.
                self.tail_capacity -= range.end;
            }
            span.len -= range.end - range.start;
        }
    }

    fn prepare_splice(&mut self, range: Range<usize>, insert: bool) -> Cursor {
        let beg = range.start;
        let end = range.end;
        self.finish_tail();
        let mut before = Run { first: self.sentinel, last: self.sentinel };
        let mut left = self.sentinel;
        let mut right = self.sentinel;
        let mut prefix = Span { text: NonNull::dangling(), len: 0 };
        let mut suffix = prefix;
        if self.len > 0 {
            let offset = if beg == end { beg.saturating_sub(1) } else { beg };
            let cursor = self.locate(offset.min(self.len - 1));
            let first = cursor.node;
            let node = unsafe { first.as_ref() };
            prefix = node.span.slice(0..beg - cursor.start);
            left = node.prev;
            let mut last = first;
            let mut remaining = end - cursor.start;
            while remaining > unsafe { last.as_ref().span.len } {
                remaining -= unsafe { last.as_ref().span.len };
                last = unsafe { last.as_ref().next };
            }
            let node = unsafe { last.as_ref() };
            suffix = node.span.slice(remaining..node.span.len);
            right = node.next;
            before = Run { first, last };
        }

        // Allocate the log and nodes before text, preserving arena-tail growth.
        let record = self.record(left, right, before);
        let mut last = left;
        if prefix.len > 0 {
            let node = self.alloc_node(prefix);
            Self::link(last, node);
            last = node;
        }
        if insert {
            // Filled by replace_tail before any read or subsequent edit can run.
            let node = self.alloc_node(Span { text: NonNull::dangling(), len: 0 });
            Self::link(last, node);
            last = node;
            self.tail = node;
            self.tail_start = beg;
            self.tail_capacity = 0;
        }
        if suffix.len > 0 {
            let node = self.alloc_node(suffix);
            Self::link(last, node);
            last = node;
        }
        Self::link(last, right);
        let after = Run { first: unsafe { left.as_ref().next }, last };
        if let Some(mut record) = record {
            unsafe { record.as_mut().after = after };
        }
        Cursor { node: after.first, start: beg - prefix.len }
    }

    fn replace_impl(&mut self, range: Range<usize>, replacement: &[u8], coalesce: bool) {
        let beg = range.start.min(self.len);
        let end = range.end.min(self.len).max(beg);
        if beg == end && replacement.is_empty() {
            return;
        }
        self.redo = None;
        let (cursor, tail_range) = if coalesce && self.can_replace_tail(&(beg..end), replacement) {
            (
                Cursor { node: self.tail, start: self.tail_start },
                beg - self.tail_start..end - self.tail_start,
            )
        } else {
            (self.prepare_splice(beg..end, !replacement.is_empty()), 0..0)
        };
        if self.tail != self.sentinel {
            self.replace_tail(tail_range, replacement);
        }
        self.len = self.len - (end - beg) + replacement.len();
        self.generation = self.generation.wrapping_add(1);
        self.cursor.set(cursor);
    }

    pub fn replace_coalescing(&mut self, range: Range<usize>, replacement: &[u8]) {
        debug_assert!(self.revision().previous.is_some());
        self.replace_impl(range, replacement, true);
    }

    pub fn clear(&mut self) {
        if self.len == 0 {
            self.generation = self.generation.wrapping_add(1);
        } else {
            self.replace_impl(0..self.len, &[], false);
        }
    }

    pub fn extract_raw(&self, range: Range<usize>, out: &mut Vec<u8>, mut out_off: usize) {
        let end = range.end.min(self.len);
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
        let mut offset = 0;
        let mut destination = 0;
        while {
            let chunk = self.read_forward(offset);
            dst.replace(destination..usize::MAX, chunk);
            destination = usize::MAX;
            offset += chunk.len();
            offset < self.len
        } {}
    }
}

impl<T: Copy> ReadableDocument for PieceList<T> {
    #[inline]
    fn read_forward(&self, off: usize) -> &[u8] {
        if off >= self.len {
            return &[];
        }
        let cursor = self.locate(off);
        let span = unsafe { cursor.node.as_ref().span };
        let offset = off - cursor.start;
        unsafe { slice::from_raw_parts(span.text.add(offset).as_ptr(), span.len - offset) }
    }

    #[inline]
    fn read_backward(&self, off: usize) -> &[u8] {
        let off = off.min(self.len);
        if off == 0 {
            return &[];
        }
        let cursor = self.locate(off - 1);
        let span = unsafe { cursor.node.as_ref().span };
        unsafe { slice::from_raw_parts(span.text.as_ptr(), off - cursor.start) }
    }
}

impl<T: Copy> WriteableDocument for PieceList<T> {
    fn replace(&mut self, range: Range<usize>, replacement: &[u8]) {
        self.replace_impl(range, replacement, false);
    }
}
