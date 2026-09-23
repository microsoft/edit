// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::cell::UnsafeCell;
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
const MAX_TREE_HEIGHT: usize = 64;
const LOCAL_PIECE_LIMIT: usize = 256;

type NodePtr = Option<NonNull<Node>>;
type RevisionPtr<T> = NonNull<Revision<T>>;

#[derive(Clone, Copy, PartialEq, Eq)]
enum Direction {
    Left,
    Right,
}

impl Direction {
    fn opposite(self) -> Self {
        match self {
            Self::Left => Self::Right,
            Self::Right => Self::Left,
        }
    }
}

#[derive(Clone, Copy)]
struct Piece {
    text: NonNull<u8>,
    len: usize,
}

impl Piece {
    fn slice(self, range: Range<usize>) -> Self {
        debug_assert!(range.start <= range.end && range.end <= self.len);
        Self { text: unsafe { self.text.add(range.start) }, len: range.end - range.start }
    }
}

struct EditBuffer {
    piece: Piece,
    capacity: usize,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Color {
    Red,
    Black,
}

#[derive(Clone, Copy)]
struct Node {
    children: [NodePtr; 2],
    piece: Piece,
    left_len: usize,
    len: usize,
    color: Color,
    black_height: u8,
}

impl Node {
    fn new(children: [NodePtr; 2], piece: Piece) -> Self {
        Self { children, piece, left_len: 0, len: 0, color: Color::Red, black_height: 0 }
    }

    #[inline(always)]
    fn get(node: NonNull<Self>) -> Self {
        unsafe { *node.as_ref() }
    }

    fn len(node: NodePtr) -> usize {
        node.map_or(0, |node| unsafe { node.as_ref().len })
    }

    fn height(node: NodePtr) -> u8 {
        node.map_or(0, |node| unsafe { node.as_ref().black_height })
    }

    fn red(node: NodePtr) -> bool {
        node.is_some_and(|node| unsafe { node.as_ref().color == Color::Red })
    }

    #[inline(always)]
    fn refresh(&mut self) {
        debug_assert!(self.piece.len > 0);
        self.left_len = Self::len(self.children[0]);
        self.len = self.left_len + self.piece.len + Self::len(self.children[1]);
        debug_assert_eq!(Self::height(self.children[0]), Self::height(self.children[1]));
        self.black_height = Self::height(self.children[0]) + u8::from(self.color == Color::Black);
    }
}

/// Owns the write permission for one revision, not the nodes themselves.
struct TreeEdit<'a> {
    arena: &'a Arena,
    watermark: usize,
}

impl TreeEdit<'_> {
    fn pending(&self, node: NonNull<Node>) -> bool {
        node.as_ptr().addr() > self.watermark
    }

    #[inline(always)]
    fn store(&self, reuse: NodePtr, mut node: Node) -> NonNull<Node> {
        node.refresh();
        if let Some(mut reuse) = reuse.filter(|&node| self.pending(node)) {
            debug_assert!(!node.children.contains(&Some(reuse)));
            unsafe { *reuse.as_mut() = node };
            reuse
        } else {
            NonNull::from(self.arena.alloc_uninit().write(node))
        }
    }

    fn blacken(&self, root: NodePtr) -> NodePtr {
        root.map(|root| {
            let mut node = Node::get(root);
            if node.color == Color::Black {
                root
            } else {
                node.color = Color::Black;
                self.store(Some(root), node)
            }
        })
    }

    // The unbalanced root stays on the stack. Each rotated node is stored only
    // in its final position, reusing its own allocation when it is pending.
    fn balance(&self, reuse: NodePtr, mut node: Node) -> NonNull<Node> {
        if node.color == Color::Black {
            for side in [Direction::Left, Direction::Right] {
                let near = side as usize;
                let far = side.opposite() as usize;
                if !Node::red(node.children[near]) {
                    continue;
                }
                let child_ptr = node.children[near].unwrap();
                let mut child = Node::get(child_ptr);
                if Node::red(child.children[near]) {
                    node.children[near] = child.children[far];
                    child.children[far] = Some(self.store(reuse, node));
                    child.children[near] = self.blacken(child.children[near]);
                    return self.store(Some(child_ptr), child);
                }
                if Node::red(child.children[far]) {
                    let middle_ptr = child.children[far].unwrap();
                    let mut middle = Node::get(middle_ptr);
                    child.color = Color::Black;
                    child.children[far] = middle.children[near];
                    node.children[near] = middle.children[far];
                    middle.children[near] = Some(self.store(Some(child_ptr), child));
                    middle.children[far] = Some(self.store(reuse, node));
                    return self.store(Some(middle_ptr), middle);
                }
            }
        }
        self.store(reuse, node)
    }

    fn join(&self, left: NodePtr, piece: Piece, right: NodePtr, reuse: NodePtr) -> NodePtr {
        if Node::height(left) == Node::height(right) {
            let mut node = Node::new([left, right], piece);
            node.color = Color::Black;
            return Some(self.store(reuse, node));
        }
        // Only the shorter root must be black for insertion-style repair along
        // the taller tree's spine. Equal-height trees need no recoloring.
        let (left, right) = if Node::height(left) > Node::height(right) {
            (left, self.blacken(right))
        } else {
            (self.blacken(left), right)
        };
        self.blacken(Some(self.join_inner(left, piece, right, reuse)))
    }

    fn join_inner(
        &self,
        left: NodePtr,
        piece: Piece,
        right: NodePtr,
        reuse: NodePtr,
    ) -> NonNull<Node> {
        let left_height = Node::height(left);
        let right_height = Node::height(right);
        if left_height == right_height {
            return self.store(reuse, Node::new([left, right], piece));
        }
        let side = if left_height > right_height { Direction::Left } else { Direction::Right };
        let root = if side == Direction::Left { left } else { right }.unwrap();
        let mut node = Node::get(root);
        let inner = side.opposite() as usize;
        node.children[inner] = Some(if side == Direction::Left {
            self.join_inner(node.children[inner], piece, right, reuse)
        } else {
            self.join_inner(left, piece, node.children[inner], reuse)
        });
        self.balance(Some(root), node)
    }

    fn insert(&self, root: NodePtr, offset: usize, piece: Piece) -> NonNull<Node> {
        let Some(root) = root else {
            return self.store(None, Node::new([None; 2], piece));
        };
        let mut node = Node::get(root);
        let end = node.left_len + node.piece.len;
        let (side, offset) = if offset <= node.left_len {
            (0, offset)
        } else {
            debug_assert!(offset >= end);
            (1, offset - end)
        };
        node.children[side] = Some(self.insert(node.children[side], offset, piece));
        self.balance(Some(root), node)
    }

    fn prefix(&self, root: NodePtr, end: usize) -> NodePtr {
        let root = root?;
        let node = Node::get(root);
        if end == 0 {
            None
        } else if end >= node.len {
            Some(root)
        } else if end <= node.left_len {
            self.prefix(node.children[0], end)
        } else if end < node.left_len + node.piece.len {
            self.join(node.children[0], node.piece.slice(0..end - node.left_len), None, Some(root))
        } else {
            let right = self.prefix(node.children[1], end - node.left_len - node.piece.len);
            self.join(node.children[0], node.piece, right, Some(root))
        }
    }

    fn suffix(&self, root: NodePtr, beg: usize) -> NodePtr {
        let root = root?;
        let node = Node::get(root);
        let end = node.left_len + node.piece.len;
        if beg == 0 {
            Some(root)
        } else if beg >= node.len {
            None
        } else if beg >= end {
            self.suffix(node.children[1], beg - end)
        } else if beg > node.left_len {
            self.join(
                None,
                node.piece.slice(beg - node.left_len..node.piece.len),
                node.children[1],
                Some(root),
            )
        } else {
            let left = self.suffix(node.children[0], beg);
            self.join(left, node.piece, node.children[1], Some(root))
        }
    }

    // Cut both boundaries together; never construct the discarded middle tree.
    fn cut(&self, root: NodePtr, range: Range<usize>) -> [NodePtr; 2] {
        let Some(root) = root else {
            return [None; 2];
        };
        let node = Node::get(root);
        let end = node.left_len + node.piece.len;
        if range.end <= node.left_len {
            let [left, middle] = self.cut(node.children[0], range);
            return [left, self.join(middle, node.piece, node.children[1], Some(root))];
        }
        if range.start >= end {
            let [middle, right] = self.cut(node.children[1], range.start - end..range.end - end);
            return [self.join(node.children[0], node.piece, middle, Some(root)), right];
        }
        let mut left = self.prefix(node.children[0], range.start);
        let mut right = self.suffix(node.children[1], range.end.saturating_sub(end));
        let mut reuse = Some(root);
        if range.start > node.left_len {
            left = self.join(
                left,
                node.piece.slice(0..range.start - node.left_len),
                None,
                reuse.take(),
            );
        }
        if range.end < end {
            right = self.join(
                None,
                node.piece.slice(range.end - node.left_len..node.piece.len),
                right,
                reuse,
            );
        }
        [left, right]
    }

    fn pop_last(&self, root: NonNull<Node>) -> (NodePtr, Piece, NonNull<Node>) {
        let node = Node::get(root);
        if let Some(right) = node.children[1] {
            let (right, piece, removed) = self.pop_last(right);
            (self.join(node.children[0], node.piece, right, Some(root)), piece, removed)
        } else {
            (node.children[0], node.piece, root)
        }
    }

    fn concat(&self, left: NodePtr, right: NodePtr) -> NodePtr {
        let (Some(left), Some(right)) = (left, right) else {
            return self.blacken(left.or(right));
        };
        let (left, piece, reuse) = self.pop_last(left);
        self.join(left, piece, Some(right), Some(reuse))
    }
}

struct Revision<T> {
    root: NodePtr,
    metadata: T,
    generation_before: u32,
    previous: Option<RevisionPtr<T>>,
    redo_next: Option<RevisionPtr<T>>,
}

impl<T> Revision<T> {
    fn alloc(
        arena: &Arena,
        root: NodePtr,
        metadata: T,
        generation: u32,
        previous: Option<RevisionPtr<T>>,
    ) -> RevisionPtr<T> {
        NonNull::from(arena.alloc_uninit().write(Self {
            root,
            metadata,
            generation_before: generation,
            previous,
            redo_next: None,
        }))
    }

    fn exchange(&mut self, metadata: T, generation: &mut u32) -> T {
        std::mem::swap(generation, &mut self.generation_before);
        std::mem::replace(&mut self.metadata, metadata)
    }
}

#[derive(Clone, Copy)]
struct PathEntry {
    node: NonNull<Node>,
    side: Direction,
}

struct Cursor {
    root: NodePtr,
    generation: u32,
    node: NonNull<Node>,
    start: usize,
    end: usize,
    path: [MaybeUninit<PathEntry>; MAX_TREE_HEIGHT],
    depth: usize,
}

impl Cursor {
    fn new() -> Self {
        Self {
            root: None,
            generation: 0,
            node: NonNull::dangling(),
            start: 0,
            end: 0,
            path: [MaybeUninit::uninit(); MAX_TREE_HEIGHT],
            depth: 0,
        }
    }

    fn push(&mut self, node: NonNull<Node>, side: Direction) {
        self.path[self.depth].write(PathEntry { node, side });
        self.depth += 1;
    }

    fn entries_mut(&mut self) -> &mut [PathEntry] {
        unsafe { self.path[..self.depth].assume_init_mut() }
    }

    #[inline(always)]
    fn locate(&mut self, root: NonNull<Node>, generation: u32, offset: usize) {
        if self.root == Some(root) && self.generation == generation {
            if (self.start..self.end).contains(&offset) {
                return;
            }
            if offset == self.end || offset + 1 == self.start {
                self.step(if offset == self.end { Direction::Right } else { Direction::Left });
                return;
            }
        }
        self.seek(root, generation, offset);
    }

    fn seek(&mut self, root: NonNull<Node>, generation: u32, offset: usize) {
        self.root = Some(root);
        self.generation = generation;
        self.depth = 0;
        let mut node = root;
        let mut start = 0;
        loop {
            let current = Node::get(node);
            let piece_start = start + current.left_len;
            let piece_end = piece_start + current.piece.len;
            if offset < piece_start {
                self.push(node, Direction::Left);
                node = current.children[0].unwrap();
            } else if offset >= piece_end {
                self.push(node, Direction::Right);
                start = piece_end;
                node = current.children[1].unwrap();
            } else {
                self.node = node;
                self.start = piece_start;
                self.end = piece_end;
                return;
            }
        }
    }

    fn step(&mut self, direction: Direction) {
        let current = Node::get(self.node);
        if let Some(mut node) = current.children[direction as usize] {
            self.push(self.node, direction);
            while let Some(next) = Node::get(node).children[direction.opposite() as usize] {
                self.push(node, direction.opposite());
                node = next;
            }
            self.node = node;
        } else {
            loop {
                self.depth -= 1;
                let entry = unsafe { self.path[self.depth].assume_init() };
                if entry.side != direction {
                    self.node = entry.node;
                    break;
                }
            }
        }
        let len = Node::get(self.node).piece.len;
        match direction {
            Direction::Left => {
                self.end = self.start;
                self.start -= len;
            }
            Direction::Right => {
                self.start = self.end;
                self.end += len;
            }
        }
    }

    fn make_pending(&mut self, edit: &TreeEdit<'_>) {
        // A pending descendant implies pending ancestors: frozen parents cannot
        // acquire new children without being copied first.
        if edit.pending(self.node) {
            return;
        }
        self.node = edit.store(None, Node::get(self.node));
        let mut root = self.node;
        for entry in self.entries_mut().iter_mut().rev() {
            let mut node = Node::get(entry.node);
            node.children[entry.side as usize] = Some(root);
            let pending = edit.pending(entry.node);
            root = edit.store(Some(entry.node), node);
            entry.node = root;
            if pending {
                return;
            }
        }
        self.root = Some(root);
    }

    fn replace_piece(&mut self, piece: Piece) {
        let delta = piece.len as isize - Node::get(self.node).piece.len as isize;
        unsafe {
            let node = self.node.as_mut();
            node.piece = piece;
            node.len = node.len.wrapping_add_signed(delta);
            for entry in self.entries_mut() {
                let node = entry.node.as_mut();
                node.len = node.len.wrapping_add_signed(delta);
                if entry.side == Direction::Left {
                    node.left_len = node.left_len.wrapping_add_signed(delta);
                }
            }
        }
        self.end = self.start + piece.len;
    }
}

/// A persistent red-black piece tree with revision-local node and text reuse.
///
/// Structural edits cut away the replaced range and join the surviving trees.
/// Balancing uses stack-local node values; only resulting nodes enter the arena.
/// See `piece_tree.md` for the ownership and balancing invariants.
pub struct PieceTree<T: Copy> {
    arena: Arena,
    current: RevisionPtr<T>,
    redo: Option<RevisionPtr<T>>,
    generation: u32,
    traversal: NonNull<UnsafeCell<Cursor>>,
    edit_buffer: Option<EditBuffer>,
}

impl<T: Copy> PieceTree<T> {
    pub fn new(small: bool, metadata: T) -> io::Result<Self> {
        let arena = Arena::new(if small { SMALL_CAPACITY } else { LARGE_CAPACITY })?;
        let traversal = NonNull::from(arena.alloc_uninit().write(UnsafeCell::new(Cursor::new())));
        Ok(Self {
            current: Revision::alloc(&arena, None, metadata, 0, None),
            arena,
            redo: None,
            generation: 0,
            traversal,
            edit_buffer: None,
        })
    }

    pub fn committed(&self) -> usize {
        const ALLOC_CHUNK_SIZE: usize = 64 * 1024;
        (self.arena.offset() + ALLOC_CHUNK_SIZE - 1) & !(ALLOC_CHUNK_SIZE - 1)
    }

    fn revision(&self) -> &Revision<T> {
        unsafe { self.current.as_ref() }
    }

    fn edit(&self) -> TreeEdit<'_> {
        TreeEdit { arena: &self.arena, watermark: self.current.as_ptr().addr() }
    }

    #[allow(clippy::mut_from_ref)]
    fn cursor(&self) -> &mut Cursor {
        unsafe { &mut *self.traversal.as_ref().get() }
    }

    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        Node::len(self.revision().root)
    }

    pub fn generation(&self) -> u32 {
        self.generation
    }

    fn push_revision(&mut self, metadata: T, previous: Option<RevisionPtr<T>>) {
        self.current =
            Revision::alloc(&self.arena, self.revision().root, metadata, self.generation, previous);
        self.redo = None;
        self.edit_buffer = None;
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
        self.edit_buffer = None;
        let previous = self.revision().previous?;
        let revision = unsafe { self.current.as_mut() };
        let metadata = revision.exchange(current_metadata, &mut self.generation);
        revision.redo_next = self.redo;
        self.redo = Some(self.current);
        self.current = previous;
        Some(metadata)
    }

    pub fn redo(&mut self, current_metadata: T) -> Option<T> {
        self.edit_buffer = None;
        let mut current = self.redo?;
        let revision = unsafe { current.as_mut() };
        let metadata = revision.exchange(current_metadata, &mut self.generation);
        self.redo = revision.redo_next.take();
        self.current = current;
        Some(metadata)
    }

    fn publish(&mut self, root: NodePtr) {
        unsafe { self.current.as_mut().root = root };
        self.generation = self.generation.wrapping_add(1);
    }

    fn replace_local(&mut self, range: Range<usize>, replacement: &[u8], coalesce: bool) -> bool {
        let Some(root) = self.revision().root else {
            return false;
        };
        let cursor = self.cursor();
        let offset = if range.is_empty() { range.start.saturating_sub(1) } else { range.start };
        cursor.locate(root, self.generation, offset);
        let piece = Node::get(cursor.node).piece;
        if range.end > cursor.end {
            return false;
        }
        let beg = range.start - cursor.start;
        let end = range.end - cursor.start;
        let len = piece.len - (end - beg) + replacement.len();
        if len == 0 {
            return false;
        }
        let edit = self.edit();
        let (text, capacity) = if len <= LOCAL_PIECE_LIMIT {
            cursor.make_pending(&edit);
            let reusable = self
                .edit_buffer
                .as_ref()
                .filter(|buffer| buffer.piece.text == piece.text && buffer.piece.len == piece.len);
            let (text, capacity) = if let Some(buffer) = reusable {
                if len <= buffer.capacity {
                    (buffer.piece.text, buffer.capacity)
                } else {
                    let capacity = len.next_power_of_two();
                    let text = unsafe {
                        self.arena.realloc(buffer.piece.text, buffer.capacity, capacity, 1).cast()
                    };
                    (text, capacity)
                }
            } else {
                let capacity = len.next_power_of_two().max(64);
                let text =
                    NonNull::new(self.arena.alloc_uninit_slice::<u8>(capacity).as_mut_ptr().cast())
                        .unwrap();
                unsafe { piece.text.as_ptr().copy_to_nonoverlapping(text.as_ptr(), beg) };
                (text, capacity)
            };
            unsafe {
                piece
                    .text
                    .add(end)
                    .as_ptr()
                    .copy_to(text.add(beg + replacement.len()).as_ptr(), piece.len - end);
                replacement
                    .as_ptr()
                    .copy_to_nonoverlapping(text.add(beg).as_ptr(), replacement.len());
            }
            (text, capacity)
        } else if coalesce && edit.pending(cursor.node) {
            if beg == piece.len && end == beg && !replacement.is_empty() {
                let text =
                    unsafe { self.arena.realloc(piece.text, piece.len, len, 1).cast::<u8>() };
                unsafe {
                    replacement
                        .as_ptr()
                        .copy_to_nonoverlapping(text.add(piece.len).as_ptr(), replacement.len())
                };
                (text, 0)
            } else if replacement.is_empty() && (beg == 0 || end == piece.len) {
                (unsafe { piece.text.add(if beg == 0 { end } else { 0 }) }, 0)
            } else {
                return false;
            }
        } else {
            return false;
        };
        let piece = Piece { text, len };
        cursor.replace_piece(piece);
        let root = cursor.root.unwrap();
        cursor.generation = self.generation.wrapping_add(1);
        self.edit_buffer = (capacity > 0).then_some(EditBuffer { piece, capacity });
        self.publish(Some(root));
        true
    }

    fn replace_impl(&mut self, range: Range<usize>, replacement: &[u8], coalesce: bool) {
        let len = self.len();
        let beg = range.start.min(len);
        let end = range.end.min(len).max(beg);
        if beg == end && replacement.is_empty() {
            return;
        }
        if self.replace_local(beg..end, replacement, coalesce) {
            return;
        }
        self.edit_buffer = None;
        let edit = self.edit();
        let piece = Piece { text: NonNull::dangling(), len: replacement.len() };
        let root = self.revision().root;
        let boundary = root.is_none() || {
            let cursor = self.cursor();
            cursor.start == beg || cursor.end == beg
        };
        let root = if beg == end && boundary {
            edit.blacken(Some(edit.insert(root, beg, piece)))
        } else {
            let [left, right] = edit.cut(root, beg..end);
            if replacement.is_empty() {
                edit.concat(left, right)
            } else {
                edit.join(left, piece, right, None)
            }
        };
        if !replacement.is_empty() {
            let cursor = self.cursor();
            cursor.root = None;
            cursor.locate(root.unwrap(), self.generation.wrapping_add(1), beg);
            let mut inserted = cursor.node;
            debug_assert_eq!(Node::get(inserted).piece.text, NonNull::dangling());
            let text =
                self.arena.alloc_uninit_slice(replacement.len()).write_copy_of_slice(replacement);
            unsafe { inserted.as_mut().piece.text = NonNull::from(&mut text[0]) };
        }
        self.publish(root);
    }

    pub fn replace_coalescing(&mut self, range: Range<usize>, replacement: &[u8]) {
        debug_assert!(self.revision().previous.is_some());
        self.replace_impl(range, replacement, true);
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
        self.edit_buffer = None;
        self.publish(None);
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

    #[inline(always)]
    fn read(&self, off: usize, direction: Direction) -> &[u8] {
        let off = off.min(self.len());
        let target = match direction {
            Direction::Left if off == 0 => return &[],
            Direction::Left => off - 1,
            Direction::Right if off == self.len() => return &[],
            Direction::Right => off,
        };
        let cursor = self.cursor();
        cursor.locate(self.revision().root.unwrap(), self.generation, target);
        let piece = Node::get(cursor.node).piece;
        let relative = off - cursor.start;
        unsafe {
            match direction {
                Direction::Left => slice::from_raw_parts(piece.text.as_ptr(), relative),
                Direction::Right => {
                    slice::from_raw_parts(piece.text.add(relative).as_ptr(), piece.len - relative)
                }
            }
        }
    }
}

impl<T: Copy> ReadableDocument for PieceTree<T> {
    #[inline]
    fn read_forward(&self, off: usize) -> &[u8] {
        self.read(off, Direction::Right)
    }

    #[inline]
    fn read_backward(&self, off: usize) -> &[u8] {
        self.read(off, Direction::Left)
    }
}

impl<T: Copy> WriteableDocument for PieceTree<T> {
    fn replace(&mut self, range: Range<usize>, replacement: &[u8]) {
        self.replace_impl(range, replacement, false);
    }
}
