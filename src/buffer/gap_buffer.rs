// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::alloc::{Allocator, Layout};
use std::ops::Range;
use std::ptr::NonNull;

use super::virtual_mem::{VALLOCATOR, VirtualAllocator};
use crate::document::{ReadableDocument, WriteableDocument};
use crate::helpers::*;
use crate::{apperr, sys};

#[cfg(target_pointer_width = "32")]
const LARGE_CAPACITY: usize = 128 * MEBI;
#[cfg(target_pointer_width = "64")]
const LARGE_CAPACITY: usize = 4 * GIBI;
const LARGE_ALLOC_CHUNK: usize = 64 * KIBI;
const LARGE_GAP_CHUNK: usize = 4 * KIBI;

const SMALL_CAPACITY: usize = 128 * KIBI;
const SMALL_ALLOC_CHUNK: usize = 256;
const SMALL_GAP_CHUNK: usize = 16;

// TODO: Instead of having a specialization for small buffers here,
// tui.rs could also just keep a MRU set of large buffers around.
enum BackingBuffer {
    VirtualMemory(Box<[u8], VirtualAllocator>),
    Vec(Vec<u8>),
}

/// Most people know how Vec<T> works: It has some spare capacity at the end,
/// so that pushing into it doesn't reallocate every single time. A gap buffer
/// is the same thing, but the spare capacity can be anywhere in the buffer.
/// This variant is optimized for large buffers and uses virtual memory.
pub struct GapBuffer {
    /// Maximum size of the buffer, including gap.
    reserve: usize,
    /// Size of the buffer, including gap.
    commit: usize,
    /// Length of the stored text, NOT including gap.
    text_length: usize,
    /// Gap offset.
    gap_off: usize,
    /// Gap length.
    gap_len: usize,
    /// Increments every time the buffer is modified.
    generation: u32,
    /// If `Vec(..)`, the buffer is optimized for small amounts of text
    /// and uses the standard heap. Otherwise, it uses virtual memory.
    buffer: BackingBuffer,
}

impl GapBuffer {
    pub fn new(small: bool) -> apperr::Result<Self> {
        let reserve;
        let buffer;

        if small {
            reserve = SMALL_CAPACITY;
            buffer = BackingBuffer::Vec(Vec::new());
        } else {
            reserve = LARGE_CAPACITY;
            let ptr = unsafe {
                VALLOCATOR
                    .allocate(Layout::from_size_align_unchecked(LARGE_CAPACITY, align_of::<u8>()))
                    .unwrap()
            };
            let buf = unsafe { Box::from_non_null_in(ptr, VALLOCATOR) };
            buffer = BackingBuffer::VirtualMemory(buf);
        }

        Ok(Self {
            reserve,
            commit: 0,
            text_length: 0,
            gap_off: 0,
            gap_len: 0,
            generation: 0,
            buffer,
        })
    }

    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        self.text_length
    }

    pub fn generation(&self) -> u32 {
        self.generation
    }

    pub fn set_generation(&mut self, generation: u32) {
        self.generation = generation;
    }

    /// WARNING: The returned slice must not necessarily be the same length as `len` (due to OOM).
    pub fn allocate_gap(&mut self, off: usize, len: usize, delete: usize) -> &mut [u8] {
        // Sanitize parameters
        let off = off.min(self.text_length);
        let delete = delete.min(self.text_length - off);

        // Move the existing gap if it exists
        if off != self.gap_off {
            self.move_gap(off);
        }

        // Delete the text
        if delete > 0 {
            self.delete_text(delete);
        }

        // Enlarge the gap if needed
        if len > self.gap_len {
            self.enlarge_gap(len);
        }

        self.generation = self.generation.wrapping_add(1);

        match &mut self.buffer {
            BackingBuffer::VirtualMemory(buf) => &mut buf[off..off + self.gap_len],
            BackingBuffer::Vec(buf) => &mut buf[self.gap_off..self.gap_off + self.gap_len],
        }
    }

    fn move_gap(&mut self, off: usize) {
        if self.gap_len > 0 {
            //
            //                       v gap_off
            // left:  |ABCDEFGHIJKLMN   OPQRSTUVWXYZ|
            //        |ABCDEFGHI   JKLMNOPQRSTUVWXYZ|
            //                  ^ off
            //        move: JKLMN
            //
            //                       v gap_off
            // !left: |ABCDEFGHIJKLMN   OPQRSTUVWXYZ|
            //        |ABCDEFGHIJKLMNOPQRS   TUVWXYZ|
            //                            ^ off
            //        move: OPQRS
            //
            let left = off < self.gap_off;
            let move_src = if left { off } else { self.gap_off + self.gap_len };
            let move_dst = if left { off + self.gap_len } else { self.gap_off };
            let move_len = if left { self.gap_off - off } else { off - self.gap_off };

            match &mut self.buffer {
                BackingBuffer::VirtualMemory(buf) => {
                    buf.copy_within(move_src..move_src + move_len, move_dst)
                }
                BackingBuffer::Vec(vec) => vec.copy_within(move_src..move_src + move_len, move_dst),
            }

            if cfg!(debug_assertions) {
                // Fill the moved-out bytes with 0xCD to make debugging easier.
                let start = self.gap_off;
                let end = start + self.gap_len;
                match &mut self.buffer {
                    BackingBuffer::VirtualMemory(buf) => {
                        for i in start..end {
                            buf[i] = 0;
                        }
                    }
                    BackingBuffer::Vec(buf) => {
                        for i in start..end {
                            buf[i] = 0;
                        }
                    }
                }
            }
        }

        self.gap_off = off;
    }

    fn delete_text(&mut self, delete: usize) {
        if cfg!(debug_assertions) {
            // Fill the deleted bytes with 0xCD to make debugging easier.
            match &mut self.buffer {
                BackingBuffer::VirtualMemory(buf) => {
                    for i in self.gap_off + self.gap_len..delete {
                        buf[i] = 0;
                    }
                }
                BackingBuffer::Vec(buf) => {
                    for i in self.gap_off + self.gap_len..delete {
                        buf[i] = 0;
                    }
                }
            }
        }

        self.gap_len += delete;
        self.text_length -= delete;
    }

    fn enlarge_gap(&mut self, len: usize) {
        let gap_chunk;
        let alloc_chunk;

        if matches!(self.buffer, BackingBuffer::VirtualMemory(..)) {
            gap_chunk = LARGE_GAP_CHUNK;
            alloc_chunk = LARGE_ALLOC_CHUNK;
        } else {
            gap_chunk = SMALL_GAP_CHUNK;
            alloc_chunk = SMALL_ALLOC_CHUNK;
        }

        let gap_len_old = self.gap_len;
        let gap_len_new = (len + gap_chunk + gap_chunk - 1) & !(gap_chunk - 1);

        let bytes_old = self.commit;
        let bytes_new = self.text_length + gap_len_new;

        if bytes_new > bytes_old {
            let bytes_new = (bytes_new + alloc_chunk - 1) & !(alloc_chunk - 1);

            if bytes_new > self.reserve {
                return;
            }

            match &mut self.buffer {
                BackingBuffer::VirtualMemory(buf) => unsafe {
                    // This function appears to expand the range of the initiallized memory past the boundries of the new gap
                    // iow I guess it's notifying the program that the bytes after the gap are initialized because the were not previously
                    if sys::virtual_commit(
                        NonNull::new_unchecked(buf.as_mut_ptr()).add(bytes_old),
                        bytes_new - bytes_old,
                    )
                    .is_err()
                    {
                        return;
                    }
                },
                BackingBuffer::Vec(buf) => {
                    buf.resize(bytes_new, 0);
                }
            }

            self.commit = bytes_new;
        }

        // When the gap expands, it will consume the contents bordering it.
        // Those contents need to be moved out of the newly enlarged gap
        let gap_end_old = self.gap_off + gap_len_old;
        let gap_len_new = self.gap_off + gap_len_new;
        let buf_contents_len = self.text_length - self.gap_off;
        match &mut self.buffer {
            BackingBuffer::VirtualMemory(buf) => {
                buf.copy_within(gap_end_old..gap_end_old + buf_contents_len, gap_len_new);
            }
            BackingBuffer::Vec(buf) => {
                buf.copy_within(gap_end_old..gap_end_old + buf_contents_len, gap_len_new);
            }
        };

        if cfg!(debug_assertions) {
            let start = self.gap_off + gap_len_old;
            let end = start + gap_len_new - gap_len_old;
            // Fill the moved-out bytes with 0xCD to make debugging easier.
            match &mut self.buffer {
                BackingBuffer::VirtualMemory(buf) => {
                    for i in start..end {
                        buf[i] = 0;
                    }
                }
                BackingBuffer::Vec(buf) => {
                    for i in start..end {
                        buf[i] = 0;
                    }
                }
            }
        }

        self.gap_len = gap_len_new;
    }

    pub fn commit_gap(&mut self, len: usize) {
        assert!(len <= self.gap_len);
        self.text_length += len;
        self.gap_off += len;
        self.gap_len -= len;
    }

    pub fn replace(&mut self, range: Range<usize>, src: &[u8]) {
        let gap = self.allocate_gap(range.start, src.len(), range.end.saturating_sub(range.start));
        let len = slice_copy_safe(gap, src);
        self.commit_gap(len);
    }

    pub fn clear(&mut self) {
        self.gap_off = 0;
        self.gap_len += self.text_length;
        self.generation = self.generation.wrapping_add(1);
        self.text_length = 0;
    }

    pub fn extract_raw(
        &self,
        mut beg: usize,
        mut end: usize,
        out: &mut Vec<u8>,
        mut out_off: usize,
    ) {
        debug_assert!(beg <= end && end <= self.text_length);

        end = end.min(self.text_length);
        beg = beg.min(end);
        out_off = out_off.min(out.len());

        if beg >= end {
            return;
        }

        out.reserve(end - beg);

        while beg < end {
            let chunk = self.read_forward(beg);
            let chunk = &chunk[..chunk.len().min(end - beg)];
            out.replace_range(out_off..out_off, chunk);
            beg += chunk.len();
            out_off += chunk.len();
        }
    }

    /// Replaces the entire buffer contents with the given `text`.
    /// The method is optimized for the case where the given `text` already matches
    /// the existing contents. Returns `true` if the buffer contents were changed.
    pub fn copy_from(&mut self, src: &dyn ReadableDocument) -> bool {
        let mut off = 0;

        // Find the position at which the contents change.
        loop {
            let dst_chunk = self.read_forward(off);
            let src_chunk = src.read_forward(off);

            let dst_len = dst_chunk.len();
            let src_len = src_chunk.len();
            let len = dst_len.min(src_len);
            let mismatch = dst_chunk[..len] != src_chunk[..len];

            if mismatch {
                break; // The contents differ.
            }
            if len == 0 {
                if dst_len == src_len {
                    return false; // Both done simultaneously. -> Done.
                }
                break; // One of the two is shorter.
            }

            off += len;
        }

        // Update the buffer starting at `off`.
        loop {
            let chunk = src.read_forward(off);
            self.replace(off..usize::MAX, chunk);
            off += chunk.len();

            // No more data to copy -> Done. By checking this _after_ the replace()
            // call, we ensure that the initial `off..usize::MAX` range is deleted.
            // This fixes going from some buffer contents to being empty.
            if chunk.is_empty() {
                return true;
            }
        }
    }

    /// Copies the contents of the buffer into a string.
    pub fn copy_into(&self, dst: &mut dyn WriteableDocument) {
        let mut beg = 0;
        let mut off = 0;

        while {
            let chunk = self.read_forward(off);

            // The first write will be 0..usize::MAX and effectively clear() the destination.
            // Every subsequent write will be usize::MAX..usize::MAX and thus effectively append().
            dst.replace(beg..usize::MAX, chunk);
            beg = usize::MAX;

            off += chunk.len();
            off < self.text_length
        } {}
    }
}

impl ReadableDocument for GapBuffer {
    fn read_forward(&self, off: usize) -> &[u8] {
        let off = off.min(self.text_length);
        let beg;
        let len;

        if off < self.gap_off {
            // Cursor is before the gap: We can read until the start of the gap.
            beg = off;
            len = self.gap_off - off;
        } else {
            // Cursor is after the gap: We can read until the end of the buffer.
            beg = off + self.gap_len;
            len = self.text_length - off;
        }

        match &self.buffer {
            BackingBuffer::VirtualMemory(buf) => &buf[beg..beg + len],
            BackingBuffer::Vec(buf) => &buf[beg..beg + len],
        }
    }

    fn read_backward(&self, off: usize) -> &[u8] {
        let off = off.min(self.text_length);
        let beg;
        let len;

        if off <= self.gap_off {
            // Cursor is before the gap: We can read until the beginning of the buffer.
            beg = 0;
            len = off;
        } else {
            // Cursor is after the gap: We can read until the end of the gap.
            beg = self.gap_off + self.gap_len;
            // The cursor_off doesn't account of the gap_len.
            // (This allows us to move the gap without recalculating the cursor position.)
            len = off - self.gap_off;
        }

        match &self.buffer {
            BackingBuffer::VirtualMemory(buf) => &buf[beg..beg + len],
            BackingBuffer::Vec(buf) => &buf[beg..beg + len],
        }
    }
}
