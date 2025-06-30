// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#![allow(clippy::mut_from_ref)]

use std::alloc::{AllocError, Allocator, Layout};
use std::cell::Cell;
use std::hint::cold_path;
use std::mem::MaybeUninit;
use std::ptr::{self, NonNull};
use std::{mem, slice};

use crate::helpers::*;
use crate::{apperr, sys};

const ALLOC_CHUNK_SIZE: usize = 64 * KIBI;

/// An arena allocator.
///
/// If you have never used an arena allocator before, think of it as
/// allocating objects on the stack, but the stack is *really* big.
/// Each time you allocate, memory gets pushed at the end of the stack,
/// each time you deallocate, memory gets popped from the end of the stack.
///
/// One reason you'd want to use this is obviously performance: It's very simple
/// and so it's also very fast, >10x faster than your system allocator.
///
/// However, modern allocators such as `mimalloc` are just as fast, so why not use them?
/// Because their performance comes at the cost of binary size and we can't have that.
///
/// The biggest benefit though is that it sometimes massively simplifies lifetime
/// and memory management. This can best be seen by this project's UI code, which
/// uses an arena to allocate a tree of UI nodes. This is infamously difficult
/// to do in Rust, but not so when you got an arena allocator:
/// All nodes have the same lifetime, so you can just use references.
///
/// <div class="warning">
///
/// **Do not** push objects into the arena that require destructors.
/// Destructors are not executed. Use a pool allocator for that.
///
/// </div>
pub struct Arena {
    base: NonNull<u8>,
    capacity: usize,
    commit: Cell<usize>,
    offset: Cell<usize>,

    /// See [`super::debug`], which uses this for borrow tracking.
    #[cfg(debug_assertions)]
    pub(super) borrows: Cell<usize>,
}

impl Arena {
    pub const fn empty() -> Self {
        Self {
            base: NonNull::dangling(),
            capacity: 0,
            commit: Cell::new(0),
            offset: Cell::new(0),

            #[cfg(debug_assertions)]
            borrows: Cell::new(0),
        }
    }

    pub fn new(capacity: usize) -> apperr::Result<Self> {
        let capacity = (capacity.max(1) + ALLOC_CHUNK_SIZE - 1) & !(ALLOC_CHUNK_SIZE - 1);
        let base = unsafe { sys::virtual_reserve(capacity)? };

        Ok(Self {
            base,
            capacity,
            commit: Cell::new(0),
            offset: Cell::new(0),

            #[cfg(debug_assertions)]
            borrows: Cell::new(0),
        })
    }

    pub fn is_empty(&self) -> bool {
        self.base == NonNull::dangling()
    }

    pub fn offset(&self) -> usize {
        self.offset.get()
    }

    /// "Deallocates" the memory in the arena down to the given offset.
    ///
    /// # Safety
    ///
    /// Obviously, this is GIGA UNSAFE. It runs no destructors and does not check
    /// whether the offset is valid. You better take care when using this function.
    pub unsafe fn reset(&self, to: usize) {
        // Fill the deallocated memory with 0xDD to aid debugging.
        if cfg!(debug_assertions) && self.offset.get() > to {
            let commit = self.commit.get();
            let len = (self.offset.get() + 128).min(commit) - to;
            unsafe { slice::from_raw_parts_mut(self.base.add(to).as_ptr(), len).fill(0xDD) };
        }

        self.offset.replace(to);
    }

    #[inline]
    pub(super) fn alloc_raw(
        &self,
        bytes: usize,
        alignment: usize,
    ) -> Result<NonNull<[u8]>, AllocError> {
        let commit = self.commit.get();
        let offset = self.offset.get();

        let beg = (offset + alignment - 1) & !(alignment - 1);
        let end = beg + bytes;

        if end > commit {
            return self.alloc_raw_bump(beg, end);
        }

        if cfg!(debug_assertions) {
            let ptr = unsafe { self.base.add(offset) };
            let len = (end + 128).min(self.commit.get()) - offset;
            unsafe { slice::from_raw_parts_mut(ptr.as_ptr(), len).fill(0xCD) };
        }

        self.offset.replace(end);
        Ok(unsafe { NonNull::slice_from_raw_parts(self.base.add(beg), bytes) })
    }

    // With the code in `alloc_raw_bump()` out of the way, `alloc_raw()` compiles down to some super tight assembly.
    #[cold]
    fn alloc_raw_bump(&self, beg: usize, end: usize) -> Result<NonNull<[u8]>, AllocError> {
        let offset = self.offset.get();
        let commit_old = self.commit.get();
        let commit_new = (end + ALLOC_CHUNK_SIZE - 1) & !(ALLOC_CHUNK_SIZE - 1);

        if commit_new > self.capacity
            || unsafe {
                sys::virtual_commit(self.base.add(commit_old), commit_new - commit_old).is_err()
            }
        {
            return Err(AllocError);
        }

        if cfg!(debug_assertions) {
            let ptr = unsafe { self.base.add(offset) };
            let len = (end + 128).min(self.commit.get()) - offset;
            unsafe { slice::from_raw_parts_mut(ptr.as_ptr(), len).fill(0xCD) };
        }

        self.commit.replace(commit_new);
        self.offset.replace(end);
        Ok(unsafe { NonNull::slice_from_raw_parts(self.base.add(beg), end - beg) })
    }

    #[allow(clippy::mut_from_ref)]
    pub fn alloc_uninit<T>(&self) -> &mut MaybeUninit<T> {
        let bytes = mem::size_of::<T>();
        let alignment = mem::align_of::<T>();
        let ptr = self.alloc_raw(bytes, alignment).unwrap();
        unsafe { ptr.cast().as_mut() }
    }

    #[allow(clippy::mut_from_ref)]
    pub fn alloc_uninit_slice<T>(&self, count: usize) -> &mut [MaybeUninit<T>] {
        let bytes = mem::size_of::<T>() * count;
        let alignment = mem::align_of::<T>();
        let ptr = self.alloc_raw(bytes, alignment).unwrap();
        unsafe { slice::from_raw_parts_mut(ptr.cast().as_ptr(), count) }
    }
}

impl Drop for Arena {
    fn drop(&mut self) {
        if !self.is_empty() {
            unsafe { sys::virtual_release(self.base, self.capacity) };
        }
    }
}

impl Default for Arena {
    fn default() -> Self {
        Self::empty()
    }
}

unsafe impl Allocator for Arena {
    fn allocate(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        self.alloc_raw(layout.size(), layout.align())
    }

    fn allocate_zeroed(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        let p = self.alloc_raw(layout.size(), layout.align())?;
        unsafe { p.cast::<u8>().as_ptr().write_bytes(0, p.len()) }
        Ok(p)
    }

    // While it is possible to shrink the tail end of the arena, it is
    // not very useful given the existence of scoped scratch arenas.
    unsafe fn deallocate(&self, _: NonNull<u8>, _: Layout) {}

    unsafe fn grow(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<[u8]>, AllocError> {
        debug_assert!(new_layout.size() >= old_layout.size());
        debug_assert!(new_layout.align() <= old_layout.align());

        let new_ptr;

        // Growing the given area is possible if it is at the end of the arena.
        if unsafe { ptr.add(old_layout.size()) == self.base.add(self.offset.get()) } {
            new_ptr = ptr;
            let delta = new_layout.size() - old_layout.size();
            // Assuming that the given ptr/length area is at the end of the arena,
            // we can just push more memory to the end of the arena to grow it.
            self.alloc_raw(delta, 1)?;
        } else {
            cold_path();

            new_ptr = self.allocate(new_layout)?.cast();

            // SAFETY: It's weird to me that this doesn't assert new_layout.size() >= old_layout.size(),
            // but neither does the stdlib code at the time of writing.
            // So, assuming that is not needed, this code is safe since it just copies the old data over.
            unsafe {
                ptr::copy_nonoverlapping(ptr.as_ptr(), new_ptr.as_ptr(), old_layout.size());
                self.deallocate(ptr, old_layout);
            }
        }

        Ok(NonNull::slice_from_raw_parts(new_ptr, new_layout.size()))
    }

    unsafe fn grow_zeroed(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<[u8]>, AllocError> {
        unsafe {
            // SAFETY: Same as grow().
            let ptr = self.grow(ptr, old_layout, new_layout)?;

            // SAFETY: At this point, `ptr` must be valid for `new_layout.size()` bytes,
            // allowing us to safely zero out the delta since `old_layout.size()`.
            ptr.cast::<u8>()
                .add(old_layout.size())
                .write_bytes(0, new_layout.size() - old_layout.size());

            Ok(ptr)
        }
    }

    unsafe fn shrink(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<[u8]>, AllocError> {
        debug_assert!(new_layout.size() <= old_layout.size());
        debug_assert!(new_layout.align() <= old_layout.align());

        let mut len = old_layout.size();

        // Shrinking the given area is possible if it is at the end of the arena.
        if unsafe { ptr.add(len) == self.base.add(self.offset.get()) } {
            self.offset.set(self.offset.get() - len + new_layout.size());
            len = new_layout.size();
        } else {
            debug_assert!(
                false,
                "Did you call shrink_to_fit()? Only the last allocation can be shrunk!"
            );
        }

        Ok(NonNull::slice_from_raw_parts(ptr, len))
    }
}
