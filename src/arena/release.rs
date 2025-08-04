// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#![allow(clippy::mut_from_ref)]

use std::cell::Cell;
use std::hint::cold_path;
use std::mem::MaybeUninit;
use std::ptr::{self, NonNull};
use std::{mem, slice};

use crate::helpers::*;
use crate::stdext::*;
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
    ) -> Result<NonNull<[u8]>, ExAllocError> {
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
    fn alloc_raw_bump(&self, beg: usize, end: usize) -> Result<NonNull<[u8]>, ExAllocError> {
        let offset = self.offset.get();
        let commit_old = self.commit.get();
        let commit_new = (end + ALLOC_CHUNK_SIZE - 1) & !(ALLOC_CHUNK_SIZE - 1);

        if commit_new > self.capacity
            || unsafe {
                sys::virtual_commit(self.base.add(commit_old), commit_new - commit_old).is_err()
            }
        {
            return Err(ExAllocError);
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

unsafe impl ExAllocator for Arena {
    fn allocate(&self, size: usize, align: usize) -> Result<NonNull<[u8]>, ExAllocError> {
        self.alloc_raw(size, align).map_err(|_| ExAllocError)
    }

    unsafe fn grow(
        &self,
        ptr: NonNull<u8>,
        old_size: usize,
        new_size: usize,
        align: usize,
    ) -> Result<NonNull<[u8]>, ExAllocError> {
        let new_ptr;

        // Growing the given area is possible if it is at the end of the arena.
        if unsafe { ptr.add(old_size) == self.base.add(self.offset.get()) } {
            new_ptr = ptr;
            let delta = new_size - old_size;
            // Assuming that the given ptr/length area is at the end of the arena,
            // we can just push more memory to the end of the arena to grow it.
            self.alloc_raw(delta, 1)?;
        } else {
            cold_path();
            new_ptr = self.allocate(new_size, align)?.cast();
            unsafe { ptr::copy_nonoverlapping(ptr.as_ptr(), new_ptr.as_ptr(), old_size) };
        }

        Ok(NonNull::slice_from_raw_parts(new_ptr, new_size))
    }

    unsafe fn deallocate(&self, _ptr: NonNull<u8>, _size: usize, _align: usize) {}
}
