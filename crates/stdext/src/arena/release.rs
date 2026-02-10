// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#![allow(clippy::mut_from_ref)]

use std::cell::Cell;
use std::mem::MaybeUninit;
use std::ptr::{self, NonNull};
use std::{io, mem, slice};

use crate::alloc::Allocator;
use crate::sys;

#[cfg(target_pointer_width = "32")]
const ALLOC_CHUNK_SIZE: usize = 32 * 1024;
#[cfg(target_pointer_width = "64")]
const ALLOC_CHUNK_SIZE: usize = 64 * 1024;

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

    pub fn new(capacity: usize) -> io::Result<Self> {
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
    pub(super) fn alloc_raw(&self, bytes: usize, alignment: usize) -> NonNull<[u8]> {
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
        unsafe { NonNull::slice_from_raw_parts(self.base.add(beg), bytes) }
    }

    // With the code in `alloc_raw_bump()` out of the way, `alloc_raw()` compiles down to some super tight assembly.
    #[cold]
    fn alloc_raw_bump(&self, beg: usize, end: usize) -> NonNull<[u8]> {
        let offset = self.offset.get();
        let commit_old = self.commit.get();
        let commit_new = (end + ALLOC_CHUNK_SIZE - 1) & !(ALLOC_CHUNK_SIZE - 1);

        if commit_new > self.capacity
            || unsafe {
                sys::virtual_commit(self.base.add(commit_old), commit_new - commit_old).is_err()
            }
        {
            // Panicking inside this [cold] function has the benefit of removing duplicated panic code from any
            // inlined alloc() function. If we ever add fallible allocations, we should probably duplicate alloc_raw()
            // and alloc_raw_bump() instead of returning a Result here and calling unwrap() in the common path.
            panic!("out of memory");
        }

        if cfg!(debug_assertions) {
            let ptr = unsafe { self.base.add(offset) };
            let len = (end + 128).min(self.commit.get()) - offset;
            unsafe { slice::from_raw_parts_mut(ptr.as_ptr(), len).fill(0xCD) };
        }

        self.commit.replace(commit_new);
        self.offset.replace(end);
        unsafe { NonNull::slice_from_raw_parts(self.base.add(beg), end - beg) }
    }

    #[inline]
    #[allow(clippy::mut_from_ref)]
    pub fn alloc_uninit<T>(&self) -> &mut MaybeUninit<T> {
        let bytes = mem::size_of::<T>();
        let alignment = mem::align_of::<T>();
        let ptr = self.alloc_raw(bytes, alignment);
        unsafe { ptr.cast().as_mut() }
    }

    #[inline]
    #[allow(clippy::mut_from_ref)]
    pub fn alloc_uninit_slice<T>(&self, count: usize) -> &mut [MaybeUninit<T>] {
        let bytes = mem::size_of::<T>() * count;
        let alignment = mem::align_of::<T>();
        let ptr = self.alloc_raw(bytes, alignment);
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

impl Allocator for Arena {
    unsafe fn realloc(
        &self,
        old_ptr: NonNull<u8>,
        old_size: usize,
        new_size: usize,
        align: usize,
    ) -> NonNull<[u8]> {
        if unsafe { old_ptr.add(old_size) == self.base.add(self.offset.get()) } {
            // Check if it's the last allocation we made.
            // If so, we can grow/shrink it in place without copying.
            if new_size > old_size {
                self.alloc_raw(new_size - old_size, align);
            } else {
                self.offset.set(self.offset.get() - old_size + new_size);
            }
            NonNull::slice_from_raw_parts(old_ptr, new_size)
        } else if new_size > old_size {
            // Otherwise, we have to allocate a new area and copy it over.
            unsafe {
                let new_ptr = self.alloc_raw(new_size, align);
                ptr::copy_nonoverlapping(old_ptr.as_ptr(), new_ptr.as_ptr() as *mut _, old_size);
                new_ptr
            }
        } else {
            debug_assert!(false, "only the last allocation can be shrunk");
            NonNull::slice_from_raw_parts(old_ptr, old_size)
        }
    }

    unsafe fn dealloc(&self, _ptr: NonNull<u8>, _size: usize, _align: usize) {}
}
