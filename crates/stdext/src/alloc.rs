// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::alloc::{Layout, alloc, dealloc, handle_alloc_error, realloc};
use std::ptr::NonNull;

pub trait Allocator {
    /// # Safety
    ///
    /// It's an allocator trait. It's unsafe.
    /// Note that `old_ptr` may be invalid if `old_size` is 0.
    unsafe fn realloc(
        &self,
        old_ptr: NonNull<u8>,
        old_size: usize,
        new_size: usize,
        align: usize,
    ) -> NonNull<[u8]>;

    /// # Safety
    ///
    /// Naturally, `ptr` must be valid.
    unsafe fn dealloc(&self, ptr: NonNull<u8>, size: usize, align: usize);
}

pub struct GlobalAllocator;

impl Allocator for GlobalAllocator {
    unsafe fn realloc(
        &self,
        old_ptr: NonNull<u8>,
        old_size: usize,
        new_size: usize,
        align: usize,
    ) -> NonNull<[u8]> {
        unsafe {
            let new_ptr = if old_size == 0 {
                let layout = Layout::from_size_align_unchecked(new_size, align);
                alloc(layout)
            } else {
                let layout = Layout::from_size_align_unchecked(old_size, align);
                realloc(old_ptr.as_ptr(), layout, new_size)
            };
            let Some(new_ptr) = NonNull::new(new_ptr) else {
                let layout = Layout::from_size_align_unchecked(new_size, align);
                handle_alloc_error(layout);
            };
            NonNull::slice_from_raw_parts(new_ptr, new_size)
        }
    }

    unsafe fn dealloc(&self, ptr: NonNull<u8>, size: usize, align: usize) {
        unsafe {
            let layout = Layout::from_size_align_unchecked(size, align);
            dealloc(ptr.as_ptr(), layout);
        }
    }
}
