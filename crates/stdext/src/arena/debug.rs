// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#![allow(clippy::missing_safety_doc, clippy::mut_from_ref)]

use std::alloc::{AllocError, Allocator, Layout};
use std::io;
use std::ops::Deref;
use std::ptr::NonNull;

use super::release;

/// A debug wrapper for [`release::Arena`].
///
/// The problem with [`super::ScratchArena`] is that it only "borrows" an underlying
/// [`release::Arena`]. Once the [`super::ScratchArena`] is dropped it resets the watermark
/// of the underlying [`release::Arena`], freeing all allocations done since borrowing it.
///
/// It is completely valid for the same [`release::Arena`] to be borrowed multiple times at once,
/// *as long as* you only use the most recent borrow. Bad example:
/// ```should_panic
/// use stdext::arena::scratch_arena;
///
/// let mut scratch1 = scratch_arena(None);
/// let mut scratch2 = scratch_arena(None);
///
/// let foo = scratch1.alloc_uninit::<usize>();
///
/// // This will also reset `scratch1`'s allocation.
/// drop(scratch2);
///
/// *foo; // BOOM! ...if it wasn't for our debug wrapper.
/// ```
///
/// To avoid this, this wraps the real [`release::Arena`] in a "debug" one, which pretends as if every
/// instance of itself is a distinct [`release::Arena`] instance. Then we use this "debug" [`release::Arena`]
/// for [`super::ScratchArena`] which allows us to track which borrow is the most recent one.
pub enum Arena {
    // Delegate is 'static, because release::Arena requires no lifetime
    // annotations, and so this mere debug helper cannot use them either.
    Delegated { delegate: &'static release::Arena, borrow: usize },
    Owned { arena: release::Arena },
}

impl Drop for Arena {
    fn drop(&mut self) {
        if let Self::Delegated { delegate, borrow } = self {
            let borrows = delegate.borrows.get();
            assert_eq!(*borrow, borrows);
            delegate.borrows.set(borrows - 1);
        }
    }
}

impl Default for Arena {
    fn default() -> Self {
        Self::empty()
    }
}

impl Arena {
    pub const fn empty() -> Self {
        Self::Owned { arena: release::Arena::empty() }
    }

    pub fn new(capacity: usize) -> io::Result<Self> {
        Ok(Self::Owned { arena: release::Arena::new(capacity)? })
    }

    pub(super) fn delegated(delegate: &release::Arena) -> Self {
        let borrow = delegate.borrows.get() + 1;
        delegate.borrows.set(borrow);
        Self::Delegated { delegate: unsafe { &*(delegate as *const _) }, borrow }
    }

    #[inline]
    pub(super) fn delegate_target(&self) -> &release::Arena {
        match *self {
            Self::Delegated { delegate, borrow } => {
                assert!(
                    borrow == delegate.borrows.get(),
                    "Arena already borrowed by a newer ScratchArena"
                );
                delegate
            }
            Self::Owned { ref arena } => arena,
        }
    }

    #[inline]
    pub(super) fn delegate_target_unchecked(&self) -> &release::Arena {
        match self {
            Self::Delegated { delegate, .. } => delegate,
            Self::Owned { arena } => arena,
        }
    }
}

impl Deref for Arena {
    type Target = release::Arena;

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.delegate_target()
    }
}

unsafe impl Allocator for Arena {
    fn allocate(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        Ok(self.delegate_target().alloc_raw(layout.size(), layout.align()))
    }

    fn allocate_zeroed(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        self.delegate_target().allocate_zeroed(layout)
    }

    // While it is possible to shrink the tail end of the arena, it is
    // not very useful given the existence of scoped scratch arenas.
    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
        unsafe { self.delegate_target().deallocate(ptr, layout) }
    }

    unsafe fn grow(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<[u8]>, AllocError> {
        unsafe { self.delegate_target().grow(ptr, old_layout, new_layout) }
    }

    unsafe fn grow_zeroed(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<[u8]>, AllocError> {
        unsafe { self.delegate_target().grow_zeroed(ptr, old_layout, new_layout) }
    }

    unsafe fn shrink(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<[u8]>, AllocError> {
        unsafe { self.delegate_target().shrink(ptr, old_layout, new_layout) }
    }
}
