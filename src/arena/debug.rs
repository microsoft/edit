// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#![allow(clippy::missing_safety_doc, clippy::mut_from_ref)]

use std::mem::{self, MaybeUninit};
use std::ptr::NonNull;

use super::release;
use crate::apperr;
use crate::stdext::*;

/// A debug wrapper for [`release::Arena`].
///
/// The problem with [`super::ScratchArena`] is that it only "borrows" an underlying
/// [`release::Arena`]. Once the [`super::ScratchArena`] is dropped it resets the watermark
/// of the underlying [`release::Arena`], freeing all allocations done since borrowing it.
///
/// It is completely valid for the same [`release::Arena`] to be borrowed multiple times at once,
/// *as long as* you only use the most recent borrow. Bad example:
/// ```should_panic
/// use edit::arena::scratch_arena;
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

    pub fn new(capacity: usize) -> apperr::Result<Self> {
        Ok(Self::Owned { arena: release::Arena::new(capacity)? })
    }

    pub(super) fn delegated(delegate: &release::Arena) -> Self {
        let borrow = delegate.borrows.get() + 1;
        delegate.borrows.set(borrow);
        Self::Delegated { delegate: unsafe { mem::transmute(delegate) }, borrow }
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

    pub fn offset(&self) -> usize {
        self.delegate_target().offset()
    }

    pub unsafe fn reset(&self, to: usize) {
        unsafe { self.delegate_target().reset(to) }
    }

    pub fn alloc_uninit<T>(&self) -> &mut MaybeUninit<T> {
        self.delegate_target().alloc_uninit()
    }

    pub fn alloc_uninit_slice<T>(&self, count: usize) -> &mut [MaybeUninit<T>] {
        self.delegate_target().alloc_uninit_slice(count)
    }
}

unsafe impl ExAllocator for Arena {
    fn allocate(&self, size: usize, align: usize) -> Result<NonNull<[u8]>, ExAllocError> {
        self.delegate_target().allocate(size, align)
    }

    unsafe fn grow(
        &self,
        ptr: NonNull<u8>,
        old_size: usize,
        new_size: usize,
        align: usize,
    ) -> Result<NonNull<[u8]>, ExAllocError> {
        // Call the `grow` implementation of `release::Arena` directly
        // to avoid the blanket `&A` impl that requires `ExDeallocator`.
        unsafe {
            <release::Arena as ExAllocator>::grow(
                self.delegate_target(),
                ptr,
                old_size,
                new_size,
                align,
            )
        }
    }

    unsafe fn deallocate(&self, _ptr: NonNull<u8>, _size: usize, _align: usize) {}
}
