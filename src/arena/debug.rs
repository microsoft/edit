#![allow(clippy::missing_safety_doc, clippy::mut_from_ref)]

use std::alloc::{AllocError, Allocator, Layout};
use std::mem::{self, MaybeUninit};
use std::ptr::NonNull;

use super::release;
use crate::apperr;

pub enum Arena {
    // Delegate is 'static, because release::Arena requires no lifetime
    // annotations, and so this struct cannot use them either.
    Delegated { delegate: &'static release::Arena, borrow: usize },
    Owned { arena: release::Arena },
}

impl Drop for Arena {
    fn drop(&mut self) {
        if let Arena::Delegated { delegate, borrow } = self {
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

    pub fn new(capacity: usize) -> apperr::Result<Arena> {
        Ok(Self::Owned { arena: release::Arena::new(capacity)? })
    }

    pub(super) fn delegated(delegate: &release::Arena) -> Arena {
        let borrow = delegate.borrows.get() + 1;
        delegate.borrows.set(borrow);
        Self::Delegated { delegate: unsafe { mem::transmute(delegate) }, borrow }
    }

    #[inline]
    pub(super) fn delegate_target(&self) -> &release::Arena {
        match *self {
            Arena::Delegated { delegate, borrow } => {
                assert!(
                    borrow == delegate.borrows.get(),
                    "Arena already borrowed by a newer ScratchArena"
                );
                delegate
            }
            Arena::Owned { ref arena } => arena,
        }
    }

    #[inline]
    pub(super) fn delegate_target_unchecked(&self) -> &release::Arena {
        match self {
            Arena::Delegated { delegate, .. } => delegate,
            Arena::Owned { arena } => arena,
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

unsafe impl Allocator for Arena {
    fn allocate(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        self.delegate_target().alloc_raw(layout.size(), layout.align())
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
