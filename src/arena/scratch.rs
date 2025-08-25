// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::ops::Deref;

#[cfg(debug_assertions)]
use super::debug;
use super::{Arena, release};
use crate::apperr;
use crate::helpers::*;

static mut S_SCRATCH: [release::Arena; 2] =
    const { [release::Arena::empty(), release::Arena::empty()] };

/// Initialize the scratch arenas with a given capacity.
/// Call this before using [`scratch_arena`].
pub fn init(capacity: usize) -> apperr::Result<()> {
    unsafe {
        for s in &mut S_SCRATCH[..] {
            *s = release::Arena::new(capacity)?;
        }
    }
    Ok(())
}

/// Need an arena for temporary allocations? [`scratch_arena`] got you covered.
/// Call [`scratch_arena`] and it'll return an [`Arena`] that resets when it goes out of scope.
///
/// ---
///
/// Most methods make just two kinds of allocations:
/// * Interior: Temporary data that can be deallocated when the function returns.
/// * Exterior: Data that is returned to the caller and must remain alive until the caller stops using it.
///
/// Such methods only have two lifetimes, for which you consequently also only need two arenas.
/// ...even if your method calls other methods recursively! This is because the exterior allocations
/// of a callee are simply interior allocations to the caller, and so on, recursively.
///
/// This works as long as the two arenas flip/flop between being used as interior/exterior allocator
/// along the callstack. To ensure that is the case, we use a recursion counter in debug builds.
///
/// This approach was described among others at: <https://nullprogram.com/blog/2023/09/27/>
///
/// # Safety
///
/// If your function takes an [`Arena`] argument, you **MUST** pass it to `scratch_arena` as `Some(&arena)`.
pub fn scratch_arena(conflict: Option<&Arena>) -> ScratchArena<'static> {
    unsafe {
        #[cfg(test)]
        if S_SCRATCH[0].is_empty() {
            init(128 * 1024 * 1024).unwrap();
        }

        #[cfg(debug_assertions)]
        let conflict = conflict.map(|a| a.delegate_target_unchecked());

        let index = opt_ptr_eq(conflict, Some(&S_SCRATCH[0])) as usize;
        let arena = &S_SCRATCH[index];
        ScratchArena::new(arena)
    }
}

/// Borrows an [`Arena`] for temporary allocations.
///
/// See [`scratch_arena`].
#[cfg(debug_assertions)]
pub struct ScratchArena<'a> {
    arena: debug::Arena,
    offset: usize,
    _phantom: std::marker::PhantomData<&'a ()>,
}

#[cfg(not(debug_assertions))]
pub struct ScratchArena<'a> {
    arena: &'a Arena,
    offset: usize,
}

#[cfg(debug_assertions)]
impl<'a> ScratchArena<'a> {
    fn new(arena: &'a release::Arena) -> Self {
        let offset = arena.offset();
        ScratchArena { arena: Arena::delegated(arena), _phantom: std::marker::PhantomData, offset }
    }
}

#[cfg(not(debug_assertions))]
impl<'a> ScratchArena<'a> {
    fn new(arena: &'a release::Arena) -> Self {
        let offset = arena.offset();
        ScratchArena { arena, offset }
    }
}

impl Drop for ScratchArena<'_> {
    fn drop(&mut self) {
        unsafe { self.arena.reset(self.offset) };
    }
}

#[cfg(debug_assertions)]
impl Deref for ScratchArena<'_> {
    type Target = debug::Arena;

    fn deref(&self) -> &Self::Target {
        &self.arena
    }
}

#[cfg(not(debug_assertions))]
impl Deref for ScratchArena<'_> {
    type Target = Arena;

    fn deref(&self) -> &Self::Target {
        self.arena
    }
}
