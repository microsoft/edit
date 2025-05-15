use std::ops::Deref;

#[cfg(debug_assertions)]
use super::debug;
use super::{Arena, release};
use crate::apperr;
use crate::helpers::*;

static mut S_SCRATCH: [release::Arena; 2] =
    const { [release::Arena::empty(), release::Arena::empty()] };

pub fn init() -> apperr::Result<()> {
    unsafe {
        for s in &mut S_SCRATCH[..] {
            *s = release::Arena::new(128 * MEBI)?;
        }
    }
    Ok(())
}

/// Returns a new scratch arena for temporary allocations,
/// ensuring it doesn't conflict with the provided arena.
pub fn scratch_arena(conflict: Option<&Arena>) -> ScratchArena<'static> {
    unsafe {
        #[cfg(debug_assertions)]
        let conflict = conflict.map(|a| a.delegate_target_unchecked());

        let index = opt_ptr_eq(conflict, Some(&S_SCRATCH[0])) as usize;
        let arena = &mut S_SCRATCH[index];
        ScratchArena::new(arena)
    }
}

// Most methods make just two kinds of allocations:
// * Interior: Temporary data that can be deallocated when the function returns.
// * Exterior: Data that is returned to the caller and must remain alive until the caller stops using it.
//
// Such methods only have two lifetimes, for which you consequently also only need two arenas.
// ...even if your method calls other methods recursively! This is because the exterior allocations
// of a callee are simply interior allocations to the caller, and so on, recursively.
//
// This works as long as the two arenas flip/flop between being used as interior/exterior allocator
// along the callstack. To ensure that is the case, we use a recursion counter in debug builds.
//
// This approach was described among others at: https://nullprogram.com/blog/2023/09/27/
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
