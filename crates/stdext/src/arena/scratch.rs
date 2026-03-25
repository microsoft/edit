// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::io;
#[cfg(debug_assertions)]
use std::marker::PhantomData;
use std::ops::Deref;

#[cfg(debug_assertions)]
use super::debug;
use super::{Arena, release};
use crate::helpers::*;

/// Borrows an [`Arena`] for temporary allocations.
///
/// See [`scratch_arena`].
#[cfg(debug_assertions)]
pub struct ScratchArena<'a> {
    arena: debug::Arena,
    offset: usize,
    _phantom: PhantomData<&'a ()>,
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
        ScratchArena { arena: Arena::delegated(arena), _phantom: PhantomData, offset }
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

mod single_threaded {
    use super::*;

    static mut S_SCRATCH: [release::Arena; 2] =
        const { [release::Arena::empty(), release::Arena::empty()] };

    /// Initialize the scratch arenas with a given capacity.
    /// Call this before using [`scratch_arena`].
    #[allow(dead_code)]
    pub fn init(capacity: usize) -> io::Result<()> {
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
    #[allow(dead_code)]
    pub fn scratch_arena(conflict: Option<&Arena>) -> ScratchArena<'static> {
        unsafe {
            #[cfg(debug_assertions)]
            let conflict = conflict.map(|a| a.delegate_target_unchecked());

            let index = opt_ptr_eq(conflict, Some(&S_SCRATCH[0])) as usize;
            let arena = &S_SCRATCH[index];
            ScratchArena::new(arena)
        }
    }
}

mod multi_threaded {
    use std::cell::Cell;
    use std::ptr;
    use std::sync::atomic::{AtomicUsize, Ordering};

    use super::*;

    thread_local! {
        static S_SCRATCH: [Cell<release::Arena>; 2] =
            const { [Cell::new(release::Arena::empty()), Cell::new(release::Arena::empty())] };
    }

    static INIT_SIZE: AtomicUsize = AtomicUsize::new(128 * MEBI);

    /// Sets the default scratch arena size.
    pub fn init(capacity: usize) -> io::Result<()> {
        if capacity != 0 {
            INIT_SIZE.store(capacity, Ordering::Relaxed);
        }
        Ok(())
    }

    /// See `single_threaded::scratch_arena`.
    #[allow(dead_code)]
    pub fn scratch_arena(conflict: Option<&Arena>) -> ScratchArena<'static> {
        #[cfg(debug_assertions)]
        let conflict = conflict.map(|a| a.delegate_target_unchecked());

        #[cold]
        fn init(s: &[Cell<release::Arena>; 2]) {
            let capacity = INIT_SIZE.load(Ordering::Relaxed);
            for s in s {
                s.set(release::Arena::new(capacity).unwrap());
            }
        }

        S_SCRATCH.with(|arenas| {
            let index = ptr::eq(opt_ptr(conflict), arenas[0].as_ptr()) as usize;
            let arena = unsafe { &*arenas[index].as_ptr() };
            if arena.is_empty() {
                init(arenas);
            }
            ScratchArena::new(arena)
        })
    }
}

#[cfg(not(feature = "single-threaded"))]
pub use multi_threaded::*;
#[cfg(feature = "single-threaded")]
pub use single_threaded::*;
