// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! Arena allocators. Small and fast.

#[cfg(debug_assertions)]
mod debug;
mod release;
mod scratch;

#[cfg(all(not(doc), debug_assertions))]
pub use self::debug::Arena;
#[cfg(any(doc, not(debug_assertions)))]
pub use self::release::Arena;
pub use self::scratch::{ScratchArena, init, scratch_arena};
