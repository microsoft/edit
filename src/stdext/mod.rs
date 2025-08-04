//! Rust doesn't have allocator support in its standard library?

mod alloc;
mod maybe_owned;

pub use self::alloc::*;
pub use self::maybe_owned::*;
