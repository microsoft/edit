mod debug;
mod release;
mod scratch;
mod string;

#[cfg(debug_assertions)]
pub use self::debug::Arena;
#[cfg(not(debug_assertions))]
pub use self::release::Arena;
pub use self::scratch::{ScratchArena, init, scratch_arena};
pub use self::string::ArenaString;
