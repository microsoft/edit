use std::ffi::c_int;
use std::num::NonZeroU8;
use std::ops::BitOr;
use std::{io, time};

use crate::arena::Arena;

// This is heavily inspired by the excellent `mio` crate.
//
// However, I'm strongly against making the `mio` API the basis for all platforms (particularly Windows).
// In my opinion, we're better served with exposing high level APIs from the `sys` module, such as
// "new HTTP client" or "new file watcher", as the differences between the platforms are too significant.
//
// Ultimately, this is also the reason I don't depend on the otherwise excellent `tokio` crate.
//
// For the purpose of msedit this is 9000% overkill, however I
// a) learned not just a lot, but pretty much everything
// b) it maps much better to `kqueue` and would otherwise be just as confusing for no good reason
pub trait Registrable {
    fn register_fd(&mut self, token: Token, fd: c_int, interest: Interest) -> io::Result<()>;
    fn register_signals(&mut self, assocs: &[(Token, c_int)]) -> io::Result<()>;
    fn wait<'a>(&mut self, arena: &'a Arena, timeout: time::Duration) -> io::Result<&'a [Token]>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct Token(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct Interest(NonZeroU8);

impl Interest {
    pub const READABLE: Interest = Interest(NonZeroU8::new(1).unwrap());
    pub const WRITABLE: Interest = Interest(NonZeroU8::new(2).unwrap());

    pub const fn readable(self) -> bool {
        self.0.get() & Self::READABLE.0.get() != 0
    }

    pub const fn writable(self) -> bool {
        self.0.get() & Self::WRITABLE.0.get() != 0
    }
}

impl BitOr for Interest {
    type Output = Self;

    #[inline]
    fn bitor(self, other: Self) -> Self {
        Self(self.0 | other.0)
    }
}
