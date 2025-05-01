use std::{io, result};

use crate::sys;

// Remember to add an entry to `Error::message()` for each new error.
pub const APP_ICU_MISSING: Error = Error::new_app(0);

pub type Result<T> = result::Result<T, Error>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Error {
    App(u32),
    Icu(u32),
    Sys(u32),
}

impl Error {
    pub const fn new_app(code: u32) -> Self {
        Error::App(code)
    }

    pub const fn new_icu(code: u32) -> Self {
        Error::Icu(code)
    }

    pub const fn new_sys(code: u32) -> Self {
        Error::Sys(code)
    }
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        sys::io_error_to_apperr(err)
    }
}
