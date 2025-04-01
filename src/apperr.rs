use crate::loc::{LocId, loc};
use crate::{icu, sys};
use std::io;
use std::result;

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

    pub fn message(&self) -> String {
        match *self {
            APP_ICU_MISSING => loc(LocId::ErrorIcuMissing).to_string(),
            Error::App(code) => format!("Unknown app error code: {}", code),
            Error::Icu(code) => icu::apperr_format(code),
            Error::Sys(code) => sys::apperr_format(code),
        }
    }
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        sys::io_error_to_apperr(err)
    }
}
