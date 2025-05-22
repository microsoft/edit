// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! Provides a transparent error type for edit.

use std::{io, result};

use crate::sys;

pub const APP_ICU_MISSING: Error = Error::new_app(0);

/// Edit's transparent `Result` type.
pub type Result<T> = result::Result<T, Error>;

/// Edit's transparent `Error` type.
/// Abstracts over system and application errors.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Error {
    App(u32),
    Icu(u32),
    Sys(u32),
}

impl Error {
    pub const fn new_app(code: u32) -> Self {
        Self::App(code)
    }

    pub const fn new_icu(code: u32) -> Self {
        Self::Icu(code)
    }

    pub const fn new_sys(code: u32) -> Self {
        Self::Sys(code)
    }
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        sys::io_error_to_apperr(err)
    }
}
