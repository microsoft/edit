// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::io;

use ie::{buffer, icu};

#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    Icu(icu::Error),
}

pub type Result<T> = std::result::Result<T, Error>;

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        Self::Io(err)
    }
}

impl From<icu::Error> for Error {
    fn from(err: icu::Error) -> Self {
        Self::Icu(err)
    }
}

impl From<buffer::IoError> for Error {
    fn from(err: buffer::IoError) -> Self {
        match err {
            buffer::IoError::Io(e) => Self::Io(e),
            buffer::IoError::Icu(e) => Self::Icu(e),
        }
    }
}
