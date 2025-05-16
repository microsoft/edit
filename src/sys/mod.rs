// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! Platform abstractions.

use std::fs::File;
use std::path::Path;

use crate::apperr;

#[cfg(unix)]
mod unix;
#[cfg(windows)]
mod windows;

#[cfg(not(windows))]
pub use std::fs::canonicalize;

#[cfg(unix)]
pub use unix::*;
#[cfg(windows)]
pub use windows::*;

pub fn file_id_at(path: &Path) -> apperr::Result<FileId> {
    let file = File::open(path)?;
    let file_id = file_id(&file)?;
    Ok(file_id)
}
