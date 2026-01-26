// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! Microsoft Edit's adapter to LSH.

pub mod cache;
mod definitions;
mod highlighter;

use std::path::Path;

pub use definitions::{FILE_ASSOCIATIONS, HighlightKind, LANGUAGES};
pub use highlighter::*;
pub use lsh::engine::Language;
use stdext::glob::glob_match;

pub fn process_file_associations<T>(
    associations: &[(T, &'static Language)],
    path: &Path,
) -> Option<&'static Language>
where
    T: AsRef<[u8]>,
{
    let path = path.as_os_str().as_encoded_bytes();

    for a in associations {
        if glob_match(a.0.as_ref(), path) {
            return Some(a.1);
        }
    }

    None
}
