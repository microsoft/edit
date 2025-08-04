// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! Abstractions over reading/writing arbitrary text containers.

use std::ffi::OsString;
use std::mem;
use std::ops::Range;
use std::path::PathBuf;

use crate::arena::scratch_arena;
use crate::helpers::ReplaceRange as _;
use crate::stdext::*;

/// An abstraction over reading from text containers.
pub trait ReadableDocument {
    /// Read some bytes starting at (including) the given absolute offset.
    ///
    /// # Warning
    ///
    /// * Be lenient on inputs:
    ///   * The given offset may be out of bounds and you MUST clamp it.
    ///   * You should not assume that offsets are at grapheme cluster boundaries.
    /// * Be strict on outputs:
    ///   * You MUST NOT break grapheme clusters across chunks.
    ///   * You MUST NOT return an empty slice unless the offset is at or beyond the end.
    fn read_forward(&self, off: usize) -> &[u8];

    /// Read some bytes before (but not including) the given absolute offset.
    ///
    /// # Warning
    ///
    /// * Be lenient on inputs:
    ///   * The given offset may be out of bounds and you MUST clamp it.
    ///   * You should not assume that offsets are at grapheme cluster boundaries.
    /// * Be strict on outputs:
    ///   * You MUST NOT break grapheme clusters across chunks.
    ///   * You MUST NOT return an empty slice unless the offset is zero.
    fn read_backward(&self, off: usize) -> &[u8];
}

/// An abstraction over writing to text containers.
pub trait WriteableDocument: ReadableDocument {
    /// Replace the given range with the given bytes.
    ///
    /// # Warning
    ///
    /// * The given range may be out of bounds and you MUST clamp it.
    /// * The replacement may not be valid UTF8.
    fn replace(&mut self, range: Range<usize>, replacement: &[u8]);
}

impl ReadableDocument for &[u8] {
    fn read_forward(&self, off: usize) -> &[u8] {
        let s = *self;
        &s[off.min(s.len())..]
    }

    fn read_backward(&self, off: usize) -> &[u8] {
        let s = *self;
        &s[..off.min(s.len())]
    }
}

impl ReadableDocument for String {
    fn read_forward(&self, off: usize) -> &[u8] {
        let s = self.as_bytes();
        &s[off.min(s.len())..]
    }

    fn read_backward(&self, off: usize) -> &[u8] {
        let s = self.as_bytes();
        &s[..off.min(s.len())]
    }
}

impl WriteableDocument for String {
    fn replace(&mut self, range: Range<usize>, replacement: &[u8]) {
        // `replacement` is not guaranteed to be valid UTF-8, so we need to sanitize it.
        let scratch = scratch_arena(None);
        let utf8 = ExString::from_utf8_lossy(&*scratch, replacement);
        let src = match &utf8 {
            Ok(s) => s,
            Err(s) => s.as_str(),
        };

        // SAFETY: `range` is guaranteed to be on codepoint boundaries.
        unsafe { self.as_mut_vec() }.replace_range(range, src.as_bytes());
    }
}

impl ReadableDocument for PathBuf {
    fn read_forward(&self, off: usize) -> &[u8] {
        let s = self.as_os_str().as_encoded_bytes();
        &s[off.min(s.len())..]
    }

    fn read_backward(&self, off: usize) -> &[u8] {
        let s = self.as_os_str().as_encoded_bytes();
        &s[..off.min(s.len())]
    }
}

impl WriteableDocument for PathBuf {
    fn replace(&mut self, range: Range<usize>, replacement: &[u8]) {
        let mut vec = mem::take(self).into_os_string().into_encoded_bytes();
        vec.replace_range(range, replacement);
        *self = unsafe { Self::from(OsString::from_encoded_bytes_unchecked(vec)) };
    }
}
