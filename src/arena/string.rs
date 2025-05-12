use std::fmt;
use std::ops::{Bound, Deref, DerefMut, RangeBounds};

use super::Arena;
use crate::helpers::*;

#[derive(Clone)]
pub struct ArenaString<'a> {
    vec: Vec<u8, &'a Arena>,
}

impl<'a> ArenaString<'a> {
    #[must_use]
    pub const fn new_in(arena: &'a Arena) -> Self {
        Self { vec: Vec::new_in(arena) }
    }

    #[inline]
    pub fn from_str(arena: &'a Arena, s: &str) -> Self {
        let mut res = Self::new_in(arena);
        res.push_str(s);
        res
    }

    /// # Safety
    ///
    /// It says "unchecked" right there. What did you expect?
    #[inline]
    #[must_use]
    pub unsafe fn from_utf8_unchecked(bytes: Vec<u8, &'a Arena>) -> Self {
        Self { vec: bytes }
    }

    pub fn from_utf8_lossy<'s>(arena: &'a Arena, v: &'s [u8]) -> Result<&'s str, ArenaString<'a>> {
        let mut iter = v.utf8_chunks();
        let Some(mut chunk) = iter.next() else {
            return Ok("");
        };

        let valid = chunk.valid();
        if chunk.invalid().is_empty() {
            debug_assert_eq!(valid.len(), v.len());
            return Ok(unsafe { str::from_utf8_unchecked(v) });
        }

        const REPLACEMENT: &str = "\u{FFFD}";

        let mut res = Self::new_in(arena);
        res.reserve(v.len());

        loop {
            res.push_str(chunk.valid());
            if !chunk.invalid().is_empty() {
                res.push_str(REPLACEMENT);
            }
            chunk = match iter.next() {
                Some(chunk) => chunk,
                None => break,
            };
        }

        Err(res)
    }

    #[must_use]
    pub fn from_utf8_lossy_owned(v: Vec<u8, &'a Arena>) -> Self {
        match Self::from_utf8_lossy(v.allocator(), &v) {
            Ok(..) => unsafe { Self::from_utf8_unchecked(v) },
            Err(s) => s,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.vec.is_empty()
    }

    pub fn len(&self) -> usize {
        self.vec.len()
    }

    pub fn capacity(&self) -> usize {
        self.vec.capacity()
    }

    pub fn as_str(&self) -> &str {
        unsafe { str::from_utf8_unchecked(self.vec.as_slice()) }
    }

    pub fn as_mut_str(&mut self) -> &mut str {
        unsafe { str::from_utf8_unchecked_mut(self.vec.as_mut_slice()) }
    }

    pub fn as_bytes(&self) -> &[u8] {
        self.vec.as_slice()
    }

    /// Returns a mutable reference to the contents of this `String`.
    ///
    /// # Safety
    ///
    /// The underlying `&mut Vec` allows writing bytes which are not valid UTF-8.
    pub unsafe fn as_mut_vec(&mut self) -> &mut Vec<u8, &'a Arena> {
        &mut self.vec
    }

    pub fn reserve(&mut self, additional: usize) {
        self.vec.reserve(additional)
    }

    pub fn shrink_to_fit(&mut self) {
        self.vec.shrink_to_fit()
    }

    pub fn clear(&mut self) {
        self.vec.clear()
    }

    pub fn push_str(&mut self, string: &str) {
        self.vec.extend_from_slice(string.as_bytes())
    }

    pub fn push(&mut self, ch: char) {
        match ch.len_utf8() {
            1 => self.vec.push(ch as u8),
            _ => self.vec.extend_from_slice(ch.encode_utf8(&mut [0; 4]).as_bytes()),
        }
    }

    /// Same as `push(char)` but with a specified number of character copies.
    /// Shockingly absent from the standard library.
    pub fn push_repeat(&mut self, ch: char, total_copies: usize) {
        if total_copies == 0 {
            return;
        }

        let buf = unsafe { self.as_mut_vec() };

        if ch.is_ascii() {
            // Compiles down to `memset()`.
            buf.extend(std::iter::repeat_n(ch as u8, total_copies));
        } else {
            // Implements efficient string padding using quadratic duplication.
            let mut utf8_buf = [0; 4];
            let utf8 = ch.encode_utf8(&mut utf8_buf).as_bytes();
            let initial_len = buf.len();
            let added_len = utf8.len() * total_copies;
            let final_len = initial_len + added_len;

            buf.reserve(added_len);
            buf.extend_from_slice(utf8);

            while buf.len() != final_len {
                let end = (final_len - buf.len() + initial_len).min(buf.len());
                buf.extend_from_within(initial_len..end);
            }
        }
    }

    pub fn replace_range<R: RangeBounds<usize>>(&mut self, range: R, replace_with: &str) {
        match range.start_bound() {
            Bound::Included(&n) => assert!(self.is_char_boundary(n)),
            Bound::Excluded(&n) => assert!(self.is_char_boundary(n + 1)),
            Bound::Unbounded => {}
        };
        match range.end_bound() {
            Bound::Included(&n) => assert!(self.is_char_boundary(n + 1)),
            Bound::Excluded(&n) => assert!(self.is_char_boundary(n)),
            Bound::Unbounded => {}
        };
        vec_replace(unsafe { self.as_mut_vec() }, range, replace_with.as_bytes());
    }
}

impl fmt::Debug for ArenaString<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&**self, f)
    }
}

impl PartialEq<&str> for ArenaString<'_> {
    fn eq(&self, other: &&str) -> bool {
        self.as_str() == *other
    }
}

impl Deref for ArenaString<'_> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}

impl DerefMut for ArenaString<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_mut_str()
    }
}

impl fmt::Display for ArenaString<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

impl fmt::Write for ArenaString<'_> {
    #[inline]
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.push_str(s);
        Ok(())
    }

    #[inline]
    fn write_char(&mut self, c: char) -> fmt::Result {
        self.push(c);
        Ok(())
    }
}

#[macro_export]
macro_rules! arena_format {
    ($arena:expr, $($arg:tt)*) => {{
        use std::fmt::Write as _;
        let mut output = $crate::arena::ArenaString::new_in($arena);
        output.write_fmt(format_args!($($arg)*)).unwrap();
        output
    }}
}
