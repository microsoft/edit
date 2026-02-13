// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::fmt::{self};
use std::ops::{Bound, Deref, DerefMut, RangeBounds};
use std::slice;
use std::str::Utf8Error;

use crate::alloc::Allocator;
use crate::cold_path;
use crate::collections::BVec;

/// Like a `String` but on borrowed memory. Built on top of [`BVec<u8>`].
pub struct BString<'a> {
    vec: BVec<'a, u8>,
}

impl<'a> BString<'a> {
    /// The label on the tin says "empty". You open it. It's empty.
    #[inline]
    pub const fn empty() -> Self {
        Self { vec: BVec::empty() }
    }

    /// See [`BVec::from_std_vec()`].
    pub fn from_std_string(str: String) -> Self {
        Self { vec: BVec::from_std_vec(str.into_bytes()) }
    }

    /// See [`BVec::into_std_vec()`].
    pub fn into_std_string(self) -> String {
        unsafe { String::from_utf8_unchecked(self.vec.into_std_vec()) }
    }

    /// Validates and wraps a byte vec as UTF-8.
    pub fn from_utf8(vec: BVec<'a, u8>) -> Result<Self, Utf8Error> {
        str::from_utf8(&vec)?;
        Ok(Self { vec })
    }

    /// Validates UTF-8, replacing invalid sequences with U+FFFD.
    pub fn from_utf8_lossy(alloc: &'a dyn Allocator, vec: BVec<'a, u8>) -> Self {
        let mut iter = vec.utf8_chunks();

        if let Some(mut chunk) = iter.next()
            && !chunk.invalid().is_empty()
        {
            // We only need to create a copy if the input is non-empty
            // and contains at least some invalid UTF-8.
            cold_path();

            let mut res = Self::empty();
            res.reserve(alloc, vec.len());

            loop {
                res.push_str(alloc, chunk.valid());
                if !chunk.invalid().is_empty() {
                    res.push_str(alloc, "\u{FFFD}");
                }
                chunk = match iter.next() {
                    Some(chunk) => chunk,
                    None => break,
                };
            }

            res
        } else {
            // Otherwise, we can just return the `vec` as-is.
            Self { vec }
        }
    }

    /// Wraps a byte vec as UTF-8 without validating it.
    ///
    /// # Safety
    ///
    /// The bytes in `vec` must be valid UTF-8.
    #[inline]
    pub unsafe fn from_utf8_unchecked(vec: BVec<'a, u8>) -> Self {
        Self { vec }
    }

    /// Copies `&str` into the allocator.
    pub fn from_str(alloc: &'a dyn Allocator, s: &str) -> Self {
        let mut res = Self::empty();
        res.push_str(alloc, s);
        res
    }

    /// Decodes UTF-16, replacing unpaired surrogates with U+FFFD.
    pub fn from_utf16_lossy(alloc: &'a dyn Allocator, string: &[u16]) -> Self {
        let mut res = Self::empty();
        res.push_utf16_lossy(alloc, string);
        res
    }

    /// Length in bytes, not characters.
    #[inline]
    pub fn len(&self) -> usize {
        self.vec.len()
    }

    /// Total byte capacity of the backing buffer.
    #[inline]
    pub fn capacity(&self) -> usize {
        self.vec.capacity()
    }

    /// True if the string is empty.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.vec.is_empty()
    }

    /// True if if the buffer is full.
    #[inline]
    pub fn is_full(&self) -> bool {
        self.vec.is_full()
    }

    /// The raw UTF-8 bytes.
    #[inline]
    pub fn as_bytes(&self) -> &[u8] {
        self.vec.as_slice()
    }

    /// View as a `&str`.
    #[inline]
    pub fn as_str(&self) -> &str {
        unsafe { str::from_utf8_unchecked(self.vec.as_slice()) }
    }

    /// View as a `&mut str`.
    #[inline]
    pub fn as_mut_str(&mut self) -> &mut str {
        unsafe { str::from_utf8_unchecked_mut(self.vec.as_mut_slice()) }
    }

    /// # Safety
    ///
    /// The underlying `&mut Vec` allows writing bytes which are not valid UTF-8.
    #[inline]
    pub unsafe fn as_mut_vec(&mut self) -> &mut BVec<'a, u8> {
        &mut self.vec
    }

    /// Consume the string, returning a `&mut str` that lives as long as the borrowed memory.
    #[inline]
    pub fn leak(self) -> &'a mut str {
        unsafe { str::from_utf8_unchecked_mut(self.vec.leak()) }
    }

    /// Ensures space for at least `additional` more bytes, with amortized growth.
    #[inline]
    pub fn reserve(&mut self, alloc: &'a dyn Allocator, additional: usize) {
        self.vec.reserve(alloc, additional);
    }

    /// Ensures space for at least `additional` more bytes, without over-allocating.
    #[inline]
    pub fn reserve_exact(&mut self, arena: &'a dyn Allocator, additional: usize) {
        self.vec.reserve_exact(arena, additional);
    }

    /// Appends a single `char`, encoding it as UTF-8.
    pub fn push(&mut self, alloc: &'a dyn Allocator, ch: char) {
        self.reserve(alloc, 4);
        unsafe {
            let len = self.vec.len();
            let dst = self.vec.as_mut_ptr().add(len);
            let add = ch.encode_utf8(slice::from_raw_parts_mut(dst, 4)).len();
            self.vec.set_len(len + add);
        }
    }

    /// Empties the string. The allocation is kept.
    pub fn clear(&mut self) {
        self.vec.clear();
    }

    /// Returns a [`BorrowedStringFormatter`] pairing this string with an allocator,
    /// enabling use with `write!` and `fmt::Write`.
    pub fn formatter<A>(&mut self, alloc: &'a A) -> BStringFormatter<'_, 'a, A>
    where
        A: Allocator,
    {
        BStringFormatter { string: self, alloc }
    }

    /// Appends a `&str`.
    pub fn push_str(&mut self, alloc: &'a dyn Allocator, string: &str) {
        self.vec.extend_from_slice(alloc, string.as_bytes());
    }

    /// Appends a UTF-16 slice, replacing unpaired surrogates with U+FFFD.
    pub fn push_utf16_lossy(&mut self, alloc: &'a dyn Allocator, string: &[u16]) {
        self.extend(
            alloc,
            char::decode_utf16(string.iter().cloned())
                .map(|r| r.unwrap_or(char::REPLACEMENT_CHARACTER)),
        );
    }

    /// Same as `push(char)` but with a specified number of character copies.
    /// Shockingly absent from the standard library.
    pub fn push_repeat(&mut self, alloc: &'a dyn Allocator, ch: char, total_copies: usize) {
        if total_copies == 0 {
            return;
        }

        let buf = unsafe { self.as_mut_vec() };

        if ch.is_ascii() {
            // Compiles down to `memset()`.
            buf.push_repeat(alloc, ch as u8, total_copies);
        } else {
            // Implements efficient string padding using quadratic duplication.
            let mut utf8_buf = [0; 4];
            let utf8 = ch.encode_utf8(&mut utf8_buf).as_bytes();
            let initial_len = buf.len();
            let added_len = utf8.len() * total_copies;
            let final_len = initial_len + added_len;

            buf.reserve(alloc, added_len);
            buf.extend_from_slice(alloc, utf8);

            while buf.len() != final_len {
                let end = (final_len - buf.len() + initial_len).min(buf.len());
                buf.extend_from_within(alloc, initial_len..end);
            }
        }
    }

    /// Appends each `char` from the iterator.
    pub fn extend<I>(&mut self, alloc: &'a dyn Allocator, iter: I)
    where
        I: IntoIterator<Item = char>,
    {
        let iterator = iter.into_iter();
        let (lower_bound, _) = iterator.size_hint();
        self.reserve(alloc, lower_bound);
        iterator.for_each(move |c| self.push(alloc, c));
    }

    /// Replaces a range of characters with a new string.
    pub fn replace_range<R: RangeBounds<usize>>(
        &mut self,
        alloc: &'a dyn Allocator,
        range: R,
        replace_with: &str,
    ) {
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
        unsafe { self.as_mut_vec() }.replace_range(alloc, range, replace_with.as_bytes());
    }

    /// Finds `old` in the string and replaces it with `new`.
    /// Only performs one replacement.
    pub fn replace_once_in_place(&mut self, alloc: &'a dyn Allocator, old: &str, new: &str) {
        if let Some(beg) = self.find(old) {
            unsafe { self.as_mut_vec().replace_range(alloc, beg..beg + old.len(), new.as_bytes()) };
        }
    }
}

impl Default for BString<'_> {
    fn default() -> Self {
        Self::empty()
    }
}

impl Deref for BString<'_> {
    type Target = str;

    #[inline]
    fn deref(&self) -> &str {
        self.as_str()
    }
}

impl DerefMut for BString<'_> {
    #[inline]
    fn deref_mut(&mut self) -> &mut str {
        self.as_mut_str()
    }
}

impl PartialEq<BString<'_>> for BString<'_> {
    #[inline]
    fn eq(&self, other: &BString) -> bool {
        self.as_str() == other.as_str()
    }
}

impl Eq for BString<'_> {}

impl PartialEq<&str> for BString<'_> {
    #[inline]
    fn eq(&self, other: &&str) -> bool {
        self.as_str() == *other
    }
}

impl PartialOrd for BString<'_> {
    #[inline]
    fn partial_cmp(&self, other: &BString) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for BString<'_> {
    #[inline]
    fn cmp(&self, other: &BString) -> std::cmp::Ordering {
        self.as_str().cmp(other.as_str())
    }
}

impl fmt::Debug for BString<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self.as_str(), f)
    }
}

impl fmt::Display for BString<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self.as_str(), f)
    }
}

/// Pairs a [`BString`] with an allocator so you can use `write!` on it.
// NOTE: This struct uses a generic allocator, because I found that it shrinks the binary by 3KB somehow.
// I never investigated why that is, or what the impact of that is, but it can't be good.
// It does kind of make sense though, since this struct is generally temporary only.
pub struct BStringFormatter<'s, 'a, A> {
    string: &'s mut BString<'a>,
    alloc: &'a A,
}

impl<A> fmt::Write for BStringFormatter<'_, '_, A>
where
    A: Allocator,
{
    #[inline]
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.string.push_str(self.alloc, s);
        Ok(())
    }

    #[inline]
    fn write_char(&mut self, c: char) -> fmt::Result {
        self.string.push(self.alloc, c);
        Ok(())
    }
}
