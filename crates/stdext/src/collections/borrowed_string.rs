use std::fmt::{self};
use std::ops::{Deref, DerefMut};
use std::slice;
use std::str::Utf8Error;

use super::BorrowedVec;
use crate::collections::Allocator;

/// Like a `String` but on borrowed memory. Built on top of [`BorrowedVec<u8>`].
pub struct BorrowedString<'a> {
    vec: BorrowedVec<'a, u8>,
}

impl<'a> BorrowedString<'a> {
    /// The label on the tin says "empty". You open it. It's empty.
    #[inline]
    pub const fn empty() -> Self {
        Self { vec: BorrowedVec::empty() }
    }

    /// Validates and wraps a byte vec as UTF-8.
    pub fn from_utf8(vec: BorrowedVec<'a, u8>) -> Result<Self, Utf8Error> {
        str::from_utf8(&vec)?;
        Ok(Self { vec })
    }

    /// Wraps a byte vec as UTF-8 without validating it.
    ///
    /// # Safety
    ///
    /// The bytes in `vec` must be valid UTF-8.
    #[inline]
    pub unsafe fn from_utf8_unchecked(vec: BorrowedVec<'a, u8>) -> Self {
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
    pub unsafe fn as_mut_vec(&mut self) -> &mut BorrowedVec<'a, u8> {
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
    pub fn formatter<A: Allocator>(&mut self, alloc: &'a A) -> BorrowedStringFormatter<'_, 'a, A> {
        BorrowedStringFormatter { string: self, alloc }
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

    /// Finds `old` in the string and replaces it with `new`.
    /// Only performs one replacement.
    pub fn replace_once_in_place(&mut self, alloc: &'a dyn Allocator, old: &str, new: &str) {
        if let Some(beg) = self.find(old) {
            unsafe { self.as_mut_vec().replace_range(alloc, beg..beg + old.len(), new.as_bytes()) };
        }
    }
}

impl Default for BorrowedString<'_> {
    fn default() -> Self {
        Self::empty()
    }
}

impl Deref for BorrowedString<'_> {
    type Target = str;

    #[inline]
    fn deref(&self) -> &str {
        self.as_str()
    }
}

impl DerefMut for BorrowedString<'_> {
    #[inline]
    fn deref_mut(&mut self) -> &mut str {
        self.as_mut_str()
    }
}

impl PartialEq<BorrowedString<'_>> for BorrowedString<'_> {
    #[inline]
    fn eq(&self, other: &BorrowedString) -> bool {
        self.as_str() == other.as_str()
    }
}

impl Eq for BorrowedString<'_> {}

impl PartialEq<&str> for BorrowedString<'_> {
    #[inline]
    fn eq(&self, other: &&str) -> bool {
        self.as_str() == *other
    }
}

impl fmt::Debug for BorrowedString<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self.as_str(), f)
    }
}

impl fmt::Display for BorrowedString<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self.as_str(), f)
    }
}

/// Pairs a [`BorrowedString`] with an allocator so you can use `write!` on it.
// NOTE: This struct uses a generic allocator, because I found that it shrinks the binary by 3KB somehow.
// I never investigated why that is, or what the impact of that is, but it can't be good.
// It does kind of make sense though, since this struct is generally temporary only.
pub struct BorrowedStringFormatter<'s, 'a, A: Allocator> {
    string: &'s mut BorrowedString<'a>,
    alloc: &'a A,
}

impl<A: Allocator> fmt::Write for BorrowedStringFormatter<'_, '_, A> {
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
