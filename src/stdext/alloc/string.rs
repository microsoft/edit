use std::fmt;
use std::ops::{Bound, Deref, DerefMut, RangeBounds};

use crate::stdext::*;

pub struct ExString<A: ExAllocator = ExStdAlloc> {
    vec: ExVec<u8, A>,
}

impl<A: ExAllocator> ExString<A> {
    /// Creates a new [`ExString`] in the given arena.
    #[must_use]
    pub fn new(alloc: A) -> Self {
        Self { vec: ExVec::new(alloc) }
    }

    #[must_use]
    pub fn with_capacity(alloc: A, capacity: usize) -> Self {
        Self { vec: ExVec::with_capacity(alloc, capacity) }
    }

    /// Turns a [`str`] into an [`ExString`].
    #[must_use]
    pub fn from_str(alloc: A, s: &str) -> Self {
        let mut res = Self::new(alloc);
        res.push_str(s);
        res
    }

    pub fn from_utf8(bytes: ExVec<u8, A>) -> Result<Self, std::string::FromUtf8Error> {
        Ok(Self { vec: bytes })
    }

    /// It says right here that you checked if `bytes` is valid UTF-8
    /// and you are sure it is. Presto! Here's an `ExString`!
    ///
    /// # Safety
    ///
    /// You fool! It says "unchecked" right there. Now the house is burning.
    #[inline]
    #[must_use]
    pub unsafe fn from_utf8_unchecked(bytes: ExVec<u8, A>) -> Self {
        Self { vec: bytes }
    }

    /// Checks whether `text` contains only valid UTF-8.
    /// If the entire string is valid, it returns `Ok(text)`.
    /// Otherwise, it returns `Err(ExString)` with all invalid sequences replaced with U+FFFD.
    pub fn from_utf8_lossy(alloc: A, text: &[u8]) -> Result<&str, Self> {
        let mut iter = text.utf8_chunks();
        let Some(mut chunk) = iter.next() else {
            return Ok("");
        };

        let valid = chunk.valid();
        if chunk.invalid().is_empty() {
            debug_assert_eq!(valid.len(), text.len());
            return Ok(unsafe { str::from_utf8_unchecked(text) });
        }

        const REPLACEMENT: &str = "\u{FFFD}";

        let mut res = Self::new(alloc);
        res.reserve(text.len());

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

    /// It's empty.
    pub fn is_empty(&self) -> bool {
        self.vec.is_empty()
    }

    /// It's lengthy.
    pub fn len(&self) -> usize {
        self.vec.len()
    }

    /// It's capacatity.
    pub fn capacity(&self) -> usize {
        self.vec.capacity()
    }

    /// It's a [`ExString`], now it's a [`str`]. Wow!
    pub fn as_str(&self) -> &str {
        unsafe { str::from_utf8_unchecked(self.vec.as_slice()) }
    }

    /// It's a [`ExString`], now it's a [`str`]. And it's mutable! WOW!
    pub fn as_mut_str(&mut self) -> &mut str {
        unsafe { str::from_utf8_unchecked_mut(self.vec.as_mut_slice()) }
    }

    /// Now it's bytes!
    pub fn as_bytes(&self) -> &[u8] {
        self.vec.as_slice()
    }

    /// Returns a mutable reference to the contents of this `ExString`.
    ///
    /// # Safety
    ///
    /// The underlying `&mut Vec` allows writing bytes which are not valid UTF-8.
    pub unsafe fn as_mut_vec(&mut self) -> &mut ExVec<u8, A> {
        &mut self.vec
    }

    /// Reserves *additional* memory. For you old folks out there (totally not me),
    /// this is different from C++'s `reserve` which reserves a total size.
    pub fn reserve(&mut self, additional: usize) {
        self.vec.reserve(additional)
    }

    /// Just like [`ExString::reserve`], but it doesn't overallocate.
    pub fn reserve_exact(&mut self, additional: usize) {
        self.vec.reserve_exact(additional)
    }

    /// Now it's small! Alarming!
    ///
    /// *Do not* call this unless this string is the last thing on the arena.
    /// Arenas are stacks, they can't deallocate what's in the middle.
    pub fn shrink_to_fit(&mut self) {
        self.vec.shrink_to_fit()
    }

    /// To no surprise, this clears the string.
    pub fn clear(&mut self) {
        self.vec.clear()
    }

    /// Append some text.
    pub fn push_str(&mut self, string: &str) {
        self.vec.extend_from_slice(string.as_bytes())
    }

    /// Append a single character.
    #[inline]
    pub fn push(&mut self, ch: char) {
        match ch.len_utf8() {
            1 => _ = self.vec.push(ch as u8),
            _ => self.vec.extend_from_slice(ch.encode_utf8(&mut [0; 4]).as_bytes()),
        }
    }

    /// Same as `push(char)` but with a specified number of character copies.
    /// Shockingly absent from the standard library.
    pub fn push_repeat(&mut self, total_copies: usize, ch: char) {
        if total_copies == 0 {
            return;
        }

        let buf = unsafe { self.as_mut_vec() };

        if ch.is_ascii() {
            // Compiles down to `memset()`.
            buf.push_repeat(total_copies, ch as u8);
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

    /// Replaces a range of characters with a new string.
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
        unsafe { self.as_mut_vec() }.replace_range(range, replace_with.as_bytes());
    }

    /// Finds `old` in the string and replaces it with `new`.
    /// Only performs one replacement.
    pub fn replace_once_in_place(&mut self, old: &str, new: &str) {
        if let Some(beg) = self.find(old) {
            unsafe { self.as_mut_vec() }.replace_range(beg..beg + old.len(), new.as_bytes());
        }
    }
}

impl<A: ExAllocator> Deref for ExString<A> {
    type Target = str;

    fn deref(&self) -> &str {
        self.as_str()
    }
}

impl<A: ExAllocator> DerefMut for ExString<A> {
    fn deref_mut(&mut self) -> &mut str {
        self.as_mut_str()
    }
}

impl<A: ExAllocator + Clone> Clone for ExString<A> {
    fn clone(&self) -> Self {
        Self { vec: self.vec.clone() }
    }
}

impl<A: ExAllocator + Default> Default for ExString<A> {
    fn default() -> Self {
        Self { vec: ExVec::new(A::default()) }
    }
}

impl<A: ExAllocator> PartialEq<&str> for ExString<A> {
    fn eq(&self, other: &&str) -> bool {
        self.as_str() == *other
    }
}

impl<A: ExAllocator> PartialEq<ExString<A>> for &str {
    fn eq(&self, other: &ExString<A>) -> bool {
        *self == other.as_str()
    }
}

impl<A: ExAllocator> PartialEq for ExString<A> {
    fn eq(&self, other: &Self) -> bool {
        self.deref().eq(other.deref())
    }
}

impl<A: ExAllocator> Eq for ExString<A> {}

impl<A: ExAllocator> fmt::Debug for ExString<A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_str().fmt(f)
    }
}

impl<A: ExAllocator> fmt::Display for ExString<A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

impl<A: ExAllocator> fmt::Write for ExString<A> {
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
macro_rules! exformat_in {
    ($arena:expr, $($arg:tt)*) => {{
        use std::fmt::Write as _;
        let mut output = $crate::stdext::ExString::new($arena);
        output.write_fmt(format_args!($($arg)*)).unwrap();
        output
    }}
}
