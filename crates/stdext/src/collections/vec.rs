// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::iter::FusedIterator;
use std::marker::PhantomData;
use std::mem::MaybeUninit;
use std::ops::{Bound, Deref, DerefMut, Range, RangeBounds};
use std::ptr::{self, NonNull};
use std::{fmt, slice};

use crate::alloc::Allocator;
#[cfg(debug_assertions)]
use crate::alloc::GlobalAllocator;
use crate::simd::memset;

/// Similar to slices in Go, this slice has an additional capacity field.
/// It allows you to push more elements into the slice beyond its length,
/// up to the capacity. Like a `Vec` but on borrowed memory.
///
/// # Safety
///
/// The struct does not drop the elements, nor does it deallocate any memory.
pub struct BVec<'a, T> {
    // NOTE: Only the first `len` elemennts are `T`, the rest are essentially `MaybeUninit<T>`.
    // This is an important distinction, due to Rust's highly nebulous rules around uninitialized memory.
    // You should avoid `self.ptr.as_ptr().add(self.len)` and use `self.spare_mut_ptr()` instead.
    ptr: NonNull<T>,
    len: usize,
    cap: usize,
    _marker: PhantomData<&'a T>,
    #[cfg(debug_assertions)]
    alloc: Option<&'a dyn Allocator>,
}

impl<'a, T> BVec<'a, T> {
    /// The label on the tin says "empty". You open it. It's empty.
    #[inline]
    pub const fn empty() -> Self {
        Self {
            ptr: NonNull::dangling(),
            len: 0,
            cap: 0,
            _marker: PhantomData,
            #[cfg(debug_assertions)]
            alloc: None,
        }
    }

    pub fn from_slice(slice: &'a mut [T]) -> Self {
        let slice = NonNull::from_mut(slice);
        Self {
            ptr: slice.cast(),
            len: slice.len(),
            cap: slice.len(),
            _marker: PhantomData,
            #[cfg(debug_assertions)]
            alloc: None,
        }
    }

    /// Leaks a `Vec` and turns it into a "borrowed" `BVec`.
    pub fn from_std_vec(vec: Vec<T>) -> Self {
        let (ptr, len, cap) = vec.into_raw_parts();
        // A `Vec` always has a non-null pointer (it's dangling).
        let ptr = unsafe { NonNull::new_unchecked(ptr) };
        Self {
            ptr,
            len,
            cap,
            _marker: PhantomData,
            #[cfg(debug_assertions)]
            alloc: Some(&GlobalAllocator),
        }
    }

    /// Under the assumption that your `BVec` uses `GlobalAlloc`,
    /// this turns it back into a standard `Vec`.
    ///
    /// It's not marked as `unsafe`, because people count the "unsafe" keyword as a measure of safety
    /// the way managers count lines of code to measure productivity. So, by not marking it as unsafe,
    /// I've effectively improved the security of this project. The "Real Men of Genius" ad plays in my head.
    ///
    /// In all seriousness though, there are debug runtime checks. That's sufficient for my purpose.
    pub fn into_std_vec(self) -> Vec<T> {
        #[cfg(debug_assertions)]
        debug_assert!(
            self.alloc.is_none_or(|a| std::ptr::eq(a, &GlobalAllocator)),
            "BVec can only be converted into Vec if it was allocated with GlobalAlloc"
        );

        unsafe { Vec::from_raw_parts(self.ptr.as_ptr(), self.len, self.cap) }
    }

    /// Number of initialized elements.
    #[inline]
    pub fn len(&self) -> usize {
        self.len
    }

    /// Total number of elements the buffer can hold.
    #[inline]
    pub fn capacity(&self) -> usize {
        self.cap
    }

    /// True if there are zero elements.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// True if if the buffer is full.
    #[inline]
    pub fn is_full(&self) -> bool {
        self.len == self.cap
    }

    /// Forcibly sets the length.
    ///
    /// # Safety
    ///
    /// The first `new_len` items must be initialized.
    /// Items beyond `new_len` are not dropped when you call `set_len()`.
    #[inline]
    pub unsafe fn set_len(&mut self, new_len: usize) {
        debug_assert!(new_len <= self.cap);
        self.len = new_len;
    }

    /// Shortens the vector.
    pub fn truncate(&mut self, len: usize) {
        unsafe {
            // NOTE: It's intentional that this doesn't avoid drops when `len == self.len`,
            // because that would introduce a branch for the common case of `truncate(0)`.
            if let Some(r) = self.len.checked_sub(len) {
                let s = ptr::slice_from_raw_parts_mut(self.as_mut_ptr().add(len), r);
                self.len = len;
                ptr::drop_in_place(s);
            }
        }
    }

    /// Raw pointer to the backing buffer.
    #[inline]
    pub fn as_ptr(&self) -> *const T {
        self.ptr.as_ptr()
    }

    /// Mutable raw pointer to the backing buffer.
    #[inline]
    pub fn as_mut_ptr(&mut self) -> *mut T {
        self.ptr.as_ptr()
    }

    #[inline]
    fn spare_mut_ptr(&mut self) -> *mut MaybeUninit<T> {
        unsafe { (self.ptr.as_ptr() as *mut MaybeUninit<T>).add(self.len) }
    }

    /// View as a shared slice.
    #[inline]
    pub fn as_slice(&self) -> &[T] {
        self
    }

    /// View as a mutable slice.
    #[inline]
    pub fn as_mut_slice(&mut self) -> &mut [T] {
        self
    }

    /// Consume the string, returning a `&mut [T]` that lives as long as the borrowed memory.
    #[inline]
    pub fn leak(self) -> &'a mut [T] {
        unsafe { slice::from_raw_parts_mut(self.ptr.as_ptr(), self.len) }
    }

    /// Drops all elements and resets length to zero. The allocation is kept.
    #[inline]
    pub fn clear(&mut self) {
        let elems = self.as_mut_slice() as *mut _;
        self.len = 0;
        unsafe { ptr::drop_in_place(elems) };
    }

    /// Ensures space for at least `additional` more elements, with amortized growth.
    #[inline]
    pub fn reserve(&mut self, alloc: &'a dyn Allocator, additional: usize) {
        if additional > self.cap - self.len {
            self.grow(alloc, self.cap, additional);
        }
    }

    /// Ensures space for at least `additional` more elements, without over-allocating.
    #[inline]
    pub fn reserve_exact(&mut self, alloc: &'a dyn Allocator, additional: usize) {
        if additional > self.cap - self.len {
            self.grow(alloc, 0, additional);
        }
    }

    #[inline]
    fn reserve_one(&mut self, alloc: &'a dyn Allocator) {
        if self.is_full() {
            self.grow(alloc, self.cap, 1);
        }
    }

    // NOTE: I'm using dyn(amic dispatch) to avoid monomorphization bloat and more
    // importantly because I counter-intuitively found it to boost performance by +20%.
    #[cold]
    fn grow(&mut self, alloc: &'a dyn Allocator, cap: usize, add: usize) {
        debug_assert!(add > 0, "growing by zero makes no sense");

        #[cfg(debug_assertions)]
        debug_assert!(
            self.alloc.is_none_or(|a| std::ptr::eq(a, alloc)),
            "switching between allocators on a single BVec heavily suggests you're about to leak memory"
        );

        let new_cap = (cap * 2).max(self.len + add).max(8);
        let new_ptr = unsafe {
            alloc.realloc(
                self.ptr.cast(),
                self.cap * size_of::<T>(),
                new_cap * size_of::<T>(),
                align_of::<T>(),
            )
        };
        self.ptr = new_ptr.cast();
        self.cap = new_ptr.len() / size_of::<T>();
    }

    /// Returns the uninitialized tail of the buffer. Fill it, then `set_len()`.
    pub fn spare_capacity_mut(&mut self) -> &mut [MaybeUninit<T>] {
        unsafe { slice::from_raw_parts_mut(self.spare_mut_ptr(), self.cap - self.len) }
    }

    /// Appends one element, returning a mutable reference to it.
    pub fn push(&mut self, alloc: &'a dyn Allocator, value: T) -> &mut T {
        self.reserve_one(alloc);

        unsafe {
            let dst = self.spare_mut_ptr();
            self.len += 1;
            (*dst).write(value)
        }
    }

    /// Append the items from the iterator `iter`.
    ///
    /// By assuming that your "exact size iterator" returns an *exact* size,
    /// it can preallocate the memory in one go and efficiently push items.
    pub fn extend<I>(&mut self, alloc: &'a dyn Allocator, iter: I)
    where
        I: IntoIterator<Item = T> + ExactSizeIterator<Item = T>,
    {
        let len = iter.len();
        self.reserve(alloc, len);

        unsafe {
            let mut dst = self.spare_mut_ptr();
            self.len += len;
            for value in iter {
                (*dst).write(value);
                dst = dst.add(1);
            }
        }
    }

    /// This is the bad path of `extend()`. It has a distinct name, because it makes
    /// it easy to find. If you use this method, you're not writing ideal code.
    pub fn extend_sloppy<I>(&mut self, alloc: &'a dyn Allocator, iter: I)
    where
        I: IntoIterator<Item = T>,
    {
        let iterator = iter.into_iter();
        let (lower_bound, _) = iterator.size_hint();
        self.reserve(alloc, lower_bound);
        iterator.for_each(move |c| _ = self.push(alloc, c));
    }
}

impl<'a, T: Copy> BVec<'a, T> {
    /// Pushes `total_copies` copies of `value`. It's basically `memset`.
    pub fn push_repeat(&mut self, alloc: &'a dyn Allocator, value: T, total_copies: usize) {
        if total_copies == 0 {
            return;
        }

        self.reserve(alloc, total_copies);

        unsafe {
            let dst = slice::from_raw_parts_mut(self.spare_mut_ptr(), total_copies);
            self.len += total_copies; // Increment first, to turn memset() into a tail call
            memset(dst, MaybeUninit::new(value));
        }
    }

    /// Appends all elements from a slice. It's basically a `memcpy`-append.
    #[allow(clippy::mut_from_ref)]
    pub fn extend_from_slice(&mut self, alloc: &'a dyn Allocator, other: &[T]) {
        let add = other.len();
        self.reserve(alloc, add);

        unsafe {
            let dst = self.spare_mut_ptr();
            self.len += add;
            ptr::copy_nonoverlapping(other.as_ptr() as *const _, dst, add);
        }
    }

    /// [`Self::extend_from_slice`] but for a subslice of the buffer itself.
    #[inline]
    pub fn extend_from_within<R>(&mut self, alloc: &'a dyn Allocator, src: R)
    where
        R: RangeBounds<usize>,
    {
        let start = match src.start_bound() {
            Bound::Included(&start) => start,
            Bound::Excluded(start) => start + 1,
            Bound::Unbounded => 0,
        };
        let end = match src.end_bound() {
            Bound::Included(end) => end + 1,
            Bound::Excluded(&end) => end,
            Bound::Unbounded => usize::MAX,
        };
        self.extend_from_within_impl(alloc, start..end);
    }

    fn extend_from_within_impl(&mut self, alloc: &'a dyn Allocator, src: Range<usize>) {
        let end = src.end.min(self.len);
        let beg = src.start.min(end);
        let add = end - beg;

        self.reserve(alloc, add);

        unsafe {
            let dst = self.spare_mut_ptr();
            let src = self.ptr.as_ptr().add(beg);
            self.len += add;
            ptr::copy_nonoverlapping(src as *const _, dst, add);
        }
    }

    /// Replaces the given range with elements from `src`. Efficient `splice` for `Copy` types.
    #[inline]
    pub fn replace_range<R>(&mut self, alloc: &'a dyn Allocator, range: R, src: &[T])
    where
        R: RangeBounds<usize>,
    {
        let start = match range.start_bound() {
            Bound::Included(&start) => start,
            Bound::Excluded(start) => start + 1,
            Bound::Unbounded => 0,
        };
        let end = match range.end_bound() {
            Bound::Included(end) => end + 1,
            Bound::Excluded(&end) => end,
            Bound::Unbounded => usize::MAX,
        };
        self.replace_range_impl(alloc, start..end, src);
    }

    // At the time of writing, this implementation of what's
    // essentially `Vec::splice` is vastly more efficient.
    fn replace_range_impl(&mut self, alloc: &'a dyn Allocator, range: Range<usize>, src: &[T]) {
        unsafe {
            let dst_len = self.len();
            let src_len = src.len();
            let off = range.start.min(dst_len);
            let del_len = range.end.saturating_sub(off).min(dst_len - off);

            if del_len == 0 && src_len == 0 {
                return; // nothing to do
            }

            let tail_len = dst_len - off - del_len;
            let new_len = dst_len - del_len + src_len;

            if src_len > del_len {
                self.reserve(alloc, src_len - del_len);
            }

            // NOTE: drop_in_place() is not needed here, because T is constrained to Copy.

            // SAFETY: as_mut_ptr() must called after reserve() to ensure that the pointer is valid.
            let ptr = self.as_mut_ptr().add(off);

            // Shift the tail.
            if tail_len > 0 && src_len != del_len {
                ptr::copy(ptr.add(del_len), ptr.add(src_len), tail_len);
            }

            // Copy in the replacement.
            ptr::copy_nonoverlapping(src.as_ptr(), ptr, src_len);
            self.set_len(new_len);
        }
    }
}

unsafe extern "system" {
    fn MultiByteToWideChar(
        CodePage: u32,
        dwFlags: u32,
        lpMultiByteStr: *const u8,
        cbMultiByte: i32,
        lpWideCharStr: *mut u16,
        cchWideChar: i32,
    ) -> i32;
}

impl<'a> BVec<'a, u16> {
    pub fn push_encode_utf16(&mut self, alloc: &'a dyn Allocator, utf8: &[u8]) {
        unsafe {
            self.reserve(alloc, utf8.len()); // worst case ASCII: 1 byte per char
            let dst = self.spare_mut_ptr() as *mut u16;
            let len = MultiByteToWideChar(
                65001,
                0,
                utf8.as_ptr(),
                utf8.len() as i32,
                dst,
                utf8.len() as i32,
            );
            self.len += len.max(0) as usize;
        }
    }
}

impl<T> Default for BVec<'_, T> {
    fn default() -> Self {
        Self::empty()
    }
}

impl<T> Deref for BVec<'_, T> {
    type Target = [T];

    #[inline]
    fn deref(&self) -> &[T] {
        unsafe { slice::from_raw_parts(self.ptr.as_ptr(), self.len) }
    }
}

impl<T> DerefMut for BVec<'_, T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut [T] {
        unsafe { slice::from_raw_parts_mut(self.ptr.as_ptr(), self.len) }
    }
}

impl<T> PartialEq<BVec<'_, T>> for BVec<'_, T>
where
    T: PartialEq,
{
    #[inline]
    fn eq(&self, other: &BVec<T>) -> bool {
        self.deref() == other.deref()
    }
}

impl<T> Eq for BVec<'_, T> where T: PartialEq {}

impl<T> PartialEq<[T]> for BVec<'_, T>
where
    T: PartialEq,
{
    #[inline]
    fn eq(&self, other: &[T]) -> bool {
        self.deref() == other
    }
}

impl<T> PartialOrd for BVec<'_, T>
where
    T: PartialOrd,
{
    #[inline]
    fn partial_cmp(&self, other: &BVec<T>) -> Option<std::cmp::Ordering> {
        self.deref().partial_cmp(other.deref())
    }
}

impl<T> Ord for BVec<'_, T>
where
    T: Ord,
{
    #[inline]
    fn cmp(&self, other: &BVec<T>) -> std::cmp::Ordering {
        self.deref().cmp(other.deref())
    }
}

impl<T> fmt::Debug for BVec<'_, T>
where
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self.deref(), f)
    }
}

impl<'a, T> IntoIterator for BVec<'a, T> {
    type Item = T;
    type IntoIter = IntoIter<'a, T>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        unsafe {
            let ptr = self.ptr;
            let end = ptr.add(self.len);
            IntoIter { ptr, end, phantom: PhantomData }
        }
    }
}

impl<'a, T> IntoIterator for &'a BVec<'a, T> {
    type Item = &'a T;
    type IntoIter = slice::Iter<'a, T>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, T> IntoIterator for &'a mut BVec<'a, T> {
    type Item = &'a mut T;
    type IntoIter = slice::IterMut<'a, T>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

/// Owning iterator over the elements of a [`BVec`].
pub struct IntoIter<'a, T> {
    ptr: NonNull<T>,
    end: NonNull<T>,
    phantom: PhantomData<&'a T>,
}

impl<'a, T> Iterator for IntoIter<'a, T> {
    type Item = T;

    #[inline]
    fn next(&mut self) -> Option<T> {
        if self.ptr == self.end {
            return None;
        }
        let ptr = self.ptr;
        self.ptr = unsafe { ptr.add(1) };
        Some(unsafe { ptr.read() })
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.len();
        (len, Some(len))
    }

    #[inline]
    fn count(self) -> usize {
        self.len()
    }

    #[inline]
    fn last(mut self) -> Option<T> {
        self.next_back()
    }

    #[inline]
    fn nth(&mut self, n: usize) -> Option<Self::Item> {
        if n >= self.len() {
            self.ptr = self.end;
            return None;
        }
        let ptr = self.ptr;
        self.ptr = unsafe { ptr.add(n + 1) };
        Some(unsafe { ptr.read() })
    }

    fn fold<B, F>(mut self, mut accum: B, mut f: F) -> B
    where
        F: FnMut(B, Self::Item) -> B,
    {
        while self.ptr != self.end {
            let ptr = self.ptr;
            self.ptr = unsafe { ptr.add(1) };
            accum = f(accum, unsafe { self.ptr.read() });
        }
        accum
    }
}

impl<'a, T> DoubleEndedIterator for IntoIter<'a, T> {
    #[inline]
    fn next_back(&mut self) -> Option<T> {
        if self.ptr == self.end {
            return None;
        }
        unsafe {
            self.end = self.end.sub(1);
            Some(self.end.read())
        }
    }
}

impl<'a, T> ExactSizeIterator for IntoIter<'a, T> {
    #[inline]
    fn len(&self) -> usize {
        unsafe { self.end.offset_from_unsigned(self.ptr) }
    }
}

impl<'a, T> FusedIterator for IntoIter<'a, T> {}
