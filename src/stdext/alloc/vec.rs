use std::marker::PhantomData;
use std::mem::{self, MaybeUninit, forget};
use std::ops::{Bound, Deref, DerefMut, Range, RangeBounds};
use std::ptr::{self, NonNull};
use std::slice;

use crate::stdext::alloc::{ExAllocator, ExStdAlloc};

/// [`Vec<T>`] with allocator support.
pub struct ExVec<T, A: ExAllocator = ExStdAlloc> {
    ptr: NonNull<T>,
    cap: usize,
    len: usize,

    alloc: A,
    _marker: PhantomData<T>,
}

impl<T, A: ExAllocator> ExVec<T, A> {
    pub const fn new(alloc: A) -> Self {
        ExVec { ptr: NonNull::dangling(), cap: 0, len: 0, alloc, _marker: PhantomData }
    }

    pub fn with_capacity(alloc: A, capacity: usize) -> Self {
        let mut s = Self::new(alloc);
        s.reserve_exact(capacity);
        s
    }

    #[inline]
    pub const fn allocator(&self) -> &A {
        &self.alloc
    }

    #[inline]
    pub const fn capacity(&self) -> usize {
        self.cap
    }

    #[inline]
    pub const fn len(&self) -> usize {
        self.len
    }

    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn as_mut_ptr(&mut self) -> *mut T {
        self.ptr.as_ptr()
    }

    pub const fn as_slice(&self) -> &[T] {
        unsafe { slice::from_raw_parts(self.ptr.as_ptr(), self.len) }
    }

    pub const fn as_mut_slice(&mut self) -> &mut [T] {
        unsafe { slice::from_raw_parts_mut(self.ptr.as_ptr(), self.len) }
    }

    pub unsafe fn set_len(&mut self, new_len: usize) {
        debug_assert!(new_len <= self.cap);
        self.len = new_len;
    }

    pub fn spare_capacity_mut(&mut self) -> &mut [MaybeUninit<T>] {
        unsafe {
            slice::from_raw_parts_mut(
                self.as_mut_ptr().add(self.len) as *mut MaybeUninit<T>,
                self.cap - self.len,
            )
        }
    }

    pub fn leak(self) -> &'static mut [T] {
        let slice = unsafe { slice::from_raw_parts_mut(self.ptr.as_ptr(), self.len) };
        forget(self);
        slice
    }

    pub fn reserve(&mut self, additional: usize) {
        self.reserve_impl(additional, false);
    }

    pub fn reserve_exact(&mut self, additional: usize) {
        self.reserve_impl(additional, true);
    }

    fn reserve_impl(&mut self, additional: usize, exact: bool) {
        if additional == 0 {
            return;
        }

        let new_cap = if exact { self.cap } else { self.cap * 2 };
        let new_cap = new_cap.max(self.len + additional);

        if new_cap > self.cap {
            let new_ptr = self
                .alloc
                .allocate(new_cap * std::mem::size_of::<T>(), std::mem::align_of::<T>())
                .unwrap()
                .cast();

            unsafe {
                ptr::copy_nonoverlapping(self.ptr.as_ptr(), new_ptr.as_ptr(), self.len);
                if self.cap != 0 {
                    self.alloc.deallocate(
                        self.ptr.cast(),
                        self.cap * std::mem::size_of::<T>(),
                        std::mem::align_of::<T>(),
                    );
                }
            }

            self.ptr = new_ptr;
            self.cap = new_cap;
        }
    }

    pub fn shrink_to_fit(&mut self) {
        todo!()
    }

    pub fn clear(&mut self) {
        todo!()
    }

    pub fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = T>,
        T: Clone,
    {
        let mut iter = iter.into_iter();
        if let Some(first) = iter.next() {
            self.push(first);
            for item in iter {
                self.push(item);
            }
        }
    }

    pub fn extend_from_within<R: RangeBounds<usize>>(&mut self, range: R)
    where
        T: Clone,
    {
        let start = match range.start_bound() {
            Bound::Included(&start) => start,
            Bound::Excluded(start) => start + 1,
            Bound::Unbounded => 0,
        };
        let end = match range.end_bound() {
            Bound::Included(end) => end + 1,
            Bound::Excluded(&end) => end,
            Bound::Unbounded => self.len,
        };
        if start < end && end <= self.len {
            todo!()
        }
    }

    pub fn push(&mut self, value: T) -> &mut T {
        if self.len == self.cap {
            self.reserve(1);
        }
        unsafe {
            let ptr = self.as_mut_ptr().add(self.len);
            ptr::write(ptr, value);
            self.set_len(self.len + 1);
            &mut *ptr
        }
    }

    pub fn push_repeat(&mut self, count: usize, value: T)
    where
        T: Copy,
    {
        todo!()
    }

    pub fn extend_from_slice(&mut self, src: &[T])
    where
        T: Clone,
    {
        todo!()
    }
}

impl<T: Copy, A: ExAllocator> ExVec<T, A> {
    pub fn replace_range<R: RangeBounds<usize>>(&mut self, range: R, src: &[T]) {
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
        self.replace_impl(start..end, src);
    }

    pub fn replace_impl(&mut self, range: Range<usize>, src: &[T]) {
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
                self.reserve(src_len - del_len);
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

impl<T, A: ExAllocator> Drop for ExVec<T, A> {
    fn drop(&mut self) {
        if self.cap != 0 {
            unsafe {
                self.alloc.deallocate(
                    self.ptr.cast(),
                    self.cap * std::mem::size_of::<T>(),
                    std::mem::align_of::<T>(),
                )
            };
        }
    }
}

impl<T> Default for ExVec<T> {
    fn default() -> Self {
        Self::new(ExStdAlloc)
    }
}

impl<T, A: ExAllocator> Deref for ExVec<T, A> {
    type Target = [T];

    fn deref(&self) -> &[T] {
        unsafe { std::slice::from_raw_parts(self.ptr.as_ptr(), self.len) }
    }
}

impl<T, A: ExAllocator> DerefMut for ExVec<T, A> {
    fn deref_mut(&mut self) -> &mut [T] {
        unsafe { std::slice::from_raw_parts_mut(self.ptr.as_ptr(), self.len) }
    }
}

impl<T: std::fmt::Debug, A: ExAllocator> std::fmt::Debug for ExVec<T, A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.deref().fmt(f)
    }
}

impl<T: PartialEq, A: ExAllocator> PartialEq for ExVec<T, A> {
    fn eq(&self, other: &Self) -> bool {
        self.deref().eq(other.deref())
    }
}

impl<T: Eq, A: ExAllocator> Eq for ExVec<T, A> {}

impl<T, A: ExAllocator + Clone> Clone for ExVec<T, A> {
    fn clone(&self) -> Self {
        let mut new_vec = Self::new(self.alloc.clone());
        new_vec.reserve(self.len);
        unsafe {
            ptr::copy_nonoverlapping(self.ptr.as_ptr(), new_vec.as_mut_ptr(), self.len);
            new_vec.set_len(self.len);
        }
        new_vec
    }
}

impl<'a, T, A: ExAllocator> IntoIterator for &'a ExVec<T, A> {
    type Item = &'a T;
    type IntoIter = std::slice::Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, T, A: ExAllocator> IntoIterator for &'a mut ExVec<T, A> {
    type Item = &'a mut T;
    type IntoIter = std::slice::IterMut<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

impl<T, A: ExAllocator> IntoIterator for ExVec<T, A> {
    type Item = T;
    type IntoIter = std::vec::IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        // SAFETY: We own the buffer, so we can move out the elements.
        let slice = self.as_slice();
        let mut v = Vec::with_capacity(slice.len());
        for item in slice {
            // SAFETY: ptr::read moves out the value without dropping it.
            v.push(unsafe { std::ptr::read(item) });
        }
        // Prevent double-drop
        std::mem::forget(self);
        v.into_iter()
    }
}
