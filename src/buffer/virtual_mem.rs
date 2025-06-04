use std::ops::{self, Deref, DerefMut, Range, RangeBounds};
use std::ptr::{self, NonNull};
use std::slice;

use crate::{apperr, sys};

/// A smart pointer to a virtual memory allocation from [`VirtualAlloc`].
///
/// # Warning
///
/// Virtual memory is commited "as needed".
/// Attempting to access [uncommited] memory is a status access violation.
///
/// [VirtualAlloc]: https://learn.microsoft.com/en-us/windows/win32/api/memoryapi/nf-memoryapi-virtualalloc
/// [uncommited]: https://learn.microsoft.com/en-us/windows/win32/Memory/page-state
pub struct VirtualMemory {
    // TODO: Is it uniquely owned? I believe it is.
    /// A pointer to an owned virtual memory allocation.
    ptr: NonNull<u8>,
    /// The size of the underlying allocation.
    size: usize,
    // TODO: Do I need to track which portions of memory are uncommited?
    // This is inefficent because the gap buffer struct already holds this same value.
    /// The size of the allocation's commited memory.
    commited: usize,
}

impl VirtualMemory {
    /// Create a new block of virtual memory.
    pub unsafe fn new(size: usize) -> Self {
        // The align is one because `T` is a byte
        Self { ptr: unsafe { sys::virtual_reserve(size).unwrap() }, size, commited: 0 }
    }

    /// Commit another `size` bytes. Return a result indicating success.
    pub unsafe fn commit(&mut self, size: usize) -> apperr::Result<()> {
        // Check the range is contained within the allocation
        // TODO: this might be redundant since commit also checks if it's a legal commit
        // It also seems to substantially slow down the function so omit for now
        // assert!(
        //     self.size >= self.len() + size,
        //     "Index {} is greater than the allocation {}.",
        //     self.len() + size,
        //     self.size
        // );

        let out = unsafe { sys::virtual_commit(self.ptr.add(self.commited), size) };
        // Update commited
        self.commited += size;
        out
    }

    /// This method is based on [`slice::copy_within`](https://doc.rust-lang.org/src/core/slice/mod.rs.html#3792-3794).
    /// Copies elements from one part of the virtual memory block to another part of itself,
    /// using a memmove.
    ///
    /// - `src` is the range within `self` to copy from.
    /// - `dest` is the starting index of the range within `self` to copy to, which will have the same
    /// length as `src`.
    ///
    /// `src` and `dst` may overlap. The ends of the two ranges must be less than or equal to `self.len()`.
    ///
    ///
    /// # Panics
    ///
    /// - `src` or `dst..dst + src.len()` exceeds the end of the slice.
    /// - The end of `src` is before the start.
    pub fn copy_within<R: RangeBounds<usize>>(&mut self, src: R, dst: usize) {
        // This is all basically copied directly from `copy_within`
        // The main change I made was changing the bounds check from the len of the slice to the len of commited
        let Range { start: src_start, end: src_end } = range(src, ..self.len());
        let count = src_end - src_start;
        // TODO: Confirm using this slice len which is the commited portion of the VM is sound.
        assert!(dst <= self.len() - count, "dst is out of bounds");
        // SAFETY: the conditions for `ptr::copy` have all been checked above,
        // as have those for `ptr::add`.
        unsafe {
            // Derive both `src_ptr` and `dest_ptr` from the same loan
            let ptr = self.ptr.as_ptr();
            let src_ptr = ptr.add(src_start);
            let dst_ptr = ptr.add(dst);
            ptr::copy(src_ptr, dst_ptr, count);
        }
    }
}

// TODO: Is this necessary?
impl Deref for VirtualMemory {
    type Target = [u8];

    /// Returns a slice containing the commited portion of the virtual memory allocation.
    fn deref(&self) -> &Self::Target {
        unsafe { slice::from_raw_parts(self.ptr.as_ptr(), self.commited) }
    }
}

impl DerefMut for VirtualMemory {
    /// Returns a mutable slice containing the commited portion of the virtual memory allocation.
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { slice::from_raw_parts_mut(self.ptr.as_ptr(), self.commited) }
    }
}

unsafe impl Send for VirtualMemory {}
unsafe impl Sync for VirtualMemory {}

impl Drop for VirtualMemory {
    fn drop(&mut self) {
        unsafe { sys::virtual_release(self.ptr, self.size) };
    }
}

// TODO: This should probably end up in helpers in a final version
/// Implementation of [`slice::range`](https://doc.rust-lang.org/src/core/slice/index.rs.html#835-837). Exists because `slice::range` is unstable.
fn range<R: RangeBounds<usize>>(range: R, bounds: ops::RangeTo<usize>) -> Range<usize> {
    let len = bounds.end;

    // TODO: In a final version it may be better for this to error in a recoverable way
    let start = match range.start_bound() {
        ops::Bound::Included(&start) => start,
        ops::Bound::Excluded(start) => start
            .checked_add(1)
            .unwrap_or_else(|| panic!("attempted to index slice from after maximum usize")),
        ops::Bound::Unbounded => 0,
    };

    let end = match range.end_bound() {
        ops::Bound::Included(end) => end
            .checked_add(1)
            .unwrap_or_else(|| panic!("attempted to index slice up to maximum usize")),
        ops::Bound::Excluded(&end) => end,
        ops::Bound::Unbounded => len,
    };

    if start > end {
        // slice_index_order_fail(start, end);
        panic!(
            "slice index start is larger than end slice index starts at {start} but ends at {end}",
        )
    }
    if end > len {
        panic!(
            "slice end index is out of range for slice range end index {end} out of range for slice of length {len}"
        )
    }

    ops::Range { start, end }
}
