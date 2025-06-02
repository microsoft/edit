use std::alloc::{AllocError, Allocator, Layout};
use std::ptr::{self, NonNull};

use crate::sys;

/// The virtual system allocator backed by [`VirtualAlloc`](https://learn.microsoft.com/en-us/windows/win32/api/memoryapi/nf-memoryapi-virtualalloc).
pub static VALLOCATOR: VirtualAllocator = VirtualAllocator;

#[derive(Debug, Clone, Copy)]
pub struct VirtualAllocator;

unsafe impl Allocator for VirtualAllocator {
    /// Attempts to allocate a block of virtual memory.
    ///
    /// On success, returns a [`NonNull<[u8]>`][NonNull] meeting the size and alignment guarantees of `layout`.
    ///
    /// The returned block may have a larger size than specified by `layout.size()`, and may or may
    /// not have its contents initialized.
    ///
    /// The returned block of memory remains valid as long as it is [*currently allocated*] and the shorter of:
    ///   - the borrow-checker lifetime of the allocator type itself.
    ///   - as long as at the allocator and all its clones has not been dropped.
    ///
    /// # Warning
    /// The returned block of memory has not been commited. Attempting to use the block of memory without commiting it will cause a `STATUS_ACCESS_VIOLATION`.
    ///
    /// # Errors
    ///
    /// Returning `Err` indicates that either memory is exhausted or `layout` does not meet
    /// allocator's size or alignment constraints.
    ///
    /// Implementations are encouraged to return `Err` on memory exhaustion rather than panicking or
    /// aborting, but this is not a strict requirement. (Specifically: it is *legal* to implement
    /// this trait atop an underlying native allocation library that aborts on memory exhaustion.)
    ///
    /// Clients wishing to abort computation in response to an allocation error are encouraged to
    /// call the [`handle_alloc_error`] function, rather than directly invoking `panic!` or similar.
    ///
    /// [`handle_alloc_error`]: ../../alloc/alloc/fn.handle_alloc_error.html
    fn allocate(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        let size = layout.size();
        unsafe {
            match sys::virtual_reserve(size) {
                Ok(ptr) => Ok(NonNull::new_unchecked(ptr::from_raw_parts_mut(ptr.as_ptr(), size))),
                Err(_) => Err(AllocError),
            }
        }
    }

    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
        unsafe { sys::virtual_release(ptr, layout.size()) };
    }
}
