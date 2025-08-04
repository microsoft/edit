mod string;
mod vec;

use std::alloc::Layout;
use std::error::Error;
use std::fmt;
use std::ptr::NonNull;

pub use string::*;
pub use vec::*;

#[derive(Debug, Clone, Copy)]
pub struct ExAllocError;

impl Error for ExAllocError {}

impl fmt::Display for ExAllocError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("memory allocation failed")
    }
}

pub unsafe trait ExAllocator {
    fn allocate(&self, size: usize, align: usize) -> Result<NonNull<[u8]>, ExAllocError>;
    unsafe fn grow(
        &self,
        ptr: NonNull<u8>,
        old_size: usize,
        new_size: usize,
        align: usize,
    ) -> Result<NonNull<[u8]>, ExAllocError>;
    unsafe fn deallocate(&self, ptr: NonNull<u8>, size: usize, align: usize);
}

#[derive(Default, Clone, Copy)]
pub struct ExStdAlloc;

unsafe impl ExAllocator for ExStdAlloc {
    fn allocate(&self, size: usize, align: usize) -> Result<NonNull<[u8]>, ExAllocError> {
        unsafe {
            NonNull::new(std::alloc::alloc(Layout::from_size_align_unchecked(size, align)))
                .ok_or(ExAllocError)
                .map(|p| NonNull::slice_from_raw_parts(p, size))
        }
    }

    unsafe fn grow(
        &self,
        ptr: NonNull<u8>,
        old_size: usize,
        new_size: usize,
        align: usize,
    ) -> Result<NonNull<[u8]>, ExAllocError> {
        unsafe {
            NonNull::new(std::alloc::realloc(
                ptr.as_ptr(),
                Layout::from_size_align_unchecked(old_size, align),
                new_size,
            ))
            .ok_or(ExAllocError)
            .map(|p| NonNull::slice_from_raw_parts(p, new_size))
        }
    }

    unsafe fn deallocate(&self, ptr: NonNull<u8>, size: usize, align: usize) {
        unsafe {
            std::alloc::dealloc(ptr.as_ptr(), Layout::from_size_align_unchecked(size, align))
        };
    }
}

unsafe impl<A: ExAllocator + ?Sized> ExAllocator for &A {
    #[inline]
    fn allocate(&self, size: usize, align: usize) -> Result<NonNull<[u8]>, ExAllocError> {
        (**self).allocate(size, align)
    }

    #[inline]
    unsafe fn grow(
        &self,
        ptr: NonNull<u8>,
        old_size: usize,
        new_size: usize,
        align: usize,
    ) -> Result<NonNull<[u8]>, ExAllocError> {
        unsafe { (**self).grow(ptr, old_size, new_size, align) }
    }

    #[inline]
    unsafe fn deallocate(&self, ptr: NonNull<u8>, size: usize, align: usize) {
        unsafe { (**self).deallocate(ptr, size, align) }
    }
}
