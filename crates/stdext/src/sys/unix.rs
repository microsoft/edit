// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::ffi::c_int;
use std::io;
use std::ptr::{self, NonNull, null_mut};

/// Reserves a virtual memory region of the given size.
/// To commit the memory, use `virtual_commit`.
/// To release the memory, use `virtual_release`.
///
/// # Safety
///
/// This function is unsafe because it uses raw pointers.
/// Don't forget to release the memory when you're done with it or you'll leak it.
pub unsafe fn virtual_reserve(size: usize) -> io::Result<NonNull<u8>> {
    unsafe {
        let ptr = libc::mmap(
            null_mut(),
            size,
            desired_mprotect(libc::PROT_READ | libc::PROT_WRITE),
            libc::MAP_PRIVATE | libc::MAP_ANONYMOUS,
            -1,
            0,
        );
        if ptr.is_null() || ptr::eq(ptr, libc::MAP_FAILED) {
            Err(io::Error::last_os_error())
        } else {
            Ok(NonNull::new_unchecked(ptr as *mut u8))
        }
    }
}

const fn desired_mprotect(_: c_int) -> c_int {
    libc::PROT_NONE
}

/// Releases a virtual memory region of the given size.
///
/// # Safety
///
/// This function is unsafe because it uses raw pointers.
/// Make sure to only pass pointers acquired from `virtual_reserve`.
pub unsafe fn virtual_release(base: NonNull<u8>, size: usize) {
    unsafe {
        libc::munmap(base.cast().as_ptr(), size);
    }
}

/// Commits a virtual memory region of the given size.
///
/// # Safety
///
/// This function is unsafe because it uses raw pointers.
/// Make sure to only pass pointers acquired from `virtual_reserve`
/// and to pass a size less than or equal to the size passed to `virtual_reserve`.
pub unsafe fn virtual_commit(base: NonNull<u8>, size: usize) -> io::Result<()> {
    unsafe {
        let status = libc::mprotect(base.cast().as_ptr(), size, libc::PROT_READ | libc::PROT_WRITE);
        if status != 0 { Err(io::Error::last_os_error()) } else { Ok(()) }
    }
}
