// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::alloc::AllocError;
use std::ffi::c_int;
use std::ptr::{self, NonNull, null_mut};

/// Reserves a virtual memory region of the given size.
/// To commit the memory, use `virtual_commit`.
/// To release the memory, use `virtual_release`.
///
/// # Safety
///
/// This function is unsafe because it uses raw pointers.
/// Don't forget to release the memory when you're done with it or you'll leak it.
pub unsafe fn virtual_reserve(size: usize) -> Result<NonNull<u8>, AllocError> {
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
            Err(AllocError)
        } else {
            Ok(NonNull::new_unchecked(ptr.cast()))
        }
    }
}

#[cfg(target_os = "netbsd")]
const fn desired_mprotect(flags: c_int) -> c_int {
    // NetBSD allows an mmap(2) caller to specify what protection flags they
    // will use later via mprotect. It does not allow a caller to move from
    // PROT_NONE to PROT_READ | PROT_WRITE.
    //
    // see PROT_MPROTECT in man 2 mmap
    flags << 3
}

#[cfg(not(target_os = "netbsd"))]
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
pub unsafe fn virtual_commit(base: NonNull<u8>, size: usize) -> Result<(), AllocError> {
    unsafe {
        let status = libc::mprotect(base.cast().as_ptr(), size, libc::PROT_READ | libc::PROT_WRITE);
        if status != 0 { Err(AllocError) } else { Ok(()) }
    }
}
