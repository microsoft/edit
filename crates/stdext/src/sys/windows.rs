// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::io;
use std::ptr::{NonNull, null_mut};

const MEM_COMMIT: u32 = 0x00001000;
const MEM_RELEASE: u32 = 0x00008000;
const MEM_RESERVE: u32 = 0x00002000;
const PAGE_READWRITE: u32 = 0x04;

unsafe extern "system" {
    fn VirtualAlloc(
        lpAddress: *mut u8,
        dwSize: usize,
        flAllocationType: u32,
        flProtect: u32,
    ) -> *mut u8;
    fn VirtualFree(lpAddress: *mut u8, dwSize: usize, dwFreeType: u32) -> i32;
}

/// Reserves a virtual memory region of the given size.
/// To commit the memory, use [`virtual_commit`].
/// To release the memory, use [`virtual_release`].
///
/// # Safety
///
/// This function is unsafe because it uses raw pointers.
/// Don't forget to release the memory when you're done with it or you'll leak it.
pub unsafe fn virtual_reserve(size: usize) -> io::Result<NonNull<u8>> {
    unsafe {
        let res = VirtualAlloc(null_mut(), size, MEM_RESERVE, PAGE_READWRITE);
        if res.is_null() {
            Err(io::Error::last_os_error())
        } else {
            Ok(NonNull::new_unchecked(res))
        }
    }
}

/// Releases a virtual memory region of the given size.
///
/// # Safety
///
/// This function is unsafe because it uses raw pointers.
/// Make sure to only pass pointers acquired from [`virtual_reserve`].
pub unsafe fn virtual_release(base: NonNull<u8>, _size: usize) {
    unsafe {
        // NOTE: `VirtualFree` fails if the pointer isn't
        // a valid base address or if the size isn't zero.
        VirtualFree(base.as_ptr() as *mut _, 0, MEM_RELEASE);
    }
}

/// Commits a virtual memory region of the given size.
///
/// # Safety
///
/// This function is unsafe because it uses raw pointers.
/// Make sure to only pass pointers acquired from [`virtual_reserve`]
/// and to pass a size less than or equal to the size passed to [`virtual_reserve`].
pub unsafe fn virtual_commit(base: NonNull<u8>, size: usize) -> io::Result<()> {
    unsafe {
        let res = VirtualAlloc(base.as_ptr() as *mut _, size, MEM_COMMIT, PAGE_READWRITE);
        if res.is_null() { Err(io::Error::last_os_error()) } else { Ok(()) }
    }
}
