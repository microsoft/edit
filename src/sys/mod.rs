// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! Platform abstractions.

#![allow(non_camel_case_types)]

#[cfg(unix)]
mod unix;
#[cfg(windows)]
mod windows;

use std::ffi::{c_char, c_void};
use std::fs::File;
#[cfg(not(windows))]
pub use std::fs::canonicalize;
use std::path::Path;
use std::ptr::NonNull;
use std::{fmt, time};

#[cfg(unix)]
pub use unix::*;
#[cfg(windows)]
pub use windows::*;

use crate::apperr;
use crate::arena::{Arena, ArenaString};

#[cfg(unix)]
pub type syscall = UnixSys;
#[cfg(windows)]
pub type syscall = WindowsSys;

pub trait Syscall {
    /// Initializes the platform-specific state.
    fn init() -> Deinit;

    /// Switches the terminal into raw mode, etc.
    fn switch_modes() -> apperr::Result<()>;

    /// During startup we need to get the window size from the terminal.
    /// Because I didn't want to type a bunch of code, this function tells
    /// [`read_stdin`] to inject a fake sequence, which gets picked up by
    /// the input parser and provided to the TUI code.
    fn inject_window_size_into_stdin();

    /// Reads from stdin.
    ///
    /// # Returns
    ///
    /// * `None` if there was an error reading from stdin.
    /// * `Some("")` if the given timeout was reached.
    /// * Otherwise, it returns the read, non-empty string.
    fn read_stdin(arena: &Arena, timeout: time::Duration) -> Option<ArenaString<'_>>;

    /// Writes a string to stdout.
    ///
    /// Use this instead of `print!` or `println!` to avoid
    /// the overhead of Rust's stdio handling. Don't need that.
    fn write_stdout(text: &str);

    /// Check if the stdin handle is redirected to a file, etc.
    ///
    /// # Returns
    ///
    /// * `Some(file)` if stdin is redirected.
    /// * Otherwise, `None`.
    fn open_stdin_if_redirected() -> Option<File>;

    /// Returns a unique identifier for the given file by handle or path.
    fn file_id(file: Option<&File>, path: &Path) -> apperr::Result<FileId>;

    /// Reserves a virtual memory region of the given size.
    /// To commit the memory, use `virtual_commit`.
    /// To release the memory, use `virtual_release`.
    ///
    /// # Safety
    ///
    /// This function is unsafe because it uses raw pointers.
    /// Don't forget to release the memory when you're done with it or you'll leak it.
    unsafe fn virtual_reserve(size: usize) -> apperr::Result<NonNull<u8>>;

    /// Commits a virtual memory region of the given size.
    ///
    /// # Safety
    ///
    /// This function is unsafe because it uses raw pointers.
    /// Make sure to only pass pointers acquired from `virtual_reserve`
    /// and to pass a size less than or equal to the size passed to `virtual_reserve`.
    unsafe fn virtual_commit(base: NonNull<u8>, size: usize) -> apperr::Result<()>;

    /// Releases a virtual memory region of the given size.
    ///
    /// # Safety
    ///
    /// This function is unsafe because it uses raw pointers.
    /// Make sure to only pass pointers acquired from `virtual_reserve`.
    unsafe fn virtual_release(base: NonNull<u8>, size: usize);

    /// Associated type for `load_library` because the library name type differs between platforms.
    /// on `Unix` it is `*const c_char`, on `Windows` it is `*const u16`.
    type LibraryName;

    /// Loads a dynamic library.
    ///
    /// # Safety
    ///
    /// This function is unsafe because it uses raw pointers.
    unsafe fn load_library(name: Self::LibraryName) -> apperr::Result<NonNull<c_void>>;

    /// Loads a function from a dynamic library.
    ///
    /// # Safety
    ///
    /// This function is highly unsafe as it requires you to know the exact type
    /// of the function you're loading. No type checks whatsoever are performed.
    //
    // It'd be nice to constrain T to std::marker::FnPtr, but that's unstable.
    unsafe fn get_proc_address<T>(
        handle: NonNull<c_void>,
        name: *const c_char,
    ) -> apperr::Result<T>;

    fn load_icu() -> apperr::Result<LibIcu>;

    /// Returns a list of preferred languages for the current user.
    fn preferred_languages(arena: &Arena) -> Vec<ArenaString<'_>, &Arena>;

    fn apperr_format(f: &mut fmt::Formatter<'_>, code: u32) -> fmt::Result;

    /// Checks if the given error is a "file not found" error.
    fn apperr_is_not_found(err: apperr::Error) -> bool;
}
