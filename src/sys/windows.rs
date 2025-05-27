// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::ffi::{CStr, OsString, c_void};
use std::fmt::Write as _;
use std::fs::{self, File};
use std::mem::MaybeUninit;
use std::os::windows::io::{AsRawHandle as _, FromRawHandle};
use std::path::{Path, PathBuf};
use std::ptr::{self, NonNull, null, null_mut};
use std::{mem, time};

use windows_sys::Win32::Storage::FileSystem;
use windows_sys::Win32::System::Diagnostics::Debug;
use windows_sys::Win32::System::{Console, IO, LibraryLoader, Memory, Threading};
use windows_sys::Win32::{Foundation, Globalization};
use windows_sys::w;

use crate::apperr;
use crate::arena::{Arena, ArenaString, scratch_arena};
use crate::helpers::*;

type ReadConsoleInputExW = unsafe extern "system" fn(
    h_console_input: Foundation::HANDLE,
    lp_buffer: *mut Console::INPUT_RECORD,
    n_length: u32,
    lp_number_of_events_read: *mut u32,
    w_flags: u16,
) -> Foundation::BOOL;

unsafe extern "system" fn read_console_input_ex_placeholder(
    _: Foundation::HANDLE,
    _: *mut Console::INPUT_RECORD,
    _: u32,
    _: *mut u32,
    _: u16,
) -> Foundation::BOOL {
    panic!();
}

const CONSOLE_READ_NOWAIT: u16 = 0x0002;

const INVALID_CONSOLE_MODE: u32 = u32::MAX;

struct State {
    read_console_input_ex: ReadConsoleInputExW,
    stdin: Foundation::HANDLE,
    stdout: Foundation::HANDLE,
    stdin_cp_old: u32,
    stdout_cp_old: u32,
    stdin_mode_old: u32,
    stdout_mode_old: u32,
    leading_surrogate: u16,
    inject_resize: bool,
    wants_exit: bool,
}

static mut STATE: State = State {
    read_console_input_ex: read_console_input_ex_placeholder,
    stdin: null_mut(),
    stdout: null_mut(),
    stdin_cp_old: 0,
    stdout_cp_old: 0,
    stdin_mode_old: INVALID_CONSOLE_MODE,
    stdout_mode_old: INVALID_CONSOLE_MODE,
    leading_surrogate: 0,
    inject_resize: false,
    wants_exit: false,
};

extern "system" fn console_ctrl_handler(_ctrl_type: u32) -> Foundation::BOOL {
    unsafe {
        STATE.wants_exit = true;
        IO::CancelIoEx(STATE.stdin, null());
    }
    1
}

/// Initializes the platform-specific state.
pub fn init() -> apperr::Result<Deinit> {
    unsafe {
        // Get the stdin and stdout handles first, so that if this function fails,
        // we at least got something to use for `write_stdout`.
        STATE.stdin = Console::GetStdHandle(Console::STD_INPUT_HANDLE);
        STATE.stdout = Console::GetStdHandle(Console::STD_OUTPUT_HANDLE);

        // Reopen stdin if it's redirected (= piped input).
        if !ptr::eq(STATE.stdin, Foundation::INVALID_HANDLE_VALUE)
            && matches!(
                FileSystem::GetFileType(STATE.stdin),
                FileSystem::FILE_TYPE_DISK | FileSystem::FILE_TYPE_PIPE
            )
        {
            STATE.stdin = FileSystem::CreateFileW(
                w!("CONIN$"),
                Foundation::GENERIC_READ | Foundation::GENERIC_WRITE,
                FileSystem::FILE_SHARE_READ | FileSystem::FILE_SHARE_WRITE,
                null_mut(),
                FileSystem::OPEN_EXISTING,
                0,
                null_mut(),
            );
        }

        if ptr::eq(STATE.stdin, Foundation::INVALID_HANDLE_VALUE)
            || ptr::eq(STATE.stdout, Foundation::INVALID_HANDLE_VALUE)
        {
            return Err(get_last_error());
        }

        unsafe fn load_read_func(module: *const u16) -> apperr::Result<ReadConsoleInputExW> {
            unsafe { get_module(module).and_then(|m| get_proc_address(m, c"ReadConsoleInputExW")) }
        }

        // `kernel32.dll` doesn't exist on OneCore variants of Windows.
        // NOTE: `kernelbase.dll` is NOT a stable API to rely on. In our case it's the best option though.
        //
        // This is written as two nested `match` statements so that we can return the error from the first
        // `load_read_func` call if it fails. The kernel32.dll lookup may contain some valid information,
        // while the kernelbase.dll lookup may not, since it's not a stable API.
        STATE.read_console_input_ex = match load_read_func(w!("kernel32.dll")) {
            Ok(func) => func,
            Err(err) => match load_read_func(w!("kernelbase.dll")) {
                Ok(func) => func,
                Err(_) => return Err(err),
            },
        };

        Ok(Deinit)
    }
}

pub struct Deinit;

impl Drop for Deinit {
    fn drop(&mut self) {
        unsafe {
            if STATE.stdin_cp_old != 0 {
                Console::SetConsoleCP(STATE.stdin_cp_old);
                STATE.stdin_cp_old = 0;
            }
            if STATE.stdout_cp_old != 0 {
                Console::SetConsoleOutputCP(STATE.stdout_cp_old);
                STATE.stdout_cp_old = 0;
            }
            if STATE.stdin_mode_old != INVALID_CONSOLE_MODE {
                Console::SetConsoleMode(STATE.stdin, STATE.stdin_mode_old);
                STATE.stdin_mode_old = INVALID_CONSOLE_MODE;
            }
            if STATE.stdout_mode_old != INVALID_CONSOLE_MODE {
                Console::SetConsoleMode(STATE.stdout, STATE.stdout_mode_old);
                STATE.stdout_mode_old = INVALID_CONSOLE_MODE;
            }
        }
    }
}

/// Switches the terminal into raw mode, etc.
pub fn switch_modes() -> apperr::Result<()> {
    unsafe {
        check_bool_return(Console::SetConsoleCtrlHandler(Some(console_ctrl_handler), 1))?;

        STATE.stdin_cp_old = Console::GetConsoleCP();
        STATE.stdout_cp_old = Console::GetConsoleOutputCP();
        check_bool_return(Console::GetConsoleMode(STATE.stdin, &raw mut STATE.stdin_mode_old))?;
        check_bool_return(Console::GetConsoleMode(STATE.stdout, &raw mut STATE.stdout_mode_old))?;

        check_bool_return(Console::SetConsoleCP(Globalization::CP_UTF8))?;
        check_bool_return(Console::SetConsoleOutputCP(Globalization::CP_UTF8))?;
        check_bool_return(Console::SetConsoleMode(
            STATE.stdin,
            Console::ENABLE_WINDOW_INPUT
                | Console::ENABLE_EXTENDED_FLAGS
                | Console::ENABLE_VIRTUAL_TERMINAL_INPUT,
        ))?;
        check_bool_return(Console::SetConsoleMode(
            STATE.stdout,
            Console::ENABLE_PROCESSED_OUTPUT
                | Console::ENABLE_WRAP_AT_EOL_OUTPUT
                | Console::ENABLE_VIRTUAL_TERMINAL_PROCESSING
                | Console::DISABLE_NEWLINE_AUTO_RETURN,
        ))?;

        Ok(())
    }
}

/// During startup we need to get the window size from the terminal.
/// Because I didn't want to type a bunch of code, this function tells
/// [`read_stdin`] to inject a fake sequence, which gets picked up by
/// the input parser and provided to the TUI code.
pub fn inject_window_size_into_stdin() {
    unsafe {
        STATE.inject_resize = true;
    }
}

fn get_console_size() -> Option<Size> {
    unsafe {
        let mut info: Console::CONSOLE_SCREEN_BUFFER_INFOEX = mem::zeroed();
        info.cbSize = mem::size_of::<Console::CONSOLE_SCREEN_BUFFER_INFOEX>() as u32;
        if Console::GetConsoleScreenBufferInfoEx(STATE.stdout, &mut info) == 0 {
            return None;
        }

        let w = (info.srWindow.Right - info.srWindow.Left + 1).max(1) as CoordType;
        let h = (info.srWindow.Bottom - info.srWindow.Top + 1).max(1) as CoordType;
        Some(Size { width: w, height: h })
    }
}

/// Reads from stdin.
///
/// # Returns
///
/// * `None` if there was an error reading from stdin.
/// * `Some("")` if the given timeout was reached.
/// * Otherwise, it returns the read, non-empty string.
pub fn read_stdin(arena: &Arena, mut timeout: time::Duration) -> Option<ArenaString<'_>> {
    let scratch = scratch_arena(Some(arena));

    // On startup we're asked to inject a window size so that the UI system can layout the elements.
    // --> Inject a fake sequence for our input parser.
    let mut resize_event = None;
    if unsafe { STATE.inject_resize } {
        unsafe { STATE.inject_resize = false };
        timeout = time::Duration::ZERO;
        resize_event = get_console_size();
    }

    let read_poll = timeout != time::Duration::MAX; // there is a timeout -> don't block in read()
    let input_buf = scratch.alloc_uninit_slice(4 * KIBI);
    let mut input_buf_cap = input_buf.len();
    let utf16_buf = scratch.alloc_uninit_slice(4 * KIBI);
    let mut utf16_buf_len = 0;

    // If there was a leftover leading surrogate from the last read, we prepend it to the buffer.
    if unsafe { STATE.leading_surrogate } != 0 {
        utf16_buf[0] = MaybeUninit::new(unsafe { STATE.leading_surrogate });
        utf16_buf_len = 1;
        input_buf_cap -= 1;
        unsafe { STATE.leading_surrogate = 0 };
    }

    // Read until there's either a timeout or we have something to process.
    loop {
        if timeout != time::Duration::MAX {
            let beg = time::Instant::now();

            match unsafe { Threading::WaitForSingleObject(STATE.stdin, timeout.as_millis() as u32) }
            {
                // Ready to read? Continue with reading below.
                Foundation::WAIT_OBJECT_0 => {}
                // Timeout? Skip reading entirely.
                Foundation::WAIT_TIMEOUT => break,
                // Error? Tell the caller stdin is broken.
                _ => return None,
            }

            timeout = timeout.saturating_sub(beg.elapsed());
        }

        // Read from stdin.
        let input = unsafe {
            // If we had a `inject_resize`, we don't want to block indefinitely for other pending input on startup,
            // but are still interested in any other pending input that may be waiting for us.
            let flags = if read_poll { CONSOLE_READ_NOWAIT } else { 0 };
            let mut read = 0;
            let ok = (STATE.read_console_input_ex)(
                STATE.stdin,
                input_buf[0].as_mut_ptr(),
                input_buf_cap as u32,
                &mut read,
                flags,
            );
            if ok == 0 || STATE.wants_exit {
                return None;
            }
            input_buf[..read as usize].assume_init_ref()
        };

        // Convert Win32 input records into UTF16.
        for inp in input {
            match inp.EventType as u32 {
                Console::KEY_EVENT => {
                    let event = unsafe { &inp.Event.KeyEvent };
                    let ch = unsafe { event.uChar.UnicodeChar };
                    if event.bKeyDown != 0 && ch != 0 {
                        utf16_buf[utf16_buf_len] = MaybeUninit::new(ch);
                        utf16_buf_len += 1;
                    }
                }
                Console::WINDOW_BUFFER_SIZE_EVENT => {
                    let event = unsafe { &inp.Event.WindowBufferSizeEvent };
                    let w = event.dwSize.X as CoordType;
                    let h = event.dwSize.Y as CoordType;
                    // Windows is prone to sending broken/useless `WINDOW_BUFFER_SIZE_EVENT`s.
                    // E.g. starting conhost will emit 3 in a row. Skip rendering in that case.
                    if w > 0 && h > 0 {
                        resize_event = Some(Size { width: w, height: h });
                    }
                }
                _ => {}
            }
        }

        if resize_event.is_some() || utf16_buf_len != 0 {
            break;
        }
    }

    const RESIZE_EVENT_FMT_MAX_LEN: usize = 16; // "\x1b[8;65535;65535t"
    let resize_event_len = if resize_event.is_some() { RESIZE_EVENT_FMT_MAX_LEN } else { 0 };
    // +1 to account for a potential `STATE.leading_surrogate`.
    let utf8_max_len = (utf16_buf_len + 1) * 3;
    let mut text = ArenaString::new_in(arena);
    text.reserve(utf8_max_len + resize_event_len);

    // Now prepend our previously extracted resize event.
    if let Some(resize_event) = resize_event {
        // If I read xterm's documentation correctly, CSI 18 t reports the window size in characters.
        // CSI 8 ; height ; width t is the response. Of course, we didn't send the request,
        // but we can use this fake response to trigger the editor to resize itself.
        _ = write!(text, "\x1b[8;{};{}t", resize_event.height, resize_event.width);
    }

    // If the input ends with a lone lead surrogate, we need to remember it for the next read.
    if utf16_buf_len > 0 {
        unsafe {
            let last_char = utf16_buf[utf16_buf_len - 1].assume_init();
            if (0xD800..0xDC00).contains(&last_char) {
                STATE.leading_surrogate = last_char;
                utf16_buf_len -= 1;
            }
        }
    }

    // Convert the remaining input to UTF8, the sane encoding.
    if utf16_buf_len > 0 {
        unsafe {
            let vec = text.as_mut_vec();
            let spare = vec.spare_capacity_mut();

            let len = Globalization::WideCharToMultiByte(
                Globalization::CP_UTF8,
                0,
                utf16_buf[0].as_ptr(),
                utf16_buf_len as i32,
                spare.as_mut_ptr() as *mut _,
                spare.len() as i32,
                null(),
                null_mut(),
            );

            if len > 0 {
                vec.set_len(vec.len() + len as usize);
            }
        }
    }

    text.shrink_to_fit();
    Some(text)
}

/// Writes a string to stdout.
///
/// Use this instead of `print!` or `println!` to avoid
/// the overhead of Rust's stdio handling. Don't need that.
pub fn write_stdout(text: &str) {
    unsafe {
        let mut offset = 0;

        while offset < text.len() {
            let ptr = text.as_ptr().add(offset);
            let write = (text.len() - offset).min(GIBI) as u32;
            let mut written = 0;
            let ok = FileSystem::WriteFile(STATE.stdout, ptr, write, &mut written, null_mut());
            offset += written as usize;
            if ok == 0 || written == 0 {
                break;
            }
        }
    }
}

/// Check if the stdin handle is redirected to a file, etc.
///
/// # Returns
///
/// * `Some(file)` if stdin is redirected.
/// * Otherwise, `None`.
pub fn open_stdin_if_redirected() -> Option<File> {
    unsafe {
        let handle = Console::GetStdHandle(Console::STD_INPUT_HANDLE);
        // Did we reopen stdin during `init()`?
        if !std::ptr::eq(STATE.stdin, handle) { Some(File::from_raw_handle(handle)) } else { None }
    }
}

pub fn drives() -> impl Iterator<Item = char> {
    unsafe {
        let mut mask = FileSystem::GetLogicalDrives();
        std::iter::from_fn(move || {
            let bit = mask.trailing_zeros();
            if bit >= 26 {
                None
            } else {
                mask &= !(1 << bit);
                Some((b'A' + bit as u8) as char)
            }
        })
    }
}

/// A unique identifier for a file.
pub enum FileId {
    Id(FileSystem::FILE_ID_INFO),
    Path(PathBuf),
}

impl PartialEq for FileId {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Id(left), Self::Id(right)) => {
                // Lowers to an efficient word-wise comparison.
                const SIZE: usize = std::mem::size_of::<FileSystem::FILE_ID_INFO>();
                let a: &[u8; SIZE] = unsafe { mem::transmute(left) };
                let b: &[u8; SIZE] = unsafe { mem::transmute(right) };
                a == b
            }
            (Self::Path(left), Self::Path(right)) => left == right,
            _ => false,
        }
    }
}

impl Eq for FileId {}

/// Returns a unique identifier for the given file by handle or path.
pub fn file_id(file: Option<&File>, path: &Path) -> apperr::Result<FileId> {
    let file = match file {
        Some(f) => f,
        None => &File::open(path)?,
    };

    file_id_from_handle(file).or_else(|_| Ok(FileId::Path(std::fs::canonicalize(path)?)))
}

fn file_id_from_handle(file: &File) -> apperr::Result<FileId> {
    unsafe {
        let mut info = MaybeUninit::<FileSystem::FILE_ID_INFO>::uninit();
        check_bool_return(FileSystem::GetFileInformationByHandleEx(
            file.as_raw_handle(),
            FileSystem::FileIdInfo,
            info.as_mut_ptr() as *mut _,
            mem::size_of::<FileSystem::FILE_ID_INFO>() as u32,
        ))?;
        Ok(FileId::Id(info.assume_init()))
    }
}

/// Canonicalizes the given path.
///
/// This differs from [`fs::canonicalize`] in that it strips the `\\?\` UNC
/// prefix on Windows. This is because it's confusing/ugly when displaying it.
pub fn canonicalize(path: &Path) -> std::io::Result<PathBuf> {
    let mut path = fs::canonicalize(path)?;
    let path = path.as_mut_os_string();
    let mut path = mem::take(path).into_encoded_bytes();

    if path.len() > 6 && &path[0..4] == br"\\?\" && path[4].is_ascii_uppercase() && path[5] == b':'
    {
        path.drain(0..4);
    }

    let path = unsafe { OsString::from_encoded_bytes_unchecked(path) };
    let path = PathBuf::from(path);
    Ok(path)
}

/// Reserves a virtual memory region of the given size.
/// To commit the memory, use [`virtual_commit`].
/// To release the memory, use [`virtual_release`].
///
/// # Safety
///
/// This function is unsafe because it uses raw pointers.
/// Don't forget to release the memory when you're done with it or you'll leak it.
pub unsafe fn virtual_reserve(size: usize) -> apperr::Result<NonNull<u8>> {
    unsafe {
        #[allow(unused_assignments, unused_mut)]
        let mut base = null_mut();

        // In debug builds, we use fixed addresses to aid in debugging.
        // Makes it possible to immediately tell which address space a pointer belongs to.
        #[cfg(all(debug_assertions, not(target_pointer_width = "32")))]
        {
            static mut S_BASE_GEN: usize = 0x0000100000000000; // 16 TiB
            S_BASE_GEN += 0x0000001000000000; // 64 GiB
            base = S_BASE_GEN as *mut _;
        }

        check_ptr_return(Memory::VirtualAlloc(
            base,
            size,
            Memory::MEM_RESERVE,
            Memory::PAGE_READWRITE,
        ) as *mut u8)
    }
}

/// Releases a virtual memory region of the given size.
///
/// # Safety
///
/// This function is unsafe because it uses raw pointers.
/// Make sure to only pass pointers acquired from [`virtual_reserve`].
pub unsafe fn virtual_release(base: NonNull<u8>, size: usize) {
    unsafe {
        Memory::VirtualFree(base.as_ptr() as *mut _, size, Memory::MEM_RELEASE);
    }
}

/// Commits a virtual memory region of the given size.
///
/// # Safety
///
/// This function is unsafe because it uses raw pointers.
/// Make sure to only pass pointers acquired from [`virtual_reserve`]
/// and to pass a size less than or equal to the size passed to [`virtual_reserve`].
pub unsafe fn virtual_commit(base: NonNull<u8>, size: usize) -> apperr::Result<()> {
    unsafe {
        check_ptr_return(Memory::VirtualAlloc(
            base.as_ptr() as *mut _,
            size,
            Memory::MEM_COMMIT,
            Memory::PAGE_READWRITE,
        ))
        .map(|_| ())
    }
}

unsafe fn get_module(name: *const u16) -> apperr::Result<NonNull<c_void>> {
    unsafe { check_ptr_return(LibraryLoader::GetModuleHandleW(name)) }
}

unsafe fn load_library(name: *const u16) -> apperr::Result<NonNull<c_void>> {
    unsafe {
        check_ptr_return(LibraryLoader::LoadLibraryExW(
            name,
            null_mut(),
            LibraryLoader::LOAD_LIBRARY_SEARCH_SYSTEM32,
        ))
    }
}

/// Loads a function from a dynamic library.
///
/// # Safety
///
/// This function is highly unsafe as it requires you to know the exact type
/// of the function you're loading. No type checks whatsoever are performed.
//
// It'd be nice to constrain T to std::marker::FnPtr, but that's unstable.
pub unsafe fn get_proc_address<T>(handle: NonNull<c_void>, name: &CStr) -> apperr::Result<T> {
    unsafe {
        let ptr = LibraryLoader::GetProcAddress(handle.as_ptr(), name.as_ptr() as *const u8);
        if let Some(ptr) = ptr { Ok(mem::transmute_copy(&ptr)) } else { Err(get_last_error()) }
    }
}

/// Loads the "common" portion of ICU4C.
pub fn load_libicuuc() -> apperr::Result<NonNull<c_void>> {
    unsafe { load_library(w!("icuuc.dll")) }
}

/// Loads the internationalization portion of ICU4C.
pub fn load_libicui18n() -> apperr::Result<NonNull<c_void>> {
    unsafe { load_library(w!("icuin.dll")) }
}

/// Returns a list of preferred languages for the current user.
pub fn preferred_languages(arena: &Arena) -> Vec<ArenaString, &Arena> {
    // If the GetUserPreferredUILanguages() don't fit into 512 characters,
    // honestly, just give up. How many languages do you realistically need?
    const LEN: usize = 512;

    let scratch = scratch_arena(Some(arena));
    let mut res = Vec::new_in(arena);

    // Get the list of preferred languages via `GetUserPreferredUILanguages`.
    let langs = unsafe {
        let buf = scratch.alloc_uninit_slice(LEN);
        let mut len = buf.len() as u32;
        let mut num = 0;

        let ok = Globalization::GetUserPreferredUILanguages(
            Globalization::MUI_LANGUAGE_NAME,
            &mut num,
            buf[0].as_mut_ptr(),
            &mut len,
        );

        if ok == 0 || num == 0 {
            len = 0;
        }

        // Drop the terminating double-null character.
        len = len.saturating_sub(1);

        buf[..len as usize].assume_init_ref()
    };

    // Convert UTF16 to UTF8.
    let langs = wide_to_utf8(&scratch, langs);

    // Split the null-delimited string into individual chunks
    // and copy them into the given arena.
    res.extend(
        langs
            .split_terminator('\0')
            .filter(|s| !s.is_empty())
            .map(|s| ArenaString::from_str(arena, s)),
    );
    res
}

fn wide_to_utf8<'a>(arena: &'a Arena, wide: &[u16]) -> ArenaString<'a> {
    let mut res = ArenaString::new_in(arena);
    res.reserve(wide.len() * 3);

    let len = unsafe {
        Globalization::WideCharToMultiByte(
            Globalization::CP_UTF8,
            0,
            wide.as_ptr(),
            wide.len() as i32,
            res.as_mut_ptr() as *mut _,
            res.capacity() as i32,
            null(),
            null_mut(),
        )
    };
    if len > 0 {
        unsafe { res.as_mut_vec().set_len(len as usize) };
    }

    res.shrink_to_fit();
    res
}

#[cold]
fn get_last_error() -> apperr::Error {
    unsafe { gle_to_apperr(Foundation::GetLastError()) }
}

#[inline]
const fn gle_to_apperr(gle: u32) -> apperr::Error {
    apperr::Error::new_sys(if gle == 0 { 0x8000FFFF } else { 0x80070000 | gle })
}

#[inline]
pub(crate) fn io_error_to_apperr(err: std::io::Error) -> apperr::Error {
    gle_to_apperr(err.raw_os_error().unwrap_or(0) as u32)
}

/// Formats a platform error code into a human-readable string.
pub fn apperr_format(f: &mut std::fmt::Formatter<'_>, code: u32) -> std::fmt::Result {
    unsafe {
        let mut ptr: *mut u8 = null_mut();
        let len = Debug::FormatMessageA(
            Debug::FORMAT_MESSAGE_ALLOCATE_BUFFER
                | Debug::FORMAT_MESSAGE_FROM_SYSTEM
                | Debug::FORMAT_MESSAGE_IGNORE_INSERTS,
            null(),
            code,
            0,
            &mut ptr as *mut *mut _ as *mut _,
            0,
            null_mut(),
        );

        write!(f, "Error {code:#08x}")?;

        if len > 0 {
            let msg = str_from_raw_parts(ptr, len as usize);
            let msg = msg.trim_ascii();
            let msg = msg.replace(['\r', '\n'], " ");
            write!(f, ": {msg}")?;
            Foundation::LocalFree(ptr as *mut _);
        }

        Ok(())
    }
}

/// Checks if the given error is a "file not found" error.
pub fn apperr_is_not_found(err: apperr::Error) -> bool {
    err == gle_to_apperr(Foundation::ERROR_FILE_NOT_FOUND)
}

fn check_bool_return(ret: Foundation::BOOL) -> apperr::Result<()> {
    if ret == 0 { Err(get_last_error()) } else { Ok(()) }
}

fn check_ptr_return<T>(ret: *mut T) -> apperr::Result<NonNull<T>> {
    NonNull::new(ret).ok_or_else(get_last_error)
}
