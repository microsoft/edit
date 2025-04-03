use crate::helpers::{CoordType, Size};
use crate::{apperr, helpers};
use std::ffi::{CStr, OsString};
use std::fmt::Write as _;
use std::fs::{self, File};
use std::mem::MaybeUninit;
use std::os::windows::io::FromRawHandle;
use std::path::{Path, PathBuf};
use std::ptr::{self, null, null_mut};
use std::{mem, time};
use windows_sys::Win32::Foundation;
use windows_sys::Win32::Globalization;
use windows_sys::Win32::Storage::FileSystem;
use windows_sys::Win32::System::Console;
use windows_sys::Win32::System::Diagnostics::Debug;
use windows_sys::Win32::System::IO;
use windows_sys::Win32::System::LibraryLoader;
use windows_sys::Win32::System::Memory;
use windows_sys::Win32::System::Threading;
use windows_sys::w;

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
    stdin_mode_old: 0,
    stdout_mode_old: 0,
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

pub fn init() -> apperr::Result<()> {
    unsafe {
        let kernel32 = LibraryLoader::GetModuleHandleW(w!("kernel32.dll"));
        STATE.read_console_input_ex = get_proc_address(kernel32, c"ReadConsoleInputExW")?;

        check_bool_return(Console::SetConsoleCtrlHandler(
            Some(console_ctrl_handler),
            1,
        ))?;

        STATE.stdin = FileSystem::CreateFileW(
            w!("CONIN$"),
            Foundation::GENERIC_READ | Foundation::GENERIC_WRITE,
            FileSystem::FILE_SHARE_READ | FileSystem::FILE_SHARE_WRITE,
            null_mut(),
            FileSystem::OPEN_EXISTING,
            0,
            null_mut(),
        );
        STATE.stdout = FileSystem::CreateFileW(
            w!("CONOUT$"),
            Foundation::GENERIC_READ | Foundation::GENERIC_WRITE,
            FileSystem::FILE_SHARE_READ | FileSystem::FILE_SHARE_WRITE,
            null_mut(),
            FileSystem::OPEN_EXISTING,
            0,
            null_mut(),
        );
        if ptr::eq(STATE.stdin, Foundation::INVALID_HANDLE_VALUE)
            || ptr::eq(STATE.stdout, Foundation::INVALID_HANDLE_VALUE)
        {
            return Err(get_last_error());
        }

        STATE.stdin_cp_old = Console::GetConsoleCP();
        STATE.stdout_cp_old = Console::GetConsoleOutputCP();
        check_bool_return(Console::GetConsoleMode(
            STATE.stdin,
            &raw mut STATE.stdin_mode_old,
        ))?;
        check_bool_return(Console::GetConsoleMode(
            STATE.stdout,
            &raw mut STATE.stdout_mode_old,
        ))?;

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

pub fn deinit() {
    unsafe {
        Console::SetConsoleCP(STATE.stdin_cp_old);
        Console::SetConsoleOutputCP(STATE.stdout_cp_old);
        Console::SetConsoleMode(STATE.stdin, STATE.stdin_mode_old);
        Console::SetConsoleMode(STATE.stdout, STATE.stdout_mode_old);
    }
}

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
        Some(Size {
            width: w,
            height: h,
        })
    }
}

/// Reads from stdin.
///
/// Returns `None` if there was an error reading from stdin.
/// Returns `Some("")` if the given timeout was reached.
/// Otherwise, it returns the read, non-empty string.
pub fn read_stdin(timeout: time::Duration) -> Option<String> {
    let mut input_buf = [const { MaybeUninit::<Console::INPUT_RECORD>::uninit() }; 1024];
    let mut input_buf_cap = input_buf.len();
    let mut utf16_buf = [const { MaybeUninit::<u16>::uninit() }; 1024];
    let mut utf16_buf_len = 0;
    let mut resize_event = None;
    let mut read_poll = timeout != time::Duration::MAX; // there is a timeout -> don't block in read()
    let deadline = if timeout != time::Duration::MAX {
        Some(time::Instant::now() + timeout)
    } else {
        None
    };

    // On startup we're asked to inject a window size so that the UI system can layout the elements.
    // --> Inject a fake sequence for our input parser.
    if unsafe { STATE.inject_resize } {
        resize_event = get_console_size();
        read_poll = true;
        unsafe { STATE.inject_resize = false };
    }

    // If there was a leftover leading surrogate from the last read, we prepend it to the buffer.
    if unsafe { STATE.leading_surrogate } != 0 {
        utf16_buf[0] = MaybeUninit::new(unsafe { STATE.leading_surrogate });
        utf16_buf_len = 1;
        input_buf_cap -= 1;
        unsafe { STATE.leading_surrogate = 0 };
    }

    // Read until there's either a timeout or we have something to process.
    loop {
        if let Some(deadline) = deadline {
            let remaining = deadline
                .saturating_duration_since(time::Instant::now())
                .as_millis() as u32;
            if remaining == 0 {
                break; // Timeout? We can stop reading.
            }

            match unsafe { Threading::WaitForSingleObject(STATE.stdin, remaining) } {
                // Ready to read? Continue with reading below.
                Foundation::WAIT_OBJECT_0 => {}
                // Timeout? Skip reading entirely.
                Foundation::WAIT_TIMEOUT => break,
                // Error? Tell the caller stdin is broken.
                _ => return None,
            }
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
            helpers::slice_assume_init_ref(&input_buf[..read as usize])
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
                        resize_event = Some(Size {
                            width: w,
                            height: h,
                        });
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
    let resize_event_len = if resize_event.is_some() {
        RESIZE_EVENT_FMT_MAX_LEN
    } else {
        0
    };
    // +1 to account for a potential `STATE.leading_surrogate`.
    let utf8_max_len = (utf16_buf_len + 1) * 3;
    let mut text = String::with_capacity(utf8_max_len + resize_event_len);

    // Now prepend our previously extracted resize event.
    if let Some(resize_event) = resize_event {
        // If I read xterm's documentation correctly, CSI 18 t reports the window size in characters.
        // CSI 8 ; height ; width t is the response. Of course, we didn't send the request,
        // but we can use this fake response to trigger the editor to resize itself.
        _ = write!(
            text,
            "\x1b[8;{};{}t",
            resize_event.height, resize_event.width
        );
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

    Some(text)
}

pub fn write_stdout(text: &str) {
    unsafe {
        let mut offset = 0;

        while offset < text.len() {
            let ptr = text.as_ptr().add(offset);
            let write = (text.len() - offset).min(1024 * 1024 * 1024) as u32;
            let mut written = 0;
            let ok = FileSystem::WriteFile(STATE.stdout, ptr, write, &mut written, null_mut());
            offset += written as usize;
            if ok == 0 || written == 0 {
                break;
            }
        }
    }
}

pub fn open_stdin_if_redirected() -> Option<File> {
    unsafe {
        let handle = Console::GetStdHandle(Console::STD_INPUT_HANDLE);
        match FileSystem::GetFileType(handle) {
            FileSystem::FILE_TYPE_DISK | FileSystem::FILE_TYPE_PIPE => {
                Some(File::from_raw_handle(handle))
            }
            _ => None,
        }
    }
}

pub fn canonicalize<P: AsRef<Path>>(path: P) -> apperr::Result<PathBuf> {
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
/// To commit the memory, use `virtual_commit`.
/// To release the memory, use `virtual_release`.
///
/// # Safety
///
/// This function is unsafe because it uses raw pointers.
/// Don't forget to release the memory when you're done with it or you'll leak it.
pub unsafe fn virtual_reserve(size: usize) -> apperr::Result<*mut u8> {
    unsafe {
        let mut base = null_mut();

        if cfg!(debug_assertions) {
            static mut S_BASE_GEN: usize = 0x0000100000000000;
            S_BASE_GEN += 0x0000100000000000;
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
/// Make sure to only pass pointers acquired from `virtual_reserve`.
pub unsafe fn virtual_release(base: *mut u8, size: usize) {
    unsafe {
        Memory::VirtualFree(base as *mut _, size, Memory::MEM_RELEASE);
    }
}

/// Commits a virtual memory region of the given size.
///
/// # Safety
///
/// This function is unsafe because it uses raw pointers.
/// Make sure to only pass pointers acquired from `virtual_reserve`
/// and to pass a size less than or equal to the size passed to `virtual_reserve`.
pub unsafe fn virtual_commit(base: *mut u8, size: usize) -> apperr::Result<()> {
    unsafe {
        check_ptr_return(Memory::VirtualAlloc(
            base as *mut _,
            size,
            Memory::MEM_COMMIT,
            Memory::PAGE_READWRITE,
        ))
        .map(|_| ())
    }
}

unsafe fn load_library(name: *const u16) -> apperr::Result<Foundation::HMODULE> {
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
pub unsafe fn get_proc_address<T>(handle: Foundation::HMODULE, name: &CStr) -> apperr::Result<T> {
    unsafe {
        let ptr = LibraryLoader::GetProcAddress(handle, name.as_ptr() as *const u8);
        if let Some(ptr) = ptr {
            Ok(mem::transmute_copy(&ptr))
        } else {
            Err(get_last_error())
        }
    }
}

pub fn load_libicuuc() -> apperr::Result<Foundation::HMODULE> {
    unsafe { load_library(w!("icu.dll")) }
}

pub fn load_libicui18n(libicuuc: Foundation::HMODULE) -> apperr::Result<Foundation::HMODULE> {
    Ok(libicuuc)
}

pub fn preferred_languages() -> Vec<String> {
    unsafe {
        const LEN: usize = 256;

        let mut lang_num = 0;
        let mut lang_buf = [const { MaybeUninit::<u16>::uninit() }; LEN];
        let mut lang_buf_len = lang_buf.len() as u32;
        if Globalization::GetUserPreferredUILanguages(
            Globalization::MUI_LANGUAGE_NAME,
            &mut lang_num,
            lang_buf[0].as_mut_ptr(),
            &mut lang_buf_len,
        ) == 0
            || lang_num == 0
        {
            return Vec::new();
        }

        // Drop the terminating double-null character.
        lang_buf_len = lang_buf_len.saturating_sub(1);

        let mut lang_buf_utf8 = [const { MaybeUninit::<u8>::uninit() }; 3 * LEN];
        let lang_buf_utf8_len = Globalization::WideCharToMultiByte(
            Globalization::CP_UTF8,
            0,
            lang_buf[0].as_mut_ptr(),
            lang_buf_len as i32,
            lang_buf_utf8[0].as_mut_ptr(),
            lang_buf_utf8.len() as i32,
            null(),
            null_mut(),
        );
        if lang_buf_utf8_len == 0 {
            return Vec::new();
        }

        let result = helpers::str_from_raw_parts_mut(
            lang_buf_utf8[0].as_mut_ptr(),
            lang_buf_utf8_len as usize,
        );
        result.make_ascii_lowercase();
        result.split_terminator('\0').map(String::from).collect()
    }
}

#[cold]
fn get_last_error() -> apperr::Error {
    unsafe { gle_to_apperr(Foundation::GetLastError()) }
}

#[inline]
const fn gle_to_apperr(gle: u32) -> apperr::Error {
    apperr::Error::new_sys(if gle == 0 {
        0x8000FFFF
    } else {
        0x80070000 | gle
    })
}

#[inline]
pub fn io_error_to_apperr(err: std::io::Error) -> apperr::Error {
    gle_to_apperr(err.raw_os_error().unwrap_or(0) as u32)
}

pub fn apperr_format(code: u32) -> String {
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

        let mut result = format!("Error {:#08x}", code);

        if len > 0 {
            let msg = helpers::str_from_raw_parts(ptr, len as usize);
            let msg = msg.trim_ascii();
            let msg = msg.replace(['\r', '\n'], " ");
            result.push_str(": ");
            result.push_str(&msg);
            Foundation::LocalFree(ptr as *mut _);
        }

        result
    }
}

pub fn apperr_is_not_found(err: apperr::Error) -> bool {
    err == gle_to_apperr(Foundation::ERROR_FILE_NOT_FOUND)
}

fn check_bool_return(ret: Foundation::BOOL) -> apperr::Result<()> {
    if ret == 0 {
        Err(get_last_error())
    } else {
        Ok(())
    }
}

fn check_ptr_return<T>(ret: *mut T) -> apperr::Result<*mut T> {
    if ret.is_null() {
        Err(get_last_error())
    } else {
        Ok(ret)
    }
}
