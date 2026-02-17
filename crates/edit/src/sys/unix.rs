// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! macOS-specific platform code.

use std::ffi::{c_char, c_int, c_void};
use std::fs::File;
use std::mem::{self, ManuallyDrop, MaybeUninit};
use std::os::fd::{AsRawFd as _, FromRawFd as _};
use std::path::Path;
use std::ptr::{NonNull, null_mut};
use std::{io, thread, time};

use stdext::arena::{Arena, scratch_arena};
use stdext::arena_format;
use stdext::collections::{BString, BVec};

use crate::helpers::*;

struct State {
    stdin: libc::c_int,
    stdin_flags: libc::c_int,
    stdout: libc::c_int,
    stdout_initial_termios: Option<libc::termios>,
    inject_resize: bool,
    // Buffer for incomplete UTF-8 sequences (max 4 bytes needed)
    utf8_buf: [u8; 4],
    utf8_len: usize,
}

static mut STATE: State = State {
    stdin: libc::STDIN_FILENO,
    stdin_flags: 0,
    stdout: libc::STDOUT_FILENO,
    stdout_initial_termios: None,
    inject_resize: false,
    utf8_buf: [0; 4],
    utf8_len: 0,
};

extern "C" fn sigwinch_handler(_: libc::c_int) {
    unsafe {
        STATE.inject_resize = true;
    }
}

pub fn init() -> Deinit {
    Deinit
}

pub fn switch_modes() -> io::Result<()> {
    unsafe {
        // Reopen stdin if it's redirected (= piped input).
        if libc::isatty(STATE.stdin) == 0 {
            STATE.stdin = check_int_return(libc::open(c"/dev/tty".as_ptr(), libc::O_RDONLY))?;
        }

        // Store the stdin flags so we can more easily toggle `O_NONBLOCK` later on.
        STATE.stdin_flags = check_int_return(libc::fcntl(STATE.stdin, libc::F_GETFL))?;

        // Set STATE.inject_resize to true whenever we get a SIGWINCH.
        let mut sigwinch_action: libc::sigaction = mem::zeroed();
        sigwinch_action.sa_sigaction = sigwinch_handler as *const () as libc::sighandler_t;
        check_int_return(libc::sigaction(libc::SIGWINCH, &sigwinch_action, null_mut()))?;

        // Get the original terminal modes so we can disable raw mode on exit.
        let mut termios = MaybeUninit::<libc::termios>::uninit();
        check_int_return(libc::tcgetattr(STATE.stdout, termios.as_mut_ptr()))?;
        let mut termios = termios.assume_init();
        STATE.stdout_initial_termios = Some(termios);

        termios.c_iflag &= !(
            // When neither IGNBRK...
            libc::IGNBRK
            // ...nor BRKINT are set, a BREAK reads as a null byte ('\0'), ...
            | libc::BRKINT
            // ...except when PARMRK is set, in which case it reads as the sequence \377 \0 \0.
            | libc::PARMRK
            // Disable input parity checking.
            | libc::INPCK
            // Disable stripping of eighth bit.
            | libc::ISTRIP
            // Disable mapping of NL to CR on input.
            | libc::INLCR
            // Disable ignoring CR on input.
            | libc::IGNCR
            // Disable mapping of CR to NL on input.
            | libc::ICRNL
            // Disable software flow control.
            | libc::IXON
        );
        // Disable output processing.
        termios.c_oflag &= !libc::OPOST;
        termios.c_cflag &= !(
            // Reset character size mask.
            libc::CSIZE
            // Disable parity generation.
            | libc::PARENB
        );
        // Set character size back to 8 bits.
        termios.c_cflag |= libc::CS8;
        termios.c_lflag &= !(
            // Disable signal generation (SIGINT, SIGTSTP, SIGQUIT).
            libc::ISIG
            // Disable canonical mode (line buffering).
            | libc::ICANON
            // Disable echoing of input characters.
            | libc::ECHO
            // Disable echoing of NL.
            | libc::ECHONL
            // Disable extended input processing (e.g. Ctrl-V).
            | libc::IEXTEN
        );

        // Set the terminal to raw mode.
        termios.c_lflag &= !(libc::ICANON | libc::ECHO);
        check_int_return(libc::tcsetattr(STATE.stdout, libc::TCSANOW, &termios))?;

        Ok(())
    }
}

pub struct Deinit;

impl Drop for Deinit {
    fn drop(&mut self) {
        unsafe {
            #[allow(static_mut_refs)]
            if let Some(termios) = STATE.stdout_initial_termios.take() {
                // Restore the original terminal modes.
                libc::tcsetattr(STATE.stdout, libc::TCSANOW, &termios);
            }
        }
    }
}

pub fn inject_window_size_into_stdin() {
    unsafe {
        STATE.inject_resize = true;
    }
}

fn get_window_size() -> (u16, u16) {
    let mut winsz: libc::winsize = unsafe { mem::zeroed() };

    for attempt in 1.. {
        let ret = unsafe { libc::ioctl(STATE.stdout, libc::TIOCGWINSZ, &raw mut winsz) };
        if ret == -1 || (winsz.ws_col != 0 && winsz.ws_row != 0) {
            break;
        }

        if attempt == 10 {
            winsz.ws_col = 80;
            winsz.ws_row = 24;
            break;
        }

        // Some terminals are bad emulators and don't report TIOCGWINSZ immediately.
        thread::sleep(time::Duration::from_millis(10 * attempt));
    }

    (winsz.ws_col, winsz.ws_row)
}

/// Reads from stdin.
///
/// Returns `None` if there was an error reading from stdin.
/// Returns `Some("")` if the given timeout was reached.
/// Otherwise, it returns the read, non-empty string.
pub fn read_stdin(arena: &Arena, mut timeout: time::Duration) -> Option<BString<'_>> {
    unsafe {
        if STATE.inject_resize {
            timeout = time::Duration::ZERO;
        }

        let read_poll = timeout != time::Duration::MAX;
        let mut buf = BVec::empty();

        // We don't know if the input is valid UTF8, so we first use a Vec and then
        // later turn it into UTF8 using `from_utf8_lossy_owned`.
        // It is important that we allocate the buffer with an explicit capacity,
        // because we later use `spare_capacity_mut` to access it.
        buf.reserve(arena, 4 * KIBI);

        // We got some leftover broken UTF8 from a previous read? Prepend it.
        if STATE.utf8_len != 0 {
            buf.extend_from_slice(arena, &STATE.utf8_buf[..STATE.utf8_len]);
            STATE.utf8_len = 0;
        }

        loop {
            if timeout != time::Duration::MAX {
                let beg = time::Instant::now();

                let mut pollfd = libc::pollfd { fd: STATE.stdin, events: libc::POLLIN, revents: 0 };
                let ret = libc::poll(&mut pollfd, 1, timeout.as_millis() as libc::c_int);
                if ret < 0 {
                    return None; // Error? Let's assume it's an EOF.
                }
                if ret == 0 {
                    break; // Timeout? We can stop reading.
                }

                timeout = timeout.saturating_sub(beg.elapsed());
            };

            // If we're asked for a non-blocking read we need
            // to manipulate `O_NONBLOCK` and vice versa.
            set_tty_nonblocking(read_poll);

            // Read from stdin.
            let spare = buf.spare_capacity_mut();
            let ret = libc::read(STATE.stdin, spare.as_mut_ptr() as *mut _, spare.len());
            if ret > 0 {
                buf.set_len(buf.len() + ret as usize);
                break;
            }
            if ret == 0 {
                return None; // EOF
            }
            if ret < 0 {
                match errno() {
                    libc::EINTR if STATE.inject_resize => break,
                    libc::EAGAIN if timeout == time::Duration::ZERO => break,
                    libc::EINTR | libc::EAGAIN => {}
                    _ => return None,
                }
            }
        }

        if !buf.is_empty() {
            // We only need to check the last 3 bytes for UTF-8 continuation bytes,
            // because we should be able to assume that any 4 byte sequence is complete.
            let lim = buf.len().saturating_sub(3);
            let mut off = buf.len() - 1;

            // Find the start of the last potentially incomplete UTF-8 sequence.
            while off > lim && buf[off] & 0b1100_0000 == 0b1000_0000 {
                off -= 1;
            }

            let seq_len = match buf[off] {
                b if b & 0b1000_0000 == 0 => 1,
                b if b & 0b1110_0000 == 0b1100_0000 => 2,
                b if b & 0b1111_0000 == 0b1110_0000 => 3,
                b if b & 0b1111_1000 == 0b1111_0000 => 4,
                // If the lead byte we found isn't actually one, we don't cache it.
                // `from_utf8_lossy_owned` will replace it with U+FFFD.
                _ => 0,
            };

            // Cache incomplete sequence if any.
            if off + seq_len > buf.len() {
                STATE.utf8_len = buf.len() - off;
                STATE.utf8_buf[..STATE.utf8_len].copy_from_slice(&buf[off..]);
                buf.truncate(off);
            }
        }

        let mut result = BString::from_utf8_lossy(arena, buf);

        // We received a SIGWINCH? Add a fake window size sequence for our input parser.
        // I prepend it so that on startup, the TUI system gets first initialized with a size.
        if STATE.inject_resize {
            STATE.inject_resize = false;
            let (w, h) = get_window_size();
            if w > 0 && h > 0 {
                let scratch = scratch_arena(Some(arena));
                let seq = arena_format!(&*scratch, "\x1b[8;{h};{w}t");
                result.replace_range(arena, 0..0, &seq);
            }
        }

        Some(result)
    }
}

pub fn write_stdout(text: &str) {
    if text.is_empty() {
        return;
    }

    // If we don't set the TTY to blocking mode,
    // the write will potentially fail with EAGAIN.
    set_tty_nonblocking(false);

    let buf = text.as_bytes();
    let mut written = 0;

    while written < buf.len() {
        let w = &buf[written..];
        let w = &buf[..w.len().min(GIBI)];
        let n = unsafe { libc::write(STATE.stdout, w.as_ptr() as *const _, w.len()) };

        if n >= 0 {
            written += n as usize;
            continue;
        }

        let err = errno();
        if err != libc::EINTR {
            return;
        }
    }
}

/// Sets/Resets `O_NONBLOCK` on the TTY handle.
///
/// Note that setting this flag applies to both stdin and stdout, because the
/// TTY is a bidirectional device and both handles refer to the same thing.
fn set_tty_nonblocking(nonblock: bool) {
    unsafe {
        let is_nonblock = (STATE.stdin_flags & libc::O_NONBLOCK) != 0;
        if is_nonblock != nonblock {
            STATE.stdin_flags ^= libc::O_NONBLOCK;
            let _ = libc::fcntl(STATE.stdin, libc::F_SETFL, STATE.stdin_flags);
        }
    }
}

pub fn open_stdin_if_redirected() -> Option<File> {
    unsafe {
        // Did we reopen stdin during `init()`?
        if STATE.stdin != libc::STDIN_FILENO {
            Some(File::from_raw_fd(libc::STDIN_FILENO))
        } else {
            None
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct FileId {
    st_dev: libc::dev_t,
    st_ino: libc::ino_t,
}

/// Returns a unique identifier for the given file by handle or path.
pub fn file_id(file: Option<&File>, path: &Path) -> io::Result<FileId> {
    let file = match file {
        Some(f) => f,
        None => &File::open(path)?,
    };

    unsafe {
        let mut stat = MaybeUninit::<libc::stat>::uninit();
        check_int_return(libc::fstat(file.as_raw_fd(), stat.as_mut_ptr()))?;
        let stat = stat.assume_init();
        Ok(FileId { st_dev: stat.st_dev, st_ino: stat.st_ino })
    }
}

unsafe fn load_library(name: *const c_char) -> io::Result<NonNull<c_void>> {
    unsafe {
        NonNull::new(libc::dlopen(name, libc::RTLD_LAZY))
            .ok_or_else(|| from_raw_os_error(libc::ENOENT))
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
pub unsafe fn get_proc_address<T>(handle: NonNull<c_void>, name: *const c_char) -> io::Result<T> {
    unsafe {
        let sym = libc::dlsym(handle.as_ptr(), name);
        if sym.is_null() {
            Err(from_raw_os_error(libc::ENOENT))
        } else {
            Ok(mem::transmute_copy(&sym))
        }
    }
}

pub struct LibIcu {
    pub libicuuc: NonNull<c_void>,
    pub libicui18n: NonNull<c_void>,
}

pub fn load_icu() -> io::Result<LibIcu> {
    const fn const_str_eq(a: &str, b: &str) -> bool {
        let a = a.as_bytes();
        let b = b.as_bytes();
        let mut i = 0;

        loop {
            if i >= a.len() || i >= b.len() {
                return a.len() == b.len();
            }
            if a[i] != b[i] {
                return false;
            }
            i += 1;
        }
    }

    const LIBICUUC: &str = concat!(env!("EDIT_CFG_ICUUC_SONAME"), "\0");
    const LIBICUI18N: &str = concat!(env!("EDIT_CFG_ICUI18N_SONAME"), "\0");

    if const { const_str_eq(LIBICUUC, LIBICUI18N) } {
        let icu = unsafe { load_library(LIBICUUC.as_ptr() as *const _)? };
        Ok(LibIcu { libicuuc: icu, libicui18n: icu })
    } else {
        let libicuuc = unsafe { load_library(LIBICUUC.as_ptr() as *const _)? };
        let libicui18n = unsafe { load_library(LIBICUI18N.as_ptr() as *const _)? };
        Ok(LibIcu { libicuuc, libicui18n })
    }
}


pub fn preferred_languages(arena: &Arena) -> BVec<'_, BString<'_>> {
    let mut locales = BVec::empty();

    for key in ["LANGUAGE", "LC_ALL", "LANG"] {
        if let Ok(val) = std::env::var(key)
            && !val.is_empty()
        {
            locales.extend_sloppy(
                arena,
                val.split(':').filter(|s| !s.is_empty()).map(|s| {
                    // Replace all underscores with dashes,
                    // because the localization code expects pt-br, not pt_BR.
                    let mut res = BVec::empty();
                    res.extend(
                        arena,
                        s.as_bytes().iter().map(|&b| if b == b'_' { b'-' } else { b }),
                    );
                    unsafe { BString::from_utf8_unchecked(res) }
                }),
            );
            break;
        }
    }

    locales
}

#[inline]
#[cold]
fn errno() -> c_int {
    // libc unfortunately doesn't export an alias for `errno` (WHY?).
    // As such we (ab)use the stdlib and use its internal errno implementation.
    //
    // Under `-O -Copt-level=s` the 1.87 compiler fails to fully inline and
    // remove the raw_os_error() call. This leaves us with the drop() call.
    // ManuallyDrop fixes that and results in a direct `std::sys::os::errno` call.
    ManuallyDrop::new(io::Error::last_os_error()).raw_os_error().unwrap_or(0)
}

#[inline]
#[cold]
fn last_os_error() -> io::Error {
    io::Error::last_os_error()
}

#[inline]
#[cold]
fn from_raw_os_error(code: c_int) -> io::Error {
    io::Error::from_raw_os_error(code)
}

fn check_int_return(ret: libc::c_int) -> io::Result<libc::c_int> {
    if ret < 0 { Err(last_os_error()) } else { Ok(ret) }
}
