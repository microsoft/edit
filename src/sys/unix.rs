use crate::apperr;
use crate::helpers;
use std::borrow::Cow;
use std::ffi::{CStr, CString, c_int, c_void};
use std::fs::{self, File};
use std::mem::{self, MaybeUninit};
use std::os::fd::FromRawFd;
use std::ptr::{self, NonNull, null, null_mut};
use std::thread;
use std::time;

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

pub fn init() -> apperr::Result<Deinit> {
    unsafe {
        // Reopen stdin if it's redirected (= piped input).
        if libc::isatty(STATE.stdin) == 0 {
            STATE.stdin = check_int_return(libc::open(c"/dev/tty".as_ptr(), libc::O_RDONLY))?;
        }

        // Store the stdin flags so we can more easily toggle `O_NONBLOCK` later on.
        STATE.stdin_flags = check_int_return(libc::fcntl(STATE.stdin, libc::F_GETFL))?;

        Ok(Deinit)
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

pub fn switch_modes() -> apperr::Result<()> {
    unsafe {
        // Set STATE.inject_resize to true whenever we get a SIGWINCH.
        let mut sigwinch_action: libc::sigaction = mem::zeroed();
        sigwinch_action.sa_sigaction = sigwinch_handler as libc::sighandler_t;
        check_int_return(libc::sigaction(
            libc::SIGWINCH,
            &sigwinch_action,
            null_mut(),
        ))?;

        // Get the original terminal modes so we can disable raw mode on exit.
        let mut termios = MaybeUninit::<libc::termios>::uninit();
        check_int_return(libc::tcgetattr(STATE.stdin, termios.as_mut_ptr()))?;
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
        check_int_return(libc::tcsetattr(STATE.stdin, libc::TCSANOW, &termios))?;

        Ok(())
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
pub fn read_stdin(mut timeout: time::Duration) -> Option<String> {
    unsafe {
        if STATE.inject_resize {
            timeout = time::Duration::ZERO;
        }

        let read_poll = timeout != time::Duration::MAX;

        // We don't know if the input is valid UTF8, so we first use a Vec and then
        // later turn it into UTF8 using `helpers::string_from_utf8_lossy_owned`.
        // It is important that we allocate the buffer with an explicit capacity,
        // because we later use `spare_capacity_mut` to access it.
        let mut buf = Vec::with_capacity(1024);

        // We got some leftover broken UTF8 from a previous read? Prepend it.
        if STATE.utf8_len != 0 {
            STATE.utf8_len = 0;
            buf.extend_from_slice(&STATE.utf8_buf[..STATE.utf8_len]);
        }

        loop {
            if timeout != time::Duration::MAX {
                let beg = time::Instant::now();

                let mut pollfd = libc::pollfd {
                    fd: STATE.stdin,
                    events: libc::POLLIN,
                    revents: 0,
                };
                let ts = libc::timespec {
                    tv_sec: timeout.as_secs() as libc::time_t,
                    tv_nsec: timeout.subsec_nanos() as libc::c_long,
                };
                let ret = libc::ppoll(&mut pollfd, 1, &ts, null());
                if ret < 0 {
                    return None; // Error? Let's assume it's an EOF.
                }
                if ret == 0 {
                    break; // Timeout? We can stop reading.
                }

                timeout = timeout.saturating_sub(beg.elapsed());
            };

            // If we're asked for a non-blocking read we need to manipulate `O_NONBLOCK` and vice versa.
            // This uses `STATE.stdin_flags` to track and skip unnecessary `fcntl` calls.
            let is_nonblock = (STATE.stdin_flags & libc::O_NONBLOCK) != 0;
            if read_poll != is_nonblock {
                STATE.stdin_flags ^= libc::O_NONBLOCK;
                let _ = libc::fcntl(STATE.stdin, libc::F_SETFL, STATE.stdin_flags);
            }

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
                match *libc::__errno_location() {
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
                // `string_from_utf8_lossy_owned` will replace it with U+FFFD.
                _ => 0,
            };

            // Cache incomplete sequence if any.
            if off + seq_len > buf.len() {
                STATE.utf8_len = buf.len() - off;
                STATE.utf8_buf[..STATE.utf8_len].copy_from_slice(&buf[off..]);
                buf.truncate(off);
            }
        }

        let mut result = helpers::string_from_utf8_lossy_owned(buf);

        // We received a SIGWINCH? Add a fake window size sequence for our input parser.
        // I prepend it so that on startup, the TUI system gets first initialized with a size.
        if STATE.inject_resize {
            STATE.inject_resize = false;
            let (w, h) = get_window_size();
            if w > 0 && h > 0 {
                result = format!("\x1b[8;{h};{w}t{result}");
            }
        }

        Some(result)
    }
}

pub fn write_stdout(text: &str) {
    let buf = text.as_bytes();
    let mut written = 0;

    while written < buf.len() {
        let w = &buf[written..];
        let n = unsafe { libc::write(STATE.stdout, w.as_ptr() as *const _, w.len()) };

        if n >= 0 {
            written += n as usize;
            continue;
        }

        let err = unsafe { *libc::__errno_location() };
        if err != libc::EINTR {
            return;
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

/// Reserves a virtual memory region of the given size.
/// To commit the memory, use `virtual_commit`.
/// To release the memory, use `virtual_release`.
///
/// # Safety
///
/// This function is unsafe because it uses raw pointers.
/// Don't forget to release the memory when you're done with it or you'll leak it.
pub unsafe fn virtual_reserve(size: usize) -> apperr::Result<NonNull<u8>> {
    unsafe {
        let ptr = libc::mmap(
            null_mut(),
            size,
            libc::PROT_NONE,
            libc::MAP_PRIVATE | libc::MAP_ANONYMOUS,
            -1,
            0,
        );
        if ptr.is_null() || ptr::eq(ptr, libc::MAP_FAILED) {
            Err(errno_to_apperr(libc::ENOMEM))
        } else {
            Ok(NonNull::new_unchecked(ptr as *mut u8))
        }
    }
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
pub unsafe fn virtual_commit(base: NonNull<u8>, size: usize) -> apperr::Result<()> {
    unsafe {
        let status = libc::mprotect(
            base.cast().as_ptr(),
            size,
            libc::PROT_READ | libc::PROT_WRITE,
        );
        if status != 0 {
            Err(errno_to_apperr(libc::ENOMEM))
        } else {
            Ok(())
        }
    }
}

unsafe fn load_library(name: &CStr) -> apperr::Result<NonNull<c_void>> {
    unsafe {
        NonNull::new(libc::dlopen(name.as_ptr(), libc::RTLD_LAZY))
            .ok_or_else(|| errno_to_apperr(libc::ELIBACC))
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
        let sym = libc::dlsym(handle.as_ptr(), name.as_ptr());
        if sym.is_null() {
            Err(errno_to_apperr(libc::ELIBACC))
        } else {
            Ok(mem::transmute_copy(&sym))
        }
    }
}

pub fn load_libicuuc() -> apperr::Result<NonNull<c_void>> {
    unsafe { load_library(c"libicuuc.so") }
}

pub fn load_libicui18n() -> apperr::Result<NonNull<c_void>> {
    unsafe { load_library(c"libicui18n.so") }
}

/// ICU, by default, adds the major version as a suffix to each exported symbol.
/// They also recommend to disable this for system-level installations (`runConfigureICU Linux --disable-renaming`),
/// but I found that many (most?) Linux distributions don't do this for some reason.
/// This function returns the suffix, if any.
#[allow(clippy::not_unsafe_ptr_arg_deref)]
pub fn icu_proc_suffix(handle: NonNull<c_void>) -> String {
    unsafe {
        type T = *const c_void;

        // Check if the ICU library is using unversioned symbols.
        // Return an empty suffix in that case.
        if get_proc_address::<T>(handle, c"u_errorName").is_ok() {
            return String::new();
        }

        // In the versions (63-76) and distributions (Arch/Debian) I tested,
        // this symbol seems to be always present. This allows us to call `dladdr`.
        // It's the `UCaseMap::~UCaseMap()` destructor which for some reason isn't
        // in a namespace. Thank you ICU maintainers for this oversight.
        let proc = match get_proc_address::<T>(handle, c"_ZN8UCaseMapD1Ev") {
            Ok(proc) => proc,
            Err(_) => return String::new(),
        };

        // `dladdr` is specific to GNU's libc unfortunately.
        let mut info: libc::Dl_info = mem::zeroed();
        let ret = libc::dladdr(proc, &mut info);
        if ret == 0 {
            return String::new();
        }

        // The library path is in `info.dli_fname`.
        let path = match CStr::from_ptr(info.dli_fname).to_str() {
            Ok(name) => name,
            Err(_) => return String::new(),
        };

        let path = match fs::read_link(path) {
            Ok(path) => path,
            Err(_) => path.into(),
        };

        // I'm going to assume it's something like "libicuuc.so.76.1".
        let path = path.into_os_string();
        let path = path.to_string_lossy();
        let suffix_start = match path.rfind(".so.") {
            Some(pos) => pos + 4,
            None => return String::new(),
        };
        let version = &path[suffix_start..];
        let version_end = version.find('.').unwrap_or(version.len());
        let version = &version[..version_end];
        format!("_{version}")
    }
}

pub fn add_icu_proc_suffix<'a>(name: &'a CStr, suffix: &str) -> Cow<'a, CStr> {
    if suffix.is_empty() {
        Cow::Borrowed(name)
    } else {
        let name = unsafe { name.to_str().unwrap_unchecked() };
        let combined = format!("{name}{suffix}\0");
        let combined = unsafe { CString::from_vec_unchecked(combined.into_bytes()) };
        Cow::Owned(combined)
    }
}

pub fn preferred_languages() -> Vec<String> {
    let mut locales = Vec::new();

    for key in ["LANGUAGE", "LC_ALL", "LANG"] {
        if let Ok(val) = std::env::var(key) {
            locales.extend(
                val.split(':')
                    .filter(|val| !val.is_empty())
                    .map(String::from),
            );
        }
    }

    locales
}

#[inline]
pub fn io_error_to_apperr(err: std::io::Error) -> apperr::Error {
    errno_to_apperr(err.raw_os_error().unwrap_or(0))
}

pub fn apperr_format(code: u32) -> String {
    let mut result = format!("Error {code}");

    unsafe {
        let ptr = libc::strerror(code as i32);
        if !ptr.is_null() {
            let msg = CStr::from_ptr(ptr).to_string_lossy();
            result.push_str(": ");
            result.push_str(&msg);
        }
    }

    result
}

pub fn apperr_is_not_found(err: apperr::Error) -> bool {
    err == errno_to_apperr(libc::ENOENT)
}

const fn errno_to_apperr(no: c_int) -> apperr::Error {
    apperr::Error::new_sys(if no < 0 { 0 } else { no as u32 })
}

fn check_int_return(ret: libc::c_int) -> apperr::Result<libc::c_int> {
    if ret < 0 {
        Err(errno_to_apperr(unsafe { *libc::__errno_location() }))
    } else {
        Ok(ret)
    }
}
