// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! Unix-specific platform code.
//!
//! Read the `windows` module for reference.
//! TODO: This reminds me that the sys API should probably be a trait.

// TODO: Get rid of that `State` global.
#![allow(static_mut_refs)]

use std::ffi::{c_char, c_int, c_void};
use std::fs::File;
use std::mem::{self, MaybeUninit};
use std::os::fd::FromRawFd as _;
use std::os::unix::fs::MetadataExt as _;
use std::path::Path;
use std::ptr::{self, NonNull, null_mut};
use std::{io, time};

use crate::arena::{Arena, scratch_arena};
use crate::collections::{BString, BVec};
use crate::helpers::*;

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
            Err(last_os_error())
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
pub unsafe fn virtual_commit(base: NonNull<u8>, size: usize) -> io::Result<()> {
    unsafe {
        let status = libc::mprotect(base.cast().as_ptr(), size, libc::PROT_READ | libc::PROT_WRITE);
        if status != 0 { Err(last_os_error()) } else { Ok(()) }
    }
}

struct State {
    stdin: libc::c_int,
    stdin_flags: libc::c_int,
    stdout_initial_termios: Option<libc::termios>,
    sigwinch: Pipe,
    // Buffer for incomplete UTF-8 sequences (max 4 bytes needed)
    utf8_buf: [u8; 4],
    utf8_len: usize,
}

static mut STATE: State = State {
    stdin: libc::STDIN_FILENO,
    stdin_flags: -1,
    stdout_initial_termios: None,
    sigwinch: Pipe::default(),
    utf8_buf: [0; 4],
    utf8_len: 0,
};

/// We use a self-pipe trick to handle SIGWINCH + stdin polling.
///
/// There are technically superior alternatives on Linux with epoll+signalfd and BSDs with kqueue.
/// I ran tests on Linux and epoll+signalfd improves startup CPU 1.03us -> 0.96us and memory use
/// 2.38KiB (+4KiB once a signal was delivered) -> 1.05KiB. CPU usage per delivery 7us -> 5us.
///
/// This is certainly nice, but way way way too little difference relative to the sum of all
/// other operations in this editor. It is worth considering if we ever need our own mio/tokio.
/// (On that note, it is worth avoiding tokio, due to lack of support for rare platforms such as UEFI.)
extern "C" fn sigwinch_handler(_: libc::c_int) {
    unsafe {
        let saved_errno = errno();
        let fd = STATE.sigwinch.writer;

        if fd >= 0 {
            let b = 0u8;
            while libc::write(fd, (&raw const b).cast(), 1) < 0 && errno() == libc::EINTR {}
        }

        set_errno(saved_errno);
    }
}

pub fn init() -> io::Result<Deinit> {
    Ok(Deinit)
}

/// Reopen stdin if it's redirected (= piped input).
pub fn reopen_stdin_if_redirected() -> io::Result<Option<File>> {
    unsafe {
        if libc::isatty(STATE.stdin) == 0 {
            STATE.stdin = libc::open(c"/dev/tty".as_ptr(), libc::O_RDONLY).check()?;
            Ok(Some(File::from_raw_fd(libc::STDIN_FILENO)))
        } else {
            Ok(None)
        }
    }
}

pub fn switch_modes() -> io::Result<Size> {
    unsafe {
        // Get the original terminal modes so we can disable raw mode on exit.
        let mut termios = MaybeUninit::<libc::termios>::uninit();
        libc::tcgetattr(libc::STDOUT_FILENO, termios.as_mut_ptr()).check()?;
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
            // Disable sending of start/stop characters.
            | libc::IXOFF
        );
        #[cfg(any(target_os = "linux", target_os = "android"))]
        {
            // Disable translating uppercase characters to lowercase.
            termios.c_iflag &= !libc::IUCLC;
        }
        // Disable output processing.
        termios.c_oflag &= !libc::OPOST;
        termios.c_cflag &= !(
            // Reset character size mask.
            libc::CSIZE
            // Disable parity generation.
            | libc::PARENB
        );
        termios.c_cflag |=
            // Set character size back to 8 bits.
            libc::CS8
            // Allow input to be received.
            | libc::CREAD;
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
        // Reset the TTY to standard blocking behavior (min 1 byte & no timeout).
        termios.c_cc[libc::VMIN] = 1;
        termios.c_cc[libc::VTIME] = 0;

        termios_setattr(libc::STDOUT_FILENO, &termios)?;

        // Set up the SIGWINCH self-pipe...
        STATE.sigwinch = Pipe::new(libc::O_CLOEXEC | libc::O_NONBLOCK)?;

        // ...and the SIGWINCH handler.
        let mut sigwinch_action: libc::sigaction = mem::zeroed();
        sigwinch_action.sa_sigaction = sigwinch_handler as *const () as libc::sighandler_t;
        libc::sigemptyset(&mut sigwinch_action.sa_mask);
        libc::sigaction(libc::SIGWINCH, &sigwinch_action, null_mut()).check()?;

        // Since we'll use poll(), stdin must be non-blocking.
        STATE.stdin_flags = libc::fcntl(STATE.stdin, libc::F_GETFL).check()?;
        libc::fcntl(STATE.stdin, libc::F_SETFL, STATE.stdin_flags | libc::O_NONBLOCK).check()?;

        // NOTE: It's crucial to get the initial window size AFTER
        // setting up the signal handler to avoid missing an update.
        get_window_size()
    }
}

fn termios_setattr(fd: c_int, termios: &libc::termios) -> io::Result<()> {
    loop {
        if unsafe { libc::tcsetattr(fd, libc::TCSANOW, termios) } == 0 {
            return Ok(());
        }
        if errno() != libc::EINTR {
            return Err(last_os_error());
        }
    }
}

pub struct Deinit;

impl Drop for Deinit {
    fn drop(&mut self) {
        unsafe {
            #[allow(static_mut_refs)]
            if let Some(termios) = STATE.stdout_initial_termios.take() {
                // Restore the original terminal modes.
                _ = termios_setattr(libc::STDOUT_FILENO, &termios);
            }

            if STATE.stdin_flags != -1 {
                libc::fcntl(STATE.stdin, libc::F_SETFL, STATE.stdin_flags);
                STATE.stdin_flags = -1;
            }
        }
    }
}

fn get_window_size() -> io::Result<Size> {
    let mut winsz: libc::winsize = unsafe { mem::zeroed() };
    let ret = unsafe { libc::ioctl(libc::STDOUT_FILENO, libc::TIOCGWINSZ, &raw mut winsz) };
    if ret != 0 {
        Err(last_os_error())
    } else if winsz.ws_row == 0 || winsz.ws_col == 0 {
        Err(io::Error::other("invalid terminal size"))
    } else {
        Ok(Size { width: winsz.ws_col as CoordType, height: winsz.ws_row as CoordType })
    }
}

/// Reads from stdin.
///
/// Returns `None` if there was an error reading from stdin.
/// Returns `Some((_, ""))` if the given timeout was reached.
/// Otherwise, it returns a pending resize and the read string.
pub fn read_stdin(
    arena: &Arena,
    mut timeout: time::Duration,
) -> Option<(Option<Size>, BString<'_>)> {
    unsafe {
        let deadline = (timeout != time::Duration::MAX)
            .then_some(timeout)
            .and_then(|t| time::Instant::now().checked_add(t));
        let mut resized = false;
        let mut buf = BVec::empty();
        let mut fds = [
            libc::pollfd { fd: STATE.sigwinch.reader, events: libc::POLLIN, revents: 0 },
            libc::pollfd { fd: STATE.stdin, events: libc::POLLIN, revents: 0 },
        ];

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
            // Wait for stdin or SIGWINCH.
            cfg_select! {
                any(
                    target_os = "linux",     // tier 1
                    target_os = "android",   // tier 2
                    target_os = "freebsd",   // tier 2
                    target_os = "fuchsia",   // tier 2
                    target_os = "netbsd",    // tier 2
                    target_os = "cygwin",    // tier 3
                    target_os = "dragonfly", // tier 3
                    target_os = "haiku",     // tier 3
                    target_os = "hurd",      // tier 3
                    target_os = "l4re",      // tier 3
                    target_os = "openbsd",   // tier 3
                ) => {
                    let t = timeout.as_secs().try_into().ok().map(|tv_sec| libc::timespec {
                        tv_sec,
                        tv_nsec: timeout.subsec_nanos() as libc::c_long,
                    });
                    let ret =
                        libc::ppoll(fds.as_mut_ptr(), 2, opt_ptr(t.as_ref()), std::ptr::null());
                }
                _ => {
                    let t = timeout.as_millis().try_into().unwrap_or(-1);
                    let ret = libc::poll(fds.as_mut_ptr(), 2, t);
                }
            }

            if ret == 0 {
                break; // Timeout? We can stop reading.
            } else if ret > 0 {
                // Drain SIGWINCH events from the self-pipe.
                if fds[0].revents & libc::POLLIN != 0 {
                    let scratch = scratch_arena(Some(arena));
                    let buf = scratch.alloc_uninit_array::<256, u8>();

                    loop {
                        let ret =
                            libc::read(STATE.sigwinch.reader, buf.as_mut_ptr().cast(), buf.len());
                        if ret > 0 {
                            resized = true;
                        } else if ret == 0 {
                            return None; // Unexpected error
                        } else {
                            #[allow(
                                unreachable_patterns,
                                reason = "On some platforms EAGAIN == EWOULDBLOCK"
                            )]
                            match errno() {
                                libc::EAGAIN | libc::EWOULDBLOCK => break, // finished draining the pipe
                                libc::EINTR => {}
                                _ => return None, // Unexpected error
                            }
                        }
                    }
                }

                // Read from stdin if available.
                if fds[1].revents & libc::POLLIN != 0 {
                    let spare = buf.spare_capacity_mut();
                    let ret = libc::read(STATE.stdin, spare.as_mut_ptr().cast(), spare.len());
                    if ret > 0 {
                        buf.set_len(buf.len() + ret as usize);
                    } else if ret == 0 {
                        return None; // EOF
                    } else {
                        #[allow(
                            unreachable_patterns,
                            reason = "On some platforms EAGAIN == EWOULDBLOCK"
                        )]
                        if !matches!(errno(), libc::EINTR | libc::EAGAIN | libc::EWOULDBLOCK) {
                            return None; // Unexpected error
                        }
                    }
                }
            } else {
                // poll() can fail with EAGAIN if the kernel was OOM and we can retry.
                // Linux returns ENOMEM, but I'm not sure if one is expected to retry that.
                if !matches!(errno(), libc::EINTR | libc::EAGAIN) {
                    return None; // Unexpected error
                }
            }

            // We got something? Great!
            if resized || !buf.is_empty() {
                break;
            }

            // Check for broken pipe events, etc.
            // We do this after draining stdin to avoid missing out on trailing, buffered inputs
            // after the TTY got closed. Technically we don't need to do that, reading that input
            // should not be meaningful. But this is technically the right way to do it.
            if ret > 0 {
                for fd in &fds {
                    if fd.revents & (libc::POLLERR | libc::POLLHUP | libc::POLLNVAL) != 0 {
                        return None; // Unexpected error
                    }
                }
            }

            // Recalculate the remaining timeout if we need to retry.
            if timeout.is_zero() {
                break;
            }
            if let Some(deadline) = deadline {
                timeout = deadline.saturating_duration_since(time::Instant::now());
                if timeout.is_zero() {
                    break;
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

        let resize = if resized { get_window_size().ok() } else { None };

        Some((resize, BString::from_utf8_lossy(arena, buf)))
    }
}

pub fn write_stdout(text: &str) {
    let mut buf = text.as_bytes();

    while !buf.is_empty() {
        let chunk = &buf[..buf.len().min(GIBI)];
        let n = unsafe { libc::write(libc::STDOUT_FILENO, chunk.as_ptr().cast(), chunk.len()) };

        if n > 0 {
            buf = &buf[n as usize..];
            continue;
        }

        if n == 0 {
            return; // broken pipe
        }

        #[allow(unreachable_patterns, reason = "On Linux EAGAIN and EWOULDBLOCK are the same")]
        match errno() {
            libc::EINTR => continue,
            libc::EAGAIN | libc::EWOULDBLOCK => {
                // Block until it becomes writable
                let mut pollfd =
                    libc::pollfd { fd: libc::STDOUT_FILENO, events: libc::POLLOUT, revents: 0 };
                loop {
                    let ret = unsafe { libc::poll(&mut pollfd, 1, -1) };
                    if ret >= 0 {
                        if pollfd.revents & (libc::POLLERR | libc::POLLHUP | libc::POLLNVAL) != 0 {
                            return; // broken pipe
                        }
                        break;
                    }
                    if errno() != libc::EINTR {
                        return; // broken pipe
                    }
                }
            }
            _ => return, // broken pipe
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct FileId {
    dev: u64,
    ino: u64,
}

/// Returns a unique identifier for the given file by handle or path.
pub fn file_id(file: Option<&File>, path: &Path) -> io::Result<FileId> {
    let metadata = match file {
        Some(file) => file.metadata()?,
        None => std::fs::metadata(path)?,
    };
    Ok(FileId { dev: metadata.dev(), ino: metadata.ino() })
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
        let icu = unsafe { load_library(LIBICUUC.as_ptr().cast())? };
        Ok(LibIcu { libicuuc: icu, libicui18n: icu })
    } else {
        let libicuuc = unsafe { load_library(LIBICUUC.as_ptr().cast())? };
        let libicui18n = unsafe { load_library(LIBICUI18N.as_ptr().cast())? };
        Ok(LibIcu { libicuuc, libicui18n })
    }
}

/// ICU, by default, adds the major version as a suffix to each exported symbol.
/// They also recommend to disable this for system-level installations (`runConfigureICU Linux --disable-renaming`),
/// but I found that many (most?) Linux distributions don't do this for some reason.
/// This function returns the suffix, if any.
#[cfg(edit_icu_renaming_auto_detect)]
pub fn icu_detect_renaming_suffix(arena: &Arena, handle: NonNull<c_void>) -> BString<'_> {
    unsafe {
        type T = *const c_void;

        let mut res = BString::empty();

        // Check if the ICU library is using unversioned symbols.
        // Return an empty suffix in that case.
        if get_proc_address::<T>(handle, c"u_errorName".as_ptr()).is_ok() {
            return res;
        }

        // In the versions (63-76) and distributions (Arch/Debian) I tested,
        // this symbol seems to be always present. This allows us to call `dladdr`.
        // It's the `UCaseMap::~UCaseMap()` destructor which for some reason isn't
        // in a namespace. Thank you ICU maintainers for this oversight.
        let proc = match get_proc_address::<T>(handle, c"_ZN8UCaseMapD1Ev".as_ptr()) {
            Ok(proc) => proc,
            Err(_) => return res,
        };

        // `dladdr` is specific to GNU's libc unfortunately.
        let mut info: libc::Dl_info = mem::zeroed();
        let ret = libc::dladdr(proc, &mut info);
        if ret == 0 {
            return res;
        }

        // The library path is in `info.dli_fname`.
        let path = match std::ffi::CStr::from_ptr(info.dli_fname).to_str() {
            Ok(name) => name,
            Err(_) => return res,
        };

        let path = match std::fs::read_link(path) {
            Ok(path) => path,
            Err(_) => path.into(),
        };

        // I'm going to assume it's something like "libicuuc.so.76.1".
        let path = path.into_os_string();
        let path = path.to_string_lossy();
        let suffix_start = match path.rfind(".so.") {
            Some(pos) => pos + 4,
            None => return res,
        };
        let version = &path[suffix_start..];
        let version_end = version.find('.').unwrap_or(version.len());
        let version = &version[..version_end];

        res.push(arena, '_');
        res.push_str(arena, version);
        res
    }
}

#[cfg(edit_icu_renaming_auto_detect)]
#[allow(clippy::not_unsafe_ptr_arg_deref)]
pub fn icu_add_renaming_suffix<'a, 'b, 'r>(
    arena: &'a Arena,
    name: *const c_char,
    suffix: &str,
) -> *const c_char
where
    'a: 'r,
    'b: 'r,
{
    if suffix.is_empty() {
        name
    } else {
        // SAFETY: In this particular case we know that the string
        // is valid UTF-8, because it comes from icu.rs.
        let name = unsafe { std::ffi::CStr::from_ptr(name) };
        let name = unsafe { name.to_str().unwrap_unchecked() };

        let mut res = BString::empty();
        res.reserve(arena, name.len() + suffix.len() + 1);
        res.push_str(arena, name);
        res.push_str(arena, suffix);
        res.push(arena, '\0');
        res.as_ptr() as *const c_char
    }
}

pub fn preferred_languages(arena: &Arena) -> BVec<'_, &'_ str> {
    let mut locales = BVec::empty();

    for key in ["LANGUAGE", "LC_ALL", "LC_MESSAGES", "LANG"] {
        if let Ok(val) = std::env::var(key)
            && !val.is_empty()
        {
            let val = BString::from_str(arena, &val).leak();

            for c in unsafe { val.as_bytes_mut() } {
                if *c == b'_' {
                    *c = b'-';
                }
            }

            locales.extend_sloppy(arena, val.split(':').filter(|s| !s.is_empty()));
            if !locales.is_empty() {
                break;
            }
        }
    }

    locales
}

#[cfg(test)]
pub fn memfd() -> io::Result<File> {
    #[cfg(target_os = "linux")]
    unsafe {
        let fd = libc::memfd_create(c"edit".as_ptr(), libc::MFD_CLOEXEC).check()?;
        Ok(File::from_raw_fd(fd))
    }
    #[cfg(not(target_os = "linux"))]
    unsafe {
        let stream = libc::tmpfile();
        if stream.is_null() {
            return Err(last_os_error());
        }

        // Duplicate the descriptor so that closing the C stream doesn't close our file.
        let file = libc::fcntl(libc::fileno(stream), libc::F_DUPFD_CLOEXEC, 0)
            .check()
            .map(|fd| File::from_raw_fd(fd));
        let close = libc::fclose(stream).check();
        let file = file?;
        close?;

        Ok(file)
    }
}

/// Represents a classic unidirectional pipe.
#[repr(C)]
struct Pipe {
    reader: c_int,
    writer: c_int,
}

impl Pipe {
    #[inline(always)]
    const fn default() -> Self {
        Self { reader: -1, writer: -1 }
    }

    fn new(flags: c_int) -> io::Result<Self> {
        let mut fds = [-1, -1];

        // Ironically, this is 1000% unneeded, because glibc, musl, etc., have shims for pipe2,
        // *but as always*, the required extern_weak marker is an unstable feature. SINCE 2015.
        // This repeated target-flaggery is a waste of everybody's time.
        cfg_select! {
            any(
                target_os = "linux",      // tier 1
                target_os = "android",    // tier 2
                target_os = "emscripten", // tier 2
                target_os = "freebsd",    // tier 2
                target_os = "fuchsia",    // tier 2
                target_os = "netbsd",     // tier 2
                target_os = "redox",      // tier 2
                target_os = "solaris",    // tier 2
                target_os = "cygwin",     // tier 3
                target_os = "dragonfly",  // tier 3
                target_os = "hurd",       // tier 3
                target_os = "illumos",    // tier 3
                target_os = "l4re",       // tier 3
                target_os = "openbsd",    // tier 3
                target_env = "newlib"     // varies by target_os
            ) => unsafe {
                libc::pipe2(fds.as_mut_ptr(), flags).check()?;
            },
            _ => unsafe {
                libc::pipe(fds.as_mut_ptr()).check()?;
                for fd in fds {
                    if flags & libc::O_CLOEXEC != 0 {
                        libc::fcntl(fd, libc::F_SETFD, libc::FD_CLOEXEC).check()?;
                    }
                    if flags & libc::O_NONBLOCK != 0 {
                        libc::fcntl(fd, libc::F_SETFL, libc::O_NONBLOCK).check()?;
                    }
                }
            },
        }

        Ok(Self { reader: fds[0], writer: fds[1] })
    }
}

// "NIH syndrome can't be that bad." UNIX bros:
#[inline(always)]
fn errno_location() -> *mut c_int {
    unsafe {
        cfg_select! {
            any(
                target_os = "linux",      // tier 1
                target_os = "emscripten", // tier 2
                target_os = "fuchsia",    // tier 2
                target_os = "redox",      // tier 2
                target_os = "dragonfly",  // tier 3
                target_os = "hurd",       // tier 3
                target_os = "l4re",       // tier 3
            ) => libc::__errno_location(),
            any(
                target_os = "android", // tier 2
                target_os = "netbsd",  // tier 2
                target_os = "cygwin",  // tier 3
                target_os = "nuttx",   // tier 3
                target_os = "openbsd", // tier 3
                target_env = "newlib", // varies by target_os
            ) => libc::__errno(),
            any(
                target_os = "solaris", // tier 2
                target_os = "illumos", // tier 2
            ) => libc::___errno(),
            any(
                target_vendor = "apple", // tier 1
                target_os = "freebsd",   // tier 2
            ) => libc::__error(),
            any(
                target_os = "haiku", // tier 3
            ) => libc::_errnop(),
            any(
                target_os = "aix", // tier 3
            ) => libc::_Errno(),
            any(
                target_os = "nto", // tier 3
                target_os = "qnx", // tier 3
            ) => libc::__get_errno_ptr(),
            _ => compile_error!("unsupported platform"),
        }
    }
}

#[inline(always)]
fn errno() -> c_int {
    unsafe { *errno_location() }
}

#[inline(always)]
pub fn set_errno(e: c_int) {
    unsafe { *errno_location() = e }
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

trait CheckIntReturn {
    fn check(self) -> io::Result<c_int>;
}

impl CheckIntReturn for c_int {
    fn check(self) -> io::Result<c_int> {
        if self < 0 { Err(last_os_error()) } else { Ok(self) }
    }
}
