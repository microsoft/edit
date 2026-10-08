use std::ffi::c_int;
use std::os::fd::{AsRawFd as _, FromRawFd as _, OwnedFd};
use std::sync::atomic::{AtomicI32, AtomicUsize, Ordering};
use std::{io, mem, ptr, thread};

use super::{Ready, check_int_return, from_raw_os_error, last_os_error};

pub(super) struct SignalState {
    writer: AtomicI32,
    active_handlers: AtomicUsize,
}

impl SignalState {
    pub(super) const fn new() -> Self {
        Self { writer: AtomicI32::new(-1), active_handlers: AtomicUsize::new(0) }
    }

    fn disconnect(&self) {
        self.writer.store(-1, Ordering::SeqCst);

        while self.active_handlers.load(Ordering::SeqCst) != 0 {
            thread::yield_now();
        }
    }
}

#[allow(static_mut_refs)]
fn signal_state() -> &'static SignalState {
    unsafe { &super::STATE.signal_pipe }
}

fn errno_location() -> *mut c_int {
    unsafe {
        cfg_select! {
            any(target_os = "solaris", target_os = "illumos") => libc::___errno(),
            target_os = "aix" => libc::_Errno(),
            target_os = "haiku" => libc::_errnop(),
            target_os = "nto" => libc::__get_errno_ptr(),
            any(target_vendor = "apple", target_os = "freebsd") => libc::__error(),
            any(
                target_os = "android",
                target_os = "netbsd",
                target_os = "openbsd",
                target_os = "cygwin",
                target_os = "nuttx",
            ) => libc::__errno(),
            _ => libc::__errno_location(),
        }
    }
}

extern "C" fn resize_handler(_: c_int) {
    unsafe {
        let errno = errno_location();
        let saved_errno = *errno;
        let state = signal_state();

        state.active_handlers.fetch_add(1, Ordering::SeqCst);
        let writer = state.writer.load(Ordering::SeqCst);

        if writer >= 0 {
            let byte = 0u8;
            while libc::write(writer, ptr::from_ref(&byte).cast(), 1) < 0 && *errno == libc::EINTR {
            }
        }

        state.active_handlers.fetch_sub(1, Ordering::SeqCst);
        *errno = saved_errno;
    }
}

pub(super) struct Events {
    reader: OwnedFd,
    _writer: OwnedFd,
    previous_action: libc::sigaction,
}

impl Events {
    pub(super) fn new() -> io::Result<Self> {
        unsafe {
            let mut descriptors = [-1; 2];
            check_int_return(libc::pipe(descriptors.as_mut_ptr()))?;
            let [reader, writer] = descriptors.map(|descriptor| OwnedFd::from_raw_fd(descriptor));

            for descriptor in [&reader, &writer] {
                let descriptor = descriptor.as_raw_fd();
                check_int_return(libc::fcntl(descriptor, libc::F_SETFD, libc::FD_CLOEXEC))?;
                let flags = check_int_return(libc::fcntl(descriptor, libc::F_GETFL))?;
                check_int_return(libc::fcntl(descriptor, libc::F_SETFL, flags | libc::O_NONBLOCK))?;
            }

            let state = signal_state();
            state
                .writer
                .compare_exchange(-1, writer.as_raw_fd(), Ordering::SeqCst, Ordering::SeqCst)
                .map_err(|_| {
                    io::Error::new(io::ErrorKind::AlreadyExists, "resize pipe already installed")
                })?;

            let mut action: libc::sigaction = mem::zeroed();
            libc::sigemptyset(&mut action.sa_mask);
            action.sa_sigaction = resize_handler as *const () as libc::sighandler_t;

            let mut previous_action = mem::zeroed();
            let result =
                check_int_return(libc::sigaction(libc::SIGWINCH, &action, &mut previous_action));
            if let Err(err) = result {
                state.disconnect();
                return Err(err);
            }

            Ok(Self { reader, _writer: writer, previous_action })
        }
    }

    pub(super) fn wait(&self, stdin: c_int, timeout: Option<&libc::timespec>) -> io::Result<Ready> {
        let mut descriptors = [
            libc::pollfd { fd: stdin, events: libc::POLLIN, revents: 0 },
            libc::pollfd { fd: self.reader.as_raw_fd(), events: libc::POLLIN, revents: 0 },
        ];

        unsafe {
            check_int_return(libc::poll(
                descriptors.as_mut_ptr(),
                descriptors.len() as _,
                timeout_millis(timeout),
            ))?;
        }

        let [input, signal] = descriptors;

        if (input.revents | signal.revents) & libc::POLLNVAL != 0 {
            return Err(from_raw_os_error(libc::EBADF));
        }
        if signal.revents & (libc::POLLERR | libc::POLLHUP) != 0 {
            return Err(io::Error::other("resize pipe became unavailable"));
        }

        let resize = signal.revents & libc::POLLIN != 0 && self.read_resize()?;

        Ok(Ready {
            input: input.revents & (libc::POLLIN | libc::POLLHUP | libc::POLLERR) != 0,
            resize,
        })
    }

    fn read_resize(&self) -> io::Result<bool> {
        let mut buffer = [0u8; 256];
        let mut resized = false;

        loop {
            let count = unsafe {
                libc::read(self.reader.as_raw_fd(), buffer.as_mut_ptr().cast(), buffer.len())
            };

            if count > 0 {
                resized = true;
                continue;
            }
            if count == 0 {
                return Err(io::Error::new(io::ErrorKind::UnexpectedEof, "resize pipe closed"));
            }

            let err = last_os_error();
            match err.kind() {
                io::ErrorKind::Interrupted => continue,
                io::ErrorKind::WouldBlock => return Ok(resized),
                _ => return Err(err),
            }
        }
    }
}

impl Drop for Events {
    fn drop(&mut self) {
        unsafe {
            libc::sigaction(libc::SIGWINCH, &self.previous_action, ptr::null_mut());
        }

        signal_state().disconnect();
    }
}

fn timeout_millis(timeout: Option<&libc::timespec>) -> c_int {
    timeout.map_or(-1, |timeout| {
        let seconds = (timeout.tv_sec as u64).saturating_mul(1000);
        let fraction = (timeout.tv_nsec as u64).div_ceil(1_000_000);
        seconds.saturating_add(fraction).min(c_int::MAX as u64) as c_int
    })
}

#[cfg(test)]
mod tests {
    use std::process::Command;
    use std::sync::mpsc;
    use std::time::Duration;

    use super::*;

    #[test]
    fn selfpipe_events() {
        if std::env::var_os("EDIT_TEST_SELFPIPE").is_none() {
            let output = Command::new(std::env::current_exe().unwrap())
                .args(["--exact", "sys::unix::selfpipe::tests::selfpipe_events", "--nocapture"])
                .env("EDIT_TEST_SELFPIPE", "1")
                .output()
                .unwrap();
            assert!(
                output.status.success(),
                "{}\n{}",
                String::from_utf8_lossy(&output.stdout),
                String::from_utf8_lossy(&output.stderr)
            );
            return;
        }

        unsafe {
            libc::alarm(10);
            let mut previous_action: libc::sigaction = mem::zeroed();
            assert_eq!(libc::sigaction(libc::SIGWINCH, ptr::null(), &mut previous_action), 0);

            let events = Events::new().unwrap();
            for descriptor in [&events.reader, &events._writer] {
                assert_ne!(
                    libc::fcntl(descriptor.as_raw_fd(), libc::F_GETFL) & libc::O_NONBLOCK,
                    0
                );
                assert_ne!(
                    libc::fcntl(descriptor.as_raw_fd(), libc::F_GETFD) & libc::FD_CLOEXEC,
                    0
                );
            }

            *errno_location() = libc::EBADF;
            resize_handler(libc::SIGWINCH);
            assert_eq!(*errno_location(), libc::EBADF);
            assert!(events.wait(-1, None).unwrap().resize);

            let buffer = [0u8; 1024];
            while libc::write(events._writer.as_raw_fd(), buffer.as_ptr().cast(), buffer.len()) > 0
            {
            }
            *errno_location() = libc::EBADF;
            resize_handler(libc::SIGWINCH);
            assert_eq!(*errno_location(), libc::EBADF);
            assert!(events.wait(-1, None).unwrap().resize);

            let zero = libc::timespec { tv_sec: 0, tv_nsec: 0 };
            assert!(!events.wait(-1, Some(&zero)).unwrap().resize);
            assert_eq!(libc::raise(libc::SIGWINCH), 0);
            assert!(events.wait(-1, None).unwrap().resize);

            let input_thread = libc::pthread_self() as usize;
            let sender = thread::spawn(move || {
                thread::sleep(Duration::from_millis(20));
                assert_eq!(libc::pthread_kill(input_thread as libc::pthread_t, libc::SIGWINCH), 0);
            });
            loop {
                match events.wait(-1, None) {
                    Ok(ready) => {
                        assert!(ready.resize);
                        break;
                    }
                    Err(err) if err.kind() == io::ErrorKind::Interrupted => continue,
                    Err(err) => panic!("{err}"),
                }
            }
            sender.join().unwrap();

            assert_eq!(Events::new().err().unwrap().kind(), io::ErrorKind::AlreadyExists);

            let reader = events.reader.as_raw_fd();
            let writer = events._writer.as_raw_fd();
            signal_state().active_handlers.fetch_add(1, Ordering::SeqCst);

            let (finished, completion) = mpsc::channel();
            let cleanup = thread::spawn(move || {
                drop(events);
                finished.send(()).unwrap();
            });

            while signal_state().writer.load(Ordering::SeqCst) != -1 {
                thread::yield_now();
            }
            assert_eq!(completion.try_recv(), Err(mpsc::TryRecvError::Empty));
            assert_ne!(libc::fcntl(reader, libc::F_GETFD), -1);
            assert_ne!(libc::fcntl(writer, libc::F_GETFD), -1);

            signal_state().active_handlers.fetch_sub(1, Ordering::SeqCst);
            completion.recv_timeout(Duration::from_secs(1)).unwrap();
            cleanup.join().unwrap();

            assert_eq!(signal_state().writer.load(Ordering::SeqCst), -1);
            assert_eq!(libc::fcntl(reader, libc::F_GETFD), -1);
            assert_eq!(libc::fcntl(writer, libc::F_GETFD), -1);

            let mut action: libc::sigaction = mem::zeroed();
            assert_eq!(libc::sigaction(libc::SIGWINCH, ptr::null(), &mut action), 0);
            assert_eq!(action.sa_sigaction, previous_action.sa_sigaction);
            resize_handler(libc::SIGWINCH);
            drop(Events::new().unwrap());
            libc::alarm(0);
        }
    }

    #[test]
    fn poll_timeouts() {
        assert_eq!(timeout_millis(None), -1);
        let mut timeout = libc::timespec { tv_sec: 0, tv_nsec: 0 };
        assert_eq!(timeout_millis(Some(&timeout)), 0);
        timeout.tv_nsec = 1;
        assert_eq!(timeout_millis(Some(&timeout)), 1);
        timeout.tv_nsec = 1_000_001;
        assert_eq!(timeout_millis(Some(&timeout)), 2);
        timeout.tv_sec = libc::time_t::MAX;
        assert_eq!(timeout_millis(Some(&timeout)), c_int::MAX);
    }
}
