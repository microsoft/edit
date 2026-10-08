use std::ffi::c_int;
use std::os::fd::{AsRawFd as _, FromRawFd as _, OwnedFd};
use std::{io, mem, ptr};

use super::{Ready, check_int_return, from_raw_os_error};

pub(super) struct Events {
    queue: OwnedFd,
    previous_action: libc::sigaction,
}

impl Events {
    pub(super) fn new() -> io::Result<Self> {
        unsafe {
            let queue = OwnedFd::from_raw_fd(check_int_return(libc::kqueue())?);
            check_int_return(libc::fcntl(queue.as_raw_fd(), libc::F_SETFD, libc::FD_CLOEXEC))?;

            let mut signal: libc::kevent = mem::zeroed();
            signal.ident = libc::SIGWINCH as _;
            signal.filter = libc::EVFILT_SIGNAL;
            signal.flags = libc::EV_ADD | libc::EV_CLEAR;

            check_int_return(libc::kevent(
                queue.as_raw_fd(),
                &signal,
                1,
                ptr::null_mut(),
                0,
                ptr::null(),
            ))?;

            let mut action: libc::sigaction = mem::zeroed();
            libc::sigemptyset(&mut action.sa_mask);
            action.sa_sigaction = libc::SIG_IGN;

            let mut previous_action = mem::zeroed();
            check_int_return(libc::sigaction(libc::SIGWINCH, &action, &mut previous_action))?;

            Ok(Self { queue, previous_action })
        }
    }

    pub(super) fn wait(&self, stdin: c_int, timeout: Option<&libc::timespec>) -> io::Result<Ready> {
        unsafe {
            let mut input: libc::kevent = mem::zeroed();
            input.ident = stdin as _;
            input.filter = libc::EVFILT_READ;
            input.flags = libc::EV_ADD;

            let mut events: [libc::kevent; 2] = mem::zeroed();
            let timeout = timeout.map_or(ptr::null(), ptr::from_ref);

            let count = check_int_return(libc::kevent(
                self.queue.as_raw_fd(),
                &input,
                1,
                events.as_mut_ptr(),
                events.len() as _,
                timeout,
            ))?;

            let mut ready = Ready::default();

            for event in &events[..count as usize] {
                if event.flags & libc::EV_ERROR != 0 {
                    return Err(from_raw_os_error(event.data as c_int));
                }

                ready.input |= event.filter == libc::EVFILT_READ;
                ready.resize |= event.filter == libc::EVFILT_SIGNAL;
            }

            Ok(ready)
        }
    }
}

impl Drop for Events {
    fn drop(&mut self) {
        unsafe {
            libc::sigaction(libc::SIGWINCH, &self.previous_action, ptr::null_mut());
        }
    }
}
