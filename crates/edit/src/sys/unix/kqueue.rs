use std::os::fd::{AsRawFd as _, FromRawFd as _, OwnedFd};

use super::*;

pub struct Events {
    fd: OwnedFd,
    previous: libc::sigaction,
}

impl Events {
    pub fn new() -> io::Result<Self> {
        unsafe {
            let fd = OwnedFd::from_raw_fd(check_int_return(libc::kqueue())?);
            check_int_return(libc::fcntl(fd.as_raw_fd(), libc::F_SETFD, libc::FD_CLOEXEC))?;
            let mut event: libc::kevent = mem::zeroed();
            event.ident = libc::SIGWINCH as _;
            event.filter = libc::EVFILT_SIGNAL;
            event.flags = libc::EV_ADD | libc::EV_CLEAR;
            check_int_return(libc::kevent(
                fd.as_raw_fd(),
                &event,
                1,
                ptr::null_mut(),
                0,
                ptr::null(),
            ))?;
            let mut action: libc::sigaction = mem::zeroed();
            libc::sigemptyset(&mut action.sa_mask);
            action.sa_sigaction = libc::SIG_IGN;
            let mut previous = MaybeUninit::uninit();
            check_int_return(libc::sigaction(libc::SIGWINCH, &action, previous.as_mut_ptr()))?;
            Ok(Self { fd, previous: previous.assume_init() })
        }
    }

    pub fn wait(&self, stdin: c_int, timeout: Duration) -> io::Result<Ready> {
        wait(timeout, |timespec| unsafe {
            let mut change: libc::kevent = mem::zeroed();
            change.ident = stdin as _;
            change.filter = libc::EVFILT_READ;
            change.flags = libc::EV_ADD;
            let mut events: [libc::kevent; 2] = mem::zeroed();
            let count = check_int_return(libc::kevent(
                self.fd.as_raw_fd(),
                &change,
                1,
                events.as_mut_ptr(),
                events.len() as _,
                timespec,
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
        })
    }
}

impl Drop for Events {
    fn drop(&mut self) {
        unsafe { libc::sigaction(libc::SIGWINCH, &self.previous, ptr::null_mut()) };
    }
}
