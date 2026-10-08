use std::os::fd::{AsRawFd as _, FromRawFd as _, OwnedFd};
use std::time::Duration;

use super::*;

pub struct Events {
    signalfd: OwnedFd,
}

impl Events {
    pub fn new() -> io::Result<Self> {
        unsafe {
            let mut signals = mem::zeroed();
            libc::sigemptyset(&mut signals);
            libc::sigaddset(&mut signals, libc::SIGWINCH);
            let fd = check_int_return(libc::signalfd(
                -1,
                &signals,
                libc::SFD_CLOEXEC | libc::SFD_NONBLOCK,
            ))?;
            Ok(Self { signalfd: OwnedFd::from_raw_fd(fd) })
        }
    }

    pub fn wait(&self, stdin: c_int, timeout: Duration) -> io::Result<Ready> {
        wait(timeout, |timespec| unsafe {
            let mut fds = [
                libc::pollfd { fd: stdin, events: libc::POLLIN, revents: 0 },
                libc::pollfd { fd: self.signalfd.as_raw_fd(), events: libc::POLLIN, revents: 0 },
            ];
            check_int_return(libc::ppoll(fds.as_mut_ptr(), fds.len() as _, timespec, ptr::null()))?;
            let mut resize = false;
            if fds[1].revents != 0 {
                let mut info = MaybeUninit::<libc::signalfd_siginfo>::uninit();
                let ret = libc::read(
                    self.signalfd.as_raw_fd(),
                    info.as_mut_ptr().cast(),
                    mem::size_of_val(&info),
                );
                if ret < 0 {
                    if errno() != libc::EAGAIN {
                        return Err(last_os_error());
                    }
                } else {
                    resize = ret > 0;
                }
            }
            Ok(Ready { input: fds[0].revents != 0, resize })
        })
    }
}
