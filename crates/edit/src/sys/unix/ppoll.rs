use std::ffi::c_int;
use std::mem::MaybeUninit;
use std::os::fd::{AsRawFd as _, FromRawFd as _, OwnedFd};
use std::{io, mem, ptr};

use super::{Ready, check_int_return, from_raw_os_error, last_os_error};

pub(super) struct Events {
    signalfd: OwnedFd,
    previous_mask: libc::sigset_t,
}

impl Events {
    pub(super) fn new() -> io::Result<Self> {
        unsafe {
            let mut signals = mem::zeroed();
            libc::sigemptyset(&mut signals);
            libc::sigaddset(&mut signals, libc::SIGWINCH);

            let mut previous_mask = mem::zeroed();
            let status = libc::pthread_sigmask(libc::SIG_BLOCK, &signals, &mut previous_mask);
            if status != 0 {
                return Err(from_raw_os_error(status));
            }

            let descriptor = libc::signalfd(-1, &signals, libc::SFD_CLOEXEC | libc::SFD_NONBLOCK);

            if descriptor < 0 {
                let err = last_os_error();
                libc::pthread_sigmask(libc::SIG_SETMASK, &previous_mask, ptr::null_mut());
                return Err(err);
            }

            Ok(Self { signalfd: OwnedFd::from_raw_fd(descriptor), previous_mask })
        }
    }

    pub(super) fn wait(&self, stdin: c_int, timeout: Option<&libc::timespec>) -> io::Result<Ready> {
        let mut descriptors = [
            libc::pollfd { fd: stdin, events: libc::POLLIN, revents: 0 },
            libc::pollfd { fd: self.signalfd.as_raw_fd(), events: libc::POLLIN, revents: 0 },
        ];
        let timeout = timeout.map_or(ptr::null(), ptr::from_ref);

        unsafe {
            check_int_return(libc::ppoll(
                descriptors.as_mut_ptr(),
                descriptors.len() as _,
                timeout,
                ptr::null(),
            ))?;
        }

        let [input, signal] = descriptors;

        if (input.revents | signal.revents) & libc::POLLNVAL != 0 {
            return Err(from_raw_os_error(libc::EBADF));
        }
        if signal.revents & (libc::POLLERR | libc::POLLHUP) != 0 {
            return Err(io::Error::other("signal descriptor became unavailable"));
        }

        let resize = signal.revents & libc::POLLIN != 0 && self.read_resize()?;

        Ok(Ready {
            input: input.revents & (libc::POLLIN | libc::POLLHUP | libc::POLLERR) != 0,
            resize,
        })
    }

    fn read_resize(&self) -> io::Result<bool> {
        let mut info = MaybeUninit::<libc::signalfd_siginfo>::uninit();
        let size = mem::size_of_val(&info);

        let count =
            unsafe { libc::read(self.signalfd.as_raw_fd(), info.as_mut_ptr().cast(), size) };

        if count < 0 {
            let err = last_os_error();
            return if err.kind() == io::ErrorKind::WouldBlock { Ok(false) } else { Err(err) };
        }
        if count as usize != size {
            return Err(io::Error::new(io::ErrorKind::UnexpectedEof, "incomplete signal record"));
        }

        Ok(true)
    }
}

impl Drop for Events {
    fn drop(&mut self) {
        unsafe {
            libc::pthread_sigmask(libc::SIG_SETMASK, &self.previous_mask, ptr::null_mut());
        }
    }
}
