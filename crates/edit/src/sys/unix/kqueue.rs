use std::ffi::c_int;
use std::os::fd::{AsRawFd as _, FromRawFd as _, OwnedFd};
use std::{io, mem, ptr};

use super::*;

pub struct Registry {
    queue: OwnedFd,
    signals: Vec<(c_int, libc::sigaction)>,
    fds: Vec<(c_int, Interest)>,
}

impl Registry {
    pub fn new() -> io::Result<Self> {
        unsafe {
            let queue = OwnedFd::from_raw_fd(libc::kqueue().check()?);
            libc::fcntl(queue.as_raw_fd(), libc::F_SETFD, libc::FD_CLOEXEC).check()?;
            Ok(Self { queue, signals: Vec::new(), fds: Vec::new() })
        }
    }

    fn register(&self, changes: &[libc::kevent]) -> io::Result<()> {
        unsafe {
            libc::kevent(
                self.queue.as_raw_fd(),
                changes.as_ptr(),
                changes.len() as c_int,
                ptr::null_mut(),
                0,
                ptr::null(),
            )
            .check()?;
        }
        Ok(())
    }
}

impl Registrable for Registry {
    fn register_fd(&mut self, token: Token, fd: c_int, interest: Interest) -> io::Result<()> {
        if fd < 0 {
            return Err(from_raw_os_error(libc::EBADF));
        }
        let mut changes = [unsafe { mem::zeroed::<libc::kevent>() }; 2];
        let mut count = 0;
        let index = self.fds.iter().position(|&(registered, _)| registered == fd);
        let previous = index.map(|i| self.fds[i].1);
        for (enabled, was_enabled, filter) in [
            (interest.readable(), previous.is_some_and(Interest::readable), libc::EVFILT_READ),
            (interest.writable(), previous.is_some_and(Interest::writable), libc::EVFILT_WRITE),
        ] {
            if enabled || was_enabled {
                let event = &mut changes[count];
                event.ident = fd as _;
                event.filter = filter;
                event.flags = if enabled { libc::EV_ADD } else { libc::EV_DELETE };
                event.udata = token.0 as _;
                count += 1;
            }
        }
        self.register(&changes[..count])?;
        if let Some(index) = index {
            self.fds[index].1 = interest;
        } else {
            self.fds.push((fd, interest));
        }
        Ok(())
    }

    fn register_signal(&mut self, token: Token, signum: c_int) -> io::Result<()> {
        unsafe {
            // Query first to validate the signal before changing the queue or disposition.
            let mut previous = mem::zeroed();
            libc::sigaction(signum, ptr::null(), &mut previous).check()?;
            if signum == libc::SIGKILL || signum == libc::SIGSTOP {
                return Err(from_raw_os_error(libc::EINVAL));
            }
            let mut event: libc::kevent = mem::zeroed();
            event.ident = signum as _;
            event.filter = libc::EVFILT_SIGNAL;
            event.flags = libc::EV_ADD | libc::EV_CLEAR;
            event.udata = token.0 as _;
            self.register(std::slice::from_ref(&event))?;

            if !self.signals.iter().any(|&(registered, _)| registered == signum) {
                let mut action: libc::sigaction = mem::zeroed();
                libc::sigemptyset(&mut action.sa_mask).check()?;
                action.sa_sigaction = signal_handler as *const () as libc::sighandler_t;
                if libc::sigaction(signum, &action, ptr::null_mut()) < 0 {
                    let err = last_os_error();
                    event.flags = libc::EV_DELETE;
                    self.register(std::slice::from_ref(&event))?;
                    return Err(err);
                }
                self.signals.push((signum, previous));
            }
            Ok(())
        }
    }

    fn wait<'a>(&mut self, arena: &'a Arena, timeout: time::Duration) -> io::Result<&'a [Token]> {
        let mut timeout = WaitTimeout::new(timeout);
        let capacity = (self.fds.len() * 2 + self.signals.len()).max(1);
        let mut events = BVec::<libc::kevent>::empty();
        events.reserve(arena, capacity);
        loop {
            let timespec = (timeout.remaining != time::Duration::MAX).then(|| libc::timespec {
                tv_sec: timeout.remaining.as_secs().min(libc::time_t::MAX as u64) as _,
                tv_nsec: timeout.remaining.subsec_nanos() as _,
            });
            let count = unsafe {
                libc::kevent(
                    self.queue.as_raw_fd(),
                    ptr::null(),
                    0,
                    events.as_mut_ptr(),
                    capacity.min(c_int::MAX as usize) as c_int,
                    timespec.as_ref().map_or(ptr::null(), ptr::from_ref),
                )
            };
            if count < 0 {
                match errno() {
                    libc::EINTR => {}
                    err => return Err(from_raw_os_error(err)),
                }
            } else if count > 0 {
                unsafe { events.set_len(count as usize) };
                let mut tokens = BVec::empty();
                for event in events.iter() {
                    if event.flags & libc::EV_ERROR != 0 {
                        return Err(from_raw_os_error(event.data as c_int));
                    }
                    let token = Token(event.udata as usize);
                    if !tokens.contains(&token) {
                        tokens.push(arena, token);
                    }
                }
                return Ok(tokens.leak());
            }
            if !timeout.retry() {
                return Ok(&[]);
            }
        }
    }
}

// EVFILT_SIGNAL observes delivery; a handler suppresses the default action without SIG_IGN's
// special SIGCHLD semantics (which would discard child exit status).
extern "C" fn signal_handler(_: c_int) {}

impl Drop for Registry {
    fn drop(&mut self) {
        for (signum, action) in &self.signals {
            if unsafe { libc::sigaction(*signum, action, ptr::null_mut()) } < 0 {
                eprintln!("failed to restore signal handler: {}", last_os_error());
            }
        }
    }
}
