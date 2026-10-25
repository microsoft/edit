use std::ffi::c_int;
use std::sync::atomic::{AtomicI32, Ordering};
use std::{io, mem};

use super::*;

static SIGNAL_PIPE: AtomicI32 = AtomicI32::new(-1);

#[derive(Default)]
pub struct Registry {
    fds: Vec<libc::pollfd>,
    tokens: Vec<Token>,
    signal_set: SignalSet,
}

impl Registry {
    pub fn new() -> io::Result<Self> {
        unsafe {
            debug_assert!(SIGNAL_PIPE.load(Ordering::Relaxed) == -1);

            let mut s = Self::default();

            // Prepare and register the signal pipe. This ensures it has slot index 0,
            // which we use to identify via `poll()` that events are ready.
            // This could be done lazily, but msedit needs signals anyway.
            let mut pipe = [-1, -1];
            libc::pipe2(pipe.as_mut_ptr(), libc::O_CLOEXEC | libc::O_NONBLOCK).check()?;
            SIGNAL_PIPE.store(pipe[1], Ordering::Relaxed);
            s.register_fd(Token(0), pipe[0], Interest::READABLE)?;

            Ok(s)
        }
    }

    fn read_signals<'a>(&mut self, arena: &'a Arena) -> io::Result<BVec<'a, Token>> {
        let mut buffer = [const { MaybeUninit::<u8>::uninit() }; 32];
        let fd = self.fds[0].fd;

        loop {
            let count = unsafe { libc::read(fd, buffer.as_mut_ptr().cast(), buffer.len()) };

            if count > 0 {
                // Mark the corresponding signals as flagged.
                // The flagging system ensures that we coalesce signals.
                let events = unsafe { buffer[..count as usize].assume_init_ref() };
                for &b in events {
                    self.signal_set.flag(b as c_int);
                }

                // POSIX does not guarantee atomicity and permits short reads on pipes,
                // so looping is required, despite this successful read.
                continue;
            }

            // Retry as needed. Exit once it's empty.
            let err = if count == 0 { libc::EPIPE } else { errno() };
            match err {
                libc::EINTR => continue,
                _ if err == libc::EAGAIN || err == libc::EWOULDBLOCK => break,
                _ => return Err(io::Error::from_raw_os_error(err)),
            }
        }

        // Collect all flagged signals.
        let mut tokens = BVec::empty();
        tokens.extend_sloppy(arena, self.signal_set.fetch_flagged());
        Ok(tokens)
    }
}

impl Registrable for Registry {
    fn register_fd(&mut self, token: Token, fd: c_int, interest: Interest) -> io::Result<()> {
        let mut events = 0;
        if interest.readable() {
            events |= libc::POLLIN;
        }
        if interest.writable() {
            events |= libc::POLLOUT;
        }
        self.fds.push(libc::pollfd { fd, events, revents: 0 });
        self.tokens.push(token);
        Ok(())
    }

    fn register_signals(&mut self, assocs: &[(Token, c_int)]) -> io::Result<()> {
        unsafe {
            for &(token, signum) in assocs {
                if self.signal_set.add(signum, token) {
                    let mut oldact = mem::MaybeUninit::uninit();
                    let mut act: libc::sigaction = mem::zeroed();
                    libc::sigemptyset(&mut act.sa_mask);
                    act.sa_sigaction = signal_handler as *const () as libc::sighandler_t;
                    libc::sigaction(signum, &act, oldact.as_mut_ptr()).check()?;
                }
            }
            Ok(())
        }
    }

    fn wait<'a>(&mut self, arena: &'a Arena, timeout: time::Duration) -> io::Result<&'a [Token]> {
        let mut timeout = WaitTimeout::new(timeout);
        // poll(), obviously.
        loop {
            let ready =
                unsafe { libc::poll(self.fds.as_mut_ptr(), self.fds.len() as _, timeout.millis()) };
            if ready > 0 {
                break;
            }
            if ready < 0 {
                match errno() {
                    libc::EINTR | libc::EAGAIN => {}
                    err => return Err(from_raw_os_error(err)),
                }
            }
            if !timeout.retry() {
                return Ok(&[]);
            }
        }

        // Invalid fd? Bad.
        if self.fds.iter().any(|fd| fd.revents & libc::POLLNVAL != 0) {
            return Err(from_raw_os_error(libc::EBADF));
        }

        // Collect signal wakeups, if any.
        let mut tokens =
            if self.fds[0].revents != 0 { self.read_signals(arena)? } else { BVec::empty() };

        // Collect ready fds.
        for (fd, token) in self.fds[1..].iter().zip(&self.tokens[1..]) {
            if fd.revents & (fd.events | libc::POLLERR | libc::POLLHUP) != 0 {
                tokens.push(arena, *token);
            }
        }

        Ok(tokens.leak())
    }
}

extern "C" fn signal_handler(signo: c_int) {
    unsafe {
        let Ok(val) = u8::try_from(signo) else {
            return;
        };

        let saved_errno = errno();
        let fd = SIGNAL_PIPE.load(Ordering::Relaxed);

        if fd >= 0 {
            while libc::write(fd, (&raw const val).cast(), 1) < 0 && errno() == libc::EINTR {}
        }

        set_errno(saved_errno);
    }
}
