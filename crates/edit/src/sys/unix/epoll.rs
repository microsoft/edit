use std::ffi::c_int;
use std::{io, mem};

use super::*;

const SIGNAL_TOKEN: u64 = u64::MAX;

pub struct Registry {
    queue: c_int,
    capacity: usize,

    signalfd: c_int,
    signalmask: libc::sigset_t,
    signal_set: SignalSet,
}

impl Registry {
    pub fn new() -> io::Result<Self> {
        unsafe {
            let queue = libc::epoll_create1(libc::EPOLL_CLOEXEC).check()?;

            let mut signalmask = mem::zeroed();
            libc::sigemptyset(&mut signalmask).check()?;

            Ok(Self {
                queue,
                capacity: 0,
                signalfd: -1,
                signalmask,
                signal_set: Default::default(),
            })
        }
    }

    fn read_signals<'a>(&mut self, arena: &'a Arena) -> io::Result<BVec<'a, Token>> {
        let scratch = scratch_arena(Some(arena));
        let buffer = scratch.alloc_uninit_array::<16, libc::signalfd_siginfo>(); // 2KiB!

        loop {
            let count = unsafe {
                libc::read(self.signalfd, buffer.as_mut_ptr().cast(), mem::size_of_val(buffer))
            };

            if count > 0 {
                let count = count as usize;
                if !count.is_multiple_of(mem::size_of::<libc::signalfd_siginfo>()) {
                    return Err(from_raw_os_error(libc::EINVAL));
                }
                let count = count / mem::size_of::<libc::signalfd_siginfo>();

                let records = unsafe { buffer[..count].assume_init_ref() };
                for record in records {
                    self.signal_set.flag(record.ssi_signo as c_int);
                }
                continue;
            }

            let err = if count == 0 { libc::EPIPE } else { errno() };
            match err {
                libc::EINTR => continue,
                libc::EAGAIN => break,
                _ => return Err(from_raw_os_error(err)),
            }
        }

        let mut tokens = BVec::empty();
        tokens.extend_sloppy(arena, self.signal_set.fetch_flagged());
        Ok(tokens)
    }

    fn register_fd_impl(&mut self, token: u64, fd: c_int, interest: Interest) -> io::Result<()> {
        let mut event = libc::epoll_event { events: 0, u64: token };
        if interest.readable() {
            event.events |= libc::EPOLLIN as u32;
        }
        if interest.writable() {
            event.events |= libc::EPOLLOUT as u32;
        }

        unsafe { libc::epoll_ctl(self.queue, libc::EPOLL_CTL_ADD, fd, &mut event).check()? };

        self.capacity = (self.capacity + 1).min(c_int::MAX as usize);
        Ok(())
    }
}

impl Registrable for Registry {
    fn register_fd(&mut self, token: Token, fd: c_int, interest: Interest) -> io::Result<()> {
        assert!(token.0 as u64 != SIGNAL_TOKEN);
        self.register_fd_impl(token.0 as u64, fd, interest)
    }

    fn register_signals(&mut self, assocs: &[(Token, c_int)]) -> io::Result<()> {
        unsafe {
            let mut changed = false;
            for &(token, signum) in assocs {
                if self.signal_set.add(signum, token) {
                    libc::sigaddset(&mut self.signalmask, signum).check()?;
                    changed = true;
                }
            }
            if !changed {
                return Ok(());
            }

            let ret = libc::signalfd(
                self.signalfd,
                &self.signalmask,
                libc::SFD_NONBLOCK | libc::SFD_CLOEXEC,
            )
            .check()?;
            if self.signalfd == -1 {
                self.signalfd = ret;
                self.register_fd_impl(SIGNAL_TOKEN, ret, Interest::READABLE)?;
            }

            let mut previous = mem::MaybeUninit::uninit();
            libc::pthread_sigmask(libc::SIG_BLOCK, &self.signalmask, previous.as_mut_ptr())
                .check()?;
            Ok(())
        }
    }

    fn wait<'a>(&mut self, arena: &'a Arena, timeout: time::Duration) -> io::Result<&'a [Token]> {
        let mut timeout = WaitTimeout::new(timeout);
        let capacity = self.capacity;
        let mut events = BVec::<libc::epoll_event>::empty();
        events.reserve(arena, capacity);
        loop {
            let count = unsafe {
                libc::epoll_wait(
                    self.queue,
                    events.as_mut_ptr(),
                    capacity as c_int,
                    timeout.millis(),
                )
            };
            if count < 0 {
                match errno() {
                    libc::EINTR => {}
                    err => return Err(from_raw_os_error(err)),
                }
            } else if count > 0 {
                unsafe { events.set_len(count as usize) };
                let mut tokens = if events.iter().any(|e| e.u64 == SIGNAL_TOKEN) {
                    self.read_signals(arena)?
                } else {
                    BVec::empty()
                };
                for event in events.iter() {
                    if event.u64 != SIGNAL_TOKEN {
                        tokens.push(arena, Token(event.u64 as usize));
                    }
                }
                if !tokens.is_empty() {
                    return Ok(tokens.leak());
                }
            }
            if !timeout.retry() {
                return Ok(&[]);
            }
        }
    }
}
