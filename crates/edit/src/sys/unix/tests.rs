#![allow(static_mut_refs)]

use std::os::fd::{AsRawFd as _, OwnedFd};
use std::process::Command;
use std::sync::mpsc;
use std::thread;
use std::time::{Duration, Instant};

use super::*;

extern "C" fn ignore_signal(_: c_int) {}

#[test]
fn terminal_events() {
    if std::env::var_os("EDIT_TEST_TERMINAL_EVENTS").is_none() {
        let output = Command::new(std::env::current_exe().unwrap())
            .args(["--exact", "sys::unix::tests::terminal_events", "--nocapture"])
            .env("EDIT_TEST_TERMINAL_EVENTS", "1")
            .output()
            .unwrap();

        assert!(
            output.status.success(),
            "{}\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr),
        );
        return;
    }

    unsafe {
        let mut original_mask = mem::zeroed();
        assert_eq!(libc::pthread_sigmask(libc::SIG_SETMASK, ptr::null(), &mut original_mask), 0);

        let mut original_action: libc::sigaction = mem::zeroed();
        assert_eq!(libc::sigaction(libc::SIGWINCH, ptr::null(), &mut original_action), 0);

        let deinit = init().unwrap();
        let (finished, completion) = mpsc::channel();
        let watchdog = thread::spawn(move || {
            if completion.recv_timeout(Duration::from_secs(10))
                == Err(mpsc::RecvTimeoutError::Timeout)
            {
                eprintln!("terminal event test hung");
                std::process::exit(1);
            }
        });

        let mut size = libc::winsize { ws_row: 40, ws_col: 100, ws_xpixel: 0, ws_ypixel: 0 };
        let mut master = -1;
        let mut slave = -1;
        assert_eq!(
            libc::openpty(&mut master, &mut slave, ptr::null_mut(), ptr::null_mut(), &raw mut size),
            0,
        );
        let master = OwnedFd::from_raw_fd(master);
        let slave = OwnedFd::from_raw_fd(slave);
        let stdout = OwnedFd::from_raw_fd(
            check_int_return(libc::fcntl(libc::STDOUT_FILENO, libc::F_DUPFD_CLOEXEC, 0)).unwrap(),
        );

        assert_eq!(libc::dup2(slave.as_raw_fd(), libc::STDOUT_FILENO), libc::STDOUT_FILENO);
        STATE.stdin = slave.as_raw_fd();

        let original_flags = check_int_return(libc::fcntl(STATE.stdin, libc::F_GETFL)).unwrap();
        let mut original_termios: libc::termios = mem::zeroed();
        assert_eq!(libc::tcgetattr(STATE.stdin, &mut original_termios), 0);

        switch_modes().unwrap();
        assert_ne!(libc::fcntl(STATE.stdin, libc::F_GETFL) & libc::O_NONBLOCK, 0);

        let arena = Arena::new(MEBI).unwrap();
        let (resize, input) = read_stdin(&arena, Duration::ZERO).unwrap();
        assert!(resize.is_none());
        assert!(input.is_empty());

        assert_eq!(libc::write(master.as_raw_fd(), b"hello".as_ptr().cast(), 5), 5);
        let (resize, input) = read_stdin(&arena, Duration::MAX).unwrap();
        assert!(resize.is_none());
        assert_eq!(&*input, "hello");

        assert_eq!(libc::write(master.as_raw_fd(), b"\xf0\x9f".as_ptr().cast(), 2), 2);
        let (_, input) = read_stdin(&arena, Duration::MAX).unwrap();
        assert!(input.is_empty());

        assert_eq!(libc::raise(libc::SIGWINCH), 0);
        let (resize, input) = read_stdin(&arena, Duration::MAX).unwrap();
        let resize = resize.unwrap();
        assert_eq!((resize.width, resize.height), (100, 40));
        assert!(input.is_empty());
        assert_eq!(STATE.utf8_len, 2);

        assert_eq!(libc::write(master.as_raw_fd(), b"\x98\x80".as_ptr().cast(), 2), 2);
        let (_, input) = read_stdin(&arena, Duration::MAX).unwrap();
        assert_eq!(&*input, "\u{1f600}");

        let reader = libc::pthread_self() as usize;
        let sender = thread::spawn(move || {
            thread::sleep(Duration::from_millis(20));
            assert_eq!(libc::pthread_kill(reader as libc::pthread_t, libc::SIGWINCH), 0);
        });
        let (resize, input) = read_stdin(&arena, Duration::MAX).unwrap();
        assert!(resize.is_some());
        assert!(input.is_empty());
        sender.join().unwrap();

        for _ in 0..3 {
            assert_eq!(libc::raise(libc::SIGWINCH), 0);
        }
        assert_eq!(libc::write(master.as_raw_fd(), b"x".as_ptr().cast(), 1), 1);
        let ready = STATE.events.as_ref().unwrap().wait(STATE.stdin, None).unwrap();
        assert!(ready.input);
        assert!(ready.resize);

        let mut byte = 0u8;
        assert_eq!(libc::read(STATE.stdin, ptr::from_mut(&mut byte).cast(), 1), 1);
        assert_eq!(libc::read(STATE.stdin, ptr::from_mut(&mut byte).cast(), 1), -1);
        assert_eq!(last_os_error().kind(), io::ErrorKind::WouldBlock);
        let (resize, input) = read_stdin(&arena, Duration::ZERO).unwrap();
        assert!(resize.is_none());
        assert!(input.is_empty());

        let mut action: libc::sigaction = mem::zeroed();
        libc::sigemptyset(&mut action.sa_mask);
        action.sa_sigaction = ignore_signal as *const () as libc::sighandler_t;
        let mut previous_action = mem::zeroed();
        assert_eq!(libc::sigaction(libc::SIGUSR1, &action, &mut previous_action), 0);

        let (stop, stopped) = mpsc::channel();
        let sender = thread::spawn(move || {
            while stopped.recv_timeout(Duration::from_millis(2))
                == Err(mpsc::RecvTimeoutError::Timeout)
            {
                assert_eq!(libc::pthread_kill(reader as libc::pthread_t, libc::SIGUSR1), 0);
            }
        });

        let timeout = Duration::from_millis(40);
        let started = Instant::now();
        let (resize, input) = read_stdin(&arena, timeout).unwrap();
        assert!(started.elapsed() >= timeout);
        assert!(resize.is_none());
        assert!(input.is_empty());

        stop.send(()).unwrap();
        sender.join().unwrap();
        assert_eq!(libc::sigaction(libc::SIGUSR1, &previous_action, ptr::null_mut()), 0);

        drop(deinit);
        drop(Deinit);
        assert!(STATE.events.is_none());
        assert_eq!(libc::fcntl(STATE.stdin, libc::F_GETFL), original_flags);

        let mut termios: libc::termios = mem::zeroed();
        assert_eq!(libc::tcgetattr(STATE.stdin, &mut termios), 0);
        assert_eq!(termios.c_lflag, original_termios.c_lflag);

        let mut original_limit = mem::zeroed();
        assert_eq!(libc::getrlimit(libc::RLIMIT_NOFILE, &mut original_limit), 0);
        let mut exhausted_limit = original_limit;
        exhausted_limit.rlim_cur = 0;
        assert_eq!(libc::setrlimit(libc::RLIMIT_NOFILE, &exhausted_limit), 0);

        let result = Events::new();
        assert_eq!(libc::setrlimit(libc::RLIMIT_NOFILE, &original_limit), 0);
        assert_eq!(result.err().unwrap().raw_os_error(), Some(libc::EMFILE));

        let mut mask = mem::zeroed();
        assert_eq!(libc::pthread_sigmask(libc::SIG_SETMASK, ptr::null(), &mut mask), 0);
        for signal in [libc::SIGWINCH, libc::SIGUSR1, libc::SIGINT] {
            assert_eq!(libc::sigismember(&mask, signal), libc::sigismember(&original_mask, signal));
        }

        assert_eq!(libc::sigaction(libc::SIGWINCH, ptr::null(), &mut action), 0);
        assert_eq!(action.sa_sigaction, original_action.sa_sigaction);
        assert_eq!(libc::dup2(stdout.as_raw_fd(), libc::STDOUT_FILENO), libc::STDOUT_FILENO);

        finished.send(()).unwrap();
        watchdog.join().unwrap();
    }
}

#[test]
fn wait_timeouts() {
    assert!(timeout_timespec(Duration::MAX).is_none());

    let zero = timeout_timespec(Duration::ZERO).unwrap();
    assert_eq!((zero.tv_sec, zero.tv_nsec), (0, 0));

    let fractional = timeout_timespec(Duration::new(2, 123)).unwrap();
    assert_eq!((fractional.tv_sec, fractional.tv_nsec), (2, 123));

    let large = timeout_timespec(Duration::new(u64::MAX - 1, 999_999_999)).unwrap();
    assert_eq!(large.tv_sec, libc::time_t::MAX);
    assert_eq!(large.tv_nsec, 999_999_999);

    let started = Instant::now().checked_sub(Duration::from_secs(1)).unwrap();
    assert_eq!(remaining(Duration::MAX, started), Duration::MAX);
    assert_eq!(remaining(Duration::from_millis(1), started), Duration::ZERO);
}
