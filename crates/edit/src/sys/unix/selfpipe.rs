use std::sync::atomic::{AtomicBool, Ordering};

use super::*;

extern "C" fn resize_handler(_: c_int) {}

pub struct Events {}

impl Events {
    pub fn new() -> io::Result<Self> {}

    pub fn wait(&self, stdin: c_int, timeout: Duration) -> io::Result<Ready> {}
}
