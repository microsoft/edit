// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! Arena allocators. Small and fast.

#[cfg(debug_assertions)]
mod debug;
mod fs;
mod release;
mod scratch;

#[cfg(all(not(doc), debug_assertions))]
pub use self::debug::*;
pub use self::fs::*;
#[cfg(any(doc, not(debug_assertions)))]
pub use self::release::*;
pub use self::scratch::*;

#[macro_export]
macro_rules! arena_format {
    ($arena:expr, $($arg:tt)*) => {{
        use std::fmt::Write as _;
        let mut output = ::stdext::collections::BString::empty();
        let _ = output.formatter($arena).write_fmt(format_args!($($arg)*));
        output
    }}
}

#[macro_export]
macro_rules! arena_write_fmt {
    ($arena:expr, $output:expr, $($arg:tt)*) => {{
        use std::fmt::Write as _;
        let _ = $output.formatter($arena).write_fmt(format_args!($($arg)*));
    }}
}
