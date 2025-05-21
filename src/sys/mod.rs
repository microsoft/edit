// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! Platform abstractions.

#[cfg(unix)]
mod unix;
#[cfg(windows)]
mod windows;

#[cfg(not(windows))]
pub use std::fs::canonicalize;

#[cfg(unix)]
pub use unix::*;
#[cfg(windows)]
pub use windows::*;