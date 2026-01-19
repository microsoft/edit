// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! Arena allocators. Small and fast.

#[cfg(debug_assertions)]
mod debug;
mod fs;
mod release;
mod scratch;
mod string;

#[cfg(all(not(doc), debug_assertions))]
pub use self::debug::*;
pub use self::fs::*;
#[cfg(any(doc, not(debug_assertions)))]
pub use self::release::*;
pub use self::scratch::*;
pub use self::string::*;
