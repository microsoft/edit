// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! Platform abstractions.

mod unix;

pub use std::fs::canonicalize;
pub use unix::*;
