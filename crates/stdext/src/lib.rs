// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! Arena allocators. Small and fast.

#![feature(allocator_api)]

pub mod arena;
pub mod sys;

mod helpers;
pub use helpers::*;
