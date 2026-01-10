// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! Arena allocators. Small and fast.

#![feature(allocator_api)]

pub mod arena;
pub mod sys;
pub mod varint;

mod helpers;
pub use helpers::*;
