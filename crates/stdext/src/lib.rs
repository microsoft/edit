// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! Arena allocators. Small and fast.

#![feature(allocator_api)]

pub mod arena;
mod helpers;
pub mod sys;

pub use helpers::*;
