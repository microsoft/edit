// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! Arena allocators. Small and fast.

#![cfg_attr(
    target_arch = "loongarch64",
    feature(stdarch_loongarch),
)]

pub mod alloc;
pub mod arena;
pub mod collections;
pub mod float;
pub mod glob;
mod helpers;
pub mod simd;
pub mod sys;
pub mod unicode;

pub use helpers::*;
