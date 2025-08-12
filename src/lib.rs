// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#![feature(
    allocator_api,
    breakpoint,
    cold_path,
    linked_list_cursors,
    maybe_uninit_fill,
    maybe_uninit_slice,
    maybe_uninit_uninit_array_transpose
)]
#![cfg_attr(
    target_arch = "loongarch64",
    feature(stdarch_loongarch, stdarch_loongarch_feature_detection, loongarch_target_feature),
    allow(clippy::incompatible_msrv)
)]
#![allow(clippy::missing_transmute_annotations, clippy::new_without_default, stable_features)]

#[macro_use]
pub mod arena;

pub mod apperr;
pub mod base64;
pub mod buffer;
pub mod cell;
pub mod clipboard;
pub mod document;
pub mod framebuffer;
pub mod fuzzy;
pub mod hash;
pub mod helpers;
pub mod icu;
pub mod input;
pub mod oklab;
pub mod path;
pub mod simd;
pub mod sys;
pub mod tui;
pub mod unicode;
pub mod vt;
