// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#![feature(
    allocator_api,
    breakpoint,
    cold_path,
    let_chains,
    linked_list_cursors,
    maybe_uninit_fill,
    maybe_uninit_slice,
    maybe_uninit_uninit_array_transpose,
    os_string_truncate,
    box_vec_non_null,
    ptr_metadata
)]
#![allow(clippy::missing_transmute_annotations, clippy::new_without_default, stable_features)]

#[macro_use]
pub mod arena;

pub mod apperr;
pub mod base64;
pub mod buffer;
pub mod cell;
pub mod document;
pub mod framebuffer;
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
