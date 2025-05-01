#![feature(
    allocator_api,
    breakpoint,
    linked_list_cursors,
    maybe_uninit_uninit_array_transpose,
    os_string_truncate
)]
#![allow(clippy::missing_transmute_annotations, clippy::new_without_default)]

#[macro_use]
pub mod arena;

pub mod apperr;
pub mod base64;
pub mod buffer;
mod cell;
pub mod framebuffer;
pub mod helpers;
pub mod icu;
pub mod input;
pub mod simd;
pub mod sys;
pub mod tui;
pub mod ucd;
mod ucd_gen;
pub mod utf8;
pub mod vt;
