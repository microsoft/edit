#![feature(allocator_api)]
#![allow(clippy::missing_transmute_annotations, clippy::new_without_default)]

#[macro_use]
pub mod arena;

pub mod apperr;
pub mod buffer;
pub mod cell;
pub mod framebuffer;
pub mod fuzzy;
pub mod helpers;
pub mod icu;
pub mod input;
pub mod loc;
pub mod simd;
pub mod sys;
pub mod tui;
pub mod ucd;
pub mod ucd_gen;
pub mod utf8;
pub mod vt;
