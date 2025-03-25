#![allow(
    dead_code,
    clippy::needless_if,
    clippy::uninit_assumed_init,
    clippy::missing_transmute_annotations
)]

pub mod apperr;
pub mod buffer;
pub mod framebuffer;
pub mod fuzzy;
pub mod helpers;
pub mod icu;
pub mod input;
pub mod loc;
pub mod memchr;
pub mod sys;
pub mod trust_me_bro;
pub mod tui;
pub mod ucd;
pub mod ucd_gen;
pub mod utf8;
pub mod vt;
