use std::cell::RefCell;
use std::collections::HashSet;
use std::fmt;
use std::fmt::Write as _;
use std::ops::{Index, IndexMut};
use std::path::{Path, PathBuf};
use std::str::FromStr;

use stdext::arena::*;

pub fn arena_clone_str<'a>(arena: &'a Arena, s: &str) -> &'a str {
    ArenaString::from_str(arena, s).leak()
}

pub trait Intern<'a, T: ?Sized> {
    fn intern(&mut self, arena: &'a Arena, item: &T) -> &'a T;
}

impl<'a> Intern<'a, str> for Vec<&'a str> {
    fn intern(&mut self, arena: &'a Arena, value: &str) -> &'a str {
        if let Some(&s) = self.iter().find(|&&v| v == value) {
            s
        } else {
            let s = arena_clone_str(arena, value);
            self.push(s);
            s
        }
    }
}
