use std::fmt::Debug;
use std::ops::{Index, IndexMut};

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum HighlightKind {
    #[default]
    Other,
    Comment,
    Number,
    String,
    Variable,
    Operator,
    Keyword,
    Method,
}

pub struct LanguageDefinition {
    #[allow(dead_code)]
    pub name: &'static str,
    pub extensions: &'static [&'static str],
    pub states: &'static [StateDefinition],
}

pub struct StateDefinition {
    pub name: &'static str,
    pub rules: &'static [Rule],
}

pub type Rule = (&'static str, HighlightKind, ActionDefinition);

#[derive(Debug, Clone, Copy)]
pub enum ActionDefinition {
    Push(&'static str), // state name to push
    Pop,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Charset([bool; 256]);

impl Charset {
    pub fn fill(&mut self, value: bool) {
        self.0.fill(value);
    }

    pub fn merge(&mut self, other: &Charset) {
        for (a, b) in self.0.iter_mut().zip(other.0.iter()) {
            *a |= *b;
        }
    }

    pub fn merge_str(&mut self, s: &str) {
        for b in s.as_bytes() {
            self.0[*b as usize] = true;
        }
    }

    pub fn merge_str_insensitive(&mut self, s: &str) {
        for b in s.as_bytes() {
            self.0[b.to_ascii_uppercase() as usize] = true;
            self.0[b.to_ascii_lowercase() as usize] = true;
        }
    }

    pub fn is_any(&self) -> bool {
        self.0.iter().all(|&b| b)
    }

    pub fn is_superset(&self, other: &Charset) -> bool {
        for (a, b) in self.0.iter().zip(other.0.iter()) {
            if *b && !*a {
                return false;
            }
        }
        true
    }

    pub fn iter(&self) -> impl Iterator<Item = bool> + '_ {
        self.0.iter().copied()
    }
}

impl Default for Charset {
    fn default() -> Self {
        Charset([false; 256])
    }
}

impl Debug for Charset {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let show_char = |f: &mut std::fmt::Formatter<'_>, b: usize| {
            let b = b as u8;
            if b.is_ascii_graphic() || b == b' ' {
                let b = b as char;
                write!(f, "'{b}'")
            } else {
                write!(f, "0x{b:02X}")
            }
        };

        let mut beg = 0;
        let mut first = true;

        write!(f, "[")?;

        while beg < 256 {
            while beg < 256 && !self.0[beg] {
                beg += 1;
            }
            if beg >= 256 {
                break;
            }

            let mut end = beg;
            while end < 256 && self.0[end] {
                end += 1;
            }

            if !first {
                write!(f, ", ")?;
            }
            show_char(f, beg)?;
            if end - beg > 1 {
                write!(f, "-")?;
                show_char(f, end - 1)?;
            }

            beg = end;
            first = false;
        }

        write!(f, "]")
    }
}

impl<I> Index<I> for Charset
where
    [bool]: Index<I>,
{
    type Output = <[bool] as Index<I>>::Output;

    #[inline]
    fn index(&self, index: I) -> &Self::Output {
        self.0.index(index)
    }
}

impl<I> IndexMut<I> for Charset
where
    [bool]: IndexMut<I>,
{
    #[inline]
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        self.0.index_mut(index)
    }
}

#[derive(Debug)]
pub struct WipState {
    name: Option<&'static str>,

    /// The transitions leading away from this state.
    transitions: Vec<WipTransition>,

    // All of these members are used to find fallback states.
    // The idea is that we find the shallowest (closest to the root) state
    // that has a superset of the characters that led to this state.
    /// Depth of this node.
    pub depth: usize,
    pub shallowest_parent: Option<usize>,
    /// A fallback state must be a superset of this charset.
    pub required_charset: Charset,
    pub fallback_done: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WipAction {
    // Same as super::Action
    Change(usize),
    Push(usize),
    Pop,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WipConsume {
    // Same as super::Consume
    Chars(usize),
    Prefix(String),
    PrefixInsensitive(String),
    Charset(Box<Charset>),
}

#[derive(Debug, Clone)]
pub struct WipTransition {
    pub test: WipConsume,
    pub kind: HighlightKind,
    pub action: WipAction,
}
