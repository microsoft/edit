use std::fmt::Debug;

#[derive(Default, Clone, Copy, PartialEq, Eq)]
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

pub struct Language {
    #[allow(dead_code)]
    pub name: &'static str,
    pub extensions: &'static [&'static str],
    pub states: &'static [State],
}

pub struct State {
    pub name: &'static str,
    pub rules: &'static [Rule],
}

pub type Rule = (&'static str, HighlightKind, ActionDefinition);

#[derive(Debug, Clone, Copy)]
pub enum ActionDefinition {
    Push(&'static str), // state name to push
    Pop,
}
