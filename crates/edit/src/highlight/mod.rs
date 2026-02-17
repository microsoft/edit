// Copyright (c) iEdit contributors.
// Licensed under the MIT License.

//! Syntax highlighting module.
//!
//! Provides lightweight, state-machine-based tokenizers for syntax highlighting.
//! Each language implements line-at-a-time tokenization with a persistent `u8`
//! state to handle multi-line constructs (block comments, heredocs, etc.).

mod conf;
mod yaml;

use crate::framebuffer::IndexedColor;
use crate::oklab::StraightRgba;

/// Supported languages for syntax highlighting.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Language {
    Yaml,
    Conf,
}

/// Detects the language from a file name based on extension or well-known names.
pub fn detect_language(filename: &str) -> Option<Language> {
    // Match by well-known config file names (no extension).
    let basename = filename.rsplit('/').next().unwrap_or(filename);
    match basename {
        "config" | "credentials" | ".gitconfig" | ".editorconfig" | ".npmrc" | ".pylintrc"
        | ".flake8" | "setup.cfg" | "tox.ini" | "my.cnf" | "php.ini" | "pgpass" => {
            return Some(Language::Conf);
        }
        _ => {}
    }

    let ext = filename.rsplit('.').next()?;
    match ext {
        "yml" | "yaml" => Some(Language::Yaml),
        "conf" | "ini" | "cfg" | "cnf" | "properties" | "env" | "toml" => Some(Language::Conf),
        _ => None,
    }
}

/// The kind of a syntax token, used to determine its color.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenKind {
    Default,
    Key,
    String,
    Comment,
    Number,
    Boolean,
    Null,
    Punctuation,
    Anchor,
    Tag,
    Section,
}

/// A token produced by the tokenizer, representing a colored span of text.
#[derive(Clone, Copy)]
pub struct Token {
    /// Byte offset from the start of the line.
    pub offset: usize,
    /// Length in bytes.
    pub len: usize,
    /// The kind of token.
    pub kind: TokenKind,
}

/// Maps a token kind to a foreground color using the terminal's indexed color palette.
pub fn token_color(kind: TokenKind, colors: &[StraightRgba]) -> StraightRgba {
    let idx = match kind {
        TokenKind::Default => IndexedColor::Foreground,
        TokenKind::Key => IndexedColor::Blue,
        TokenKind::String => IndexedColor::Green,
        TokenKind::Comment => IndexedColor::BrightBlack,
        TokenKind::Number => IndexedColor::Cyan,
        TokenKind::Boolean => IndexedColor::Magenta,
        TokenKind::Null => IndexedColor::BrightMagenta,
        TokenKind::Punctuation => IndexedColor::BrightBlack,
        TokenKind::Anchor => IndexedColor::Yellow,
        TokenKind::Tag => IndexedColor::BrightCyan,
        TokenKind::Section => IndexedColor::Yellow,
    };
    colors[idx as usize]
}

/// Tokenizes a line for the given language.
///
/// `state` is the tokenizer state from the end of the previous line (0 for the first line).
/// Returns the new state at the end of this line.
pub fn tokenize_line(lang: Language, line: &[u8], state: u8, tokens: &mut Vec<Token>) -> u8 {
    match lang {
        Language::Yaml => yaml::tokenize_line(line, state, tokens),
        Language::Conf => conf::tokenize_line(line, state, tokens),
    }
}

/// Holds the highlighting state for a document.
pub struct HighlightState {
    pub language: Language,
    /// Per-line tokenizer state. Index = logical line number. Value = state at the *start* of that line.
    pub line_states: Vec<u8>,
}

impl HighlightState {
    pub fn new(language: Language) -> Self {
        Self { language, line_states: vec![0] }
    }

    /// Recomputes line states starting from `from_line`.
    /// Call this after edits to update multi-line state propagation.
    pub fn recompute_states(
        &mut self,
        from_line: usize,
        get_line: &dyn Fn(usize) -> Option<Vec<u8>>,
    ) {
        if from_line >= self.line_states.len() {
            return;
        }

        let mut state = self.line_states[from_line];
        let mut tokens = Vec::new();
        let mut line_idx = from_line;

        while let Some(line) = get_line(line_idx) {
            state = tokenize_line(self.language, &line, state, &mut tokens);
            tokens.clear();

            let next = line_idx + 1;
            if next < self.line_states.len() {
                if self.line_states[next] == state {
                    break; // State converged, no need to continue.
                }
                self.line_states[next] = state;
            } else {
                self.line_states.push(state);
            }
            line_idx = next;
        }
    }
}
