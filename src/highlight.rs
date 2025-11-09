// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::ops::Range;
use std::path::Path;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SyntaxKind {
    Plain,
    Rust,
    Cpp,
    Json,
    Toml,
    Shell,
    Python,
    Markdown,
}

impl Default for SyntaxKind {
    fn default() -> Self {
        Self::Plain
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum HighlightClass {
    Keyword,
    Type,
    String,
    Number,
    Comment,
    Macro,
}

#[derive(Clone, Debug)]
pub struct HighlightSpan {
    pub range: Range<usize>,
    pub class: HighlightClass,
}

impl SyntaxKind {
    pub fn from_path(path: Option<&Path>) -> Self {
        let Some(path) = path else {
            return Self::Plain;
        };

        let name = path.file_name().and_then(|s| s.to_str()).unwrap_or("").to_ascii_lowercase();
        let ext = path.extension().and_then(|s| s.to_str()).map(|s| s.to_ascii_lowercase());

        match ext.as_deref() {
            Some("rs") => SyntaxKind::Rust,
            Some("c" | "h" | "cc" | "hh" | "cpp" | "hpp" | "cxx" | "hxx" | "ino") => {
                SyntaxKind::Cpp
            }
            Some("java" | "kt" | "kts" | "swift" | "go") => SyntaxKind::Cpp,
            Some("js" | "jsx" | "ts" | "tsx" | "mjs" | "cjs") => SyntaxKind::Cpp,
            Some("cs") => SyntaxKind::Cpp,
            Some("json" | "jsonc") => SyntaxKind::Json,
            Some("toml") => SyntaxKind::Toml,
            Some("yaml" | "yml") => SyntaxKind::Toml,
            Some("py" | "pyi") => SyntaxKind::Python,
            Some("sh" | "bash" | "zsh" | "fish" | "ps1") => SyntaxKind::Shell,
            Some("md" | "mdx" | "markdown") => SyntaxKind::Markdown,
            _ => match name.as_str() {
                "cargo.toml" | "cargo.lock" | "pyproject.toml" | "poetry.lock" => SyntaxKind::Toml,
                "package.json" | "tsconfig.json" | "composer.json" => SyntaxKind::Json,
                _ => SyntaxKind::Plain,
            },
        }
    }

    pub fn has_highlighting(self) -> bool {
        !matches!(self, SyntaxKind::Plain)
    }

    pub fn indent_after_colon(self) -> bool {
        matches!(self, SyntaxKind::Python)
    }
}

pub fn highlight_line(kind: SyntaxKind, line: &str, out: &mut Vec<HighlightSpan>) {
    out.clear();
    if !kind.has_highlighting() || line.is_empty() {
        return;
    }

    match kind {
        SyntaxKind::Plain => {}
        SyntaxKind::Rust => highlight_rust(line, out),
        SyntaxKind::Cpp => highlight_c_like(line, out, CPP_KEYWORDS, CPP_TYPES, true),
        SyntaxKind::Json => highlight_json(line, out),
        SyntaxKind::Toml => highlight_toml(line, out),
        SyntaxKind::Shell => highlight_shell(line, out),
        SyntaxKind::Python => highlight_python(line, out),
        SyntaxKind::Markdown => highlight_markdown(line, out),
    }
}

const RUST_KEYWORDS: &[&str] = &[
    "as", "async", "await", "break", "const", "continue", "crate", "else", "enum", "extern", "fn",
    "for", "if", "impl", "in", "let", "loop", "match", "mod", "move", "mut", "pub", "ref",
    "return", "Self", "self", "static", "struct", "super", "trait", "type", "unsafe", "use",
    "where", "while",
];

const RUST_TYPES: &[&str] = &[
    "bool", "char", "usize", "isize", "u8", "u16", "u32", "u64", "u128", "i8", "i16", "i32", "i64",
    "i128", "f32", "f64", "String", "Vec", "Option", "Result",
];

const CPP_KEYWORDS: &[&str] = &[
    "alignas",
    "alignof",
    "and",
    "and_eq",
    "asm",
    "auto",
    "bitand",
    "bitor",
    "bool",
    "break",
    "case",
    "catch",
    "char",
    "char8_t",
    "char16_t",
    "char32_t",
    "class",
    "compl",
    "concept",
    "const",
    "consteval",
    "constexpr",
    "constinit",
    "const_cast",
    "continue",
    "co_await",
    "co_return",
    "co_yield",
    "decltype",
    "default",
    "delete",
    "do",
    "double",
    "dynamic_cast",
    "else",
    "enum",
    "explicit",
    "export",
    "extern",
    "false",
    "float",
    "for",
    "friend",
    "goto",
    "if",
    "inline",
    "int",
    "long",
    "mutable",
    "namespace",
    "new",
    "noexcept",
    "not",
    "not_eq",
    "nullptr",
    "operator",
    "or",
    "or_eq",
    "private",
    "protected",
    "public",
    "register",
    "reinterpret_cast",
    "requires",
    "return",
    "short",
    "signed",
    "sizeof",
    "static",
    "static_assert",
    "static_cast",
    "struct",
    "switch",
    "template",
    "this",
    "thread_local",
    "throw",
    "true",
    "try",
    "typedef",
    "typeid",
    "typename",
    "union",
    "unsigned",
    "using",
    "virtual",
    "void",
    "volatile",
    "wchar_t",
    "while",
    "xor",
    "xor_eq",
    "override",
    "final",
];

const CPP_TYPES: &[&str] = &[
    "int8_t",
    "uint8_t",
    "int16_t",
    "uint16_t",
    "int32_t",
    "uint32_t",
    "int64_t",
    "uint64_t",
    "size_t",
    "ptrdiff_t",
    "ssize_t",
    "wint_t",
    "clock_t",
    "time_t",
    "FILE",
    "std",
    "string",
    "wstring",
    "vector",
    "map",
    "set",
    "unordered_map",
    "unordered_set",
    "unique_ptr",
    "shared_ptr",
    "weak_ptr",
    "tuple",
    "array",
    "optional",
    "variant",
    "span",
];

const PYTHON_KEYWORDS: &[&str] = &[
    "and", "as", "assert", "async", "await", "break", "class", "continue", "def", "del", "elif",
    "else", "except", "False", "finally", "for", "from", "global", "if", "import", "in", "is",
    "lambda", "None", "nonlocal", "not", "or", "pass", "raise", "return", "True", "try", "while",
    "with", "yield",
];

fn highlight_c_like(
    line: &str,
    out: &mut Vec<HighlightSpan>,
    keywords: &[&str],
    types: &[&str],
    highlight_macros_flag: bool,
) {
    let bytes = line.as_bytes();
    let comment_idx = find_line_comment(bytes, b'/', b'/');
    let (body, comment) = split_comment(bytes, comment_idx);

    highlight_strings(body, true, true, out);
    let occupied = build_occupied(body.len(), out, &[HighlightClass::String]);
    highlight_numbers_with_mask(body, Some(&occupied), true, out);
    highlight_words_with_mask(body, keywords, HighlightClass::Keyword, Some(&occupied), out);
    highlight_words_with_mask(body, types, HighlightClass::Type, Some(&occupied), out);
    if highlight_macros_flag {
        highlight_macros(body, Some(&occupied), out);
    }
    highlight_preprocessor(bytes, out);

    if let Some(range) = comment {
        push_span(out, range, HighlightClass::Comment);
    }
}

fn highlight_rust(line: &str, out: &mut Vec<HighlightSpan>) {
    let bytes = line.as_bytes();
    let comment_idx = find_line_comment(bytes, b'/', b'/');
    let (body, comment) = split_comment(bytes, comment_idx);

    highlight_rust_strings(body, out);
    highlight_rust_char_literals(body, out);
    let occupied = build_occupied(body.len(), out, &[HighlightClass::String]);
    highlight_numbers_with_mask(body, Some(&occupied), true, out);
    highlight_words_with_mask(body, RUST_KEYWORDS, HighlightClass::Keyword, Some(&occupied), out);
    highlight_words_with_mask(body, RUST_TYPES, HighlightClass::Type, Some(&occupied), out);
    highlight_rust_lifetimes(body, Some(&occupied), out);
    highlight_macros(body, Some(&occupied), out);
    highlight_preprocessor(body, out);

    if let Some(range) = comment {
        push_span(out, range, HighlightClass::Comment);
    }
}

fn highlight_json(line: &str, out: &mut Vec<HighlightSpan>) {
    let bytes = line.as_bytes();
    highlight_strings(bytes, false, true, out);
    let occupied = build_occupied(bytes.len(), out, &[HighlightClass::String]);
    highlight_numbers_with_mask(bytes, Some(&occupied), false, out);
    highlight_words_with_mask(
        bytes,
        &["true", "false", "null"],
        HighlightClass::Keyword,
        Some(&occupied),
        out,
    );
}

fn highlight_toml(line: &str, out: &mut Vec<HighlightSpan>) {
    let bytes = line.as_bytes();
    let comment_idx = find_line_comment(bytes, b'#', b'#');
    let (body, comment) = split_comment(bytes, comment_idx);

    highlight_strings(body, true, true, out);
    let occupied = build_occupied(body.len(), out, &[HighlightClass::String]);
    highlight_numbers_with_mask(body, Some(&occupied), false, out);

    if let Some(range) = find_key_span(body) {
        push_span(out, range, HighlightClass::Keyword);
    }

    if let Some(range) = comment {
        push_span(out, range, HighlightClass::Comment);
    }
}

fn highlight_shell(line: &str, out: &mut Vec<HighlightSpan>) {
    let bytes = line.as_bytes();
    let comment_idx = find_line_comment(bytes, b'#', b'#');
    let (body, comment) = split_comment(bytes, comment_idx);

    highlight_strings(body, true, true, out);
    let occupied = build_occupied(body.len(), out, &[HighlightClass::String]);
    highlight_words_with_mask(
        body,
        &["if", "then", "fi", "for", "in", "do", "done", "case", "esac", "function"],
        HighlightClass::Keyword,
        Some(&occupied),
        out,
    );

    if let Some(range) = comment {
        push_span(out, range, HighlightClass::Comment);
    }
}

fn highlight_python(line: &str, out: &mut Vec<HighlightSpan>) {
    let bytes = line.as_bytes();
    let comment_idx = find_line_comment(bytes, b'#', b'#');
    let (body, comment) = split_comment(bytes, comment_idx);

    highlight_strings(body, true, true, out);
    let occupied = build_occupied(body.len(), out, &[HighlightClass::String]);
    highlight_numbers_with_mask(body, Some(&occupied), true, out);
    highlight_words_with_mask(body, PYTHON_KEYWORDS, HighlightClass::Keyword, Some(&occupied), out);

    if let Some(range) = comment {
        push_span(out, range, HighlightClass::Comment);
    }
}

fn highlight_markdown(line: &str, out: &mut Vec<HighlightSpan>) {
    let trimmed = line.trim_start();
    if trimmed.starts_with('#') {
        push_span(out, 0..line.len(), HighlightClass::Keyword);
        return;
    }

    if trimmed.starts_with("```") {
        push_span(out, 0..line.len(), HighlightClass::Comment);
        return;
    }

    let mut idx = 0;
    let bytes = line.as_bytes();
    while idx < bytes.len() {
        if bytes[idx] == b'`' {
            let start = idx;
            idx += 1;
            while idx < bytes.len() && bytes[idx] != b'`' {
                idx += 1;
            }
            if idx < bytes.len() {
                idx += 1;
            }
            push_span(out, start..idx, HighlightClass::String);
        } else {
            idx += 1;
        }
    }
}

fn highlight_rust_strings(bytes: &[u8], out: &mut Vec<HighlightSpan>) {
    let mut idx = 0;
    while idx < bytes.len() {
        match bytes[idx] {
            b'"' => {
                let end = scan_rust_standard_string(bytes, idx, false).unwrap_or(bytes.len());
                push_span(out, idx..end, HighlightClass::String);
                idx = end;
            }
            b'b' => {
                let Some(next) = bytes.get(idx + 1).copied() else {
                    idx += 1;
                    continue;
                };
                match next {
                    b'"' => {
                        let end =
                            scan_rust_standard_string(bytes, idx + 1, false).unwrap_or(bytes.len());
                        push_span(out, idx..end, HighlightClass::String);
                        idx = end;
                    }
                    b'\'' => {
                        if let Some(end) = scan_rust_standard_string(bytes, idx + 1, true) {
                            push_span(out, idx..end, HighlightClass::String);
                            idx = end;
                        } else {
                            idx += 1;
                        }
                    }
                    b'r' => {
                        if let Some(end) = scan_rust_raw_string(bytes, idx + 2) {
                            push_span(out, idx..end, HighlightClass::String);
                            idx = end;
                        } else {
                            idx += 1;
                        }
                    }
                    _ => idx += 1,
                }
            }
            b'r' => {
                if let Some(end) = scan_rust_raw_string(bytes, idx + 1) {
                    push_span(out, idx..end, HighlightClass::String);
                    idx = end;
                } else {
                    idx += 1;
                }
            }
            _ => idx += 1,
        }
    }
}

fn scan_rust_standard_string(
    bytes: &[u8],
    quote_idx: usize,
    require_terminator: bool,
) -> Option<usize> {
    let &delim = bytes.get(quote_idx)?;
    let mut idx = quote_idx + 1;
    while idx < bytes.len() {
        let b = bytes[idx];
        idx += 1;
        if b == b'\\' && idx < bytes.len() {
            idx += 1;
            continue;
        }
        if b == delim {
            return Some(idx);
        }
    }
    if require_terminator { None } else { Some(bytes.len()) }
}

fn scan_rust_raw_string(bytes: &[u8], mut idx: usize) -> Option<usize> {
    let mut hashes = 0;
    while idx < bytes.len() && bytes[idx] == b'#' {
        idx += 1;
        hashes += 1;
    }
    if idx >= bytes.len() || bytes[idx] != b'"' {
        return None;
    }
    idx += 1; // Skip the opening quote.
    let mut cursor = idx;
    while cursor < bytes.len() {
        if bytes[cursor] == b'"' {
            let mut end = cursor + 1;
            let mut matched = 0;
            while matched < hashes && end < bytes.len() && bytes[end] == b'#' {
                end += 1;
                matched += 1;
            }
            if matched == hashes {
                return Some(end);
            }
        }
        cursor += 1;
    }
    Some(bytes.len())
}

fn highlight_rust_char_literals(bytes: &[u8], out: &mut Vec<HighlightSpan>) {
    let mut idx = 0;
    while idx < bytes.len() {
        if bytes[idx] == b'b' && idx + 1 < bytes.len() && bytes[idx + 1] == b'\'' {
            if let Some(end) = scan_rust_char_literal(bytes, idx + 1) {
                push_span(out, idx..end, HighlightClass::String);
                idx = end;
                continue;
            }
            idx += 1;
            continue;
        }

        if bytes[idx] == b'\'' {
            if let Some(end) = scan_rust_char_literal(bytes, idx) {
                push_span(out, idx..end, HighlightClass::String);
                idx = end;
            } else {
                idx += 1;
            }
            continue;
        }

        idx += 1;
    }
}

fn scan_rust_char_literal(bytes: &[u8], quote_idx: usize) -> Option<usize> {
    let mut idx = quote_idx + 1;
    if idx >= bytes.len() {
        return None;
    }

    if bytes[idx] == b'\\' {
        idx += 1;
        if idx >= bytes.len() {
            return None;
        }
        match bytes[idx] {
            b'x' => {
                idx += 1;
                let mut consumed = 0;
                while consumed < 2 && idx < bytes.len() && bytes[idx].is_ascii_hexdigit() {
                    idx += 1;
                    consumed += 1;
                }
                if consumed == 0 {
                    return None;
                }
            }
            b'u' => {
                idx += 1;
                if idx >= bytes.len() || bytes[idx] != b'{' {
                    return None;
                }
                idx += 1;
                let mut consumed = 0;
                while idx < bytes.len() && bytes[idx].is_ascii_hexdigit() {
                    idx += 1;
                    consumed += 1;
                }
                if consumed == 0 || idx >= bytes.len() || bytes[idx] != b'}' {
                    return None;
                }
                idx += 1;
            }
            _ => {
                idx += 1;
            }
        }
    } else {
        let slice = std::str::from_utf8(&bytes[idx..]).ok()?;
        let mut chars = slice.chars();
        let ch = chars.next()?;
        idx += ch.len_utf8();
    }

    if idx < bytes.len() && bytes[idx] == b'\'' { Some(idx + 1) } else { None }
}

fn highlight_rust_lifetimes(bytes: &[u8], occupied: Option<&[bool]>, out: &mut Vec<HighlightSpan>) {
    let mut idx = 0;
    while idx < bytes.len() {
        if occupied.map_or(false, |mask| mask.get(idx).copied().unwrap_or(false)) {
            idx += 1;
            continue;
        }
        if bytes[idx] != b'\'' {
            idx += 1;
            continue;
        }
        let next = idx + 1;
        if next >= bytes.len() || !is_ident_start(bytes[next]) {
            idx += 1;
            continue;
        }
        let start = idx;
        idx += 2;
        while idx < bytes.len() && is_ident_continue(bytes[idx]) {
            idx += 1;
        }
        push_span(out, start..idx, HighlightClass::Type);
    }
}

fn highlight_strings(
    bytes: &[u8],
    allow_single: bool,
    allow_double: bool,
    out: &mut Vec<HighlightSpan>,
) {
    let mut idx = 0;
    while idx < bytes.len() {
        let ch = bytes[idx];
        let is_single = allow_single && ch == b'\'';
        let is_double = allow_double && ch == b'"';
        if !is_single && !is_double {
            idx += 1;
            continue;
        }

        let delim = ch;
        let start = idx;
        idx += 1;
        while idx < bytes.len() {
            let b = bytes[idx];
            idx += 1;
            if b == b'\\' && idx < bytes.len() {
                idx += 1;
                continue;
            }
            if b == delim {
                break;
            }
        }

        push_span(out, start..idx, HighlightClass::String);
    }
}

fn highlight_numbers_with_mask(
    bytes: &[u8],
    occupied: Option<&[bool]>,
    allow_alpha_suffix: bool,
    out: &mut Vec<HighlightSpan>,
) {
    let mut idx = 0;
    while idx < bytes.len() {
        if occupied.map_or(false, |mask| mask.get(idx).copied().unwrap_or(false)) {
            idx += 1;
            continue;
        }
        if !bytes[idx].is_ascii_digit() {
            idx += 1;
            continue;
        }
        let start = idx;
        idx += 1;
        if idx < bytes.len()
            && bytes[start] == b'0'
            && matches!(bytes[idx], b'b' | b'B' | b'o' | b'O' | b'x' | b'X')
        {
            idx += 1;
        }

        let mut allow_fraction = true;
        while idx < bytes.len() {
            let b = bytes[idx];
            if b.is_ascii_hexdigit() || b == b'_' {
                idx += 1;
                continue;
            }
            if allow_fraction && b == b'.' {
                if idx + 1 < bytes.len() && bytes[idx + 1].is_ascii_digit() {
                    idx += 1;
                    continue;
                }
                break;
            }
            if matches!(b, b'e' | b'E' | b'p' | b'P') {
                let mut exp_idx = idx + 1;
                if exp_idx < bytes.len() && matches!(bytes[exp_idx], b'+' | b'-') {
                    exp_idx += 1;
                }
                let exp_start = exp_idx;
                while exp_idx < bytes.len() && bytes[exp_idx].is_ascii_digit() {
                    exp_idx += 1;
                }
                if exp_idx == exp_start {
                    break;
                }
                idx = exp_idx;
                allow_fraction = false;
                continue;
            }
            break;
        }

        if allow_alpha_suffix {
            while idx < bytes.len() && (bytes[idx].is_ascii_alphanumeric() || bytes[idx] == b'#') {
                idx += 1;
            }
        }

        push_span(out, start..idx, HighlightClass::Number);
    }
}

fn highlight_words_with_mask(
    bytes: &[u8],
    words: &[&str],
    class: HighlightClass,
    occupied: Option<&[bool]>,
    out: &mut Vec<HighlightSpan>,
) {
    if words.is_empty() {
        return;
    }
    let mut idx = 0;
    while idx < bytes.len() {
        if occupied.map_or(false, |mask| mask.get(idx).copied().unwrap_or(false)) {
            idx += 1;
            continue;
        }
        if !is_ident_start(bytes[idx]) {
            idx += 1;
            continue;
        }
        let start = idx;
        idx += 1;
        while idx < bytes.len() && is_ident_continue(bytes[idx]) {
            idx += 1;
        }

        if let Ok(word) = std::str::from_utf8(&bytes[start..idx]) {
            if words.iter().any(|w| *w == word) {
                push_span(out, start..idx, class);
            }
        }
    }
}

fn highlight_macros(bytes: &[u8], occupied: Option<&[bool]>, out: &mut Vec<HighlightSpan>) {
    let mut idx = 0;
    while idx < bytes.len() {
        if occupied.map_or(false, |mask| mask.get(idx).copied().unwrap_or(false)) {
            idx += 1;
            continue;
        }
        if !is_ident_start(bytes[idx]) {
            idx += 1;
            continue;
        }

        let start = idx;
        idx += 1;
        while idx < bytes.len() && is_ident_continue(bytes[idx]) {
            idx += 1;
        }

        if idx < bytes.len() && bytes[idx] == b'!' {
            push_span(out, start..idx + 1, HighlightClass::Macro);
            idx += 1;
        }
    }
}

fn highlight_preprocessor(bytes: &[u8], out: &mut Vec<HighlightSpan>) {
    let mut idx = 0;
    while idx < bytes.len() && bytes[idx].is_ascii_whitespace() {
        idx += 1;
    }
    if idx < bytes.len() && bytes[idx] == b'#' {
        push_span(out, idx..bytes.len(), HighlightClass::Macro);
    }
}

fn find_key_span(bytes: &[u8]) -> Option<Range<usize>> {
    let mut idx = 0;
    while idx < bytes.len() && bytes[idx].is_ascii_whitespace() {
        idx += 1;
    }
    let mut start = idx;
    while idx < bytes.len() && bytes[idx] != b'=' {
        idx += 1;
    }

    while start < idx && bytes[start].is_ascii_whitespace() {
        start += 1;
    }
    while idx > start && bytes[idx - 1].is_ascii_whitespace() {
        idx -= 1;
    }
    if start < idx { Some(start..idx) } else { None }
}

fn find_line_comment(bytes: &[u8], first: u8, second: u8) -> Option<usize> {
    let mut idx = 0;
    let mut in_string = None;
    while idx < bytes.len() {
        let b = bytes[idx];
        if let Some(delim) = in_string {
            idx += 1;
            if b == b'\\' && idx < bytes.len() {
                idx += 1;
                continue;
            }
            if b == delim {
                in_string = None;
            }
            continue;
        }

        if b == b'"' || b == b'\'' {
            in_string = Some(b);
            idx += 1;
            continue;
        }

        if first == second {
            if b == first {
                return Some(idx);
            }
        } else if b == first && idx + 1 < bytes.len() && bytes[idx + 1] == second {
            return Some(idx);
        }
        idx += 1;
    }
    None
}

fn split_comment<'a>(bytes: &'a [u8], idx: Option<usize>) -> (&'a [u8], Option<Range<usize>>) {
    match idx {
        Some(pos) => {
            let (body, comment) = bytes.split_at(pos);
            (body, Some(pos..pos + comment.len()))
        }
        None => (bytes, None),
    }
}

fn push_span(spans: &mut Vec<HighlightSpan>, range: Range<usize>, class: HighlightClass) {
    if range.start < range.end {
        spans.push(HighlightSpan { range, class });
    }
}

fn is_ident_start(byte: u8) -> bool {
    byte == b'_' || byte.is_ascii_alphabetic()
}

fn is_ident_continue(byte: u8) -> bool {
    is_ident_start(byte) || byte.is_ascii_digit()
}

fn build_occupied(len: usize, spans: &[HighlightSpan], classes: &[HighlightClass]) -> Vec<bool> {
    let mut occupied = vec![false; len];
    for span in spans {
        if !classes.contains(&span.class) {
            continue;
        }
        let start = span.range.start.min(len);
        let end = span.range.end.min(len);
        for i in start..end {
            occupied[i] = true;
        }
    }
    occupied
}

#[cfg(test)]
mod tests {
    use super::*;

    fn collect_spans(kind: SyntaxKind, line: &str) -> Vec<HighlightSpan> {
        let mut spans = Vec::new();
        highlight_line(kind, line, &mut spans);
        spans
    }

    fn has_span(spans: &[HighlightSpan], line: &str, class: HighlightClass, needle: &str) -> bool {
        spans.iter().any(|span| span.class == class && &line[span.range.clone()] == needle)
    }

    #[test]
    fn rust_lifetimes_are_not_strings() {
        let line = "fn foo<'a>(x: &'a str, y: &'static str) {}";
        let spans = collect_spans(SyntaxKind::Rust, line);
        assert!(has_span(&spans, line, HighlightClass::Type, "'a"));
        assert!(has_span(&spans, line, HighlightClass::Type, "'static"));

        let lifetime_idx = line.find("'static").unwrap();
        let in_string = spans.iter().any(|span| {
            span.class == HighlightClass::String
                && span.range.start <= lifetime_idx
                && lifetime_idx < span.range.end
        });
        assert!(!in_string, "lifetime token should not be highlighted as a string");
    }

    #[test]
    fn rust_raw_and_byte_strings_are_supported() {
        let line = "let msg = r#\"hello\"#; let data = br##\"hi\"##;";
        let spans = collect_spans(SyntaxKind::Rust, line);
        assert!(has_span(&spans, line, HighlightClass::String, "r#\"hello\"#"));
        assert!(has_span(&spans, line, HighlightClass::String, "br##\"hi\"##"));
    }

    #[test]
    fn numbers_cover_prefixes_and_suffixes() {
        let line = "let size = 10usize + 0b1010;";
        let spans = collect_spans(SyntaxKind::Rust, line);
        assert!(has_span(&spans, line, HighlightClass::Number, "10usize"));
        assert!(has_span(&spans, line, HighlightClass::Number, "0b1010"));
    }
}
