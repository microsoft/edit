use crate::framebuffer::{Framebuffer, IndexedColor};

pub struct Token {
    pub byte_offset: usize,
    pub length: usize,
    pub foreground: u32,
}

fn color_of(kind: &str) -> IndexedColor {
    match kind {
        "keyword" | "operator" => IndexedColor::Magenta,
        "string" => IndexedColor::Yellow,
        "comment" => IndexedColor::BrightBlack,
        "number" => IndexedColor::Blue,
        "punctuation.bracket" => IndexedColor::Green,
        _ => IndexedColor::White,
    }
}

// @TODO
const KEYWORDS: &[&str] = &[
    "as", "async", "await", "break", "const", "continue", "crate", "dyn", "else", "enum", "extern",
    "false", "fn", "for", "if", "impl", "in", "let", "loop", "match", "mod", "move", "pub", "ref",
    "return", "self", "Self", "static", "struct", "super", "trait", "true", "type", "union",
    "unsafe", "use", "where", "while", "function", "return", "import", "export", "from", "to",
    "as", "is", "of", "in", "and", "or", "not", "isnt", "if", "else", "then", "switch", "case",
    "default", "break", "continue", "for", "while", "do", "try", "catch", "finally", "throw",
    "new", "delete", "typeof", "void", "debugger", "instanceof", "with", "let", "yield",
    "class", "extends", "super", "const", "export", "import", "implements", "interface", "let",
    "package", "private", "protected", "public", "static", "yield", "null", "true", "false",
    "NaN", "Infinity", "undefined", "eval", "arguments", "this", "super", "var", "let", "new",
    "delete", "typeof", "void", "case", "default", "if", "else", "switch", "while", "do", "try",
    "catch", "finally", "throw", "with", "abstract", "boolean", "byte", "char", "class", "double",
    "enum", "export", "extends", "final", "float", "goto", "implements", "import", "instanceof",
    "int", "interface", "long", "native", "package", "private", "protected", "public", "short",
    "static", "super", "synchronized", "throws", "transient",
];

pub struct Highlighter;

impl Highlighter {
    pub fn new() -> Self {
        Self
    }

    pub fn highlight(&self, src: &str, palette: &Framebuffer) -> Vec<Token> {
        let bytes = src.as_bytes();
        let mut tokens = Vec::new();
        let mut i = 0;

        while i < bytes.len() {
            match bytes[i] {
                b'/' if i + 1 < bytes.len() && bytes[i + 1] == b'/' => {
                    let start = i;
                    i += 2;
                    while i < bytes.len() && bytes[i] != b'\n' {
                        i += 1;
                    }
                    tokens.push(Token {
                        byte_offset: start,
                        length: i - start,
                        foreground: palette.indexed(color_of("comment")),
                    });
                }

                b'"' | b'\'' => {
                    let start = i;
                    i += 1;
                    while i < bytes.len() {
                        if bytes[i] == b'\\' {
                            i += 2;
                        } else if bytes[i] == b'"' || bytes[i] == b'\'' {
                            i += 1;
                            break;
                        } else {
                            i += 1;
                        }
                    }
                    tokens.push(Token {
                        byte_offset: start,
                        length: i - start,
                        foreground: palette.indexed(color_of("string")),
                    });
                }

                b'0'..=b'9' => {
                    let start = i;
                    while i < bytes.len() && (bytes[i] as char).is_ascii_hexdigit() {
                        i += 1;
                    }
                    if i < bytes.len() && bytes[i] == b'.' {
                        i += 1;
                        while i < bytes.len() && (bytes[i] as char).is_ascii_digit() {
                            i += 1;
                        }
                    }
                    tokens.push(Token {
                        byte_offset: start,
                        length: i - start,
                        foreground: palette.indexed(color_of("number")),
                    });
                }

                b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                    let start = i;
                    i += 1;
                    while i < bytes.len()
                        && ((bytes[i] as char).is_alphanumeric() || bytes[i] == b'_')
                    {
                        i += 1;
                    }
                    let ident = &src[start..i];
                    let kind = if KEYWORDS.contains(&ident) { "keyword" } else { "identifier" };
                    if kind == "keyword" {
                        tokens.push(Token {
                            byte_offset: start,
                            length: i - start,
                            foreground: palette.indexed(color_of(kind)),
                        });
                    }
                }

                b'+' | b'-' | b'*' | b'%' | b'=' | b'!' | b'&' | b'|' | b'^' | b':' | b';'
                | b',' | b'.' | b'<' | b'>' => {
                    tokens.push(Token {
                        byte_offset: i,
                        length: 1,
                        foreground: palette.indexed(color_of("operator")),
                    });
                    i += 1;
                }

                b'(' | b')' | b'[' | b']' | b'{' | b'}' => {
                    tokens.push(Token {
                        byte_offset: i,
                        length: 1,
                        foreground: palette.indexed(color_of("punctuation.bracket")),
                    });
                    i += 1;
                }

                _ => {
                    i += 1;
                }
            }
        }
        tokens
    }
}
