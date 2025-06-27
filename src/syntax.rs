// Copyright (c) Pavel Sich.
// Licensed under the MIT License.

//! Syntax highlighting for various programming languages.

use regex::Regex;
use crate::framebuffer::IndexedColor;

/// Color type that can be either indexed or RGB
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SyntaxColor {
    Indexed(IndexedColor),
    Rgb(u32),
}

/// Represents different types of syntax elements
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SyntaxElement {
    Keyword,
    Type,
    String,
    Comment,
    Number,
    Operator,
    Function,
    Variable,
    None,
}

impl SyntaxElement {
    /// Returns the color to use for this syntax element
    /// Uses custom RGB colors for strings and functions, IndexedColor for others
    pub fn color(self) -> SyntaxColor {
        match self {
            SyntaxElement::Keyword => SyntaxColor::Indexed(IndexedColor::BrightMagenta),  // Purple for keywords
            SyntaxElement::Type => SyntaxColor::Indexed(IndexedColor::BrightCyan),        // Bright cyan for types
            SyntaxElement::String => SyntaxColor::Rgb(0xfffd8273),                       // Custom coral/salmon color
            SyntaxElement::Comment => SyntaxColor::Indexed(IndexedColor::BrightBlack),    // Gray for comments
            SyntaxElement::Number => SyntaxColor::Indexed(IndexedColor::BrightYellow),    // Yellow for numbers
            SyntaxElement::Operator => SyntaxColor::Indexed(IndexedColor::White),         // White for operators
            SyntaxElement::Function => SyntaxColor::Rgb(0xff75c2b3),                     // Custom teal green color
            SyntaxElement::Variable => SyntaxColor::Indexed(IndexedColor::Foreground),    // Default foreground
            SyntaxElement::None => SyntaxColor::Indexed(IndexedColor::Foreground),
        }
    }

    /// Legacy method that returns IndexedColor for backward compatibility
    pub fn indexed_color(self) -> IndexedColor {
        match self.color() {
            SyntaxColor::Indexed(color) => color,
            SyntaxColor::Rgb(_) => match self {
                SyntaxElement::String => IndexedColor::BrightRed,     // Fallback for strings
                SyntaxElement::Function => IndexedColor::BrightGreen, // Fallback for functions
                _ => IndexedColor::Foreground,
            },
        }
    }
}

/// A syntax highlighter for a specific programming language
pub struct SyntaxHighlighter {
    language: Language,
    keyword_regex: Regex,
    type_regex: Regex,
    string_regex: Regex,
    comment_regex: Regex,
    number_regex: Regex,
    function_regex: Regex,
}

/// Supported programming languages
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Language {
    Rust,
    Go,
    C,
    Cpp,
    CSharp,
    Unknown,
}

impl Language {
    /// Detect language from file extension
    pub fn from_extension(ext: &str) -> Self {
        match ext.to_lowercase().as_str() {
            "rs" => Language::Rust,
            "go" => Language::Go,
            "c" | "h" => Language::C,
            "cpp" | "cc" | "cxx" | "hpp" | "hxx" => Language::Cpp,
            "cs" => Language::CSharp,
            _ => Language::Unknown,
        }
    }
}

impl SyntaxHighlighter {
    /// Create a new syntax highlighter for the given language
    pub fn new(language: Language) -> Self {
        let (keywords, types, comment_pattern, function_pattern) = match language {
            Language::Rust => (
                r"\b(?:fn|let|mut|struct|enum|trait|impl|for|if|else|while|loop|match|return|use|mod|pub|crate|self|super|const|static|async|await|move|unsafe|extern|dyn|where|type|as|in|ref|break|continue)\b",
                r"\b(?:u8|u16|u32|u64|u128|i8|i16|i32|i64|i128|f32|f64|usize|isize|bool|char|String|str|Vec|Option|Result|Box|Rc|Arc|RefCell|Cell)\b",
                r"//.*|/\*[\s\S]*?\*/",
                r"\b([a-zA-Z_][a-zA-Z0-9_]*)\s*\(",
            ),
            Language::Go => (
                r"\b(?:func|var|const|type|struct|interface|package|import|for|if|else|switch|case|default|return|break|continue|go|defer|select|chan|map|range|fallthrough)\b",
                r"\b(?:int|int8|int16|int32|int64|uint|uint8|uint16|uint32|uint64|float32|float64|bool|string|byte|rune|error|interface\{\})\b",
                r"//.*|/\*[\s\S]*?\*/",
                r"\b([a-zA-Z_][a-zA-Z0-9_]*)\s*\(",
            ),
            Language::C => (
                r"\b(?:auto|break|case|char|const|continue|default|do|double|else|enum|extern|float|for|goto|if|inline|int|long|register|restrict|return|short|signed|sizeof|static|struct|switch|typedef|union|unsigned|void|volatile|while|_Bool|_Complex|_Imaginary)\b",
                r"\b(?:char|short|int|long|float|double|void|signed|unsigned|size_t|ptrdiff_t|FILE|NULL)\b",
                r"//.*|/\*[\s\S]*?\*/",
                r"\b([a-zA-Z_][a-zA-Z0-9_]*)\s*\(",
            ),
            Language::Cpp => (
                r"\b(?:alignas|alignof|and|and_eq|asm|auto|bitand|bitor|bool|break|case|catch|char|char16_t|char32_t|class|compl|const|constexpr|const_cast|continue|decltype|default|delete|do|double|dynamic_cast|else|enum|explicit|export|extern|false|float|for|friend|goto|if|inline|int|long|mutable|namespace|new|noexcept|not|not_eq|nullptr|operator|or|or_eq|private|protected|public|register|reinterpret_cast|return|short|signed|sizeof|static|static_assert|static_cast|struct|switch|template|this|thread_local|throw|true|try|typedef|typeid|typename|union|unsigned|using|virtual|void|volatile|wchar_t|while|xor|xor_eq)\b",
                r"\b(?:std::string|std::vector|std::map|std::set|std::pair|std::shared_ptr|std::unique_ptr|std::weak_ptr|bool|char|short|int|long|float|double|void|size_t|ptrdiff_t)\b",
                r"//.*|/\*[\s\S]*?\*/",
                r"\b([a-zA-Z_][a-zA-Z0-9_]*)\s*\(",
            ),
            Language::CSharp => (
                r"\b(?:abstract|as|base|bool|break|byte|case|catch|char|checked|class|const|continue|decimal|default|delegate|do|double|else|enum|event|explicit|extern|false|finally|fixed|float|for|foreach|goto|if|implicit|in|int|interface|internal|is|lock|long|namespace|new|null|object|operator|out|override|params|private|protected|public|readonly|ref|return|sbyte|sealed|short|sizeof|stackalloc|static|string|struct|switch|this|throw|true|try|typeof|uint|ulong|unchecked|unsafe|ushort|using|virtual|void|volatile|while)\b",
                r"\b(?:bool|byte|sbyte|char|decimal|double|float|int|uint|long|ulong|short|ushort|object|string|var|dynamic|List|Dictionary|IEnumerable|ICollection|Array)\b",
                r"//.*|/\*[\s\S]*?\*/",
                r"\b([a-zA-Z_][a-zA-Z0-9_]*)\s*\(",
            ),
            Language::Unknown => return Self::default(),
        };

        Self {
            language,
            keyword_regex: Regex::new(keywords).unwrap(),
            type_regex: Regex::new(types).unwrap(),
            string_regex: Regex::new(r#""([^"\\]|\\.)*"|'([^'\\]|\\.)*'"#).unwrap(),
            comment_regex: Regex::new(comment_pattern).unwrap(),
            number_regex: Regex::new(r"\b\d+\.?\d*([eE][+-]?\d+)?[fFdDlL]?\b|\b0[xX][0-9a-fA-F]+[lL]?\b|\b0[bB][01]+[lL]?\b").unwrap(),
            function_regex: Regex::new(function_pattern).unwrap(),
        }
    }

    /// Get the syntax element type for text at the given position
    pub fn get_syntax_element(&self, text: &str, position: usize) -> SyntaxElement {
        if self.language == Language::Unknown {
            return SyntaxElement::None;
        }

        // Check if position is within a comment
        for mat in self.comment_regex.find_iter(text) {
            if position >= mat.start() && position < mat.end() {
                return SyntaxElement::Comment;
            }
        }

        // Check if position is within a string
        for mat in self.string_regex.find_iter(text) {
            if position >= mat.start() && position < mat.end() {
                return SyntaxElement::String;
            }
        }

        // Check for keywords
        for mat in self.keyword_regex.find_iter(text) {
            if position >= mat.start() && position < mat.end() {
                return SyntaxElement::Keyword;
            }
        }

        // Check for types
        for mat in self.type_regex.find_iter(text) {
            if position >= mat.start() && position < mat.end() {
                return SyntaxElement::Type;
            }
        }

        // Check for numbers
        for mat in self.number_regex.find_iter(text) {
            if position >= mat.start() && position < mat.end() {
                return SyntaxElement::Number;
            }
        }

        // Check for functions
        for mat in self.function_regex.find_iter(text) {
            if position >= mat.start() && position < mat.end() {
                return SyntaxElement::Function;
            }
        }

        SyntaxElement::None
    }

    /// Highlight a line of text and return syntax elements for each character position
    pub fn highlight_line(&self, line: &str) -> Vec<SyntaxElement> {
        let mut result = vec![SyntaxElement::None; line.len()];
        
        if self.language == Language::Unknown {
            return result;
        }

        // Apply highlighting in order of precedence (comments and strings first)
        
        // Comments (highest precedence)
        for mat in self.comment_regex.find_iter(line) {
            for i in mat.start()..mat.end().min(line.len()) {
                result[i] = SyntaxElement::Comment;
            }
        }

        // Strings (second highest precedence)
        for mat in self.string_regex.find_iter(line) {
            for i in mat.start()..mat.end().min(line.len()) {
                if result[i] == SyntaxElement::None {
                    result[i] = SyntaxElement::String;
                }
            }
        }

        // Keywords
        for mat in self.keyword_regex.find_iter(line) {
            for i in mat.start()..mat.end().min(line.len()) {
                if result[i] == SyntaxElement::None {
                    result[i] = SyntaxElement::Keyword;
                }
            }
        }

        // Types
        for mat in self.type_regex.find_iter(line) {
            for i in mat.start()..mat.end().min(line.len()) {
                if result[i] == SyntaxElement::None {
                    result[i] = SyntaxElement::Type;
                }
            }
        }

        // Numbers
        for mat in self.number_regex.find_iter(line) {
            for i in mat.start()..mat.end().min(line.len()) {
                if result[i] == SyntaxElement::None {
                    result[i] = SyntaxElement::Number;
                }
            }
        }

        // Functions (captured groups only)
        for mat in self.function_regex.find_iter(line) {
            if let Some(func_match) = mat.as_str().find('(') {
                let func_end = mat.start() + func_match;
                for i in mat.start()..func_end.min(line.len()) {
                    if result[i] == SyntaxElement::None && line.chars().nth(i).map_or(false, |c| c.is_alphabetic() || c == '_') {
                        result[i] = SyntaxElement::Function;
                    }
                }
            }
        }

        result
    }
}

impl Default for SyntaxHighlighter {
    fn default() -> Self {
        Self {
            language: Language::Unknown,
            keyword_regex: Regex::new(r"$^").unwrap(), // Never matches
            type_regex: Regex::new(r"$^").unwrap(),
            string_regex: Regex::new(r"$^").unwrap(),
            comment_regex: Regex::new(r"$^").unwrap(),
            number_regex: Regex::new(r"$^").unwrap(),
            function_regex: Regex::new(r"$^").unwrap(),
        }
    }
}
