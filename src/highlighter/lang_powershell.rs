use super::Consume::*;
use super::HighlightKind::*;
use super::StateStack::*;
use super::*;

type T = Transition;

// NOTE: These are indices into the `LANG.states` array.
const _GROUND: u8 = 0;
const LINE_COMMENT: u8 = 1;
const BLOCK_COMMENT: u8 = 2;
const STRING_SINGLE: u8 = 3;
const STRING_DOUBLE: u8 = 4;
const STRING_ESCAPE: u8 = 5;
const VARIABLE: u8 = 6;
const VARIABLE_BRACE: u8 = 7;
const VARIABLE_PAREN: u8 = 8;
const PARAMETER: u8 = 9;
const KEYWORD: u8 = 10;
const METHOD: u8 = 11;

pub const LANG: Language = Language {
    name: "PowerShell",
    extensions: &["ps1", "psm1", "psd1"],
    word_chars: &[
        // /.-,+*)('&%$#"!
        0b_1110110000101010,
        // ?>=<;:9876543210
        0b_1111011111111111,
        // ONMLKJIHGFEDCBA@
        0b_1111111111111110,
        // _^]\[ZYXWVUTSRQP
        0b_1111111111111111,
        // onmlkjihgfedcba`
        0b_1111111111111111,
        //  ~}|{zyxwvutsrqp
        0b_0100011111111111,
    ],
    states: &[
        // GROUND
        &[
            // Comments
            T { test: Prefix("#"), kind: Comment, state: Push(LINE_COMMENT) },
            T { test: Prefix("<#"), kind: Comment, state: Push(BLOCK_COMMENT) },
            // Numbers
            T { test: Digits, kind: Number, state: Pop(1) },
            // Strings
            T { test: Prefix("'"), kind: String, state: Push(STRING_SINGLE) },
            T { test: Prefix("\""), kind: String, state: Push(STRING_DOUBLE) },
            // Variables
            T { test: Prefix("$("), kind: Other, state: Push(VARIABLE_PAREN) },
            T { test: Prefix("$"), kind: Variable, state: Push(VARIABLE) },
            // Operators
            T { test: Prefix("-"), kind: Operator, state: Push(PARAMETER) },
            T { test: Prefix("!"), kind: Operator, state: Pop(1) },
            T { test: Prefix("*"), kind: Operator, state: Pop(1) },
            T { test: Prefix("/"), kind: Operator, state: Pop(1) },
            T { test: Prefix("%"), kind: Operator, state: Pop(1) },
            T { test: Prefix("+"), kind: Operator, state: Pop(1) },
            T { test: Prefix("<"), kind: Operator, state: Pop(1) },
            T { test: Prefix("="), kind: Operator, state: Pop(1) },
            T { test: Prefix(">"), kind: Operator, state: Pop(1) },
            T { test: Prefix("|"), kind: Operator, state: Pop(1) },
            // Keywords
            T { test: Prefix("break"), kind: Keyword, state: Push(KEYWORD) },
            T { test: Prefix("catch"), kind: Keyword, state: Push(KEYWORD) },
            T { test: Prefix("continue"), kind: Keyword, state: Push(KEYWORD) },
            T { test: Prefix("do"), kind: Keyword, state: Push(KEYWORD) },
            T { test: Prefix("else"), kind: Keyword, state: Push(KEYWORD) },
            T { test: Prefix("finally"), kind: Keyword, state: Push(KEYWORD) },
            T { test: Prefix("foreach"), kind: Keyword, state: Push(KEYWORD) },
            T { test: Prefix("function"), kind: Keyword, state: Push(KEYWORD) },
            T { test: Prefix("if"), kind: Keyword, state: Push(KEYWORD) },
            T { test: Prefix("return"), kind: Keyword, state: Push(KEYWORD) },
            T { test: Prefix("switch"), kind: Keyword, state: Push(KEYWORD) },
            T { test: Prefix("throw"), kind: Keyword, state: Push(KEYWORD) },
            T { test: Prefix("try"), kind: Keyword, state: Push(KEYWORD) },
            T { test: Prefix("using"), kind: Keyword, state: Push(KEYWORD) },
            T { test: Prefix("while"), kind: Keyword, state: Push(KEYWORD) },
            // Methods
            T { test: Word, kind: Method, state: Push(METHOD) },
        ],
        // LINE_COMMENT: # comment
        &[T { test: Line, kind: Comment, state: Pop(1) }],
        // BLOCK_COMMENT: <# comment #>
        &[T { test: Prefix("#>"), kind: Comment, state: Pop(1) }],
        // STRING_SINGLE: 'string'
        &[T { test: Prefix("'"), kind: String, state: Pop(1) }],
        // STRING_DOUBLE: "string"
        &[
            T { test: Prefix("`"), kind: String, state: Push(STRING_ESCAPE) },
            T { test: Prefix("$("), kind: Other, state: Push(VARIABLE_PAREN) },
            T { test: Prefix("$"), kind: Variable, state: Push(VARIABLE) },
            T { test: Prefix("\""), kind: String, state: Pop(1) },
        ],
        // STRING_ESCAPE: "`a"
        &[T { test: Chars(1), kind: String, state: Pop(1) }],
        // VARIABLE: $variable
        &[
            T { test: Prefix("{"), kind: Variable, state: Change(VARIABLE_BRACE) },
            T { test: Word, kind: Variable, state: Pop(1) },
        ],
        // VARIABLE_BRACE: ${variable}
        &[T { test: Prefix("}"), kind: Variable, state: Pop(1) }],
        // VARIABLE_PAREN: $(command)
        // This is largely a copy of the ground state.
        &[
            // Ground state Overrides
            T { test: Prefix("("), kind: Other, state: Push(VARIABLE_PAREN) },
            T { test: Prefix(")"), kind: Other, state: Pop(1) },
            // Numbers
            T { test: Digits, kind: Number, state: Pop(1) },
            // Strings
            T { test: Prefix("'"), kind: String, state: Push(STRING_SINGLE) },
            T { test: Prefix("\""), kind: String, state: Push(STRING_DOUBLE) },
            // Variables
            T { test: Prefix("$"), kind: Variable, state: Push(VARIABLE) },
            // Operators
            T { test: Prefix("-"), kind: Operator, state: Push(PARAMETER) },
            T { test: Prefix("!"), kind: Operator, state: Pop(1) },
            T { test: Prefix("*"), kind: Operator, state: Pop(1) },
            T { test: Prefix("/"), kind: Operator, state: Pop(1) },
            T { test: Prefix("%"), kind: Operator, state: Pop(1) },
            T { test: Prefix("+"), kind: Operator, state: Pop(1) },
            T { test: Prefix("<"), kind: Operator, state: Pop(1) },
            T { test: Prefix("="), kind: Operator, state: Pop(1) },
            T { test: Prefix(">"), kind: Operator, state: Pop(1) },
            T { test: Prefix("|"), kind: Operator, state: Pop(1) },
            // Keywords
            T { test: Prefix("break"), kind: Keyword, state: Push(KEYWORD) },
            T { test: Prefix("catch"), kind: Keyword, state: Push(KEYWORD) },
            T { test: Prefix("continue"), kind: Keyword, state: Push(KEYWORD) },
            T { test: Prefix("do"), kind: Keyword, state: Push(KEYWORD) },
            T { test: Prefix("else"), kind: Keyword, state: Push(KEYWORD) },
            T { test: Prefix("finally"), kind: Keyword, state: Push(KEYWORD) },
            T { test: Prefix("foreach"), kind: Keyword, state: Push(KEYWORD) },
            T { test: Prefix("function"), kind: Keyword, state: Push(KEYWORD) },
            T { test: Prefix("if"), kind: Keyword, state: Push(KEYWORD) },
            T { test: Prefix("return"), kind: Keyword, state: Push(KEYWORD) },
            T { test: Prefix("switch"), kind: Keyword, state: Push(KEYWORD) },
            T { test: Prefix("throw"), kind: Keyword, state: Push(KEYWORD) },
            T { test: Prefix("try"), kind: Keyword, state: Push(KEYWORD) },
            T { test: Prefix("using"), kind: Keyword, state: Push(KEYWORD) },
            T { test: Prefix("while"), kind: Keyword, state: Push(KEYWORD) },
            // Methods
            T { test: Word, kind: Method, state: Push(METHOD) },
        ],
        // PARAMETER: -parameter
        &[
            T { test: Word, kind: Operator, state: Pop(1) },
            T { test: Chars(0), kind: Operator, state: Pop(1) },
        ],
        // KEYWORD: foreach, if, etc.
        &[
            T { test: Word, kind: Method, state: Change(METHOD) },
            T { test: Chars(0), kind: Keyword, state: Pop(1) },
        ],
        // METHOD: Foo-Bar
        &[
            T { test: Word, kind: Method, state: Change(METHOD) },
            T { test: Prefix("-"), kind: Method, state: Change(METHOD) },
            T { test: Chars(0), kind: Method, state: Pop(1) },
        ],
    ],
};
