use super::Consume::*;
use super::HighlightKind::*;
use super::StateStack::*;
use super::*;

type T = Transition;

// NOTE: These are indices into the `LANG.charsets` array.
const C_DIGITS: usize = 0;
const C_VARIABLE: usize = 1;

// NOTE: These are indices into the `LANG.states` array.
const _S_GROUND: u8 = 0;
const S_COMMENT: u8 = 1;
const S_STRING_SINGLE: u8 = 2;
const S_STRING_DOUBLE: u8 = 3;
const S_STRING_ESCAPE: u8 = 4;
const S_VARIABLE: u8 = 5;

pub const LANG: Language = Language {
    name: "Bash",
    extensions: &["sh", "bash", "zsh", "ksh", "csh", "tcsh"],
    charsets: &[
        // C_DIGITS
        &[
            // /.-,+*)('&%$#"!
            0b_0010100000000000,
            // ?>=<;:9876543210
            0b_0000001111111111,
            // ONMLKJIHGFEDCBA@
            0b_0000000000000000,
            // _^]\[ZYXWVUTSRQP
            0b_0000000000000000,
            // onmlkjihgfedcba`
            0b_0000000000000000,
            //  ~}|{zyxwvutsrqp
            0b_0000000000000000,
        ],
        // C_VARIABLE
        &[
            // /.-,+*)('&%$#"!
            0b_0000110000000000,
            // ?>=<;:9876543210
            0b_0000001111111111,
            // ONMLKJIHGFEDCBA@
            0b_1111111111111110,
            // _^]\[ZYXWVUTSRQP
            0b_1000000000000000,
            // onmlkjihgfedcba`
            0b_1111111111111111,
            //  ~}|{zyxwvutsrqp
            0b_0000000000000000,
        ],
    ],
    states: &[
        // S_GROUND
        &[
            // Comments
            T { test: Prefix("#"), kind: Comment, state: Push(S_COMMENT) },
            T { test: Prefix("<#"), kind: Comment, state: Push(S_COMMENT) },
            // Strings
            T { test: Prefix("'"), kind: String, state: Push(S_STRING_SINGLE) },
            T { test: Prefix("\""), kind: String, state: Push(S_STRING_DOUBLE) },
            // Variables
            T { test: Prefix("$"), kind: Variable, state: Push(S_VARIABLE) },
            // Numbers
            T { test: Charset(C_DIGITS), kind: Number, state: Pop(1) },
            // Operators
            T { test: Prefix("|"), kind: Operator, state: Pop(1) },
            T { test: Prefix("&"), kind: Operator, state: Pop(1) },
            T { test: Prefix(";"), kind: Operator, state: Pop(1) },
            T { test: Prefix("<"), kind: Operator, state: Pop(1) },
            T { test: Prefix(">"), kind: Operator, state: Pop(1) },
            // Keywords (common)
            T { test: Prefix("if"), kind: Keyword, state: Pop(1) },
            T { test: Prefix("then"), kind: Keyword, state: Pop(1) },
            T { test: Prefix("else"), kind: Keyword, state: Pop(1) },
            T { test: Prefix("elif"), kind: Keyword, state: Pop(1) },
            T { test: Prefix("fi"), kind: Keyword, state: Pop(1) },
            T { test: Prefix("for"), kind: Keyword, state: Pop(1) },
            T { test: Prefix("while"), kind: Keyword, state: Pop(1) },
            T { test: Prefix("do"), kind: Keyword, state: Pop(1) },
            T { test: Prefix("done"), kind: Keyword, state: Pop(1) },
            T { test: Prefix("case"), kind: Keyword, state: Pop(1) },
            T { test: Prefix("esac"), kind: Keyword, state: Pop(1) },
            T { test: Prefix("function"), kind: Keyword, state: Pop(1) },
        ],
        // S_COMMENT
        &[T { test: Line, kind: Comment, state: Pop(1) }],
        // S_STRING_SINGLE
        &[T { test: Prefix("'"), kind: String, state: Pop(1) }],
        // S_STRING_DOUBLE
        &[
            T { test: Prefix("\\"), kind: String, state: Push(S_STRING_ESCAPE) },
            T { test: Prefix("$"), kind: Variable, state: Push(S_VARIABLE) },
            T { test: Prefix("\""), kind: String, state: Pop(1) },
        ],
        // S_STRING_ESCAPE
        &[T { test: Chars(1), kind: String, state: Pop(1) }],
        // S_VARIABLE
        &[T { test: Charset(C_VARIABLE), kind: Variable, state: Pop(1) }],
    ],
};
