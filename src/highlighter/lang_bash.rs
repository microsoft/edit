use super::Consume::*;
use super::HighlightKind::*;
use super::StateStack::*;
use super::*;

type T = Transition;

// NOTE: These are indices into the `LANG.states` array.
const _GROUND: u8 = 0;
const COMMENT: u8 = 1;
const STRING_SINGLE: u8 = 2;
const STRING_DOUBLE: u8 = 3;
const STRING_ESCAPE: u8 = 4;
const VARIABLE: u8 = 5;

pub const LANG: Language = Language {
    name: "Bash",
    extensions: &["sh", "bash", "zsh", "ksh", "csh", "tcsh"],
    word_chars: &[
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
    states: &[
        // GROUND
        &[
            // Comments
            T { test: Prefix("#"), kind: Comment, state: Push(COMMENT) },
            T { test: Prefix("<#"), kind: Comment, state: Push(COMMENT) },
            // Strings
            T { test: Prefix("'"), kind: String, state: Push(STRING_SINGLE) },
            T { test: Prefix("\""), kind: String, state: Push(STRING_DOUBLE) },
            // Variables
            T { test: Prefix("$"), kind: Variable, state: Push(VARIABLE) },
            // Numbers
            T { test: Digits, kind: Number, state: Pop(1) },
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
        // COMMENT
        &[T { test: Line, kind: Comment, state: Pop(1) }],
        // STRING_SINGLE
        &[T { test: Prefix("'"), kind: String, state: Pop(1) }],
        // STRING_DOUBLE
        &[
            T { test: Prefix("\\"), kind: String, state: Push(STRING_ESCAPE) },
            T { test: Prefix("$"), kind: Variable, state: Push(VARIABLE) },
            T { test: Prefix("\""), kind: String, state: Pop(1) },
        ],
        // STRING_ESCAPE
        &[T { test: Chars(1), kind: String, state: Pop(1) }],
        // VARIABLE
        &[T { test: Word, kind: Variable, state: Pop(1) }],
    ],
};
