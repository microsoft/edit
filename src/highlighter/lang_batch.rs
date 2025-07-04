use super::Consume::*;
use super::HighlightKind::*;
use super::StateStack::*;
use super::*;

type T = Transition;

// NOTE: These are indices into the `LANG.states` array.
const _GROUND: u8 = 0;
const COMMENT: u8 = 1;
const STRING: u8 = 2;
const VARIABLE: u8 = 3;

pub const LANG: Language = Language {
    name: "Batch",
    extensions: &["bat", "cmd"],
    word_chars: &[
        // /.-,+*)('&%$#"!
        0b_0000000000000000,
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
            // Comments (REM or ::)
            T { test: Prefix("REM "), kind: Comment, state: Push(COMMENT) },
            T { test: Prefix("::"), kind: Comment, state: Push(COMMENT) },
            // Strings (quoted)
            T { test: Prefix("\""), kind: String, state: Push(STRING) },
            // Variables
            T { test: Prefix("%"), kind: Variable, state: Push(VARIABLE) },
            // Numbers
            T { test: Digits, kind: Number, state: Pop(1) },
            // Operators
            T { test: Prefix("|"), kind: Operator, state: Pop(1) },
            T { test: Prefix("&"), kind: Operator, state: Pop(1) },
            T { test: Prefix("<"), kind: Operator, state: Pop(1) },
            T { test: Prefix(">"), kind: Operator, state: Pop(1) },
            // Keywords (common)
            T { test: Prefix("if"), kind: Keyword, state: Pop(1) },
            T { test: Prefix("else"), kind: Keyword, state: Pop(1) },
            T { test: Prefix("for"), kind: Keyword, state: Pop(1) },
            T { test: Prefix("in"), kind: Keyword, state: Pop(1) },
            T { test: Prefix("do"), kind: Keyword, state: Pop(1) },
            T { test: Prefix("not"), kind: Keyword, state: Pop(1) },
            T { test: Prefix("exist"), kind: Keyword, state: Pop(1) },
            T { test: Prefix("set"), kind: Keyword, state: Pop(1) },
            T { test: Prefix("echo"), kind: Keyword, state: Pop(1) },
            T { test: Prefix("goto"), kind: Keyword, state: Pop(1) },
            T { test: Prefix("call"), kind: Keyword, state: Pop(1) },
        ],
        // COMMENT
        &[T { test: Line, kind: Comment, state: Pop(1) }],
        // STRING
        &[T { test: Prefix("\""), kind: String, state: Pop(1) }],
        // VARIABLE
        &[
            T { test: Prefix("%"), kind: Variable, state: Pop(1) },
            T { test: Word, kind: Variable, state: Pop(1) },
        ],
    ],
};
