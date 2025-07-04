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

pub const LANG: Language = Language {
    name: "YAML",
    extensions: &["yaml", "yml"],
    word_chars: &[
        // /.-,+*)('&%$#"!
        0b_0000000000000000,
        // ?>=<;:9876543210
        0b_0000000000000000,
        // ONMLKJIHGFEDCBA@
        0b_0000000000000000,
        // _^]\[ZYXWVUTSRQP
        0b_0000000000000000,
        // onmlkjihgfedcba`
        0b_0000000000000000,
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
            // Numbers
            T { test: Digits, kind: Number, state: Pop(1) },
            // Booleans/null
            T { test: Prefix("true"), kind: Keyword, state: Pop(1) },
            T { test: Prefix("false"), kind: Keyword, state: Pop(1) },
            T { test: Prefix("null"), kind: Keyword, state: Pop(1) },
            // Punctuation
            T { test: Prefix("-"), kind: Operator, state: Pop(1) },
            T { test: Prefix(":"), kind: Operator, state: Pop(1) },
            T { test: Prefix(","), kind: Operator, state: Pop(1) },
            T { test: Prefix("["), kind: Operator, state: Pop(1) },
            T { test: Prefix("]"), kind: Operator, state: Pop(1) },
            T { test: Prefix("{"), kind: Operator, state: Pop(1) },
            T { test: Prefix("}"), kind: Operator, state: Pop(1) },
        ],
        // COMMENT
        &[T { test: Line, kind: Comment, state: Pop(1) }],
        // STRING_SINGLE
        &[T { test: Prefix("'"), kind: String, state: Pop(1) }],
        // STRING_DOUBLE
        &[T { test: Prefix("\""), kind: String, state: Pop(1) }],
    ],
};
