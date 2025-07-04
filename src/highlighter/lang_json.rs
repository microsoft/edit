use super::Consume::*;
use super::HighlightKind::*;
use super::StateStack::*;
use super::*;

type T = Transition;

// NOTE: These are indices into the `LANG.states` array.
const _GROUND: u8 = 0;
const LINE_COMMENT: u8 = 1;
const BLOCK_COMMENT: u8 = 2;
const STRING: u8 = 3;
const STRING_ESCAPE: u8 = 4;

pub const LANG: Language = Language {
    name: "JSON",
    extensions: &["json", "jsonc"],
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
            // Comments (jsonc)
            T { test: Prefix("//"), kind: Comment, state: Push(LINE_COMMENT) },
            T { test: Prefix("/*"), kind: Comment, state: Push(BLOCK_COMMENT) },
            // Strings
            T { test: Prefix("\""), kind: String, state: Push(STRING) },
            // Numbers
            T { test: Digits, kind: Number, state: Pop(1) },
            // Booleans/null
            T { test: Prefix("true"), kind: Keyword, state: Pop(1) },
            T { test: Prefix("false"), kind: Keyword, state: Pop(1) },
            T { test: Prefix("null"), kind: Keyword, state: Pop(1) },
        ],
        // LINE_COMMENT (// single-line)
        &[T { test: Line, kind: Comment, state: Pop(1) }],
        // BLOCK_COMMENT (/* ... */)
        &[T { test: Prefix("*/"), kind: Comment, state: Pop(1) }],
        // STRING ("...")
        &[
            T { test: Prefix("\\"), kind: String, state: Push(STRING_ESCAPE) },
            T { test: Prefix("\""), kind: String, state: Pop(1) },
        ],
        // STRING_ESCAPE (escape in string)
        &[T { test: Chars(1), kind: String, state: Pop(1) }],
    ],
};
