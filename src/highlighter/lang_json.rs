use super::Consume::*;
use super::HighlightKind::*;
use super::StateStack::*;
use super::*;

type T = Transition;

// NOTE: These are indices into the `LANG.charsets` array.
const C_DIGITS: usize = 0;

// NOTE: These are indices into the `LANG.states` array.
const _S_GROUND: u8 = 0;
const S_LINE_COMMENT: u8 = 1;
const S_BLOCK_COMMENT: u8 = 2;
const S_STRING: u8 = 3;
const S_STRING_ESCAPE: u8 = 4;

pub const LANG: Language = Language {
    name: "JSON",
    extensions: &["json", "jsonc"],
    charsets: &[
        // C_DIGITS
        &[
            // /.-,+*)('&%$#"!
            0b_0110100000000000,
            // ?>=<;:9876543210
            0b_0000001111111111,
            // ONMLKJIHGFEDCBA@
            0b_0000000000100000,
            // _^]\[ZYXWVUTSRQP
            0b_0000000000000000,
            // onmlkjihgfedcba`
            0b_0000000000100000,
            //  ~}|{zyxwvutsrqp
            0b_0000000000000000,
        ],
    ],
    states: &[
        // S_GROUND
        &[
            // Comments (jsonc)
            T { test: Prefix("//"), kind: Comment, state: Push(S_LINE_COMMENT) },
            T { test: Prefix("/*"), kind: Comment, state: Push(S_BLOCK_COMMENT) },
            // Strings
            T { test: Prefix("\""), kind: String, state: Push(S_STRING) },
            // Numbers (start: minus or digit)
            T { test: Charset(C_DIGITS), kind: Number, state: Pop(1) },
            // Booleans/null
            T { test: Prefix("true"), kind: Keyword, state: Pop(1) },
            T { test: Prefix("false"), kind: Keyword, state: Pop(1) },
            T { test: Prefix("null"), kind: Keyword, state: Pop(1) },
        ],
        // S_LINE_COMMENT (// single-line)
        &[T { test: Line, kind: Comment, state: Pop(1) }],
        // S_BLOCK_COMMENT (/* ... */)
        &[T { test: Prefix("*/"), kind: Comment, state: Pop(1) }],
        // S_STRING ("...")
        &[
            T { test: Prefix("\\"), kind: String, state: Push(S_STRING_ESCAPE) },
            T { test: Prefix("\""), kind: String, state: Pop(1) },
        ],
        // S_STRING_ESCAPE (escape in string)
        &[T { test: Chars(1), kind: String, state: Pop(1) }],
    ],
};
