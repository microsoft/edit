use super::Consume::*;
use super::HighlightKind::*;
use super::StateStack::*;
use super::*;

type T = Transition;

// NOTE: These are indices into the `LANG.charsets` array.
const C_DIGITS: usize = 0;
const C_KEY_STRING: usize = 1;
const C_VALUE_STRING: usize = 2;

// NOTE: These are indices into the `LANG.states` array.
const _S_GROUND: u8 = 0;
const S_COMMENT: u8 = 1;
const S_STRING_SINGLE: u8 = 2;
const S_STRING_DOUBLE: u8 = 3;
const S_KEYWORD_MAYBE: u8 = 4;
const S_KEYVALUE: u8 = 5;

pub const LANG: Language = Language {
    name: "YAML",
    extensions: &["yaml", "yml"],
    charsets: &[
        // C_DIGITS
        &[
            // /.-,+*)('&%$#"!
            0b_0000000000000000,
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
        // C_KEY_STRING
        &[
            // /.-,+*)('&%$#"!
            0b_1111111111111110,
            // ?>=<;:9876543210
            0b_1111101111111111,
            // ONMLKJIHGFEDCBA@
            0b_1111111111111111,
            // _^]\[ZYXWVUTSRQP
            0b_1111111111111111,
            // onmlkjihgfedcba`
            0b_1111111111111111,
            //  ~}|{zyxwvutsrqp
            0b_1111111111111111,
        ],
        // C_VALUE_STRING
        &[
            // /.-,+*)('&%$#"!
            0b_1111111101110011,
            // ?>=<;:9876543210
            0b_1111111111111111,
            // ONMLKJIHGFEDCBA@
            0b_1111111111111111,
            // _^]\[ZYXWVUTSRQP
            0b_1111111111111111,
            // onmlkjihgfedcba`
            0b_1111111111111111,
            //  ~}|{zyxwvutsrqp
            0b_1111111111111111,
        ],
    ],
    states: &[
        // S_GROUND
        &[
            // Comments
            T { test: Prefix("#"), kind: Comment, state: Push(S_COMMENT) },
            // Strings
            T { test: Prefix("'"), kind: String, state: Push(S_STRING_SINGLE) },
            T { test: Prefix("\""), kind: String, state: Push(S_STRING_DOUBLE) },
            // Numbers
            T { test: Charset(C_DIGITS), kind: Number, state: Pop(1) },
            // Booleans/Null
            T { test: Prefix("true"), kind: Keyword, state: Push(S_KEYWORD_MAYBE) },
            T { test: Prefix("false"), kind: Keyword, state: Push(S_KEYWORD_MAYBE) },
            T { test: Prefix("null"), kind: Keyword, state: Push(S_KEYWORD_MAYBE) },
            T { test: Charset(C_KEY_STRING), kind: Other, state: Push(S_KEYVALUE) },
        ],
        // S_COMMENT
        &[T { test: Line, kind: Comment, state: Pop(1) }],
        // S_STRING_SINGLE
        &[T { test: Prefix("'"), kind: String, state: Pop(1) }],
        // S_STRING_DOUBLE
        &[T { test: Prefix("\""), kind: String, state: Pop(1) }],
        // S_KEYWORD_MAYBE
        &[
            T { test: Charset(C_KEY_STRING), kind: Other, state: Push(S_KEYVALUE) },
            T { test: Chars(0), kind: Keyword, state: Pop(1) },
        ],
        // S_KEYVALUE
        &[
            T { test: Prefix(":"), kind: Other, state: Push(S_KEYWORD_MAYBE) },
            T { test: Chars(0), kind: Other, state: Pop(1) },
        ],
        // S_VALUE
        &[
            // Comments
            T { test: Prefix("#"), kind: Comment, state: Push(S_COMMENT) },
            // Strings
            T { test: Prefix("'"), kind: String, state: Push(S_STRING_SINGLE) },
            T { test: Prefix("\""), kind: String, state: Push(S_STRING_DOUBLE) },
            // Numbers
            T { test: Charset(C_DIGITS), kind: Number, state: Pop(1) },
            // Booleans/Null
            T { test: Prefix("true"), kind: Keyword, state: Push(S_KEYWORD_MAYBE) },
            T { test: Prefix("false"), kind: Keyword, state: Push(S_KEYWORD_MAYBE) },
            T { test: Prefix("null"), kind: Keyword, state: Push(S_KEYWORD_MAYBE) },
            T { test: Charset(C_KEY_STRING), kind: Other, state: Push(S_KEYVALUE) },
            T { test: Chars(0), kind: Other, state: Pop(1) },
        ],
    ],
};
