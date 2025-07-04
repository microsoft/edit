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
const S_STRING: u8 = 2;
const S_VARIABLE: u8 = 3;

pub const LANG: Language = Language {
    name: "Batch",
    extensions: &["bat", "cmd"],
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
        // C_COMMAND
        &[
            // /.-,+*)('&%$#"!
            0b_0000000000000000,
            // ?>=<;:9876543210
            0b_0000001111111111,
            // ONMLKJIHGFEDCBA@
            0b_1111111111111110,
            // _^]\[ZYXWVUTSRQP
            0b_1000011111111111,
            // onmlkjihgfedcba`
            0b_1111111111111111,
            //  ~}|{zyxwvutsrqp
            0b_0100011111111111,
        ],
    ],
    states: &[
        // S_GROUND
        &[
            // Comments (REM or ::)
            T { test: PrefixInsensitive("rem "), kind: Comment, state: Push(S_COMMENT) },
            T { test: Prefix("::"), kind: Comment, state: Push(S_COMMENT) },
            // Strings (quoted)
            T { test: Prefix("\""), kind: String, state: Push(S_STRING) },
            // Variables
            T { test: Prefix("%"), kind: Variable, state: Push(S_VARIABLE) },
            // Numbers
            T { test: Charset(C_DIGITS), kind: Number, state: Pop(1) },
            // Operators
            T { test: Prefix("|"), kind: Operator, state: Pop(1) },
            T { test: Prefix("&"), kind: Operator, state: Pop(1) },
            T { test: Prefix("<"), kind: Operator, state: Pop(1) },
            T { test: Prefix(">"), kind: Operator, state: Pop(1) },
            // Keywords (common)
            T { test: PrefixInsensitive("if"), kind: Keyword, state: Pop(1) },
            T { test: PrefixInsensitive("else"), kind: Keyword, state: Pop(1) },
            T { test: PrefixInsensitive("for"), kind: Keyword, state: Pop(1) },
            T { test: PrefixInsensitive("in"), kind: Keyword, state: Pop(1) },
            T { test: PrefixInsensitive("do"), kind: Keyword, state: Pop(1) },
            T { test: PrefixInsensitive("not"), kind: Keyword, state: Pop(1) },
            T { test: PrefixInsensitive("exist"), kind: Keyword, state: Pop(1) },
            T { test: PrefixInsensitive("set"), kind: Keyword, state: Pop(1) },
            T { test: PrefixInsensitive("echo"), kind: Keyword, state: Pop(1) },
            T { test: PrefixInsensitive("goto"), kind: Keyword, state: Pop(1) },
            T { test: PrefixInsensitive("call"), kind: Keyword, state: Pop(1) },
        ],
        // S_COMMENT
        &[T { test: Line, kind: Comment, state: Pop(1) }],
        // S_STRING
        &[T { test: Prefix("\""), kind: String, state: Pop(1) }],
        // S_VARIABLE
        &[
            T { test: Prefix("%"), kind: Variable, state: Pop(1) },
            T { test: Charset(C_VARIABLE), kind: Variable, state: Pop(1) },
        ],
    ],
};
