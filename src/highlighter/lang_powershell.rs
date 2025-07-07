use super::Action::*;
use super::Consume::*;
use super::HighlightKind::*;
use super::*;

type T = Transition<'static>;

// NOTE: These are indices into the `LANG.charsets` array.
const C_DIGITS: usize = 0;
const C_METHOD: usize = 1;

// NOTE: These are indices into the `LANG.states` array.
const _S_GROUND: u8 = 0;
const S_LINE_COMMENT: u8 = 1;
const S_BLOCK_COMMENT: u8 = 2;
const S_STRING_SINGLE: u8 = 3;
const S_STRING_DOUBLE: u8 = 4;
const S_STRING_ESCAPE: u8 = 5;
const S_VARIABLE: u8 = 6;
const S_VARIABLE_BRACE: u8 = 7;
const S_VARIABLE_PAREN: u8 = 8;
const S_PARAMETER: u8 = 9;
const S_KEYWORD: u8 = 10;
const S_METHOD: u8 = 11;

pub const LANG: Language = Language {
    name: "PowerShell",
    extensions: &["ps1", "psm1", "psd1"],
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
        // C_METHOD
        &[
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
    ],
    states: &[
        // S_GROUND
        &[
            // Comments
            T { test: Prefix("#"), kind: Comment, state: Push(S_LINE_COMMENT) },
            T { test: Prefix("<#"), kind: Comment, state: Push(S_BLOCK_COMMENT) },
            // Numbers
            T { test: Charset(C_DIGITS), kind: Number, state: Pop(1) },
            // Strings
            T { test: Prefix("'"), kind: String, state: Push(S_STRING_SINGLE) },
            T { test: Prefix("\""), kind: String, state: Push(S_STRING_DOUBLE) },
            // Variables
            T { test: Prefix("$("), kind: Other, state: Push(S_VARIABLE_PAREN) },
            T { test: Prefix("$"), kind: Variable, state: Push(S_VARIABLE) },
            // Operators
            T { test: Prefix("-"), kind: Operator, state: Push(S_PARAMETER) },
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
            T { test: PrefixInsensitive("break"), kind: Keyword, state: Push(S_KEYWORD) },
            T { test: PrefixInsensitive("catch"), kind: Keyword, state: Push(S_KEYWORD) },
            T { test: PrefixInsensitive("continue"), kind: Keyword, state: Push(S_KEYWORD) },
            T { test: PrefixInsensitive("do"), kind: Keyword, state: Push(S_KEYWORD) },
            T { test: PrefixInsensitive("else"), kind: Keyword, state: Push(S_KEYWORD) },
            T { test: PrefixInsensitive("finally"), kind: Keyword, state: Push(S_KEYWORD) },
            T { test: PrefixInsensitive("foreach"), kind: Keyword, state: Push(S_KEYWORD) },
            T { test: PrefixInsensitive("function"), kind: Keyword, state: Push(S_KEYWORD) },
            T { test: PrefixInsensitive("if"), kind: Keyword, state: Push(S_KEYWORD) },
            T { test: PrefixInsensitive("return"), kind: Keyword, state: Push(S_KEYWORD) },
            T { test: PrefixInsensitive("switch"), kind: Keyword, state: Push(S_KEYWORD) },
            T { test: PrefixInsensitive("throw"), kind: Keyword, state: Push(S_KEYWORD) },
            T { test: PrefixInsensitive("try"), kind: Keyword, state: Push(S_KEYWORD) },
            T { test: PrefixInsensitive("using"), kind: Keyword, state: Push(S_KEYWORD) },
            T { test: PrefixInsensitive("while"), kind: Keyword, state: Push(S_KEYWORD) },
            // Methods
            T { test: Charset(C_METHOD), kind: Method, state: Push(S_METHOD) },
        ],
        // S_LINE_COMMENT: # comment
        &[T { test: Line, kind: Comment, state: Pop(1) }],
        // S_BLOCK_COMMENT: <# comment #>
        &[T { test: Prefix("#>"), kind: Comment, state: Pop(1) }],
        // S_STRING_SINGLE: 'string'
        &[T { test: Prefix("'"), kind: String, state: Pop(1) }],
        // S_STRING_DOUBLE: "string"
        &[
            T { test: Prefix("`"), kind: String, state: Push(S_STRING_ESCAPE) },
            T { test: Prefix("$("), kind: Other, state: Push(S_VARIABLE_PAREN) },
            T { test: Prefix("$"), kind: Variable, state: Push(S_VARIABLE) },
            T { test: Prefix("\""), kind: String, state: Pop(1) },
        ],
        // S_STRING_ESCAPE: "`a"
        &[T { test: Chars(1), kind: String, state: Pop(1) }],
        // S_VARIABLE: $variable
        &[
            T { test: Prefix("{"), kind: Variable, state: Change(S_VARIABLE_BRACE) },
            T { test: Charset(C_METHOD), kind: Variable, state: Pop(1) },
        ],
        // S_VARIABLE_BRACE: ${variable}
        &[T { test: Prefix("}"), kind: Variable, state: Pop(1) }],
        // S_VARIABLE_PAREN: $(command)
        // This is largely a copy of the ground state.
        &[
            // Ground state Overrides
            T { test: Prefix("("), kind: Other, state: Push(S_VARIABLE_PAREN) },
            T { test: Prefix(")"), kind: Other, state: Pop(1) },
            // Numbers
            T { test: Charset(C_DIGITS), kind: Number, state: Pop(1) },
            // Strings
            T { test: Prefix("'"), kind: String, state: Push(S_STRING_SINGLE) },
            T { test: Prefix("\""), kind: String, state: Push(S_STRING_DOUBLE) },
            // Variables
            T { test: Prefix("$"), kind: Variable, state: Push(S_VARIABLE) },
            // Operators
            T { test: Prefix("-"), kind: Operator, state: Push(S_PARAMETER) },
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
            T { test: PrefixInsensitive("break"), kind: Keyword, state: Push(S_KEYWORD) },
            T { test: PrefixInsensitive("catch"), kind: Keyword, state: Push(S_KEYWORD) },
            T { test: PrefixInsensitive("continue"), kind: Keyword, state: Push(S_KEYWORD) },
            T { test: PrefixInsensitive("do"), kind: Keyword, state: Push(S_KEYWORD) },
            T { test: PrefixInsensitive("else"), kind: Keyword, state: Push(S_KEYWORD) },
            T { test: PrefixInsensitive("finally"), kind: Keyword, state: Push(S_KEYWORD) },
            T { test: PrefixInsensitive("foreach"), kind: Keyword, state: Push(S_KEYWORD) },
            T { test: PrefixInsensitive("function"), kind: Keyword, state: Push(S_KEYWORD) },
            T { test: PrefixInsensitive("if"), kind: Keyword, state: Push(S_KEYWORD) },
            T { test: PrefixInsensitive("return"), kind: Keyword, state: Push(S_KEYWORD) },
            T { test: PrefixInsensitive("switch"), kind: Keyword, state: Push(S_KEYWORD) },
            T { test: PrefixInsensitive("throw"), kind: Keyword, state: Push(S_KEYWORD) },
            T { test: PrefixInsensitive("try"), kind: Keyword, state: Push(S_KEYWORD) },
            T { test: PrefixInsensitive("using"), kind: Keyword, state: Push(S_KEYWORD) },
            T { test: PrefixInsensitive("while"), kind: Keyword, state: Push(S_KEYWORD) },
            // Methods
            T { test: Charset(C_METHOD), kind: Method, state: Push(S_METHOD) },
        ],
        // S_PARAMETER: -parameter
        &[
            T { test: Charset(C_METHOD), kind: Operator, state: Pop(1) },
            T { test: Chars(0), kind: Operator, state: Pop(1) },
        ],
        // S_KEYWORD: foreach, if, etc.
        &[
            T { test: Charset(C_METHOD), kind: Method, state: Change(S_METHOD) },
            T { test: Chars(0), kind: Keyword, state: Pop(1) },
        ],
        // S_METHOD: Foo-Bar
        &[
            T { test: Charset(C_METHOD), kind: Method, state: Change(S_METHOD) },
            T { test: Prefix("-"), kind: Method, state: Change(S_METHOD) },
            T { test: Chars(0), kind: Method, state: Pop(1) },
        ],
    ],
};
