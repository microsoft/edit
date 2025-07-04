use super::Consume::*;
use super::HighlightKind::*;
use super::StateStack::*;
use super::*;

type T = Transition;

const _GROUND: u8 = 0;

pub const LANG: Language = Language {
    name: "JSON",
    extensions: &["json", "jsonc"],
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
            T { test: Prefix("#"), kind: Comment, state: Push(_GROUND) },
            T { test: Prefix("<#"), kind: Comment, state: Push(_GROUND) },
        ],
    ],
};
