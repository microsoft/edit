// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! Everything related to Unicode lives here.

mod measurement;
mod sanitize;
mod tables;
mod utf8;

pub use measurement::*;
pub use sanitize::*;
pub use utf8::*;

/// Conservatively tests a boundary without the preceding grapheme state.
pub(crate) fn graphemes_may_join(left: &[u8], right: &[u8]) -> bool {
    use tables::*;
    let (Some(&l), Some(&r)) = (left.last(), right.first()) else {
        return false;
    };
    if l.is_ascii() && r.is_ascii() {
        return l == b'\r' && r == b'\n';
    }
    let left = Utf8Chars::new(left, left.len()).prev().unwrap();
    let right = Utf8Chars::new(right, 0).next().unwrap();
    !ucd_grapheme_cluster_joins_done(ucd_grapheme_cluster_joins(
        0,
        ucd_grapheme_cluster_lookup(left),
        ucd_grapheme_cluster_lookup(right),
    ))
}

#[cfg(test)]
mod tests {
    use super::graphemes_may_join;

    #[test]
    fn conservative_grapheme_boundaries() {
        for (left, right, joins) in [
            ("", "a", false),
            ("a", "", false),
            ("a", "b", false),
            ("a", "\u{754c}", false),
            ("\u{754c}", "a", false),
            ("a", "\u{301}", true),
            ("\n", "\u{301}", false),
            ("\r", "\n", true),
            ("\u{1100}", "\u{1161}", true),
            ("\u{200d}", "\u{1f4bb}", true),
            ("\u{600}", "a", true),
            // The real boundary after an RI pair breaks; state zero deliberately overestimates.
            ("\u{1f1fa}\u{1f1f8}", "\u{1f1ec}", true),
        ] {
            assert_eq!(
                graphemes_may_join(left.as_bytes(), right.as_bytes()),
                joins,
                "{left:?} | {right:?}"
            );
        }
    }
}
