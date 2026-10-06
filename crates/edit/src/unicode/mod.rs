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

/// Returns `true` if `left` and `right` may join as graphemes.
/// This may produce false positives.
pub fn graphemes_may_join(left: &[u8], right: &[u8]) -> bool {
    use tables::*;

    let Some(left) = utf8_decode_last(left) else {
        return false;
    };
    let Some(right) = Utf8Chars::new(right, 0).next() else {
        return false;
    };

    let left = ucd_grapheme_cluster_lookup(left);
    let right = ucd_grapheme_cluster_lookup(right);
    let state = ucd_grapheme_cluster_joins(0, left, right);
    !ucd_grapheme_cluster_joins_done(state)
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_graphemes_may_join() {
        for (left, right, joins) in [
            ("", "a", false),
            ("a", "", false),
            ("a", "b", false),
            ("\r", "\n", true),
            ("a", "\u{754c}", false),
            ("\u{754c}", "a", false),
            ("a", "\u{0301}", true),
            ("\u{0600}", "a", true),
            ("\u{1100}", "\u{1161}", true),
        ] {
            assert_eq!(
                super::graphemes_may_join(left.as_bytes(), right.as_bytes()),
                joins,
                "{left:?} | {right:?}"
            );
        }
    }
}
