use stdext::cold_path;
use stdext::unicode::Utf8Chars;

use crate::document::ReadableDocument;
use crate::helpers::CoordType;
use crate::unicode::ambiguous_width;
use crate::unicode::tables::*;

/// Splits a string into lines of a given width, breaking at word boundaries.
/// Returns [`None`] at the next hard newline or at the end of the string.
///
/// # Warning
///
/// The iterator assumes that the input starts at a newline boundary (first column).
struct LineWrapIterator<'a> {
    doc: &'a dyn ReadableDocument,
    offset: usize,
    column: CoordType,
    tab_size: CoordType,
    word_wrap_column: CoordType,
}

impl<'a> LineWrapIterator<'a> {
    pub fn new(
        doc: &'a dyn ReadableDocument,
        tab_size: CoordType,
        word_wrap_column: CoordType,
    ) -> Self {
        debug_assert!(word_wrap_column > 0);
        let word_wrap_column = word_wrap_column.max(1);
        Self { doc, offset: 0, column: 0, tab_size, word_wrap_column }
    }

    /// Resets the iterator and starts the iteration from the given offset.
    ///
    /// # Warning
    ///
    /// The iterator assumes that the input starts at a newline boundary (first column).
    pub fn reset(&mut self, offset: usize) {
        self.offset = offset;
        self.column = 0;
    }
}

impl<'a> Iterator for LineWrapIterator<'a> {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        let mut offset = self.offset;
        let mut column = self.column;
        let mut wrap_offset = usize::MAX;

        let mut chunk_iter = Utf8Chars::new(b"", 0);

        let mut props_next_cluster = ucd_start_of_text_properties();
        let mut props_current_cluster = ucd_start_of_text_properties();
        let mut props_last_char = ucd_start_of_text_properties();
        let mut offset_next_cluster = ucd_start_of_text_properties();

        let mut state = 0;
        let mut width = 0;

        loop {
            if props_next_cluster == ucd_linefeed_properties() {
                return None;
            }

            if !chunk_iter.has_next() {
                cold_path();
                chunk_iter = Utf8Chars::new(self.doc.read_forward(offset), 0);
            }

            let ch = match chunk_iter.next() {
                Some(ch) => ch,
                None => break,
            };

            width += ucd_grapheme_cluster_character_width(props_next_cluster, ambiguous_width())
                as CoordType;
            props_next_cluster = ucd_grapheme_cluster_lookup(ch);
            state = ucd_grapheme_cluster_joins(state, props_last_char, props_next_cluster);

            // Grapheme cluster break?
            if ucd_grapheme_cluster_joins_done(state) {
                // The max. width of a terminal cell is 2.
                width = width.min(2);

                // Tabs require special handling because they can have a variable width.
                if props_last_char == ucd_tab_properties() {
                    // SAFETY: `self.tab_size` is clamped to >= 1 in `with_tab_size`.
                    // This assert ensures that Rust doesn't insert panicking null checks.
                    unsafe { std::hint::assert_unchecked(self.tab_size >= 1) };
                    width = self.tab_size - (column % self.tab_size);
                }

                // Stop at a newline. Never cross it.
                if props_last_char == ucd_linefeed_properties() {
                    break;
                }

                // Avoid advancing past the column target, because `width` can be greater than 1.
                if column + width > self.word_wrap_column {
                    break;
                }

                // Wrap opportunity?
                if !ucd_line_break_joins(props_current_cluster, props_next_cluster) {
                    wrap_offset = offset;
                }

                // Reset the state, since we're starting a new cluster.
                props_current_cluster = props_next_cluster;
                state = 0;
                width = 0;
            }

            offset = offset_next_cluster;
            column += width;
        }

        self.offset = offset;
        self.column = column;

        if wrap_offset != usize::MAX {
            offset = wrap_offset;
        }
        Some(offset)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_wrap_line_basic() {
        //   |foo␣  |
        //   |bar␣  |  (trailing space before \n)
        let text = "foo bar \nbaz".as_bytes();
        let breaks: Vec<_> = LineWrapIterator::new(&text, 8, 6).collect();
        assert_eq!(breaks.len(), 1);
        assert_eq!(breaks[0], 4);
    }

    #[test]
    fn test_wrap_line_force_wrap() {
        // "// aaaaaaaaaaaa" with wrap at 8
        // |//_     |  (3 chars + space = wrap opp at 3)
        // |aaaaaaaa|  (8 a's, force wrap)
        // |aaaa    |  (4 a's remaining)
        let text = "// aaaaaaaaaaaa".as_bytes();
        let breaks: Vec<_> = LineWrapIterator::new(&text, 8, 8).collect();
        assert_eq!(breaks.len(), 2);
        assert_eq!(breaks[0], 3);
        assert_eq!(breaks[1], 11);
    }

    #[test]
    fn test_wrap_line_wide_chars() {
        // Japanese characters forming word wrap opportunities between each character.
        let text = "零一二三四五六七八九";
        let bytes = text.as_bytes();
        let breaks: Vec<_> = LineWrapIterator::new(&bytes, 8, 5).collect();
        // Each char is 2 columns wide. 5 columns = 2 chars per line. 10 chars = 5 lines = 4 breaks.
        assert_eq!(breaks.len(), 4);
        // "零一" = 6 bytes, then "二三" = 6 bytes, etc.
        assert_eq!(breaks[0], 6);
        assert_eq!(breaks[1], 12);
        assert_eq!(breaks[2], 18);
        assert_eq!(breaks[3], 24);
    }

    #[test]
    fn test_wrap_line_no_opportunity() {
        // Yijing Hexagram Symbols form no word wrap opportunities.
        let text = "䷀䷁䷂䷃䷄䷅䷆䷇䷈䷉";
        let bytes = text.as_bytes();
        let breaks: Vec<_> = LineWrapIterator::new(&bytes, 8, 5).collect();
        // Each char is 2 columns wide, no wrap opportunities, so force-wraps at every 2 chars.
        assert_eq!(breaks.len(), 4);
    }

    #[test]
    fn test_wrap_line_with_tab() {
        // "foo \t b" with wrap at 8, tab size 4
        // |foo_    | <- "foo " = 4 cols, wrap opp
        // |____b   | <- "\t" = 4 cols (tabstop at col 4->8 but visual starts at 0), " b" = 2 cols
        let text = "foo \t b";
        let bytes = text.as_bytes();
        let breaks: Vec<_> = LineWrapIterator::new(&bytes, 4, 8).collect();
        assert_eq!(breaks.len(), 1);
        assert_eq!(breaks[0], 4);
    }
}
