// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use stdext::cold_path;
use stdext::unicode::Utf8Chars;

use super::tables::*;
use crate::document::ReadableDocument;
use crate::helpers::{CoordType, Point};
use crate::simd;

// On one hand it's disgusting that I wrote this as a global variable, but on the
// other hand, this isn't a public library API, and it makes the code a lot cleaner,
// because we don't need to inject this once-per-process value everywhere.
static mut AMBIGUOUS_WIDTH: usize = 1;

/// Sets the width of "ambiguous" width characters as per "UAX #11: East Asian Width".
///
/// Defaults to 1.
pub fn setup_ambiguous_width(ambiguous_width: CoordType) {
    unsafe { AMBIGUOUS_WIDTH = ambiguous_width as usize };
}

#[inline]
fn ambiguous_width() -> usize {
    // SAFETY: This is a global variable that is set once per process.
    // It is never changed after that, so this is safe to call.
    unsafe { AMBIGUOUS_WIDTH }
}

/// Stores a position within a [`ReadableDocument`].
///
/// Tracks the byte-offset, logical position (line and grapheme cluster),
/// and the visual column within the current line.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct Cursor {
    /// Offset in bytes within the buffer.
    pub offset: usize,
    /// Logical position: `.y` = line number, `.x` = grapheme cluster index within the line.
    ///
    /// Single-line methods only update `.x` and leave `.y` unchanged.
    /// Multi-line methods (e.g. [`MeasurementConfig::goto_offset_multiline`]) update both.
    pub logical_pos: Point,
    /// Horizontal position in visual columns from the start of the logical line.
    ///
    /// This differs from `logical_pos.x` because grapheme clusters can be wider
    /// than 1 column (e.g. wide CJK characters = 2 columns, tabs = variable).
    pub column: CoordType,
}

/// Your entrypoint to navigating grapheme clusters inside a [`ReadableDocument`].
///
/// The single-line methods (`goto_pos`, `goto_column`, `goto_offset`) navigate
/// **forward** within a single logical line and stop at LF/CRLF.
///
/// The multi-line method (`goto_offset_multiline`) navigates forward across
/// line boundaries, counting newlines to update `logical_pos.y`.
#[derive(Clone)]
pub struct MeasurementConfig<'doc> {
    cursor: Cursor,
    tab_size: CoordType,
    buffer: &'doc dyn ReadableDocument,
}

impl<'doc> MeasurementConfig<'doc> {
    /// Creates a new [`MeasurementConfig`] for the given document.
    pub fn new(buffer: &'doc dyn ReadableDocument) -> Self {
        Self { cursor: Default::default(), tab_size: 8, buffer }
    }

    /// Sets the initial cursor to the given position.
    ///
    /// WARNING: While the code doesn't panic if the cursor is invalid,
    /// the results will obviously be complete garbage.
    pub fn with_cursor(mut self, cursor: Cursor) -> Self {
        self.cursor = cursor;
        self
    }

    /// Sets the tab size.
    ///
    /// Defaults to 8, because that's what a tab in terminals evaluates to.
    pub fn with_tab_size(mut self, tab_size: CoordType) -> Self {
        self.tab_size = tab_size.max(1);
        self
    }

    /// Navigates **forward** to the given absolute offset.
    /// Stops at LF/CRLF.
    ///
    /// # Returns
    ///
    /// The cursor position after the navigation.
    pub fn goto_offset(&mut self, offset: usize) -> Cursor {
        self.measure_forward(offset, CoordType::MAX, CoordType::MAX)
    }

    /// Navigates **forward** to the given grapheme cluster position.
    /// Stops at LF/CRLF. Only updates `logical_pos.x` and `column`.
    ///
    /// # Returns
    ///
    /// The cursor position after the navigation.
    pub fn goto_pos(&mut self, pos_target: CoordType) -> Cursor {
        self.measure_forward(usize::MAX, pos_target, CoordType::MAX)
    }

    /// Navigates **forward** to the given visual column.
    /// Stops at LF/CRLF. Only updates `logical_pos.x` and `column`.
    ///
    /// # Returns
    ///
    /// The cursor position after the navigation.
    pub fn goto_column(&mut self, column_target: CoordType) -> Cursor {
        self.measure_forward(usize::MAX, CoordType::MAX, column_target)
    }

    /// Navigates **forward** to the given absolute byte offset, crossing line boundaries.
    ///
    /// Unlike `goto_offset`, this method does NOT stop at LF/CRLF. It counts newlines
    /// using SIMD to update `logical_pos.y`, then measures graphemes on the final line
    /// for `logical_pos.x` and `column`.
    ///
    /// This is useful for callers (like the JSON parser) that need a full `(line, column)`
    /// from a byte offset without going through `TextBuffer`.
    pub fn goto_offset_multiline(&mut self, target: usize) -> Cursor {
        if self.cursor.offset >= target {
            return self.cursor;
        }

        // Use SIMD to seek forward through lines up to the target offset.
        let mut offset = self.cursor.offset;
        let mut line = self.cursor.logical_pos.y;

        loop {
            let chunk = self.buffer.read_forward(offset);
            if chunk.is_empty() {
                break;
            }

            // How far into this chunk can we go without passing `target`?
            let available = chunk.len().min(target - offset);

            // Count newlines within the usable portion of this chunk.
            let (delta, new_line) = simd::lines_fwd(chunk, 0, line, CoordType::MAX);

            if offset + delta >= target {
                // The target is within this chunk (or we overshot).
                // Count newlines only up to `target`.
                let (_, new_line) = simd::lines_fwd(&chunk[..available], 0, line, CoordType::MAX);
                line = new_line;
                break;
            }

            offset += chunk.len();
            line = new_line;

            if offset >= target {
                break;
            }
        }

        // Now find the start of the final line by seeking backward.
        let line_start = {
            let mut off = target;
            loop {
                let chunk = self.buffer.read_backward(off);
                if chunk.is_empty() {
                    off = 0;
                    break;
                }
                // Scan backward for a newline.
                if let Some(pos) = chunk.iter().rposition(|&b| b == b'\n') {
                    off -= chunk.len() - pos - 1;
                    break;
                }
                off -= chunk.len();
            }
            off
        };

        // Measure graphemes on the final line from its start to the target offset.
        self.cursor =
            Cursor { offset: line_start, logical_pos: Point { x: 0, y: line }, column: 0 };
        self.measure_forward(target, CoordType::MAX, CoordType::MAX)
    }

    /// Returns the current cursor position.
    pub fn cursor(&self) -> Cursor {
        self.cursor
    }

    fn measure_forward(
        &mut self,
        offset_target: usize,
        pos_target: CoordType,
        column_target: CoordType,
    ) -> Cursor {
        if self.cursor.offset >= offset_target
            || self.cursor.logical_pos.x >= pos_target
            || self.cursor.column >= column_target
        {
            return self.cursor;
        }

        let mut offset = self.cursor.offset;
        let mut pos = self.cursor.logical_pos.x;
        let mut column = self.cursor.column;

        let mut chunk_iter = Utf8Chars::new(b"", 0);
        let mut chunk_range = offset..offset;
        let mut props_next_cluster = ucd_start_of_text_properties();

        loop {
            // Have we reached the target already? Stop.
            if offset >= offset_target || pos >= pos_target || column >= column_target {
                break;
            }

            let mut props_last_char;
            let mut offset_next_cluster;
            let mut state = 0;
            let mut width = 0;

            // Since we want to measure the width of the current cluster,
            // by necessity we need to seek to the next cluster.
            // We'll then reuse the offset and properties of the next cluster in
            // the next iteration of the this (outer) loop (`props_next_cluster`).
            loop {
                if !chunk_iter.has_next() {
                    cold_path();
                    chunk_iter = Utf8Chars::new(self.buffer.read_forward(chunk_range.end), 0);
                    chunk_range = chunk_range.end..chunk_range.end + chunk_iter.len();
                }

                // Since this loop seeks ahead to the next cluster, and since `chunk_iter`
                // records the offset of the next character after the returned one, we need
                // to save the offset of the previous `chunk_iter` before calling `next()`.
                // Similar applies to the width.
                props_last_char = props_next_cluster;
                offset_next_cluster = chunk_range.start + chunk_iter.offset();
                width += ucd_grapheme_cluster_character_width(props_next_cluster, ambiguous_width())
                    as CoordType;

                // The `Document::read_forward` interface promises us that it will not split
                // grapheme clusters across chunks. Therefore, we can safely break here.
                let ch = match chunk_iter.next() {
                    Some(ch) => ch,
                    None => break,
                };

                // Get the properties of the next cluster.
                props_next_cluster = ucd_grapheme_cluster_lookup(ch);
                state = ucd_grapheme_cluster_joins(state, props_last_char, props_next_cluster);

                // Stop if the next character does not join.
                if ucd_grapheme_cluster_joins_done(state) {
                    break;
                }
            }

            if offset_next_cluster == offset {
                // No advance and the iterator is empty? End of text reached.
                if chunk_iter.is_empty() {
                    break;
                }
                // Ignore the first iteration when processing the start-of-text.
                continue;
            }

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
            if column + width > column_target {
                break;
            }

            offset = offset_next_cluster;
            pos += 1;
            column += width;
        }

        self.cursor.offset = offset;
        self.cursor.logical_pos.x = pos;
        self.cursor.column = column;
        self.cursor
    }
}

/// A visual line break produced by [`LineWrapIterator`].
///
/// Represents the start of a new visual line within a single logical line.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VisualLineBreak {
    /// Byte offset from the start of the document where this visual line begins.
    pub offset: usize,
    /// Grapheme cluster position from the start of the logical line.
    pub pos: CoordType,
    /// Column from the start of the logical line (used for tab calculations).
    pub column: CoordType,
}

/// Iterates over visual line breaks within a single logical line,
/// given a word wrap column width.
///
/// Each call to `next()` returns the position where the next visual line starts.
/// The first visual line is implicit (starts at the beginning of the logical line),
/// so only subsequent visual lines are returned.
pub struct LineWrapIterator<'doc> {
    buffer: &'doc dyn ReadableDocument,
    tab_size: CoordType,
    word_wrap_column: CoordType,
    // Current scanning position
    offset: usize,
    pos: CoordType,
    column: CoordType,
    // Column within the current visual line
    visual_column: CoordType,
    // Last wrap opportunity
    wrap_opp: bool,
    wrap_opp_offset: usize,
    wrap_opp_pos: CoordType,
    wrap_opp_column: CoordType,
    wrap_opp_visual_column: CoordType,
    // Chunk iteration state
    chunk_iter: Utf8Chars<'doc>,
    chunk_range: std::ops::Range<usize>,
    props_next_cluster: usize,
    // Set to true when we've hit the end of the line
    done: bool,
}

impl<'doc> LineWrapIterator<'doc> {
    /// Creates a new iterator that yields visual line breaks for the logical line
    /// starting at the given offset.
    ///
    /// `word_wrap_column` must be > 0 (caller should not create this if wrapping is disabled).
    pub fn new(
        buffer: &'doc dyn ReadableDocument,
        tab_size: CoordType,
        word_wrap_column: CoordType,
        line_start_offset: usize,
    ) -> Self {
        Self {
            buffer,
            tab_size: tab_size.max(1),
            word_wrap_column,
            offset: line_start_offset,
            pos: 0,
            column: 0,
            visual_column: 0,
            wrap_opp: false,
            wrap_opp_offset: line_start_offset,
            wrap_opp_pos: 0,
            wrap_opp_column: 0,
            wrap_opp_visual_column: 0,
            chunk_iter: Utf8Chars::new(b"", 0),
            chunk_range: line_start_offset..line_start_offset,
            props_next_cluster: ucd_start_of_text_properties(),
            done: false,
        }
    }

    /// Returns the current scanning position as a [`Cursor`].
    /// Useful for knowing where the iterator stopped (end of line).
    pub fn cursor(&self) -> Cursor {
        Cursor {
            offset: self.offset,
            logical_pos: Point { x: self.pos, y: 0 },
            column: self.column,
        }
    }
}

impl Iterator for LineWrapIterator<'_> {
    type Item = VisualLineBreak;

    fn next(&mut self) -> Option<VisualLineBreak> {
        if self.done {
            return None;
        }

        loop {
            let props_current_cluster = self.props_next_cluster;
            let mut props_last_char;
            let mut offset_next_cluster;
            let mut state = 0;
            let mut width = 0;

            // Advance to the next grapheme cluster.
            loop {
                if !self.chunk_iter.has_next() {
                    cold_path();
                    self.chunk_iter =
                        Utf8Chars::new(self.buffer.read_forward(self.chunk_range.end), 0);
                    self.chunk_range =
                        self.chunk_range.end..self.chunk_range.end + self.chunk_iter.len();
                }

                props_last_char = self.props_next_cluster;
                offset_next_cluster = self.chunk_range.start + self.chunk_iter.offset();
                width += ucd_grapheme_cluster_character_width(
                    self.props_next_cluster,
                    ambiguous_width(),
                ) as CoordType;

                let ch = match self.chunk_iter.next() {
                    Some(ch) => ch,
                    None => break,
                };

                self.props_next_cluster = ucd_grapheme_cluster_lookup(ch);
                state = ucd_grapheme_cluster_joins(state, props_last_char, self.props_next_cluster);

                if ucd_grapheme_cluster_joins_done(state) {
                    break;
                }
            }

            if offset_next_cluster == self.offset {
                if self.chunk_iter.is_empty() {
                    // End of text reached.
                    self.done = true;
                    return None;
                }
                // Ignore the first iteration when processing the start-of-text.
                continue;
            }

            width = width.min(2);

            if props_last_char == ucd_tab_properties() {
                unsafe { std::hint::assert_unchecked(self.tab_size >= 1) };
                width = self.tab_size - (self.column % self.tab_size);
            }

            // Hit a newline? Done with this logical line.
            if props_last_char == ucd_linefeed_properties() {
                self.done = true;
                return None;
            }

            // Check if this cluster would exceed the wrap column.
            if self.visual_column + width > self.word_wrap_column {
                let result;

                if !self.wrap_opp {
                    // No wrap opportunity: force-break right here.
                    result =
                        VisualLineBreak { offset: self.offset, pos: self.pos, column: self.column };
                    self.visual_column = 0;
                } else {
                    // Wrap at the last opportunity.
                    result = VisualLineBreak {
                        offset: self.wrap_opp_offset,
                        pos: self.wrap_opp_pos,
                        column: self.wrap_opp_column,
                    };
                    self.visual_column -= self.wrap_opp_visual_column;
                }

                self.wrap_opp = false;

                // Advance past the current cluster.
                self.offset = offset_next_cluster;
                self.pos += 1;
                self.column += width;
                self.visual_column += width;

                // Check for new wrap opportunity.
                if !ucd_line_break_joins(props_current_cluster, self.props_next_cluster) {
                    self.wrap_opp = true;
                    self.wrap_opp_offset = self.offset;
                    self.wrap_opp_pos = self.pos;
                    self.wrap_opp_column = self.column;
                    self.wrap_opp_visual_column = self.visual_column;
                }

                return Some(result);
            }

            // Advance past the current cluster.
            self.offset = offset_next_cluster;
            self.pos += 1;
            self.column += width;
            self.visual_column += width;

            // Track wrap opportunities (word boundaries for line breaking).
            if !ucd_line_break_joins(props_current_cluster, self.props_next_cluster) {
                self.wrap_opp = true;
                self.wrap_opp_offset = self.offset;
                self.wrap_opp_pos = self.pos;
                self.wrap_opp_column = self.column;
                self.wrap_opp_visual_column = self.visual_column;
            }
        }
    }
}

/// Returns an offset past a newline.
///
/// If `offset` is right in front of a newline,
/// this will return the offset past said newline.
pub fn skip_newline(text: &[u8], mut offset: usize) -> usize {
    if offset >= text.len() {
        return offset;
    }
    if text[offset] == b'\r' {
        offset += 1;
    }
    if offset >= text.len() {
        return offset;
    }
    if text[offset] == b'\n' {
        offset += 1;
    }
    offset
}

/// Strips a trailing newline from the given text.
pub fn strip_newline(mut text: &[u8]) -> &[u8] {
    // Rust generates surprisingly tight assembly for this.
    if text.last() == Some(&b'\n') {
        text = &text[..text.len() - 1];
    }
    if text.last() == Some(&b'\r') {
        text = &text[..text.len() - 1];
    }
    text
}

#[cfg(test)]
mod test {
    use super::*;

    struct ChunkedDoc<'a>(&'a [&'a [u8]]);

    impl ReadableDocument for ChunkedDoc<'_> {
        fn read_forward(&self, mut off: usize) -> &[u8] {
            for chunk in self.0 {
                if off < chunk.len() {
                    return &chunk[off..];
                }
                off -= chunk.len();
            }
            &[]
        }

        fn read_backward(&self, mut off: usize) -> &[u8] {
            for chunk in self.0.iter().rev() {
                if off < chunk.len() {
                    return &chunk[..chunk.len() - off];
                }
                off -= chunk.len();
            }
            &[]
        }
    }

    #[test]
    fn test_measure_forward_to_end_of_line() {
        let cursor = MeasurementConfig::new(&"foo\nbar".as_bytes()).goto_column(CoordType::MAX);
        assert_eq!(cursor, Cursor { offset: 3, logical_pos: Point { x: 3, y: 0 }, column: 3 });
    }

    #[test]
    fn test_measure_forward_clipped_wide_char() {
        let cursor = MeasurementConfig::new(&"a😶‍🌫️b".as_bytes()).goto_column(2);
        assert_eq!(cursor, Cursor { offset: 1, logical_pos: Point { x: 1, y: 0 }, column: 1 });
    }

    #[test]
    fn test_measure_forward_tabs() {
        let text = "a\tb\tc".as_bytes();
        let cursor = MeasurementConfig::new(&text).with_tab_size(4).goto_column(4);
        assert_eq!(cursor, Cursor { offset: 2, logical_pos: Point { x: 2, y: 0 }, column: 4 });
    }

    #[test]
    fn test_measure_forward_chunk_boundaries() {
        let chunks = [
            "Hello".as_bytes(),
            "\u{1F469}\u{1F3FB}".as_bytes(), // 8 bytes, 2 columns
            "World".as_bytes(),
        ];
        let doc = ChunkedDoc(&chunks);
        let cursor = MeasurementConfig::new(&doc).goto_column(5 + 2 + 3);
        assert_eq!(cursor.offset, 5 + 8 + 3);
        assert_eq!(cursor.logical_pos.x, 5 + 1 + 3);
    }

    #[test]
    fn test_stops_at_newline() {
        // Should stop at the LF and not cross it.
        let text = "abc\ndef".as_bytes();
        let cursor = MeasurementConfig::new(&text).goto_column(CoordType::MAX);
        assert_eq!(cursor, Cursor { offset: 3, logical_pos: Point { x: 3, y: 0 }, column: 3 });

        // goto_offset past newline should also stop at the newline.
        let cursor = MeasurementConfig::new(&text).goto_offset(5);
        assert_eq!(cursor, Cursor { offset: 3, logical_pos: Point { x: 3, y: 0 }, column: 3 });
    }

    #[test]
    fn test_crlf_stops() {
        let text = "ab\r\ncd".as_bytes();
        let cursor = MeasurementConfig::new(&text).goto_column(CoordType::MAX);
        assert_eq!(cursor, Cursor { offset: 2, logical_pos: Point { x: 2, y: 0 }, column: 2 });
    }

    #[test]
    fn test_wrap_line_basic() {
        //   |foo␣  |
        //   |bar␣  |  (trailing space before \n)
        let text = "foo bar \nbaz".as_bytes();
        let breaks: Vec<_> = LineWrapIterator::new(&text, 8, 6, 0).collect();
        assert_eq!(breaks.len(), 1);
        assert_eq!(breaks[0], VisualLineBreak { offset: 4, pos: 4, column: 4 });
    }

    #[test]
    fn test_wrap_line_force_wrap() {
        // "// aaaaaaaaaaaa" with wrap at 8
        // |//_     |  (3 chars + space = wrap opp at 3)
        // |aaaaaaaa|  (8 a's, force wrap)
        // |aaaa    |  (4 a's remaining)
        let text = "// aaaaaaaaaaaa".as_bytes();
        let breaks: Vec<_> = LineWrapIterator::new(&text, 8, 8, 0).collect();
        assert_eq!(breaks.len(), 2);
        assert_eq!(breaks[0], VisualLineBreak { offset: 3, pos: 3, column: 3 });
        assert_eq!(breaks[1], VisualLineBreak { offset: 11, pos: 11, column: 11 });
    }

    #[test]
    fn test_wrap_line_wide_chars() {
        // Japanese characters forming word wrap opportunities between each character.
        let text = "零一二三四五六七八九";
        let bytes = text.as_bytes();
        let breaks: Vec<_> = LineWrapIterator::new(&bytes, 8, 5, 0).collect();
        // Each char is 2 columns wide. 5 columns = 2 chars per line. 10 chars = 5 lines = 4 breaks.
        assert_eq!(breaks.len(), 4);
        // "零一" = 6 bytes, then "二三" = 6 bytes, etc.
        assert_eq!(breaks[0].offset, 6);
        assert_eq!(breaks[1].offset, 12);
        assert_eq!(breaks[2].offset, 18);
        assert_eq!(breaks[3].offset, 24);
    }

    #[test]
    fn test_wrap_line_no_opportunity() {
        // Yijing Hexagram Symbols form no word wrap opportunities.
        let text = "䷀䷁䷂䷃䷄䷅䷆䷇䷈䷉";
        let bytes = text.as_bytes();
        let breaks: Vec<_> = LineWrapIterator::new(&bytes, 8, 5, 0).collect();
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
        let breaks: Vec<_> = LineWrapIterator::new(&bytes, 4, 8, 0).collect();
        assert_eq!(breaks.len(), 1);
        assert_eq!(breaks[0], VisualLineBreak { offset: 4, pos: 4, column: 4 });
    }

    #[test]
    fn test_strip_newline() {
        assert_eq!(strip_newline(b"hello\n"), b"hello");
        assert_eq!(strip_newline(b"hello\r\n"), b"hello");
        assert_eq!(strip_newline(b"hello"), b"hello");
    }
}
