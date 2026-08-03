// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use stdext::cold_path;
use stdext::unicode::Utf8Chars;

use super::tables::*;
use crate::document::ReadableDocument;
use crate::helpers::{CoordType, Point};

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
pub(super) fn ambiguous_width() -> usize {
    // SAFETY: This is a global variable that is set once per process.
    // It is never changed after that, so this is safe to call.
    unsafe { AMBIGUOUS_WIDTH }
}

/// Stores a position inside a [`ReadableDocument`].
///
/// The cursor tracks both the absolute byte-offset,
/// as well as the position in terminal-related coordinates.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct Cursor {
    /// Offset in bytes within the buffer.
    pub offset: usize,
    /// Position in the buffer in lines (.y) and grapheme clusters (.x).
    ///
    /// Line wrapping has NO influence on this.
    pub logical_pos: Point,
    /// Position in the buffer in laid out rows (.y) and columns (.x).
    ///
    /// Line wrapping has an influence on this.
    pub visual_pos: Point,
    /// Horizontal position in visual columns.
    ///
    /// Line wrapping has NO influence on this and if word wrap is disabled,
    /// it's identical to `visual_pos.x`. This is useful for calculating tab widths.
    pub column: CoordType,
}

/// Measures text along the X axis inside a [`ReadableDocument`].
///
/// It never crosses a line feed and never modifies the `.y` members of the
/// [`Cursor`]. Y axis navigation is a SIMD newline search, or a
/// [`WordWrapper`](super::WordWrapper) if word wrap is enabled.
#[derive(Clone)]
pub struct MeasurementConfig<'doc> {
    cursor: Cursor,
    end_offset: usize,
    tab_size: CoordType,
    buffer: &'doc dyn ReadableDocument,
}

impl<'doc> MeasurementConfig<'doc> {
    /// Creates a new [`MeasurementConfig`] for the given document.
    pub fn new(buffer: &'doc dyn ReadableDocument) -> Self {
        Self { cursor: Default::default(), end_offset: usize::MAX, tab_size: 8, buffer }
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

    /// Sets a hard upper bound for the offset that any navigation may reach.
    ///
    /// Defaults to [`usize::MAX`]. Use this to restrict measurements to a
    /// single, word-wrapped row, as those don't end in a line feed.
    pub fn with_end_offset(mut self, end_offset: usize) -> Self {
        self.end_offset = end_offset;
        self
    }

    /// Navigates **forward** to the given absolute offset.
    ///
    /// # Returns
    ///
    /// The cursor position after the navigation.
    pub fn goto_offset(&mut self, offset: usize) -> Cursor {
        self.measure_forward(offset, CoordType::MAX, CoordType::MAX)
    }

    /// Navigates **forward** to the given logical column,
    /// that is, the given number of grapheme clusters.
    ///
    /// # Returns
    ///
    /// The cursor position after the navigation.
    pub fn goto_logical_x(&mut self, logical_target_x: CoordType) -> Cursor {
        self.measure_forward(usize::MAX, logical_target_x, CoordType::MAX)
    }

    /// Navigates **forward** to the given visual column.
    ///
    /// # Returns
    ///
    /// The cursor position after the navigation.
    pub fn goto_visual_x(&mut self, visual_target_x: CoordType) -> Cursor {
        self.measure_forward(usize::MAX, CoordType::MAX, visual_target_x)
    }

    /// Returns the current cursor position.
    pub fn cursor(&self) -> Cursor {
        self.cursor
    }

    fn measure_forward(
        &mut self,
        offset_target: usize,
        logical_target_x: CoordType,
        visual_target_x: CoordType,
    ) -> Cursor {
        let offset_target = offset_target.min(self.end_offset);
        let ambiguous_width = ambiguous_width();
        let tab_size = self.tab_size;

        let mut offset = self.cursor.offset;
        let mut logical_pos_x = self.cursor.logical_pos.x;
        let mut visual_pos_x = self.cursor.visual_pos.x;
        let mut column = self.cursor.column;

        let mut chunk_iter = Utf8Chars::new(b"", 0);
        let mut chunk_range = offset..offset;
        let mut props_next_cluster = ucd_start_of_text_properties();

        while offset < offset_target
            && logical_pos_x < logical_target_x
            && visual_pos_x < visual_target_x
        {
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
                width += ucd_grapheme_cluster_character_width(props_next_cluster, ambiguous_width)
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

            // We only ever measure a single line. Crossing line feeds is the
            // caller's job, because it can do so with a SIMD search.
            if props_last_char == ucd_linefeed_properties() {
                cold_path();
                break;
            }

            // Tabs require special handling because they can have a variable width.
            if props_last_char == ucd_tab_properties() {
                // SAFETY: `self.tab_size` is clamped to >= 1 in `with_tab_size`.
                // This assert ensures that Rust doesn't insert panicking null checks.
                unsafe { std::hint::assert_unchecked(tab_size >= 1) };
                width = tab_size - (column % tab_size);
            }

            // Avoid advancing past the visual target, because `width` can be greater than 1.
            if visual_pos_x + width > visual_target_x {
                break;
            }

            offset = offset_next_cluster;
            logical_pos_x += 1;
            visual_pos_x += width;
            column += width;
        }

        self.cursor.offset = offset;
        self.cursor.logical_pos.x = logical_pos_x;
        self.cursor.visual_pos.x = visual_pos_x;
        self.cursor.column = column;
        self.cursor
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
    fn test_measure_forward_clipped_wide_char() {
        let cursor = MeasurementConfig::new(&"a😶‍🌫️b".as_bytes()).goto_visual_x(2);
        assert_eq!(
            cursor,
            Cursor {
                offset: 1,
                logical_pos: Point { x: 1, y: 0 },
                visual_pos: Point { x: 1, y: 0 },
                column: 1,
            }
        );
    }

    #[test]
    fn test_measure_forward_tabs() {
        let text = "a\tb\tc".as_bytes();
        let cursor = MeasurementConfig::new(&text).with_tab_size(4).goto_visual_x(4);
        assert_eq!(
            cursor,
            Cursor {
                offset: 2,
                logical_pos: Point { x: 2, y: 0 },
                visual_pos: Point { x: 4, y: 0 },
                column: 4,
            }
        );
    }

    #[test]
    fn test_measure_forward_chunk_boundaries() {
        let chunks = [
            "Hello".as_bytes(),
            "\u{1F469}\u{1F3FB}".as_bytes(), // 8 bytes, 2 columns
            "World".as_bytes(),
        ];
        let doc = ChunkedDoc(&chunks);
        let cursor = MeasurementConfig::new(&doc).goto_visual_x(5 + 2 + 3);
        assert_eq!(cursor.offset, 5 + 8 + 3);
        assert_eq!(cursor.logical_pos, Point { x: 5 + 1 + 3, y: 0 });
    }

    #[test]
    fn test_strip_newline() {
        assert_eq!(strip_newline(b"hello\n"), b"hello");
        assert_eq!(strip_newline(b"hello\r\n"), b"hello");
        assert_eq!(strip_newline(b"hello"), b"hello");
    }
}
