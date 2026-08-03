// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use stdext::cold_path;
use stdext::unicode::Utf8Chars;

use super::measurement::{Cursor, ambiguous_width};
use super::tables::*;
use crate::document::ReadableDocument;
use crate::helpers::{CoordType, Point};

/// The result of laying out a single row with [`WordWrapper::next_row`].
pub struct VisualRow {
    /// Offset at which the text of the row ends.
    /// Excludes a trailing line feed, if there is one.
    pub end: usize,
    /// `true` if another row follows, in which case the cursor
    /// of the [`WordWrapper`] now sits at the start of it.
    pub more: bool,
}

/// Lays out a [`ReadableDocument`] into word-wrapped rows.
///
/// It's the counterpart to [`MeasurementConfig`](super::MeasurementConfig):
/// It only ever moves along the Y axis and its cursor always sits at the
/// start of a row, that is, at a `visual_pos.x` of 0.
#[derive(Clone)]
pub struct WordWrapper<'doc> {
    cursor: Cursor,
    tab_size: CoordType,
    word_wrap_column: CoordType,
    buffer: &'doc dyn ReadableDocument,
}

impl<'doc> WordWrapper<'doc> {
    /// Creates a new [`WordWrapper`] that wraps at the given column.
    pub fn new(buffer: &'doc dyn ReadableDocument, word_wrap_column: CoordType) -> Self {
        debug_assert!(word_wrap_column > 0);
        Self { cursor: Default::default(), tab_size: 8, word_wrap_column, buffer }
    }

    /// Sets the initial cursor. It must sit at the start of a row.
    ///
    /// WARNING: While the code doesn't panic if the cursor is invalid,
    /// the results will obviously be complete garbage.
    pub fn with_cursor(mut self, cursor: Cursor) -> Self {
        debug_assert!(cursor.visual_pos.x == 0);
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

    /// Returns the start of the row we're currently on.
    pub fn cursor(&self) -> Cursor {
        self.cursor
    }

    /// Lays out the row the cursor is on and, if another row follows,
    /// advances the cursor to the start of it.
    pub fn next_row(&mut self) -> VisualRow {
        let ambiguous_width = ambiguous_width();
        let tab_size = self.tab_size;
        let word_wrap_column = self.word_wrap_column;

        let mut offset = self.cursor.offset;
        let mut logical_pos_x = self.cursor.logical_pos.x;
        let mut column = self.cursor.column;
        let mut visual_pos_x = 0;

        // The position just past the last wrap opportunity on this row.
        // `opp` being false means that the row consists of a single word so far.
        let mut opp = false;
        let mut opp_offset = 0;
        let mut opp_logical_pos_x = 0;
        let mut opp_column = 0;

        let mut chunk_iter = Utf8Chars::new(b"", 0);
        let mut chunk_range = offset..offset;
        let mut props_next_cluster = ucd_start_of_text_properties();

        loop {
            let props_current_cluster = props_next_cluster;
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
                    return VisualRow { end: offset, more: false };
                }
                // Ignore the first iteration when processing the start-of-text.
                continue;
            }

            // The max. width of a terminal cell is 2.
            width = width.min(2);

            // Hard wrap: The next row is the start of the next logical line.
            if props_last_char == ucd_linefeed_properties() {
                cold_path();
                self.cursor = Cursor {
                    offset: offset_next_cluster,
                    logical_pos: Point { x: 0, y: self.cursor.logical_pos.y + 1 },
                    visual_pos: Point { x: 0, y: self.cursor.visual_pos.y + 1 },
                    column: 0,
                };
                return VisualRow { end: offset, more: true };
            }

            // Tabs require special handling because they can have a variable width.
            if props_last_char == ucd_tab_properties() {
                // SAFETY: `self.tab_size` is clamped to >= 1 in `with_tab_size`.
                // This assert ensures that Rust doesn't insert panicking null checks.
                unsafe { std::hint::assert_unchecked(tab_size >= 1) };
                width = tab_size - (column % tab_size);
            }

            // Soft wrap: The cluster doesn't fit anymore, so the row ends at the last wrap
            // opportunity. If there was none, the word is wider than the row and we break
            // right here. Testing for `visual_pos_x` ensures that we always consume at least
            // one cluster, even if that single cluster is wider than the entire row.
            if visual_pos_x + width > word_wrap_column && visual_pos_x > 0 {
                let end = if opp {
                    logical_pos_x = opp_logical_pos_x;
                    column = opp_column;
                    opp_offset
                } else {
                    offset
                };
                self.cursor = Cursor {
                    offset: end,
                    logical_pos: Point { x: logical_pos_x, y: self.cursor.logical_pos.y },
                    visual_pos: Point { x: 0, y: self.cursor.visual_pos.y + 1 },
                    column,
                };
                return VisualRow { end, more: true };
            }

            offset = offset_next_cluster;
            logical_pos_x += 1;
            visual_pos_x += width;
            column += width;

            if !ucd_line_break_joins(props_current_cluster, props_next_cluster) {
                opp = true;
                opp_offset = offset;
                opp_logical_pos_x = logical_pos_x;
                opp_column = column;
            }
        }
    }
}
