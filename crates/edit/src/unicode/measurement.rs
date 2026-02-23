// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::ops::Range;

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
    /// Position in the buffer in lines (.y) and grapheme clusters (.x).
    ///
    /// Line wrapping has NO influence on this.
    pub logical_pos: Point,
    /// Position in the buffer in laid out rows (.y) and columns (.x).
    ///
    /// Line wrapping has an influence on this.
    pub visual_pos: Point,
    /// Horizontal position in visual columns from the start of the logical line.
    ///
    /// Line wrapping has NO influence on this and if word wrap is disabled,
    /// it's identical to `visual_pos.x`. This is useful for calculating tab widths.
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
    word_wrap_column: CoordType,
    buffer: &'doc dyn ReadableDocument,
}

impl<'doc> MeasurementConfig<'doc> {
    /// Creates a new [`MeasurementConfig`] for the given document.
    pub fn new(buffer: &'doc dyn ReadableDocument) -> Self {
        Self { cursor: Default::default(), tab_size: 8, word_wrap_column: 0, buffer }
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

    /// Sets the word wrap column.
    ///
    /// A value of 0 (the default) disables word wrap.
    pub fn with_word_wrap_column(mut self, word_wrap_column: CoordType) -> Self {
        self.word_wrap_column = word_wrap_column;
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
        self.cursor = Cursor {
            offset: line_start,
            logical_pos: Point { x: 0, y: line },
            visual_pos: Point { x: 0, y: 0 },
            column: 0,
        };
        self.measure_forward(target, CoordType::MAX, CoordType::MAX)
    }

    /// Returns the current cursor position.
    pub fn cursor(&self) -> Cursor {
        self.cursor
    }

    // ---- Single-line navigation with visual_pos update ----

    /// Navigates forward by grapheme clusters within the current logical line,
    /// seeking to the given grapheme position. Updates `visual_pos.x` when word wrap is off.
    pub fn goto_pos_on_line(&mut self, pos: CoordType) -> Cursor {
        self.measure_forward(usize::MAX, pos, CoordType::MAX);
        if self.word_wrap_column <= 0 {
            self.cursor.visual_pos.x = self.cursor.column;
        }
        self.cursor
    }

    /// Navigates forward by grapheme clusters within the current logical line,
    /// seeking to the given column. Updates `visual_pos.x` when word wrap is off.
    pub fn goto_column_on_line(&mut self, column: CoordType) -> Cursor {
        self.measure_forward(usize::MAX, CoordType::MAX, column);
        if self.word_wrap_column <= 0 {
            self.cursor.visual_pos.x = self.cursor.column;
        }
        self.cursor
    }

    /// Navigates forward by grapheme clusters within the current logical line,
    /// seeking to the given byte offset. Updates `visual_pos.x` when word wrap is off.
    pub fn goto_offset_on_line(&mut self, offset: usize) -> Cursor {
        self.measure_forward(offset, CoordType::MAX, CoordType::MAX);
        if self.word_wrap_column <= 0 {
            self.cursor.visual_pos.x = self.cursor.column;
        }
        self.cursor
    }

    // ---- Multi-line helpers ----

    /// Counts the number of visual rows a logical line occupies when word wrap is enabled.
    /// Returns 1 if word wrap is disabled.
    pub fn count_visual_rows_for_line(&self, line_start_offset: usize) -> CoordType {
        if self.word_wrap_column <= 0 {
            return 1;
        }
        let mut count: CoordType = 1;
        for _ in LineWrapIterator::new(
            self.buffer,
            self.tab_size,
            self.word_wrap_column,
            line_start_offset,
        ) {
            count += 1;
        }
        count
    }

    /// Computes and sets `visual_pos` for the current cursor within its logical line.
    ///
    /// Requires that `cursor.visual_pos.y` is set to the visual row of the START
    /// of this logical line. This function adds the intra-line visual row offset.
    pub fn compute_visual_pos(&mut self, line_start_offset: usize) {
        if self.word_wrap_column <= 0 {
            self.cursor.visual_pos.x = self.cursor.column;
            return;
        }

        let mut visual_x = self.cursor.column;
        let mut sub_row: CoordType = 0;

        for brk in LineWrapIterator::new(
            self.buffer,
            self.tab_size,
            self.word_wrap_column,
            line_start_offset,
        ) {
            if brk.offset > self.cursor.offset {
                break;
            }
            sub_row += 1;
            visual_x = self.cursor.column - brk.column;
        }

        self.cursor.visual_pos.x = visual_x;
        self.cursor.visual_pos.y += sub_row;
    }

    /// Returns which visual sub-row `offset` falls on within the line starting at `line_start_offset`.
    pub fn sub_row_of(&self, offset: usize, line_start_offset: usize) -> CoordType {
        if self.word_wrap_column <= 0 {
            return 0;
        }
        let mut sub_row: CoordType = 0;
        for brk in LineWrapIterator::new(
            self.buffer,
            self.tab_size,
            self.word_wrap_column,
            line_start_offset,
        ) {
            if brk.offset > offset {
                break;
            }
            sub_row += 1;
        }
        sub_row
    }

    /// Find the byte offset of the start of a logical line by seeking backward.
    pub fn find_line_start_offset(&self, offset_hint: usize, y: CoordType) -> usize {
        let mut off = offset_hint;
        let mut line = y;

        loop {
            let chunk = self.buffer.read_backward(off);
            if chunk.is_empty() {
                break;
            }

            let (delta, l) = simd::lines_bwd(chunk, chunk.len(), line, y);
            off -= chunk.len() - delta;
            line = l;
            if delta > 0 {
                break;
            }
        }

        off
    }

    /// Seeks the cursor to the start of logical line `y`.
    ///
    /// Handles both forward and backward seeking, counting visual rows
    /// for proper `visual_pos.y` tracking when word wrap is enabled.
    pub fn goto_line_start(&mut self, y: CoordType) -> Cursor {
        let original = self.cursor;
        let mut seek_to_line_start = true;

        let forward = y > self.cursor.logical_pos.y;
        let mut visual_y_delta: CoordType = 0;

        if forward {
            while y > self.cursor.logical_pos.y {
                if self.word_wrap_column > 0 {
                    visual_y_delta += self.count_visual_rows_for_line(
                        self.find_line_start_offset(self.cursor.offset, self.cursor.logical_pos.y),
                    );
                } else {
                    visual_y_delta += 1;
                }

                let chunk = self.buffer.read_forward(self.cursor.offset);
                if chunk.is_empty() {
                    break;
                }

                let (delta, line) = simd::lines_fwd(
                    chunk,
                    0,
                    self.cursor.logical_pos.y,
                    self.cursor.logical_pos.y + 1,
                );
                self.cursor.offset += delta;
                self.cursor.logical_pos.y = line;
            }

            seek_to_line_start =
                self.cursor.offset == self.buffer.len() && self.cursor.offset != original.offset;
        }

        if seek_to_line_start {
            loop {
                let chunk = self.buffer.read_backward(self.cursor.offset);
                if chunk.is_empty() {
                    break;
                }

                let (delta, line) =
                    simd::lines_bwd(chunk, chunk.len(), self.cursor.logical_pos.y, y);
                let lines_crossed = self.cursor.logical_pos.y - line;
                self.cursor.offset -= chunk.len() - delta;
                self.cursor.logical_pos.y = line;

                if !forward && self.word_wrap_column > 0 && lines_crossed > 0 {
                    let mut off = self.cursor.offset;
                    let mut ly = self.cursor.logical_pos.y;
                    for _ in 0..lines_crossed {
                        let chunk_fwd = self.buffer.read_forward(off);
                        if chunk_fwd.is_empty() {
                            break;
                        }
                        let (d, l) = simd::lines_fwd(chunk_fwd, 0, ly, ly + 1);
                        if self.word_wrap_column > 0 {
                            visual_y_delta += self.count_visual_rows_for_line(off);
                        } else {
                            visual_y_delta += 1;
                        }
                        off += d;
                        ly = l;
                    }
                }

                if delta > 0 {
                    break;
                }
            }
        }

        if self.cursor.offset == original.offset {
            return self.cursor;
        }

        self.cursor.logical_pos.x = 0;
        self.cursor.column = 0;

        if self.word_wrap_column <= 0 {
            self.cursor.visual_pos = Point { x: 0, y: self.cursor.logical_pos.y };
        } else if forward {
            let cursor_line_start =
                self.find_line_start_offset(original.offset, original.logical_pos.y);
            let mut cursor_sub_row: CoordType = 0;
            for brk in LineWrapIterator::new(
                self.buffer,
                self.tab_size,
                self.word_wrap_column,
                cursor_line_start,
            ) {
                if brk.offset > original.offset {
                    break;
                }
                cursor_sub_row += 1;
            }
            let cursor_line_visual_y = original.visual_pos.y - cursor_sub_row;
            self.cursor.visual_pos = Point { x: 0, y: cursor_line_visual_y + visual_y_delta };
        } else {
            let cursor_line_start =
                self.find_line_start_offset(original.offset, original.logical_pos.y);
            let mut cursor_sub_row: CoordType = 0;
            for brk in LineWrapIterator::new(
                self.buffer,
                self.tab_size,
                self.word_wrap_column,
                cursor_line_start,
            ) {
                if brk.offset > original.offset {
                    break;
                }
                cursor_sub_row += 1;
            }
            let cursor_line_visual_y = original.visual_pos.y - cursor_sub_row;
            self.cursor.visual_pos = Point { x: 0, y: cursor_line_visual_y - visual_y_delta };
        }

        self.cursor
    }

    // ---- Full cursor movement ----

    /// Moves the cursor to the given byte offset, potentially crossing lines.
    pub fn cursor_move_to_offset(&mut self, offset: usize) -> Cursor {
        if offset == self.cursor.offset {
            return self.cursor;
        }

        if self.word_wrap_column <= 0 && offset.saturating_sub(self.cursor.offset) > 1024 {
            loop {
                let prev_offset = self.cursor.offset;
                self.goto_line_start(self.cursor.logical_pos.y + 1);
                if self.cursor.offset > offset || self.cursor.offset <= prev_offset {
                    break;
                }
            }
        }

        while offset < self.cursor.offset {
            self.goto_line_start(self.cursor.logical_pos.y - 1);
        }

        let line_start_offset = if self.cursor.logical_pos.x == 0 {
            self.cursor.offset
        } else {
            self.find_line_start_offset(self.cursor.offset, self.cursor.logical_pos.y)
        };
        self.goto_offset_on_line(offset);
        self.compute_visual_pos(line_start_offset);
        self.cursor
    }

    /// Moves the cursor to the given logical position.
    pub fn cursor_move_to_logical(&mut self, pos: Point) -> Cursor {
        let pos = Point { x: pos.x.max(0), y: pos.y.max(0) };

        if pos == self.cursor.logical_pos {
            return self.cursor;
        }

        if pos.y != self.cursor.logical_pos.y || pos.x < self.cursor.logical_pos.x {
            self.goto_line_start(pos.y);
        }

        let line_start_offset = if self.cursor.logical_pos.x == 0 {
            self.cursor.offset
        } else {
            self.find_line_start_offset(self.cursor.offset, self.cursor.logical_pos.y)
        };
        self.goto_pos_on_line(pos.x);
        self.compute_visual_pos(line_start_offset);
        self.cursor
    }

    /// Moves the cursor to the given visual position.
    pub fn cursor_move_to_visual(&mut self, pos: Point) -> Cursor {
        let pos = Point { x: pos.x.max(0), y: pos.y.max(0) };

        if pos == self.cursor.visual_pos {
            return self.cursor;
        }

        if self.word_wrap_column <= 0 {
            if pos.y != self.cursor.visual_pos.y || pos.x < self.cursor.visual_pos.x {
                self.goto_line_start(pos.y);
            }
            self.goto_column_on_line(pos.x);
            self.cursor.visual_pos = Point { x: self.cursor.column, y: self.cursor.logical_pos.y };
            self.cursor
        } else {
            // First, seek backward if needed.
            while pos.y < self.cursor.visual_pos.y {
                self.goto_line_start(self.cursor.logical_pos.y - 1);
            }
            if pos.y == self.cursor.visual_pos.y && pos.x < self.cursor.visual_pos.x {
                self.goto_line_start(self.cursor.logical_pos.y);
            }

            // Now seek forward through visual rows.
            while self.cursor.visual_pos.y < pos.y {
                let line_start = if self.cursor.logical_pos.x == 0 {
                    self.cursor.offset
                } else {
                    self.find_line_start_offset(self.cursor.offset, self.cursor.logical_pos.y)
                };
                let total_rows = self.count_visual_rows_for_line(line_start);
                let line_visual_start =
                    self.cursor.visual_pos.y - self.sub_row_of(self.cursor.offset, line_start);
                let rows_remaining_in_line =
                    total_rows - (self.cursor.visual_pos.y - line_visual_start);

                if self.cursor.visual_pos.y + rows_remaining_in_line <= pos.y {
                    self.goto_line_start(self.cursor.logical_pos.y + 1);
                } else {
                    let target_sub_row = pos.y - line_visual_start;
                    let mut sub_row: CoordType = 0;
                    let mut visual_line_start_column: CoordType = 0;

                    for brk in LineWrapIterator::new(
                        self.buffer,
                        self.tab_size,
                        self.word_wrap_column,
                        line_start,
                    ) {
                        sub_row += 1;
                        if sub_row > target_sub_row {
                            break;
                        }
                        visual_line_start_column = brk.column;
                        self.cursor.offset = brk.offset;
                        self.cursor.logical_pos.x = brk.pos;
                        self.cursor.column = brk.column;
                    }

                    self.goto_column_on_line(visual_line_start_column + pos.x);
                    self.cursor.visual_pos =
                        Point { x: self.cursor.column - visual_line_start_column, y: pos.y };
                    return self.cursor;
                }
            }

            // We're on the right visual row. Position within it.
            let line_start = if self.cursor.logical_pos.x == 0 {
                self.cursor.offset
            } else {
                self.find_line_start_offset(self.cursor.offset, self.cursor.logical_pos.y)
            };
            let mut visual_line_start_column: CoordType = 0;
            let sub_row = self.sub_row_of(self.cursor.offset, line_start);
            let line_visual_start = self.cursor.visual_pos.y - sub_row;

            let target_sub_row = pos.y - line_visual_start;
            if target_sub_row > 0 {
                let mut current_sub_row: CoordType = 0;
                for brk in LineWrapIterator::new(
                    self.buffer,
                    self.tab_size,
                    self.word_wrap_column,
                    line_start,
                ) {
                    current_sub_row += 1;
                    if current_sub_row > target_sub_row {
                        break;
                    }
                    visual_line_start_column = brk.column;
                    self.cursor.offset = brk.offset;
                    self.cursor.logical_pos.x = brk.pos;
                    self.cursor.column = brk.column;
                }
            }

            self.goto_column_on_line(visual_line_start_column + pos.x);
            self.compute_visual_pos(line_start);
            if self.cursor.visual_pos.y > pos.y {
                // Clamping: we went past the end of this visual sub-row.
                let target_sub_row = pos.y - line_visual_start;
                let mut sub_row: CoordType = 0;
                let mut next_brk_column = CoordType::MAX;
                for brk in LineWrapIterator::new(
                    self.buffer,
                    self.tab_size,
                    self.word_wrap_column,
                    line_start,
                ) {
                    sub_row += 1;
                    if sub_row > target_sub_row {
                        next_brk_column = brk.column;
                        break;
                    }
                }
                self.cursor = Cursor {
                    offset: line_start,
                    logical_pos: Point { x: 0, y: self.cursor.logical_pos.y },
                    visual_pos: Point { x: 0, y: line_visual_start },
                    column: 0,
                };
                self.goto_column_on_line(next_brk_column);
                self.cursor.visual_pos =
                    Point { x: self.cursor.column - visual_line_start_column, y: pos.y };
            }

            self.cursor
        }
    }

    /// Moves the cursor by a delta number of grapheme clusters, crossing line boundaries.
    pub fn cursor_move_delta_grapheme(&mut self, mut delta: CoordType) -> Cursor {
        if delta == 0 {
            return self.cursor;
        }

        let sign = if delta > 0 { 1 } else { -1 };
        let start_x = if delta > 0 { 0 } else { CoordType::MAX };
        let text_length = self.buffer.len();

        loop {
            let target_x = self.cursor.logical_pos.x + delta;

            self.cursor_move_to_logical(Point { x: target_x, y: self.cursor.logical_pos.y });

            delta = target_x - self.cursor.logical_pos.x;
            if delta.signum() != sign
                || (delta < 0 && self.cursor.offset == 0)
                || (delta > 0 && self.cursor.offset >= text_length)
            {
                break;
            }

            self.cursor_move_to_logical(Point { x: start_x, y: self.cursor.logical_pos.y + sign });

            delta -= sign;
            if delta.signum() != sign
                || self.cursor.offset == 0
                || self.cursor.offset >= text_length
            {
                break;
            }
        }

        self.cursor
    }

    /// Moves the cursor by a delta number of words, crossing line boundaries.
    pub fn cursor_move_delta_word(&mut self, mut delta: CoordType) -> Cursor {
        if delta == 0 {
            return self.cursor;
        }

        let sign = if delta > 0 { 1 } else { -1 };
        let mut offset = self.cursor.offset;

        while delta != 0 {
            if delta < 0 {
                offset = word_backward(self.buffer, offset);
            } else {
                offset = word_forward(self.buffer, offset);
            }
            delta -= sign;
        }

        self.cursor_move_to_offset(offset)
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
            visual_pos: Point { x: 0, y: 0 },
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

// ---- Word navigation ----

#[derive(Clone, Copy, PartialEq, Eq)]
enum CharClass {
    Whitespace,
    Newline,
    Separator,
    Word,
}

const fn construct_classifier(separators: &[u8]) -> [CharClass; 256] {
    let mut classifier = [CharClass::Word; 256];

    classifier[b' ' as usize] = CharClass::Whitespace;
    classifier[b'\t' as usize] = CharClass::Whitespace;
    classifier[b'\n' as usize] = CharClass::Newline;
    classifier[b'\r' as usize] = CharClass::Newline;

    let mut i = 0;
    let len = separators.len();
    while i < len {
        let ch = separators[i];
        assert!(ch < 128, "Only ASCII separators are supported.");
        classifier[ch as usize] = CharClass::Separator;
        i += 1;
    }

    classifier
}

const WORD_CLASSIFIER: [CharClass; 256] =
    construct_classifier(br#"`~!@#$%^&*()-=+[{]}\|;:'",.<>/?"#);

/// Finds the next word boundary given a document cursor offset.
/// Returns the offset of the next word boundary.
pub fn word_forward(doc: &dyn ReadableDocument, offset: usize) -> usize {
    word_navigation(WordForward { doc, offset, chunk: &[], chunk_off: 0 })
}

/// The backward version of `word_forward`.
pub fn word_backward(doc: &dyn ReadableDocument, offset: usize) -> usize {
    word_navigation(WordBackward { doc, offset, chunk: &[], chunk_off: 0 })
}

/// Word navigation implementation. Matches the behavior of VS Code.
fn word_navigation<T: WordNavigation>(mut nav: T) -> usize {
    // First, fill `self.chunk` with at least 1 grapheme.
    nav.read();

    // Skip one newline, if any.
    nav.skip_newline();

    // Skip any whitespace.
    nav.skip_class(CharClass::Whitespace);

    // Skip one word or separator and take note of the class.
    let class = nav.peek(CharClass::Whitespace);
    if matches!(class, CharClass::Separator | CharClass::Word) {
        nav.next();

        let off = nav.offset();

        // Continue skipping the same class.
        nav.skip_class(class);

        // If the class was a separator and we only moved one character,
        // continue skipping characters of the word class.
        if off == nav.offset() && class == CharClass::Separator {
            nav.skip_class(CharClass::Word);
        }
    }

    nav.offset()
}

trait WordNavigation {
    fn read(&mut self);
    fn skip_newline(&mut self);
    fn skip_class(&mut self, class: CharClass);
    fn peek(&self, default: CharClass) -> CharClass;
    fn next(&mut self);
    fn offset(&self) -> usize;
}

struct WordForward<'a> {
    doc: &'a dyn ReadableDocument,
    offset: usize,
    chunk: &'a [u8],
    chunk_off: usize,
}

impl WordNavigation for WordForward<'_> {
    fn read(&mut self) {
        self.chunk = self.doc.read_forward(self.offset);
        self.chunk_off = 0;
    }

    fn skip_newline(&mut self) {
        self.chunk_off += match self.chunk.get(self.chunk_off) {
            Some(&b'\n') => 1,
            Some(&b'\r') if self.chunk.get(self.chunk_off + 1) == Some(&b'\n') => 2,
            _ => 0,
        }
    }

    fn skip_class(&mut self, class: CharClass) {
        while !self.chunk.is_empty() {
            while self.chunk_off < self.chunk.len() {
                if WORD_CLASSIFIER[self.chunk[self.chunk_off] as usize] != class {
                    return;
                }
                self.chunk_off += 1;
            }

            self.offset += self.chunk.len();
            self.chunk = self.doc.read_forward(self.offset);
            self.chunk_off = 0;
        }
    }

    fn peek(&self, default: CharClass) -> CharClass {
        if self.chunk_off < self.chunk.len() {
            WORD_CLASSIFIER[self.chunk[self.chunk_off] as usize]
        } else {
            default
        }
    }

    fn next(&mut self) {
        self.chunk_off += 1;
    }

    fn offset(&self) -> usize {
        self.offset + self.chunk_off
    }
}

struct WordBackward<'a> {
    doc: &'a dyn ReadableDocument,
    offset: usize,
    chunk: &'a [u8],
    chunk_off: usize,
}

impl WordNavigation for WordBackward<'_> {
    fn read(&mut self) {
        self.chunk = self.doc.read_backward(self.offset);
        self.chunk_off = self.chunk.len();
    }

    fn skip_newline(&mut self) {
        if self.chunk_off > 0 && self.chunk[self.chunk_off - 1] == b'\n' {
            self.chunk_off -= 1;
        }
        if self.chunk_off > 0 && self.chunk[self.chunk_off - 1] == b'\r' {
            self.chunk_off -= 1;
        }
    }

    fn skip_class(&mut self, class: CharClass) {
        while !self.chunk.is_empty() {
            while self.chunk_off > 0 {
                if WORD_CLASSIFIER[self.chunk[self.chunk_off - 1] as usize] != class {
                    return;
                }
                self.chunk_off -= 1;
            }

            self.offset -= self.chunk.len();
            self.chunk = self.doc.read_backward(self.offset);
            self.chunk_off = self.chunk.len();
        }
    }

    fn peek(&self, default: CharClass) -> CharClass {
        if self.chunk_off > 0 {
            WORD_CLASSIFIER[self.chunk[self.chunk_off - 1] as usize]
        } else {
            default
        }
    }

    fn next(&mut self) {
        self.chunk_off -= 1;
    }

    fn offset(&self) -> usize {
        self.offset - self.chunk.len() + self.chunk_off
    }
}

/// Returns the offset range of the "word" at the given offset.
/// Does not cross newlines. Works similar to VS Code.
pub fn word_select(doc: &dyn ReadableDocument, offset: usize) -> Range<usize> {
    let mut beg = offset;
    let mut end = offset;
    let mut class = CharClass::Newline;

    let mut chunk = doc.read_forward(end);
    if !chunk.is_empty() {
        class = WORD_CLASSIFIER[chunk[0] as usize];

        let mut chunk_off = 0;

        if class != CharClass::Newline {
            loop {
                chunk_off += 1;
                end += 1;

                if chunk_off >= chunk.len() {
                    chunk = doc.read_forward(end);
                    chunk_off = 0;
                    if chunk.is_empty() {
                        break;
                    }
                }

                if WORD_CLASSIFIER[chunk[chunk_off] as usize] != class {
                    break;
                }
            }
        }
    }

    let mut chunk = doc.read_backward(beg);
    if !chunk.is_empty() {
        let mut chunk_off = chunk.len();

        if class == CharClass::Newline {
            class = WORD_CLASSIFIER[chunk[chunk_off - 1] as usize];
        }

        if class != CharClass::Newline {
            loop {
                if WORD_CLASSIFIER[chunk[chunk_off - 1] as usize] != class {
                    break;
                }

                chunk_off -= 1;
                beg -= 1;

                if chunk_off == 0 {
                    chunk = doc.read_backward(beg);
                    chunk_off = chunk.len();
                    if chunk.is_empty() {
                        break;
                    }
                }
            }
        }
    }

    beg..end
}

#[cfg(test)]
mod test {
    use super::*;

    struct ChunkedDoc<'a>(&'a [&'a [u8]]);

    impl ReadableDocument for ChunkedDoc<'_> {
        fn len(&self) -> usize {
            self.0.iter().map(|c| c.len()).sum()
        }

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
        assert_eq!(
            cursor,
            Cursor {
                offset: 3,
                logical_pos: Point { x: 3, y: 0 },
                column: 3,
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_measure_forward_clipped_wide_char() {
        let cursor = MeasurementConfig::new(&"a😶‍🌫️b".as_bytes()).goto_column(2);
        assert_eq!(
            cursor,
            Cursor {
                offset: 1,
                logical_pos: Point { x: 1, y: 0 },
                column: 1,
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_measure_forward_tabs() {
        let text = "a\tb\tc".as_bytes();
        let cursor = MeasurementConfig::new(&text).with_tab_size(4).goto_column(4);
        assert_eq!(
            cursor,
            Cursor {
                offset: 2,
                logical_pos: Point { x: 2, y: 0 },
                column: 4,
                ..Default::default()
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
        let cursor = MeasurementConfig::new(&doc).goto_column(5 + 2 + 3);
        assert_eq!(cursor.offset, 5 + 8 + 3);
        assert_eq!(cursor.logical_pos.x, 5 + 1 + 3);
    }

    #[test]
    fn test_stops_at_newline() {
        // Should stop at the LF and not cross it.
        let text = "abc\ndef".as_bytes();
        let cursor = MeasurementConfig::new(&text).goto_column(CoordType::MAX);
        assert_eq!(
            cursor,
            Cursor {
                offset: 3,
                logical_pos: Point { x: 3, y: 0 },
                column: 3,
                ..Default::default()
            }
        );

        // goto_offset past newline should also stop at the newline.
        let cursor = MeasurementConfig::new(&text).goto_offset(5);
        assert_eq!(
            cursor,
            Cursor {
                offset: 3,
                logical_pos: Point { x: 3, y: 0 },
                column: 3,
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_crlf_stops() {
        let text = "ab\r\ncd".as_bytes();
        let cursor = MeasurementConfig::new(&text).goto_column(CoordType::MAX);
        assert_eq!(
            cursor,
            Cursor {
                offset: 2,
                logical_pos: Point { x: 2, y: 0 },
                column: 2,
                ..Default::default()
            }
        );
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

    #[test]
    fn test_word_navigation() {
        assert_eq!(word_forward(&"Hello World".as_bytes(), 0), 5);
        assert_eq!(word_forward(&"Hello,World".as_bytes(), 0), 5);
        assert_eq!(word_forward(&"   Hello".as_bytes(), 0), 8);
        assert_eq!(word_forward(&"\n\nHello".as_bytes(), 0), 1);

        assert_eq!(word_backward(&"Hello World".as_bytes(), 11), 6);
        assert_eq!(word_backward(&"Hello,World".as_bytes(), 10), 6);
        assert_eq!(word_backward(&"Hello   ".as_bytes(), 7), 0);
        assert_eq!(word_backward(&"Hello\n\n".as_bytes(), 7), 6);
    }
}
