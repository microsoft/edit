// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::hint::cold_path;

use super::Utf8Chars;
use super::tables::*;
use crate::document::ReadableDocument;
use crate::helpers::{CoordType, Point};
use crate::simd::{memchr2, memrchr2};

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
    /// When `measure_forward` hits the `word_wrap_column`, the question is:
    /// Was there a wrap opportunity on this line? Because if there wasn't,
    /// a hard-wrap is required; otherwise, the word that is being laid-out is
    /// moved to the next line. This boolean carries this state between calls.
    pub wrap_opp: bool,
}

/// Your entrypoint to navigating inside a [`ReadableDocument`].
#[derive(Clone)]
pub struct MeasurementConfig<'doc> {
    buffer: &'doc dyn ReadableDocument,
    tab_size: CoordType,
    word_wrap_column: CoordType,
    cursor: Cursor,
}

impl<'doc> MeasurementConfig<'doc> {
    /// Creates a new [`MeasurementConfig`] for the given document.
    pub fn new(buffer: &'doc dyn ReadableDocument) -> Self {
        Self { buffer, tab_size: 8, word_wrap_column: 0, cursor: Default::default() }
    }

    /// Sets the tab size.
    ///
    /// Defaults to 8, because that's what a tab in terminals evaluates to.
    pub fn with_tab_size(mut self, tab_size: CoordType) -> Self {
        self.tab_size = tab_size.max(1);
        self
    }

    /// You want word wrap? Set it here!
    ///
    /// Defaults to 0, which means no word wrap.
    pub fn with_word_wrap_column(mut self, word_wrap_column: CoordType) -> Self {
        self.word_wrap_column = word_wrap_column;
        self
    }

    /// Sets the initial cursor to the given position.
    ///
    /// WARNING: While the code doesn't panic if the cursor is invalid,
    /// the results will obviously be complete garbage.
    pub fn with_cursor(mut self, cursor: Cursor) -> Self {
        self.cursor = cursor;
        self
    }

    /// Navigates **forward** to the given absolute offset.
    ///
    /// # Returns
    ///
    /// The cursor position after the navigation.
    pub fn goto_offset(&mut self, offset: usize) -> Cursor {
        self.cursor = Self::measure_forward(
            self.tab_size,
            self.word_wrap_column,
            offset,
            Point::MAX,
            Point::MAX,
            self.cursor,
            self.buffer,
        );
        self.cursor
    }

    /// Navigates **forward** to the given logical position.
    ///
    /// Logical positions are in lines and grapheme clusters.
    ///
    /// # Returns
    ///
    /// The cursor position after the navigation.
    pub fn goto_logical(&mut self, logical_target: Point) -> Cursor {
        self.cursor = Self::measure_forward(
            self.tab_size,
            self.word_wrap_column,
            usize::MAX,
            logical_target,
            Point::MAX,
            self.cursor,
            self.buffer,
        );
        self.cursor
    }

    /// Navigates **forward** to the given visual position.
    ///
    /// Visual positions are in laid out rows and columns.
    ///
    /// # Returns
    ///
    /// The cursor position after the navigation.
    pub fn goto_visual(&mut self, visual_target: Point) -> Cursor {
        self.cursor = Self::measure_forward(
            self.tab_size,
            self.word_wrap_column,
            usize::MAX,
            Point::MAX,
            visual_target,
            self.cursor,
            self.buffer,
        );
        self.cursor
    }

    /// Returns the current cursor position.
    pub fn cursor(&self) -> Cursor {
        self.cursor
    }

    // NOTE that going to a visual target can result in ambiguous results,
    // where going to an identical logical target will yield a different result.
    //
    // Imagine if you have a `word_wrap_column` of 6 and there's "Hello World" on the line:
    // `goto_logical` will return a `visual_pos` of {0,1}, while `goto_visual` returns {6,0}.
    // This is because from a logical POV, if the wrap location equals the wrap column,
    // the wrap exists on both lines and it'll default to wrapping. `goto_visual` however will always
    // try to return a Y position that matches the requested position, so that Home/End works properly.
    fn measure_forward(
        tab_size: CoordType,
        word_wrap_column: CoordType,
        offset_target: usize,
        logical_target: Point,
        visual_target: Point,
        cursor: Cursor,
        buffer: &dyn ReadableDocument,
    ) -> Cursor {
        if cursor.offset >= offset_target
            || cursor.logical_pos >= logical_target
            || cursor.visual_pos >= visual_target
        {
            return cursor;
        }

        let mut offset = cursor.offset;
        let mut logical_pos_x = cursor.logical_pos.x;
        let mut logical_pos_y = cursor.logical_pos.y;
        let mut visual_pos_x = cursor.visual_pos.x;
        let mut visual_pos_y = cursor.visual_pos.y;
        let mut column = cursor.column;

        let mut logical_target_x = Self::calc_target_x(logical_target, logical_pos_y);
        let mut visual_target_x = Self::calc_target_x(visual_target, visual_pos_y);

        // wrap_opp = Wrap Opportunity
        // These store the position and column of the last wrap opportunity. If `word_wrap_column` is
        // zero (word wrap disabled), all grapheme clusters are a wrap opportunity, because none are.
        let mut wrap_opp = cursor.wrap_opp;
        let mut wrap_opp_offset = offset;
        let mut wrap_opp_logical_pos_x = logical_pos_x;
        let mut wrap_opp_visual_pos_x = visual_pos_x;
        let mut wrap_opp_column = column;

        let mut chunk_iter = Utf8Chars::new(b"", 0);
        let mut chunk_range = offset..offset;
        let mut props_next_cluster = ucd_start_of_text_properties();

        loop {
            // Have we reached the target already? Stop.
            if offset >= offset_target
                || logical_pos_x >= logical_target_x
                || visual_pos_x >= visual_target_x
            {
                break;
            }

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
                    chunk_iter = Utf8Chars::new(buffer.read_forward(chunk_range.end), 0);
                    chunk_range = chunk_range.end..chunk_range.end + chunk_iter.len();
                }

                // Since this loop seeks ahead to the next cluster, and since `chunk_iter`
                // records the offset of the next character after the returned one, we need
                // to save the offset of the previous `chunk_iter` before calling `next()`.
                // Similar applies to the width.
                props_last_char = props_next_cluster;
                offset_next_cluster = chunk_range.start + chunk_iter.offset();
                width += ucd_grapheme_cluster_character_width(props_next_cluster) as CoordType;

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
                // SAFETY: `tab_size` is clamped to >= 1 in `with_tab_size`.
                // This assert ensures that Rust doesn't insert panicking null checks.
                unsafe { std::hint::assert_unchecked(tab_size >= 1) };
                width = tab_size - (column % tab_size);
            }

            // Hard wrap: Both the logical and visual position advance by one line.
            if props_last_char == ucd_linefeed_properties() {
                cold_path();

                wrap_opp = false;

                // Don't cross the newline if the target is on this line but we haven't reached it.
                // E.g. if the callers asks for column 100 on a 10 column line,
                // we'll return with the cursor set to column 10.
                if logical_pos_y >= logical_target.y || visual_pos_y >= visual_target.y {
                    break;
                }

                offset = offset_next_cluster;
                logical_pos_x = 0;
                logical_pos_y += 1;
                visual_pos_x = 0;
                visual_pos_y += 1;
                column = 0;

                logical_target_x = Self::calc_target_x(logical_target, logical_pos_y);
                visual_target_x = Self::calc_target_x(visual_target, visual_pos_y);
                continue;
            }

            // Avoid advancing past the visual target, because `width` can be greater than 1.
            if visual_pos_x + width > visual_target_x {
                break;
            }

            // Since this code above may need to revert to a previous `wrap_opp_*`,
            // it must be done before advancing / checking for `ucd_line_break_joins`.
            if word_wrap_column > 0 && visual_pos_x + width > word_wrap_column {
                if !wrap_opp {
                    // Otherwise, the lack of a wrap opportunity means that a single word
                    // is wider than the word wrap column. We need to force-break the word.
                    // This is similar to the above, but "bar" gets written at column 0.
                    wrap_opp_offset = offset;
                    wrap_opp_logical_pos_x = logical_pos_x;
                    wrap_opp_visual_pos_x = visual_pos_x;
                    wrap_opp_column = column;
                    visual_pos_x = 0;
                } else {
                    // If we had a wrap opportunity on this line, we can move all
                    // characters since then to the next line without stopping this loop:
                    //   +---------+      +---------+      +---------+
                    //   |      foo|  ->  |         |  ->  |         |
                    //   |         |      |foo      |      |foobar   |
                    //   +---------+      +---------+      +---------+
                    // We don't actually move "foo", but rather just change where "bar" goes.
                    // Since this function doesn't copy text, the end result is the same.
                    visual_pos_x -= wrap_opp_visual_pos_x;
                }

                wrap_opp = false;
                visual_pos_y += 1;
                visual_target_x = Self::calc_target_x(visual_target, visual_pos_y);

                if visual_pos_x == visual_target_x {
                    break;
                }

                // Imagine the word is "hello" and on the "o" we notice it wraps.
                // If the target however was the "e", then we must revert back to "h" and search for it.
                if visual_pos_x > visual_target_x {
                    cold_path();

                    offset = wrap_opp_offset;
                    logical_pos_x = wrap_opp_logical_pos_x;
                    visual_pos_x = 0;
                    column = wrap_opp_column;

                    chunk_iter.seek(chunk_iter.len());
                    chunk_range = offset..offset;
                    props_next_cluster = ucd_start_of_text_properties();
                    continue;
                }
            }

            offset = offset_next_cluster;
            logical_pos_x += 1;
            visual_pos_x += width;
            column += width;

            if word_wrap_column > 0
                && !ucd_line_break_joins(props_current_cluster, props_next_cluster)
            {
                wrap_opp = true;
                wrap_opp_offset = offset;
                wrap_opp_logical_pos_x = logical_pos_x;
                wrap_opp_visual_pos_x = visual_pos_x;
                wrap_opp_column = column;
            }
        }

        // If we're here, we hit our target. Now the only question is:
        // Is the word we're currently on so wide that it will be wrapped further down the document?
        if word_wrap_column > 0 {
            if !wrap_opp {
                // If the current laid-out line had no wrap opportunities, it means we had an input
                // such as "fooooooooooooooooooooo" at a `word_wrap_column` of e.g. 10. The word
                // didn't fit and the lack of a `wrap_opp` indicates we must force a hard wrap.
                // Thankfully, if we reach this point, that was already done by the code above.
            } else if wrap_opp_logical_pos_x != logical_pos_x && visual_pos_y <= visual_target.y {
                // Imagine the string "foo bar" with a word wrap column of 6. If I ask for the cursor at
                // `logical_pos={5,0}`, then the code above exited while reaching the target.
                // At this point, this function doesn't know yet that after the "b" there's "ar"
                // which causes a word wrap, and causes the final visual position to be {1,1}.
                // This code thus seeks ahead and checks if the current word will wrap or not.
                // Of course we only need to do this if the cursor isn't on a wrap opportunity already.

                // The loop below should not modify the target we already found.
                let mut visual_pos_x_lookahead = visual_pos_x;

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
                            chunk_iter = Utf8Chars::new(buffer.read_forward(chunk_range.end), 0);
                            chunk_range = chunk_range.end..chunk_range.end + chunk_iter.len();
                        }

                        // Since this loop seeks ahead to the next cluster, and since `chunk_iter`
                        // records the offset of the next character after the returned one, we need
                        // to save the offset of the previous `chunk_iter` before calling `next()`.
                        // Similar applies to the width.
                        props_last_char = props_next_cluster;
                        offset_next_cluster = chunk_range.start + chunk_iter.offset();
                        width +=
                            ucd_grapheme_cluster_character_width(props_next_cluster) as CoordType;

                        // The `Document::read_forward` interface promises us that it will not split
                        // grapheme clusters across chunks. Therefore, we can safely break here.
                        let ch = match chunk_iter.next() {
                            Some(ch) => ch,
                            None => break,
                        };

                        // Get the properties of the next cluster.
                        props_next_cluster = ucd_grapheme_cluster_lookup(ch);
                        state =
                            ucd_grapheme_cluster_joins(state, props_last_char, props_next_cluster);

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
                        // SAFETY: `tab_size` is clamped to >= 1 in `with_tab_size`.
                        // This assert ensures that Rust doesn't insert panicking null checks.
                        unsafe { std::hint::assert_unchecked(tab_size >= 1) };
                        width = tab_size - (column % tab_size);
                    }

                    // Hard wrap: Both the logical and visual position advance by one line.
                    if props_last_char == ucd_linefeed_properties() {
                        break;
                    }

                    visual_pos_x_lookahead += width;

                    if visual_pos_x_lookahead > word_wrap_column {
                        visual_pos_x -= wrap_opp_visual_pos_x;
                        visual_pos_y += 1;
                        break;
                    } else if !ucd_line_break_joins(props_current_cluster, props_next_cluster) {
                        break;
                    }
                }
            }

            if visual_pos_y > visual_target.y {
                // Imagine the string "foo bar" with a word wrap column of 6. If I ask for the cursor at
                // `visual_pos={100,0}`, the code above exited early after wrapping without reaching the target.
                // Since I asked for the last character on the first line, we must wrap back up the last wrap
                offset = wrap_opp_offset;
                logical_pos_x = wrap_opp_logical_pos_x;
                visual_pos_x = wrap_opp_visual_pos_x;
                visual_pos_y = visual_target.y;
                column = wrap_opp_column;
                wrap_opp = true;
            }
        }

        Cursor {
            offset,
            logical_pos: Point { x: logical_pos_x, y: logical_pos_y },
            visual_pos: Point { x: visual_pos_x, y: visual_pos_y },
            column,
            wrap_opp,
        }
    }

    #[inline]
    fn calc_target_x(target: Point, pos_y: CoordType) -> CoordType {
        match pos_y.cmp(&target.y) {
            std::cmp::Ordering::Less => CoordType::MAX,
            std::cmp::Ordering::Equal => target.x,
            std::cmp::Ordering::Greater => 0,
        }
    }
}

/// Seeks forward to the given line start.
///
/// If given a piece of `text`, and assuming you're currently at `offset` which
/// is on the logical line `line`, this will seek forward until the logical line
/// `line_stop` is reached. For instance, if `line` is 0 and `line_stop` is 2,
/// it'll seek forward past 2 line feeds.
///
/// This function always stops exactly past a line feed
/// and thus returns a position at the start of a line.
///
/// # Warning
///
/// If the end of `text` is hit before reaching `line_stop`, the function
/// will return an offset of `text.len()`, not at the start of a line.
///
/// # Parameters
///
/// * `text`: The text to search in.
/// * `offset`: The offset to start searching from.
/// * `line`: The current line.
/// * `line_stop`: The line to stop at.
///
/// # Returns
///
/// A tuple consisting of:
/// * The new offset.
/// * The line number that was reached.
pub fn newlines_forward(
    text: &[u8],
    mut offset: usize,
    mut line: CoordType,
    line_stop: CoordType,
) -> (usize, CoordType) {
    // Leaving the cursor at the beginning of the current line when the limit
    // is 0 makes this function behave identical to ucd_newlines_backward.
    if line >= line_stop {
        return newlines_backward(text, offset, line, line_stop);
    }

    let len = text.len();
    offset = offset.min(len);

    loop {
        // TODO: This code could be optimized by replacing memchr with manual line counting.
        //
        // If `line_stop` is very far away, we could accumulate newline counts horizontally
        // in a AVX2 register (= 32 u8 slots). Then, every 256 bytes we compute the horizontal
        // sum via `_mm256_sad_epu8` yielding us the newline count in the last block.
        //
        // We could also just use `_mm256_sad_epu8` on each fetch as-is.
        offset = memchr2(b'\n', b'\n', text, offset);
        if offset >= len {
            break;
        }

        offset += 1;
        line += 1;
        if line >= line_stop {
            break;
        }
    }

    (offset, line)
}

/// Seeks backward to the given line start.
///
/// See [`newlines_forward`] for details.
/// This function does almost the same thing, but in reverse.
///
/// # Warning
///
/// In addition to the notes in [`newlines_forward`]:
///
/// No matter what parameters are given, [`newlines_backward`] only returns an
/// offset at the start of a line. Put differently, even if `line == line_stop`,
/// it'll seek backward to the line start.
pub fn newlines_backward(
    text: &[u8],
    mut offset: usize,
    mut line: CoordType,
    line_stop: CoordType,
) -> (usize, CoordType) {
    offset = offset.min(text.len());

    loop {
        offset = match memrchr2(b'\n', b'\n', text, offset) {
            Some(i) => i,
            None => return (0, line),
        };
        if line <= line_stop {
            // +1: Past the newline, at the start of the current line.
            return (offset + 1, line);
        }
        line -= 1;
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
    fn test_measure_forward_newline_start() {
        let cursor =
            MeasurementConfig::new(&"foo\nbar".as_bytes()).goto_visual(Point { x: 0, y: 1 });
        assert_eq!(
            cursor,
            Cursor {
                offset: 4,
                logical_pos: Point { x: 0, y: 1 },
                visual_pos: Point { x: 0, y: 1 },
                column: 0,
                wrap_opp: false,
            }
        );
    }

    #[test]
    fn test_measure_forward_clipped_wide_char() {
        let cursor = MeasurementConfig::new(&"a😶‍🌫️b".as_bytes()).goto_visual(Point { x: 2, y: 0 });
        assert_eq!(
            cursor,
            Cursor {
                offset: 1,
                logical_pos: Point { x: 1, y: 0 },
                visual_pos: Point { x: 1, y: 0 },
                column: 1,
                wrap_opp: false,
            }
        );
    }

    #[test]
    fn test_measure_forward_word_wrap() {
        //   |foo␣  |
        //   |bar␣  |
        //   |baz   |
        let text = "foo bar \nbaz".as_bytes();

        // Does hitting a logical target wrap the visual position along with the word?
        let mut cfg = MeasurementConfig::new(&text).with_word_wrap_column(6);
        let cursor = cfg.goto_logical(Point { x: 5, y: 0 });
        assert_eq!(
            cursor,
            Cursor {
                offset: 5,
                logical_pos: Point { x: 5, y: 0 },
                visual_pos: Point { x: 1, y: 1 },
                column: 5,
                wrap_opp: true,
            }
        );

        // Does hitting the visual target within a word reset the hit back to the end of the visual line?
        let mut cfg = MeasurementConfig::new(&text).with_word_wrap_column(6);
        let cursor = cfg.goto_visual(Point { x: CoordType::MAX, y: 0 });
        assert_eq!(
            cursor,
            Cursor {
                offset: 4,
                logical_pos: Point { x: 4, y: 0 },
                visual_pos: Point { x: 4, y: 0 },
                column: 4,
                wrap_opp: true,
            }
        );

        // Does hitting the same target but with a non-zero starting position result in the same outcome?
        let mut cfg = MeasurementConfig::new(&text).with_word_wrap_column(6).with_cursor(Cursor {
            offset: 1,
            logical_pos: Point { x: 1, y: 0 },
            visual_pos: Point { x: 1, y: 0 },
            column: 1,
            wrap_opp: false,
        });
        let cursor = cfg.goto_visual(Point { x: 5, y: 0 });
        assert_eq!(
            cursor,
            Cursor {
                offset: 4,
                logical_pos: Point { x: 4, y: 0 },
                visual_pos: Point { x: 4, y: 0 },
                column: 4,
                wrap_opp: true,
            }
        );

        let cursor = cfg.goto_visual(Point { x: 0, y: 1 });
        assert_eq!(
            cursor,
            Cursor {
                offset: 4,
                logical_pos: Point { x: 4, y: 0 },
                visual_pos: Point { x: 0, y: 1 },
                column: 4,
                wrap_opp: false,
            }
        );

        let cursor = cfg.goto_visual(Point { x: 5, y: 1 });
        assert_eq!(
            cursor,
            Cursor {
                offset: 8,
                logical_pos: Point { x: 8, y: 0 },
                visual_pos: Point { x: 4, y: 1 },
                column: 8,
                wrap_opp: false,
            }
        );

        let cursor = cfg.goto_visual(Point { x: 0, y: 2 });
        assert_eq!(
            cursor,
            Cursor {
                offset: 9,
                logical_pos: Point { x: 0, y: 1 },
                visual_pos: Point { x: 0, y: 2 },
                column: 0,
                wrap_opp: false,
            }
        );

        let cursor = cfg.goto_visual(Point { x: 5, y: 2 });
        assert_eq!(
            cursor,
            Cursor {
                offset: 12,
                logical_pos: Point { x: 3, y: 1 },
                visual_pos: Point { x: 3, y: 2 },
                column: 3,
                wrap_opp: false,
            }
        );
    }

    #[test]
    fn test_measure_forward_tabs() {
        let text = "a\tb\tc".as_bytes();
        let cursor =
            MeasurementConfig::new(&text).with_tab_size(4).goto_visual(Point { x: 4, y: 0 });
        assert_eq!(
            cursor,
            Cursor {
                offset: 2,
                logical_pos: Point { x: 2, y: 0 },
                visual_pos: Point { x: 4, y: 0 },
                column: 4,
                wrap_opp: false,
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
        let cursor = MeasurementConfig::new(&doc).goto_visual(Point { x: 5 + 2 + 3, y: 0 });
        assert_eq!(cursor.offset, 5 + 8 + 3);
        assert_eq!(cursor.logical_pos, Point { x: 5 + 1 + 3, y: 0 });
    }

    #[test]
    fn test_exact_wrap() {
        //   |foo_   |
        //   |bar.   |
        //   |abc    |
        let chunks = ["foo ".as_bytes(), "bar".as_bytes(), ".\n".as_bytes(), "abc".as_bytes()];
        let doc = ChunkedDoc(&chunks);
        let mut cfg = MeasurementConfig::new(&doc).with_word_wrap_column(7);
        let max = CoordType::MAX;

        let end0 = cfg.goto_visual(Point { x: 7, y: 0 });
        assert_eq!(
            end0,
            Cursor {
                offset: 4,
                logical_pos: Point { x: 4, y: 0 },
                visual_pos: Point { x: 4, y: 0 },
                column: 4,
                wrap_opp: true,
            }
        );

        let beg1 = cfg.goto_visual(Point { x: 0, y: 1 });
        assert_eq!(
            beg1,
            Cursor {
                offset: 4,
                logical_pos: Point { x: 4, y: 0 },
                visual_pos: Point { x: 0, y: 1 },
                column: 4,
                wrap_opp: false,
            }
        );

        let end1 = cfg.goto_visual(Point { x: max, y: 1 });
        assert_eq!(
            end1,
            Cursor {
                offset: 8,
                logical_pos: Point { x: 8, y: 0 },
                visual_pos: Point { x: 4, y: 1 },
                column: 8,
                wrap_opp: false,
            }
        );

        let beg2 = cfg.goto_visual(Point { x: 0, y: 2 });
        assert_eq!(
            beg2,
            Cursor {
                offset: 9,
                logical_pos: Point { x: 0, y: 1 },
                visual_pos: Point { x: 0, y: 2 },
                column: 0,
                wrap_opp: false,
            }
        );

        let end2 = cfg.goto_visual(Point { x: max, y: 2 });
        assert_eq!(
            end2,
            Cursor {
                offset: 12,
                logical_pos: Point { x: 3, y: 1 },
                visual_pos: Point { x: 3, y: 2 },
                column: 3,
                wrap_opp: false,
            }
        );
    }

    #[test]
    fn test_force_wrap() {
        // |//_     |
        // |aaaaaaaa|
        // |aaaa    |
        let bytes = "// aaaaaaaaaaaa".as_bytes();
        let mut cfg = MeasurementConfig::new(&bytes).with_word_wrap_column(8);
        let max = CoordType::MAX;

        // At the end of "// " there should be a wrap.
        let end0 = cfg.goto_visual(Point { x: max, y: 0 });
        assert_eq!(
            end0,
            Cursor {
                offset: 3,
                logical_pos: Point { x: 3, y: 0 },
                visual_pos: Point { x: 3, y: 0 },
                column: 3,
                wrap_opp: true,
            }
        );

        // Test if the ambiguous visual position at the wrap location doesn't change the offset.
        let beg0 = cfg.goto_visual(Point { x: 0, y: 1 });
        assert_eq!(
            beg0,
            Cursor {
                offset: 3,
                logical_pos: Point { x: 3, y: 0 },
                visual_pos: Point { x: 0, y: 1 },
                column: 3,
                wrap_opp: false,
            }
        );

        // Test if navigating inside the wrapped line doesn't cause further wrapping.
        //
        // This step of the test is important, as it ensures that the following force-wrap works,
        // even if 1 of the 8 "a"s was already processed.
        let beg0_off1 = cfg.goto_logical(Point { x: 4, y: 0 });
        assert_eq!(
            beg0_off1,
            Cursor {
                offset: 4,
                logical_pos: Point { x: 4, y: 0 },
                visual_pos: Point { x: 1, y: 1 },
                column: 4,
                wrap_opp: false,
            }
        );

        // Test if the force-wrap applies at the end of the first 8 "a"s.
        let end1 = cfg.goto_visual(Point { x: max, y: 1 });
        assert_eq!(
            end1,
            Cursor {
                offset: 11,
                logical_pos: Point { x: 11, y: 0 },
                visual_pos: Point { x: 8, y: 1 },
                column: 11,
                wrap_opp: true,
            }
        );

        // Test if the remaining 4 "a"s are properly laid-out.
        let end2 = cfg.goto_visual(Point { x: max, y: 2 });
        assert_eq!(
            end2,
            Cursor {
                offset: 15,
                logical_pos: Point { x: 15, y: 0 },
                visual_pos: Point { x: 4, y: 2 },
                column: 15,
                wrap_opp: false,
            }
        );
    }

    #[test]
    fn test_force_wrap_wide() {
        // These Yijing Hexagram Symbols form no word wrap opportunities.
        let text = "䷀䷁䷂䷃䷄䷅䷆䷇䷈䷉";
        let expected = ["䷀䷁", "䷂䷃", "䷄䷅", "䷆䷇", "䷈䷉"];
        let bytes = text.as_bytes();
        let mut cfg = MeasurementConfig::new(&bytes).with_word_wrap_column(5);

        for (y, &expected) in expected.iter().enumerate() {
            let y = y as CoordType;
            // In order for `goto_visual()` to hit column 0 after a word wrap,
            // it MUST be able to go back by 1 grapheme, which is what this tests.
            let beg = cfg.goto_visual(Point { x: 0, y });
            let end = cfg.goto_visual(Point { x: 5, y });
            let actual = &text[beg.offset..end.offset];
            assert_eq!(actual, expected);
        }
    }

    // Similar to the `test_force_wrap` test, but here we vertically descend
    // down the document without ever touching the first or last column.
    // I found that this finds curious bugs at times.
    #[test]
    fn test_force_wrap_column() {
        // |//_     |
        // |aaaaaaaa|
        // |aaaa    |
        let bytes = "// aaaaaaaaaaaa".as_bytes();
        let mut cfg = MeasurementConfig::new(&bytes).with_word_wrap_column(8);

        // At the end of "// " there should be a wrap.
        let end0 = cfg.goto_visual(Point { x: CoordType::MAX, y: 0 });
        assert_eq!(
            end0,
            Cursor {
                offset: 3,
                logical_pos: Point { x: 3, y: 0 },
                visual_pos: Point { x: 3, y: 0 },
                column: 3,
                wrap_opp: true,
            }
        );

        let mid1 = cfg.goto_visual(Point { x: end0.visual_pos.x, y: 1 });
        assert_eq!(
            mid1,
            Cursor {
                offset: 6,
                logical_pos: Point { x: 6, y: 0 },
                visual_pos: Point { x: 3, y: 1 },
                column: 6,
                wrap_opp: false,
            }
        );

        let mid2 = cfg.goto_visual(Point { x: end0.visual_pos.x, y: 2 });
        assert_eq!(
            mid2,
            Cursor {
                offset: 14,
                logical_pos: Point { x: 14, y: 0 },
                visual_pos: Point { x: 3, y: 2 },
                column: 14,
                wrap_opp: false,
            }
        );
    }

    #[test]
    fn test_any_wrap() {
        // |//_-----|
        // |------- |
        let bytes = "// ------------".as_bytes();
        let mut cfg = MeasurementConfig::new(&bytes).with_word_wrap_column(8);
        let max = CoordType::MAX;

        let end0 = cfg.goto_visual(Point { x: max, y: 0 });
        assert_eq!(
            end0,
            Cursor {
                offset: 8,
                logical_pos: Point { x: 8, y: 0 },
                visual_pos: Point { x: 8, y: 0 },
                column: 8,
                wrap_opp: true,
            }
        );

        let end1 = cfg.goto_visual(Point { x: max, y: 1 });
        assert_eq!(
            end1,
            Cursor {
                offset: 15,
                logical_pos: Point { x: 15, y: 0 },
                visual_pos: Point { x: 7, y: 1 },
                column: 15,
                wrap_opp: true,
            }
        );
    }

    #[test]
    fn test_any_wrap_wide() {
        // These Japanese characters form word wrap opportunity between each character.
        let text = "零一二三四五六七八九";
        let expected = ["零一", "二三", "四五", "六七", "八九"];
        let bytes = text.as_bytes();
        let mut cfg = MeasurementConfig::new(&bytes).with_word_wrap_column(5);

        for (y, &expected) in expected.iter().enumerate() {
            let y = y as CoordType;
            // In order for `goto_visual()` to hit column 0 after a word wrap,
            // it MUST be able to go back by 1 grapheme, which is what this tests.
            let beg = cfg.goto_visual(Point { x: 0, y });
            let end = cfg.goto_visual(Point { x: 5, y });
            let actual = &text[beg.offset..end.offset];
            assert_eq!(actual, expected);
        }
    }

    #[test]
    fn test_wrap_tab() {
        // |foo_    | <- 1 space
        // |____b   | <- 1 tab, 1 space
        let text = "foo \t b";
        let bytes = text.as_bytes();
        let mut cfg = MeasurementConfig::new(&bytes).with_word_wrap_column(8).with_tab_size(4);
        let max = CoordType::MAX;

        let end0 = cfg.goto_visual(Point { x: max, y: 0 });
        assert_eq!(
            end0,
            Cursor {
                offset: 4,
                logical_pos: Point { x: 4, y: 0 },
                visual_pos: Point { x: 4, y: 0 },
                column: 4,
                wrap_opp: true,
            },
        );

        let beg1 = cfg.goto_visual(Point { x: 0, y: 1 });
        assert_eq!(
            beg1,
            Cursor {
                offset: 4,
                logical_pos: Point { x: 4, y: 0 },
                visual_pos: Point { x: 0, y: 1 },
                column: 4,
                wrap_opp: false,
            },
        );

        let end1 = cfg.goto_visual(Point { x: max, y: 1 });
        assert_eq!(
            end1,
            Cursor {
                offset: 7,
                logical_pos: Point { x: 7, y: 0 },
                visual_pos: Point { x: 6, y: 1 },
                column: 10,
                wrap_opp: true,
            },
        );
    }

    #[test]
    fn test_crlf() {
        let text = "a\r\nbcd\r\ne".as_bytes();
        let cursor = MeasurementConfig::new(&text).goto_visual(Point { x: CoordType::MAX, y: 1 });
        assert_eq!(
            cursor,
            Cursor {
                offset: 6,
                logical_pos: Point { x: 3, y: 1 },
                visual_pos: Point { x: 3, y: 1 },
                column: 3,
                wrap_opp: false,
            }
        );
    }

    #[test]
    fn test_wrapped_cursor_can_seek_backward() {
        let bytes = "hello world".as_bytes();
        let mut cfg = MeasurementConfig::new(&bytes).with_word_wrap_column(10);

        // When the word wrap at column 10 hits, the cursor will be at the end of the word "world" (between l and d).
        // This tests if the algorithm is capable of going back to the start of the word and find the actual target.
        let cursor = cfg.goto_visual(Point { x: 2, y: 1 });
        assert_eq!(
            cursor,
            Cursor {
                offset: 8,
                logical_pos: Point { x: 8, y: 0 },
                visual_pos: Point { x: 2, y: 1 },
                column: 8,
                wrap_opp: false,
            }
        );
    }

    #[test]
    fn test_newlines_and_strip() {
        // Offset line 0: 0
        // Offset line 1: 6
        // Offset line 2: 13
        // Offset line 3: 18
        let text = "line1\nline2\r\nline3".as_bytes();

        assert_eq!(newlines_forward(text, 0, 0, 2), (13, 2));
        assert_eq!(newlines_forward(text, 0, 0, 0), (0, 0));
        assert_eq!(newlines_forward(text, 100, 2, 100), (18, 2));

        assert_eq!(newlines_backward(text, 18, 2, 1), (6, 1));
        assert_eq!(newlines_backward(text, 18, 2, 0), (0, 0));
        assert_eq!(newlines_backward(text, 100, 2, 1), (6, 1));
    }

    #[test]
    fn test_strip_newline() {
        assert_eq!(strip_newline(b"hello\n"), b"hello");
        assert_eq!(strip_newline(b"hello\r\n"), b"hello");
        assert_eq!(strip_newline(b"hello"), b"hello");
    }
}
