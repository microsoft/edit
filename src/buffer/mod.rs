// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! A text buffer for a text editor.
//!
//! Implements a Unicode-aware, layout-aware text buffer for terminals.
//! It's based on a gap buffer. It has no line cache and instead relies
//! on the performance of the ucd module for fast text navigation.
//!
//! ---
//!
//! If the project ever outgrows a basic gap buffer (e.g. to add time travel)
//! an ideal, alternative architecture would be a piece table with immutable trees.
//! The tree nodes can be allocated on the same arena allocator as the added chunks,
//! making lifetime management fairly easy. The algorithm is described here:
//! * <https://cdacamar.github.io/data%20structures/algorithms/benchmarking/text%20editors/c++/editor-data-structures/>
//! * <https://github.com/cdacamar/fredbuf>
//!
//! The downside is that text navigation & search takes a performance hit due to small chunks.
//! The solution to the former is to keep line caches, which further complicates the architecture.
//! There's no solution for the latter. However, there's a chance that the performance will still be sufficient.

mod gap_buffer;
mod navigation;

use std::borrow::Cow;
use std::cell::UnsafeCell;
use std::collections::LinkedList;
use std::fmt::Write as _;
use std::fs::File;
use std::io::{Read as _, Write as _};
use std::mem::{self, MaybeUninit};
use std::ops::Range;
use std::rc::Rc;
use std::str;

use gap_buffer::GapBuffer;

use crate::arena::{ArenaString, scratch_arena};
use crate::cell::SemiRefCell;
use crate::document::{ReadableDocument, WriteableDocument};
use crate::framebuffer::{Framebuffer, IndexedColor};
use crate::helpers::*;
use crate::oklab::oklab_blend;
use crate::simd::memchr2;
use crate::unicode::{self, Cursor, MeasurementConfig};
use crate::{apperr, icu};

/// The margin template is used for line numbers.
/// The max. line number we should ever expect is probably 64-bit,
/// and so this template fits 19 digits, followed by " │ ".
const MARGIN_TEMPLATE: &str = "                    │ ";
/// Just a bunch of whitespace you can use for turning tabs into spaces.
/// Happens to reuse MARGIN_TEMPLATE, because it has sufficient whitespace.
const TAB_WHITESPACE: &str = MARGIN_TEMPLATE;

/// Stores statistics about the whole document.
#[derive(Copy, Clone)]
pub struct TextBufferStatistics {
    logical_lines: CoordType,
    visual_lines: CoordType,
}

/// Stores the active text selection anchors.
///
/// The two points are not sorted. Instead, `beg` refers to where the selection
/// started being made and `end` refers to the currently being updated position.
#[derive(Copy, Clone)]
struct TextBufferSelection {
    beg: Point,
    end: Point,
}

/// In order to group actions into a single undo step,
/// we need to know the type of action that was performed.
/// This stores the action type.
#[derive(Copy, Clone, Eq, PartialEq)]
enum HistoryType {
    Other,
    Write,
    Delete,
}

/// An undo/redo entry.
struct HistoryEntry {
    /// [`TextBuffer::cursor`] position before the change was made.
    cursor_before: Point,
    /// [`TextBuffer::selection`] before the change was made.
    selection_before: Option<TextBufferSelection>,
    /// [`TextBuffer::stats`] before the change was made.
    stats_before: TextBufferStatistics,
    /// [`GapBuffer::generation`] before the change was made.
    generation_before: u32,
    /// Logical cursor position where the change took place.
    /// The position is at the start of the changed range.
    cursor: Point,
    /// Text that was deleted from the buffer.
    deleted: Vec<u8>,
    /// Text that was added to the buffer.
    added: Vec<u8>,
}

/// Caches an ICU search operation.
struct ActiveSearch {
    /// The search pattern.
    pattern: String,
    /// The search options.
    options: SearchOptions,
    /// The ICU `UText` object.
    text: icu::Text,
    /// The ICU `URegularExpression` object.
    regex: icu::Regex,
    /// [`GapBuffer::generation`] when the search was created.
    /// This is used to detect if we need to refresh the
    /// [`ActiveSearch::regex`] object.
    buffer_generation: u32,
    /// [`TextBuffer::selection_generation`] when the search was
    /// created. When the user manually selects text, we need to
    /// refresh the [`ActiveSearch::pattern`] with it.
    selection_generation: u32,
    /// Stores the text buffer offset in between searches.
    next_search_offset: usize,
    /// If we know there were no hits, we can skip searching.
    no_matches: bool,
}

/// Options for a search operation.
#[derive(Default, Clone, Copy, Eq, PartialEq)]
pub struct SearchOptions {
    /// If true, the search is case-sensitive.
    pub match_case: bool,
    /// If true, the search matches whole words.
    pub whole_word: bool,
    /// If true, the search uses regex.
    pub use_regex: bool,
}

/// Caches the start and length of the active edit line for a single edit.
/// This helps us avoid having to remeasure the buffer after an edit.
struct ActiveEditLineInfo {
    /// Points to the start of the currently being edited line.
    safe_start: Cursor,
    /// Number of visual rows of the line that starts
    /// at [`ActiveEditLineInfo::safe_start`].
    line_height_in_rows: CoordType,
    /// Byte distance from the start of the line at
    /// [`ActiveEditLineInfo::safe_start`] to the next line.
    distance_next_line_start: usize,
}

/// Char- or word-wise navigation? Your choice.
pub enum CursorMovement {
    Grapheme,
    Word,
}

/// The result of a call to [`TextBuffer::render()`].
pub struct RenderResult {
    /// The maximum visual X position we encountered during rendering.
    pub visual_pos_x_max: CoordType,
}

/// A [`TextBuffer`] with inner mutability.
pub type TextBufferCell = SemiRefCell<TextBuffer>;

/// A [`TextBuffer`] inside an [`Rc`].
///
/// We need this because the TUI system needs to borrow
/// the given text buffer(s) until after the layout process.
pub type RcTextBuffer = Rc<TextBufferCell>;

/// A text buffer for a text editor.
pub struct TextBuffer {
    buffer: GapBuffer,

    undo_stack: LinkedList<SemiRefCell<HistoryEntry>>,
    redo_stack: LinkedList<SemiRefCell<HistoryEntry>>,
    last_history_type: HistoryType,
    last_save_generation: u32,

    active_edit_line_info: Option<ActiveEditLineInfo>,
    active_edit_depth: i32,
    active_edit_off: usize,

    stats: TextBufferStatistics,
    cursor: Cursor,
    // When scrolling significant amounts of text away from the cursor,
    // rendering will naturally slow down proportionally to the distance.
    // To avoid this, we cache the cursor position for rendering.
    // Must be cleared on every edit or reflow.
    cursor_for_rendering: Option<Cursor>,
    selection: Option<TextBufferSelection>,
    selection_generation: u32,
    search: Option<UnsafeCell<ActiveSearch>>,

    width: CoordType,
    margin_width: CoordType,
    margin_enabled: bool,
    word_wrap_column: CoordType,
    word_wrap_enabled: bool,
    tab_size: CoordType,
    indent_with_tabs: bool,
    line_highlight_enabled: bool,
    ruler: CoordType,
    encoding: &'static str,
    newlines_are_crlf: bool,
    insert_final_newline: bool,
    overtype: bool,

    wants_cursor_visibility: bool,
}

impl TextBuffer {
    /// Creates a new text buffer inside an [`Rc`].
    /// See [`TextBuffer::new()`].
    pub fn new_rc(small: bool) -> apperr::Result<RcTextBuffer> {
        let buffer = Self::new(small)?;
        Ok(Rc::new(SemiRefCell::new(buffer)))
    }

    /// Creates a new text buffer. With `small` you can control
    /// if the buffer is optimized for <1MiB contents.
    pub fn new(small: bool) -> apperr::Result<Self> {
        Ok(Self {
            buffer: GapBuffer::new(small)?,

            undo_stack: LinkedList::new(),
            redo_stack: LinkedList::new(),
            last_history_type: HistoryType::Other,
            last_save_generation: 0,

            active_edit_line_info: None,
            active_edit_depth: 0,
            active_edit_off: 0,

            stats: TextBufferStatistics { logical_lines: 1, visual_lines: 1 },
            cursor: Default::default(),
            cursor_for_rendering: None,
            selection: None,
            selection_generation: 0,
            search: None,

            width: 0,
            margin_width: 0,
            margin_enabled: false,
            word_wrap_column: 0,
            word_wrap_enabled: false,
            tab_size: 4,
            indent_with_tabs: false,
            line_highlight_enabled: false,
            ruler: 0,
            encoding: "UTF-8",
            newlines_are_crlf: cfg!(windows), // Windows users want CRLF
            insert_final_newline: false,
            overtype: false,

            wants_cursor_visibility: false,
        })
    }

    /// Length of the document in bytes.
    pub fn text_length(&self) -> usize {
        self.buffer.len()
    }

    /// Number of logical lines in the document,
    /// that is, lines separated by newlines.
    pub fn logical_line_count(&self) -> CoordType {
        self.stats.logical_lines
    }

    /// Number of visual lines in the document,
    /// that is, the number of lines after layout.
    pub fn visual_line_count(&self) -> CoordType {
        self.stats.visual_lines
    }

    /// Does the buffer need to be saved?
    pub fn is_dirty(&self) -> bool {
        self.last_save_generation != self.buffer.generation()
    }

    /// The buffer generation changes on every edit.
    /// With this you can check if it has changed since
    /// the last time you called this function.
    pub fn generation(&self) -> u32 {
        self.buffer.generation()
    }

    /// Force the buffer to be dirty.
    pub fn mark_as_dirty(&mut self) {
        self.last_save_generation = self.buffer.generation().wrapping_sub(1);
    }

    fn mark_as_clean(&mut self) {
        self.last_save_generation = self.buffer.generation();
    }

    /// The encoding used during reading/writing. "UTF-8" is the default.
    pub fn encoding(&self) -> &'static str {
        self.encoding
    }

    /// Set the encoding used during reading/writing.
    pub fn set_encoding(&mut self, encoding: &'static str) {
        if self.encoding != encoding {
            self.encoding = encoding;
            self.mark_as_dirty();
        }
    }

    /// The newline type used in the document. LF or CRLF.
    pub fn is_crlf(&self) -> bool {
        self.newlines_are_crlf
    }

    /// Changes the newline type used in the document.
    ///
    /// NOTE: Cannot be undone.
    pub fn normalize_newlines(&mut self, crlf: bool) {
        let newline: &[u8] = if crlf { b"\r\n" } else { b"\n" };
        let mut off = 0;

        let mut cursor_offset = self.cursor.offset;
        let mut cursor_for_rendering_offset =
            self.cursor_for_rendering.map_or(cursor_offset, |c| c.offset);

        #[cfg(debug_assertions)]
        let mut adjusted_newlines = 0;

        'outer: loop {
            // Seek to the offset of the next line start.
            loop {
                let chunk = self.read_forward(off);
                if chunk.is_empty() {
                    break 'outer;
                }

                let (delta, line) = unicode::newlines_forward(chunk, 0, 0, 1);
                off += delta;
                if line == 1 {
                    break;
                }
            }

            // Get the preceding newline.
            let chunk = self.read_backward(off);
            let chunk_newline_len = if chunk.ends_with(b"\r\n") { 2 } else { 1 };
            let chunk_newline = &chunk[chunk.len() - chunk_newline_len..];

            if chunk_newline != newline {
                // If this newline is still before our cursor position, then it still has an effect on its offset.
                // Any newline adjustments past that cursor position are irrelevant.
                let delta = newline.len() as isize - chunk_newline_len as isize;
                if off <= cursor_offset {
                    cursor_offset = cursor_offset.saturating_add_signed(delta);
                    #[cfg(debug_assertions)]
                    {
                        adjusted_newlines += 1;
                    }
                }
                if off <= cursor_for_rendering_offset {
                    cursor_for_rendering_offset =
                        cursor_for_rendering_offset.saturating_add_signed(delta);
                }

                // Replace the newline.
                off -= chunk_newline_len;
                self.buffer.replace(off..off + chunk_newline_len, newline);
                off += newline.len();
            }
        }

        // If this fails, the cursor offset calculation above is wrong.
        #[cfg(debug_assertions)]
        debug_assert_eq!(adjusted_newlines, self.cursor.logical_pos.y);

        self.cursor.offset = cursor_offset;
        if let Some(cursor) = &mut self.cursor_for_rendering {
            cursor.offset = cursor_for_rendering_offset;
        }

        self.newlines_are_crlf = crlf;
    }

    /// If enabled, automatically insert a final newline
    /// when typing at the end of the file.
    pub fn set_insert_final_newline(&mut self, enabled: bool) {
        self.insert_final_newline = enabled;
    }

    /// Whether to insert or overtype text when writing.
    pub fn is_overtype(&self) -> bool {
        self.overtype
    }

    /// Set the overtype mode.
    pub fn set_overtype(&mut self, overtype: bool) {
        self.overtype = overtype;
    }

    /// Gets the logical cursor position, that is,
    /// the position in lines and graphemes per line.
    pub fn cursor_logical_pos(&self) -> Point {
        self.cursor.logical_pos
    }

    /// Gets the visual cursor position, that is,
    /// the position in laid out rows and columns.
    pub fn cursor_visual_pos(&self) -> Point {
        self.cursor.visual_pos
    }

    /// Gets the width of the left margin.
    pub fn margin_width(&self) -> CoordType {
        self.margin_width
    }

    /// Is the left margin enabled?
    pub fn set_margin_enabled(&mut self, enabled: bool) -> bool {
        if self.margin_enabled == enabled {
            false
        } else {
            self.margin_enabled = enabled;
            self.reflow(true);
            true
        }
    }

    /// Gets the width of the text contents for layout.
    pub fn text_width(&self) -> CoordType {
        self.width - self.margin_width
    }

    /// Ask the TUI system to scroll the buffer and make the cursor visible.
    ///
    /// TODO: This function shows that [`TextBuffer`] is poorly abstracted
    /// away from the TUI system. The only reason this exists is so that
    /// if someone outside the TUI code enables word-wrap, the TUI code
    /// recognizes this and scrolls the cursor into view. But outside of this
    /// scrolling, views, etc., are all UI concerns = this should not be here.
    pub fn make_cursor_visible(&mut self) {
        self.wants_cursor_visibility = true;
    }

    /// For the TUI code to retrieve a prior [`TextBuffer::make_cursor_visible()`] request.
    pub fn take_cursor_visibility_request(&mut self) -> bool {
        mem::take(&mut self.wants_cursor_visibility)
    }

    /// Is word-wrap enabled?
    ///
    /// Technically, this is a misnomer, because it's line-wrapping.
    pub fn is_word_wrap_enabled(&self) -> bool {
        self.word_wrap_enabled
    }

    /// Enable or disable word-wrap.
    ///
    /// NOTE: It's expected that the tui code calls `set_width()` sometime after this.
    /// This will then trigger the actual recalculation of the cursor position.
    pub fn set_word_wrap(&mut self, enabled: bool) {
        if self.word_wrap_enabled != enabled {
            self.word_wrap_enabled = enabled;
            self.width = 0; // Force a reflow.
            self.make_cursor_visible();
        }
    }

    /// Set the width available for layout.
    ///
    /// Ideally this would be a pure UI concern, but the text buffer needs this
    /// so that it can abstract away  visual cursor movement such as "go a line up".
    /// What would that even mean if it didn't know how wide a line is?
    pub fn set_width(&mut self, width: CoordType) -> bool {
        if width <= 0 || width == self.width {
            false
        } else {
            self.width = width;
            self.reflow(true);
            true
        }
    }

    /// Set the tab width. Could be anything, but is expected to be 1-8.
    pub fn tab_size(&self) -> CoordType {
        self.tab_size
    }

    /// Set the tab size. Clamped to 1-8.
    pub fn set_tab_size(&mut self, width: CoordType) -> bool {
        let width = width.clamp(1, 8);
        if width == self.tab_size {
            false
        } else {
            self.tab_size = width;
            self.reflow(true);
            true
        }
    }

    /// Returns whether tabs are used for indentation.
    pub fn indent_with_tabs(&self) -> bool {
        self.indent_with_tabs
    }

    /// Sets whether tabs or spaces are used for indentation.
    pub fn set_indent_with_tabs(&mut self, indent_with_tabs: bool) {
        self.indent_with_tabs = indent_with_tabs;
    }

    /// Sets whether the line the cursor is on should be highlighted.
    pub fn set_line_highlight_enabled(&mut self, enabled: bool) {
        self.line_highlight_enabled = enabled;
    }

    /// Sets a ruler column, e.g. 80.
    pub fn set_ruler(&mut self, column: CoordType) {
        self.ruler = column;
    }

    fn reflow(&mut self, force: bool) {
        // +1 onto logical_lines, because line numbers are 1-based.
        // +1 onto log10, because we want the digit width and not the actual log10.
        // +3 onto log10, because we append " | " to the line numbers to form the margin.
        self.margin_width = if self.margin_enabled {
            self.stats.logical_lines.ilog10() as CoordType + 4
        } else {
            0
        };

        let text_width = self.text_width();
        // 2 columns are required, because otherwise wide glyphs wouldn't ever fit.
        let word_wrap_column =
            if self.word_wrap_enabled && text_width >= 2 { text_width } else { 0 };

        if force || self.word_wrap_column > word_wrap_column {
            self.word_wrap_column = word_wrap_column;

            if self.cursor.offset != 0 {
                self.cursor = self
                    .cursor_move_to_logical_internal(Default::default(), self.cursor.logical_pos);
            }

            // Recalculate the line statistics.
            if self.word_wrap_enabled {
                let end = self.cursor_move_to_logical_internal(self.cursor, Point::MAX);
                self.stats.visual_lines = end.visual_pos.y + 1;
            } else {
                self.stats.visual_lines = self.stats.logical_lines;
            }
        }

        self.cursor_for_rendering = None;
    }

    /// Replaces the entire buffer contents with the given `text`.
    /// Assumes that the line count doesn't change.
    pub fn copy_from_str(&mut self, text: &dyn ReadableDocument) {
        if self.buffer.copy_from(text) {
            self.recalc_after_content_swap();
            self.cursor_move_to_logical(Point { x: CoordType::MAX, y: 0 });

            let delete = self.buffer.len() - self.cursor.offset;
            if delete != 0 {
                self.buffer.allocate_gap(self.cursor.offset, 0, delete);
            }
        }
    }

    fn recalc_after_content_swap(&mut self) {
        // If the buffer was changed, nothing we previously saved can be relied upon.
        self.undo_stack.clear();
        self.redo_stack.clear();
        self.last_history_type = HistoryType::Other;
        self.cursor = Default::default();
        self.cursor_for_rendering = None;
        self.set_selection(None);
        self.search = None;
        self.mark_as_clean();
        self.reflow(true);
    }

    /// Copies the contents of the buffer into a string.
    pub fn save_as_string(&mut self, dst: &mut dyn WriteableDocument) {
        self.buffer.copy_into(dst);
        self.mark_as_clean();
    }

    /// Reads a file from disk into the text buffer, detecting encoding and BOM.
    pub fn read_file(
        &mut self,
        file: &mut File,
        encoding: Option<&'static str>,
    ) -> apperr::Result<()> {
        let scratch = scratch_arena(None);
        let mut buf = scratch.alloc_uninit().transpose();
        let mut first_chunk_len = 0;
        let mut read = 0;

        // Read enough bytes to detect the BOM.
        while first_chunk_len < BOM_MAX_LEN {
            read = file_read_uninit(file, &mut buf[first_chunk_len..])?;
            if read == 0 {
                break;
            }
            first_chunk_len += read;
        }

        if let Some(encoding) = encoding {
            self.encoding = encoding;
        } else {
            let bom = detect_bom(unsafe { buf[..first_chunk_len].assume_init_ref() });
            self.encoding = bom.unwrap_or("UTF-8");
        }

        // TODO: Since reading the file can fail, we should ensure that we also reset the cursor here.
        // I don't do it, so that `recalc_after_content_swap()` works.
        self.buffer.clear();

        let done = read == 0;
        if self.encoding == "UTF-8" {
            self.read_file_as_utf8(file, &mut buf, first_chunk_len, done)?;
        } else {
            self.read_file_with_icu(file, &mut buf, first_chunk_len, done)?;
        }

        // Figure out
        // * the logical line count
        // * the newline type (LF or CRLF)
        // * the indentation type (tabs or spaces)
        // * whether there's a final newline
        {
            let chunk = self.read_forward(0);
            let mut offset = 0;
            let mut lines = 0;
            // Number of lines ending in CRLF.
            let mut crlf_count = 0;
            // Number of lines starting with a tab.
            let mut tab_indentations = 0;
            // Number of lines starting with a space.
            let mut space_indentations = 0;
            // Histogram of the indentation depth of lines starting with between 2 and 8 spaces.
            // In other words, `space_indentation_sizes[0]` is the number of lines starting with 2 spaces.
            let mut space_indentation_sizes = [0; 7];

            loop {
                // Check if the line starts with a tab.
                if offset < chunk.len() && chunk[offset] == b'\t' {
                    tab_indentations += 1;
                } else {
                    // Otherwise, check how many spaces the line starts with. Searching for >8 spaces
                    // allows us to reject lines that have more than 1 level of indentation.
                    let space_indentation =
                        chunk[offset..].iter().take(9).take_while(|&&c| c == b' ').count();

                    // We'll also reject lines starting with 1 space, because that's too fickle as a heuristic.
                    if (2..=8).contains(&space_indentation) {
                        space_indentations += 1;

                        // If we encounter an indentation depth of 6, it may either be a 6-space indentation,
                        // two 3-space indentation or 3 2-space indentations. To make this work, we increment
                        // all 3 possible histogram slots.
                        //   2 -> 2
                        //   3 -> 3
                        //   4 -> 4 2
                        //   5 -> 5
                        //   6 -> 6 3 2
                        //   7 -> 7
                        //   8 -> 8 4 2
                        space_indentation_sizes[space_indentation - 2] += 1;
                        if space_indentation & 4 != 0 {
                            space_indentation_sizes[0] += 1;
                        }
                        if space_indentation == 6 || space_indentation == 8 {
                            space_indentation_sizes[space_indentation / 2 - 2] += 1;
                        }
                    }
                }

                (offset, lines) = unicode::newlines_forward(chunk, offset, lines, lines + 1);

                // Check if the preceding line ended in CRLF.
                if offset >= 2 && &chunk[offset - 2..offset] == b"\r\n" {
                    crlf_count += 1;
                }

                // We'll limit our heuristics to the first 1000 lines.
                // That should hopefully be enough in practice.
                if offset >= chunk.len() || lines >= 1000 {
                    break;
                }
            }

            // We'll assume CRLF if more than half of the lines end in CRLF.
            let newlines_are_crlf = crlf_count >= lines / 2;

            // We'll assume tabs if there are more lines starting with tabs than with spaces.
            let indent_with_tabs = tab_indentations > space_indentations;
            let tab_size = if indent_with_tabs {
                // Tabs will get a visual size of 4 spaces by default.
                4
            } else {
                // Otherwise, we'll assume the most common indentation depth.
                // If there are conflicting indentation depths, we'll prefer the maximum, because in the loop
                // above we incremented the histogram slot for 2-spaces when encountering 4-spaces and so on.
                let mut max = 1;
                let mut tab_size = 4;
                for (i, &count) in space_indentation_sizes.iter().enumerate() {
                    if count >= max {
                        max = count;
                        tab_size = i as CoordType + 2;
                    }
                }
                tab_size
            };

            // If the file has more than 1000 lines, figure out how many are remaining.
            if offset < chunk.len() {
                (_, lines) = unicode::newlines_forward(chunk, offset, lines, CoordType::MAX);
            }

            let final_newline = chunk.ends_with(b"\n");

            // Add 1, because the last line doesn't end in a newline (it ends in the literal end).
            self.stats.logical_lines = lines + 1;
            self.stats.visual_lines = self.stats.logical_lines;
            self.newlines_are_crlf = newlines_are_crlf;
            self.insert_final_newline = final_newline;
            self.indent_with_tabs = indent_with_tabs;
            self.tab_size = tab_size;
        }

        self.recalc_after_content_swap();
        Ok(())
    }

    fn read_file_as_utf8(
        &mut self,
        file: &mut File,
        buf: &mut [MaybeUninit<u8>; 4 * KIBI],
        first_chunk_len: usize,
        done: bool,
    ) -> apperr::Result<()> {
        {
            let mut first_chunk = unsafe { buf[..first_chunk_len].assume_init_ref() };
            if first_chunk.starts_with(b"\xEF\xBB\xBF") {
                first_chunk = &first_chunk[3..];
                self.encoding = "UTF-8 BOM";
            }

            self.buffer.replace(0..0, first_chunk);
        }

        if done {
            return Ok(());
        }

        // If we don't have file metadata, the input may be a pipe or a socket.
        // Every read will have the same size until we hit the end.
        let mut chunk_size = 128 * KIBI;
        let mut extra_chunk_size = 128 * KIBI;

        if let Ok(m) = file.metadata() {
            // Usually the next read of size `chunk_size` will read the entire file,
            // but if the size has changed for some reason, then `extra_chunk_size`
            // should be large enough to read the rest of the file.
            // 4KiB is not too large and not too slow.
            let len = m.len() as usize;
            chunk_size = len.saturating_sub(first_chunk_len);
            extra_chunk_size = 4 * KIBI;
        }

        loop {
            let gap = self.buffer.allocate_gap(self.text_length(), chunk_size, 0);
            if gap.is_empty() {
                break;
            }

            let read = file.read(gap)?;
            if read == 0 {
                break;
            }

            self.buffer.commit_gap(read);
            chunk_size = extra_chunk_size;
        }

        Ok(())
    }

    fn read_file_with_icu(
        &mut self,
        file: &mut File,
        buf: &mut [MaybeUninit<u8>; 4 * KIBI],
        first_chunk_len: usize,
        mut done: bool,
    ) -> apperr::Result<()> {
        let scratch = scratch_arena(None);
        let pivot_buffer = scratch.alloc_uninit_slice(4 * KIBI);
        let mut c = icu::Converter::new(pivot_buffer, self.encoding, "UTF-8")?;
        let mut first_chunk = unsafe { buf[..first_chunk_len].assume_init_ref() };

        while !first_chunk.is_empty() {
            let off = self.text_length();
            let gap = self.buffer.allocate_gap(off, 8 * KIBI, 0);
            let (input_advance, mut output_advance) =
                c.convert(first_chunk, slice_as_uninit_mut(gap))?;

            // Remove the BOM from the file, if this is the first chunk.
            // Our caller ensures to only call us once the BOM has been identified,
            // which means that if there's a BOM it must be wholly contained in this chunk.
            if off == 0 {
                let written = &mut gap[..output_advance];
                if written.starts_with(b"\xEF\xBB\xBF") {
                    written.copy_within(3.., 0);
                    output_advance -= 3;
                }
            }

            self.buffer.commit_gap(output_advance);
            first_chunk = &first_chunk[input_advance..];
        }

        let mut buf_len = 0;

        loop {
            if !done {
                let read = file_read_uninit(file, &mut buf[buf_len..])?;
                buf_len += read;
                done = read == 0;
            }

            let gap = self.buffer.allocate_gap(self.text_length(), 8 * KIBI, 0);
            if gap.is_empty() {
                break;
            }

            let read = unsafe { buf[..buf_len].assume_init_ref() };
            let (input_advance, output_advance) = c.convert(read, slice_as_uninit_mut(gap))?;

            self.buffer.commit_gap(output_advance);

            let flush = done && buf_len == 0;
            buf_len -= input_advance;
            buf.copy_within(input_advance.., 0);

            if flush {
                break;
            }
        }

        Ok(())
    }

    /// Writes the text buffer contents to a file, handling BOM and encoding.
    pub fn write_file(&mut self, file: &mut File) -> apperr::Result<()> {
        let mut offset = 0;

        if self.encoding.starts_with("UTF-8") {
            if self.encoding == "UTF-8 BOM" {
                file.write_all(b"\xEF\xBB\xBF")?;
            }
            loop {
                let chunk = self.read_forward(offset);
                if chunk.is_empty() {
                    break;
                }
                file.write_all(chunk)?;
                offset += chunk.len();
            }
        } else {
            self.write_file_with_icu(file)?;
        }

        self.mark_as_clean();
        Ok(())
    }

    fn write_file_with_icu(&mut self, file: &mut File) -> apperr::Result<()> {
        let scratch = scratch_arena(None);
        let pivot_buffer = scratch.alloc_uninit_slice(4 * KIBI);
        let buf = scratch.alloc_uninit_slice(4 * KIBI);
        let mut c = icu::Converter::new(pivot_buffer, "UTF-8", self.encoding)?;
        let mut offset = 0;

        // Write the BOM for the encodings we know need it.
        if self.encoding.starts_with("UTF-16")
            || self.encoding.starts_with("UTF-32")
            || self.encoding == "GB18030"
        {
            let (_, output_advance) = c.convert(b"\xEF\xBB\xBF", buf)?;
            let chunk = unsafe { buf[..output_advance].assume_init_ref() };
            file.write_all(chunk)?;
        }

        loop {
            let chunk = self.read_forward(offset);
            if chunk.is_empty() {
                break;
            }

            let (input_advance, output_advance) = c.convert(chunk, buf)?;
            let chunk = unsafe { buf[..output_advance].assume_init_ref() };
            file.write_all(chunk)?;
            offset += input_advance;
        }

        Ok(())
    }

    /// Returns the current selection.
    pub fn has_selection(&self) -> bool {
        self.selection.is_some()
    }

    fn set_selection(&mut self, selection: Option<TextBufferSelection>) -> u32 {
        self.selection = selection.filter(|s| s.beg != s.end);
        self.selection_generation = self.selection_generation.wrapping_add(1);
        self.selection_generation
    }

    /// Moves the cursor to `visual_pos` and updates the selection to contain it.
    pub fn selection_update_visual(&mut self, visual_pos: Point) {
        self.set_cursor_for_selection(self.cursor_move_to_visual_internal(self.cursor, visual_pos));
    }

    /// Moves the cursor to `logical_pos` and updates the selection to contain it.
    pub fn selection_update_logical(&mut self, logical_pos: Point) {
        self.set_cursor_for_selection(
            self.cursor_move_to_logical_internal(self.cursor, logical_pos),
        );
    }

    /// Moves the cursor by `delta` and updates the selection to contain it.
    pub fn selection_update_delta(&mut self, granularity: CursorMovement, delta: CoordType) {
        self.set_cursor_for_selection(self.cursor_move_delta_internal(
            self.cursor,
            granularity,
            delta,
        ));
    }

    /// Select the current word.
    pub fn select_word(&mut self) {
        let Range { start, end } = navigation::word_select(&self.buffer, self.cursor.offset);
        let beg = self.cursor_move_to_offset_internal(self.cursor, start);
        let end = self.cursor_move_to_offset_internal(beg, end);
        unsafe { self.set_cursor(end) };
        self.set_selection(Some(TextBufferSelection {
            beg: beg.logical_pos,
            end: end.logical_pos,
        }));
    }

    /// Select the current line.
    pub fn select_line(&mut self) {
        let beg = self.cursor_move_to_logical_internal(
            self.cursor,
            Point { x: 0, y: self.cursor.logical_pos.y },
        );
        let end = self
            .cursor_move_to_logical_internal(beg, Point { x: 0, y: self.cursor.logical_pos.y + 1 });
        unsafe { self.set_cursor(end) };
        self.set_selection(Some(TextBufferSelection {
            beg: beg.logical_pos,
            end: end.logical_pos,
        }));
    }

    /// Select the entire document.
    pub fn select_all(&mut self) {
        let beg = Default::default();
        let end = self.cursor_move_to_logical_internal(beg, Point::MAX);
        unsafe { self.set_cursor(end) };
        self.set_selection(Some(TextBufferSelection {
            beg: beg.logical_pos,
            end: end.logical_pos,
        }));
    }

    /// Starts a new selection, if there's none already.
    pub fn start_selection(&mut self) {
        if self.selection.is_none() {
            self.set_selection(Some(TextBufferSelection {
                beg: self.cursor.logical_pos,
                end: self.cursor.logical_pos,
            }));
        }
    }

    /// Destroy the current selection.
    pub fn clear_selection(&mut self) -> bool {
        let had_selection = self.selection.is_some();
        self.set_selection(None);
        had_selection
    }

    /// Find the next occurrence of the given `pattern` and select it.
    pub fn find_and_select(&mut self, pattern: &str, options: SearchOptions) -> apperr::Result<()> {
        if let Some(search) = &mut self.search {
            let search = search.get_mut();
            // When the search input changes we must reset the search.
            if search.pattern != pattern || search.options != options {
                self.search = None;
            }

            // When transitioning from some search to no search, we must clear the selection.
            if pattern.is_empty()
                && let Some(TextBufferSelection { beg, .. }) = self.selection
            {
                self.cursor_move_to_logical(beg);
            }
        }

        if pattern.is_empty() {
            return Ok(());
        }

        let search = match &self.search {
            Some(search) => unsafe { &mut *search.get() },
            None => {
                let search = self.find_construct_search(pattern, options)?;
                self.search = Some(UnsafeCell::new(search));
                unsafe { &mut *self.search.as_ref().unwrap().get() }
            }
        };

        // If we previously searched through the entire document and found 0 matches,
        // then we can avoid searching again.
        if search.no_matches {
            return Ok(());
        }

        // If the user moved the cursor since the last search, but the needle remained the same,
        // we still need to move the start of the search to the new cursor position.
        let next_search_offset = match self.selection {
            Some(TextBufferSelection { beg, end }) => {
                if self.selection_generation == search.selection_generation {
                    search.next_search_offset
                } else {
                    self.cursor_move_to_logical_internal(self.cursor, beg.min(end)).offset
                }
            }
            _ => self.cursor.offset,
        };

        self.find_select_next(search, next_search_offset, true);
        Ok(())
    }

    /// Find the next occurrence of the given `pattern` and replace it with `replacement`.
    pub fn find_and_replace(
        &mut self,
        pattern: &str,
        options: SearchOptions,
        replacement: &str,
    ) -> apperr::Result<()> {
        // Editors traditionally replace the previous search hit, not the next possible one.
        if let (Some(search), Some(..)) = (&mut self.search, &self.selection) {
            let search = search.get_mut();
            if search.selection_generation == self.selection_generation {
                self.write(replacement.as_bytes(), true);
            }
        }

        self.find_and_select(pattern, options)
    }

    /// Find all occurrences of the given `pattern` and replace them with `replacement`.
    pub fn find_and_replace_all(
        &mut self,
        pattern: &str,
        options: SearchOptions,
        replacement: &str,
    ) -> apperr::Result<()> {
        let replacement = replacement.as_bytes();
        let mut search = self.find_construct_search(pattern, options)?;
        let mut offset = 0;

        loop {
            self.find_select_next(&mut search, offset, false);
            if !self.has_selection() {
                break;
            }
            self.write(replacement, true);
            offset = self.cursor.offset;
        }

        Ok(())
    }

    fn find_construct_search(
        &self,
        pattern: &str,
        options: SearchOptions,
    ) -> apperr::Result<ActiveSearch> {
        if pattern.is_empty() {
            return Err(apperr::Error::Icu(1)); // U_ILLEGAL_ARGUMENT_ERROR
        }

        let sanitized_pattern = if options.whole_word && options.use_regex {
            Cow::Owned(format!(r"\b(?:{pattern})\b"))
        } else if options.whole_word {
            let mut p = String::with_capacity(pattern.len() + 16);
            p.push_str(r"\b");

            // Escape regex special characters.
            let b = unsafe { p.as_mut_vec() };
            for &byte in pattern.as_bytes() {
                match byte {
                    b'*' | b'?' | b'+' | b'[' | b'(' | b')' | b'{' | b'}' | b'^' | b'$' | b'|'
                    | b'\\' | b'.' => {
                        b.push(b'\\');
                        b.push(byte);
                    }
                    _ => b.push(byte),
                }
            }

            p.push_str(r"\b");
            Cow::Owned(p)
        } else {
            Cow::Borrowed(pattern)
        };

        let mut flags = icu::Regex::MULTILINE;
        if !options.match_case {
            flags |= icu::Regex::CASE_INSENSITIVE;
        }
        if !options.use_regex && !options.whole_word {
            flags |= icu::Regex::LITERAL;
        }

        // Move the start of the search to the start of the selection,
        // or otherwise to the current cursor position.

        let text = unsafe { icu::Text::new(self)? };
        let regex = unsafe { icu::Regex::new(&sanitized_pattern, flags, &text)? };

        Ok(ActiveSearch {
            pattern: pattern.to_string(),
            options,
            text,
            regex,
            buffer_generation: self.buffer.generation(),
            selection_generation: 0,
            next_search_offset: 0,
            no_matches: false,
        })
    }

    fn find_select_next(&mut self, search: &mut ActiveSearch, offset: usize, wrap: bool) {
        if search.buffer_generation != self.buffer.generation() {
            unsafe { search.regex.set_text(&mut search.text, offset) };
            search.buffer_generation = self.buffer.generation();
            search.next_search_offset = offset;
        } else if search.next_search_offset != offset {
            search.next_search_offset = offset;
            search.regex.reset(offset);
        }

        let mut hit = search.regex.next();

        // If we hit the end of the buffer, and we know that there's something to find,
        // start the search again from the beginning (= wrap around).
        if wrap && hit.is_none() && search.next_search_offset != 0 {
            search.next_search_offset = 0;
            search.regex.reset(0);
            hit = search.regex.next();
        }

        search.selection_generation = if let Some(range) = hit {
            // Now the search offset is no more at the start of the buffer.
            search.next_search_offset = range.end;

            let beg = self.cursor_move_to_offset_internal(self.cursor, range.start);
            let end = self.cursor_move_to_offset_internal(beg, range.end);

            unsafe { self.set_cursor(end) };
            self.make_cursor_visible();

            self.set_selection(Some(TextBufferSelection {
                beg: beg.logical_pos,
                end: end.logical_pos,
            }))
        } else {
            // Avoid searching through the entire document again if we know there's nothing to find.
            search.no_matches = true;
            self.set_selection(None)
        };
    }

    fn measurement_config(&self) -> MeasurementConfig {
        MeasurementConfig::new(&self.buffer)
            .with_word_wrap_column(self.word_wrap_column)
            .with_tab_size(self.tab_size)
    }

    fn goto_line_start(&self, cursor: Cursor, y: CoordType) -> Cursor {
        let mut result = cursor;
        let mut seek_to_line_start = true;

        if y > result.logical_pos.y {
            while y > result.logical_pos.y {
                let chunk = self.read_forward(result.offset);
                if chunk.is_empty() {
                    break;
                }

                let (delta, line) = unicode::newlines_forward(chunk, 0, result.logical_pos.y, y);
                result.offset += delta;
                result.logical_pos.y = line;
            }

            // If we're at the end of the buffer, we could either be there because the last
            // character in the buffer is genuinely a newline, or because the buffer ends in a
            // line of text without trailing newline. The only way to make sure is to seek
            // backwards to the line start again. But otherwise we can skip that.
            seek_to_line_start =
                result.offset == self.text_length() && result.offset != cursor.offset;
        }

        if seek_to_line_start {
            loop {
                let chunk = self.read_backward(result.offset);
                if chunk.is_empty() {
                    break;
                }

                let (delta, line) =
                    unicode::newlines_backward(chunk, chunk.len(), result.logical_pos.y, y);
                result.offset -= chunk.len() - delta;
                result.logical_pos.y = line;
                if delta > 0 {
                    break;
                }
            }
        }

        if result.offset == cursor.offset {
            return result;
        }

        result.logical_pos.x = 0;
        result.visual_pos.x = 0;
        result.visual_pos.y = result.logical_pos.y;
        result.column = 0;
        result.wrap_opp = false;

        if self.word_wrap_column > 0 {
            let upward = result.offset < cursor.offset;
            let (top, bottom) = if upward { (result, cursor) } else { (cursor, result) };

            let mut bottom_remeasured =
                self.measurement_config().with_cursor(top).goto_logical(bottom.logical_pos);

            // The second problem is that visual positions can be ambiguous. A single logical position
            // can map to two visual positions: One at the end of the preceding line in front of
            // a word wrap, and another at the start of the next line after the same word wrap.
            //
            // This, however, only applies if we go upwards, because only then `bottom ≅ cursor`,
            // and thus only then this `bottom` is ambiguous. Otherwise, `bottom ≅ result`
            // and `result` is at a line start which is never ambiguous.
            if upward {
                let a = bottom_remeasured.visual_pos.x;
                let b = bottom.visual_pos.x;
                bottom_remeasured.visual_pos.y = bottom_remeasured.visual_pos.y
                    + (a != 0 && b == 0) as CoordType
                    - (a == 0 && b != 0) as CoordType;
            }

            let mut delta = bottom_remeasured.visual_pos.y - top.visual_pos.y;
            if upward {
                delta = -delta;
            }

            result.visual_pos.y = cursor.visual_pos.y + delta;
        }

        result
    }

    fn cursor_move_to_offset_internal(&self, mut cursor: Cursor, offset: usize) -> Cursor {
        if offset == cursor.offset {
            return cursor;
        }

        // goto_line_start() is fast for seeking across lines _if_ line wrapping is disabled.
        // For backward seeking we have to use it either way, so we're covered there.
        // This implements the forward seeking portion, if it's approx. worth doing so.
        if self.word_wrap_column <= 0 && offset.saturating_sub(cursor.offset) > 1024 {
            // Replacing this with a more optimal, direct memchr() loop appears
            // to improve performance only marginally by another 2% or so.
            // Still, it's kind of "meh" looking at how poorly this is implemented...
            loop {
                let next = self.goto_line_start(cursor, cursor.logical_pos.y + 1);
                // Stop when we either ran past the target offset,
                // or when we hit the end of the buffer and `goto_line_start` backtracked to the line start.
                if next.offset > offset || next.offset <= cursor.offset {
                    break;
                }
                cursor = next;
            }
        }

        while offset < cursor.offset {
            cursor = self.goto_line_start(cursor, cursor.logical_pos.y - 1);
        }

        self.measurement_config().with_cursor(cursor).goto_offset(offset)
    }

    fn cursor_move_to_logical_internal(&self, mut cursor: Cursor, pos: Point) -> Cursor {
        let pos = Point { x: pos.x.max(0), y: pos.y.max(0) };

        if pos == cursor.logical_pos {
            return cursor;
        }

        // goto_line_start() is the fastest way for seeking across lines. As such we always
        // use it if the requested `.y` position is different. We still need to use it if the
        // `.x` position is smaller, but only because `goto_logical()` cannot seek backwards.
        if pos.y != cursor.logical_pos.y || pos.x < cursor.logical_pos.x {
            cursor = self.goto_line_start(cursor, pos.y);
        }

        self.measurement_config().with_cursor(cursor).goto_logical(pos)
    }

    fn cursor_move_to_visual_internal(&self, mut cursor: Cursor, pos: Point) -> Cursor {
        let pos = Point { x: pos.x.max(0), y: pos.y.max(0) };

        if pos == cursor.visual_pos {
            return cursor;
        }

        if self.word_wrap_column <= 0 {
            // Identical to the fast-pass in `cursor_move_to_logical_internal()`.
            if pos.y != cursor.logical_pos.y || pos.x < cursor.logical_pos.x {
                cursor = self.goto_line_start(cursor, pos.y);
            }
        } else {
            // `goto_visual()` can only seek forward, so we need to seek backward here if needed.
            // NOTE that this intentionally doesn't use the `Eq` trait of `Point`, because if
            // `pos.y == cursor.visual_pos.y` we don't need to go to `cursor.logical_pos.y - 1`.
            while pos.y < cursor.visual_pos.y {
                cursor = self.goto_line_start(cursor, cursor.logical_pos.y - 1);
            }
            if pos.y == cursor.visual_pos.y && pos.x < cursor.visual_pos.x {
                cursor = self.goto_line_start(cursor, cursor.logical_pos.y);
            }
        }

        self.measurement_config().with_cursor(cursor).goto_visual(pos)
    }

    fn cursor_move_delta_internal(
        &self,
        mut cursor: Cursor,
        granularity: CursorMovement,
        mut delta: CoordType,
    ) -> Cursor {
        if delta == 0 {
            return cursor;
        }

        let sign = if delta > 0 { 1 } else { -1 };

        match granularity {
            CursorMovement::Grapheme => {
                let start_x = if delta > 0 { 0 } else { CoordType::MAX };

                loop {
                    let target_x = cursor.logical_pos.x + delta;

                    cursor = self.cursor_move_to_logical_internal(
                        cursor,
                        Point { x: target_x, y: cursor.logical_pos.y },
                    );

                    // We can stop if we ran out of remaining delta
                    // (or perhaps ran past the goal; in either case the sign would've changed),
                    // or if we hit the beginning or end of the buffer.
                    delta = target_x - cursor.logical_pos.x;
                    if delta.signum() != sign
                        || (delta < 0 && cursor.offset == 0)
                        || (delta > 0 && cursor.offset >= self.text_length())
                    {
                        break;
                    }

                    cursor = self.cursor_move_to_logical_internal(
                        cursor,
                        Point { x: start_x, y: cursor.logical_pos.y + sign },
                    );

                    // We crossed a newline which counts for 1 grapheme cluster.
                    // So, we also need to run the same check again.
                    delta -= sign;
                    if delta.signum() != sign
                        || cursor.offset == 0
                        || cursor.offset >= self.text_length()
                    {
                        break;
                    }
                }
            }
            CursorMovement::Word => {
                let doc = &self.buffer as &dyn ReadableDocument;
                let mut offset = self.cursor.offset;

                while delta != 0 {
                    if delta < 0 {
                        offset = navigation::word_backward(doc, offset);
                    } else {
                        offset = navigation::word_forward(doc, offset);
                    }
                    delta -= sign;
                }

                cursor = self.cursor_move_to_offset_internal(cursor, offset);
            }
        }

        cursor
    }

    /// Moves the cursor to the given offset.
    pub fn cursor_move_to_offset(&mut self, offset: usize) {
        unsafe { self.set_cursor(self.cursor_move_to_offset_internal(self.cursor, offset)) }
    }

    /// Moves the cursor to the given logical position.
    pub fn cursor_move_to_logical(&mut self, pos: Point) {
        unsafe { self.set_cursor(self.cursor_move_to_logical_internal(self.cursor, pos)) }
    }

    /// Moves the cursor to the given visual position.
    pub fn cursor_move_to_visual(&mut self, pos: Point) {
        unsafe { self.set_cursor(self.cursor_move_to_visual_internal(self.cursor, pos)) }
    }

    /// Moves the cursor by the given delta.
    pub fn cursor_move_delta(&mut self, granularity: CursorMovement, delta: CoordType) {
        unsafe { self.set_cursor(self.cursor_move_delta_internal(self.cursor, granularity, delta)) }
    }

    /// Sets the cursor to the given position, and clears the selection.
    ///
    /// # Safety
    ///
    /// This function performs no checks that the cursor is valid. "Valid" in this case means
    /// that the TextBuffer has not been modified since you received the cursor from this class.
    pub unsafe fn set_cursor(&mut self, cursor: Cursor) {
        self.set_cursor_internal(cursor);
        self.last_history_type = HistoryType::Other;
        self.set_selection(None);
    }

    fn set_cursor_for_selection(&mut self, cursor: Cursor) {
        let beg = match self.selection {
            Some(TextBufferSelection { beg, .. }) => beg,
            None => self.cursor.logical_pos,
        };

        self.set_cursor_internal(cursor);
        self.last_history_type = HistoryType::Other;

        let end = self.cursor.logical_pos;
        self.set_selection(if beg == end { None } else { Some(TextBufferSelection { beg, end }) });
    }

    fn set_cursor_internal(&mut self, cursor: Cursor) {
        debug_assert!(
            cursor.offset <= self.text_length()
                && cursor.logical_pos.x >= 0
                && cursor.logical_pos.y >= 0
                && cursor.logical_pos.y <= self.stats.logical_lines
                && cursor.visual_pos.x >= 0
                && (self.word_wrap_column <= 0 || cursor.visual_pos.x <= self.word_wrap_column)
                && cursor.visual_pos.y >= 0
                && cursor.visual_pos.y <= self.stats.visual_lines
        );
        self.cursor = cursor;
    }

    /// Extracts a rectangular region of the text buffer and writes it to the framebuffer.
    /// The `destination` rect is framebuffer coordinates. The extracted region within this
    /// text buffer has the given `origin` and the same size as the `destination` rect.
    pub fn render(
        &mut self,
        origin: Point,
        destination: Rect,
        focused: bool,
        fb: &mut Framebuffer,
    ) -> Option<RenderResult> {
        if destination.is_empty() {
            return None;
        }

        let scratch = scratch_arena(None);
        let width = destination.width();
        let height = destination.height();
        let line_number_width = self.margin_width.max(3) as usize - 3;
        let text_width = width - self.margin_width;
        let mut visualizer_buf = [0xE2, 0x90, 0x80]; // U+2400 in UTF8
        let mut line = ArenaString::new_in(&scratch);
        let mut visual_pos_x_max = 0;

        // Pick the cursor closer to the `origin.y`.
        let mut cursor = {
            let a = self.cursor;
            let b = self.cursor_for_rendering.unwrap_or_default();
            let da = (a.visual_pos.y - origin.y).abs();
            let db = (b.visual_pos.y - origin.y).abs();
            if da < db { a } else { b }
        };

        let [selection_beg, selection_end] = match self.selection {
            None => [Point::MIN, Point::MIN],
            Some(TextBufferSelection { beg, end }) => minmax(beg, end),
        };

        line.reserve(width as usize * 2);

        for y in 0..height {
            line.clear();

            let visual_line = origin.y + y;
            let mut cursor_beg =
                self.cursor_move_to_visual_internal(cursor, Point { x: origin.x, y: visual_line });
            let cursor_end = self.cursor_move_to_visual_internal(
                cursor_beg,
                Point { x: origin.x + text_width, y: visual_line },
            );

            // Accelerate the next render pass by remembering where we started off.
            if y == 0 {
                self.cursor_for_rendering = Some(cursor_beg);
            }

            if line_number_width != 0 {
                if visual_line >= self.stats.visual_lines {
                    // Past the end of the buffer? Place "    | " in the margin.
                    // Since we know that we won't see line numbers greater than i64::MAX (9223372036854775807)
                    // any time soon, we can use a static string as the template (`MARGIN`) and slice it,
                    // because `line_number_width` can't possibly be larger than 19.
                    let off = 19 - line_number_width;
                    unsafe { std::hint::assert_unchecked(off < MARGIN_TEMPLATE.len()) };
                    line.push_str(&MARGIN_TEMPLATE[off..]);
                } else if self.word_wrap_column <= 0 || cursor_beg.logical_pos.x == 0 {
                    // Regular line? Place "123 | " in the margin.
                    _ = write!(line, "{:1$} │ ", cursor_beg.logical_pos.y + 1, line_number_width);
                } else {
                    // Wrapped line? Place " ... | " in the margin.
                    let number_width = (cursor_beg.logical_pos.y + 1).ilog10() as usize + 1;
                    _ = write!(
                        line,
                        "{0:1$}{0:∙<2$} │ ",
                        "",
                        line_number_width - number_width,
                        number_width
                    );
                    // Blending in the background color will "dim" the indicator dots.
                    let left = destination.left;
                    let top = destination.top + y;
                    fb.blend_fg(
                        Rect {
                            left,
                            top,
                            right: left + line_number_width as CoordType,
                            bottom: top + 1,
                        },
                        fb.indexed_alpha(IndexedColor::Background, 1, 2),
                    );
                }
            }

            // Nothing to do if the entire line is empty.
            if cursor_beg.offset != cursor_end.offset {
                // If we couldn't reach the left edge, we may have stopped short due to a wide glyph.
                // In that case we'll try to find the next character and then compute by how many
                // columns it overlaps the left edge (can be anything between 1 and 7).
                if cursor_beg.visual_pos.x < origin.x {
                    let cursor_next = self.cursor_move_to_logical_internal(
                        cursor_beg,
                        Point { x: cursor_beg.logical_pos.x + 1, y: cursor_beg.logical_pos.y },
                    );

                    if cursor_next.visual_pos.x > origin.x {
                        let overlap = cursor_next.visual_pos.x - origin.x;
                        debug_assert!((1..=7).contains(&overlap));
                        line.push_str(&TAB_WHITESPACE[..overlap as usize]);
                        cursor_beg = cursor_next;
                    }
                }

                fn find_control_char(text: &[u8], mut offset: usize) -> usize {
                    while offset < text.len() && (text[offset] >= 0x20 && text[offset] != 0x7f) {
                        offset += 1;
                    }
                    offset
                }

                let mut global_off = cursor_beg.offset;
                let mut cursor_tab = cursor_beg;

                while global_off < cursor_end.offset {
                    let chunk = self.read_forward(global_off);
                    let chunk = &chunk[..chunk.len().min(cursor_end.offset - global_off)];

                    let mut chunk_off = 0;
                    while chunk_off < chunk.len() {
                        let beg = chunk_off;
                        chunk_off = find_control_char(chunk, beg);

                        for chunk in chunk[beg..chunk_off].utf8_chunks() {
                            if !chunk.valid().is_empty() {
                                line.push_str(chunk.valid());
                            }
                            if !chunk.invalid().is_empty() {
                                line.push('\u{FFFD}');
                            }
                        }

                        while chunk_off < chunk.len()
                            && (chunk[chunk_off] < 0x20 || chunk[chunk_off] == 0x7f)
                        {
                            let ch = chunk[chunk_off];
                            chunk_off += 1;

                            if ch == b'\t' {
                                cursor_tab = self.cursor_move_to_offset_internal(
                                    cursor_tab,
                                    global_off + chunk_off - 1,
                                );
                                let tab_size = self.tab_size - (cursor_tab.column % self.tab_size);
                                line.push_str(&TAB_WHITESPACE[..tab_size as usize]);

                                // Since we know that we just aligned ourselves to the next tab stop,
                                // we can trivially process any successive tabs.
                                while chunk_off < chunk.len() && chunk[chunk_off] == b'\t' {
                                    line.push_str(&TAB_WHITESPACE[..self.tab_size as usize]);
                                    chunk_off += 1;
                                }
                                continue;
                            }

                            visualizer_buf[2] = if ch == 0x7F {
                                0xA1 // U+2421
                            } else {
                                0x80 | ch // 0x00..=0x1F => U+2400..=U+241F
                            };
                            // Our manually constructed UTF8 is never going to be invalid. Trust.
                            line.push_str(unsafe { str::from_utf8_unchecked(&visualizer_buf) });
                        }
                    }

                    global_off += chunk.len();
                }

                visual_pos_x_max = visual_pos_x_max.max(cursor_end.visual_pos.x);
            }

            fb.replace_text(destination.top + y, destination.left, destination.right, &line);

            // Draw the selection on this line, if any.
            // FYI: `cursor_beg.visual_pos.y == visual_line` is necessary as the `visual_line`
            // may be past the end of the document, and so it may not receive a highlight.
            if cursor_beg.visual_pos.y == visual_line
                && selection_beg <= cursor_end.logical_pos
                && selection_end >= cursor_beg.logical_pos
            {
                // By default, we assume the entire line is selected.
                let mut beg = 0;
                let mut end = COORD_TYPE_SAFE_MAX;
                let mut cursor = cursor_beg;

                // The start of the selection is within this line. We need to update selection_beg.
                if selection_beg <= cursor_end.logical_pos
                    && selection_beg >= cursor_beg.logical_pos
                {
                    cursor = self.cursor_move_to_logical_internal(cursor, selection_beg);
                    beg = cursor.visual_pos.x;
                }

                // The end of the selection is within this line. We need to update selection_end.
                if selection_end <= cursor_end.logical_pos
                    && selection_end >= cursor_beg.logical_pos
                {
                    cursor = self.cursor_move_to_logical_internal(cursor, selection_end);
                    end = cursor.visual_pos.x;
                }

                beg = beg.max(origin.x);
                end = end.min(origin.x + text_width);

                let left = destination.left + self.margin_width - origin.x;
                let top = destination.top + y;
                let rect = Rect { left: left + beg, top, right: left + end, bottom: top + 1 };

                let mut bg = oklab_blend(
                    fb.indexed(IndexedColor::Foreground),
                    fb.indexed_alpha(IndexedColor::BrightBlue, 1, 2),
                );
                if !focused {
                    bg = oklab_blend(bg, fb.indexed_alpha(IndexedColor::Background, 1, 2))
                };
                let fg = fb.contrasted(bg);
                fb.blend_bg(rect, bg);
                fb.blend_fg(rect, fg);
            }

            cursor = cursor_end;
        }

        // Colorize the margin that we wrote above.
        if self.margin_width > 0 {
            let margin = Rect {
                left: destination.left,
                top: destination.top,
                right: destination.left + self.margin_width,
                bottom: destination.bottom,
            };
            fb.blend_fg(margin, 0x7f3f3f3f);
        }

        if self.ruler > 0 {
            let left = destination.left + self.margin_width + (self.ruler - origin.x).max(0);
            let right = destination.right;
            if left < right {
                fb.blend_bg(
                    Rect { left, top: destination.top, right, bottom: destination.bottom },
                    fb.indexed_alpha(IndexedColor::BrightRed, 1, 4),
                );
            }
        }

        if focused {
            let mut x = self.cursor.visual_pos.x;
            let mut y = self.cursor.visual_pos.y;

            if self.word_wrap_column > 0 && x >= self.word_wrap_column {
                // The line the cursor is on wraps exactly on the word wrap column which
                // means the cursor is invisible. We need to move it to the next line.
                x = 0;
                y += 1;
            }

            // Move the cursor into screen space.
            x += destination.left - origin.x + self.margin_width;
            y += destination.top - origin.y;

            let cursor = Point { x, y };
            let text = Rect {
                left: destination.left + self.margin_width,
                top: destination.top,
                right: destination.right,
                bottom: destination.bottom,
            };

            if text.contains(cursor) {
                fb.set_cursor(cursor, self.overtype);

                if self.line_highlight_enabled && selection_beg >= selection_end {
                    fb.blend_bg(
                        Rect {
                            left: destination.left,
                            top: cursor.y,
                            right: destination.right,
                            bottom: cursor.y + 1,
                        },
                        0x50282828,
                    );
                }
            }
        }

        Some(RenderResult { visual_pos_x_max })
    }

    /// Inserts `text` at the current cursor position.
    ///
    /// If there's a current selection, it will be replaced.
    /// The selection is cleared after the call.
    pub fn write(&mut self, text: &[u8], raw: bool) {
        if text.is_empty() {
            return;
        }

        if let Some((beg, end)) = self.selection_range_internal(false) {
            self.edit_begin(HistoryType::Write, beg);
            self.edit_delete(end);
            self.set_selection(None);
        }
        if self.active_edit_depth <= 0 {
            self.edit_begin(HistoryType::Write, self.cursor);
        }

        let mut offset = 0;
        let scratch = scratch_arena(None);
        let mut newline_buffer = ArenaString::new_in(&scratch);

        loop {
            // Can't use `unicode::newlines_forward` because bracketed paste uses CR instead of LF/CRLF.
            let offset_next = memchr2(b'\r', b'\n', text, offset);
            let line = &text[offset..offset_next];
            let column_before = self.cursor.logical_pos.x;

            // Write the contents of the line into the buffer.
            let mut line_off = 0;
            while line_off < line.len() {
                // Split the line into chunks of non-tabs and tabs.
                let mut plain = line;
                if !raw && !self.indent_with_tabs {
                    let end = memchr2(b'\t', b'\t', line, line_off);
                    plain = &line[line_off..end];
                }

                // Non-tabs are written as-is, because the outer loop already handles newline translation.
                self.edit_write(plain);
                line_off += plain.len();

                // Now replace tabs with spaces.
                while line_off < line.len() && line[line_off] == b'\t' {
                    let spaces = self.tab_size - (self.cursor.column % self.tab_size);
                    let spaces = &TAB_WHITESPACE.as_bytes()[..spaces as usize];
                    self.edit_write(spaces);
                    line_off += 1;
                }
            }

            if !raw && self.overtype {
                let delete = self.cursor.logical_pos.x - column_before;
                let end = self.cursor_move_to_logical_internal(
                    self.cursor,
                    Point { x: self.cursor.logical_pos.x + delete, y: self.cursor.logical_pos.y },
                );
                self.edit_delete(end);
            }

            offset += line.len();
            if offset >= text.len() {
                break;
            }

            // First, write the newline.
            newline_buffer.clear();
            newline_buffer.push_str(if self.newlines_are_crlf { "\r\n" } else { "\n" });

            if !raw {
                // We'll give the next line the same indentation as the previous one.
                // This block figures out how much that is. We can't reuse that value,
                // because "  a\n  a\n" should give the 3rd line a total indentation of 4.
                // Assuming your terminal has bracketed paste, this won't be a concern though.
                // (If it doesn't, use a different terminal.)
                let tab_size = self.tab_size as usize;
                let line_beg = self.goto_line_start(self.cursor, self.cursor.logical_pos.y);
                let limit = self.cursor.offset;
                let mut off = line_beg.offset;
                let mut newline_indentation = 0usize;

                'outer: while off < limit {
                    let chunk = self.read_forward(off);
                    let chunk = &chunk[..chunk.len().min(limit - off)];

                    for &c in chunk {
                        if c == b' ' {
                            newline_indentation += 1;
                        } else if c == b'\t' {
                            newline_indentation += tab_size - (newline_indentation % tab_size);
                        } else {
                            break 'outer;
                        }
                    }

                    off += chunk.len();
                }

                // If tabs are enabled, add as many tabs as we can.
                if self.indent_with_tabs {
                    let tab_count = newline_indentation / tab_size;
                    newline_buffer.push_repeat('\t', tab_count);
                    newline_indentation -= tab_count * tab_size;
                }

                // If tabs are disabled, or if the indentation wasn't a multiple of the tab size,
                // add spaces to make up the difference.
                newline_buffer.push_repeat(' ', newline_indentation);
            }

            self.edit_write(newline_buffer.as_bytes());

            // Skip one CR/LF/CRLF.
            if offset >= text.len() {
                break;
            }
            if text[offset] == b'\r' {
                offset += 1;
            }
            if offset >= text.len() {
                break;
            }
            if text[offset] == b'\n' {
                offset += 1;
            }
            if offset >= text.len() {
                break;
            }
        }

        // POSIX mandates that all valid lines end in a newline.
        // This isn't all that common on Windows and so we have
        // `self.final_newline` to control this.
        //
        // In order to not annoy people with this, we only add a
        // newline if you just edited the very end of the buffer.
        if self.insert_final_newline
            && self.cursor.offset > 0
            && self.cursor.offset == self.text_length()
            && self.cursor.logical_pos.x > 0
        {
            let cursor = self.cursor;
            self.edit_write(if self.newlines_are_crlf { b"\r\n" } else { b"\n" });
            self.set_cursor_internal(cursor);
        }

        self.edit_end();
    }

    /// Deletes 1 grapheme cluster from the buffer.
    /// `cursor_movements` is expected to be -1 for backspace and 1 for delete.
    /// If there's a current selection, it will be deleted and `cursor_movements` ignored.
    /// The selection is cleared after the call.
    /// Deletes characters from the buffer based on a delta from the cursor.
    pub fn delete(&mut self, granularity: CursorMovement, delta: CoordType) {
        debug_assert!(delta == -1 || delta == 1);

        let mut beg;
        let mut end;

        if let Some(r) = self.selection_range_internal(false) {
            (beg, end) = r;
        } else {
            if (delta == -1 && self.cursor.offset == 0)
                || (delta == 1 && self.cursor.offset >= self.text_length())
            {
                // Nothing to delete.
                return;
            }

            beg = self.cursor;
            end = self.cursor_move_delta_internal(beg, granularity, delta);
            if beg.offset == end.offset {
                return;
            }
            if beg.offset > end.offset {
                mem::swap(&mut beg, &mut end);
            }
        }

        self.edit_begin(HistoryType::Delete, beg);
        self.edit_delete(end);
        self.edit_end();

        self.set_selection(None);
    }

    /// Returns the logical position of the first character on this line.
    /// Return `.x == 0` if there are no non-whitespace characters.
    pub fn indent_end_logical_pos(&self) -> Point {
        let cursor = self.goto_line_start(self.cursor, self.cursor.logical_pos.y);
        let mut chars = 0;
        let mut offset = cursor.offset;

        'outer: loop {
            let chunk = self.read_forward(offset);
            if chunk.is_empty() {
                break;
            }

            for &c in chunk {
                if c == b'\n' || c == b'\r' || (c != b' ' && c != b'\t') {
                    break 'outer;
                }
                chars += 1;
            }

            offset += chunk.len();
        }

        Point { x: chars, y: cursor.logical_pos.y }
    }

    /// Unindents the current selection or line.
    ///
    /// TODO: This function is ripe for some optimizations:
    /// * Instead of replacing the entire selection,
    ///   it should unindent each line directly (as if multiple cursors had been used).
    /// * The cursor movement at the end is rather costly, but at least without word wrap
    ///   it should be possible to calculate it directly from the removed amount.
    pub fn unindent(&mut self) {
        let mut selection_beg = self.cursor.logical_pos;
        let mut selection_end = selection_beg;

        if let Some(TextBufferSelection { beg, end }) = self.selection {
            selection_beg = beg;
            selection_end = end;
        }

        let [beg, end] = minmax(selection_beg, selection_end);
        let beg = self.cursor_move_to_logical_internal(self.cursor, Point { x: 0, y: beg.y });
        let end = self.cursor_move_to_logical_internal(beg, Point { x: CoordType::MAX, y: end.y });

        let mut replacement = Vec::new();
        self.buffer.extract_raw(beg.offset, end.offset, &mut replacement, 0);

        let initial_len = replacement.len();
        let mut offset = 0;
        let mut y = beg.logical_pos.y;

        loop {
            if offset >= replacement.len() {
                break;
            }

            let mut remove = 0;

            if replacement[offset] == b'\t' {
                remove = 1;
            } else {
                while remove < self.tab_size as usize
                    && offset + remove < replacement.len()
                    && replacement[offset + remove] == b' '
                {
                    remove += 1;
                }
            }

            if remove > 0 {
                replacement.drain(offset..offset + remove);
            }

            if y == selection_beg.y {
                selection_beg.x -= remove as CoordType;
            }
            if y == selection_end.y {
                selection_end.x -= remove as CoordType;
            }

            (offset, y) = unicode::newlines_forward(&replacement, offset, y, y + 1);
        }

        if replacement.len() == initial_len {
            // Nothing to do.
            return;
        }

        self.edit_begin(HistoryType::Other, beg);
        self.edit_delete(end);
        self.edit_write(&replacement);
        self.edit_end();

        if let Some(TextBufferSelection { beg, end }) = &mut self.selection {
            *beg = selection_beg;
            *end = selection_end;
        }

        self.set_cursor_internal(self.cursor_move_to_logical_internal(self.cursor, selection_end));
    }

    /// Extracts the contents of the current selection.
    /// May optionally delete it, if requested. This is meant to be used for Ctrl+X.
    pub fn extract_selection(&mut self, delete: bool) -> Vec<u8> {
        let Some((beg, end)) = self.selection_range_internal(true) else {
            return Vec::new();
        };

        let mut out = Vec::new();
        self.buffer.extract_raw(beg.offset, end.offset, &mut out, 0);

        if delete && !out.is_empty() {
            self.edit_begin(HistoryType::Delete, beg);
            self.edit_delete(end);
            self.edit_end();
            self.set_selection(None);
        }

        out
    }

    /// Extracts the contents of the current selection the user made.
    /// This differs from [`TextBuffer::extract_selection()`] in that
    /// it does nothing if the selection was made by searching.
    pub fn extract_user_selection(&mut self, delete: bool) -> Option<Vec<u8>> {
        if !self.has_selection() {
            return None;
        }

        if let Some(search) = &self.search {
            let search = unsafe { &*search.get() };
            if search.selection_generation == self.selection_generation {
                return None;
            }
        }

        Some(self.extract_selection(delete))
    }

    /// Returns the current selection anchors, or `None` if there
    /// is no selection. The returned logical positions are sorted.
    pub fn selection_range(&self) -> Option<(Cursor, Cursor)> {
        self.selection_range_internal(false)
    }

    /// Returns the current selection anchors.
    ///
    /// If there's no selection and `line_fallback` is `true`,
    /// the start/end of the current line are returned.
    /// This is meant to be used for Ctrl+C / Ctrl+X.
    fn selection_range_internal(&self, line_fallback: bool) -> Option<(Cursor, Cursor)> {
        let [beg, end] = match self.selection {
            None if !line_fallback => return None,
            None => [
                Point { x: 0, y: self.cursor.logical_pos.y },
                Point { x: 0, y: self.cursor.logical_pos.y + 1 },
            ],
            Some(TextBufferSelection { beg, end }) => minmax(beg, end),
        };

        let beg = self.cursor_move_to_logical_internal(self.cursor, beg);
        let end = self.cursor_move_to_logical_internal(beg, end);

        if beg.offset < end.offset { Some((beg, end)) } else { None }
    }

    /// Starts a new edit operation.
    /// This is used for tracking the undo/redo history.
    fn edit_begin(&mut self, history_type: HistoryType, cursor: Cursor) {
        self.active_edit_depth += 1;
        if self.active_edit_depth > 1 {
            return;
        }

        let cursor_before = self.cursor;
        self.set_cursor_internal(cursor);

        // If both the last and this are a Write/Delete operation, we skip allocating a new undo history item.
        if history_type != self.last_history_type
            || !matches!(history_type, HistoryType::Write | HistoryType::Delete)
        {
            self.redo_stack.clear();
            while self.undo_stack.len() > 1000 {
                self.undo_stack.pop_front();
            }

            self.last_history_type = history_type;
            self.undo_stack.push_back(SemiRefCell::new(HistoryEntry {
                cursor_before: cursor_before.logical_pos,
                selection_before: self.selection,
                stats_before: self.stats,
                generation_before: self.buffer.generation(),
                cursor: cursor.logical_pos,
                deleted: Vec::new(),
                added: Vec::new(),
            }));
        }

        self.active_edit_off = cursor.offset;

        // If word-wrap is enabled, the visual layout of all logical lines affected by the write
        // may have changed. This includes even text before the insertion point up to the line
        // start, because this write may have joined with a word before the initial cursor.
        // See other uses of `word_wrap_cursor_next_line` in this function.
        if self.word_wrap_column > 0 {
            let safe_start = self.goto_line_start(cursor, cursor.logical_pos.y);
            let next_line = self.cursor_move_to_logical_internal(
                cursor,
                Point { x: 0, y: cursor.logical_pos.y + 1 },
            );
            self.active_edit_line_info = Some(ActiveEditLineInfo {
                safe_start,
                line_height_in_rows: next_line.visual_pos.y - safe_start.visual_pos.y,
                distance_next_line_start: next_line.offset - cursor.offset,
            });
        }
    }

    /// Writes `text` into the buffer at the current cursor position.
    /// It records the change in the undo stack.
    fn edit_write(&mut self, text: &[u8]) {
        let logical_y_before = self.cursor.logical_pos.y;

        // Copy the written portion into the undo entry.
        {
            let mut undo = self.undo_stack.back_mut().unwrap().borrow_mut();
            undo.added.extend_from_slice(text);
        }

        // Write!
        self.buffer.replace(self.active_edit_off..self.active_edit_off, text);

        // Move self.cursor to the end of the newly written text. Can't use `self.set_cursor_internal`,
        // because we're still in the progress of recalculating the line stats.
        self.active_edit_off += text.len();
        self.cursor = self.cursor_move_to_offset_internal(self.cursor, self.active_edit_off);
        self.stats.logical_lines += self.cursor.logical_pos.y - logical_y_before;
    }

    /// Deletes the text between the current cursor position and `to`.
    /// It records the change in the undo stack.
    fn edit_delete(&mut self, to: Cursor) {
        debug_assert!(to.offset >= self.active_edit_off);

        let logical_y_before = self.cursor.logical_pos.y;
        let off = self.active_edit_off;
        let mut out_off = usize::MAX;

        let mut undo = self.undo_stack.back_mut().unwrap().borrow_mut();
        if self.cursor.logical_pos < undo.cursor {
            out_off = 0; // Prepend the deleted portion.
            undo.cursor = self.cursor.logical_pos; // Note the start of the deleted portion.
        }

        // Copy the deleted portion into the undo entry.
        let deleted = &mut undo.deleted;
        self.buffer.extract_raw(off, to.offset, deleted, out_off);

        // Delete the portion from the buffer by enlarging the gap.
        let count = to.offset - off;
        self.buffer.allocate_gap(off, 0, count);

        self.stats.logical_lines += logical_y_before - to.logical_pos.y;
    }

    /// Finalizes the current edit operation
    /// and recalculates the line statistics.
    fn edit_end(&mut self) {
        self.active_edit_depth -= 1;
        assert!(self.active_edit_depth >= 0);
        if self.active_edit_depth > 0 {
            return;
        }

        #[cfg(debug_assertions)]
        {
            let entry = self.undo_stack.back_mut().unwrap().borrow_mut();
            debug_assert!(!entry.deleted.is_empty() || !entry.added.is_empty());
        }

        if let Some(info) = self.active_edit_line_info.take() {
            let deleted_count = self.undo_stack.back_mut().unwrap().borrow_mut().deleted.len();
            let target = self.cursor.logical_pos;

            // From our safe position we can measure the actual visual position of the cursor.
            self.set_cursor_internal(self.cursor_move_to_logical_internal(info.safe_start, target));

            // If content is added at the insertion position, that's not a problem:
            // We can just remeasure the height of this one line and calculate the delta.
            // `deleted_count` is 0 in this case.
            //
            // The problem is when content is deleted, because it may affect lines
            // beyond the end of the `next_line`. In that case we have to measure
            // the entire buffer contents until the end to compute `self.stats.visual_lines`.
            if deleted_count < info.distance_next_line_start {
                // Now we can measure how many more visual rows this logical line spans.
                let next_line = self
                    .cursor_move_to_logical_internal(self.cursor, Point { x: 0, y: target.y + 1 });
                let lines_before = info.line_height_in_rows;
                let lines_after = next_line.visual_pos.y - info.safe_start.visual_pos.y;
                self.stats.visual_lines += lines_after - lines_before;
            } else {
                let end = self.cursor_move_to_logical_internal(self.cursor, Point::MAX);
                self.stats.visual_lines = end.visual_pos.y + 1;
            }
        } else {
            // If word-wrap is disabled the visual line count always matches the logical one.
            self.stats.visual_lines = self.stats.logical_lines;
        }

        self.search = None;

        // Also takes care of clearing `cursor_for_rendering`.
        self.reflow(false);
    }

    /// Undo the last edit operation.
    pub fn undo(&mut self) {
        self.undo_redo(true);
    }

    /// Redo the last undo operation.
    pub fn redo(&mut self) {
        self.undo_redo(false);
    }

    fn undo_redo(&mut self, undo: bool) {
        // Transfer the last entry from the undo stack to the redo stack or vice versa.
        {
            let (from, to) = if undo {
                (&mut self.undo_stack, &mut self.redo_stack)
            } else {
                (&mut self.redo_stack, &mut self.undo_stack)
            };

            let Some(list) = from.cursor_back_mut().remove_current_as_list() else {
                return;
            };

            to.cursor_back_mut().splice_after(list);
        }

        let change = {
            let to = if undo { &self.redo_stack } else { &self.undo_stack };
            to.back().unwrap()
        };

        // Move to the point where the modification took place.
        let cursor = self.cursor_move_to_logical_internal(self.cursor, change.borrow().cursor);

        let safe_cursor = if self.word_wrap_column > 0 {
            // If word-wrap is enabled, we need to move the cursor to the beginning of the line.
            // This is because the undo/redo operation may have changed the visual position of the cursor.
            self.goto_line_start(cursor, cursor.logical_pos.y)
        } else {
            cursor
        };

        {
            let buffer_generation = self.buffer.generation();
            let mut change = change.borrow_mut();
            let change = &mut *change;

            // Undo: Whatever was deleted is now added and vice versa.
            mem::swap(&mut change.deleted, &mut change.added);

            // Delete the inserted portion.
            self.buffer.allocate_gap(cursor.offset, 0, change.deleted.len());

            // Reinsert the deleted portion.
            {
                let added = &change.added[..];
                let mut beg = 0;
                let mut offset = cursor.offset;

                while beg < added.len() {
                    let (end, line) = unicode::newlines_forward(added, beg, 0, 1);
                    let has_newline = line != 0;
                    let link = &added[beg..end];
                    let line = unicode::strip_newline(link);
                    let mut written;

                    {
                        let gap = self.buffer.allocate_gap(offset, line.len() + 2, 0);
                        written = slice_copy_safe(gap, line);

                        if has_newline {
                            if self.newlines_are_crlf && written < gap.len() {
                                gap[written] = b'\r';
                                written += 1;
                            }
                            if written < gap.len() {
                                gap[written] = b'\n';
                                written += 1;
                            }
                        }

                        self.buffer.commit_gap(written);
                    }

                    beg = end;
                    offset += written;
                }
            }

            // Restore the previous line statistics.
            mem::swap(&mut self.stats, &mut change.stats_before);

            // Restore the previous selection.
            mem::swap(&mut self.selection, &mut change.selection_before);

            // Pretend as if the buffer was never modified.
            self.buffer.set_generation(change.generation_before);
            change.generation_before = buffer_generation;

            // Restore the previous cursor.
            let cursor_before =
                self.cursor_move_to_logical_internal(safe_cursor, change.cursor_before);
            change.cursor_before = self.cursor.logical_pos;
            // Can't use `set_cursor_internal` here, because we haven't updated the line stats yet.
            self.cursor = cursor_before;

            if self.undo_stack.is_empty() {
                self.last_history_type = HistoryType::Other;
            }
        }

        // Also takes care of clearing `cursor_for_rendering`.
        self.reflow(false);
    }

    /// For interfacing with ICU.
    pub(crate) fn read_backward(&self, off: usize) -> &[u8] {
        self.buffer.read_backward(off)
    }

    /// For interfacing with ICU.
    pub fn read_forward(&self, off: usize) -> &[u8] {
        self.buffer.read_forward(off)
    }
}

pub enum Bom {
    None,
    UTF8,
    UTF16LE,
    UTF16BE,
    UTF32LE,
    UTF32BE,
    GB18030,
}

const BOM_MAX_LEN: usize = 4;

fn detect_bom(bytes: &[u8]) -> Option<&'static str> {
    if bytes.len() >= 4 {
        if bytes.starts_with(b"\xFF\xFE\x00\x00") {
            return Some("UTF-32LE");
        }
        if bytes.starts_with(b"\x00\x00\xFE\xFF") {
            return Some("UTF-32BE");
        }
        if bytes.starts_with(b"\x84\x31\x95\x33") {
            return Some("GB18030");
        }
    }
    if bytes.len() >= 3 && bytes.starts_with(b"\xEF\xBB\xBF") {
        return Some("UTF-8");
    }
    if bytes.len() >= 2 {
        if bytes.starts_with(b"\xFF\xFE") {
            return Some("UTF-16LE");
        }
        if bytes.starts_with(b"\xFE\xFF") {
            return Some("UTF-16BE");
        }
    }
    None
}
