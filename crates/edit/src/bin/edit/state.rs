// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::borrow::Cow;
use std::ffi::{OsStr, OsString};
use std::mem;
use std::path::{Path, PathBuf};

use edit::framebuffer::IndexedColor;
use edit::helpers::*;
use edit::oklab::StraightRgba;
use edit::tui::*;
use edit::{buffer, icu};

use crate::apperr;
use crate::documents::DocumentManager;
use crate::localization::*;

#[repr(transparent)]
pub struct FormatApperr(apperr::Error);

impl From<apperr::Error> for FormatApperr {
    fn from(err: apperr::Error) -> Self {
        Self(err)
    }
}

impl std::fmt::Display for FormatApperr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            apperr::Error::Icu(icu::ICU_MISSING_ERROR) => f.write_str(loc(LocId::ErrorIcuMissing)),
            apperr::Error::Icu(ref err) => err.fmt(f),
            apperr::Error::Io(ref err) => err.fmt(f),
        }
    }
}

pub struct DisplayablePathBuf {
    value: PathBuf,
    str: Cow<'static, str>,
}

impl DisplayablePathBuf {
    #[allow(dead_code, reason = "only used on Windows")]
    pub fn from_string(string: String) -> Self {
        let str = Cow::Borrowed(string.as_str());
        let str = unsafe { mem::transmute::<Cow<'_, str>, Cow<'_, str>>(str) };
        let value = PathBuf::from(string);
        Self { value, str }
    }

    pub fn from_path(value: PathBuf) -> Self {
        let str = value.to_string_lossy();
        let str = unsafe { mem::transmute::<Cow<'_, str>, Cow<'_, str>>(str) };
        Self { value, str }
    }

    pub fn as_path(&self) -> &Path {
        &self.value
    }

    pub fn as_str(&self) -> &str {
        &self.str
    }

    pub fn as_bytes(&self) -> &[u8] {
        self.value.as_os_str().as_encoded_bytes()
    }
}

impl Default for DisplayablePathBuf {
    fn default() -> Self {
        Self { value: Default::default(), str: Cow::Borrowed("") }
    }
}

impl Clone for DisplayablePathBuf {
    fn clone(&self) -> Self {
        Self::from_path(self.value.clone())
    }
}

impl From<OsString> for DisplayablePathBuf {
    fn from(s: OsString) -> Self {
        Self::from_path(PathBuf::from(s))
    }
}

impl<T: ?Sized + AsRef<OsStr>> From<&T> for DisplayablePathBuf {
    fn from(s: &T) -> Self {
        Self::from_path(PathBuf::from(s))
    }
}

pub struct StateSearch {
    pub kind: StateSearchKind,
    pub focus: bool,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum StateSearchKind {
    Hidden,
    Disabled,
    Search,
    Replace,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum StateFilePicker {
    None,
    Open,
    SaveAs,

    SaveAsShown, // Transitioned from SaveAs
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum StateEncodingChange {
    None,
    Convert,
    Reopen,
}

#[derive(Default)]
pub struct OscTitleFileStatus {
    pub filename: String,
    pub dirty: bool,
}

/// Direction for split views.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub enum SplitDirection {
    #[default]
    None,       // Single pane (no split)
    Horizontal, // Panes arranged left | right
    Vertical,   // Panes arranged top / bottom
}

/// A pane in the split view layout.
/// Each pane displays a document (via its buffer reference).
#[derive(Clone)]
pub struct Pane {
    /// Index of the document in DocumentManager (for reference).
    /// Note: This may become stale when documents are closed.
    pub document_index: usize,
    /// Reference to the document's text buffer.
    /// Multiple panes can share the same buffer.
    pub buffer: edit::buffer::RcTextBuffer,
    /// The filename to display for this pane.
    pub filename: String,
}

/// Manages the split view layout.
#[derive(Default)]
pub struct SplitLayout {
    /// List of panes. When split_direction is None, only panes[0] is used.
    pub panes: Vec<Pane>,
    /// Index of the currently active/focused pane.
    pub active_pane: usize,
    /// How panes are arranged.
    pub split_direction: SplitDirection,
}

impl SplitLayout {
    /// Returns the number of visible panes.
    #[inline]
    pub fn pane_count(&self) -> usize {
        if self.split_direction == SplitDirection::None {
            1.min(self.panes.len())
        } else {
            self.panes.len()
        }
    }

    /// Resets the layout to default state.
    #[inline]
    pub fn reset(&mut self) {
        *self = Self::default();
    }
}

pub struct State {
    pub menubar_color_bg: StraightRgba,
    pub menubar_color_fg: StraightRgba,

    pub documents: DocumentManager,

    // A ring buffer of the last 10 errors.
    pub error_log: [String; 10],
    pub error_log_index: usize,
    pub error_log_count: usize,

    pub wants_file_picker: StateFilePicker,
    pub file_picker_pending_dir: DisplayablePathBuf,
    pub file_picker_pending_dir_revision: u64, // Bumped every time `file_picker_pending_dir` changes.
    pub file_picker_pending_name: PathBuf,
    pub file_picker_entries: Option<[Vec<DisplayablePathBuf>; 3]>, // ["..", directories, files]
    pub file_picker_overwrite_warning: Option<PathBuf>,            // The path the warning is about.
    pub file_picker_autocomplete: Vec<DisplayablePathBuf>,

    pub wants_search: StateSearch,
    pub search_needle: String,
    pub search_replacement: String,
    pub search_options: buffer::SearchOptions,
    pub search_success: bool,

    pub wants_encoding_picker: bool,
    pub wants_encoding_change: StateEncodingChange,
    pub encoding_picker_needle: String,
    pub encoding_picker_results: Option<Vec<icu::Encoding>>,

    pub wants_save: bool,
    pub wants_statusbar_focus: bool,
    pub wants_indentation_picker: bool,
    pub wants_go_to_file: bool,
    pub wants_about: bool,
    pub wants_close: bool,
    pub wants_exit: bool,
    pub wants_goto: bool,
    pub goto_target: String,
    pub goto_invalid: bool,

    // Split view state
    pub split_layout: SplitLayout,
    pub wants_split_horizontal: bool,
    pub wants_split_vertical: bool,
    pub wants_close_pane: bool,
    pub wants_focus_next_pane: bool,
    pub wants_focus_prev_pane: bool,

    pub osc_title_file_status: OscTitleFileStatus,
    pub osc_clipboard_sync: bool,
    pub osc_clipboard_always_send: bool,
    pub exit: bool,
}

impl State {
    pub fn new() -> apperr::Result<Self> {
        Ok(Self {
            menubar_color_bg: StraightRgba::zero(),
            menubar_color_fg: StraightRgba::zero(),

            documents: Default::default(),

            error_log: [const { String::new() }; 10],
            error_log_index: 0,
            error_log_count: 0,

            wants_file_picker: StateFilePicker::None,
            file_picker_pending_dir: Default::default(),
            file_picker_pending_dir_revision: 0,
            file_picker_pending_name: Default::default(),
            file_picker_entries: None,
            file_picker_overwrite_warning: None,
            file_picker_autocomplete: Vec::new(),

            wants_search: StateSearch { kind: StateSearchKind::Hidden, focus: false },
            search_needle: Default::default(),
            search_replacement: Default::default(),
            search_options: Default::default(),
            search_success: true,

            wants_encoding_picker: false,
            encoding_picker_needle: Default::default(),
            encoding_picker_results: Default::default(),

            wants_save: false,
            wants_statusbar_focus: false,
            wants_encoding_change: StateEncodingChange::None,
            wants_indentation_picker: false,
            wants_go_to_file: false,
            wants_about: false,
            wants_close: false,
            wants_exit: false,
            wants_goto: false,
            goto_target: Default::default(),
            goto_invalid: false,

            // Split view state
            split_layout: Default::default(),
            wants_split_horizontal: false,
            wants_split_vertical: false,
            wants_close_pane: false,
            wants_focus_next_pane: false,
            wants_focus_prev_pane: false,

            osc_title_file_status: Default::default(),
            osc_clipboard_sync: false,
            osc_clipboard_always_send: false,
            exit: false,
        })
    }
}

pub fn draw_add_untitled_document(ctx: &mut Context, state: &mut State) {
    if let Err(err) = state.documents.add_untitled() {
        error_log_add(ctx, state, err);
    }
}

pub fn error_log_add(ctx: &mut Context, state: &mut State, err: apperr::Error) {
    let msg = format!("{}", FormatApperr::from(err));
    if !msg.is_empty() {
        state.error_log[state.error_log_index] = msg;
        state.error_log_index = (state.error_log_index + 1) % state.error_log.len();
        state.error_log_count = state.error_log.len().min(state.error_log_count + 1);
        ctx.needs_rerender();
    }
}

pub fn draw_error_log(ctx: &mut Context, state: &mut State) {
    ctx.modal_begin("error", loc(LocId::ErrorDialogTitle));
    ctx.attr_background_rgba(ctx.indexed(IndexedColor::Red));
    ctx.attr_foreground_rgba(ctx.indexed(IndexedColor::BrightWhite));
    {
        ctx.block_begin("content");
        ctx.attr_padding(Rect::three(0, 2, 1));
        {
            let off = state.error_log_index + state.error_log.len() - state.error_log_count;

            for i in 0..state.error_log_count {
                let idx = (off + i) % state.error_log.len();
                let msg = &state.error_log[idx][..];

                if !msg.is_empty() {
                    ctx.next_block_id_mixin(i as u64);
                    ctx.label("error", msg);
                    ctx.attr_overflow(Overflow::TruncateTail);
                }
            }
        }
        ctx.block_end();

        if ctx.button("ok", loc(LocId::Ok), ButtonStyle::default()) {
            state.error_log_count = 0;
        }
        ctx.attr_position(Position::Center);
        ctx.inherit_focus();
    }
    if ctx.modal_end() {
        state.error_log_count = 0;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use edit::buffer::TextBuffer;

    /// Helper to create a test pane with a dummy buffer.
    fn create_test_pane(filename: &str) -> Pane {
        Pane {
            document_index: 0,
            buffer: TextBuffer::new_rc(true).unwrap(),
            filename: filename.to_string(),
        }
    }

    #[test]
    fn split_layout_default_is_empty() {
        let layout = SplitLayout::default();
        assert!(layout.panes.is_empty());
        assert_eq!(layout.active_pane, 0);
        assert_eq!(layout.split_direction, SplitDirection::None);
    }

    #[test]
    fn pane_count_returns_zero_when_empty() {
        let layout = SplitLayout::default();
        // pane_count returns min(1, 0) = 0 when no panes
        assert_eq!(layout.pane_count(), 0);
    }

    #[test]
    fn pane_count_returns_one_in_single_mode() {
        let mut layout = SplitLayout::default();
        layout.panes.push(create_test_pane("test.txt"));
        layout.split_direction = SplitDirection::None;

        assert_eq!(layout.pane_count(), 1);
    }

    #[test]
    fn pane_count_returns_one_even_with_multiple_panes_in_none_mode() {
        let mut layout = SplitLayout::default();
        layout.panes.push(create_test_pane("file1.txt"));
        layout.panes.push(create_test_pane("file2.txt"));
        layout.split_direction = SplitDirection::None;

        // In None mode, only 1 pane is visible regardless of how many exist
        assert_eq!(layout.pane_count(), 1);
    }

    #[test]
    fn pane_count_returns_actual_count_in_horizontal_split() {
        let mut layout = SplitLayout::default();
        layout.panes.push(create_test_pane("file1.txt"));
        layout.panes.push(create_test_pane("file2.txt"));
        layout.split_direction = SplitDirection::Horizontal;

        assert_eq!(layout.pane_count(), 2);
    }

    #[test]
    fn pane_count_returns_actual_count_in_vertical_split() {
        let mut layout = SplitLayout::default();
        layout.panes.push(create_test_pane("file1.txt"));
        layout.panes.push(create_test_pane("file2.txt"));
        layout.split_direction = SplitDirection::Vertical;

        assert_eq!(layout.pane_count(), 2);
    }

    #[test]
    fn reset_clears_layout() {
        let mut layout = SplitLayout::default();
        layout.panes.push(create_test_pane("file1.txt"));
        layout.panes.push(create_test_pane("file2.txt"));
        layout.active_pane = 1;
        layout.split_direction = SplitDirection::Horizontal;

        layout.reset();

        assert!(layout.panes.is_empty());
        assert_eq!(layout.active_pane, 0);
        assert_eq!(layout.split_direction, SplitDirection::None);
    }

    #[test]
    fn split_direction_default_is_none() {
        assert_eq!(SplitDirection::default(), SplitDirection::None);
    }

    #[test]
    fn focus_next_pane_wraps_around() {
        let mut layout = SplitLayout::default();
        layout.panes.push(create_test_pane("file1.txt"));
        layout.panes.push(create_test_pane("file2.txt"));
        layout.split_direction = SplitDirection::Horizontal;
        layout.active_pane = 0;

        // Simulate focus next
        let count = layout.pane_count();
        layout.active_pane = (layout.active_pane + 1) % count;
        assert_eq!(layout.active_pane, 1);

        // Wrap around
        layout.active_pane = (layout.active_pane + 1) % count;
        assert_eq!(layout.active_pane, 0);
    }

    #[test]
    fn focus_prev_pane_wraps_around() {
        let mut layout = SplitLayout::default();
        layout.panes.push(create_test_pane("file1.txt"));
        layout.panes.push(create_test_pane("file2.txt"));
        layout.split_direction = SplitDirection::Horizontal;
        layout.active_pane = 0;

        // Simulate focus prev (wraps to last)
        let count = layout.pane_count();
        layout.active_pane = (layout.active_pane + count - 1) % count;
        assert_eq!(layout.active_pane, 1);

        // Go back to first
        layout.active_pane = (layout.active_pane + count - 1) % count;
        assert_eq!(layout.active_pane, 0);
    }
}
