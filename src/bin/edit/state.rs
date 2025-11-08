// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::borrow::Cow;
use std::ffi::{OsStr, OsString};
use std::mem;
use std::path::{Path, PathBuf};

use edit::framebuffer::{self, INDEXED_COLORS_COUNT, IndexedColor};
use edit::helpers::*;
use edit::input::vk;
use edit::oklab::StraightRgba;
use edit::tui::*;
use edit::{apperr, arena_format, buffer, icu, sys};

use crate::documents::{Document, DocumentManager};
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
            apperr::APP_ICU_MISSING => f.write_str(loc(LocId::ErrorIcuMissing)),
            apperr::Error::App(code) => write!(f, "Unknown app error code: {code}"),
            apperr::Error::Icu(code) => icu::apperr_format(f, code),
            apperr::Error::Sys(code) => sys::apperr_format(f, code),
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
    pub wants_preferences: bool,
    pub wants_about: bool,
    pub wants_close: bool,
    pub wants_exit: bool,
    pub wants_goto: bool,
    pub goto_target: String,
    pub goto_invalid: bool,

    pub osc_title_file_status: OscTitleFileStatus,
    pub osc_clipboard_sync: bool,
    pub osc_clipboard_always_send: bool,
    pub exit: bool,
    pub preferences: Preferences,
    system_palette: [StraightRgba; INDEXED_COLORS_COUNT],
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
            wants_preferences: false,
            wants_about: false,
            wants_close: false,
            wants_exit: false,
            wants_goto: false,
            goto_target: Default::default(),
            goto_invalid: false,

            osc_title_file_status: Default::default(),
            osc_clipboard_sync: false,
            osc_clipboard_always_send: false,
            exit: false,
            preferences: Preferences::default(),
            system_palette: framebuffer::DEFAULT_THEME,
        })
    }
}

pub fn draw_add_untitled_document(ctx: &mut Context, state: &mut State) {
    let prefs = state.preferences.clone();
    match state.documents.add_untitled() {
        Ok(doc) => prefs.apply_to_document(doc),
        Err(err) => error_log_add(ctx, state, err),
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

pub fn draw_dialog_preferences(ctx: &mut Context, state: &mut State) {
    ctx.modal_begin("preferences", loc(LocId::PreferencesDialogTitle));
    ctx.attr_focus_well();
    ctx.attr_padding(Rect::three(1, 2, 1));
    let mut close = false;
    if ctx.contains_focus() && ctx.consume_shortcut(vk::ESCAPE) {
        close = true;
    }

    ctx.block_begin("content");
    ctx.attr_padding(Rect::three(0, 0, 1));
    {
        if ctx.checkbox(
            "pref-auto-close",
            loc(LocId::PreferencesAutoClose),
            &mut state.preferences.auto_close_pairs,
        ) {
            state.apply_preferences_to_documents();
            ctx.needs_rerender();
        }

        if ctx.checkbox(
            "pref-line-highlight",
            loc(LocId::PreferencesLineHighlight),
            &mut state.preferences.line_highlight,
        ) {
            state.apply_preferences_to_documents();
            ctx.needs_rerender();
        }

        ctx.label("colorscheme-label", loc(LocId::PreferencesColorscheme));
        ctx.attr_padding(Rect::three(0, 0, 1));

        for &scheme in ColorScheme::ALL.iter() {
            let selected = state.preferences.colorscheme == scheme;
            let text = arena_format!(
                ctx.arena(),
                "{} {}",
                if selected { "(●)" } else { "(○)" },
                loc(scheme.label_loc())
            );
            if ctx.button(scheme.widget_id(), &text, ButtonStyle::default())
                && state.preferences.colorscheme != scheme
            {
                state.preferences.colorscheme = scheme;
                state.apply_colorscheme_to_context(ctx);
                ctx.needs_rerender();
            }
        }
    }
    ctx.block_end();

    ctx.attr_position(Position::Center);
    if ctx.button("preferences-close", loc(LocId::SearchClose), ButtonStyle::default()) {
        close = true;
    }

    if close || ctx.modal_end() {
        state.wants_preferences = false;
    }
}

impl State {
    pub fn apply_preferences_to_documents(&mut self) {
        let prefs = self.preferences.clone();
        for doc in self.documents.iter_mut() {
            prefs.apply_to_document(doc);
        }
    }

    pub fn set_system_palette(&mut self, palette: [StraightRgba; INDEXED_COLORS_COUNT]) {
        self.system_palette = palette;
    }

    pub fn current_palette(&self) -> [StraightRgba; INDEXED_COLORS_COUNT] {
        self.palette_for_scheme(self.preferences.colorscheme)
    }

    pub fn apply_colorscheme_to_context(&mut self, ctx: &mut Context) {
        ctx.set_color_palette(self.current_palette());
        let (floater_bg, floater_fg) = self.refresh_theme_colors_with(
            |idx| ctx.indexed(idx),
            |idx, n, d| ctx.indexed_alpha(idx, n, d),
            |color| ctx.contrasted(color),
        );
        ctx.set_floater_default_bg(floater_bg);
        ctx.set_floater_default_fg(floater_fg);
        ctx.set_modal_default_bg(floater_bg);
        ctx.set_modal_default_fg(floater_fg);
    }

    pub fn apply_colorscheme_to_tui(&mut self, tui: &mut Tui) {
        tui.setup_indexed_colors(self.current_palette());
        let (floater_bg, floater_fg) = self.refresh_theme_colors_with(
            |idx| tui.indexed(idx),
            |idx, n, d| tui.indexed_alpha(idx, n, d),
            |color| tui.contrasted(color),
        );
        tui.set_floater_default_bg(floater_bg);
        tui.set_floater_default_fg(floater_fg);
        tui.set_modal_default_bg(floater_bg);
        tui.set_modal_default_fg(floater_fg);
    }

    fn refresh_theme_colors_with<F, G, H>(
        &mut self,
        mut indexed: F,
        mut indexed_alpha: G,
        mut contrasted: H,
    ) -> (StraightRgba, StraightRgba)
    where
        F: FnMut(IndexedColor) -> StraightRgba,
        G: FnMut(IndexedColor, u32, u32) -> StraightRgba,
        H: FnMut(StraightRgba) -> StraightRgba,
    {
        self.menubar_color_bg = indexed(IndexedColor::Background).oklab_blend(indexed_alpha(
            IndexedColor::BrightBlue,
            1,
            2,
        ));
        self.menubar_color_fg = contrasted(self.menubar_color_bg);
        let floater_bg = indexed_alpha(IndexedColor::Background, 2, 3).oklab_blend(indexed_alpha(
            IndexedColor::Foreground,
            1,
            3,
        ));
        let floater_fg = contrasted(floater_bg);
        (floater_bg, floater_fg)
    }

    fn palette_for_scheme(&self, scheme: ColorScheme) -> [StraightRgba; INDEXED_COLORS_COUNT] {
        match scheme {
            ColorScheme::System => self.system_palette,
            ColorScheme::Midnight => COLOR_SCHEME_MIDNIGHT,
            ColorScheme::Daylight => COLOR_SCHEME_DAYLIGHT,
            ColorScheme::Nord => COLOR_SCHEME_NORD,
            ColorScheme::HighContrast => COLOR_SCHEME_HIGH_CONTRAST,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ColorScheme {
    System,
    Midnight,
    Daylight,
    Nord,
    HighContrast,
}

impl ColorScheme {
    const ALL: [ColorScheme; 5] = [
        ColorScheme::System,
        ColorScheme::Midnight,
        ColorScheme::Daylight,
        ColorScheme::Nord,
        ColorScheme::HighContrast,
    ];

    fn label_loc(self) -> LocId {
        match self {
            ColorScheme::System => LocId::PreferencesSchemeSystem,
            ColorScheme::Midnight => LocId::PreferencesSchemeMidnight,
            ColorScheme::Daylight => LocId::PreferencesSchemeDaylight,
            ColorScheme::Nord => LocId::PreferencesSchemeNord,
            ColorScheme::HighContrast => LocId::PreferencesSchemeHighContrast,
        }
    }

    fn widget_id(self) -> &'static str {
        match self {
            ColorScheme::System => "scheme-system",
            ColorScheme::Midnight => "scheme-midnight",
            ColorScheme::Daylight => "scheme-daylight",
            ColorScheme::Nord => "scheme-nord",
            ColorScheme::HighContrast => "scheme-high-contrast",
        }
    }
}

#[derive(Clone)]
pub struct Preferences {
    pub auto_close_pairs: bool,
    pub line_highlight: bool,
    pub colorscheme: ColorScheme,
}

impl Default for Preferences {
    fn default() -> Self {
        Self { auto_close_pairs: true, line_highlight: true, colorscheme: ColorScheme::System }
    }
}

impl Preferences {
    fn apply_to_text_buffer(&self, tb: &mut buffer::TextBuffer) {
        tb.set_auto_pair_enabled(self.auto_close_pairs);
        tb.set_line_highlight_enabled(self.line_highlight);
    }

    pub fn apply_to_document(&self, doc: &mut Document) {
        let mut tb = doc.buffer.borrow_mut();
        self.apply_to_text_buffer(&mut tb);
    }
}

const fn rgba(color: u32) -> StraightRgba {
    StraightRgba::from_be(color)
}

const COLOR_SCHEME_MIDNIGHT: [StraightRgba; INDEXED_COLORS_COUNT] = [
    rgba(0x073642ff),
    rgba(0xdc322fff),
    rgba(0x859900ff),
    rgba(0xb58900ff),
    rgba(0x268bd2ff),
    rgba(0xd33682ff),
    rgba(0x2aa198ff),
    rgba(0xeee8d5ff),
    rgba(0x002b36ff),
    rgba(0xcb4b16ff),
    rgba(0x586e75ff),
    rgba(0x657b83ff),
    rgba(0x839496ff),
    rgba(0x6c71c4ff),
    rgba(0x93a1a1ff),
    rgba(0xfdf6e3ff),
    rgba(0x002b36ff),
    rgba(0x839496ff),
];

const COLOR_SCHEME_DAYLIGHT: [StraightRgba; INDEXED_COLORS_COUNT] = [
    rgba(0xeee8d5ff),
    rgba(0xdc322fff),
    rgba(0x859900ff),
    rgba(0xb58900ff),
    rgba(0x268bd2ff),
    rgba(0xd33682ff),
    rgba(0x2aa198ff),
    rgba(0x073642ff),
    rgba(0xfdf6e3ff),
    rgba(0xcb4b16ff),
    rgba(0x93a1a1ff),
    rgba(0x839496ff),
    rgba(0x657b83ff),
    rgba(0x6c71c4ff),
    rgba(0x586e75ff),
    rgba(0x002b36ff),
    rgba(0xfdf6e3ff),
    rgba(0x586e75ff),
];

const COLOR_SCHEME_NORD: [StraightRgba; INDEXED_COLORS_COUNT] = [
    rgba(0x2e3440ff),
    rgba(0xbf616aff),
    rgba(0xa3be8cff),
    rgba(0xebcb8bff),
    rgba(0x81a1c1ff),
    rgba(0xb48eadff),
    rgba(0x88c0d0ff),
    rgba(0xe5e9f0ff),
    rgba(0x3b4252ff),
    rgba(0xbf616aff),
    rgba(0xa3be8cff),
    rgba(0xebcb8bff),
    rgba(0x81a1c1ff),
    rgba(0xb48eadff),
    rgba(0x8fbcbbff),
    rgba(0xeceff4ff),
    rgba(0x2e3440ff),
    rgba(0xe5e9f0ff),
];

const COLOR_SCHEME_HIGH_CONTRAST: [StraightRgba; INDEXED_COLORS_COUNT] = [
    rgba(0x000000ff),
    rgba(0xff5555ff),
    rgba(0x55ff55ff),
    rgba(0xffff55ff),
    rgba(0x5555ffff),
    rgba(0xff55ffff),
    rgba(0x55ffffff),
    rgba(0xffffffff),
    rgba(0x000000ff),
    rgba(0xff0000ff),
    rgba(0x00ff00ff),
    rgba(0xffff00ff),
    rgba(0x0000ffff),
    rgba(0xff00ffff),
    rgba(0x00ffffff),
    rgba(0xffffffff),
    rgba(0x000000ff),
    rgba(0xffffffff),
];
