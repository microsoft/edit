// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::borrow::Cow;
use std::ffi::{OsStr, OsString};
use std::path::{Path, PathBuf};
use std::{env, fs, mem};

use edit::framebuffer::{self, INDEXED_COLORS_COUNT, IndexedColor};
use edit::helpers::*;
use edit::input::vk;
use edit::oklab::StraightRgba;
use edit::tui::*;
use edit::{apperr, arena_format, buffer, icu, path, sys};

use crate::documents::{Document, DocumentManager};
use crate::localization::*;
use crate::{localization, session};

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

const RECENT_FILES_LIMIT: usize = 15;
const SESSION_DOCUMENT_LIMIT: usize = 8;

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
    pub recent_files: Vec<DisplayablePathBuf>,

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
    pub wants_recent_files: bool,
    pub wants_command_palette: bool,
    pub wants_close: bool,
    pub wants_exit: bool,
    pub wants_goto: bool,
    pub goto_target: String,
    pub goto_invalid: bool,

    pub osc_title_file_status: OscTitleFileStatus,
    pub osc_clipboard_sync: bool,
    pub osc_clipboard_always_send: bool,
    pub exit: bool,
    pub skip_session_restore: bool,
    pub preferences: Preferences,
    pub command_palette_filter: String,
    pub command_palette_selection: usize,
    pub command_palette_reset_selection: bool,
    pub command_palette_focus_filter: bool,
    pub preferences_focus_reset: bool,
    system_palette: [StraightRgba; INDEXED_COLORS_COUNT],
}

impl State {
    pub fn new() -> apperr::Result<Self> {
        let preferences = Preferences::load_from_disk();
        preferences.apply_language();
        Ok(Self {
            menubar_color_bg: StraightRgba::zero(),
            menubar_color_fg: StraightRgba::zero(),

            documents: Default::default(),
            recent_files: Vec::new(),

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
            wants_recent_files: false,
            wants_command_palette: false,
            wants_close: false,
            wants_exit: false,
            wants_goto: false,
            goto_target: Default::default(),
            goto_invalid: false,

            osc_title_file_status: Default::default(),
            osc_clipboard_sync: false,
            osc_clipboard_always_send: false,
            exit: false,
            skip_session_restore: false,
            preferences,
            command_palette_filter: String::new(),
            command_palette_selection: 0,
            command_palette_reset_selection: false,
            command_palette_focus_filter: false,
            preferences_focus_reset: false,
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
        macro_rules! take_focus {
            ($ctx:expr) => {
                if state.preferences_focus_reset {
                    $ctx.steal_focus();
                    state.preferences_focus_reset = false;
                }
            };
        }

        ctx.table_begin("preferences-controls");
        ctx.attr_padding(Rect::three(0, 0, 1));
        ctx.inherit_focus();

        ctx.table_next_row();
        ctx.label("general-label", loc(LocId::PreferencesGeneral));
        ctx.attr_padding(Rect::three(0, 0, 1));
        ctx.focus_on_first_present();

        ctx.table_next_row();
        ctx.attr_focusable();
        if ctx.checkbox(
            "pref-auto-close",
            loc(LocId::PreferencesAutoClose),
            &mut state.preferences.auto_close_pairs,
        ) {
            state.apply_preferences_to_documents();
            state.save_preferences();
            ctx.needs_rerender();
        }
        take_focus!(ctx);

        ctx.table_next_row();
        ctx.attr_focusable();
        if ctx.checkbox(
            "pref-line-highlight",
            loc(LocId::PreferencesLineHighlight),
            &mut state.preferences.line_highlight,
        ) {
            state.apply_preferences_to_documents();
            state.save_preferences();
            ctx.needs_rerender();
        }
        take_focus!(ctx);

        ctx.table_next_row();
        ctx.attr_focusable();
        if ctx.checkbox(
            "pref-line-numbers",
            loc(LocId::PreferencesShowLineNumbers),
            &mut state.preferences.show_line_numbers,
        ) {
            state.apply_preferences_to_documents();
            state.save_preferences();
            ctx.needs_rerender();
        }
        take_focus!(ctx);

        ctx.table_next_row();
        ctx.attr_focusable();
        if ctx.checkbox(
            "pref-word-wrap",
            loc(LocId::PreferencesWordWrap),
            &mut state.preferences.word_wrap,
        ) {
            state.apply_preferences_to_documents();
            state.save_preferences();
            ctx.needs_rerender();
        }
        take_focus!(ctx);

        ctx.table_next_row();
        ctx.attr_focusable();
        if ctx.checkbox(
            "pref-indent-tabs",
            loc(LocId::PreferencesIndentWithTabs),
            &mut state.preferences.indent_with_tabs,
        ) {
            state.apply_preferences_to_documents();
            state.save_preferences();
            ctx.needs_rerender();
        }
        take_focus!(ctx);

        ctx.table_next_row();
        ctx.attr_focusable();
        let tab_width_text = arena_format!(
            ctx.arena(),
            "{}: {} (←/→)",
            loc(LocId::PreferencesTabWidth),
            state.preferences.tab_width
        );
        ctx.label("pref-tab-width", &tab_width_text);
        if ctx.is_focused() {
            let mut new_width = state.preferences.tab_width;
            if ctx.consume_shortcut(vk::LEFT) && new_width > 1 {
                new_width -= 1;
            } else if ctx.consume_shortcut(vk::RIGHT) && new_width < 8 {
                new_width += 1;
            }
            if new_width != state.preferences.tab_width {
                state.preferences.tab_width = new_width;
                state.apply_preferences_to_documents();
                state.save_preferences();
                ctx.needs_rerender();
            }
        }
        take_focus!(ctx);

        ctx.table_next_row();
        ctx.label("colorscheme-label", loc(LocId::PreferencesColorscheme));
        ctx.attr_padding(Rect::three(0, 0, 1));

        for &scheme in ColorScheme::ALL.iter() {
            ctx.table_next_row();
            ctx.attr_focusable();
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
                state.save_preferences();
                ctx.needs_rerender();
            }
            take_focus!(ctx);
        }

        ctx.table_next_row();
        ctx.label("spacer-colors-language", " ");
        ctx.attr_padding(Rect::three(0, 0, 1));

        ctx.table_next_row();
        ctx.label("language-label", loc(LocId::PreferencesLanguage));
        ctx.attr_padding(Rect::three(0, 0, 1));

        ctx.table_next_row();
        ctx.attr_focusable();
        let system_selected = state.preferences.language.is_none();
        let system_text = arena_format!(
            ctx.arena(),
            "{} {}",
            if system_selected { "(●)" } else { "(○)" },
            loc(LocId::PreferencesLanguageSystem)
        );
        if ctx.button("language-system", &system_text, ButtonStyle::default()) && !system_selected {
            state.preferences.language = None;
            localization::reset_language();
            state.save_preferences();
            ctx.needs_rerender();
        }
        take_focus!(ctx);

        let mut lang_idx = 0u64;
        for (tag, _) in localization::unique_languages() {
            ctx.table_next_row();
            ctx.attr_focusable();
            ctx.next_block_id_mixin(lang_idx);
            lang_idx += 1;

            let selected = state
                .preferences
                .language
                .as_deref()
                .map_or(false, |saved| lang_tag_eq(saved, tag));
            let pretty_tag = tag.replace('_', "-");
            let text = arena_format!(
                ctx.arena(),
                "{} {} ({})",
                if selected { "(●)" } else { "(○)" },
                localization::language_display_name(tag),
                pretty_tag
            );
            if ctx.button("language-option", &text, ButtonStyle::default()) && !selected {
                if localization::set_language_tag(tag).is_some() {
                    state.preferences.language = Some(tag.to_string());
                    state.save_preferences();
                    ctx.needs_rerender();
                }
            }
            take_focus!(ctx);
        }

        ctx.table_next_row();
        ctx.label("spacer-language-close", " ");
        ctx.attr_padding(Rect::three(0, 0, 1));

        ctx.table_next_row();
        ctx.attr_focusable();
        if ctx.button("preferences-close", loc(LocId::SearchClose), ButtonStyle::default()) {
            close = true;
        }
        take_focus!(ctx);

        ctx.table_end();
    }
    ctx.block_end();

    if close || ctx.modal_end() {
        state.wants_preferences = false;
        state.preferences_focus_reset = false;
    }
}

pub fn draw_recent_files_dialog(ctx: &mut Context, state: &mut State) {
    if state.recent_files.is_empty() {
        state.wants_recent_files = false;
        return;
    }

    let mut close = false;
    let mut open_path: Option<PathBuf> = None;

    ctx.modal_begin("recent-files", loc(LocId::RecentFilesDialogTitle));
    ctx.attr_focus_well();
    ctx.attr_padding(Rect::three(1, 2, 1));
    {
        if ctx.contains_focus() && ctx.consume_shortcut(vk::ESCAPE) {
            close = true;
        }

        ctx.block_begin("recent-list");
        ctx.attr_padding(Rect::three(0, 0, 1));
        for (idx, entry) in state.recent_files.iter().enumerate() {
            ctx.next_block_id_mixin(idx as u64);
            ctx.attr_overflow(Overflow::TruncateTail);
            if ctx.button("recent-entry", entry.as_str(), ButtonStyle::default()) {
                open_path = Some(entry.as_path().to_path_buf());
            }
        }
        ctx.block_end();

        ctx.attr_position(Position::Center);
        if ctx.button("recent-close", loc(LocId::SearchClose), ButtonStyle::default()) {
            close = true;
        }
    }
    if ctx.modal_end() {
        close = true;
    }

    if let Some(path) = open_path {
        let prefs = state.preferences.clone();
        match state.documents.add_file_path(&path) {
            Ok(doc) => {
                prefs.apply_to_document(doc);
                state.mark_file_recent_path(&path);
                state.wants_recent_files = false;
                ctx.needs_rerender();
            }
            Err(err) => error_log_add(ctx, state, err),
        }
        return;
    }

    if close {
        state.wants_recent_files = false;
    }
}

impl State {
    pub fn apply_preferences_to_documents(&mut self) {
        let prefs = self.preferences.clone();
        for doc in self.documents.iter_mut() {
            prefs.apply_to_document(doc);
        }
    }

    pub fn save_preferences(&self) {
        self.preferences.save_to_disk();
    }

    pub fn initialize_session(&mut self) {
        if let Some(session_file) = session::load() {
            self.set_recent_files_from_session(session_file.recent_files);
            if !self.skip_session_restore {
                self.restore_session_documents(&session_file.open_documents);
            }
        }
    }

    pub fn save_session(&self) {
        let mut session_file = session::SessionFile {
            version: session::SESSION_VERSION,
            open_documents: Vec::new(),
            recent_files: Vec::new(),
        };

        for doc in self.documents.iter().take(SESSION_DOCUMENT_LIMIT) {
            if let Some(path) = &doc.path {
                let cursor = doc.buffer.borrow().cursor_logical_pos();
                session_file.open_documents.push(session::SessionDocument {
                    path: path.to_string_lossy().into_owned(),
                    line: cursor.y as i64,
                    column: cursor.x as i64,
                });
            }
        }

        for entry in self.recent_files.iter().take(RECENT_FILES_LIMIT) {
            session_file.recent_files.push(entry.as_str().to_string());
        }

        let _ = session::save(&session_file);
    }

    pub fn mark_file_recent_path<P: AsRef<Path>>(&mut self, path: P) {
        let normalized = path::normalize(path.as_ref());
        if normalized.as_os_str().is_empty() {
            return;
        }
        self.recent_files.retain(|entry| entry.as_path() != normalized.as_path());
        self.recent_files.insert(0, DisplayablePathBuf::from_path(normalized));
        if self.recent_files.len() > RECENT_FILES_LIMIT {
            self.recent_files.truncate(RECENT_FILES_LIMIT);
        }
    }

    fn set_recent_files_from_session(&mut self, entries: Vec<String>) {
        self.recent_files.clear();
        for entry in entries.into_iter().rev() {
            if entry.is_empty() {
                continue;
            }
            self.mark_file_recent_path(PathBuf::from(entry));
        }
    }

    fn restore_session_documents(&mut self, entries: &[session::SessionDocument]) {
        let prefs = self.preferences.clone();
        for entry in entries.iter().rev().take(SESSION_DOCUMENT_LIMIT) {
            if entry.path.is_empty() {
                continue;
            }
            let path = PathBuf::from(&entry.path);
            let mut opened = false;
            if let Ok(doc) = self.documents.add_file_path(&path) {
                opened = true;
                prefs.apply_to_document(doc);
                {
                    let mut tb = doc.buffer.borrow_mut();
                    let target = Point { x: clamp_coord(entry.column), y: clamp_coord(entry.line) };
                    tb.cursor_move_to_logical(target);
                }
            }
            if opened {
                self.mark_file_recent_path(&path);
            }
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
            ColorScheme::GruvboxDark => COLOR_SCHEME_GRUVBOX_DARK,
            ColorScheme::GruvboxLight => COLOR_SCHEME_GRUVBOX_LIGHT,
            ColorScheme::Dracula => COLOR_SCHEME_DRACULA,
            ColorScheme::Kanagawa => COLOR_SCHEME_KANAGAWA,
            ColorScheme::Tokyonight => COLOR_SCHEME_TOKYONIGHT,
            ColorScheme::Monokai => COLOR_SCHEME_MONOKAI,
            ColorScheme::AtomOneDark => COLOR_SCHEME_ATOM_ONE_DARK,
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
    GruvboxDark,
    GruvboxLight,
    Dracula,
    Kanagawa,
    Tokyonight,
    Monokai,
    AtomOneDark,
}

impl ColorScheme {
    const ALL: [ColorScheme; 12] = [
        ColorScheme::System,
        ColorScheme::Midnight,
        ColorScheme::Daylight,
        ColorScheme::Nord,
        ColorScheme::HighContrast,
        ColorScheme::GruvboxDark,
        ColorScheme::GruvboxLight,
        ColorScheme::Dracula,
        ColorScheme::Kanagawa,
        ColorScheme::Tokyonight,
        ColorScheme::Monokai,
        ColorScheme::AtomOneDark,
    ];

    fn label_loc(self) -> LocId {
        match self {
            ColorScheme::System => LocId::PreferencesSchemeSystem,
            ColorScheme::Midnight => LocId::PreferencesSchemeMidnight,
            ColorScheme::Daylight => LocId::PreferencesSchemeDaylight,
            ColorScheme::Nord => LocId::PreferencesSchemeNord,
            ColorScheme::HighContrast => LocId::PreferencesSchemeHighContrast,
            ColorScheme::GruvboxDark => LocId::PreferencesSchemeGruvboxDark,
            ColorScheme::GruvboxLight => LocId::PreferencesSchemeGruvboxLight,
            ColorScheme::Dracula => LocId::PreferencesSchemeDracula,
            ColorScheme::Kanagawa => LocId::PreferencesSchemeKanagawa,
            ColorScheme::Tokyonight => LocId::PreferencesSchemeTokyonight,
            ColorScheme::Monokai => LocId::PreferencesSchemeMonokai,
            ColorScheme::AtomOneDark => LocId::PreferencesSchemeAtom,
        }
    }

    fn widget_id(self) -> &'static str {
        match self {
            ColorScheme::System => "scheme-system",
            ColorScheme::Midnight => "scheme-midnight",
            ColorScheme::Daylight => "scheme-daylight",
            ColorScheme::Nord => "scheme-nord",
            ColorScheme::HighContrast => "scheme-high-contrast",
            ColorScheme::GruvboxDark => "scheme-gruvbox-dark",
            ColorScheme::GruvboxLight => "scheme-gruvbox-light",
            ColorScheme::Dracula => "scheme-dracula",
            ColorScheme::Kanagawa => "scheme-kanagawa",
            ColorScheme::Tokyonight => "scheme-tokyonight",
            ColorScheme::Monokai => "scheme-monokai",
            ColorScheme::AtomOneDark => "scheme-atom-one-dark",
        }
    }

    fn as_str(self) -> &'static str {
        match self {
            ColorScheme::System => "system",
            ColorScheme::Midnight => "midnight",
            ColorScheme::Daylight => "daylight",
            ColorScheme::Nord => "nord",
            ColorScheme::HighContrast => "high_contrast",
            ColorScheme::GruvboxDark => "gruvbox_dark",
            ColorScheme::GruvboxLight => "gruvbox_light",
            ColorScheme::Dracula => "dracula",
            ColorScheme::Kanagawa => "kanagawa",
            ColorScheme::Tokyonight => "tokyonight",
            ColorScheme::Monokai => "monokai",
            ColorScheme::AtomOneDark => "atom_one_dark",
        }
    }

    fn from_str(value: &str) -> Option<Self> {
        match value.to_ascii_lowercase().as_str() {
            "system" => Some(ColorScheme::System),
            "midnight" => Some(ColorScheme::Midnight),
            "daylight" => Some(ColorScheme::Daylight),
            "nord" => Some(ColorScheme::Nord),
            "high_contrast" | "high-contrast" => Some(ColorScheme::HighContrast),
            "gruvbox_dark" | "gruvbox-dark" => Some(ColorScheme::GruvboxDark),
            "gruvbox_light" | "gruvbox-light" => Some(ColorScheme::GruvboxLight),
            "dracula" => Some(ColorScheme::Dracula),
            "kanagawa" => Some(ColorScheme::Kanagawa),
            "tokyonight" | "tokyo_night" | "tokyo-night" => Some(ColorScheme::Tokyonight),
            "monokai" => Some(ColorScheme::Monokai),
            "atom_one_dark" | "atom-one-dark" | "atom" => Some(ColorScheme::AtomOneDark),
            _ => None,
        }
    }
}

#[derive(Clone)]
pub struct Preferences {
    pub auto_close_pairs: bool,
    pub line_highlight: bool,
    pub colorscheme: ColorScheme,
    pub show_line_numbers: bool,
    pub word_wrap: bool,
    pub indent_with_tabs: bool,
    pub tab_width: u8,
    pub language: Option<String>,
}

impl Default for Preferences {
    fn default() -> Self {
        Self {
            auto_close_pairs: true,
            line_highlight: true,
            colorscheme: ColorScheme::System,
            show_line_numbers: true,
            word_wrap: false,
            indent_with_tabs: false,
            tab_width: 4,
            language: None,
        }
    }
}

impl Preferences {
    fn apply_to_text_buffer(&self, tb: &mut buffer::TextBuffer) {
        tb.set_auto_pair_enabled(self.auto_close_pairs);
        tb.set_line_highlight_enabled(self.line_highlight);
        tb.set_margin_enabled(self.show_line_numbers);
        tb.set_word_wrap(self.word_wrap);
        tb.set_indent_with_tabs(self.indent_with_tabs);
        tb.set_tab_size(CoordType::from(self.tab_width));
    }

    pub fn apply_to_document(&self, doc: &mut Document) {
        let mut tb = doc.buffer.borrow_mut();
        self.apply_to_text_buffer(&mut tb);
    }

    fn load_from_disk() -> Self {
        let Some(path) = preferences_file_path() else {
            return Self::default();
        };
        let Ok(text) = fs::read_to_string(path) else {
            return Self::default();
        };
        let mut prefs = Preferences::default();
        for line in text.lines() {
            let line = line.trim();
            if line.is_empty() || line.starts_with('#') {
                continue;
            }
            let Some((key, value)) = line.split_once('=') else {
                continue;
            };
            let key = key.trim();
            let value = value.trim();
            match key {
                "auto_close_pairs" => {
                    if let Some(val) = parse_bool(value) {
                        prefs.auto_close_pairs = val;
                    }
                }
                "line_highlight" => {
                    if let Some(val) = parse_bool(value) {
                        prefs.line_highlight = val;
                    }
                }
                "colorscheme" => {
                    if let Some(val) = ColorScheme::from_str(value) {
                        prefs.colorscheme = val;
                    }
                }
                "show_line_numbers" => {
                    if let Some(val) = parse_bool(value) {
                        prefs.show_line_numbers = val;
                    }
                }
                "word_wrap" => {
                    if let Some(val) = parse_bool(value) {
                        prefs.word_wrap = val;
                    }
                }
                "indent_with_tabs" => {
                    if let Some(val) = parse_bool(value) {
                        prefs.indent_with_tabs = val;
                    }
                }
                "tab_width" => {
                    if let Some(val) = parse_u8_in_range(value, 1, 8) {
                        prefs.tab_width = val;
                    }
                }
                "language" => {
                    let value = value.trim();
                    if value.is_empty() {
                        prefs.language = None;
                    } else {
                        prefs.language = Some(value.to_string());
                    }
                }
                _ => {}
            }
        }
        prefs
    }

    fn save_to_disk(&self) {
        let Some(path) = preferences_file_path() else {
            return;
        };
        if let Some(parent) = path.parent() {
            if fs::create_dir_all(parent).is_err() {
                return;
            }
        }
        let contents = format!(
            "auto_close_pairs={}\n\
             line_highlight={}\n\
             colorscheme={}\n\
             show_line_numbers={}\n\
             word_wrap={}\n\
             indent_with_tabs={}\n\
             tab_width={}\n\
             language={}\n",
            self.auto_close_pairs,
            self.line_highlight,
            self.colorscheme.as_str(),
            self.show_line_numbers,
            self.word_wrap,
            self.indent_with_tabs,
            self.tab_width,
            self.language.as_deref().unwrap_or(""),
        );
        let _ = fs::write(path, contents);
    }

    fn apply_language(&self) {
        if let Some(lang) = &self.language {
            if localization::set_language_tag(lang).is_none() {
                // If the stored tag is invalid, fall back to system default.
                localization::reset_language();
            }
        }
    }
}

fn normalize_lang_tag_value(tag: &str) -> String {
    tag.chars()
        .map(|c| match c {
            'A'..='Z' => c.to_ascii_lowercase(),
            '-' => '_',
            _ => c,
        })
        .collect()
}

fn lang_tag_eq(a: &str, b: &str) -> bool {
    normalize_lang_tag_value(a) == normalize_lang_tag_value(b)
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

const COLOR_SCHEME_GRUVBOX_DARK: [StraightRgba; INDEXED_COLORS_COUNT] = [
    rgba(0x282828ff),
    rgba(0xcc241dff),
    rgba(0x98971aff),
    rgba(0xd79921ff),
    rgba(0x458588ff),
    rgba(0xb16286ff),
    rgba(0x689d6aff),
    rgba(0xa89984ff),
    rgba(0x928374ff),
    rgba(0xfb4934ff),
    rgba(0xb8bb26ff),
    rgba(0xfabd2fff),
    rgba(0x83a598ff),
    rgba(0xd3869bff),
    rgba(0x8ec07cff),
    rgba(0xebdbb2ff),
    rgba(0x282828ff),
    rgba(0xebdbb2ff),
];

const COLOR_SCHEME_GRUVBOX_LIGHT: [StraightRgba; INDEXED_COLORS_COUNT] = [
    rgba(0xfbf1c7ff),
    rgba(0xcc241dff),
    rgba(0x98971aff),
    rgba(0xd79921ff),
    rgba(0x458588ff),
    rgba(0xb16286ff),
    rgba(0x689d6aff),
    rgba(0x7c6f64ff),
    rgba(0x928374ff),
    rgba(0x9d0006ff),
    rgba(0x79740eff),
    rgba(0xb57614ff),
    rgba(0x076678ff),
    rgba(0x8f3f71ff),
    rgba(0x427b58ff),
    rgba(0x3c3836ff),
    rgba(0xfbf1c7ff),
    rgba(0x3c3836ff),
];

const COLOR_SCHEME_DRACULA: [StraightRgba; INDEXED_COLORS_COUNT] = [
    rgba(0x282a36ff),
    rgba(0xff5555ff),
    rgba(0x50fa7bff),
    rgba(0xf1fa8cff),
    rgba(0x6272a4ff),
    rgba(0xff79c6ff),
    rgba(0x8be9fdff),
    rgba(0xf8f8f2ff),
    rgba(0x44475aff),
    rgba(0xff6e6eff),
    rgba(0x69ff94ff),
    rgba(0xffffa5ff),
    rgba(0xbd93f9ff),
    rgba(0xff92dfff),
    rgba(0xa4ffffff),
    rgba(0xffffffff),
    rgba(0x282a36ff),
    rgba(0xf8f8f2ff),
];

const COLOR_SCHEME_KANAGAWA: [StraightRgba; INDEXED_COLORS_COUNT] = [
    rgba(0x1f1f28ff),
    rgba(0xc34043ff),
    rgba(0x76946aff),
    rgba(0xc0a36eff),
    rgba(0x7e9cd8ff),
    rgba(0x957fb8ff),
    rgba(0x6a9589ff),
    rgba(0xdcd7baff),
    rgba(0x2a2a37ff),
    rgba(0xe82424ff),
    rgba(0x98bb6cff),
    rgba(0xe6c384ff),
    rgba(0x7fb4caff),
    rgba(0x938aa9ff),
    rgba(0x7aa89fff),
    rgba(0xf2ecbcff),
    rgba(0x1f1f28ff),
    rgba(0xdcd7baff),
];

const COLOR_SCHEME_TOKYONIGHT: [StraightRgba; INDEXED_COLORS_COUNT] = [
    rgba(0x1a1b26ff),
    rgba(0xf7768eff),
    rgba(0x9ece6aff),
    rgba(0xe0af68ff),
    rgba(0x7aa2f7ff),
    rgba(0xbb9af7ff),
    rgba(0x7dcfffff),
    rgba(0xc0caf5ff),
    rgba(0x24283bff),
    rgba(0xff9e64ff),
    rgba(0xc3e88dff),
    rgba(0xffc777ff),
    rgba(0x82aaffff),
    rgba(0xc5a3ffff),
    rgba(0x89dcebff),
    rgba(0xe5e9f0ff),
    rgba(0x1a1b26ff),
    rgba(0xc0caf5ff),
];

const COLOR_SCHEME_MONOKAI: [StraightRgba; INDEXED_COLORS_COUNT] = [
    rgba(0x272822ff),
    rgba(0xf92672ff),
    rgba(0xa6e22eff),
    rgba(0xf4bf75ff),
    rgba(0x66d9efff),
    rgba(0xae81ffff),
    rgba(0xa1efe4ff),
    rgba(0xf9f8f5ff),
    rgba(0x383830ff),
    rgba(0xfd5ff1ff),
    rgba(0xb6e354ff),
    rgba(0xfbe760ff),
    rgba(0x9effffff),
    rgba(0xc4a3ffff),
    rgba(0xaffff8ff),
    rgba(0xffffffff),
    rgba(0x272822ff),
    rgba(0xf9f8f5ff),
];

const COLOR_SCHEME_ATOM_ONE_DARK: [StraightRgba; INDEXED_COLORS_COUNT] = [
    rgba(0x282c34ff),
    rgba(0xe06c75ff),
    rgba(0x98c379ff),
    rgba(0xe5c07bff),
    rgba(0x61afefff),
    rgba(0xc678ddff),
    rgba(0x56b6c2ff),
    rgba(0xabb2bfff),
    rgba(0x323842ff),
    rgba(0xff7b86ff),
    rgba(0xb1d196ff),
    rgba(0xffd689ff),
    rgba(0x73c8ffff),
    rgba(0xd7a1ffff),
    rgba(0x65d4d5ff),
    rgba(0xe6eaf3ff),
    rgba(0x282c34ff),
    rgba(0xabb2bfff),
];

pub(crate) fn config_dir() -> Option<PathBuf> {
    let base = if cfg!(windows) {
        env::var_os("APPDATA").map(PathBuf::from)
    } else {
        env::var_os("XDG_CONFIG_HOME")
            .map(PathBuf::from)
            .or_else(|| env::var_os("HOME").map(|home| PathBuf::from(home).join(".config")))
    }?;

    #[cfg(windows)]
    let subdir = PathBuf::from("Microsoft").join("Edit");
    #[cfg(not(windows))]
    let subdir = PathBuf::from("edit");

    Some(base.join(subdir))
}

fn preferences_file_path() -> Option<PathBuf> {
    config_dir().map(|dir| dir.join("preferences.toml"))
}

fn parse_bool(value: &str) -> Option<bool> {
    match value.to_ascii_lowercase().as_str() {
        "1" | "true" | "yes" | "on" => Some(true),
        "0" | "false" | "no" | "off" => Some(false),
        _ => None,
    }
}

fn parse_u8_in_range(value: &str, min: u8, max: u8) -> Option<u8> {
    value.parse::<u8>().ok().map(|v| v.clamp(min, max))
}

fn clamp_coord(value: i64) -> CoordType {
    value.clamp(0, isize::MAX as i64) as CoordType
}
