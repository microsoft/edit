// TODO:
// * Find & Replace doesn't find all matches.
// * Find & Replace All, followed by Ctrl+Z breaks the buffer.
// * Rapid clicking on buttons is recognized as double clicks.
// * And of course the word wrap dilemma.
// * Global shortcuts should not trigger when there are modal dialogs.
// --------------------------------------------------
// * Backspace at an indented line start should unindent by 1.
// * ...same for Shift+Tab.
// * Goto line
// * grid layout
// * Output diffing / compression
// * BUG: When word-wrap is enabled, insert a tab into the last word of the first of a wrapped line.
//   The entire word will wrap with the tab in the middle, as if the tab is not whitespace.
// * This would allow us to skip computing the stats.visual_lines,
//   because we could simply scroll by the number of logical lines.
// * Word wrapping could not be part of ucd, but rather get computed
//   lazily by the render function. This trivializes everything else.
// * Multi-Cursor
// * For the focus path we can use the tree depth to O(1) check if the path contains the focus.

use edit::buffer::{self, RcTextBuffer};
use edit::framebuffer::{self, IndexedColor};
use edit::helpers::*;
use edit::input::{self, kbmod, vk};
use edit::loc::{LocId, loc};
use edit::sys;
use edit::tui::*;
use edit::vt::{self, Token};
use edit::{apperr, icu};
use std::fs::File;
use std::path::{Component, Path, PathBuf};
use std::{cmp, process};

#[cfg(feature = "debug-latency")]
use std::fmt::Write;

struct StateSearch {
    kind: StateSearchKind,
    focus: bool,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum StateSearchKind {
    Hidden,
    Disabled,
    Search,
    Replace,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum StateFilePicker {
    None,
    Open,
    Save,
    SaveAs,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum StateEncodingChange {
    None,
    Convert,
    Reopen,
}

struct State {
    path: Option<PathBuf>,
    filename: String,
    buffer: RcTextBuffer,

    // A ring buffer of the last 10 errors.
    error_log: [String; 10],
    error_log_index: usize,
    error_log_count: usize,

    wants_file_picker: StateFilePicker,
    file_picker_pending_dir: DisplayablePathBuf,
    file_picker_pending_name: String, // This could be PathBuf, if `tui` would expose its TextBuffer for editline.
    file_picker_entries: Option<Vec<DisplayablePathBuf>>,
    file_picker_overwrite_warning: Option<PathBuf>, // The path the warning is about.

    wants_search: StateSearch,
    search_needle: String,
    search_replacement: String,
    search_options: buffer::SearchOptions,
    search_success: bool,

    wants_encoding_focus: bool,
    wants_encoding_change: StateEncodingChange,
    wants_indentation_focus: bool,
    wants_about: bool,
    wants_exit: bool,
    exit: bool,
}

impl State {
    fn new() -> apperr::Result<Self> {
        let mut buffer = RcTextBuffer::new(false)?;
        buffer.set_margin_enabled(true);
        buffer.set_line_highlight_enabled(true);

        Ok(Self {
            path: None,
            filename: String::new(),
            buffer,

            error_log: [const { String::new() }; 10],
            error_log_index: 0,
            error_log_count: 0,

            wants_file_picker: StateFilePicker::None,
            // TODO: Ideally this would use the directory of the given file path.
            file_picker_pending_dir: DisplayablePathBuf::new(std::env::current_dir()?),
            file_picker_pending_name: String::new(),
            file_picker_entries: None,
            file_picker_overwrite_warning: None,

            wants_search: StateSearch {
                kind: StateSearchKind::Hidden,
                focus: false,
            },
            search_needle: String::new(),
            search_replacement: String::new(),
            search_options: buffer::SearchOptions::default(),
            search_success: true,

            wants_encoding_focus: false,
            wants_encoding_change: StateEncodingChange::None,
            wants_indentation_focus: false,
            wants_about: false,
            wants_exit: false,
            exit: false,
        })
    }

    fn set_path(&mut self, path: PathBuf, filename: String) {
        let ruler = if filename == "COMMIT_EDITMSG" { 72 } else { 0 };
        self.buffer.set_ruler(ruler);
        self.filename = filename;
        self.path = Some(path);
    }
}

fn main() -> process::ExitCode {
    if cfg!(debug_assertions) {
        let hook = std::panic::take_hook();
        std::panic::set_hook(Box::new(move |info| {
            drop(RestoreModes);
            sys::deinit();
            hook(info);
        }));
    }

    let code = match run() {
        Ok(()) => process::ExitCode::SUCCESS,
        Err(err) => {
            let mut msg = err.message();
            msg.push_str("\r\n");
            sys::write_stdout(&msg);
            process::ExitCode::FAILURE
        }
    };
    sys::deinit();
    code
}

fn run() -> apperr::Result<()> {
    sys::init()?;

    let mut state = State::new()?;
    let mut vt_parser = vt::Parser::new();
    let mut input_parser = input::Parser::new();
    let mut tui = Tui::new();

    if let Some(path) = std::env::args_os()
        .nth(1)
        .and_then(|p| if p == "-" { None } else { Some(p) })
        .map(PathBuf::from)
    {
        let filename = get_filename_from_path(&path);
        if !filename.is_empty() {
            match file_open(&path) {
                Ok(mut file) => state.buffer.read_file(&mut file, None)?,
                Err(err) if sys::apperr_is_not_found(err) => {}
                Err(err) => return Err(err),
            }
        }
        state.set_path(path, filename);
    } else if let Some(mut file) = sys::open_stdin_if_redirected() {
        state.buffer.read_file(&mut file, None)?;
        state.buffer.mark_as_dirty();
    }

    let _restore_modes = set_modes();
    query_color_palette(&mut tui, &mut vt_parser);
    sys::inject_window_size_into_stdin();

    #[cfg(feature = "debug-latency")]
    let mut last_latency_width = 0;

    loop {
        let read_timeout = vt_parser.read_timeout().min(tui.read_timeout());
        let Some(input) = sys::read_stdin(read_timeout) else {
            break;
        };

        #[cfg(feature = "debug-latency")]
        let time_beg = std::time::Instant::now();
        #[cfg(feature = "debug-latency")]
        let mut passes = 0usize;

        let vt_iter = vt_parser.parse(&input);
        let mut input_iter = input_parser.parse(vt_iter);

        // Process all input.
        while {
            let input = input_iter.next();
            let more = input.is_some();
            let mut ctx = tui.create_context(input);

            draw(&mut ctx, &mut state);

            #[cfg(feature = "debug-latency")]
            {
                passes += 1;
            }

            more
        } {}

        // Continue rendering until the layout has settled.
        // This can take >1 frame, if the input focus is tossed between different controls.
        while tui.needs_settling() {
            let mut ctx = tui.create_context(None);

            draw(&mut ctx, &mut state);

            #[cfg(feature = "debug-layout")]
            {
                drop(ctx);
                state.buffer.debug_replace_everything(&tui.debug_layout());
            }

            #[cfg(feature = "debug-latency")]
            {
                passes += 1;
            }
        }

        if state.exit {
            break;
        }

        #[cfg(feature = "debug-latency")]
        {
            let mut output = tui.render();

            // Print the number of passes and latency in the top right corner.
            let time_end = std::time::Instant::now();
            let status = time_end - time_beg;
            let status = format!("{}x {:.3}μs", passes, status.as_nanos() as f64 / 1000.0);

            // "μs" is 3 bytes and 2 columns.
            let cols = status.len() as i32 - 3 + 2;

            // Since the status may shrink and grow, we may have to overwrite the previous one with whitespace.
            let padding = (last_latency_width - cols).max(0);

            // To avoid moving the cursor, push and pop it onto the VT cursor stack.
            _ = write!(
                output,
                "\x1b7\x1b[1;{0}H{1:2$}{3}\x1b8",
                tui.size().width - cols - padding + 1,
                "",
                padding as usize,
                status
            );

            last_latency_width = cols;
            sys::write_stdout(&output);
        }
        #[cfg(not(feature = "debug-latency"))]
        {
            let output = tui.render();
            sys::write_stdout(&output);
        }
    }

    Ok(())
}

fn draw(ctx: &mut Context, state: &mut State) {
    let root_focused = ctx.contains_focus();

    draw_menubar(ctx, state);
    if state.wants_search.kind != StateSearchKind::Hidden {
        draw_search(ctx, state);
    }
    draw_editor(ctx, state);
    draw_statusbar(ctx, state);

    if state.wants_file_picker != StateFilePicker::None {
        draw_file_picker(ctx, state);
    }

    if state.wants_file_picker == StateFilePicker::Save {
        draw_handle_save(ctx, state, None);
    }

    if state.wants_encoding_change != StateEncodingChange::None {
        draw_dialog_encoding_change(ctx, state);
    }

    // If the user presses "Save" on the exit dialog we'll possible show a SaveAs dialog.
    // The exit dialog should then get hidden.
    if state.wants_exit && state.wants_file_picker == StateFilePicker::None {
        draw_handle_wants_exit(ctx, state);
    }

    if state.wants_about {
        draw_dialog_about(ctx, state);
    }

    if state.error_log_count != 0 {
        draw_error_log(ctx, state);
    }

    if root_focused {
        // Shortcuts that are not handled as part of the textarea, etc.
        if ctx.consume_shortcut(kbmod::CTRL | vk::O) {
            state.wants_file_picker = StateFilePicker::Open;
        }
        if ctx.consume_shortcut(kbmod::CTRL | vk::S) {
            state.wants_file_picker = StateFilePicker::Save;
        }
        if ctx.consume_shortcut(kbmod::CTRL_SHIFT | vk::S) {
            state.wants_file_picker = StateFilePicker::SaveAs;
        }
        if ctx.consume_shortcut(kbmod::CTRL | vk::Q) {
            state.wants_exit = true;
        }
        if state.wants_search.kind != StateSearchKind::Disabled {
            if ctx.consume_shortcut(kbmod::CTRL | vk::F) {
                state.wants_search.kind = StateSearchKind::Search;
                state.wants_search.focus = true;
            }
            if ctx.consume_shortcut(kbmod::CTRL | vk::H) {
                state.wants_search.kind = StateSearchKind::Replace;
                state.wants_search.focus = true;
            }
        }
    }
}

fn draw_menubar(ctx: &mut Context, state: &mut State) {
    ctx.menubar_begin();
    ctx.attr_background_rgba(ctx.indexed(IndexedColor::White));
    ctx.attr_foreground_rgba(ctx.indexed(IndexedColor::Black));
    {
        if ctx.menubar_menu_begin(loc(LocId::File), 'F') {
            draw_menu_file(ctx, state);
        }
        if ctx.menubar_menu_begin(loc(LocId::Edit), 'E') {
            draw_menu_edit(ctx, state);
        }
        if ctx.menubar_menu_begin(loc(LocId::View), 'V') {
            draw_menu_view(ctx, state);
        }
        if ctx.menubar_menu_begin(loc(LocId::Help), 'H') {
            draw_menu_help(ctx, state);
        }
    }
    ctx.menubar_end();
}

fn draw_menu_file(ctx: &mut Context, state: &mut State) {
    if ctx.menubar_menu_item(loc(LocId::FileOpen), 'O', kbmod::CTRL | vk::O) {
        state.wants_file_picker = StateFilePicker::Open;
    }
    if ctx.menubar_menu_item(loc(LocId::FileSave), 'S', kbmod::CTRL | vk::S) {
        state.wants_file_picker = StateFilePicker::Save;
    }
    if ctx.menubar_menu_item(loc(LocId::FileSaveAs), 'A', vk::NULL) {
        state.wants_file_picker = StateFilePicker::SaveAs;
    }
    if ctx.menubar_menu_item(loc(LocId::FileExit), 'X', kbmod::CTRL | vk::Q) {
        state.wants_exit = true;
    }
    ctx.menubar_menu_end();
}

fn draw_menu_edit(ctx: &mut Context, state: &mut State) {
    if ctx.menubar_menu_item(loc(LocId::EditUndo), 'U', kbmod::CTRL | vk::Z) {
        state.buffer.undo();
        ctx.needs_rerender();
    }
    if ctx.menubar_menu_item(loc(LocId::EditRedo), 'R', kbmod::CTRL | vk::Y) {
        state.buffer.redo();
        ctx.needs_rerender();
    }
    if ctx.menubar_menu_item(loc(LocId::EditCut), 'T', kbmod::CTRL | vk::X) {
        ctx.set_clipboard(state.buffer.extract_selection(true));
    }
    if ctx.menubar_menu_item(loc(LocId::EditCopy), 'C', kbmod::CTRL | vk::C) {
        ctx.set_clipboard(state.buffer.extract_selection(false));
    }
    if ctx.menubar_menu_item(loc(LocId::EditPaste), 'P', kbmod::CTRL | vk::V) {
        state.buffer.write(ctx.get_clipboard(), true);
        ctx.needs_rerender();
    }
    if state.wants_search.kind != StateSearchKind::Disabled {
        if ctx.menubar_menu_item(loc(LocId::EditFind), 'F', kbmod::CTRL | vk::F) {
            state.wants_search.kind = StateSearchKind::Search;
            state.wants_search.focus = true;
        }
        if ctx.menubar_menu_item(loc(LocId::EditReplace), 'R', kbmod::CTRL | vk::H) {
            state.wants_search.kind = StateSearchKind::Replace;
            state.wants_search.focus = true;
        }
    }
    if ctx.menubar_menu_item(loc(LocId::EditChangeNewlineSequence), 'N', vk::NULL) {
        let crlf = state.buffer.is_crlf();
        state.buffer.normalize_newlines(!crlf);
        ctx.needs_rerender();
    }
    if ctx.menubar_menu_item(loc(LocId::EditChangeEncoding), 'E', vk::NULL) {
        state.wants_encoding_focus = true;
    }
    if ctx.menubar_menu_item(loc(LocId::EditChangeIndentation), 'I', vk::NULL) {
        state.wants_indentation_focus = true;
    }
    ctx.menubar_menu_end();
}

fn draw_menu_view(ctx: &mut Context, state: &mut State) {
    if ctx.menubar_menu_item(loc(LocId::ViewWordWrap), 'W', kbmod::ALT | vk::Z) {
        state.buffer.toggle_word_wrap();
        ctx.needs_rerender();
    }
    ctx.menubar_menu_end();
}

fn draw_menu_help(ctx: &mut Context, state: &mut State) {
    if ctx.menubar_menu_item(loc(LocId::HelpAbout), 'A', vk::NULL) {
        state.wants_about = true;
    }
    ctx.menubar_menu_end();
}

fn draw_search(ctx: &mut Context, state: &mut State) {
    enum SearchAction {
        None,
        Search,
        Replace,
        ReplaceAll,
    }

    if let Err(err) = icu::init() {
        error_log_add(ctx, state, err);
        state.wants_search.kind = StateSearchKind::Disabled;
        return;
    }

    let mut action = SearchAction::None;
    let mut focus = StateSearchKind::Hidden;

    if state.wants_search.focus {
        state.wants_search.focus = false;
        focus = StateSearchKind::Search;

        // If the selection is empty, focus the search input field.
        // Otherwise, focus the replace input field, if it exists.
        if state.buffer.has_selection() {
            let selection = state.buffer.extract_selection(false);
            let selection = string_from_utf8_lossy_owned(selection);
            state.search_needle = selection;
            focus = state.wants_search.kind;
        }
    }

    ctx.block_begin("search");
    ctx.attr_focus_well();
    ctx.attr_background_rgba(ctx.indexed(IndexedColor::White));
    ctx.attr_foreground_rgba(ctx.indexed(IndexedColor::Black));
    {
        if ctx.contains_focus() && ctx.consume_shortcut(vk::ESCAPE) {
            state.wants_search.kind = StateSearchKind::Hidden;
        }

        ctx.table_begin("needle");
        ctx.table_set_cell_gap(Size {
            width: 1,
            height: 0,
        });
        {
            {
                ctx.table_next_row();
                ctx.label("label", Overflow::Clip, loc(LocId::SearchNeedleLabel));

                if ctx.editline("needle", &mut state.search_needle) {
                    action = SearchAction::Search;
                }
                if !state.search_success {
                    ctx.attr_background_rgba(ctx.indexed(IndexedColor::Red));
                    ctx.attr_foreground_rgba(ctx.indexed(IndexedColor::BrightWhite));
                }
                ctx.attr_intrinsic_size(Size {
                    width: COORD_TYPE_SAFE_MAX,
                    height: 1,
                });
                if focus == StateSearchKind::Search {
                    ctx.steal_focus();
                }
                if ctx.is_focused() && ctx.consume_shortcut(vk::RETURN) {
                    action = SearchAction::Search;
                }
            }

            if state.wants_search.kind == StateSearchKind::Replace {
                ctx.table_next_row();
                ctx.label("label", Overflow::Clip, loc(LocId::SearchReplacementLabel));

                ctx.editline("replacement", &mut state.search_replacement);
                ctx.attr_intrinsic_size(Size {
                    width: COORD_TYPE_SAFE_MAX,
                    height: 1,
                });
                if focus == StateSearchKind::Replace {
                    ctx.steal_focus();
                }
                if ctx.is_focused() {
                    if ctx.consume_shortcut(vk::RETURN) {
                        action = SearchAction::Replace;
                    } else if ctx.consume_shortcut(kbmod::CTRL_ALT | vk::RETURN) {
                        action = SearchAction::ReplaceAll;
                    }
                }
            }
        }
        ctx.table_end();

        ctx.table_begin("options");
        ctx.table_set_cell_gap(Size {
            width: 2,
            height: 0,
        });
        {
            ctx.table_next_row();

            let mut change = false;
            change |= ctx.checkbox(
                "match-case",
                Overflow::Clip,
                loc(LocId::SearchMatchCase),
                &mut state.search_options.match_case,
            );
            change |= ctx.checkbox(
                "whole-word",
                Overflow::Clip,
                loc(LocId::SearchWholeWord),
                &mut state.search_options.whole_word,
            );
            change |= ctx.checkbox(
                "use-regex",
                Overflow::Clip,
                loc(LocId::SearchUseRegex),
                &mut state.search_options.use_regex,
            );
            if change {
                action = SearchAction::Search;
                state.wants_search.focus = true;
                ctx.needs_rerender();
            }

            if state.wants_search.kind == StateSearchKind::Replace
                && ctx.button("replace-all", Overflow::Clip, loc(LocId::SearchReplaceAll))
            {
                action = SearchAction::ReplaceAll;
            }

            if ctx.button("close", Overflow::Clip, loc(LocId::SearchClose)) {
                state.wants_search.kind = StateSearchKind::Hidden;
            }
        }
        ctx.table_end();
    }
    ctx.block_end();

    state.search_success = match action {
        SearchAction::None => return,
        SearchAction::Search => state
            .buffer
            .find_and_select(&state.search_needle, state.search_options),
        SearchAction::Replace => state.buffer.find_and_replace(
            &state.search_needle,
            state.search_options,
            &state.search_replacement,
        ),
        SearchAction::ReplaceAll => state.buffer.find_and_replace_all(
            &state.search_needle,
            state.search_options,
            &state.search_replacement,
        ),
    }
    .is_ok();

    ctx.needs_rerender();
}

fn draw_editor(ctx: &mut Context, state: &mut State) {
    let size = ctx.size();
    // TODO: The layout code should be able to just figure out the height on its own.
    let height_reduction = match state.wants_search.kind {
        StateSearchKind::Search => 4,
        StateSearchKind::Replace => 5,
        _ => 2,
    };

    ctx.textarea("textarea", state.buffer.clone());
    ctx.inherit_focus();
    ctx.attr_intrinsic_size(Size {
        width: 0,
        height: size.height - height_reduction,
    });
}

fn draw_statusbar(ctx: &mut Context, state: &mut State) {
    ctx.table_begin("statusbar");
    ctx.attr_background_rgba(ctx.indexed(IndexedColor::White));
    ctx.attr_foreground_rgba(ctx.indexed(IndexedColor::Black));
    ctx.table_set_cell_gap(Size {
        width: 2,
        height: 0,
    });
    ctx.attr_padding(Rect::two(0, 1));
    {
        ctx.table_next_row();

        if ctx.button(
            "newline",
            Overflow::Clip,
            if state.buffer.is_crlf() { "CRLF" } else { "LF" },
        ) {
            let is_crlf = state.buffer.is_crlf();
            state.buffer.normalize_newlines(!is_crlf);
            ctx.toss_focus_up();
        }

        ctx.button("encoding", Overflow::Clip, state.buffer.encoding());
        if state.wants_encoding_focus {
            state.wants_encoding_focus = false;
            ctx.steal_focus();
        }
        if ctx.contains_focus() {
            if state.path.is_some() {
                ctx.block_begin("frame");
                ctx.attr_float(FloatSpec {
                    anchor: Anchor::Last,
                    gravity_x: 0.0,
                    gravity_y: 1.0,
                    offset_x: 0,
                    offset_y: 0,
                });
                ctx.attr_background_rgba(ctx.indexed(IndexedColor::White));
                ctx.attr_foreground_rgba(ctx.indexed(IndexedColor::Black));
                ctx.attr_padding(Rect::two(0, 1));
                ctx.attr_border();
                {
                    ctx.list_begin("options");
                    ctx.focus_on_first_present();
                    {
                        if ctx.list_item(false, Overflow::Clip, loc(LocId::EncodingReopen))
                            == ListSelection::Activated
                        {
                            state.wants_encoding_change = StateEncodingChange::Reopen;
                        }
                        if ctx.list_item(false, Overflow::Clip, loc(LocId::EncodingConvert))
                            == ListSelection::Activated
                        {
                            state.wants_encoding_change = StateEncodingChange::Convert;
                        }
                    }
                    ctx.list_end();
                }
                ctx.block_end();
            } else {
                // Can't reopen a file that doesn't exist.
                state.wants_encoding_change = StateEncodingChange::Convert;
            }
        }

        ctx.button(
            "indentation",
            Overflow::Clip,
            &format!(
                "{}:{}",
                loc(if state.buffer.indent_with_tabs() {
                    LocId::IndentationTabs
                } else {
                    LocId::IndentationSpaces
                }),
                state.buffer.tab_size(),
            ),
        );
        if state.wants_indentation_focus {
            state.wants_indentation_focus = false;
            ctx.steal_focus();
        }
        if ctx.contains_focus() {
            ctx.table_begin("indentation-picker");
            ctx.attr_float(FloatSpec {
                anchor: Anchor::Last,
                gravity_x: 0.0,
                gravity_y: 1.0,
                offset_x: 0,
                offset_y: 0,
            });
            ctx.attr_background_rgba(ctx.indexed(IndexedColor::White));
            ctx.attr_foreground_rgba(ctx.indexed(IndexedColor::Black));
            ctx.attr_border();
            ctx.attr_padding(Rect::two(0, 1));
            ctx.table_set_cell_gap(Size {
                width: 1,
                height: 0,
            });
            {
                ctx.table_next_row();

                ctx.list_begin("type");
                ctx.focus_on_first_present();
                ctx.attr_padding(Rect::two(0, 1));
                {
                    if ctx.list_item(
                        state.buffer.indent_with_tabs(),
                        Overflow::Clip,
                        loc(LocId::IndentationTabs),
                    ) != ListSelection::Unchanged
                    {
                        state.buffer.set_indent_with_tabs(true);
                        ctx.needs_rerender();
                    }
                    if ctx.list_item(
                        !state.buffer.indent_with_tabs(),
                        Overflow::Clip,
                        loc(LocId::IndentationSpaces),
                    ) != ListSelection::Unchanged
                    {
                        state.buffer.set_indent_with_tabs(false);
                        ctx.needs_rerender();
                    }
                }
                ctx.list_end();

                ctx.list_begin("width");
                ctx.attr_padding(Rect::two(0, 2));
                {
                    for width in 1u8..=8 {
                        let ch = [b'0' + width];
                        let label = unsafe { std::str::from_utf8_unchecked(&ch) };

                        if ctx.list_item(
                            state.buffer.tab_size() == width as i32,
                            Overflow::Clip,
                            label,
                        ) != ListSelection::Unchanged
                        {
                            state.buffer.set_tab_size(width as i32);
                            ctx.needs_rerender();
                        }
                    }
                }
                ctx.list_end();
            }
            ctx.table_end();
        }

        ctx.label(
            "location",
            Overflow::Clip,
            &format!(
                "{}:{}",
                state.buffer.get_cursor_logical_pos().y + 1,
                state.buffer.get_cursor_logical_pos().x + 1
            ),
        );

        #[cfg(any(feature = "debug-layout", feature = "debug-latency"))]
        ctx.label(
            "stats",
            Overflow::Clip,
            &format!(
                "{}/{}",
                state.buffer.get_logical_line_count(),
                state.buffer.get_visual_line_count(),
            ),
        );

        if state.buffer.is_overtype() && ctx.button("overtype", Overflow::Clip, "OVR") {
            state.buffer.set_overtype(false);
            ctx.needs_rerender();
        }

        if state.buffer.is_dirty() {
            ctx.label("dirty", Overflow::Clip, "*");
        }

        ctx.block_begin("filename-container");
        ctx.attr_intrinsic_size(Size {
            width: COORD_TYPE_SAFE_MAX,
            height: 1,
        });
        {
            ctx.label("filename", Overflow::TruncateMiddle, &state.filename);
            ctx.attr_position(Position::Right);
        }
        ctx.block_end();
    }
    ctx.table_end();
}

fn draw_file_picker(ctx: &mut Context, state: &mut State) {
    if state.wants_file_picker == StateFilePicker::Save {
        if state.path.is_some() {
        // `draw_handle_save` will handle things.
        return;
        }
        state.wants_file_picker = StateFilePicker::SaveAs;
    }

    let width = (ctx.size().width - 20).max(10);
    let height = (ctx.size().height - 10).max(10);
    let mut save_path = None;

    ctx.modal_begin(
        "file-picker",
        if state.wants_file_picker == StateFilePicker::Open {
            loc(LocId::FileOpen)
        } else {
            loc(LocId::FileSaveAs)
        },
    );
    ctx.attr_intrinsic_size(Size { width, height });
    {
        let mut activated = false;

        ctx.table_begin("path");
        ctx.table_set_columns(&[0, COORD_TYPE_SAFE_MAX]);
        ctx.table_set_cell_gap(Size {
            width: 1,
            height: 0,
        });
        ctx.attr_padding(Rect::two(1, 1));
        ctx.inherit_focus();
        {
            ctx.table_next_row();

            ctx.label(
                "dir-label",
                Overflow::Clip,
                loc(LocId::SaveAsDialogPathLabel),
            );
            ctx.label(
                "dir",
                Overflow::TruncateMiddle,
                state.file_picker_pending_dir.as_str(),
            );

            ctx.table_next_row();
            ctx.inherit_focus();

            ctx.label(
                "name-label",
                Overflow::Clip,
                loc(LocId::SaveAsDialogNameLabel),
            );
            ctx.editline("name", &mut state.file_picker_pending_name);
            ctx.inherit_focus();
            if ctx.is_focused() && ctx.consume_shortcut(vk::RETURN) {
                activated = true;
            }
        }
        ctx.table_end();

        if state.file_picker_entries.is_none() {
            draw_dialog_saveas_refresh_files(state);
        }

        let files = state.file_picker_entries.as_ref().unwrap();

        ctx.scrollarea_begin(
            "directory",
            Size {
                width: 0,
                // -1 for the label (top)
                // -1 for the label (bottom)
                // -1 for the editline (bottom)
                height: height - 3,
            },
        );
        ctx.attr_background_rgba(ctx.indexed(IndexedColor::Cyan));
        ctx.next_block_id_mixin(state.file_picker_pending_dir.as_str().len() as u64);
        {
            ctx.list_begin("files");
            ctx.inherit_focus();
            for entry in files.iter() {
                match ctx.list_item(
                    state.file_picker_pending_name == entry.as_str(),
                    Overflow::TruncateMiddle,
                    entry.as_str(),
                ) {
                    ListSelection::Unchanged => {}
                    ListSelection::Selected => {
                        state.file_picker_pending_name = entry.as_str().to_string();
                    }
                    ListSelection::Activated => {
                        activated = true;
                    }
                }
            }
            ctx.list_end();
        }
        ctx.scrollarea_end();

        if activated {
            if let Some(path) = draw_dialog_saveas_update_path(state) {
                if state.wants_file_picker == StateFilePicker::Open {
                    // File Open? Just load the file and store the path if it was successful.
                    if draw_handle_load_impl(ctx, state, Some(&path), None) {
                        save_path = Some(path);
                    }
                } else {
                    // File Save? Check if the file exists and show a warning if it does.
                    // Otherwise, save the file and store the path if it was successful.
                    if path.exists() && Some(&path) != state.path.as_ref() {
                        state.file_picker_overwrite_warning = Some(path);
                    } else if draw_handle_save_impl(ctx, state, Some(&path)) {
                        save_path = Some(path);
                    }
                }
            }
        }
    }
    if ctx.modal_end() {
        state.wants_file_picker = StateFilePicker::None;
    }

    if state.file_picker_overwrite_warning.is_some() {
        let mut save;

        ctx.modal_begin("overwrite", loc(LocId::FileOverwriteWarning));
        ctx.attr_background_rgba(ctx.indexed(IndexedColor::Red));
        ctx.attr_foreground_rgba(ctx.indexed(IndexedColor::BrightWhite));
        {
            ctx.label(
                "description",
                Overflow::TruncateTail,
                loc(LocId::FileOverwriteWarningDescription),
            );
            ctx.attr_padding(Rect::three(1, 2, 1));

            ctx.table_begin("choices");
            ctx.attr_padding(Rect::three(0, 2, 1));
            ctx.attr_position(Position::Center);
            ctx.table_set_cell_gap(Size {
                width: 2,
                height: 0,
            });
            {
                ctx.table_next_row();

                save = ctx.button("yes", Overflow::Clip, loc(LocId::Yes));
                ctx.focus_on_first_present();
                if ctx.button("no", Overflow::Clip, loc(LocId::No)) {
                    state.file_picker_overwrite_warning = None;
                }
            }
            ctx.table_end();

            save |= ctx.consume_shortcut(vk::Y);
            if ctx.consume_shortcut(vk::N) {
                state.file_picker_overwrite_warning = None;
            }
        }
        if ctx.modal_end() {
            state.file_picker_overwrite_warning = None;
        }

        if save {
            let path = state.file_picker_overwrite_warning.take();
            if draw_handle_save_impl(ctx, state, path.as_ref()) {
                save_path = path;
            }
            state.file_picker_overwrite_warning = None;
        }
    }

    if let Some(path) = save_path {
        // Only update the path if the save was successful.
        state.set_path(path, state.file_picker_pending_name.clone());
        state.wants_file_picker = StateFilePicker::None;
        state.file_picker_entries = None;
    }
}

// Returns Some(path) if the caller should attempt to save the file.
fn draw_dialog_saveas_update_path(state: &mut State) -> Option<PathBuf> {
    let path = state.file_picker_pending_dir.as_path();
    let path = path.join(&state.file_picker_pending_name);
    let mut normalized = PathBuf::new();

    for c in path.components() {
        match c {
            Component::CurDir => {}
            Component::ParentDir => _ = normalized.pop(),
            _ => normalized.push(c.as_os_str()),
        }
    }

    let (dir, name) = if normalized.is_dir() {
        (normalized.as_path(), String::new())
    } else {
        let dir = normalized.parent().unwrap_or(&normalized);
        let name = get_filename_from_path(&normalized);
        (dir, name)
    };
    if dir != state.file_picker_pending_dir.as_path() {
        state.file_picker_pending_dir = DisplayablePathBuf::new(dir.to_path_buf());
        state.file_picker_entries = None;
    }

    state.file_picker_pending_name = name;
    if state.file_picker_pending_name.is_empty() {
        None
    } else {
        Some(normalized)
    }
}

fn draw_dialog_saveas_refresh_files(state: &mut State) {
    let dir = state.file_picker_pending_dir.as_path();
    let mut files = Vec::new();

    if dir.parent().is_some() {
        files.push(DisplayablePathBuf::from(".."));
    }

    if let Ok(iter) = std::fs::read_dir(dir) {
        for entry in iter.flatten() {
            if let Ok(metadata) = entry.metadata() {
                let mut name = entry.file_name();
                if metadata.is_dir() {
                    name.push("/");
                }
                files.push(DisplayablePathBuf::from(name));
            }
        }
    }

    // Sort directories first, then by name, case-insensitive.
    files[1..].sort_by(|a, b| {
        let a = a.as_bytes();
        let b = b.as_bytes();

        let a_is_dir = a.last() == Some(&b'/');
        let b_is_dir = b.last() == Some(&b'/');

        match b_is_dir.cmp(&a_is_dir) {
            cmp::Ordering::Equal => icu::compare_strings(a, b),
            other => other,
        }
    });

    state.file_picker_entries = Some(files);
}

fn draw_handle_load_impl(
    ctx: &mut Context,
    state: &mut State,
    path: Option<&PathBuf>,
    encoding: Option<&'static str>,
) -> bool {
    let Some(path) = path.or(state.path.as_ref()) else {
        return false;
    };

    if let Err(err) =
        file_open(path).and_then(|mut file| state.buffer.read_file(&mut file, encoding))
    {
        error_log_add(ctx, state, err);
        return false;
    }

    ctx.needs_rerender();
    true
}

fn draw_handle_save(ctx: &mut Context, state: &mut State, path: Option<&PathBuf>) -> bool {
    // Don't retry if the upcoming save fails.
    state.wants_file_picker = StateFilePicker::None;
    draw_handle_save_impl(ctx, state, path)
}

fn draw_handle_save_impl(ctx: &mut Context, state: &mut State, path: Option<&PathBuf>) -> bool {
    let Some(path) = path.or(state.path.as_ref()) else {
        return false;
    };

    if let Err(err) = state.buffer.write_file(path) {
        error_log_add(ctx, state, err);
        return false;
    }

    ctx.needs_rerender();
    true
}

fn draw_dialog_encoding_change(ctx: &mut Context, state: &mut State) {
    let reopen = state.wants_encoding_change == StateEncodingChange::Reopen;
    let width = (ctx.size().width - 20).max(10);
    let height = (ctx.size().height - 10).max(10);

    ctx.modal_begin(
        "encode",
        if reopen {
            loc(LocId::EncodingReopen)
        } else {
            loc(LocId::EncodingConvert)
        },
    );
    {
        ctx.scrollarea_begin("scrollarea", Size { width, height });
        ctx.inherit_focus();
        ctx.attr_background_rgba(ctx.indexed(IndexedColor::Cyan));
        {
            let encodings = icu::get_available_encodings();

            ctx.list_begin("encodings");
            ctx.inherit_focus();
            for encoding in encodings {
                if ctx.list_item(
                    encoding.as_str() == state.buffer.encoding(),
                    Overflow::Clip,
                    encoding.as_str(),
                ) == ListSelection::Activated
                {
                    state.wants_encoding_change = StateEncodingChange::None;
                    if reopen && state.path.is_some() {
                        if state.buffer.is_dirty() {
                            draw_handle_save_impl(ctx, state, None);
                        }
                        draw_handle_load_impl(ctx, state, None, Some(encoding.as_str()));
                    } else {
                        state.buffer.set_encoding(encoding.as_str());
                        ctx.needs_rerender();
                    }
                }
            }
            ctx.list_end();
        }
        ctx.scrollarea_end();
    }
    if ctx.modal_end() {
        state.wants_encoding_change = StateEncodingChange::None;
    }
}

fn draw_handle_wants_exit(ctx: &mut Context, state: &mut State) {
    if !state.buffer.is_dirty() {
        state.exit = true;
        return;
    }

    ctx.modal_begin("unsaved-changes", loc(LocId::UnsavedChangesDialogTitle));
    ctx.attr_background_rgba(ctx.indexed(IndexedColor::Red));
    ctx.attr_foreground_rgba(ctx.indexed(IndexedColor::BrightWhite));
    {
        ctx.label(
            "description",
            Overflow::Clip,
            loc(LocId::UnsavedChangesDialogDescription),
        );
        ctx.attr_padding(Rect::three(1, 2, 1));

        ctx.table_begin("choices");
        ctx.attr_padding(Rect::three(0, 2, 1));
        ctx.attr_position(Position::Center);
        ctx.table_set_cell_gap(Size {
            width: 2,
            height: 0,
        });
        {
            ctx.table_next_row();

            if ctx.button("yes", Overflow::Clip, loc(LocId::UnsavedChangesDialogYes)) {
                state.wants_file_picker = StateFilePicker::Save;
            }
            ctx.focus_on_first_present();
            if ctx.button("no", Overflow::Clip, loc(LocId::UnsavedChangesDialogNo)) {
                state.exit = true;
            }
            if ctx.button(
                "cancel",
                Overflow::Clip,
                loc(LocId::UnsavedChangesDialogCancel),
            ) {
                state.wants_exit = false;
            }

            // TODO: This should highlight the corresponding letter in the label.
            if ctx.consume_shortcut(vk::S) {
                state.wants_file_picker = StateFilePicker::Save;
            } else if ctx.consume_shortcut(vk::N) {
                state.exit = true;
            }
        }
        ctx.table_end();
    }

    if ctx.modal_end() {
        state.wants_exit = false;
    }
}

fn draw_dialog_about(ctx: &mut Context, state: &mut State) {
    ctx.modal_begin("about", loc(LocId::AboutDialogTitle));
    {
        ctx.block_begin("content");
        ctx.attr_padding(Rect::three(1, 2, 1));
        {
            ctx.label("description", Overflow::TruncateTail, "Microsoft Edit");
            ctx.attr_position(Position::Center);

            ctx.label(
                "version",
                Overflow::TruncateHead,
                &format!(
                    "{}{}",
                    loc(LocId::AboutDialogVersion),
                    env!("CARGO_PKG_VERSION")
                ),
            );
            ctx.attr_position(Position::Center);

            ctx.label(
                "copyright",
                Overflow::TruncateTail,
                "Copyright (c) Microsoft Corp 2025",
            );
            ctx.attr_position(Position::Center);
        }
        ctx.block_end();
    }
    if ctx.modal_end() {
        state.wants_about = false;
    }
}

fn draw_error_log(ctx: &mut Context, state: &mut State) {
    ctx.modal_begin("errors", "Error");
    ctx.attr_background_rgba(ctx.indexed(IndexedColor::Red));
    ctx.attr_foreground_rgba(ctx.indexed(IndexedColor::BrightWhite));
    {
        ctx.block_begin("content");
        ctx.attr_padding(Rect::two(1, 2));
        {
            let off = state.error_log_index + state.error_log.len() - state.error_log_count;

            for i in 0..state.error_log_count {
                let idx = (off + i) % state.error_log.len();
                let msg = &state.error_log[idx][..];

                if !msg.is_empty() {
                    ctx.next_block_id_mixin(i as u64);
                    ctx.label("error", Overflow::TruncateTail, msg);
                }
            }
        }
        ctx.block_end();

        if ctx.button("ok", Overflow::Clip, "Ok") {
            state.error_log_count = 0;
        }
        ctx.attr_padding(Rect::three(1, 2, 1));
        ctx.attr_position(Position::Center);
        ctx.focus_on_first_present();
    }
    if ctx.modal_end() {
        state.error_log_count = 0;
    }
}

fn error_log_add(ctx: &mut Context, state: &mut State, err: apperr::Error) {
    let msg = err.message();
    if !msg.is_empty() {
        state.error_log[state.error_log_index] = msg;
        state.error_log_index = (state.error_log_index + 1) % state.error_log.len();
        state.error_log_count = cmp::min(state.error_log_count + 1, state.error_log.len());
        ctx.needs_rerender();
    }
}

fn file_open(path: &Path) -> apperr::Result<File> {
    File::open(path).map_err(apperr::Error::from)
}

fn get_filename_from_path(path: &Path) -> String {
    path.file_name()
        .unwrap_or_default()
        .to_string_lossy()
        .into_owned()
}

fn set_modes() -> RestoreModes {
    // 1049: Alternative Screen Buffer
    //   I put the ASB switch in the beginning, just in case the terminal performs
    //   some additional state tracking beyond the modes we enable/disable.
    // 1002: Cell Motion Mouse Tracking
    // 1006: SGR Mouse Mode
    // 2004: Bracketed Paste Mode
    sys::write_stdout("\x1b[?1049h\x1b[?1002;1006;2004h");
    RestoreModes
}

struct RestoreModes;

impl Drop for RestoreModes {
    fn drop(&mut self) {
        // Same as in the beginning but in the reverse order.
        // It also includes DECSCUSR 0 to reset the cursor style and DECTCEM to show the cursor.
        sys::write_stdout("\x1b[?1002;1006;2004l\x1b[?1049l\x1b[0 q\x1b[?25h");
    }
}

fn query_color_palette(tui: &mut Tui, vt_parser: &mut vt::Parser) {
    let mut indexed_colors = framebuffer::DEFAULT_THEME;

    sys::write_stdout(concat!(
        // OSC 4 color table requests for indices 0 through 15 (base colors).
        "\x1b]4;0;?;1;?;2;?;3;?;4;?;5;?;6;?;7;?\x07",
        "\x1b]4;8;?;9;?;10;?;11;?;12;?;13;?;14;?;15;?\x07",
        // OSC 10 and 11 queries for the current foreground and background colors.
        "\x1b]10;?\x07\x1b]11;?\x07",
        // CSI c reports the terminal capabilities.
        // It also helps us to detect the end of the responses, because not all
        // terminals support the OSC queries, but all of them support CSI c.
        "\x1b[c",
    ));

    let mut done = false;
    let mut osc_buffer = String::new();

    while !done {
        let Some(input) = sys::read_stdin(vt_parser.read_timeout()) else {
            break;
        };

        let mut vt_stream = vt_parser.parse(&input);
        while let Some(token) = vt_stream.next() {
            match token {
                Token::Csi(state) if state.final_byte == 'c' => done = true,
                Token::Osc { mut data, partial } => {
                    if partial {
                        osc_buffer.push_str(data);
                        continue;
                    }
                    if !osc_buffer.is_empty() {
                        osc_buffer.push_str(data);
                        data = &osc_buffer;
                    }

                    let mut splits = data.split_terminator(';');

                    let color = match splits.next().unwrap_or("") {
                        // The response is `4;<color>;rgb:<r>/<g>/<b>`.
                        "4" => match splits.next().unwrap_or("").parse::<usize>() {
                            Ok(val) if val < 16 => &mut indexed_colors[val],
                            _ => continue,
                        },
                        // The response is `10;rgb:<r>/<g>/<b>`.
                        "10" => &mut indexed_colors[IndexedColor::DefaultForeground as usize],
                        // The response is `11;rgb:<r>/<g>/<b>`.
                        "11" => &mut indexed_colors[IndexedColor::DefaultBackground as usize],
                        _ => continue,
                    };

                    let color_param = splits.next().unwrap_or("");
                    if !color_param.starts_with("rgb:") {
                        continue;
                    }

                    let mut iter = color_param[4..].split_terminator('/');
                    let rgb_parts = [(); 3].map(|_| iter.next().unwrap_or("0"));
                    let mut rgb = 0;

                    for part in rgb_parts {
                        if part.len() == 2 || part.len() == 4 {
                            let Ok(mut val) = usize::from_str_radix(part, 16) else {
                                continue;
                            };
                            if part.len() == 4 {
                                val = (val * 0xff + 0x80) / 0xffff;
                            }
                            rgb = (rgb >> 8) | ((val as u32) << 16);
                        }
                    }

                    *color = rgb | 0xff000000;
                    osc_buffer.clear();
                }
                _ => {}
            }
        }
    }

    tui.setup_indexed_colors(indexed_colors);
}
