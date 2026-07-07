// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::fs;
use std::num::ParseIntError;
use std::path::PathBuf;

use edit::framebuffer::IndexedColor;
use edit::helpers::*;
use edit::input::{kbmod, vk};
use edit::tui::*;
use edit::{icu, path};
use stdext::string_from_utf8_lossy_owned;

use crate::localization::*;
use crate::state::*;

/// Width in columns of the file browser pane, including its border.
const FILE_PANE_WIDTH: CoordType = 30;

pub fn draw_editor(ctx: &mut Context, state: &mut State) {
    if !matches!(state.wants_search.kind, StateSearchKind::Hidden | StateSearchKind::Disabled) {
        draw_search(ctx, state);
    }

    let size = ctx.size();
    // TODO: The layout code should be able to just figure out the height on its own.
    let height_reduction = match state.wants_search.kind {
        StateSearchKind::Search => 4,
        StateSearchKind::Replace => 5,
        _ => 2,
    };
    let content_height = size.height - height_reduction;

    if state.file_pane_visible {
        ctx.table_begin("editor-row");
        ctx.table_set_columns(&[FILE_PANE_WIDTH, COORD_TYPE_SAFE_MAX]);
        ctx.attr_intrinsic_size(Size { width: 0, height: content_height });
        {
            ctx.table_next_row();

            draw_file_pane(ctx, state, content_height);
            draw_editor_area(ctx, state, content_height);
        }
        ctx.table_end();
    } else {
        draw_editor_area(ctx, state, content_height);
    }
}

fn draw_editor_area(ctx: &mut Context, state: &mut State, content_height: CoordType) {
    if let Some(doc) = state.documents.active() {
        ctx.textarea("textarea", doc.buffer.clone());
        ctx.inherit_focus();
        if state.wants_editor_focus {
            state.wants_editor_focus = false;
            ctx.steal_focus();
        }
    } else {
        state.wants_editor_focus = false;
        ctx.block_begin("empty");
        ctx.block_end();
    }

    ctx.attr_intrinsic_size(Size { width: 0, height: content_height });
}

/// Draws the Yazi-like file browser pane on the left-hand side.
fn draw_file_pane(ctx: &mut Context, state: &mut State, content_height: CoordType) {
    // Initialize the directory to the active document's directory (or the CWD)
    // the first time the pane is shown.
    if state.file_pane_dir.as_path().as_os_str().is_empty() {
        let dir = state
            .documents
            .active()
            .and_then(|doc| doc.dir.as_ref().map(|d| d.as_path().to_path_buf()))
            .or_else(|| std::env::current_dir().ok())
            .unwrap_or_default();
        state.file_pane_dir = DisplayablePathBuf::from_path(dir);
        state.file_pane_entries = None;
    }

    if state.file_pane_entries.is_none() {
        refresh_file_pane_entries(state);
    }

    let mut navigate_to: Option<PathBuf> = None;
    let mut open_file: Option<PathBuf> = None;

    ctx.block_begin("file-pane");
    ctx.attr_focus_well();
    ctx.attr_border();
    ctx.attr_background_rgba(ctx.indexed_alpha(IndexedColor::Black, 1, 4));
    ctx.attr_intrinsic_size(Size { width: FILE_PANE_WIDTH, height: content_height });
    {
        let contains_focus = ctx.contains_focus();

        ctx.label("dir", state.file_pane_dir.as_str());
        ctx.attr_overflow(Overflow::TruncateMiddle);

        if contains_focus && ctx.consume_shortcut(vk::ESCAPE) {
            state.wants_editor_focus = true;
        }

        ctx.scrollarea_begin("file-pane-scroll", Size { width: 0, height: content_height - 3 });
        {
            ctx.next_block_id_mixin(state.file_pane_dir_revision);
            ctx.list_begin("entries");

            let mut first = true;
            for entries in state.file_pane_entries.as_ref().unwrap() {
                for entry in entries {
                    let sel = ctx.list_item(false, entry.as_str());
                    ctx.attr_overflow(Overflow::TruncateMiddle);

                    if first && state.file_pane_focus {
                        ctx.list_item_steal_focus();
                    }
                    first = false;

                    if sel == ListSelection::Activated {
                        let path =
                            path::normalize(&state.file_pane_dir.as_path().join(entry.as_path()));
                        if path.is_dir() {
                            navigate_to = Some(path);
                        } else {
                            open_file = Some(path);
                        }
                    }
                }
            }

            ctx.list_end();
        }
        ctx.scrollarea_end();
    }
    ctx.block_end();

    state.file_pane_focus = false;

    if let Some(dir) = navigate_to {
        state.file_pane_dir = DisplayablePathBuf::from_path(dir);
        state.file_pane_dir_revision = state.file_pane_dir_revision.wrapping_add(1);
        state.file_pane_entries = None;
        ctx.needs_rerender();
    } else if let Some(path) = open_file {
        match state.documents.add_file_path(&path) {
            Ok(..) => {
                state.wants_editor_focus = true;
                ctx.needs_rerender();
            }
            Err(err) => error_log_add(ctx, state, err),
        }
    }
}

/// Reads the contents of `state.file_pane_dir` into `state.file_pane_entries`.
/// Handles inaccessible directories gracefully (yields an empty listing).
fn refresh_file_pane_entries(state: &mut State) {
    let dir = state.file_pane_dir.as_path();
    // ["..", directories, files]
    let mut dirs_files = [Vec::new(), Vec::new(), Vec::new()];

    if cfg!(windows) || dir.parent().is_some() {
        dirs_files[0].push(DisplayablePathBuf::from(".."));
    }

    if let Ok(iter) = fs::read_dir(dir) {
        for entry in iter.flatten() {
            if let Ok(metadata) = entry.metadata() {
                let mut name = entry.file_name();
                let is_dir = metadata.is_dir()
                    || (metadata.is_symlink()
                        && fs::metadata(entry.path()).is_ok_and(|m| m.is_dir()));
                let idx = if is_dir { 1 } else { 2 };

                if is_dir {
                    name.push("/");
                }

                dirs_files[idx].push(DisplayablePathBuf::from(name));
            }
        }
    }

    for entries in &mut dirs_files[1..] {
        entries.sort_unstable_by(|a, b| icu::compare_strings(a.as_bytes(), b.as_bytes()));
    }

    state.file_pane_entries = Some(dirs_files);
}

fn draw_search(ctx: &mut Context, state: &mut State) {
    if let Err(err) = icu::init() {
        error_log_add(ctx, state, err.into());
        state.wants_search.kind = StateSearchKind::Disabled;
        return;
    }

    let Some(doc) = state.documents.active() else {
        state.wants_search.kind = StateSearchKind::Hidden;
        return;
    };

    let mut action = None;
    let mut focus = StateSearchKind::Hidden;

    if state.wants_search.focus {
        state.wants_search.focus = false;
        focus = StateSearchKind::Search;

        // If the selection is empty, focus the search input field.
        // Otherwise, focus the replace input field, if it exists.
        if let Some(selection) = doc.buffer.borrow_mut().extract_user_selection(false) {
            state.search_needle = string_from_utf8_lossy_owned(selection);
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
        ctx.table_set_cell_gap(Size { width: 1, height: 0 });
        {
            {
                ctx.table_next_row();
                ctx.label("label", loc(LocId::SearchNeedleLabel));

                if ctx.editline("needle", &mut state.search_needle) {
                    action = Some(SearchAction::Search);
                }
                if !state.search_success {
                    ctx.attr_background_rgba(ctx.indexed(IndexedColor::Red));
                    ctx.attr_foreground_rgba(ctx.indexed(IndexedColor::BrightWhite));
                }
                ctx.attr_intrinsic_size(Size { width: COORD_TYPE_SAFE_MAX, height: 1 });
                if focus == StateSearchKind::Search {
                    ctx.steal_focus();
                }
                if ctx.is_focused() && ctx.consume_shortcut(vk::RETURN) {
                    action = Some(SearchAction::Search);
                }
            }

            if state.wants_search.kind == StateSearchKind::Replace {
                ctx.table_next_row();
                ctx.label("label", loc(LocId::SearchReplacementLabel));

                ctx.editline("replacement", &mut state.search_replacement);
                ctx.attr_intrinsic_size(Size { width: COORD_TYPE_SAFE_MAX, height: 1 });
                if focus == StateSearchKind::Replace {
                    ctx.steal_focus();
                }
                if ctx.is_focused() {
                    if ctx.consume_shortcut(vk::RETURN) {
                        action = Some(SearchAction::Replace);
                    } else if ctx.consume_shortcut(kbmod::CTRL_ALT | vk::RETURN) {
                        action = Some(SearchAction::ReplaceAll);
                    }
                }
            }
        }
        ctx.table_end();

        ctx.table_begin("options");
        ctx.table_set_cell_gap(Size { width: 2, height: 0 });
        {
            let mut change = false;
            let mut change_action = Some(SearchAction::Search);

            ctx.table_next_row();

            change |= ctx.checkbox(
                "match-case",
                loc(LocId::SearchMatchCase),
                &mut state.search_options.match_case,
            );
            change |= ctx.checkbox(
                "whole-word",
                loc(LocId::SearchWholeWord),
                &mut state.search_options.whole_word,
            );
            change |= ctx.checkbox(
                "use-regex",
                loc(LocId::SearchUseRegex),
                &mut state.search_options.use_regex,
            );
            if state.wants_search.kind == StateSearchKind::Replace
                && ctx.button("replace-all", loc(LocId::SearchReplaceAll), ButtonStyle::default())
            {
                change = true;
                change_action = Some(SearchAction::ReplaceAll);
            }
            if ctx.button("close", loc(LocId::SearchClose), ButtonStyle::default()) {
                state.wants_search.kind = StateSearchKind::Hidden;
            }

            if change {
                action = change_action;
            }
        }
        ctx.table_end();
    }
    ctx.block_end();

    if let Some(action) = action {
        search_execute(ctx, state, action);
    }
}

pub enum SearchAction {
    Search,
    Replace,
    ReplaceAll,
}

pub fn search_execute(ctx: &mut Context, state: &mut State, action: SearchAction) {
    let Some(doc) = state.documents.active_mut() else {
        return;
    };

    state.search_success = match action {
        SearchAction::Search => {
            doc.buffer.borrow_mut().find_and_select(&state.search_needle, state.search_options)
        }
        SearchAction::Replace => doc.buffer.borrow_mut().find_and_replace(
            &state.search_needle,
            state.search_options,
            state.search_replacement.as_bytes(),
        ),
        SearchAction::ReplaceAll => doc.buffer.borrow_mut().find_and_replace_all(
            &state.search_needle,
            state.search_options,
            state.search_replacement.as_bytes(),
        ),
    }
    .is_ok();

    ctx.needs_rerender();
}

pub fn draw_handle_save(ctx: &mut Context, state: &mut State) {
    if let Some(doc) = state.documents.active_mut() {
        if doc.path.is_some() {
            if let Err(err) = doc.save(None) {
                error_log_add(ctx, state, err);
            }
        } else {
            // No path? Show the file picker.
            state.wants_file_picker = StateFilePicker::SaveAs;
            state.wants_save = false;
            ctx.needs_rerender();
        }
    }

    state.wants_save = false;
}

pub fn draw_handle_wants_close(ctx: &mut Context, state: &mut State) {
    let Some(doc) = state.documents.active() else {
        state.wants_close = false;
        return;
    };

    if !doc.buffer.borrow().is_dirty() {
        state.documents.remove_active();
        state.wants_close = false;
        ctx.needs_rerender();
        return;
    }

    enum Action {
        None,
        Save,
        Discard,
        Cancel,
    }
    let mut action = Action::None;

    ctx.modal_begin("unsaved-changes", loc(LocId::UnsavedChangesDialogTitle));
    ctx.attr_background_rgba(ctx.indexed(IndexedColor::Red));
    ctx.attr_foreground_rgba(ctx.indexed(IndexedColor::BrightWhite));
    {
        let contains_focus = ctx.contains_focus();

        ctx.label("description", loc(LocId::UnsavedChangesDialogDescription));
        ctx.attr_padding(Rect::three(1, 2, 1));

        ctx.table_begin("choices");
        ctx.inherit_focus();
        ctx.attr_padding(Rect::three(0, 2, 1));
        ctx.attr_position(Position::Center);
        ctx.table_set_cell_gap(Size { width: 2, height: 0 });
        {
            ctx.table_next_row();
            ctx.inherit_focus();

            if ctx.button(
                "yes",
                loc(LocId::UnsavedChangesDialogYes),
                ButtonStyle::default().accelerator('S'),
            ) {
                action = Action::Save;
            }
            ctx.inherit_focus();
            if ctx.button(
                "no",
                loc(LocId::UnsavedChangesDialogNo),
                ButtonStyle::default().accelerator('N'),
            ) {
                action = Action::Discard;
            }
            if ctx.button("cancel", loc(LocId::Cancel), ButtonStyle::default()) {
                action = Action::Cancel;
            }

            // Handle accelerator shortcuts
            if contains_focus {
                if ctx.consume_shortcut(vk::S) {
                    action = Action::Save;
                } else if ctx.consume_shortcut(vk::N) {
                    action = Action::Discard;
                }
            }
        }
        ctx.table_end();
    }
    if ctx.modal_end() {
        action = Action::Cancel;
    }

    match action {
        Action::None => return,
        Action::Save => {
            state.wants_save = true;
        }
        Action::Discard => {
            state.documents.remove_active();
            state.wants_close = false;
        }
        Action::Cancel => {
            state.wants_exit = false;
            state.wants_close = false;
        }
    }

    ctx.needs_rerender();
}

pub fn draw_goto_menu(ctx: &mut Context, state: &mut State) {
    let mut done = false;

    if let Some(doc) = state.documents.active_mut() {
        ctx.modal_begin("goto", loc(LocId::FileGoto));
        {
            if ctx.editline("goto-line", &mut state.goto_target) {
                state.goto_invalid = false;
            }
            if state.goto_invalid {
                ctx.attr_background_rgba(ctx.indexed(IndexedColor::Red));
                ctx.attr_foreground_rgba(ctx.indexed(IndexedColor::BrightWhite));
            }

            ctx.attr_intrinsic_size(Size { width: 24, height: 1 });
            ctx.steal_focus();

            if ctx.consume_shortcut(vk::RETURN) {
                match validate_goto_point(&state.goto_target) {
                    Ok(point) => {
                        let mut buf = doc.buffer.borrow_mut();
                        buf.cursor_move_to_logical(point);
                        buf.make_cursor_visible();
                        done = true;
                    }
                    Err(_) => state.goto_invalid = true,
                }
                ctx.needs_rerender();
            }
        }
        done |= ctx.modal_end();
    } else {
        done = true;
    }

    if done {
        state.wants_goto = false;
        state.goto_target.clear();
        state.goto_invalid = false;
        ctx.needs_rerender();
    }
}

fn validate_goto_point(line: &str) -> Result<Point, ParseIntError> {
    let mut coords = [0; 2];
    let (y, x) = line.split_once(':').unwrap_or((line, "0"));
    // Using a loop here avoids 2 copies of the str->int code.
    // This makes the binary more compact.
    for (i, s) in [x, y].iter().enumerate() {
        coords[i] = s.parse::<CoordType>()?.saturating_sub(1);
    }
    Ok(Point { x: coords[0], y: coords[1] })
}
