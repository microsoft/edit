// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::num::ParseIntError;

use edit::framebuffer::IndexedColor;
use edit::helpers::*;
use edit::icu;
use edit::input::{kbmod, vk};
use edit::tui::*;

use crate::localization::*;
use crate::state::*;

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

    let editor_height = size.height - height_reduction;

    // Synchronize split layout with current document state
    sync_split_layout(state);

    // Handle split view requests
    handle_split_requests(state);

    // Draw the editor area (single or split)
    draw_editor_panes(ctx, state, editor_height);
}

/// Synchronize the split layout with the current document state.
/// Ensures panes reference valid documents.
fn sync_split_layout(state: &mut State) {
    // If no panes exist, create one for the active document
    if state.split_layout.panes.is_empty() {
        if let Some(doc) = state.documents.active() {
            state.split_layout.panes.push(Pane {
                document_index: 0,
                buffer: doc.buffer.clone(),
                filename: doc.filename.clone(),
            });
            state.split_layout.active_pane = 0;
        }
    } else if state.split_layout.split_direction == SplitDirection::None {
        // In single-pane mode, always sync with the active document
        if let Some(doc) = state.documents.active() {
            if let Some(pane) = state.split_layout.panes.get_mut(0) {
                pane.buffer = doc.buffer.clone();
                pane.filename = doc.filename.clone();
                pane.document_index = 0;
            }
        }
    }
}

/// Handle split view requests (split, close pane, focus navigation).
fn handle_split_requests(state: &mut State) {
    if std::mem::take(&mut state.wants_split_horizontal) {
        do_split(state, SplitDirection::Horizontal);
    }
    if std::mem::take(&mut state.wants_split_vertical) {
        do_split(state, SplitDirection::Vertical);
    }
    if std::mem::take(&mut state.wants_close_pane) {
        close_active_pane(state);
    }

    let count = state.split_layout.pane_count();
    if count > 1 {
        if std::mem::take(&mut state.wants_focus_next_pane) {
            state.split_layout.active_pane = (state.split_layout.active_pane + 1) % count;
        }
        if std::mem::take(&mut state.wants_focus_prev_pane) {
            state.split_layout.active_pane = (state.split_layout.active_pane + count - 1) % count;
        }
    }
}

/// Perform a split operation.
fn do_split(state: &mut State, direction: SplitDirection) {
    // Need at least one document to split
    let Some(doc) = state.documents.active() else {
        return;
    };

    if state.split_layout.split_direction == SplitDirection::None {
        // First split: change direction and add a second pane
        state.split_layout.split_direction = direction;

        // Clone the current document's buffer into the new pane
        // (same document shown in both panes)
        state.split_layout.panes.push(Pane {
            document_index: 0,
            buffer: doc.buffer.clone(),
            filename: doc.filename.clone(),
        });

        // Focus the new pane
        state.split_layout.active_pane = state.split_layout.panes.len() - 1;
    } else {
        // Already split - just change direction (we only support 2 panes for now)
        state.split_layout.split_direction = direction;
    }
}

/// Close the active pane.
fn close_active_pane(state: &mut State) {
    if state.split_layout.panes.len() <= 1 {
        // Can't close the last pane, close the document instead
        return;
    }

    state.split_layout.panes.remove(state.split_layout.active_pane);

    // Adjust active pane index
    if state.split_layout.active_pane >= state.split_layout.panes.len() {
        state.split_layout.active_pane = state.split_layout.panes.len() - 1;
    }

    // If only one pane remains, go back to single-pane mode
    if state.split_layout.panes.len() == 1 {
        state.split_layout.split_direction = SplitDirection::None;
    }
}

/// Draw the editor panes based on split layout.
fn draw_editor_panes(ctx: &mut Context, state: &mut State, editor_height: CoordType) {
    let pane_count = state.split_layout.pane_count();

    if pane_count == 0 {
        // No documents open
        ctx.block_begin("empty");
        ctx.block_end();
        ctx.attr_intrinsic_size(Size { width: 0, height: editor_height });
        return;
    }

    if pane_count == 1 || state.split_layout.split_direction == SplitDirection::None {
        // Single pane mode
        if let Some(pane) = state.split_layout.panes.first() {
            ctx.textarea("textarea", pane.buffer.clone());
            ctx.inherit_focus();
        } else {
            ctx.block_begin("empty");
            ctx.block_end();
        }
        ctx.attr_intrinsic_size(Size { width: 0, height: editor_height });
    } else {
        // Split mode - draw panes in a table layout
        draw_split_panes(ctx, state, editor_height);
    }
}

/// Draw split panes using table layout.
fn draw_split_panes(ctx: &mut Context, state: &mut State, editor_height: CoordType) {
    let direction = state.split_layout.split_direction;
    let active_pane = state.split_layout.active_pane;
    let pane_count = state.split_layout.panes.len();

    // For now, only support 2 panes to keep it simple
    if pane_count != 2 {
        // Fall back to single pane
        if let Some(pane) = state.split_layout.panes.first() {
            ctx.textarea("textarea", pane.buffer.clone());
            ctx.inherit_focus();
        }
        ctx.attr_intrinsic_size(Size { width: 0, height: editor_height });
        return;
    }

    let screen_width = ctx.size().width;
    let half_width = (screen_width - 1) / 2; // -1 for the gap/separator

    if direction == SplitDirection::Horizontal {
        draw_horizontal_split(ctx, state, editor_height, half_width, active_pane);
    } else {
        draw_vertical_split(ctx, state, editor_height, active_pane);
    }
}

/// Draw horizontal split layout (side by side panes).
fn draw_horizontal_split(
    ctx: &mut Context,
    state: &State,
    editor_height: CoordType,
    half_width: CoordType,
    active_pane: usize,
) {
    ctx.table_begin("split-h");
    ctx.table_set_columns(&[half_width, half_width]);
    ctx.table_set_cell_gap(Size { width: 1, height: 0 });
    ctx.table_next_row();

    for (idx, (pane, name)) in state.split_layout.panes.iter()
        .zip(["left-pane", "right-pane"])
        .enumerate()
    {
        let is_active = active_pane == idx;
        ctx.block_begin(name);
        {
            draw_pane_header(ctx, pane, is_active);
            let textarea_name = if idx == 0 { "textarea-left" } else { "textarea-right" };
            ctx.textarea(textarea_name, pane.buffer.clone());
            if is_active {
                ctx.inherit_focus();
            }
            ctx.attr_intrinsic_size(Size { width: 0, height: editor_height - 1 });
        }
        ctx.block_end();
        ctx.attr_intrinsic_size(Size { width: 0, height: editor_height });
    }

    ctx.table_end();
    ctx.attr_intrinsic_size(Size { width: 0, height: editor_height });
}

/// Draw vertical split layout (stacked panes).
fn draw_vertical_split(
    ctx: &mut Context,
    state: &State,
    editor_height: CoordType,
    active_pane: usize,
) {
    let pane_height = (editor_height - 1) / 2; // -1 for separator

    ctx.block_begin("split-v");
    {
        // Top pane
        draw_pane_block(ctx, &state.split_layout.panes[0], "top", "textarea-top", active_pane == 0, pane_height);

        // Separator
        ctx.block_begin("separator");
        ctx.attr_background_rgba(ctx.indexed(IndexedColor::BrightBlack));
        ctx.attr_intrinsic_size(Size { width: COORD_TYPE_SAFE_MAX, height: 1 });
        ctx.block_end();

        // Bottom pane
        draw_pane_block(ctx, &state.split_layout.panes[1], "bottom", "textarea-bottom", active_pane == 1, pane_height);
    }
    ctx.block_end();
    ctx.attr_intrinsic_size(Size { width: 0, height: editor_height });
}

/// Draw a single pane block with header and textarea.
fn draw_pane_block(
    ctx: &mut Context,
    pane: &Pane,
    block_name: &'static str,
    textarea_name: &'static str,
    is_active: bool,
    height: CoordType,
) {
    ctx.block_begin(block_name);
    {
        draw_pane_header(ctx, pane, is_active);
        ctx.textarea(textarea_name, pane.buffer.clone());
        if is_active {
            ctx.inherit_focus();
        }
        ctx.attr_intrinsic_size(Size { width: 0, height: height - 1 });
    }
    ctx.block_end();
    ctx.attr_intrinsic_size(Size { width: 0, height: height });
}

/// Draw a pane header showing the filename.
fn draw_pane_header(ctx: &mut Context, pane: &Pane, is_active: bool) {
    use std::borrow::Cow;

    let (bg, fg) = if is_active {
        (ctx.indexed(IndexedColor::Blue), ctx.indexed(IndexedColor::BrightWhite))
    } else {
        (ctx.indexed(IndexedColor::BrightBlack), ctx.indexed(IndexedColor::White))
    };

    ctx.block_begin("pane-header");
    {
        let label: Cow<'_, str> = if pane.buffer.borrow().is_dirty() {
            format!("● {}", pane.filename).into()
        } else {
            Cow::Borrowed(&pane.filename)
        };

        ctx.label("filename", &label);
        ctx.attr_overflow(Overflow::TruncateMiddle);
        ctx.attr_background_rgba(bg);
        ctx.attr_foreground_rgba(fg);
    }
    ctx.block_end();
    ctx.attr_intrinsic_size(Size { width: COORD_TYPE_SAFE_MAX, height: 1 });
    ctx.attr_background_rgba(bg);
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
            state.search_needle = String::from_utf8_lossy_owned(selection);
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
                state.wants_search.focus = true;
                ctx.needs_rerender();
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
        // Reset split layout when document is removed (will be re-synced)
        state.split_layout.reset();
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
            // Reset split layout when document is removed (will be re-synced)
            state.split_layout.reset();
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
