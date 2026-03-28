// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::num::ParseIntError;

use edit::framebuffer::IndexedColor;
use edit::helpers::*;
use edit::icu;
use edit::input::{kbmod, vk};
use edit::tui::*;
use stdext::string_from_utf8_lossy_owned;

use crate::localization::*;
use crate::state::*;
use std::path::{Path, PathBuf};

// ---------------------------------------------------------
// 1. THE MAIN LAYOUT ENGINE
// ---------------------------------------------------------
pub fn draw_editor(ctx: &mut Context, state: &mut State) {
    if !matches!(state.wants_search.kind, StateSearchKind::Hidden | StateSearchKind::Disabled) {
        draw_search(ctx, state);
    }

    let size = ctx.size();
    let height_reduction = match state.wants_search.kind {
        StateSearchKind::Search => 4,
        StateSearchKind::Replace => 5,
        _ => 2,
    };

    // Determine dynamic column widths: Explorer(30) | Editor(auto) | AI Tab(40)
    let mut columns = Vec::new();
    if state.wants_file_explorer { columns.push(30); } 
    columns.push(-1); 
    if state.wants_ai_tab { columns.push(40); } 
    
    ctx.table_begin("main_split");
    ctx.table_set_columns(&columns);
    ctx.table_next_row();

    // -- LEFT PANEL: FILE EXPLORER --
    if state.wants_file_explorer {
        ctx.block_begin("explorer_col");
        ctx.attr_position(Position::Stretch); // Force full height
        draw_file_explorer(ctx, state);
        ctx.block_end();
    }

    // -- CENTER PANEL: TEXT EDITOR --
    ctx.block_begin("editor_col");
    ctx.attr_position(Position::Stretch); // Force full height
    
    draw_tab_bar(ctx, state); 
    
    if let Some(doc) = state.documents.active() {
        ctx.textarea("textarea", doc.buffer.clone());
        ctx.inherit_focus();
    } else {
        ctx.block_begin("empty");
        ctx.block_end();
    }
    ctx.block_end(); 

    // -- RIGHT PANEL: AI SIDEBAR --
    if state.wants_ai_tab {
        ctx.block_begin("ai_col");
        ctx.attr_position(Position::Stretch); // Force full height
        draw_ai_sidebar(ctx, state);
        ctx.block_end();
    }

    ctx.table_end(); // Close the 3-way split

    ctx.attr_intrinsic_size(Size { width: 0, height: size.height - height_reduction });
}

// ---------------------------------------------------------
// 2. THE AI SIDEBAR (Right Panel)
// ---------------------------------------------------------
pub fn draw_ai_sidebar(ctx: &mut Context, state: &mut State) {
    // Width/Height 0 allows the parent column to dictate the stretch limits
    ctx.scrollarea_begin("ai_sidebar_scroll", Size { width: 0, height: 0 });
    
    ctx.attr_position(Position::Stretch);
    ctx.attr_border(); 

    ctx.block_begin("ai_content");
    ctx.attr_padding(Rect::three(1, 2, 1));
    {
        ctx.label("ai_title", " AI Assistant ");
        ctx.attr_position(Position::Center);
        ctx.attr_foreground_rgba(ctx.indexed(IndexedColor::BrightBlue));
        
        ctx.label("ai_desc", "I am ready to assist.");
        ctx.attr_padding(Rect::three(1, 0, 1));

        ctx.label("ai_placeholder", "[ Chat UI goes here ]");
        ctx.attr_foreground_rgba(ctx.indexed_alpha(IndexedColor::Foreground, 1, 2));
        
        ctx.block_begin("ai_close_btn");
        ctx.attr_position(Position::Center);
        ctx.attr_padding(Rect::three(2, 0, 0));
        
        // This button toggles the state, closing the sidebar
        if ctx.button("close_ai", " Close Panel ", ButtonStyle::default()) {
            state.wants_ai_tab = false;
            ctx.needs_rerender();
        }
        ctx.block_end();
    }
    ctx.block_end(); 

    ctx.scrollarea_end();
}

// ---------------------------------------------------------
// 3. THE FILE EXPLORER (Left Panel)
// ---------------------------------------------------------
pub fn draw_file_explorer(ctx: &mut Context, state: &mut State) {
    ctx.scrollarea_begin("explorer_scroll", Size { width: 0, height: 0 });
    ctx.attr_position(Position::Stretch);
    ctx.attr_border();

    let mut to_toggle = None;
    let mut to_open = None;
    let mut node_id_counter = 0u64;

    fn draw_node(
        ctx: &mut Context,
        path: &Path,
        depth: usize,
        expanded_dirs: &std::collections::HashSet<PathBuf>,
        to_toggle: &mut Option<PathBuf>,
        to_open: &mut Option<PathBuf>,
        id_counter: &mut u64,
    ) {
        let name = path.file_name()
            .map(|n| n.to_string_lossy().to_string())
            .unwrap_or_else(|| "Workspace".to_string());
            
        let is_dir = path.is_dir();
        let is_expanded = expanded_dirs.contains(path);

        let indent = " ".repeat(depth * 2);
        let prefix = if is_dir {
            if is_expanded { "▼ " } else { "▶ " }
        } else {
            "  "
        };

        let mut label = format!("{}{}{}", indent, prefix, name);

        // --- UNDERFLOW/OVERFLOW FIX ---
        // Strictly cap the label length so it never forces the table column to stretch
        // beyond the 30 characters we assigned to it.
        let max_chars = 26; 
        if label.chars().count() > max_chars {
            label = label.chars().take(max_chars - 1).collect::<String>() + "…";
        }

        *id_counter += 1;
        ctx.next_block_id_mixin(*id_counter);
        
        if ctx.button("file_node", &label, ButtonStyle::default().bracketed(false)) {
            if is_dir {
                *to_toggle = Some(path.to_path_buf());
            } else {
                *to_open = Some(path.to_path_buf());
            }
        }

        if is_dir && is_expanded {
            if let Ok(entries) = std::fs::read_dir(path) {
                let mut children: Vec<_> = entries.filter_map(Result::ok).collect();
                children.sort_by_key(|e| {
                    let is_d = e.file_type().map(|ft| ft.is_dir()).unwrap_or(false);
                    (!is_d, e.file_name()) 
                });

                for child in children {
                    draw_node(ctx, &child.path(), depth + 1, expanded_dirs, to_toggle, to_open, id_counter);
                }
            }
        }
    }

    draw_node(ctx, &state.explorer_root.clone(), 0, &state.explorer_expanded_dirs, &mut to_toggle, &mut to_open, &mut node_id_counter);

    ctx.scrollarea_end();

    if let Some(dir) = to_toggle {
        if state.explorer_expanded_dirs.contains(&dir) {
            state.explorer_expanded_dirs.remove(&dir);
        } else {
            state.explorer_expanded_dirs.insert(dir);
        }
        ctx.needs_rerender();
    }

    if let Some(file) = to_open {
        match state.documents.add_file_path(&file) {
            Ok(_) => {},
            Err(err) => error_log_add(ctx, state, err.into()),
        }
        ctx.needs_rerender();
    }
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


pub fn draw_tab_bar(ctx: &mut Context, state: &mut State) {
    let doc_count = state.documents.len();
    if doc_count < 2 {
        // Don't waste screen space if only 0 or 1 file is open
        return; 
    }

    ctx.table_begin("tab_bar");
    
    // Create flexible columns for each tab
    let mut columns = Vec::new();
    for _ in 0..doc_count {
        columns.push(-1); // -1 means distribute space evenly
    }
    ctx.table_set_columns(&columns);
    ctx.table_next_row();

    let mut clicked_idx = None;

    // Draw a tab for each document
    for (idx, doc) in state.documents.iter().enumerate() {
        let is_active = idx == doc_count - 1; // The last doc is always the active one
        
        let tb = doc.buffer.borrow();
        let dirty_marker = if tb.is_dirty() { "*" } else { "" };
        let label = format!(" {}{} ", doc.filename, dirty_marker);

        // Highlight the active tab
        if is_active {
            ctx.attr_background_rgba(ctx.indexed(IndexedColor::BrightBlue));
            ctx.attr_foreground_rgba(ctx.contrasted(ctx.indexed(IndexedColor::BrightBlue)));
        } else {
            ctx.attr_background_rgba(ctx.indexed(IndexedColor::Background));
            ctx.attr_foreground_rgba(ctx.indexed(IndexedColor::Foreground));
        }

        // We use next_block_id_mixin so each button gets a unique IMGUI hash
        ctx.next_block_id_mixin(idx as u64);
        if ctx.button("tab_button", &label, ButtonStyle::default().bracketed(false)) {
            clicked_idx = Some(idx);
        }
    }

    ctx.table_end();

    // If a tab was clicked, make it active
    if let Some(idx) = clicked_idx {
        state.documents.make_active_by_index(idx);
        ctx.needs_rerender();
    }
}