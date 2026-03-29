// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.
extern crate ai_plugin;
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

    // --- 1. THE LAYOUT FIX: Calculate exact center width ---
    let total_width = size.width as i32;
    let mut center_width = total_width;

    let mut columns: Vec<CoordType> = Vec::new();

    if state.wants_file_explorer { 
        columns.push(state.explorer_width as CoordType); 
        columns.push(1); // Splitter             
        center_width -= state.explorer_width + 1;
    } 
    
    // Push a placeholder for the center column so we can replace it later
    let center_idx = columns.len();
    columns.push(0);                       
    
    if state.wants_ai_tab { 
        columns.push(1); // Splitter                    
        columns.push(state.ai_width as CoordType);       
        center_width -= state.ai_width + 1;
    } 
    
    // Ensure the center width never breaks if the terminal shrinks too much
    center_width = center_width.max(10);
    columns[center_idx] = center_width as CoordType;
    
    ctx.table_begin("main_split");
    ctx.attr_position(Position::Stretch); 
    ctx.table_set_columns(&columns);
    ctx.table_next_row();

    // -- LEFT PANEL: FILE EXPLORER --
    if state.wants_file_explorer {
        ctx.block_begin("explorer_col");
        ctx.attr_position(Position::Stretch); 
        draw_file_explorer(ctx, state);
        ctx.block_end();

        // -- EXPLORER SPLITTER --
        ctx.block_begin("explorer_splitter");
        ctx.attr_position(Position::Stretch);
        ctx.attr_focusable(); 
        ctx.attr_background_rgba(ctx.indexed_alpha(IndexedColor::Foreground, 1, 6)); 

        // THE DRAG FIX: Sync start width ONLY on the exact frame the mouse clicks down
        if let Some(delta) = ctx.drag_delta() {
            if delta.x == 0 && delta.y == 0 {
                state.explorer_drag_start = state.explorer_width;
            }
            state.explorer_width = (state.explorer_drag_start + delta.x as i32).clamp(10, 80);
            ctx.needs_rerender();
        }
        ctx.block_end();
    }

    // -- CENTER PANEL: TEXT EDITOR --
    ctx.block_begin("editor_col");
    ctx.attr_position(Position::Stretch); 
    
    draw_tab_bar(ctx, state); 
    
    if let Some(doc) = state.documents.active() {
        ctx.textarea("textarea", doc.buffer.clone());
        ctx.inherit_focus();
    } else {
        ctx.block_begin("empty");
        ctx.attr_position(Position::Stretch);
        ctx.block_end();
    }
    ctx.block_end(); 

    // -- RIGHT PANEL: AI SIDEBAR --
    if state.wants_ai_tab {
        // -- AI SPLITTER --
        ctx.block_begin("ai_splitter");
        ctx.attr_position(Position::Stretch);
        ctx.attr_focusable();
        ctx.attr_background_rgba(ctx.indexed_alpha(IndexedColor::Foreground, 1, 6));

        // THE DRAG FIX: Sync start width ONLY on the exact frame the mouse clicks down
        if let Some(delta) = ctx.drag_delta() {
            if delta.x == 0 && delta.y == 0 {
                state.ai_drag_start = state.ai_width;
            }
            // Dragging left means negative delta.x, which increases AI width
            state.ai_width = (state.ai_drag_start - delta.x as i32).clamp(20, 120);
            ctx.needs_rerender();
        }
        ctx.block_end();

        // -- AI CONTENT --
        ctx.block_begin("ai_col");
        ctx.attr_position(Position::Stretch); 
        draw_ai_sidebar(ctx, state);
        ctx.block_end();
    }

    ctx.table_end(); // Close the split

    ctx.attr_intrinsic_size(Size { width: 0, height: size.height - height_reduction });
}

// ---------------------------------------------------------
// 2. THE AI SIDEBAR (Right Panel)
// ---------------------------------------------------------
pub fn draw_ai_sidebar(ctx: &mut Context, state: &mut State) {
    if let Some(rx) = &state.ai_receiver {
        if let Ok(response) = rx.try_recv() {
            state.ai_history.push(("AI".to_string(), response));
            state.is_ai_thinking = false;
            state.ai_receiver = None;
            ctx.needs_rerender();
        }
    }

    // THE FIX: Wrap the ENTIRE sidebar in a flex column
    ctx.block_begin("ai_sidebar_main");
    ctx.attr_position(Position::Stretch);

    // --- 1. HEADER (Top) ---
    ctx.table_begin("ai_header");
    ctx.table_set_columns(&[-1, 5]);
    ctx.table_next_row();
    ctx.label("ai_title_lbl", " Edit Copilot");
    
    // THE FIX: Move close button to the top right so it never gets clipped
    if ctx.button("close_ai_btn", " [X] ", ButtonStyle::default()) {
        state.wants_ai_tab = false;
        ctx.needs_rerender();
    }
    ctx.table_end();

    // --- 2. CHAT HISTORY (Middle Flex Region) ---
    // THE FIX: The scroll area now ONLY wraps the history. 
    // Position::Stretch forces it to push the input box down to the screen floor!
    ctx.scrollarea_begin("ai_sidebar_scroll", Size { width: 0, height: 0 });
    ctx.attr_position(Position::Stretch);
    ctx.attr_border(); 
    ctx.attr_padding(Rect::three(1, 1, 1));

    if state.ai_history.is_empty() {
        ctx.block_begin("ai_welcome");
        ctx.attr_position(Position::Center);

        if state.ai_width > 35 {
            let ascii_logo = [
                "       ++++++++++++++:...           ",
                "     ==============:.:::::          ",
                "    ==============-:::::---         ",
                "   ===============::::------        ",
                "  ===============:::---------       ",
                "  ===============:     +++++======= ",
                " ================     ==============",
                "++++++++++++++++     ===============",
                "+++++++++++++++      ===============",
                "+++++++********     ================",
                "**************     ++++============ ",
                " ************     -+++++++++======  ",
                "       ****++++====+++++++++++++++  ",
                "         *+++++==-*++++++++++++++   ",
                "         +++++===*******++++++++    ",
                "          ++===-=**************     ",
                "            =-=**************       ",
            ];

            for (i, &line) in ascii_logo.iter().enumerate() {
                if i < 9 {
                    ctx.attr_foreground_rgba(ctx.indexed(IndexedColor::BrightBlue));
                } else if i < 18 {
                    ctx.attr_foreground_rgba(ctx.indexed(IndexedColor::BrightCyan));
                } else {
                    ctx.attr_foreground_rgba(ctx.indexed(IndexedColor::BrightGreen));
                }
                ctx.next_block_id_mixin(i as u64);
                ctx.label("logo_line", line);
            }
            ctx.attr_padding(Rect::three(0, 0, 1));
        }

        ctx.label("ai_desc", "How can I help you code today?");
        ctx.attr_foreground_rgba(ctx.indexed_alpha(IndexedColor::Foreground, 1, 2));
        ctx.block_end(); 
    } else {
        let max_text_width = (state.ai_width - 6).max(10) as usize;

        for (i, (role, msg)) in state.ai_history.iter().enumerate() {
            ctx.next_block_id_mixin(i as u64);
            ctx.block_begin("msg_block");
            ctx.attr_padding(Rect::three(0, 0, 1));
            
            if role == "User" {
                ctx.label("role_user", " You:");
                ctx.attr_foreground_rgba(ctx.indexed(IndexedColor::BrightCyan));
            } else {
                ctx.label("role_ai", " Copilot:");
                ctx.attr_foreground_rgba(ctx.indexed(IndexedColor::BrightGreen));
            }
            
            for (j, line) in msg.lines().enumerate() {
                let mut chars: Vec<char> = line.chars().collect();
                let mut sub_line_idx = 0;
                
                if chars.is_empty() {
                    ctx.next_block_id_mixin((i * 10000 + j * 100) as u64);
                    ctx.label("msg_line", "");
                    continue;
                }

                while !chars.is_empty() {
                    let chunk_size = chars.len().min(max_text_width);
                    let chunk: String = chars.drain(0..chunk_size).collect();
                    
                    ctx.next_block_id_mixin((i * 10000 + j * 100 + sub_line_idx) as u64);
                    ctx.label("msg_line", &format!("  {}", chunk));
                    ctx.attr_foreground_rgba(ctx.indexed(IndexedColor::Foreground));
                    sub_line_idx += 1;
                }
            }
            ctx.block_end();
        }

        if state.is_ai_thinking {
            ctx.label("thinking", " Copilot is typing...");
            ctx.attr_foreground_rgba(ctx.indexed_alpha(IndexedColor::Foreground, 1, 2));
        }
    }
    ctx.scrollarea_end();

    // --- 3. INPUT AREA & ACTIONS (Bottom Floor) ---
    ctx.block_begin("ai_input_container");
    ctx.attr_padding(Rect::three(1, 1, 0));

    ctx.label("ai_input_lbl", "Ask anything:");
    ctx.attr_foreground_rgba(ctx.indexed_alpha(IndexedColor::Foreground, 1, 2));

    ctx.editline("ai_prompt_input", &mut state.ai_prompt);
    ctx.attr_intrinsic_size(Size { width: COORD_TYPE_SAFE_MAX, height: 2 }); 

    ctx.table_begin("ai_actions");
    ctx.table_set_columns(&[-1, 7, 8]); // Compact spacing
    ctx.table_next_row();
    ctx.block_begin("spacer"); ctx.block_end(); 

    if ctx.button("key_btn", " Key ", ButtonStyle::default()) {
        state.api_key_input = state.ai_api_key.clone(); 
        state.wants_api_key_modal = true;
        ctx.needs_rerender();
    }

    let can_send = !state.ai_prompt.trim().is_empty() && !state.is_ai_thinking;
    
    if ctx.button("send_btn", " Send ", ButtonStyle::default()) && can_send {
        if state.ai_api_key.trim().is_empty() {
            state.wants_api_key_modal = true;
            ctx.needs_rerender();
        } else {
            let prompt_text = state.ai_prompt.clone();
            state.ai_history.push(("User".to_string(), prompt_text.clone()));
            state.ai_prompt.clear();
            state.is_ai_thinking = true;

            let (tx, rx) = std::sync::mpsc::channel();
            state.ai_receiver = Some(rx);
            let api_key = state.ai_api_key.clone();

            std::thread::spawn(move || {
                let endpoint = "https://api.groq.com/openai/v1/chat/completions";
                
                // Use one of Groq's blazing fast open-source models
                // Options: "llama3-8b-8192", "llama3-70b-8192", "mixtral-8x7b-32768"
                let model = "llama3-70b-8192";

                let response = ::ai_plugin::fetch_ai_response(endpoint, &api_key, model, &prompt_text);
                
                let final_msg = response.unwrap_or_else(|err| format!("Copilot Error: {}", err));
                let _ = tx.send(final_msg);
            });

            ctx.needs_rerender();
        }
    }
    ctx.table_end(); 
    ctx.block_end(); 

    ctx.block_end(); // Close the main sidebar column

    // --- 4. API KEY POPUP MODAL ---
    if state.wants_api_key_modal {
        ctx.modal_begin("api_key_modal", " API Key Setup ");
        
        ctx.block_begin("modal_content");
        ctx.attr_padding(Rect::three(1, 2, 1));
        
        ctx.label("api_key_desc", "Please enter your API Key (Ctrl+V to paste):");
        ctx.attr_padding(Rect::three(0, 0, 1));
        
        ctx.editline("api_key_input", &mut state.api_key_input);
        ctx.attr_intrinsic_size(Size { width: 45, height: 1 });
        
        // THE FIX: Removed steal_focus() so the mouse and clipboard work naturally.
        
        let mut save_triggered = false;
        if ctx.consume_shortcut(vk::RETURN) {
            save_triggered = true;
        }

        ctx.table_begin("modal_actions");
        ctx.table_set_columns(&[-1, 10, 10]);
        ctx.table_next_row();
        ctx.block_begin("modal_spacer"); ctx.block_end();
        
        if ctx.button("save_key_btn", "  Save  ", ButtonStyle::default()) {
            save_triggered = true;
        }
        
        if ctx.button("cancel_key_btn", " Cancel ", ButtonStyle::default()) {
            state.wants_api_key_modal = false;
            ctx.needs_rerender();
        }
        ctx.table_end();
        ctx.block_end();
        
        let modal_closed = ctx.modal_end();

        // THE FIX: Handle the save or close properly
        if save_triggered {
            state.ai_api_key = state.api_key_input.clone();
            state.wants_api_key_modal = false;
            ctx.needs_rerender();
        } else if modal_closed {
            state.wants_api_key_modal = false;
            ctx.needs_rerender();
        }
    }
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

    // --- THE FIX: Prevent Duplicate Tabs ---
    if let Some(file) = to_open {
        let mut found_idx = None;
        
        // Check if the file is already open
        for (idx, doc) in state.documents.iter().enumerate() {
            if let Some(p) = &doc.path {
                if p == &file {
                    found_idx = Some(idx);
                    break;
                }
            }
        }

        if let Some(idx) = found_idx {
            // Already open, just focus it
            state.documents.make_active_by_index(idx);
        } else {
            // Not open, add it
            match state.documents.add_file_path(&file) {
                Ok(_) => {},
                Err(err) => error_log_add(ctx, state, err.into()),
            }
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


// ---------------------------------------------------------
// 4. THE MULTI-TAB BAR
// ---------------------------------------------------------
pub fn draw_tab_bar(ctx: &mut Context, state: &mut State) {
    let doc_count = state.documents.len();
    if doc_count < 2 {
        return; 
    }

    ctx.table_begin("tab_bar");
    ctx.table_next_row();

    // --- THE FIX: Disambiguate Same-Name Tabs ---
    // Count occurrences of each filename
    let mut name_counts = std::collections::HashMap::new();
    for doc in state.documents.iter() {
        *name_counts.entry(doc.filename.as_str()).or_insert(0) += 1;
    }

    let mut clicked_idx = None;

    for (idx, doc) in state.documents.iter().enumerate() {
        let is_active = idx == doc_count - 1; 
        
        let tb = doc.buffer.borrow();
        let dirty_marker = if tb.is_dirty() { "*" } else { "" };
        
        // If multiple files share this name, append the parent folder to the tab
        let is_duplicate_name = name_counts[doc.filename.as_str()] > 1;
        let display_name = if is_duplicate_name && doc.path.is_some() {
            let parent_dir = doc.path.as_ref().unwrap().parent().and_then(|p| p.file_name()).unwrap_or_default();
            format!("{} - {}", doc.filename, parent_dir.to_string_lossy())
        } else {
            doc.filename.clone()
        };

        let label = format!(" {}{} ", display_name, dirty_marker);

        if is_active {
            ctx.attr_background_rgba(ctx.indexed(IndexedColor::BrightBlue));
            ctx.attr_foreground_rgba(ctx.contrasted(ctx.indexed(IndexedColor::BrightBlue)));
        } else {
            ctx.attr_background_rgba(ctx.indexed(IndexedColor::Background));
            ctx.attr_foreground_rgba(ctx.indexed(IndexedColor::Foreground));
        }

        ctx.next_block_id_mixin(idx as u64);
        if ctx.button("tab_button", &label, ButtonStyle::default().bracketed(false)) {
            clicked_idx = Some(idx);
        }
    }

    ctx.table_end();

    if let Some(idx) = clicked_idx {
        state.documents.make_active_by_index(idx);
        ctx.needs_rerender();
    }
}