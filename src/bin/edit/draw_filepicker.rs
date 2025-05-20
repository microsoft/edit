// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::cmp::Ordering;
use std::fs;
use std::path::PathBuf;

use edit::framebuffer::IndexedColor;
use edit::helpers::*;
use edit::input::vk;
use edit::tui::*;
use edit::{arena, icu, path, sys};
use edit::fuzzy::score_fuzzy;

use crate::localization::*;
use crate::state::*;

pub fn draw_file_picker(ctx: &mut Context, state: &mut State) {
    // The save dialog is pre-filled with the current document filename.
    if state.wants_file_picker == StateFilePicker::SaveAs {
        state.wants_file_picker = StateFilePicker::SaveAsShown;

        if state.file_picker_pending_name.as_os_str().is_empty() {
            state.file_picker_pending_name =
                state.documents.active().map_or("Untitled.txt", |doc| doc.filename.as_str()).into();
        }
    }

    let width = (ctx.size().width - 20).max(10);
    let height = (ctx.size().height - 10).max(10);
    let mut doit = None;
    let mut done = false;

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
        ctx.table_set_cell_gap(Size { width: 1, height: 0 });
        ctx.attr_padding(Rect::two(1, 1));
        ctx.inherit_focus();
        {
            ctx.table_next_row();

            ctx.label("dir-label", loc(LocId::SaveAsDialogPathLabel));
            ctx.label("dir", state.file_picker_pending_dir.as_str());
            ctx.attr_overflow(Overflow::TruncateMiddle);

            ctx.table_next_row();
            ctx.inherit_focus();

            ctx.label("name-label", loc(LocId::SaveAsDialogNameLabel));
            let filename_changed = ctx.editline_begin("name");
            ctx.editline_text(&mut state.file_picker_pending_name);
            
            // Record if the editline is focused for autocomplete display
            let filename_focused = ctx.is_focused();
            
            // Check if the filename has changed to update autocomplete
            if filename_changed && filename_focused {
                // Update autocomplete suggestions whenever the filename changes
                update_filename_autocomplete(state);
            }
            
            // Handle completion with Tab key
            if filename_focused && ctx.consume_shortcut(vk::TAB) && state.file_picker_autocomplete.is_some() {
                if let Some(suggestions) = &state.file_picker_autocomplete {
                    if !suggestions.is_empty() {
                        // Use the first suggestion on Tab
                        state.file_picker_pending_name = suggestions[0].as_path().into();
                        // Clear suggestions after completing
                        state.file_picker_autocomplete = None;
                    }
                }
            }
            
            ctx.editline_end();
            
            // Display autocomplete suggestions as a floating panel
            if filename_focused && let Some(suggestions) = &state.file_picker_autocomplete {
                if !suggestions.is_empty() {
                    ctx.block_begin("autocomplete-panel");
                    ctx.attr_float(FloatSpec {
                        anchor: Anchor::Last,
                        gravity_x: 0.0,
                        gravity_y: 1.0,
                        offset_x: 0.0,
                        offset_y: 1.0,
                    });
                    ctx.attr_border();
                    ctx.attr_background_rgba(ctx.indexed_alpha(IndexedColor::Black, 1, 4));
                    ctx.attr_padding(Rect::two(0, 1));
                    
                    ctx.list_begin("autocomplete-suggestions");
                    for suggestion in suggestions {
                        if ctx.list_item(false, suggestion.as_str()) == ListSelection::Activated {
                            state.file_picker_pending_name = suggestion.as_path().into();
                            // Clear suggestions after selecting one
                            state.file_picker_autocomplete = None;
                        }
                    }
                    ctx.list_end();
                    
                    ctx.block_end();
                }
            }
            
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
        ctx.attr_background_rgba(ctx.indexed_alpha(IndexedColor::Black, 1, 4));
        ctx.next_block_id_mixin(state.file_picker_pending_dir.as_str().len() as u64);
        {
            ctx.list_begin("files");
            ctx.inherit_focus();
            for entry in files {
                match ctx
                    .list_item(state.file_picker_pending_name == entry.as_path(), entry.as_str())
                {
                    ListSelection::Unchanged => {}
                    ListSelection::Selected => {
                        state.file_picker_pending_name = entry.as_path().into()
                    }
                    ListSelection::Activated => activated = true,
                }
                ctx.attr_overflow(Overflow::TruncateMiddle);
            }
            ctx.list_end();

            if ctx.contains_focus() && ctx.consume_shortcut(vk::BACK) {
                state.file_picker_pending_name = "..".into();
                activated = true;
            }
        }
        ctx.scrollarea_end();

        if activated {
            doit = draw_file_picker_update_path(state);

            // Check if the file already exists and show an overwrite warning in that case.
            if state.wants_file_picker != StateFilePicker::Open
                && let Some(path) = doit.as_deref()
                && let Some(doc) = state.documents.active()
                && let Some(file_id) = &doc.file_id
                && sys::file_id_at(path).is_ok_and(|id| &id == file_id)
            {
                state.file_picker_overwrite_warning = doit.take();
            }
        }
    }
    if ctx.modal_end() {
        done = true;
    }

    if state.file_picker_overwrite_warning.is_some() {
        let mut save;

        ctx.modal_begin("overwrite", loc(LocId::FileOverwriteWarning));
        ctx.attr_background_rgba(ctx.indexed(IndexedColor::Red));
        ctx.attr_foreground_rgba(ctx.indexed(IndexedColor::BrightWhite));
        {
            ctx.label("description", loc(LocId::FileOverwriteWarningDescription));
            ctx.attr_overflow(Overflow::TruncateTail);
            ctx.attr_padding(Rect::three(1, 2, 1));

            ctx.table_begin("choices");
            ctx.inherit_focus();
            ctx.attr_padding(Rect::three(0, 2, 1));
            ctx.attr_position(Position::Center);
            ctx.table_set_cell_gap(Size { width: 2, height: 0 });
            {
                ctx.table_next_row();
                ctx.inherit_focus();

                save = ctx.button("yes", loc(LocId::Yes));
                ctx.inherit_focus();

                if ctx.button("no", loc(LocId::No)) {
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
            doit = state.file_picker_overwrite_warning.take();
        }
    }

    if let Some(path) = doit {
        let res = if state.wants_file_picker == StateFilePicker::Open {
            state.documents.add_file_path(&path).map(|_| ())
        } else if let Some(doc) = state.documents.active_mut() {
            doc.save(Some(path))
        } else {
            Ok(())
        };
        match res {
            Ok(..) => {
                ctx.needs_rerender();
                done = true;
            }
            Err(err) => error_log_add(ctx, state, err),
        }
    }

    if done {
        state.wants_file_picker = StateFilePicker::None;
        state.file_picker_pending_name = Default::default();
        state.file_picker_entries = Default::default();
        state.file_picker_overwrite_warning = Default::default();
        state.file_picker_autocomplete = Default::default();
    }
}

// Returns Some(path) if the path refers to a file.
fn draw_file_picker_update_path(state: &mut State) -> Option<PathBuf> {
    let path = state.file_picker_pending_dir.as_path();
    let path = path.join(&state.file_picker_pending_name);
    let path = path::normalize(&path);

    let (dir, name) = if path.is_dir() {
        (path.as_path(), PathBuf::new())
    } else {
        let dir = path.parent().unwrap_or(&path);
        let name = path.file_name().map_or(Default::default(), |s| s.into());
        (dir, name)
    };
    if dir != state.file_picker_pending_dir.as_path() {
        state.file_picker_pending_dir = DisplayablePathBuf::new(dir.to_path_buf());
        state.file_picker_entries = None;
        state.file_picker_autocomplete = None;
    }

    state.file_picker_pending_name = name;
    
    // Update autocomplete suggestions after directory/name changes
    if !state.file_picker_pending_name.as_os_str().is_empty() {
        update_filename_autocomplete(state);
    }
    if state.file_picker_pending_name.as_os_str().is_empty() { None } else { Some(path) }
}

fn draw_dialog_saveas_refresh_files(state: &mut State) {
    let dir = state.file_picker_pending_dir.as_path();
    let mut files = Vec::new();

    if dir.parent().is_some() {
        files.push(DisplayablePathBuf::from(".."));
    }

    if let Ok(iter) = fs::read_dir(dir) {
        for entry in iter.flatten() {
            if let Ok(metadata) = entry.metadata() {
                let mut name = entry.file_name();
                if metadata.is_dir()
                    || (metadata.is_symlink()
                        && fs::metadata(entry.path()).is_ok_and(|m| m.is_dir()))
                {
                    name.push("/");
                }
                files.push(DisplayablePathBuf::from(name));
            }
        }
    }

    // Sort directories first, then by name, case-insensitive.
    let off = files.len().saturating_sub(1);
    files[off..].sort_by(|a, b| {
        let a = a.as_bytes();
        let b = b.as_bytes();

        let a_is_dir = a.last() == Some(&b'/');
        let b_is_dir = b.last() == Some(&b'/');

        match b_is_dir.cmp(&a_is_dir) {
            Ordering::Equal => icu::compare_strings(a, b),
            other => other,
        }
    });

    state.file_picker_entries = Some(files);
    
    // Update autocomplete suggestions after refreshing file list
    if !state.file_picker_pending_name.as_os_str().is_empty() {
        update_filename_autocomplete(state);
    }
}

// Updates the autocomplete suggestions based on the current input
fn update_filename_autocomplete(state: &mut State) {
    // Don't show autocomplete suggestions if the filename is empty
    if state.file_picker_pending_name.as_os_str().is_empty() {
        state.file_picker_autocomplete = None;
        return;
    }
    
    let filename_input = state.file_picker_pending_name.to_string_lossy().to_string();
    
    // Don't show autocomplete for directory navigation
    if filename_input == ".." || filename_input.ends_with('/') || filename_input.ends_with('\\') {
        state.file_picker_autocomplete = None;
        return;
    }
    
    if let Some(files) = &state.file_picker_entries {
        let scratch = arena::scratch_arena(None);
        let mut matches = Vec::new();
        
        for entry in files {
            let entry_str = entry.as_str();
            
            // Don't include directories in autocomplete
            if entry_str.ends_with('/') || entry_str == ".." {
                continue;
            }
            
            // Score using fuzzy matching
            let (score, _) = score_fuzzy(&scratch, entry_str, &filename_input, true);
            
            // If there's a match (score > 0), add it to our suggestions
            if score > 0 {
                matches.push(entry.clone());
            }
        }
        
        // Sort matches by their score in descending order
        matches.sort_by(|a, b| {
            let (score_a, _) = score_fuzzy(&scratch, a.as_str(), &filename_input, true);
            let (score_b, _) = score_fuzzy(&scratch, b.as_str(), &filename_input, true);
            
            // Higher scores come first
            score_b.cmp(&score_a)
        });
        
        // Limit the number of suggestions
        let max_suggestions = 5;
        if matches.len() > max_suggestions {
            matches.truncate(max_suggestions);
        }
        
        state.file_picker_autocomplete = if matches.is_empty() { None } else { Some(matches) };
    } else {
        state.file_picker_autocomplete = None;
    }
}
