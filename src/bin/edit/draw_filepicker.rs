// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::cmp::Ordering;
use std::fs;
use std::path::{Path, PathBuf};

use edit::framebuffer::IndexedColor;
use edit::helpers::*;
use edit::input::vk;
use edit::tui::*;
use edit::{icu, path};

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
        ctx.attr_background_rgba(ctx.indexed_alpha(IndexedColor::Black, 1, 4));
        ctx.next_block_id_mixin(state.file_picker_pending_dir_revision);
        {
            ctx.list_begin("files");
            ctx.inherit_focus();
            for entry in files {
                match ctx.list_item(false, entry.as_str()) {
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
                && path.exists()
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
            let contains_focus = ctx.contains_focus();

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

                save = ctx.button("yes", loc(LocId::Yes), ButtonStyle::default());
                ctx.inherit_focus();

                if ctx.button("no", loc(LocId::No), ButtonStyle::default()) {
                    state.file_picker_overwrite_warning = None;
                }
            }
            ctx.table_end();

            if contains_focus {
                save |= ctx.consume_shortcut(vk::Y);
                if ctx.consume_shortcut(vk::N) {
                    state.file_picker_overwrite_warning = None;
                }
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
    }
}

// Returns Some(path) if the path refers to a file.
fn draw_file_picker_update_path(state: &mut State) -> Option<PathBuf> {
    let old_path = state.file_picker_pending_dir.as_path();
    let path = old_path.join(&state.file_picker_pending_name);
    let path = path::normalize(&path);

    let (dir, name) = if path.is_dir() {
        // If the current path is C:\ and the user selects "..", we want to
        // navigate to the drive picker. Since `path::normalize` will turn C:\.. into C:\,
        // we can detect this by checking if the length of the path didn't change.
        let dir = if cfg!(windows)
            && state.file_picker_pending_name == Path::new("..")
            // It's unnecessary to check the contents of the paths.
            && old_path.as_os_str().len() == path.as_os_str().len()
        {
            Path::new("")
        } else {
            path.as_path()
        };
        (dir, PathBuf::new())
    } else {
        let dir = path.parent().unwrap_or(&path);
        let name = path.file_name().map_or(Default::default(), |s| s.into());
        (dir, name)
    };
    if dir != state.file_picker_pending_dir.as_path() {
        state.file_picker_pending_dir = DisplayablePathBuf::from_path(dir.to_path_buf());
        state.file_picker_entries = None;
    }

    state.file_picker_pending_name = name;
    if state.file_picker_pending_name.as_os_str().is_empty() { None } else { Some(path) }
}

fn draw_dialog_saveas_refresh_files(state: &mut State) {
    let dir = state.file_picker_pending_dir.as_path();
    let mut files = Vec::new();
    let mut off = 0;

    #[cfg(windows)]
    if dir.as_os_str().is_empty() {
        // If the path is empty, we are at the drive picker.
        // Add all drives as entries.
        for drive in edit::sys::drives() {
            files.push(DisplayablePathBuf::from_string(format!("{drive}:\\")));
        }

        state.file_picker_entries = Some(files);
        return;
    }

    if cfg!(windows) || dir.parent().is_some() {
        files.push(DisplayablePathBuf::from(".."));
        off = 1;
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
}
