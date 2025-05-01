use std::cmp::Ordering;
use std::path::{Component, PathBuf};

use edit::framebuffer::IndexedColor;
use edit::helpers::*;
use edit::icu;
use edit::input::vk;
use edit::tui::*;

use crate::documents::*;
use crate::loc::*;
use crate::state::*;

pub fn draw_file_picker(ctx: &mut Context, state: &mut State) {
    let width = (ctx.size().width - 20).max(10);
    let height = (ctx.size().height - 10).max(10);
    let mut doit = None;

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

            ctx.label("dir-label", Overflow::Clip, loc(LocId::SaveAsDialogPathLabel));
            ctx.label("dir", Overflow::TruncateMiddle, state.file_picker_pending_dir.as_str());

            ctx.table_next_row();
            ctx.inherit_focus();

            ctx.label("name-label", Overflow::Clip, loc(LocId::SaveAsDialogNameLabel));
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
        ctx.attr_background_rgba(ctx.indexed_alpha(IndexedColor::Black, 0x3f));
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
                        state.file_picker_pending_name = entry.as_str().to_string()
                    }
                    ListSelection::Activated => activated = true,
                }
            }
            ctx.list_end();
        }
        ctx.scrollarea_end();

        if activated {
            doit = draw_file_picker_update_path(state);

            // Check if the file already exists and show an overwrite warning in that case.
            if state.wants_file_picker == StateFilePicker::SaveAs
                && let Some(path) = doit.as_deref()
                && let Some(doc) = state.documents.active()
                && !doc.path.eq_canonical(path)
                && path.exists()
            {
                state.file_picker_overwrite_warning = doit.take();
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
            ctx.inherit_focus();
            ctx.attr_padding(Rect::three(0, 2, 1));
            ctx.attr_position(Position::Center);
            ctx.table_set_cell_gap(Size { width: 2, height: 0 });
            {
                ctx.table_next_row();
                ctx.inherit_focus();

                save = ctx.button("yes", Overflow::Clip, loc(LocId::Yes));
                ctx.inherit_focus();

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
            doit = state.file_picker_overwrite_warning.take();
        }
    }

    if let Some(path) = doit {
        let res = if state.wants_file_picker == StateFilePicker::SaveAs {
            state.documents.save_active(Some(&path))
        } else {
            state.documents.add_file_path(&path).map(|_| ())
        };
        match res {
            Ok(..) => {
                ctx.needs_rerender();
                state.wants_file_picker = StateFilePicker::None;
            }
            Err(err) => error_log_add(ctx, state, err),
        }
    }
}

// Returns Some(path) if the path refers to a file.
fn draw_file_picker_update_path(state: &mut State) -> Option<PathBuf> {
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
        let name = DocumentManager::get_filename_from_path(&normalized);
        (dir, name)
    };
    if dir != state.file_picker_pending_dir.as_path() {
        state.file_picker_pending_dir = DisplayablePathBuf::new(dir.to_path_buf());
        state.file_picker_entries = None;
    }

    state.file_picker_pending_name = name;
    if state.file_picker_pending_name.is_empty() { None } else { Some(normalized) }
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
            Ordering::Equal => icu::compare_strings(a, b),
            other => other,
        }
    });

    state.file_picker_entries = Some(files);
}
