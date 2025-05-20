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
            // 为输入框混入一个固定的 ID，确保跨帧保持一致，避免焦点丢失
            ctx.next_block_id_mixin(0);
            let filename_changed = ctx.editline("name", &mut state.file_picker_pending_name);
            // 第一次出现时自动聚焦，以后不再反复篡改焦点
            ctx.focus_on_first_present();
            
            // 继续检查焦点（此时 last_node 仍是输入框）
            let filename_focused = ctx.is_focused();
            
            if filename_changed && filename_focused {
                update_filename_autocomplete(state);
            }
            
            if filename_focused {
                if ctx.consume_shortcut(vk::TAB) {
                    if let Some(suggestions) = &state.file_picker_autocomplete {
                        if !suggestions.is_empty() {
                            let first_suggestion = suggestions[0].clone();
                            state.file_picker_pending_name = first_suggestion.as_path().into();
                            state.file_picker_autocomplete = None;
                            ctx.needs_rerender();
                        }
                    }
                }
            }
            
            // 始终在输入框之后插入一个 gap 行（高度 1），作为面板锚点 / 占位，保持布局稳定
            ctx.table_next_row();
            ctx.block_begin("gap");
            ctx.attr_intrinsic_size(Size { width: 0, height: 1 });
            ctx.attr_background_rgba(ctx.indexed_alpha(IndexedColor::Black, 1, 4));
            ctx.block_end();

            // 现在 last_node 是 gap 行。若需要显示补全面板，将其浮动在 gap 行正上方
            if filename_focused && state.file_picker_autocomplete.as_ref().map_or(false, |s| !s.is_empty()) {
                if let Some(suggestions) = &state.file_picker_autocomplete {
                    ctx.block_begin("autocomplete-panel");
                    ctx.attr_float(FloatSpec {
                        anchor: Anchor::Last,   // 锚定 gap 行
                        gravity_x: 0.0,
                        gravity_y: 0.0,        // 面板顶边 == gap 行顶边
                        offset_x: 0.0,
                        offset_y: 0.0,
                    });
                    ctx.attr_border();
                    ctx.attr_background_rgba(ctx.indexed_alpha(IndexedColor::Black, 1, 4));
                    ctx.attr_padding(Rect { left: 1, top: 0, right: 1, bottom: 0 });

                    ctx.table_begin("suggestions");
                    ctx.table_set_columns(&[0]);
                    for suggestion in suggestions.clone() {
                        ctx.table_next_row();
                        ctx.block_begin("item");
                        ctx.label("label", suggestion.as_str());
                        if ctx.was_mouse_down() {
                            state.file_picker_pending_name = suggestion.as_path().into();
                            state.file_picker_autocomplete = None;
                            ctx.needs_rerender();
                        }
                        ctx.block_end();
                    }
                    ctx.table_end();
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
    
    update_autocomplete_if_needed(state);
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
    update_autocomplete_if_needed(state);
}

// 优化自动补全更新逻辑，避免重复调用和不必要的更新
fn update_autocomplete_if_needed(state: &mut State) {
    // 仅当文件名非空且不在最近更新过时才更新
    if !state.file_picker_pending_name.as_os_str().is_empty() {
        update_filename_autocomplete(state);
    } else {
        // 文件名为空时清除自动补全
        state.file_picker_autocomplete = None;
    }
}

// 更改为使用前缀匹配的函数实现
fn update_filename_autocomplete(state: &mut State) {
    // 文件名为空时不显示自动补全建议
    if state.file_picker_pending_name.as_os_str().is_empty() {
        state.file_picker_autocomplete = None;
        return;
    }
    
    let filename_input = state.file_picker_pending_name.to_string_lossy().to_string().to_lowercase();
    
    // 不为目录导航显示自动补全
    if filename_input == ".." || filename_input.ends_with('/') || filename_input.ends_with('\\') {
        state.file_picker_autocomplete = None;
        return;
    }
    
    // 只有当有文件列表时才进行匹配
    if let Some(files) = &state.file_picker_entries {
        let mut matches = Vec::new();
        
        // 收集所有匹配项
        for entry in files {
            let entry_str = entry.as_str();
            
            // 不包括目录在自动补全中
            if entry_str.ends_with('/') || entry_str == ".." {
                continue;
            }
            
            // 使用前缀匹配和包含匹配
            let entry_lower = entry_str.to_lowercase();
            let match_score = if entry_lower.starts_with(&filename_input) {
                // 前缀匹配得分高
                100 - entry_lower.len() as i32 // 越短的匹配越靠前
            } else if entry_lower.contains(&filename_input) {
                // 包含匹配得分低
                50 - entry_lower.len() as i32
            } else {
                // 不匹配
                0
            };
            
            // 如果有匹配，将其添加到建议中
            if match_score > 0 {
                matches.push((entry.clone(), match_score));
            }
        }
        
        // 按分数排序
        matches.sort_by(|a, b| b.1.cmp(&a.1)); // 按降序排序分数
        
        // 提取排序后的条目
        let matches: Vec<DisplayablePathBuf> = matches.into_iter().map(|(entry, _)| entry).collect();
        
        // 限制建议数量
        let max_suggestions = 5;
        let matches = if matches.len() > max_suggestions {
            matches[..max_suggestions].to_vec()
        } else {
            matches
        };
        
        // 更新自动补全状态
        state.file_picker_autocomplete = if matches.is_empty() { None } else { Some(matches) };
    } else {
        state.file_picker_autocomplete = None;
    }
}
