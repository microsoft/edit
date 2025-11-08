// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use edit::arena_format;
use edit::framebuffer::IndexedColor;
use edit::helpers::*;
use edit::input::{kbmod, vk};
use edit::tui::*;

use crate::localization::*;
use crate::state::*;

pub fn draw_menubar(ctx: &mut Context, state: &mut State) {
    ctx.menubar_begin();
    ctx.attr_background_rgba(state.menubar_color_bg);
    ctx.attr_foreground_rgba(state.menubar_color_fg);
    {
        let contains_focus = ctx.contains_focus();

        if ctx.menubar_menu_begin(loc(LocId::File), 'F') {
            draw_menu_file(ctx, state);
        }
        if !contains_focus && ctx.consume_shortcut(vk::F10) {
            ctx.steal_focus();
        }
        if state.documents.active().is_some() {
            if ctx.menubar_menu_begin(loc(LocId::Edit), 'E') {
                draw_menu_edit(ctx, state);
            }
            if ctx.menubar_menu_begin(loc(LocId::View), 'V') {
                draw_menu_view(ctx, state);
            }
        }
        if ctx.menubar_menu_begin(loc(LocId::Help), 'H') {
            draw_menu_help(ctx, state);
        }
    }
    ctx.menubar_end();
}

pub fn draw_tabstrip(ctx: &mut Context, state: &mut State) {
    if state.documents.len() <= 1 {
        return;
    }

    let mut tabs = Vec::new();
    for doc in state.documents.iter() {
        let dirty = doc.buffer.borrow().is_dirty();
        tabs.push((doc.id, doc.filename.clone(), dirty));
    }

    let active_id = state.documents.active().map(|doc| doc.id);

    ctx.block_begin("tabstrip");
    ctx.attr_background_rgba(ctx.indexed_alpha(IndexedColor::Background, 5, 6));
    ctx.attr_padding(Rect::three(0, 1, 0));
    let columns = vec![0; tabs.len().max(1)];
    ctx.table_begin("tabstrip-row");
    ctx.table_set_columns(&columns);
    ctx.table_next_row();
    ctx.table_set_cell_gap(Size { width: 1, height: 0 });
    for (id, title, dirty) in tabs {
        ctx.next_block_id_mixin(id);
        if Some(id) == active_id {
            ctx.attr_background_rgba(state.menubar_color_bg);
            ctx.attr_foreground_rgba(state.menubar_color_fg);
        } else {
            ctx.attr_background_rgba(ctx.indexed(IndexedColor::Background));
            ctx.attr_foreground_rgba(ctx.indexed(IndexedColor::Foreground));
        }

        let mut label = title.clone();
        if dirty {
            label.push('*');
        }

        if ctx.button("tab", &label, ButtonStyle::default()) {
            if state.documents.activate(id) {
                ctx.needs_rerender();
            }
        }
    }
    ctx.table_end();
    ctx.block_end();
}

fn draw_menu_file(ctx: &mut Context, state: &mut State) {
    if ctx.menubar_menu_button(loc(LocId::FileNew), 'N', kbmod::CTRL | vk::N) {
        draw_add_untitled_document(ctx, state);
    }
    if ctx.menubar_menu_button(loc(LocId::FileOpen), 'O', kbmod::CTRL | vk::O) {
        state.wants_file_picker = StateFilePicker::Open;
    }
    if state.documents.active().is_some() {
        if ctx.menubar_menu_button(loc(LocId::FileSave), 'S', kbmod::CTRL | vk::S) {
            state.wants_save = true;
        }
        if ctx.menubar_menu_button(loc(LocId::FileSaveAs), 'A', vk::NULL) {
            state.wants_file_picker = StateFilePicker::SaveAs;
        }
        if ctx.menubar_menu_button(loc(LocId::FileClose), 'C', kbmod::CTRL | vk::W) {
            state.wants_close = true;
        }
    }
    if ctx.menubar_menu_button(loc(LocId::FileExit), 'X', kbmod::CTRL | vk::Q) {
        state.wants_exit = true;
    }
    ctx.menubar_menu_end();
}

fn draw_menu_edit(ctx: &mut Context, state: &mut State) {
    let doc = state.documents.active().unwrap();
    let mut tb = doc.buffer.borrow_mut();

    if ctx.menubar_menu_button(loc(LocId::EditUndo), 'U', kbmod::CTRL | vk::Z) {
        tb.undo();
        ctx.needs_rerender();
    }
    if ctx.menubar_menu_button(loc(LocId::EditRedo), 'R', kbmod::CTRL | vk::Y) {
        tb.redo();
        ctx.needs_rerender();
    }
    if ctx.menubar_menu_button(loc(LocId::EditCut), 'T', kbmod::CTRL | vk::X) {
        tb.cut(ctx.clipboard_mut());
        ctx.needs_rerender();
    }
    if ctx.menubar_menu_button(loc(LocId::EditCopy), 'C', kbmod::CTRL | vk::C) {
        tb.copy(ctx.clipboard_mut());
        ctx.needs_rerender();
    }
    if ctx.menubar_menu_button(loc(LocId::EditPaste), 'P', kbmod::CTRL | vk::V) {
        tb.paste(ctx.clipboard_ref());
        ctx.needs_rerender();
    }
    if state.wants_search.kind != StateSearchKind::Disabled {
        if ctx.menubar_menu_button(loc(LocId::EditFind), 'F', kbmod::CTRL | vk::F) {
            state.wants_search.kind = StateSearchKind::Search;
            state.wants_search.focus = true;
        }
        if ctx.menubar_menu_button(loc(LocId::EditReplace), 'L', kbmod::CTRL | vk::R) {
            state.wants_search.kind = StateSearchKind::Replace;
            state.wants_search.focus = true;
        }
    }
    if ctx.menubar_menu_button(loc(LocId::EditSelectAll), 'A', kbmod::CTRL | vk::A) {
        tb.select_all();
        ctx.needs_rerender();
    }
    if ctx.menubar_menu_button(loc(LocId::EditPreferences), 'R', vk::NULL) {
        state.wants_preferences = true;
    }
    ctx.menubar_menu_end();
}

fn draw_menu_view(ctx: &mut Context, state: &mut State) {
    if let Some(doc) = state.documents.active() {
        let mut tb = doc.buffer.borrow_mut();
        let word_wrap = tb.is_word_wrap_enabled();

        // All values on the statusbar are currently document specific.
        if ctx.menubar_menu_button(loc(LocId::ViewFocusStatusbar), 'S', vk::NULL) {
            state.wants_statusbar_focus = true;
        }
        if ctx.menubar_menu_button(loc(LocId::ViewGoToFile), 'F', kbmod::CTRL | vk::P) {
            state.wants_go_to_file = true;
        }
        if ctx.menubar_menu_button(loc(LocId::FileGoto), 'G', kbmod::CTRL | vk::G) {
            state.wants_goto = true;
        }
        if ctx.menubar_menu_checkbox(loc(LocId::ViewWordWrap), 'W', kbmod::ALT | vk::Z, word_wrap) {
            tb.set_word_wrap(!word_wrap);
            ctx.needs_rerender();
        }
    }

    ctx.menubar_menu_end();
}

fn draw_menu_help(ctx: &mut Context, state: &mut State) {
    if ctx.menubar_menu_button(loc(LocId::HelpAbout), 'A', vk::NULL) {
        state.wants_about = true;
    }
    ctx.menubar_menu_end();
}

pub fn draw_dialog_about(ctx: &mut Context, state: &mut State) {
    ctx.modal_begin("about", loc(LocId::AboutDialogTitle));
    {
        ctx.block_begin("content");
        ctx.inherit_focus();
        ctx.attr_padding(Rect::three(1, 2, 1));
        {
            ctx.label("description", "Microsoft Edit");
            ctx.attr_overflow(Overflow::TruncateTail);
            ctx.attr_position(Position::Center);

            ctx.label(
                "version",
                &arena_format!(
                    ctx.arena(),
                    "{}{}",
                    loc(LocId::AboutDialogVersion),
                    env!("CARGO_PKG_VERSION")
                ),
            );
            ctx.attr_overflow(Overflow::TruncateHead);
            ctx.attr_position(Position::Center);

            ctx.label("copyright", "Copyright (c) Microsoft Corp 2025");
            ctx.attr_overflow(Overflow::TruncateTail);
            ctx.attr_position(Position::Center);

            ctx.block_begin("choices");
            ctx.inherit_focus();
            ctx.attr_padding(Rect::three(1, 2, 0));
            ctx.attr_position(Position::Center);
            {
                if ctx.button("ok", loc(LocId::Ok), ButtonStyle::default()) {
                    state.wants_about = false;
                }
                ctx.inherit_focus();
            }
            ctx.block_end();
        }
        ctx.block_end();
    }
    if ctx.modal_end() {
        state.wants_about = false;
    }
}
