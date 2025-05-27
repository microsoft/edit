// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use edit::arena_format;
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
        if state.documents.active().is_some() && ctx.menubar_menu_begin(loc(LocId::Edit), 'E') {
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
    }
    if ctx.menubar_menu_button(loc(LocId::FileClose), 'C', kbmod::CTRL | vk::W) {
        state.wants_close = true;
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
        ctx.set_clipboard(tb.extract_selection(true));
    }
    if ctx.menubar_menu_button(loc(LocId::EditCopy), 'C', kbmod::CTRL | vk::C) {
        ctx.set_clipboard(tb.extract_selection(false));
    }
    if ctx.menubar_menu_button(loc(LocId::EditPaste), 'P', kbmod::CTRL | vk::V) {
        tb.write(ctx.clipboard(), true);
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
    ctx.menubar_menu_end();
}

fn draw_menu_view(ctx: &mut Context, state: &mut State) {
    if ctx.menubar_menu_button(loc(LocId::ViewFocusStatusbar), 'S', vk::NULL) {
        state.wants_statusbar_focus = true;
    }

    if let Some(doc) = state.documents.active() {
        let mut tb = doc.buffer.borrow_mut();
        let word_wrap = tb.is_word_wrap_enabled();

        if ctx.menubar_menu_button(loc(LocId::ViewDocumentPicker), 'P', kbmod::CTRL | vk::P) {
            state.wants_document_picker = true;
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
