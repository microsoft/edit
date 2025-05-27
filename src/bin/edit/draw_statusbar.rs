// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use edit::framebuffer::{Attributes, IndexedColor};
use edit::helpers::*;
use edit::input::vk;
use edit::tui::*;
use edit::{arena_format, icu};

use crate::localization::*;
use crate::state::*;

pub fn draw_statusbar(ctx: &mut Context, state: &mut State) {
    ctx.table_begin("statusbar");
    ctx.attr_focus_well();
    ctx.attr_background_rgba(state.menubar_color_bg);
    ctx.attr_foreground_rgba(state.menubar_color_fg);
    ctx.table_set_cell_gap(Size { width: 2, height: 0 });
    ctx.attr_intrinsic_size(Size { width: COORD_TYPE_SAFE_MAX, height: 1 });
    ctx.attr_padding(Rect::two(0, 1));

    if let Some(doc) = state.documents.active() {
        let mut tb = doc.buffer.borrow_mut();

        ctx.table_next_row();

        if ctx.button("newline", if tb.is_crlf() { "CRLF" } else { "LF" }, ButtonStyle::default()) {
            let is_crlf = tb.is_crlf();
            tb.normalize_newlines(!is_crlf);
        }
        if state.wants_statusbar_focus {
            state.wants_statusbar_focus = false;
            ctx.steal_focus();
        }

        state.wants_encoding_picker |=
            ctx.button("encoding", tb.encoding(), ButtonStyle::default());
        if state.wants_encoding_picker {
            if doc.path.is_some() {
                ctx.block_begin("frame");
                ctx.attr_float(FloatSpec {
                    anchor: Anchor::Last,
                    gravity_x: 0.0,
                    gravity_y: 1.0,
                    offset_x: 0.0,
                    offset_y: 0.0,
                });
                ctx.attr_padding(Rect::two(0, 1));
                ctx.attr_border();
                {
                    if ctx.button("reopen", loc(LocId::EncodingReopen), ButtonStyle::default()) {
                        state.wants_encoding_change = StateEncodingChange::Reopen;
                    }
                    ctx.focus_on_first_present();
                    if ctx.button("convert", loc(LocId::EncodingConvert), ButtonStyle::default()) {
                        state.wants_encoding_change = StateEncodingChange::Convert;
                    }
                }
                ctx.block_end();
            } else {
                // Can't reopen a file that doesn't exist.
                state.wants_encoding_change = StateEncodingChange::Convert;
            }

            if !ctx.contains_focus() {
                state.wants_encoding_picker = false;
                ctx.needs_rerender();
            }
        }

        state.wants_indentation_picker |= ctx.button(
            "indentation",
            &arena_format!(
                ctx.arena(),
                "{}:{}",
                loc(if tb.indent_with_tabs() {
                    LocId::IndentationTabs
                } else {
                    LocId::IndentationSpaces
                }),
                tb.tab_size(),
            ),
            ButtonStyle::default(),
        );
        if state.wants_indentation_picker {
            ctx.table_begin("indentation-picker");
            ctx.attr_float(FloatSpec {
                anchor: Anchor::Last,
                gravity_x: 0.0,
                gravity_y: 1.0,
                offset_x: 0.0,
                offset_y: 0.0,
            });
            ctx.attr_border();
            ctx.attr_padding(Rect::two(0, 1));
            ctx.table_set_cell_gap(Size { width: 1, height: 0 });
            {
                if ctx.contains_focus() && ctx.consume_shortcut(vk::RETURN) {
                    ctx.toss_focus_up();
                }

                ctx.table_next_row();

                ctx.list_begin("type");
                ctx.focus_on_first_present();
                ctx.attr_padding(Rect::two(0, 1));
                {
                    if ctx.list_item(tb.indent_with_tabs(), loc(LocId::IndentationTabs))
                        != ListSelection::Unchanged
                    {
                        tb.set_indent_with_tabs(true);
                        ctx.needs_rerender();
                    }
                    if ctx.list_item(!tb.indent_with_tabs(), loc(LocId::IndentationSpaces))
                        != ListSelection::Unchanged
                    {
                        tb.set_indent_with_tabs(false);
                        ctx.needs_rerender();
                    }
                }
                ctx.list_end();

                ctx.list_begin("width");
                ctx.attr_padding(Rect::two(0, 2));
                {
                    for width in 1u8..=8 {
                        let ch = [b'0' + width];
                        let label = unsafe { std::str::from_utf8_unchecked(&ch) };

                        if ctx.list_item(tb.tab_size() == width as CoordType, label)
                            != ListSelection::Unchanged
                        {
                            tb.set_tab_size(width as CoordType);
                            ctx.needs_rerender();
                        }
                    }
                }
                ctx.list_end();
            }
            ctx.table_end();

            if !ctx.contains_focus() {
                state.wants_indentation_picker = false;
                ctx.needs_rerender();
            }
        }

        ctx.label(
            "location",
            &arena_format!(
                ctx.arena(),
                "{}:{}",
                tb.cursor_logical_pos().y + 1,
                tb.cursor_logical_pos().x + 1
            ),
        );

        #[cfg(any(feature = "debug-layout", feature = "debug-latency"))]
        ctx.label(
            "stats",
            &arena_format!(ctx.arena(), "{}/{}", tb.logical_line_count(), tb.visual_line_count(),),
        );

        if tb.is_overtype() && ctx.button("overtype", "OVR", ButtonStyle::default()) {
            tb.set_overtype(false);
            ctx.needs_rerender();
        }

        if tb.is_dirty() {
            ctx.label("dirty", "*");
        }

        ctx.block_begin("filename-container");
        ctx.attr_intrinsic_size(Size { width: COORD_TYPE_SAFE_MAX, height: 1 });
        {
            let total = state.documents.len();
            let mut filename = doc.filename.as_str();
            let filename_buf;

            if total > 1 {
                filename_buf = arena_format!(ctx.arena(), "{} + {}", filename, total - 1);
                filename = &filename_buf;
            }

            state.wants_document_picker |= ctx.button("filename", filename, ButtonStyle::default());
            ctx.inherit_focus();
            ctx.attr_overflow(Overflow::TruncateMiddle);
            ctx.attr_position(Position::Right);
        }
        ctx.block_end();
    }

    ctx.table_end();
}

pub fn draw_dialog_encoding_change(ctx: &mut Context, state: &mut State) {
    let doc = state.documents.active_mut().unwrap();
    let reopen = state.wants_encoding_change == StateEncodingChange::Reopen;
    let width = (ctx.size().width - 20).max(10);
    let height = (ctx.size().height - 10).max(10);
    let mut change = None;

    ctx.modal_begin(
        "encode",
        if reopen { loc(LocId::EncodingReopen) } else { loc(LocId::EncodingConvert) },
    );
    {
        ctx.scrollarea_begin("scrollarea", Size { width, height });
        ctx.attr_background_rgba(ctx.indexed_alpha(IndexedColor::Black, 1, 4));
        ctx.inherit_focus();
        {
            let encodings = icu::get_available_encodings();

            ctx.list_begin("encodings");
            ctx.inherit_focus();
            for &encoding in encodings {
                if ctx.list_item(encoding == doc.buffer.borrow().encoding(), encoding)
                    == ListSelection::Activated
                {
                    change = Some(encoding);
                    break;
                }
            }
            ctx.list_end();
        }
        ctx.scrollarea_end();
    }
    if ctx.modal_end() {
        state.wants_encoding_change = StateEncodingChange::None;
    }

    if let Some(encoding) = change {
        if reopen && doc.path.is_some() {
            let mut res = Ok(());
            if doc.buffer.borrow().is_dirty() {
                res = doc.save(None);
            }
            if res.is_ok() {
                res = doc.reread(Some(encoding));
            }
            if let Err(err) = res {
                error_log_add(ctx, state, err);
            }
        } else {
            doc.buffer.borrow_mut().set_encoding(encoding);
        }

        state.wants_encoding_change = StateEncodingChange::None;
        ctx.needs_rerender();
    }
}

pub fn draw_document_picker(ctx: &mut Context, state: &mut State) {
    ctx.modal_begin("document-picker", "");
    {
        let width = (ctx.size().width - 20).max(10);
        let height = (ctx.size().height - 10).max(10);

        ctx.scrollarea_begin("scrollarea", Size { width, height });
        ctx.attr_background_rgba(ctx.indexed_alpha(IndexedColor::Black, 1, 4));
        ctx.inherit_focus();
        {
            ctx.list_begin("documents");
            ctx.inherit_focus();

            if state.documents.update_active(|doc| {
                let tb = doc.buffer.borrow();

                ctx.styled_list_item_begin();
                ctx.attr_overflow(Overflow::TruncateTail);
                ctx.styled_label_add_text(if tb.is_dirty() { "* " } else { "  " });
                ctx.styled_label_add_text(&doc.filename);

                if let Some(path) = &doc.dir {
                    ctx.styled_label_add_text("   ");
                    ctx.styled_label_set_attributes(Attributes::Italic);
                    ctx.styled_label_add_text(path.as_str());
                }

                ctx.styled_list_item_end(false) == ListSelection::Activated
            }) {
                state.wants_document_picker = false;
                ctx.needs_rerender();
            }

            ctx.list_end();
        }
        ctx.scrollarea_end();
    }
    if ctx.modal_end() {
        state.wants_document_picker = false;
    }
}
