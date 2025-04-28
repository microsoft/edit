use std::ptr;

use crate::documents::*;
use crate::loc::*;
use crate::state::*;
use edit::framebuffer::IndexedColor;
use edit::helpers::*;
use edit::icu;
use edit::input::vk;
use edit::tui::*;

pub fn draw_statusbar(ctx: &mut Context, state: &mut State) {
    ctx.table_begin("statusbar");
    ctx.attr_background_rgba(state.menubar_color_bg);
    ctx.attr_foreground_rgba(state.menubar_color_fg);
    ctx.table_set_cell_gap(Size {
        width: 2,
        height: 0,
    });
    ctx.attr_intrinsic_size(Size {
        width: COORD_TYPE_SAFE_MAX,
        height: 1,
    });
    ctx.attr_padding(Rect::two(0, 1));

    if let Some(doc) = state.documents.active() {
        let mut tb = doc.buffer.borrow_mut();

        ctx.table_next_row();

        if ctx.button(
            "newline",
            Overflow::Clip,
            if tb.is_crlf() { "CRLF" } else { "LF" },
        ) {
            let is_crlf = tb.is_crlf();
            tb.normalize_newlines(!is_crlf);
        }
        if state.wants_statusbar_focus {
            state.wants_statusbar_focus = false;
            ctx.steal_focus();
        }

        state.wants_encoding_picker |= ctx.button("encoding", Overflow::Clip, tb.encoding());
        if state.wants_encoding_picker {
            if matches!(&doc.path, DocumentPath::Canonical(_)) {
                ctx.block_begin("frame");
                ctx.attr_float(FloatSpec {
                    anchor: Anchor::Last,
                    gravity_x: 0.0,
                    gravity_y: 1.0,
                    offset_x: 0,
                    offset_y: 0,
                });
                ctx.attr_padding(Rect::two(0, 1));
                ctx.attr_border();
                {
                    ctx.list_begin("options");
                    ctx.focus_on_first_present();
                    {
                        if ctx.list_item(false, Overflow::Clip, loc(LocId::EncodingReopen))
                            == ListSelection::Activated
                        {
                            state.wants_encoding_change = StateEncodingChange::Reopen;
                        }
                        if ctx.list_item(false, Overflow::Clip, loc(LocId::EncodingConvert))
                            == ListSelection::Activated
                        {
                            state.wants_encoding_change = StateEncodingChange::Convert;
                        }
                    }
                    ctx.list_end();
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
            Overflow::Clip,
            &format!(
                "{}:{}",
                loc(if tb.indent_with_tabs() {
                    LocId::IndentationTabs
                } else {
                    LocId::IndentationSpaces
                }),
                tb.tab_size(),
            ),
        );
        if state.wants_indentation_picker {
            ctx.table_begin("indentation-picker");
            ctx.attr_float(FloatSpec {
                anchor: Anchor::Last,
                gravity_x: 0.0,
                gravity_y: 1.0,
                offset_x: 0,
                offset_y: 0,
            });
            ctx.attr_border();
            ctx.attr_padding(Rect::two(0, 1));
            ctx.table_set_cell_gap(Size {
                width: 1,
                height: 0,
            });
            {
                if ctx.consume_shortcut(vk::RETURN) {
                    ctx.toss_focus_up();
                }

                ctx.table_next_row();

                ctx.list_begin("type");
                ctx.focus_on_first_present();
                ctx.attr_padding(Rect::two(0, 1));
                {
                    if ctx.list_item(
                        tb.indent_with_tabs(),
                        Overflow::Clip,
                        loc(LocId::IndentationTabs),
                    ) != ListSelection::Unchanged
                    {
                        tb.set_indent_with_tabs(true);
                        ctx.needs_rerender();
                    }
                    if ctx.list_item(
                        !tb.indent_with_tabs(),
                        Overflow::Clip,
                        loc(LocId::IndentationSpaces),
                    ) != ListSelection::Unchanged
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

                        if ctx.list_item(tb.tab_size() == width as i32, Overflow::Clip, label)
                            != ListSelection::Unchanged
                        {
                            tb.set_tab_size(width as i32);
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
            Overflow::Clip,
            &format!(
                "{}:{}",
                tb.get_cursor_logical_pos().y + 1,
                tb.get_cursor_logical_pos().x + 1
            ),
        );

        #[cfg(any(feature = "debug-layout", feature = "debug-latency"))]
        ctx.label(
            "stats",
            Overflow::Clip,
            &format!(
                "{}/{}",
                tb.get_logical_line_count(),
                tb.get_visual_line_count(),
            ),
        );

        if tb.is_overtype() && ctx.button("overtype", Overflow::Clip, "OVR") {
            tb.set_overtype(false);
            ctx.needs_rerender();
        }

        if tb.is_dirty() {
            ctx.label("dirty", Overflow::Clip, "*");
        }

        ctx.block_begin("filename-container");
        ctx.attr_intrinsic_size(Size {
            width: COORD_TYPE_SAFE_MAX,
            height: 1,
        });
        {
            let total = state.documents.len();
            let mut filename = doc.filename.as_str();
            let filename_buf;

            if total > 1 {
                filename_buf = format!("{} + {}", filename, total - 1);
                filename = &filename_buf;
            }

            state.wants_document_picker |=
                ctx.button("filename", Overflow::TruncateMiddle, filename);
            ctx.attr_position(Position::Right);
        }
        ctx.block_end();
    }

    ctx.table_end();
}

pub fn draw_dialog_encoding_change(ctx: &mut Context, state: &mut State) {
    let doc = state.documents.active().unwrap();
    let reopen = state.wants_encoding_change == StateEncodingChange::Reopen;
    let width = (ctx.size().width - 20).max(10);
    let height = (ctx.size().height - 10).max(10);
    let mut change = None;

    ctx.modal_begin(
        "encode",
        if reopen {
            loc(LocId::EncodingReopen)
        } else {
            loc(LocId::EncodingConvert)
        },
    );
    {
        ctx.scrollarea_begin("scrollarea", Size { width, height });
        ctx.attr_background_rgba(ctx.indexed_alpha(IndexedColor::Black, 0x3f));
        ctx.inherit_focus();
        {
            let encodings = icu::get_available_encodings();

            ctx.list_begin("encodings");
            ctx.inherit_focus();
            for encoding in encodings {
                if ctx.list_item(
                    encoding.as_str() == doc.buffer.borrow().encoding(),
                    Overflow::Clip,
                    encoding.as_str(),
                ) == ListSelection::Activated
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
        let mut tb = doc.buffer.borrow_mut();

        if reopen && let DocumentPath::Canonical(path) = &doc.path {
            let mut res = Ok(());
            if tb.is_dirty() {
                res = tb.write_file(path);
            }
            if res.is_ok() {
                res = tb.read_file_path(path, Some(encoding.as_str()));
            }
            if let Err(err) = res {
                drop(tb);
                error_log_add(ctx, state, err);
            }
        } else {
            tb.set_encoding(encoding.as_str());
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
        ctx.attr_background_rgba(ctx.indexed_alpha(IndexedColor::Black, 0x3f));
        ctx.inherit_focus();
        {
            ctx.list_begin("documents");
            ctx.inherit_focus();

            let active = opt_ptr(state.documents.active());

            if state.documents.update_active(|doc| {
                ctx.list_item(
                    ptr::eq(doc, active),
                    Overflow::TruncateMiddle,
                    &doc.filename,
                ) == ListSelection::Activated
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
