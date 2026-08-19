// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use edit::buffer::RcTextBuffer;
use edit::framebuffer::{Attributes, IndexedColor};
use edit::helpers::*;
use edit::tui::*;

use crate::markdown_preview::{InlineStyle, MarkdownPreview, RenderedLineKind};

pub fn draw_markdown_preview(
    ctx: &mut Context,
    buffer: &RcTextBuffer,
    preview: &mut MarkdownPreview,
    height: CoordType,
) {
    let width = (ctx.size().width - 3).max(2);
    preview.prepare(buffer, width);
    let reset_scroll = preview.take_scroll_reset();

    ctx.scrollarea_begin("markdown-preview", Size { width: 0, height });
    ctx.focus_on_first_present();
    ctx.attr_padding(Rect::two(0, 1));
    if reset_scroll {
        ctx.scrollarea_scroll_to(Point::default());
    }
    {
        for (index, line) in preview.lines().iter().enumerate() {
            ctx.next_block_id_mixin(index as u64);
            ctx.styled_label_begin("line");

            let mut style = None;
            for span in &line.spans {
                if style != Some(span.style) {
                    style = Some(span.style);
                    ctx.styled_label_set_attributes(attributes(span.style));
                }
                ctx.styled_label_add_text(&span.text);
            }

            ctx.styled_label_end();

            match line.kind {
                RenderedLineKind::Text => {}
                RenderedLineKind::Heading(level) => {
                    let color =
                        if level <= 2 { IndexedColor::BrightCyan } else { IndexedColor::Cyan };
                    ctx.attr_foreground_rgba(ctx.indexed(color));
                }
                RenderedLineKind::Quote => {
                    ctx.attr_foreground_rgba(ctx.indexed(IndexedColor::BrightGreen));
                }
                RenderedLineKind::Rule => {
                    ctx.attr_foreground_rgba(ctx.indexed(IndexedColor::BrightBlack));
                }
                RenderedLineKind::Code => {
                    ctx.attr_background_rgba(ctx.indexed_alpha(IndexedColor::Black, 1, 4));
                    ctx.attr_foreground_rgba(ctx.indexed(IndexedColor::BrightYellow));
                }
                RenderedLineKind::Unavailable => {
                    ctx.attr_foreground_rgba(ctx.indexed(IndexedColor::BrightYellow));
                }
            }
        }
    }
    ctx.scrollarea_end();
}

fn attributes(style: InlineStyle) -> Attributes {
    let mut attributes = Attributes::None;

    if style.is_bold() {
        attributes = attributes | Attributes::Bold;
    }
    if style.is_italic() || style.is_code() {
        attributes = attributes | Attributes::Italic;
    }
    if style.is_strikethrough() {
        attributes = attributes | Attributes::Strikethrough;
    }
    if style.is_link() || style.is_code() {
        attributes = attributes | Attributes::Underlined;
    }

    attributes
}
