// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use edit::buffer::TextBuffer;
use edit::framebuffer::IndexedColor;
use edit::helpers::*;
use edit::input::vk;
use edit::tui::*;

use crate::localization::*;
use crate::state::*;

pub fn draw_dialog_boundary_align(ctx: &mut Context, state: &mut State) {
    // EN: Ask for a positive boundary column, defaulting to 80, before changing the document.
    // 中文：修改文件前要求輸入正整數邊界欄位，預設值為 80。
    let mut apply;
    let mut cancel;

    ctx.modal_begin("boundary-align", loc(LocId::BoundaryAlignDialogTitle));
    {
        ctx.block_begin("boundary-align-content");
        ctx.inherit_focus();
        ctx.attr_padding(Rect::three(1, 2, 1));
        {
            let contains_focus = ctx.contains_focus();
            ctx.label("boundary-align-description", loc(LocId::BoundaryAlignDescription));

            if ctx.editline("boundary-align-column", &mut state.boundary_align_column) {
                state.boundary_align_invalid = false;
            }
            ctx.attr_intrinsic_size(Size { width: 24, height: 1 });
            if state.boundary_align_invalid {
                ctx.attr_background_rgba(ctx.indexed(IndexedColor::Red));
                ctx.attr_foreground_rgba(ctx.indexed(IndexedColor::BrightWhite));
                ctx.label("boundary-align-invalid", loc(LocId::BoundaryAlignInvalid));
                ctx.attr_foreground_rgba(ctx.indexed(IndexedColor::Red));
            }

            ctx.table_begin("boundary-align-choices");
            ctx.inherit_focus();
            ctx.attr_padding(Rect::three(1, 0, 0));
            ctx.attr_position(Position::Center);
            ctx.table_set_cell_gap(Size { width: 2, height: 0 });
            {
                ctx.table_next_row();
                ctx.inherit_focus();
                apply = ctx.button("ok", loc(LocId::Ok), ButtonStyle::default());
                ctx.inherit_focus();
                cancel = ctx.button("cancel", loc(LocId::Cancel), ButtonStyle::default());
            }
            ctx.table_end();

            if contains_focus && ctx.consume_shortcut(vk::RETURN) {
                apply = true;
            }
        }
        ctx.block_end();
    }
    cancel |= ctx.modal_end();

    if apply {
        let column =
            state.boundary_align_column.trim().parse::<usize>().ok().filter(|column| *column > 0);
        if let Some(column) = column {
            if let Some(doc) = state.documents.active_mut() {
                let mut tb = doc.buffer.borrow_mut();
                align_buffer_to_boundary(&mut tb, column);
                tb.make_cursor_visible();
            }
            state.wants_boundary_align = false;
            reset_boundary_input(state);
            ctx.needs_rerender();
        } else {
            state.boundary_align_invalid = true;
            ctx.needs_rerender();
        }
    } else if cancel {
        state.wants_boundary_align = false;
        reset_boundary_input(state);
        ctx.needs_rerender();
    }
}

/// EN: Restores the input state shared by apply and cancel paths.
/// 中文：還原套用與取消流程共用的輸入狀態。
fn reset_boundary_input(state: &mut State) {
    state.boundary_align_column.clear();
    state.boundary_align_column.push_str("80");
    state.boundary_align_invalid = false;
}

fn align_buffer_to_boundary(tb: &mut TextBuffer, column: usize) {
    let text = read_buffer_text(tb);
    let aligned = align_text_to_boundary(&String::from_utf8_lossy(&text), column);
    if aligned.as_bytes() == text {
        return;
    }

    tb.select_all();
    tb.write_raw(aligned.as_bytes());
}

fn read_buffer_text(tb: &TextBuffer) -> Vec<u8> {
    let mut text = Vec::with_capacity(tb.text_length());
    while text.len() < tb.text_length() {
        let chunk = tb.read_forward(text.len());
        if chunk.is_empty() {
            break;
        }
        text.extend_from_slice(chunk);
    }
    text
}

fn align_text_to_boundary(text: &str, column: usize) -> String {
    // EN: Overflow becomes carry text joined to the next original line until end-of-file.
    // 中文：超出邊界的文字會成為延續內容，與下一原始行合併並持續處理至檔尾。
    if column == 0 || text.is_empty() {
        return text.to_string();
    }

    let newline = if text.contains("\r\n") { "\r\n" } else { "\n" };
    let has_final_newline = text.ends_with('\n');
    let mut output = Vec::new();
    let mut carry = None::<String>;

    for line in text.lines() {
        let mut combined = carry.take().unwrap_or_default();
        combined.push_str(line);
        let mut split = false;

        while let Some(byte_offset) = combined.char_indices().nth(column).map(|(offset, _)| offset)
        {
            output.push(combined[..byte_offset].to_string());
            combined = combined[byte_offset..].to_string();
            split = true;
        }

        if split {
            carry = Some(combined);
        } else {
            output.push(combined);
        }
    }

    if let Some(carry) = carry {
        output.push(carry);
    }

    let mut aligned = output.join(newline);
    if has_final_newline {
        aligned.push_str(newline);
    }
    aligned
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn wraps_overflow_and_merges_it_with_the_next_line() {
        assert_eq!(
            align_text_to_boundary("1234567\nABC\nshort\n12345678901\nXY", 5),
            "12345\n67ABC\nshort\n12345\n67890\n1XY"
        );
    }

    #[test]
    fn counts_unicode_characters_and_preserves_crlf_and_final_newline() {
        assert_eq!(
            align_text_to_boundary("一二三四五六\r\n甲乙\r\n短\r\n", 5),
            "一二三四五\r\n六甲乙\r\n短\r\n"
        );
    }

    #[test]
    fn keeps_short_lines_unchanged() {
        assert_eq!(align_text_to_boundary("one\ntwo\nthree", 80), "one\ntwo\nthree");
    }
}
