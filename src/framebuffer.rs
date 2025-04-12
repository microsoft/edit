use crate::helpers::{self, CoordType, Point, Rect, Size};
use crate::ucd;
use std::fmt::Write;
use std::ops::{BitOr, BitXor};
use std::ptr;
use std::slice::ChunksExact;

#[derive(Clone, Copy)]
pub enum IndexedColor {
    Black,
    Red,
    Green,
    Yellow,
    Blue,
    Magenta,
    Cyan,
    White,
    BrightBlack,
    BrightRed,
    BrightGreen,
    BrightYellow,
    BrightBlue,
    BrightMagenta,
    BrightCyan,
    BrightWhite,
    Background,
    Foreground,
}

pub const INDEXED_COLORS_COUNT: usize = 18;

pub const DEFAULT_THEME: [u32; INDEXED_COLORS_COUNT] = [
    0xff000000, 0xff212cbe, 0xff3aae3f, 0xff4a9abe, 0xffbe4d20, 0xffbe54bb, 0xffb2a700, 0xffbebebe,
    0xff808080, 0xff303eff, 0xff51ea58, 0xff44c9ff, 0xffff6a2f, 0xffff74fc, 0xfff0e100, 0xffffffff,
    0xff000000, 0xffffffff,
];

pub struct Framebuffer {
    indexed_colors: [u32; INDEXED_COLORS_COUNT],
    buffers: [Buffer; 2],
    frame_counter: usize,
    auto_colors: [u32; 2], // [dark, light]
}

impl Framebuffer {
    pub fn new() -> Self {
        Self {
            indexed_colors: DEFAULT_THEME,
            buffers: Default::default(),
            frame_counter: 0,
            auto_colors: [0, 0],
        }
    }

    pub fn set_indexed_colors(&mut self, colors: [u32; INDEXED_COLORS_COUNT]) {
        self.indexed_colors = colors;

        self.auto_colors = [
            self.indexed_colors[IndexedColor::Black as usize],
            self.indexed_colors[IndexedColor::BrightWhite as usize],
        ];
        if !Bitmap::quick_is_dark(self.auto_colors[0]) {
            self.auto_colors.swap(0, 1);
        }
    }

    pub fn reset(&mut self, size: Size) {
        if size != self.buffers[0].bg_bitmap.size {
            for buffer in &mut self.buffers {
                buffer.text = LineBuffer::new(size);
                buffer.bg_bitmap = Bitmap::new(size);
                buffer.fg_bitmap = Bitmap::new(size);
                buffer.attributes = AttributeBuffer::new(size);
            }

            let front = &mut self.buffers[self.frame_counter & 1];
            // Trigger a full redraw. (Yes, it's a hack.)
            front.fg_bitmap.fill(1);
            // Trigger a cursor update as well, just to be sure.
            front.cursor = Cursor::new_invalid();
        }

        self.frame_counter = self.frame_counter.wrapping_add(1);

        let back = &mut self.buffers[self.frame_counter & 1];
        let bg = self.indexed_colors[IndexedColor::Background as usize];
        let fg = self.indexed_colors[IndexedColor::Foreground as usize];

        back.text.fill_whitespace();
        back.bg_bitmap.fill(bg);
        back.fg_bitmap.fill(fg);
        back.attributes.reset();
        back.cursor = Cursor::new_disabled();
    }

    /// Replaces text contents in a single line of the framebuffer.
    /// All coordinates are in viewport coordinates.
    /// Assumes that all tabs have been replaced with spaces.
    ///
    /// TODO: This function is ripe for performance improvements.
    pub fn replace_text(
        &mut self,
        y: CoordType,
        origin_x: CoordType,
        clip_right: CoordType,
        text: &str,
    ) -> Rect {
        let back = &mut self.buffers[self.frame_counter & 1];
        back.text.replace_text(y, origin_x, clip_right, text)
    }

    pub fn draw_scrollbar(
        &mut self,
        clip_rect: Rect,
        track: Rect,
        content_offset: CoordType,
        content_height: CoordType,
    ) -> CoordType {
        let track_clipped = track.intersect(clip_rect);
        if track_clipped.is_empty() {
            return 0;
        }

        let viewport_height = track.height();
        // The content height is at least the viewport height.
        let content_height = content_height.max(viewport_height);

        // No need to draw a scrollbar if the content fits in the viewport.
        let content_offset_max = content_height - viewport_height;
        if content_offset_max == 0 {
            return 0;
        }

        // The content offset must be at least one viewport height from the bottom.
        // You don't want to scroll past the end after all...
        let content_offset = content_offset.clamp(0, content_offset_max);

        // In order to increase the visual resolution of the scrollbar,
        // we'll use 1/8th blocks to represent the thumb.
        // First, scale the offsets to get that 1/8th resolution.
        let viewport_height = viewport_height as i64 * 8;
        let content_offset_max = content_offset_max as i64 * 8;
        let content_offset = content_offset as i64 * 8;
        let content_height = content_height as i64 * 8;

        // The proportional thumb height (0-1) is the fraction of viewport and
        // content height. The taller the content, the smaller the thumb:
        // = viewport_height / content_height
        // We then scale that to the viewport height to get the height in 1/8th units.
        // = viewport_height * viewport_height / content_height
        // We add content_height/2 to round the integer division, which results in a numerator of:
        // = viewport_height * viewport_height + content_height / 2
        let numerator = viewport_height * viewport_height + content_height / 2;
        let thumb_height = numerator / content_height;
        // Ensure the thumb has a minimum size of 1 row.
        let thumb_height = thumb_height.max(8);

        // The proportional thumb top position (0-1) is:
        // = content_offset / content_offset_max
        // The maximum thumb top position is the viewport height minus the thumb height:
        // = viewport_height - thumb_height
        // To get the thumb top position in 1/8th units, we multiply both:
        // = (viewport_height - thumb_height) * content_offset / content_offset_max
        // We add content_offset_max/2 to round the integer division, which results in a numerator of:
        // = (viewport_height - thumb_height) * content_offset + content_offset_max / 2
        let numerator = (viewport_height - thumb_height) * content_offset + content_offset_max / 2;
        let thumb_top = numerator / content_offset_max;
        // The thumb bottom position is the thumb top position plus the thumb height.
        let thumb_bottom = thumb_top + thumb_height;

        // Shift to absolute coordinates.
        let thumb_top = thumb_top + track.top as i64 * 8;
        let thumb_bottom = thumb_bottom + track.top as i64 * 8;

        // Clamp to the visible area.
        let thumb_top = thumb_top.max(track_clipped.top as i64 * 8);
        let thumb_bottom = thumb_bottom.min(track_clipped.bottom as i64 * 8);

        // Calculate the height of the top/bottom cell of the thumb.
        let top_fract = (thumb_top % 8) as CoordType;
        let bottom_fract = (thumb_bottom % 8) as CoordType;

        // Shift to absolute coordinates.
        let thumb_top = ((thumb_top + 7) / 8) as CoordType;
        let thumb_bottom = (thumb_bottom / 8) as CoordType;

        self.blend_bg(track_clipped, self.indexed(IndexedColor::BrightBlack));
        self.blend_fg(track_clipped, self.indexed(IndexedColor::BrightWhite));

        // Draw the full blocks.
        for y in thumb_top..thumb_bottom {
            self.replace_text(y, track_clipped.left, track_clipped.right, "█");
        }

        // Draw the top/bottom cell of the thumb.
        // U+2581 to U+2588, 1/8th block to 8/8th block elements glyphs: ▁▂▃▄▅▆▇█
        // In UTF8: E2 96 81 to E2 96 88
        let mut fract_buf = [0xE2, 0x96, 0x88];
        if top_fract != 0 {
            fract_buf[2] = (0x88 - top_fract) as u8;
            self.replace_text(
                thumb_top - 1,
                track_clipped.left,
                track_clipped.right,
                unsafe { std::str::from_utf8_unchecked(&fract_buf) },
            );
        }
        if bottom_fract != 0 {
            fract_buf[2] = (0x88 - bottom_fract) as u8;
            let rect = self.replace_text(
                thumb_bottom,
                track_clipped.left,
                track_clipped.right,
                unsafe { std::str::from_utf8_unchecked(&fract_buf) },
            );
            self.blend_bg(rect, self.indexed(IndexedColor::BrightWhite));
            self.blend_fg(rect, self.indexed(IndexedColor::BrightBlack));
        }

        ((thumb_height + 4) / 8) as CoordType
    }

    #[inline]
    pub fn indexed(&self, index: IndexedColor) -> u32 {
        self.indexed_colors[index as usize]
    }

    #[inline]
    pub fn indexed_alpha(&self, index: IndexedColor, alpha: u8) -> u32 {
        self.indexed_colors[index as usize] & 0x00ffffff | (alpha as u32) << 24
    }

    pub fn contrasted(&self, color: u32) -> u32 {
        self.auto_colors[Bitmap::quick_is_dark(color) as usize]
    }

    pub fn blend_bg(&mut self, target: Rect, bg: u32) {
        let back = &mut self.buffers[self.frame_counter & 1];
        back.bg_bitmap.blend(target, bg);
    }

    pub fn blend_fg(&mut self, target: Rect, fg: u32) {
        let back = &mut self.buffers[self.frame_counter & 1];
        back.fg_bitmap.blend(target, fg);
    }

    pub fn reverse(&mut self, target: Rect) {
        let back = &mut self.buffers[self.frame_counter & 1];

        let target = target.intersect(back.bg_bitmap.size.as_rect());
        if target.is_empty() {
            return;
        }

        let top = target.top as usize;
        let bottom = target.bottom as usize;
        let left = target.left as usize;
        let right = target.right as usize;
        let stride = back.bg_bitmap.size.width as usize;

        for y in top..bottom {
            let beg = y * stride + left;
            let end = y * stride + right;
            let bg = &mut back.bg_bitmap.data[beg..end];
            let fg = &mut back.fg_bitmap.data[beg..end];
            bg.swap_with_slice(fg);
        }
    }

    pub fn replace_attr(&mut self, target: Rect, mask: Attributes, attr: Attributes) {
        let back = &mut self.buffers[self.frame_counter & 1];
        back.attributes.replace(target, mask, attr);
    }
    pub fn flip_attr(&mut self, target: Rect, attr: Attributes) {
        let back = &mut self.buffers[self.frame_counter & 1];
        back.attributes.flip(target, attr);
    }

    pub fn set_cursor(&mut self, pos: Point, overtype: bool) {
        let back = &mut self.buffers[self.frame_counter & 1];
        back.cursor.pos = pos;
        back.cursor.overtype = overtype;
    }

    pub fn render(&mut self) -> String {
        let idx = self.frame_counter & 1;
        // Borrows the front/back buffers without letting Rust know that we have a reference to self.
        // SAFETY: Well this is certainly correct, but whether Rust and its strict rules likes it is another question.
        let (back, front) = unsafe {
            let ptr = self.buffers.as_mut_ptr();
            let back = &mut *ptr.add(idx);
            let front = &*ptr.add(1 - idx);
            (back, front)
        };

        let mut front_lines = front.text.lines.iter(); // hahaha
        let mut front_bgs = front.bg_bitmap.iter();
        let mut front_fgs = front.fg_bitmap.iter();
        let mut front_attrs = front.attributes.iter();

        let mut back_lines = back.text.lines.iter();
        let mut back_bgs = back.bg_bitmap.iter();
        let mut back_fgs = back.fg_bitmap.iter();
        let mut back_attrs = back.attributes.iter();

        let mut result = String::with_capacity(256);
        let mut last_bg = self.indexed(IndexedColor::Background);
        let mut last_fg = self.indexed(IndexedColor::Foreground);
        let mut last_attr = Attributes::None;

        for y in 0..front.text.size.height {
            // SAFETY: The only thing that changes the size of these containers,
            // is the reset() method and it always resets front/back to the same size.
            let front_line = unsafe { front_lines.next().unwrap_unchecked() };
            let front_bg = unsafe { front_bgs.next().unwrap_unchecked() };
            let front_fg = unsafe { front_fgs.next().unwrap_unchecked() };
            let front_attr = unsafe { front_attrs.next().unwrap_unchecked() };

            let back_line = unsafe { back_lines.next().unwrap_unchecked() };
            let back_bg = unsafe { back_bgs.next().unwrap_unchecked() };
            let back_fg = unsafe { back_fgs.next().unwrap_unchecked() };
            let back_attr = unsafe { back_attrs.next().unwrap_unchecked() };

            // TODO: Ideally, we should properly diff the contents and so if
            // only parts of a line change, we should only update those parts.
            if front_line == back_line
                && front_bg == back_bg
                && front_fg == back_fg
                && front_attr == back_attr
            {
                continue;
            }

            let line_bytes = back_line.as_bytes();
            let mut cfg = ucd::MeasurementConfig::new(&line_bytes);
            let mut chunk_end = 0;

            if result.is_empty() {
                result.push_str("\x1b[m");
            }
            _ = write!(result, "\x1b[{};1H", y + 1);

            while {
                let bg = back_bg[chunk_end];
                let fg = back_fg[chunk_end];
                let attr = back_attr[chunk_end];

                // Chunk into runs of the same color.
                while {
                    chunk_end += 1;
                    chunk_end < back_bg.len()
                        && back_bg[chunk_end] == bg
                        && back_fg[chunk_end] == fg
                        && back_attr[chunk_end] == attr
                } {}

                if last_bg != bg {
                    last_bg = bg;
                    if bg == self.indexed_colors[IndexedColor::Background as usize] {
                        result.push_str("\x1b[49m");
                    } else {
                        _ = write!(
                            result,
                            "\x1b[48;2;{};{};{}m",
                            bg & 0xff,
                            (bg >> 8) & 0xff,
                            (bg >> 16) & 0xff
                        );
                    }
                }

                if last_fg != fg {
                    last_fg = fg;
                    if fg == self.indexed_colors[IndexedColor::Foreground as usize] {
                        result.push_str("\x1b[39m");
                    } else {
                        _ = write!(
                            result,
                            "\x1b[38;2;{};{};{}m",
                            fg & 0xff,
                            (fg >> 8) & 0xff,
                            (fg >> 16) & 0xff
                        );
                    }
                }

                if last_attr != attr {
                    let diff = last_attr ^ attr;
                    if diff.underlined() {
                        if attr.underlined() {
                            result.push_str("\x1b[4m");
                        } else {
                            result.push_str("\x1b[24m");
                        }
                    }
                    last_attr = attr;
                }

                let beg = cfg.cursor().offset;
                let end = cfg
                    .goto_visual(Point {
                        x: chunk_end as CoordType,
                        y: 0,
                    })
                    .offset;
                result.push_str(&back_line[beg..end]);

                chunk_end < back_bg.len()
            } {}
        }

        // If the cursor has changed since the last frame we naturally need to update it,
        // but this also applies if the code above wrote to the screen,
        // as it uses CUP sequences to reposition the cursor for writing.
        if !result.is_empty() || back.cursor != front.cursor {
            if back.cursor.pos.x >= 0 && back.cursor.pos.y >= 0 {
                // CUP to the cursor position.
                // DECSCUSR to set the cursor style.
                // DECTCEM to show the cursor.
                _ = write!(
                    result,
                    "\x1b[{};{}H\x1b[{} q\x1b[?25h",
                    back.cursor.pos.y + 1,
                    back.cursor.pos.x + 1,
                    if back.cursor.overtype { 1 } else { 5 }
                );
            } else {
                // DECTCEM to hide the cursor.
                result.push_str("\x1b[?25l");
            }
        }

        result
    }
}

pub fn alpha_blend(dst: u32, src: u32) -> u32 {
    Bitmap::alpha_blend(dst, src)
}

#[derive(Default)]
struct Buffer {
    text: LineBuffer,
    bg_bitmap: Bitmap,
    fg_bitmap: Bitmap,
    attributes: AttributeBuffer,
    cursor: Cursor,
}

#[derive(Default)]
struct LineBuffer {
    lines: Vec<String>,
    size: Size,
}

impl LineBuffer {
    fn new(size: Size) -> Self {
        Self {
            lines: vec![String::new(); size.height as usize],
            size,
        }
    }

    fn fill_whitespace(&mut self) {
        let width = self.size.width as usize;
        for l in &mut self.lines {
            l.clear();
            l.reserve(width + width / 2);
            helpers::string_append_repeat(l, ' ', width);
        }
    }

    /// Replaces text contents in a single line of the framebuffer.
    /// All coordinates are in viewport coordinates.
    /// Assumes that all tabs have been replaced with spaces.
    ///
    /// TODO: This function is ripe for performance improvements.
    pub fn replace_text(
        &mut self,
        y: CoordType,
        origin_x: CoordType,
        clip_right: CoordType,
        text: &str,
    ) -> Rect {
        let Some(line) = self.lines.get_mut(y as usize) else {
            return Rect::default();
        };

        let bytes = text.as_bytes();
        let clip_right = clip_right.clamp(0, self.size.width);
        let layout_width = clip_right - origin_x;

        // Can't insert text that can't fit or is empty.
        if layout_width <= 0 || bytes.is_empty() {
            return Rect::default();
        }

        let mut cfg = ucd::MeasurementConfig::new(&bytes);

        // Check if the text intersects with the left edge of the framebuffer
        // and figure out the parts that are inside.
        let mut left = origin_x;
        if left < 0 {
            let cursor = cfg.goto_visual(Point { x: -left, y: 0 });
            left += cursor.visual_pos.x;

            if left < 0 && cursor.offset < text.len() {
                // `-left` must've intersected a wide glyph and since goto_visual stops _before_ reaching the target,
                // we stoped before the wide glyph and thus must step forward to the next glyph.
                let cursor = cfg.goto_logical(Point {
                    x: cursor.logical_pos.x + 1,
                    y: 0,
                });
                left += cursor.visual_pos.x;
            }
        }

        // If the text still starts outside the framebuffer, we must've ran out of text above.
        // Otherwise, if it starts outside the right edge to begin with, we can't insert it anyway.
        if left < 0 || left >= clip_right {
            return Rect::default();
        }

        // Measure the width of the new text (= `res_new.visual_target.x`).
        let res_new = cfg.goto_visual(Point {
            x: layout_width,
            y: 0,
        });

        // Figure out at which byte offset the new text gets inserted.
        let right = left + res_new.visual_pos.x;
        let line_bytes = line.as_bytes();
        let mut cfg_old = ucd::MeasurementConfig::new(&line_bytes);
        let res_old_beg = cfg_old.goto_visual(Point { x: left, y: 0 });
        let mut res_old_end = cfg_old.goto_visual(Point { x: right, y: 0 });

        // Since the goto functions will always stop short of the target position,
        // we need to manually step beyond it if we intersect with a wide glyph.
        if res_old_end.visual_pos.x < right {
            res_old_end = cfg_old.goto_logical(Point {
                x: res_old_end.logical_pos.x + 1,
                y: 0,
            });
        }

        // If we intersect a wide glyph, we need to pad the new text with spaces.
        let src = &text[..res_new.offset];
        let overlap_beg = (left - res_old_beg.visual_pos.x).max(0) as usize;
        let overlap_end = (res_old_end.visual_pos.x - right).max(0) as usize;
        let total_add = src.len() + overlap_beg + overlap_end;
        let total_del = res_old_end.offset - res_old_beg.offset;

        // This is basically a hand-written version of `Vec::splice()`,
        // but for strings under the assumption that all inputs are valid.
        // It also takes care of `overlap_beg` and `overlap_end` by inserting spaces.
        unsafe {
            // SAFETY: Our ucd code only returns valid UTF-8 offsets.
            // If it didn't that'd be a priority -9000 bug for any text editor.
            // And apart from that, all inputs are &str (= UTF8).
            let dst = line.as_mut_vec();

            let dst_len = dst.len();
            let src_len = src.len();

            // Make room for the new elements. NOTE that this must be done before
            // we call as_mut_ptr, or else we risk accessing a stale pointer.
            // We only need to reserve as much as the string actually grows by.
            dst.reserve(total_add.saturating_sub(total_del));

            // Move the pointer to the start of the insert.
            let mut ptr = dst.as_mut_ptr().add(res_old_beg.offset);

            // Move the tail end of the string by `total_add - total_del`-many bytes.
            // This both effectively deletes the old text and makes room for the new text.
            if total_add != total_del {
                // Move the tail of the vector to make room for the new elements.
                ptr::copy(
                    ptr.add(total_del),
                    ptr.add(total_add),
                    dst_len - total_del - res_old_beg.offset,
                );
            }

            // Pad left.
            for _ in 0..overlap_beg {
                ptr.write(b' ');
                ptr = ptr.add(1);
            }

            // Copy the new elements into the vector.
            ptr::copy_nonoverlapping(src.as_ptr(), ptr, src_len);
            ptr = ptr.add(src_len);

            // Pad right.
            for _ in 0..overlap_end {
                ptr.write(b' ');
                ptr = ptr.add(1);
            }

            // Update the length of the vector.
            dst.set_len(dst_len - total_del + total_add);
        }

        Rect {
            left,
            top: y,
            right,
            bottom: y + 1,
        }
    }
}

#[derive(Default)]
struct Bitmap {
    data: Vec<u32>,
    size: Size,
}

impl Bitmap {
    fn new(size: Size) -> Self {
        Self {
            data: vec![0; (size.width * size.height) as usize],
            size,
        }
    }

    fn fill(&mut self, color: u32) {
        self.data.fill(color);
    }

    fn blend(&mut self, target: Rect, color: u32) {
        if (color & 0xff000000) == 0x00000000 {
            return;
        }

        let target = target.intersect(self.size.as_rect());
        if target.is_empty() {
            return;
        }

        let top = target.top as usize;
        let bottom = target.bottom as usize;
        let left = target.left as usize;
        let right = target.right as usize;
        let stride = self.size.width as usize;

        for y in top..bottom {
            let beg = y * stride + left;
            let end = y * stride + right;
            let data = &mut self.data[beg..end];

            if (color & 0xff000000) == 0xff000000 {
                data.fill(color);
            } else {
                let end = data.len();
                let mut off = 0;

                while {
                    let c = data[off];

                    // Chunk into runs of the same color, so that we only call alpha_blend once per run.
                    let chunk_beg = off;
                    while {
                        off += 1;
                        off < end && data[off] == c
                    } {}
                    let chunk_end = off;

                    data[chunk_beg..chunk_end].fill(Self::alpha_blend(c, color));

                    off < end
                } {}
            }
        }
    }

    fn alpha_blend(dst: u32, src: u32) -> u32 {
        let src_a = (src >> 24) as f32 * (1.0 / 255.0);
        let src_b = Self::srgb_to_linear(src >> 16);
        let src_g = Self::srgb_to_linear(src >> 8);
        let src_r = Self::srgb_to_linear(src);

        let dst_a = (dst >> 24) as f32 * (1.0 / 255.0);
        let dst_b = Self::srgb_to_linear(dst >> 16);
        let dst_g = Self::srgb_to_linear(dst >> 8);
        let dst_r = Self::srgb_to_linear(dst);

        let out_a = src_a + dst_a * (1.0 - src_a);
        // The formula is technically:
        //   (src_bgr * src_a + dst_bgr * dst_a * (1 - src_a)) / out_a
        // but we can merge the division of out_a with the two preceding terms.
        // This saves us a bunch of operations that cannot be optimized away otherwise.
        let out_a_inv = 1.0 / out_a;
        let src_mul = src_a * out_a_inv;
        let dst_mul = dst_a * (1.0 - src_a) * out_a_inv;
        let out_b = src_b * src_mul + dst_b * dst_mul;
        let out_g = src_g * src_mul + dst_g * dst_mul;
        let out_r = src_r * src_mul + dst_r * dst_mul;

        let out_b = Self::linear_to_srgb(out_b);
        let out_g = Self::linear_to_srgb(out_g);
        let out_r = Self::linear_to_srgb(out_r);

        (((out_a * 255.0f32) as u32) << 24) | (out_b << 16) | (out_g << 8) | out_r
    }

    fn srgb_to_linear(c: u32) -> f32 {
        // Generated using:
        // ```rs
        // let fc = c as f32 / 255.0;
        // if fc <= 0.04045 {
        //     fc / 12.92
        // } else {
        //     ((fc + 0.055) / 1.055).powf(2.4)
        // }
        // ```
        // I'd love to use hex floats, but for some reason Rust maintainers decided against it...
        #[rustfmt::skip]
        #[allow(clippy::excessive_precision)]
        const LUT: [f32; 256] = [
            0.0000000000, 0.0003035270, 0.0006070540, 0.0009105810, 0.0012141080, 0.0015176350, 0.0018211619, 0.0021246888, 0.0024282159, 0.0027317430, 0.0030352699, 0.0033465356, 0.0036765069, 0.0040247170, 0.0043914421, 0.0047769533,
            0.0051815170, 0.0056053917, 0.0060488326, 0.0065120910, 0.0069954102, 0.0074990317, 0.0080231922, 0.0085681248, 0.0091340570, 0.0097212177, 0.0103298230, 0.0109600937, 0.0116122449, 0.0122864870, 0.0129830306, 0.0137020806,
            0.0144438436, 0.0152085144, 0.0159962922, 0.0168073755, 0.0176419523, 0.0185002182, 0.0193823613, 0.0202885624, 0.0212190095, 0.0221738834, 0.0231533647, 0.0241576303, 0.0251868572, 0.0262412224, 0.0273208916, 0.0284260381,
            0.0295568332, 0.0307134409, 0.0318960287, 0.0331047624, 0.0343398079, 0.0356013142, 0.0368894450, 0.0382043645, 0.0395462364, 0.0409151986, 0.0423114114, 0.0437350273, 0.0451862030, 0.0466650836, 0.0481718220, 0.0497065634,
            0.0512694679, 0.0528606549, 0.0544802807, 0.0561284944, 0.0578054339, 0.0595112406, 0.0612460710, 0.0630100295, 0.0648032799, 0.0666259527, 0.0684781820, 0.0703601092, 0.0722718611, 0.0742135793, 0.0761853904, 0.0781874284,
            0.0802198276, 0.0822827145, 0.0843762159, 0.0865004659, 0.0886556059, 0.0908417329, 0.0930589810, 0.0953074843, 0.0975873619, 0.0998987406, 0.1022417471, 0.1046164930, 0.1070231125, 0.1094617173, 0.1119324341, 0.1144353822,
            0.1169706732, 0.1195384338, 0.1221387982, 0.1247718409, 0.1274376959, 0.1301364899, 0.1328683347, 0.1356333494, 0.1384316236, 0.1412633061, 0.1441284865, 0.1470272839, 0.1499598026, 0.1529261619, 0.1559264660, 0.1589608639,
            0.1620294005, 0.1651322246, 0.1682693958, 0.1714410931, 0.1746473908, 0.1778884083, 0.1811642349, 0.1844749898, 0.1878207624, 0.1912016720, 0.1946178079, 0.1980693042, 0.2015562356, 0.2050787061, 0.2086368501, 0.2122307271,
            0.2158605307, 0.2195262313, 0.2232279778, 0.2269658893, 0.2307400703, 0.2345506549, 0.2383976579, 0.2422811985, 0.2462013960, 0.2501583695, 0.2541521788, 0.2581829131, 0.2622507215, 0.2663556635, 0.2704978585, 0.2746773660,
            0.2788943350, 0.2831487954, 0.2874408960, 0.2917706966, 0.2961383164, 0.3005438447, 0.3049873710, 0.3094689548, 0.3139887452, 0.3185468316, 0.3231432438, 0.3277781308, 0.3324515820, 0.3371636569, 0.3419144452, 0.3467040956,
            0.3515326977, 0.3564002514, 0.3613068759, 0.3662526906, 0.3712377846, 0.3762622178, 0.3813261092, 0.3864295185, 0.3915725648, 0.3967553079, 0.4019778669, 0.4072403014, 0.4125427008, 0.4178851545, 0.4232677519, 0.4286905527,
            0.4341537058, 0.4396572411, 0.4452012479, 0.4507858455, 0.4564110637, 0.4620770514, 0.4677838385, 0.4735315442, 0.4793202281, 0.4851499796, 0.4910208881, 0.4969330430, 0.5028865933, 0.5088814497, 0.5149177909, 0.5209956765,
            0.5271152258, 0.5332764983, 0.5394796133, 0.5457245708, 0.5520114899, 0.5583404899, 0.5647116303, 0.5711249113, 0.5775805116, 0.5840784907, 0.5906189084, 0.5972018838, 0.6038274169, 0.6104956269, 0.6172066331, 0.6239604354,
            0.6307572126, 0.6375969648, 0.6444797516, 0.6514056921, 0.6583748460, 0.6653873324, 0.6724432111, 0.6795425415, 0.6866854429, 0.6938719153, 0.7011020184, 0.7083759308, 0.7156936526, 0.7230552435, 0.7304608822, 0.7379105687,
            0.7454043627, 0.7529423237, 0.7605246305, 0.7681512833, 0.7758223414, 0.7835379243, 0.7912980318, 0.7991028428, 0.8069523573, 0.8148466945, 0.8227858543, 0.8307699561, 0.8387991190, 0.8468732834, 0.8549926877, 0.8631572723,
            0.8713672161, 0.8796223402, 0.8879231811, 0.8962693810, 0.9046613574, 0.9130986929, 0.9215820432, 0.9301108718, 0.9386858940, 0.9473065734, 0.9559735060, 0.9646862745, 0.9734454751, 0.9822505713, 0.9911022186, 1.0000000000,
        ];
        LUT[(c & 0xff) as usize]
    }

    fn linear_to_srgb(c: f32) -> u32 {
        if c <= 0.0031308f32 {
            (c * 12.92f32 * 255.0f32) as u32
        } else {
            ((1.055f32 * c.powf(1.0f32 / 2.4f32) - 0.055f32) * 255.0f32) as u32
        }
    }

    fn quick_is_dark(c: u32) -> bool {
        let r = c & 0xff;
        let g = (c >> 8) & 0xff;
        let b = (c >> 16) & 0xff;
        // Rough approximation of the sRGB luminance Y = 0.2126 R + 0.7152 G + 0.0722 B.
        let l = r * 3 + g * 10 + b;
        l < 128 * 14
    }

    /// Iterates over each row in the bitmap.
    fn iter(&self) -> ChunksExact<u32> {
        self.data.chunks_exact(self.size.width as usize)
    }
}

#[repr(transparent)]
#[derive(Default, Clone, Copy, PartialEq, Eq)]
pub struct Attributes(u8);

#[allow(non_upper_case_globals)] // Mimics an enum, but it's actually a bitfield. Allows simple diffing.
impl Attributes {
    pub const None: Attributes = Attributes(0);
    pub const Underlined: Attributes = Attributes(0b1);
    pub const All: Attributes = Attributes(0b1);

    pub const fn underlined(self) -> bool {
        self.0 & Self::Underlined.0 != 0
    }
}

impl BitOr for Attributes {
    type Output = Attributes;

    fn bitor(self, rhs: Self) -> Self::Output {
        Attributes(self.0 | rhs.0)
    }
}

impl BitXor for Attributes {
    type Output = Attributes;

    fn bitxor(self, rhs: Self) -> Self::Output {
        Attributes(self.0 ^ rhs.0)
    }
}

#[derive(Default)]
struct AttributeBuffer {
    data: Vec<Attributes>,
    size: Size,
}

impl AttributeBuffer {
    fn new(size: Size) -> Self {
        Self {
            data: vec![Default::default(); (size.width * size.height) as usize],
            size,
        }
    }

    fn reset(&mut self) {
        self.data.fill(Default::default());
    }

    fn replace(&mut self, target: Rect, mask: Attributes, attr: Attributes) {
        let target = target.intersect(self.size.as_rect());
        if target.is_empty() {
            return;
        }

        let top = target.top as usize;
        let bottom = target.bottom as usize;
        let left = target.left as usize;
        let right = target.right as usize;
        let stride = self.size.width as usize;

        for y in top..bottom {
            let beg = y * stride + left;
            let end = y * stride + right;
            for a in &mut self.data[beg..end] {
                *a = Attributes(a.0 & !mask.0 | attr.0);
            }
        }
    }

    fn flip(&mut self, target: Rect, attr: Attributes) {
        let target = target.intersect(self.size.as_rect());
        if target.is_empty() {
            return;
        }

        let top = target.top as usize;
        let bottom = target.bottom as usize;
        let left = target.left as usize;
        let right = target.right as usize;
        let stride = self.size.width as usize;

        for y in top..bottom {
            let beg = y * stride + left;
            let end = y * stride + right;
            for a in &mut self.data[beg..end] {
                *a = Attributes(a.0 ^ attr.0);
            }
        }
    }

    /// Iterates over each row in the bitmap.
    fn iter(&self) -> ChunksExact<Attributes> {
        self.data.chunks_exact(self.size.width as usize)
    }
}

#[derive(Default, PartialEq, Eq)]
struct Cursor {
    pos: Point,
    overtype: bool,
}

impl Cursor {
    const fn new_invalid() -> Self {
        Self {
            pos: Point::MIN,
            overtype: false,
        }
    }

    const fn new_disabled() -> Self {
        Self {
            pos: Point { x: -1, y: -1 },
            overtype: false,
        }
    }
}
