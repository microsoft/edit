use crate::helpers::{self, CoordType, Point, Rect, Size};
use crate::ucd;
use std::fmt::Write;
use std::ops::{BitOr, BitXor};
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
        if track.is_empty() {
            return 0;
        }

        let viewport_height = track.height();
        // The content height is at least the viewport height.
        let content_height = content_height.max(viewport_height);
        // The content offset must be at least one viewport height from the bottom.
        // You don't want to scroll past the end after all...
        let content_offset = content_offset.clamp(0, content_height - viewport_height);

        // In order to increase the visual resolution of the scrollbar,
        // we'll use 1/8th blocks to represent the thumb.
        // First, scale the offsets to get that 1/8th resolution.
        let viewport_height = viewport_height as i64 * 8;
        let content_offset = content_offset as i64 * 8;
        let content_height = content_height as i64 * 8;

        // The proportional thumb height (0-1) is the fraction of viewport and
        // content height. The taller the content, the smaller the thumb:
        // = viewport_height / content_height
        //
        // We then scale that to the viewport height to get the height in 1/8th units.
        // = viewport_height * viewport_height / content_height
        //
        // We add content_height/2 to round the integer division, which results in a numerator of:
        // = viewport_height * viewport_height + content_height / 2
        //
        // Finally we add +1 to round up the division if `content_height` is uneven. This ensures that
        // in case of a rounding issue, we'll make the track too large and clamp it to the track size.
        let thumb_numerator = viewport_height * viewport_height + content_height / 2 + 1;
        let thumb_height = thumb_numerator / content_height;
        // Ensure the thumb has a minimum size of 1 row.
        let thumb_height = thumb_height.max(8);

        // The proportional thumb top position (0-1) is naturally:
        // = content_offset / content_height
        //
        // The bottom position is 1 viewport-height below the top position:
        // = (viewport_height + content_offset) / content_height
        //
        // Since everything must be scaled to the 1/8th units we must multiply by viewport_height:
        // = viewport_height * (viewport_height + content_offset) / content_height
        // = viewport_height * viewport_height + viewport_height * content_offset / content_height
        //
        // And we also want that rounded integer division as before. This transforms the
        // `viewport_height * viewport_height` portion into the `thumb_enumerator` above.
        // = thumb_numerator + viewport_height * content_offset / content_height
        //
        let thumb_bottom = (viewport_height * content_offset + thumb_numerator) / content_height;
        // Now that the bottom is flush with the bottom of the track, we can calculate the top.
        let thumb_top = (thumb_bottom - thumb_height).max(0);

        // Calculate the height of the top/bottom cell of the thumb.
        let top_fract = (thumb_top % 8) as CoordType;
        let bottom_fract = (thumb_bottom % 8) as CoordType;

        // Shift to absolute coordinates.
        let thumb_top = ((thumb_top + 7) / 8) as CoordType + track.top;
        let thumb_bottom = (thumb_bottom / 8) as CoordType + track.top;

        let track_clipped = track.intersect(clip_rect);

        // Clamp to the visible area.
        let thumb_top_clipped = thumb_top.max(track_clipped.top);
        let thumb_bottom_clipped = thumb_bottom.min(track_clipped.bottom);

        self.blend_bg(track_clipped, self.indexed(IndexedColor::BrightBlack));
        self.blend_fg(track_clipped, self.indexed(IndexedColor::BrightWhite));

        // Draw the full blocks.
        for y in thumb_top_clipped..thumb_bottom_clipped {
            self.replace_text(y, track_clipped.left, track_clipped.right, "█");
        }

        // Draw the top/bottom cell of the thumb.
        // U+2581 to U+2588, 1/8th block to 8/8th block elements glyphs: ▁▂▃▄▅▆▇█
        // In UTF8: E2 96 81 to E2 96 88
        let mut fract_buf = [0xE2, 0x96, 0x88];
        if top_fract != 0 {
            fract_buf[2] = (0x88 - top_fract) as u8;
            self.replace_text(
                thumb_top_clipped - 1,
                track_clipped.left,
                track_clipped.right,
                unsafe { std::str::from_utf8_unchecked(&fract_buf) },
            );
        }
        if bottom_fract != 0 {
            fract_buf[2] = (0x88 - bottom_fract) as u8;
            let rect = self.replace_text(
                thumb_bottom_clipped,
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
        self.indexed_colors[index as usize] & ((alpha as u32) << 24 | 0xffffff)
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

    pub fn replace_attr(&mut self, target: Rect, attr: Attributes) {
        let back = &mut self.buffers[self.frame_counter & 1];
        back.attributes.replace(target, attr);
    }

    pub fn set_cursor(&mut self, pos: Point, overtype: bool) {
        let back = &mut self.buffers[self.frame_counter & 1];
        back.cursor.pos = pos;
        back.cursor.overtype = overtype;
    }

    pub fn render(&mut self) -> String {
        let idx = self.frame_counter & 1;
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
        let mut last_bg = None;
        let mut last_fg = None;
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

                if last_bg != Some(bg) {
                    last_bg = Some(bg);
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

                if last_fg != Some(fg) {
                    last_fg = Some(fg);
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

        if back.cursor != front.cursor {
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

pub fn mix(dst: u32, src: u32, balance: f32) -> u32 {
    Bitmap::mix(dst, src, balance, 1.0 - balance)
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
        let mut str_new = &text[..res_new.offset];
        let mut str_buf = String::new();
        let overlap_beg = left - res_old_beg.visual_pos.x;
        let overlap_end = res_old_end.visual_pos.x - right;
        if overlap_beg > 0 || overlap_end > 0 {
            if overlap_beg > 0 {
                helpers::string_append_repeat(&mut str_buf, ' ', overlap_beg as usize);
            }
            str_buf.push_str(str_new);
            if overlap_end > 0 {
                helpers::string_append_repeat(&mut str_buf, ' ', overlap_end as usize);
            }
            str_new = &str_buf;
        }

        (*line).replace_range(res_old_beg.offset..res_old_end.offset, str_new);

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

                    data[chunk_beg..chunk_end].fill(Self::mix(c, color, 1.0, 1.0));

                    off < end
                } {}
            }
        }
    }

    fn mix(dst: u32, src: u32, dst_balance: f32, src_balance: f32) -> u32 {
        let src_r = Self::srgb_to_linear(src & 0xff);
        let src_g = Self::srgb_to_linear((src >> 8) & 0xff);
        let src_b = Self::srgb_to_linear((src >> 16) & 0xff);
        let src_a = (src >> 24) as f32 / 255.0f32;
        let src_a = src_a * dst_balance;

        let dst_r = Self::srgb_to_linear(dst & 0xff);
        let dst_g = Self::srgb_to_linear((dst >> 8) & 0xff);
        let dst_b = Self::srgb_to_linear((dst >> 16) & 0xff);
        let dst_a = (dst >> 24) as f32 / 255.0f32;
        let dst_a = dst_a * src_balance;

        let out_a = src_a + dst_a * (1.0f32 - src_a);
        let out_r = (src_r * src_a + dst_r * dst_a * (1.0f32 - src_a)) / out_a;
        let out_g = (src_g * src_a + dst_g * dst_a * (1.0f32 - src_a)) / out_a;
        let out_b = (src_b * src_a + dst_b * dst_a * (1.0f32 - src_a)) / out_a;

        (((out_a * 255.0f32) as u32) << 24)
            | (Self::linear_to_srgb(out_b) << 16)
            | (Self::linear_to_srgb(out_g) << 8)
            | Self::linear_to_srgb(out_r)
    }

    fn srgb_to_linear(c: u32) -> f32 {
        let fc = c as f32 / 255.0f32;
        if fc <= 0.04045f32 {
            fc / 12.92f32
        } else {
            ((fc + 0.055f32) / 1.055f32).powf(2.4f32)
        }
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

    fn replace(&mut self, target: Rect, attr: Attributes) {
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
            data.fill(attr);
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
