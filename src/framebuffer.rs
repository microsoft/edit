// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! A shoddy framebuffer for terminal applications.

use std::cell::Cell;
use std::fmt::Write;
use std::ops::{BitOr, BitXor};
use std::ptr;
use std::slice::ChunksExact;

use crate::arena::{Arena, ArenaString};
use crate::helpers::{CoordType, Point, Rect, Size};
use crate::oklab::{oklab_blend, srgb_to_oklab};
use crate::simd::{MemsetSafe, memset};
use crate::unicode::MeasurementConfig;

// Same constants as used in the PCG family of RNGs.
#[cfg(target_pointer_width = "32")]
const HASH_MULTIPLIER: usize = 747796405; // https://doi.org/10.1090/S0025-5718-99-00996-5, Table 5
#[cfg(target_pointer_width = "64")]
const HASH_MULTIPLIER: usize = 6364136223846793005; // Knuth's MMIX multiplier

/// The size of our cache table. 1<<8 = 256.
const CACHE_TABLE_LOG2_SIZE: usize = 8;
const CACHE_TABLE_SIZE: usize = 1 << CACHE_TABLE_LOG2_SIZE;
/// To index into the cache table, we use `color * HASH_MULTIPLIER` as the hash.
/// Since the multiplication "shifts" the bits up, we don't just mask the lowest
/// 8 bits out, but rather shift 56 bits down to get the best bits from the top.
const CACHE_TABLE_SHIFT: usize = usize::BITS as usize - CACHE_TABLE_LOG2_SIZE;

/// Standard 16 VT & default foreground/background colors.
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

/// Number of indices used by [`IndexedColor`].
pub const INDEXED_COLORS_COUNT: usize = 18;

/// Fallback theme. Matches Windows Terminal's Ottosson theme.
pub const DEFAULT_THEME: [u32; INDEXED_COLORS_COUNT] = [
    0xff000000, 0xff212cbe, 0xff3aae3f, 0xff4a9abe, 0xffbe4d20, 0xffbe54bb, 0xffb2a700, 0xffbebebe,
    0xff808080, 0xff303eff, 0xff51ea58, 0xff44c9ff, 0xffff6a2f, 0xffff74fc, 0xfff0e100, 0xffffffff,
    0xff000000, 0xffffffff,
];

/// A shoddy framebuffer for terminal applications.
///
/// The idea is that you create a [`Framebuffer`], draw a bunch of text and
/// colors into it, and it takes care of figuring out what changed since the
/// last rendering and sending the differences as VT to the terminal.
///
/// This is an improvement over how many other terminal applications work,
/// as they fail to accurately track what changed. If you watch the output
/// of `vim` for instance, you'll notice that it redraws unrelated parts of
/// the screen all the time.
pub struct Framebuffer {
    /// Store the color palette.
    indexed_colors: [u32; INDEXED_COLORS_COUNT],
    /// Front and back buffers. Indexed by `frame_counter & 1`.
    buffers: [Buffer; 2],
    /// The current frame counter. Increments on every `flip` call.
    frame_counter: usize,
    /// The colors used for `contrast()`. It stores the default colors
    /// of the palette as [dark, light], unless the palette is recognized
    /// as a light them, in which case it swaps them.
    auto_colors: [u32; 2],
    /// A cache table for previously contrasted colors.
    /// See: <https://fgiesen.wordpress.com/2019/02/11/cache-tables/>
    contrast_colors: [Cell<(u32, u32)>; CACHE_TABLE_SIZE],
    background_fill: u32,
    foreground_fill: u32,
}

impl Framebuffer {
    /// Creates a new framebuffer.
    pub fn new() -> Self {
        Self {
            indexed_colors: DEFAULT_THEME,
            buffers: Default::default(),
            frame_counter: 0,
            auto_colors: [
                DEFAULT_THEME[IndexedColor::Black as usize],
                DEFAULT_THEME[IndexedColor::White as usize],
            ],
            contrast_colors: [const { Cell::new((0, 0)) }; CACHE_TABLE_SIZE],
            background_fill: DEFAULT_THEME[IndexedColor::Background as usize],
            foreground_fill: DEFAULT_THEME[IndexedColor::Foreground as usize],
        }
    }

    /// Sets the base color palette.
    ///
    /// If you call this method, [`Framebuffer`] expects that you
    /// successfully detect the light/dark mode of the terminal.
    pub fn set_indexed_colors(&mut self, colors: [u32; INDEXED_COLORS_COUNT]) {
        self.indexed_colors = colors;
        self.background_fill = 0;
        self.foreground_fill = 0;

        self.auto_colors = [
            self.indexed_colors[IndexedColor::Black as usize],
            self.indexed_colors[IndexedColor::BrightWhite as usize],
        ];
        if !Self::is_dark(self.auto_colors[0]) {
            self.auto_colors.swap(0, 1);
        }
    }

    /// Begins a new frame with the given `size`.
    pub fn flip(&mut self, size: Size) {
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

        back.text.fill_whitespace();
        back.bg_bitmap.fill(self.background_fill);
        back.fg_bitmap.fill(self.foreground_fill);
        back.attributes.reset();
        back.cursor = Cursor::new_disabled();
    }

    /// Replaces text contents in a single line of the framebuffer.
    /// All coordinates are in viewport coordinates.
    /// Assumes that control characters have been replaced or escaped.
    pub fn replace_text(
        &mut self,
        y: CoordType,
        origin_x: CoordType,
        clip_right: CoordType,
        text: &str,
    ) {
        let back = &mut self.buffers[self.frame_counter & 1];
        back.text.replace_text(y, origin_x, clip_right, text)
    }

    /// Draws a scrollbar in the given `track` rectangle.
    ///
    /// Not entirely sure why I put it here instead of elsewhere.
    ///
    /// # Parameters
    ///
    /// * `clip_rect`: Clips the rendering to this rectangle.
    ///   This is relevant when you have scrollareas inside scrollareas.
    /// * `track`: The rectangle in which to draw the scrollbar.
    ///   In absolute viewport coordinates.
    /// * `content_offset`: The current offset of the scrollarea.
    /// * `content_height`: The height of the scrollarea content.
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
            self.replace_text(thumb_top - 1, track_clipped.left, track_clipped.right, unsafe {
                std::str::from_utf8_unchecked(&fract_buf)
            });
        }
        if bottom_fract != 0 {
            fract_buf[2] = (0x88 - bottom_fract) as u8;
            self.replace_text(thumb_bottom, track_clipped.left, track_clipped.right, unsafe {
                std::str::from_utf8_unchecked(&fract_buf)
            });
            let rect = Rect {
                left: track_clipped.left,
                top: thumb_bottom,
                right: track_clipped.right,
                bottom: thumb_bottom + 1,
            };
            self.blend_bg(rect, self.indexed(IndexedColor::BrightWhite));
            self.blend_fg(rect, self.indexed(IndexedColor::BrightBlack));
        }

        ((thumb_height + 4) / 8) as CoordType
    }

    #[inline]
    pub fn indexed(&self, index: IndexedColor) -> u32 {
        self.indexed_colors[index as usize]
    }

    /// Returns a color from the palette.
    ///
    /// To facilitate constant folding by the compiler,
    /// alpha is given as a fraction (`numerator` / `denominator`).
    #[inline]
    pub fn indexed_alpha(&self, index: IndexedColor, numerator: u32, denominator: u32) -> u32 {
        let c = self.indexed_colors[index as usize];
        let a = 255 * numerator / denominator;
        let r = (((c >> 16) & 0xFF) * numerator) / denominator;
        let g = (((c >> 8) & 0xFF) * numerator) / denominator;
        let b = ((c & 0xFF) * numerator) / denominator;
        a << 24 | r << 16 | g << 8 | b
    }

    /// Returns a color opposite to the brightness of the given `color`.
    pub fn contrasted(&self, color: u32) -> u32 {
        let idx = (color as usize).wrapping_mul(HASH_MULTIPLIER) >> CACHE_TABLE_SHIFT;
        let slot = self.contrast_colors[idx].get();
        if slot.0 == color { slot.1 } else { self.contrasted_slow(color) }
    }

    #[cold]
    fn contrasted_slow(&self, color: u32) -> u32 {
        let idx = (color as usize).wrapping_mul(HASH_MULTIPLIER) >> CACHE_TABLE_SHIFT;
        let contrast = self.auto_colors[Self::is_dark(color) as usize];
        self.contrast_colors[idx].set((color, contrast));
        contrast
    }

    fn is_dark(color: u32) -> bool {
        srgb_to_oklab(color).l < 0.5
    }

    /// Blends the given sRGB color onto the background bitmap.
    ///
    /// TODO: The current approach blends foreground/background independently,
    /// but ideally `blend_bg` with semi-transparent dark should also darken text below it.
    pub fn blend_bg(&mut self, target: Rect, bg: u32) {
        let back = &mut self.buffers[self.frame_counter & 1];
        back.bg_bitmap.blend(target, bg);
    }

    /// Blends the given sRGB color onto the foreground bitmap.
    ///
    /// TODO: The current approach blends foreground/background independently,
    /// but ideally `blend_fg` should blend with the background color below it.
    pub fn blend_fg(&mut self, target: Rect, fg: u32) {
        let back = &mut self.buffers[self.frame_counter & 1];
        back.fg_bitmap.blend(target, fg);
    }

    /// Reverses the foreground and background colors in the given rectangle.
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

    /// Replaces VT attributes in the given rectangle.
    pub fn replace_attr(&mut self, target: Rect, mask: Attributes, attr: Attributes) {
        let back = &mut self.buffers[self.frame_counter & 1];
        back.attributes.replace(target, mask, attr);
    }

    /// Sets the current visible cursor position and type.
    ///
    /// Call this when focus is inside an editable area and you want to show the cursor.
    pub fn set_cursor(&mut self, pos: Point, overtype: bool) {
        let back = &mut self.buffers[self.frame_counter & 1];
        back.cursor.pos = pos;
        back.cursor.overtype = overtype;
    }

    /// Renders the framebuffer contents accumulated since the
    /// last call to `flip()` and returns them serialized as VT.
    pub fn render<'a>(&mut self, arena: &'a Arena) -> ArenaString<'a> {
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

        let mut result = ArenaString::new_in(arena);
        let mut last_bg = u64::MAX;
        let mut last_fg = u64::MAX;
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
            let mut cfg = MeasurementConfig::new(&line_bytes);
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

                if last_bg != bg as u64 {
                    last_bg = bg as u64;
                    self.format_color(&mut result, false, bg);
                }

                if last_fg != fg as u64 {
                    last_fg = fg as u64;
                    self.format_color(&mut result, true, fg);
                }

                if last_attr != attr {
                    let diff = last_attr ^ attr;
                    if diff.is(Attributes::Italic) {
                        if attr.is(Attributes::Italic) {
                            result.push_str("\x1b[3m");
                        } else {
                            result.push_str("\x1b[23m");
                        }
                    }
                    if diff.is(Attributes::Underlined) {
                        if attr.is(Attributes::Underlined) {
                            result.push_str("\x1b[4m");
                        } else {
                            result.push_str("\x1b[24m");
                        }
                    }
                    last_attr = attr;
                }

                let beg = cfg.cursor().offset;
                let end = cfg.goto_visual(Point { x: chunk_end as CoordType, y: 0 }).offset;
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

    fn format_color(&self, dst: &mut ArenaString, fg: bool, mut color: u32) {
        let typ = if fg { '3' } else { '4' };

        // Some terminals support transparent backgrounds which are used
        // if the default background color is active (CSI 49 m).
        //
        // If [`Framebuffer::set_indexed_colors`] was never called, we assume
        // that the terminal doesn't support transparency and initialize the
        // background bitmap with the `DEFAULT_THEME` default background color.
        // Otherwise, we assume that the terminal supports transparency
        // and initialize it with 0x00000000 (transparent).
        //
        // We also apply this to the foreground color, because it compresses
        // the output slightly and ensures that we keep "default foreground"
        // and "color that happens to be default foreground" separate.
        // (This also applies to the background color by the way.)
        if color == 0 {
            _ = write!(dst, "\x1b[{typ}9m");
            return;
        }

        if (color & 0xff000000) != 0xff000000 {
            let idx = if fg { IndexedColor::Foreground } else { IndexedColor::Background };
            let dst = self.indexed(idx);
            color = oklab_blend(dst, color);
        }

        let r = color & 0xff;
        let g = (color >> 8) & 0xff;
        let b = (color >> 16) & 0xff;
        _ = write!(dst, "\x1b[{typ}8;2;{r};{g};{b}m");
    }
}

#[derive(Default)]
struct Buffer {
    text: LineBuffer,
    bg_bitmap: Bitmap,
    fg_bitmap: Bitmap,
    attributes: AttributeBuffer,
    cursor: Cursor,
}

/// A buffer for the text contents of the framebuffer.
#[derive(Default)]
struct LineBuffer {
    lines: Vec<String>,
    size: Size,
}

impl LineBuffer {
    fn new(size: Size) -> Self {
        Self { lines: vec![String::new(); size.height as usize], size }
    }

    fn fill_whitespace(&mut self) {
        let width = self.size.width as usize;
        for l in &mut self.lines {
            l.clear();
            l.reserve(width + width / 2);

            let buf = unsafe { l.as_mut_vec() };
            // Compiles down to `memset()`.
            buf.extend(std::iter::repeat_n(b' ', width));
        }
    }

    /// Replaces text contents in a single line of the framebuffer.
    /// All coordinates are in viewport coordinates.
    /// Assumes that control characters have been replaced or escaped.
    fn replace_text(
        &mut self,
        y: CoordType,
        origin_x: CoordType,
        clip_right: CoordType,
        text: &str,
    ) {
        let Some(line) = self.lines.get_mut(y as usize) else {
            return;
        };

        let bytes = text.as_bytes();
        let clip_right = clip_right.clamp(0, self.size.width);
        let layout_width = clip_right - origin_x;

        // Can't insert text that can't fit or is empty.
        if layout_width <= 0 || bytes.is_empty() {
            return;
        }

        let mut cfg = MeasurementConfig::new(&bytes);

        // Check if the text intersects with the left edge of the framebuffer
        // and figure out the parts that are inside.
        let mut left = origin_x;
        if left < 0 {
            let mut cursor = cfg.goto_visual(Point { x: -left, y: 0 });

            if left + cursor.visual_pos.x < 0 && cursor.offset < text.len() {
                // `-left` must've intersected a wide glyph and since goto_visual stops _before_ reaching the target,
                // we stopped before the wide glyph and thus must step forward to the next glyph.
                cursor = cfg.goto_logical(Point { x: cursor.logical_pos.x + 1, y: 0 });
            }

            left += cursor.visual_pos.x;
        }

        // If the text still starts outside the framebuffer, we must've ran out of text above.
        // Otherwise, if it starts outside the right edge to begin with, we can't insert it anyway.
        if left < 0 || left >= clip_right {
            return;
        }

        // Measure the width of the new text (= `res_new.visual_target.x`).
        let beg_off = cfg.cursor().offset;
        let end = cfg.goto_visual(Point { x: layout_width, y: 0 });

        // Figure out at which byte offset the new text gets inserted.
        let right = left + end.visual_pos.x;
        let line_bytes = line.as_bytes();
        let mut cfg_old = MeasurementConfig::new(&line_bytes);
        let res_old_beg = cfg_old.goto_visual(Point { x: left, y: 0 });
        let mut res_old_end = cfg_old.goto_visual(Point { x: right, y: 0 });

        // Since the goto functions will always stop short of the target position,
        // we need to manually step beyond it if we intersect with a wide glyph.
        if res_old_end.visual_pos.x < right {
            res_old_end = cfg_old.goto_logical(Point { x: res_old_end.logical_pos.x + 1, y: 0 });
        }

        // If we intersect a wide glyph, we need to pad the new text with spaces.
        let src = &text[beg_off..end.offset];
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
    }
}

/// An sRGB bitmap.
#[derive(Default)]
struct Bitmap {
    data: Vec<u32>,
    size: Size,
}

impl Bitmap {
    fn new(size: Size) -> Self {
        Self { data: vec![0; (size.width * size.height) as usize], size }
    }

    fn fill(&mut self, color: u32) {
        memset(&mut self.data, color);
    }

    /// Blends the given sRGB color onto the bitmap.
    ///
    /// This uses the `oklab` color space for blending so the
    /// resulting colors may look different from what you'd expect.
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
                memset(data, color);
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

                    let c = oklab_blend(c, color);
                    memset(&mut data[chunk_beg..chunk_end], c);

                    off < end
                } {}
            }
        }
    }

    /// Iterates over each row in the bitmap.
    fn iter(&self) -> ChunksExact<u32> {
        self.data.chunks_exact(self.size.width as usize)
    }
}

/// A bitfield for VT text attributes.
///
/// It being a bitfield allows for simple diffing.
#[repr(transparent)]
#[derive(Default, Clone, Copy, PartialEq, Eq)]
pub struct Attributes(u8);

#[allow(non_upper_case_globals)]
impl Attributes {
    pub const None: Self = Self(0);
    pub const Italic: Self = Self(0b1);
    pub const Underlined: Self = Self(0b10);
    pub const All: Self = Self(0b11);

    pub const fn is(self, attr: Self) -> bool {
        (self.0 & attr.0) == attr.0
    }
}

unsafe impl MemsetSafe for Attributes {}

impl BitOr for Attributes {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}

impl BitXor for Attributes {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self::Output {
        Self(self.0 ^ rhs.0)
    }
}

/// Stores VT attributes for the framebuffer.
#[derive(Default)]
struct AttributeBuffer {
    data: Vec<Attributes>,
    size: Size,
}

impl AttributeBuffer {
    fn new(size: Size) -> Self {
        Self { data: vec![Default::default(); (size.width * size.height) as usize], size }
    }

    fn reset(&mut self) {
        memset(&mut self.data, Default::default());
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
            let dst = &mut self.data[beg..end];

            if mask == Attributes::All {
                memset(dst, attr);
            } else {
                for a in dst {
                    *a = Attributes(a.0 & !mask.0 | attr.0);
                }
            }
        }
    }

    /// Iterates over each row in the bitmap.
    fn iter(&self) -> ChunksExact<Attributes> {
        self.data.chunks_exact(self.size.width as usize)
    }
}

/// Stores cursor position and type for the framebuffer.
#[derive(Default, PartialEq, Eq)]
struct Cursor {
    pos: Point,
    overtype: bool,
}

impl Cursor {
    const fn new_invalid() -> Self {
        Self { pos: Point::MIN, overtype: false }
    }

    const fn new_disabled() -> Self {
        Self { pos: Point { x: -1, y: -1 }, overtype: false }
    }
}
