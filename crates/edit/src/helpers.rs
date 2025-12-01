// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! Random assortment of helpers I didn't know where to put.

use std::alloc::Allocator;
use std::cmp::Ordering;
use std::io::Read;
use std::mem::{self, MaybeUninit};
use std::ops::{Bound, Range, RangeBounds};
use std::{fmt, ptr, slice, str};

use crate::apperr;

pub const KILO: usize = 1000;
pub const MEGA: usize = 1000 * 1000;
pub const GIGA: usize = 1000 * 1000 * 1000;

pub const KIBI: usize = 1024;
pub const MEBI: usize = 1024 * 1024;
pub const GIBI: usize = 1024 * 1024 * 1024;

pub struct MetricFormatter<T>(pub T);

impl fmt::Display for MetricFormatter<usize> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut value = self.0;
        let mut suffix = "B";
        if value >= GIGA {
            value /= GIGA;
            suffix = "GB";
        } else if value >= MEGA {
            value /= MEGA;
            suffix = "MB";
        } else if value >= KILO {
            value /= KILO;
            suffix = "kB";
        }
        write!(f, "{value}{suffix}")
    }
}

/// A viewport coordinate type used throughout the application.
pub type CoordType = isize;

/// To avoid overflow issues because you're adding two [`CoordType::MAX`]
/// values together, you can use [`COORD_TYPE_SAFE_MAX`] instead.
///
/// It equates to half the bits contained in [`CoordType`], which
/// for instance is 32767 (0x7FFF) when [`CoordType`] is a [`i32`].
pub const COORD_TYPE_SAFE_MAX: CoordType = (1 << (CoordType::BITS / 2 - 1)) - 1;

/// A 2D point. Uses [`CoordType`].
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct Point {
    pub x: CoordType,
    pub y: CoordType,
}

impl Point {
    pub const MIN: Self = Self { x: CoordType::MIN, y: CoordType::MIN };
    pub const MAX: Self = Self { x: CoordType::MAX, y: CoordType::MAX };
}

impl PartialOrd<Self> for Point {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Point {
    fn cmp(&self, other: &Self) -> Ordering {
        self.y.cmp(&other.y).then(self.x.cmp(&other.x))
    }
}

/// A 2D size. Uses [`CoordType`].
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct Size {
    pub width: CoordType,
    pub height: CoordType,
}

impl Size {
    pub fn as_rect(&self) -> Rect {
        Rect { left: 0, top: 0, right: self.width, bottom: self.height }
    }
}

/// A 2D rectangle. Uses [`CoordType`].
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct Rect {
    pub left: CoordType,
    pub top: CoordType,
    pub right: CoordType,
    pub bottom: CoordType,
}

impl Rect {
    /// Mimics CSS's `padding` property where `padding: a` is `a a a a`.
    pub fn one(value: CoordType) -> Self {
        Self { left: value, top: value, right: value, bottom: value }
    }

    /// Mimics CSS's `padding` property where `padding: a b` is `a b a b`,
    /// and `a` is top/bottom and `b` is left/right.
    pub fn two(top_bottom: CoordType, left_right: CoordType) -> Self {
        Self { left: left_right, top: top_bottom, right: left_right, bottom: top_bottom }
    }

    /// Mimics CSS's `padding` property where `padding: a b c` is `a b c b`,
    /// and `a` is top, `b` is left/right, and `c` is bottom.
    pub fn three(top: CoordType, left_right: CoordType, bottom: CoordType) -> Self {
        Self { left: left_right, top, right: left_right, bottom }
    }

    /// Is the rectangle empty?
    pub fn is_empty(&self) -> bool {
        self.left >= self.right || self.top >= self.bottom
    }

    /// Width of the rectangle.
    pub fn width(&self) -> CoordType {
        self.right - self.left
    }

    /// Height of the rectangle.
    pub fn height(&self) -> CoordType {
        self.bottom - self.top
    }

    /// Check if it contains a point.
    pub fn contains(&self, point: Point) -> bool {
        point.x >= self.left && point.x < self.right && point.y >= self.top && point.y < self.bottom
    }

    /// Intersect two rectangles.
    pub fn intersect(&self, rhs: Self) -> Self {
        let l = self.left.max(rhs.left);
        let t = self.top.max(rhs.top);
        let r = self.right.min(rhs.right);
        let b = self.bottom.min(rhs.bottom);

        // Ensure that the size is non-negative. This avoids bugs,
        // because some height/width is negative all of a sudden.
        let r = l.max(r);
        let b = t.max(b);

        Self { left: l, top: t, right: r, bottom: b }
    }
}

/// [`std::cmp::minmax`] is unstable, as per usual.
pub fn minmax<T>(v1: T, v2: T) -> [T; 2]
where
    T: Ord,
{
    if v2 < v1 { [v2, v1] } else { [v1, v2] }
}

#[inline(always)]
#[allow(clippy::ptr_eq)]
pub fn opt_ptr<T>(a: Option<&T>) -> *const T {
    unsafe { mem::transmute(a) }
}

/// Surprisingly, there's no way in Rust to do a `ptr::eq` on `Option<&T>`.
/// Uses `unsafe` so that the debug performance isn't too bad.
#[inline(always)]
#[allow(clippy::ptr_eq)]
pub fn opt_ptr_eq<T>(a: Option<&T>, b: Option<&T>) -> bool {
    opt_ptr(a) == opt_ptr(b)
}

/// Creates a `&str` from a pointer and a length.
/// Exists, because `std::str::from_raw_parts` is unstable, par for the course.
///
/// # Safety
///
/// The given data must be valid UTF-8.
/// The given data must outlive the returned reference.
#[inline]
#[must_use]
pub const unsafe fn str_from_raw_parts<'a>(ptr: *const u8, len: usize) -> &'a str {
    unsafe { str::from_utf8_unchecked(slice::from_raw_parts(ptr, len)) }
}

/// [`<[T]>::copy_from_slice`] panics if the two slices have different lengths.
/// This one just returns the copied amount.
pub fn slice_copy_safe<T: Copy>(dst: &mut [T], src: &[T]) -> usize {
    let len = src.len().min(dst.len());
    unsafe { ptr::copy_nonoverlapping(src.as_ptr(), dst.as_mut_ptr(), len) };
    len
}

/// [`Vec::splice`] results in really bad assembly.
/// This doesn't. Don't use [`Vec::splice`].
pub trait ReplaceRange<T: Copy> {
    fn replace_range<R: RangeBounds<usize>>(&mut self, range: R, src: &[T]);
}

impl<T: Copy, A: Allocator> ReplaceRange<T> for Vec<T, A> {
    fn replace_range<R: RangeBounds<usize>>(&mut self, range: R, src: &[T]) {
        let start = match range.start_bound() {
            Bound::Included(&start) => start,
            Bound::Excluded(start) => start + 1,
            Bound::Unbounded => 0,
        };
        let end = match range.end_bound() {
            Bound::Included(end) => end + 1,
            Bound::Excluded(&end) => end,
            Bound::Unbounded => usize::MAX,
        };
        vec_replace_impl(self, start..end, src);
    }
}

fn vec_replace_impl<T: Copy, A: Allocator>(dst: &mut Vec<T, A>, range: Range<usize>, src: &[T]) {
    unsafe {
        let dst_len = dst.len();
        let src_len = src.len();
        let off = range.start.min(dst_len);
        let del_len = range.end.saturating_sub(off).min(dst_len - off);

        if del_len == 0 && src_len == 0 {
            return; // nothing to do
        }

        let tail_len = dst_len - off - del_len;
        let new_len = dst_len - del_len + src_len;

        if src_len > del_len {
            dst.reserve(src_len - del_len);
        }

        // NOTE: drop_in_place() is not needed here, because T is constrained to Copy.

        // SAFETY: as_mut_ptr() must called after reserve() to ensure that the pointer is valid.
        let ptr = dst.as_mut_ptr().add(off);

        // Shift the tail.
        if tail_len > 0 && src_len != del_len {
            ptr::copy(ptr.add(del_len), ptr.add(src_len), tail_len);
        }

        // Copy in the replacement.
        ptr::copy_nonoverlapping(src.as_ptr(), ptr, src_len);
        dst.set_len(new_len);
    }
}

/// [`Read`] but with [`MaybeUninit<u8>`] buffers.
pub fn file_read_uninit<T: Read>(
    file: &mut T,
    buf: &mut [MaybeUninit<u8>],
) -> apperr::Result<usize> {
    unsafe {
        let buf_slice = slice::from_raw_parts_mut(buf.as_mut_ptr() as *mut u8, buf.len());
        let n = file.read(buf_slice)?;
        Ok(n)
    }
}

/// Turns a [`&[u8]`] into a [`&[MaybeUninit<T>]`].
#[inline(always)]
pub const fn slice_as_uninit_ref<T>(slice: &[T]) -> &[MaybeUninit<T>] {
    unsafe { slice::from_raw_parts(slice.as_ptr() as *const MaybeUninit<T>, slice.len()) }
}

/// Turns a [`&mut [T]`] into a [`&mut [MaybeUninit<T>]`].
#[inline(always)]
pub const fn slice_as_uninit_mut<T>(slice: &mut [T]) -> &mut [MaybeUninit<T>] {
    unsafe { slice::from_raw_parts_mut(slice.as_mut_ptr() as *mut MaybeUninit<T>, slice.len()) }
}

/// Helpers for ASCII string comparisons.
pub trait AsciiStringHelpers {
    /// Tests if a string starts with a given ASCII prefix.
    ///
    /// This function name really is a mouthful, but it's a combination
    /// of [`str::starts_with`] and [`str::eq_ignore_ascii_case`].
    fn starts_with_ignore_ascii_case(&self, prefix: &str) -> bool;
}

impl AsciiStringHelpers for str {
    fn starts_with_ignore_ascii_case(&self, prefix: &str) -> bool {
        // Casting to bytes first ensures we skip any UTF8 boundary checks.
        // Since the comparison is ASCII, we don't need to worry about that.
        let s = self.as_bytes();
        let p = prefix.as_bytes();
        p.len() <= s.len() && s[..p.len()].eq_ignore_ascii_case(p)
    }
}
