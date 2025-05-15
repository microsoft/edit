use std::alloc::Allocator;
use std::cmp::Ordering;
use std::io::Read;
use std::mem::{self, MaybeUninit};
use std::ops::{Bound, Range, RangeBounds};
use std::{ptr, slice, str};

use crate::apperr;

pub const KILO: usize = 1000;
pub const MEGA: usize = 1000 * 1000;
pub const GIGA: usize = 1000 * 1000 * 1000;

pub const KIBI: usize = 1024;
pub const MEBI: usize = 1024 * 1024;
pub const GIBI: usize = 1024 * 1024 * 1024;

pub type CoordType = i32;

pub const COORD_TYPE_SAFE_MAX: CoordType = 32767;
pub const COORD_TYPE_SAFE_MIN: CoordType = -32767 - 1;

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct Point {
    pub x: CoordType,
    pub y: CoordType,
}

impl Point {
    pub const MIN: Point = Point { x: CoordType::MIN, y: CoordType::MIN };
    pub const MAX: Point = Point { x: CoordType::MAX, y: CoordType::MAX };
}

impl PartialOrd<Point> for Point {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Point {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.y.cmp(&other.y) {
            Ordering::Equal => self.x.cmp(&other.x),
            ord => ord,
        }
    }
}

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

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct Rect {
    pub left: CoordType,
    pub top: CoordType,
    pub right: CoordType,
    pub bottom: CoordType,
}

impl Rect {
    pub fn one(value: CoordType) -> Self {
        Self { left: value, top: value, right: value, bottom: value }
    }

    pub fn two(top_bottom: CoordType, left_right: CoordType) -> Self {
        Self { left: left_right, top: top_bottom, right: left_right, bottom: top_bottom }
    }

    pub fn three(top: CoordType, left_right: CoordType, bottom: CoordType) -> Self {
        Self { left: left_right, top, right: left_right, bottom }
    }

    pub fn is_empty(&self) -> bool {
        self.left >= self.right || self.top >= self.bottom
    }

    pub fn width(&self) -> CoordType {
        self.right - self.left
    }

    pub fn height(&self) -> CoordType {
        self.bottom - self.top
    }

    pub fn contains(&self, point: Point) -> bool {
        point.x >= self.left && point.x < self.right && point.y >= self.top && point.y < self.bottom
    }

    pub fn intersect(&self, rhs: Self) -> Self {
        let l = self.left.max(rhs.left);
        let t = self.top.max(rhs.top);
        let r = self.right.min(rhs.right);
        let b = self.bottom.min(rhs.bottom);

        // Ensure that the size is non-negative. This avoids bugs,
        // because some height/width is negative all of a sudden.
        let r = l.max(r);
        let b = t.max(b);

        Rect { left: l, top: t, right: r, bottom: b }
    }
}

/// `std::cmp::minmax` is unstable, as per usual.
pub fn minmax<T>(v1: T, v2: T) -> [T; 2]
where
    T: Ord,
{
    if v2 < v1 { [v2, v1] } else { [v1, v2] }
}

#[inline(always)]
#[allow(clippy::ptr_eq)]
fn opt_ptr<T>(a: Option<&T>) -> *const T {
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

pub fn slice_copy_safe<T: Copy>(dst: &mut [T], src: &[T]) -> usize {
    let len = src.len().min(dst.len());
    unsafe { ptr::copy_nonoverlapping(src.as_ptr(), dst.as_mut_ptr(), len) };
    len
}

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

#[inline(always)]
pub const fn slice_as_uninit_ref<T>(slice: &[T]) -> &[MaybeUninit<T>] {
    unsafe { slice::from_raw_parts(slice.as_ptr() as *const MaybeUninit<T>, slice.len()) }
}

#[inline(always)]
pub const fn slice_as_uninit_mut<T>(slice: &mut [T]) -> &mut [MaybeUninit<T>] {
    unsafe { slice::from_raw_parts_mut(slice.as_mut_ptr() as *mut MaybeUninit<T>, slice.len()) }
}
