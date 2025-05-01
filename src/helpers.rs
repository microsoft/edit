use crate::apperr;
use std::borrow::Cow;
use std::cmp::Ordering;
use std::io::Read;
use std::mem::{self, MaybeUninit};
use std::ptr;
use std::slice;
use std::str;

pub type CoordType = i32;

pub const COORD_TYPE_SAFE_MIN: CoordType = -32767 - 1;
pub const COORD_TYPE_SAFE_MAX: CoordType = 32767;

#[derive(Clone, Copy, PartialEq, Eq, Default, Debug)]
pub struct Point {
    pub x: CoordType,
    pub y: CoordType,
}

impl Point {
    pub const MIN: Point = Point {
        x: CoordType::MIN,
        y: CoordType::MIN,
    };
    pub const MAX: Point = Point {
        x: CoordType::MAX,
        y: CoordType::MAX,
    };
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

#[derive(Clone, Copy, PartialEq, Eq, Default, Debug)]
pub struct Size {
    pub width: CoordType,
    pub height: CoordType,
}

impl Size {
    pub fn as_rect(&self) -> Rect {
        Rect {
            left: 0,
            top: 0,
            right: self.width,
            bottom: self.height,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Default, Debug)]
pub struct Rect {
    pub left: CoordType,
    pub top: CoordType,
    pub right: CoordType,
    pub bottom: CoordType,
}

impl Rect {
    pub fn one(value: CoordType) -> Self {
        Self {
            left: value,
            top: value,
            right: value,
            bottom: value,
        }
    }

    pub fn two(top_bottom: CoordType, left_right: CoordType) -> Self {
        Self {
            left: left_right,
            top: top_bottom,
            right: left_right,
            bottom: top_bottom,
        }
    }

    pub fn three(top: CoordType, left_right: CoordType, bottom: CoordType) -> Self {
        Self {
            left: left_right,
            top,
            right: left_right,
            bottom,
        }
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

        Rect {
            left: l,
            top: t,
            right: r,
            bottom: b,
        }
    }
}

unsafe fn wyr3(p: *const u8, k: usize) -> u64 {
    let p0 = unsafe { p.read() as u64 };
    let p1 = unsafe { p.add(k >> 1).read() as u64 };
    let p2 = unsafe { p.add(k - 1).read() as u64 };
    (p0 << 16) | (p1 << 8) | p2
}

unsafe fn wyr4(p: *const u8) -> u64 {
    unsafe { (p as *const u32).read_unaligned() as u64 }
}

unsafe fn wyr8(p: *const u8) -> u64 {
    unsafe { (p as *const u64).read_unaligned() }
}

// This is a weak mix function on its own. It may be worth considering
// replacing external uses of this function with a stronger one.
// On the other hand, it's very fast.
pub fn wymix(lhs: u64, rhs: u64) -> u64 {
    let lhs = lhs as u128;
    let rhs = rhs as u128;
    let r = lhs * rhs;
    (r >> 64) as u64 ^ (r as u64)
}

// The venerable wyhash hash function. It's fast and has good statistical properties.
// It's in the public domain.
pub fn hash(mut seed: u64, data: &[u8]) -> u64 {
    unsafe {
        const S0: u64 = 0xa0761d6478bd642f;
        const S1: u64 = 0xe7037ed1a0b428db;
        const S2: u64 = 0x8ebc6af09c88c6e3;
        const S3: u64 = 0x589965cc75374cc3;

        let len = data.len();
        let mut p = data.as_ptr();
        let a;
        let b;

        seed ^= S0;

        if len <= 16 {
            if len >= 4 {
                a = (wyr4(p) << 32) | wyr4(p.add((len >> 3) << 2));
                b = (wyr4(p.add(len - 4)) << 32) | wyr4(p.add(len - 4 - ((len >> 3) << 2)));
            } else if len > 0 {
                a = wyr3(p, len);
                b = 0;
            } else {
                a = 0;
                b = 0;
            }
        } else {
            let mut i = len;
            if i > 48 {
                let mut seed1 = seed;
                let mut seed2 = seed;
                while {
                    seed = wymix(wyr8(p) ^ S1, wyr8(p.add(8)) ^ seed);
                    seed1 = wymix(wyr8(p.add(16)) ^ S2, wyr8(p.add(24)) ^ seed1);
                    seed2 = wymix(wyr8(p.add(32)) ^ S3, wyr8(p.add(40)) ^ seed2);
                    p = p.add(48);
                    i -= 48;
                    i > 48
                } {}
                seed ^= seed1 ^ seed2;
            }
            while i > 16 {
                seed = wymix(wyr8(p) ^ S1, wyr8(p.add(8)) ^ seed);
                i -= 16;
                p = p.add(16);
            }
            a = wyr8(p.offset(i as isize - 16));
            b = wyr8(p.offset(i as isize - 8));
        }

        wymix(S1 ^ (len as u64), wymix(a ^ S1, b ^ seed))
    }
}

pub fn hash_str(seed: u64, s: &str) -> u64 {
    hash(seed, s.as_bytes())
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

/// Creates a `&mut str` from a pointer and a length.
///
/// # Safety
///
/// The given data must be valid UTF-8.
/// The given data must outlive the returned reference.
#[inline]
#[must_use]
pub const unsafe fn str_from_raw_parts_mut<'a>(ptr: *mut u8, len: usize) -> &'a mut str {
    unsafe { str::from_utf8_unchecked_mut(slice::from_raw_parts_mut(ptr, len)) }
}

pub fn vec_replace<T: Copy>(dst: &mut Vec<T>, off: usize, remove: usize, src: &[T]) {
    unsafe {
        let dst_len = dst.len();
        let src_len = src.len();
        let off = off.min(dst_len);
        let del_len = remove.min(dst_len - off);

        if del_len == 0 && src_len == 0 {
            return; // nothing to do
        }

        let tail_len = dst_len - off - del_len;
        let new_len = dst_len - del_len + src_len;

        if src_len > del_len {
            dst.reserve(src_len - del_len);
        }

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

pub fn vec_replace_all_reuse<T: Clone>(dst: &mut Vec<T>, src: &[T]) {
    dst.clear();
    dst.extend_from_slice(src);
}

pub fn string_from_utf8_lossy_owned(v: Vec<u8>) -> String {
    if let Cow::Owned(string) = String::from_utf8_lossy(&v) {
        string
    } else {
        unsafe { String::from_utf8_unchecked(v) }
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

/// Turns a `&[MaybeUninit<T>]` into a `&[T]`.
/// A copy of `MaybeUninit::assume_init_ref` which is unstable (yay).
///
/// # Safety
///
/// Rust is happy to blast you in the face if you look at a `MaybeUninit` the wrong way.
/// As such, make sure that the slice is fully initialized.
#[inline(always)]
pub const unsafe fn slice_assume_init_ref<T>(m: &[MaybeUninit<T>]) -> &[T] {
    unsafe { &*(m as *const _ as *const [T]) }
}

/// Turns a `&mut [MaybeUninit<T>]` into a `&mut [T]`.
/// A copy of `MaybeUninit::assume_init_mut` which is unstable (yay).
///
/// # Safety
///
/// Rust is happy to blast you in the face if you look at a `MaybeUninit` the wrong way.
/// As such, make sure that the slice is fully initialized.
#[inline(always)]
pub const unsafe fn slice_assume_init_mut<T>(m: &mut [MaybeUninit<T>]) -> &mut [T] {
    unsafe { &mut *(m as *mut _ as *mut [T]) }
}

#[inline(always)]
pub const fn slice_as_uninit_ref<T>(slice: &[T]) -> &[MaybeUninit<T>] {
    unsafe { slice::from_raw_parts(slice.as_ptr() as *const MaybeUninit<T>, slice.len()) }
}

#[inline(always)]
pub const fn slice_as_uninit_mut<T>(slice: &mut [T]) -> &mut [MaybeUninit<T>] {
    unsafe { slice::from_raw_parts_mut(slice.as_mut_ptr() as *mut MaybeUninit<T>, slice.len()) }
}

// Works just like `std::hint::cold_path`, but it's stable.
#[cold]
#[inline(always)]
pub const fn cold_path() {}
