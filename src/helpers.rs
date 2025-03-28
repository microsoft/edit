use std::borrow::Cow;
use std::cmp::Ordering;
use std::ffi::{CStr, CString, OsStr, OsString, c_char};
use std::mem;
use std::path::{Path, PathBuf};
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

pub fn string_append_repeat(dst: &mut String, ch: char, total_copies: usize) {
    if total_copies == 0 {
        return;
    }

    let buf = unsafe { dst.as_mut_vec() };

    if ch.is_ascii() {
        // Compiles down to `memset()`.
        buf.extend(std::iter::repeat_n(ch as u8, total_copies));
    } else {
        // Implements efficient string padding using quadratic duplication.
        let mut utf8_buf = [0; 4];
        let utf8 = ch.encode_utf8(&mut utf8_buf).as_bytes();
        let initial_len = buf.len();
        let added_len = utf8.len() * total_copies;
        let final_len = initial_len + added_len;

        buf.reserve(added_len);
        buf.extend_from_slice(utf8);

        while buf.len() != final_len {
            let end = (final_len - buf.len() + initial_len).min(buf.len());
            buf.extend_from_within(initial_len..end);
        }
    }
}

/// `std::cmp::minmax` is unstable, as per usual.
pub fn minmax<T>(v1: T, v2: T) -> [T; 2]
where
    T: Ord,
{
    if v2 < v1 { [v2, v1] } else { [v1, v2] }
}

pub struct DisplayablePathBuf {
    value: PathBuf,
    str: Cow<'static, str>,
}

impl DisplayablePathBuf {
    pub fn new(value: PathBuf) -> Self {
        let str = value.to_string_lossy();
        let str = unsafe { mem::transmute(str) };
        Self { value, str }
    }

    pub fn as_path(&self) -> &Path {
        &self.value
    }

    pub fn as_str(&self) -> &str {
        &self.str
    }

    pub fn as_bytes(&self) -> &[u8] {
        self.value.as_os_str().as_encoded_bytes()
    }

    pub fn clone_path_buf(&self) -> PathBuf {
        self.value.clone()
    }

    pub fn take(self) -> PathBuf {
        self.value
    }
}

impl Default for DisplayablePathBuf {
    fn default() -> Self {
        Self {
            value: PathBuf::default(),
            str: Cow::Borrowed(""),
        }
    }
}

impl Clone for DisplayablePathBuf {
    fn clone(&self) -> Self {
        DisplayablePathBuf::new(self.value.clone())
    }
}

impl From<OsString> for DisplayablePathBuf {
    fn from(s: OsString) -> DisplayablePathBuf {
        DisplayablePathBuf::new(PathBuf::from(s))
    }
}

impl<T: ?Sized + AsRef<OsStr>> From<&T> for DisplayablePathBuf {
    fn from(s: &T) -> DisplayablePathBuf {
        DisplayablePathBuf::new(PathBuf::from(s))
    }
}

pub struct DisplayableCString {
    value: CString,
    str: Cow<'static, str>,
}

impl DisplayableCString {
    pub fn new(value: CString) -> Self {
        let str = value.to_string_lossy();
        let str = unsafe { mem::transmute(str) };
        Self { value, str }
    }

    pub unsafe fn from_ptr(ptr: *const c_char) -> Self {
        let s = unsafe { CStr::from_ptr(ptr) };
        Self::new(s.to_owned())
    }

    pub fn as_cstr(&self) -> &CStr {
        &self.value
    }

    pub fn as_str(&self) -> &str {
        &self.str
    }
}

#[inline]
#[must_use]
pub const unsafe fn str_from_raw_parts<'a>(ptr: *const u8, len: usize) -> &'a str {
    unsafe { str::from_utf8_unchecked(slice::from_raw_parts(ptr, len)) }
}

#[inline]
#[must_use]
pub const unsafe fn str_from_raw_parts_mut<'a>(ptr: *mut u8, len: usize) -> &'a mut str {
    unsafe { str::from_utf8_unchecked_mut(slice::from_raw_parts_mut(ptr, len)) }
}

pub fn vec_insert_at<T: Copy>(dst: &mut Vec<T>, off: usize, src: &[T]) {
    unsafe {
        let dst_len = dst.len();
        let src_len = src.len();

        // Make room for the new elements. NOTE that this must be done before
        // we call as_mut_ptr, or else we risk accessing a stale pointer.
        dst.reserve(src_len);

        let off = off.min(dst_len);
        let ptr = dst.as_mut_ptr().add(off);

        if off < dst_len {
            // Move the tail of the vector to make room for the new elements.
            std::ptr::copy(ptr, ptr.add(src_len), dst_len - off);
        }

        // Copy the new elements into the vector.
        std::ptr::copy_nonoverlapping(src.as_ptr(), ptr, src_len);
        // Update the length of the vector.
        dst.set_len(dst_len + src_len);
    }
}

// How many functions do you want stuck in unstable? Oh all of them? Okay.
pub fn string_from_utf8_lossy_owned(v: Vec<u8>) -> String {
    if let Cow::Owned(string) = String::from_utf8_lossy(&v) {
        string
    } else {
        unsafe { String::from_utf8_unchecked(v) }
    }
}

pub fn vec_replace_all_reuse<T: Clone>(dst: &mut Vec<T>, src: &[T]) {
    dst.clear();
    dst.extend_from_slice(src);
}

// Works just like `std::hint::cold_path`, but it's stable.
#[cold]
#[inline(always)]
pub const fn cold_path() {}
