// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! Random assortment of helpers I didn't know where to put.

use std::cmp::Ordering;
use std::io::{self, Read};
use std::mem::MaybeUninit;
use std::{fmt, slice};

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
    pub const MIN: Self = Self { width: 0, height: 0 };
    pub const MAX: Self = Self { width: CoordType::MAX, height: CoordType::MAX };

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

/// [`Read`] but with [`MaybeUninit<u8>`] buffers.
pub fn file_read_uninit<T: Read>(file: &mut T, buf: &mut [MaybeUninit<u8>]) -> io::Result<usize> {
    unsafe {
        let buf_slice = slice::from_raw_parts_mut(buf.as_mut_ptr() as *mut u8, buf.len());
        let n = file.read(buf_slice)?;
        Ok(n)
    }
}
