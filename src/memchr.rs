//! Rust has a very popular `memchr` crate. It's quite fast, so you may ask yourself
//! why we don't just use it: Simply put, this is optimized for short inputs.

use std::ptr::{self, null};

/// memchr(), but with two needles.
/// Returns the index of the first occurrence of either needle in the `haystack`.
/// If no needle is found, `haystack.len()` is returned.
/// `offset` specifies the index to start searching from.
pub fn memchr2(needle1: u8, needle2: u8, haystack: &[u8], offset: usize) -> usize {
    unsafe {
        let beg = haystack.as_ptr();
        let end = beg.add(haystack.len());
        let it = beg.add(offset.min(haystack.len()));
        let it = memchr2_raw(needle1, needle2, it, end);
        distance(it, beg)
    }
}

// In order to make `memchr2_raw` slim and fast, we use a function pointer that updates
// itself to the correct implementation on the first call. This reduces binary size.
// It would also reduce branches if we had >2 implementations (a jump still needs to be predicted).
// NOTE that this ONLY works if Control Flow Guard is disabled on Windows.
#[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
static mut MEMCHR2_DISPATCH: unsafe fn(
    needle1: u8,
    needle2: u8,
    beg: *const u8,
    end: *const u8,
) -> *const u8 = memchr2_dispatch;

#[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
unsafe fn memchr2_dispatch(needle1: u8, needle2: u8, beg: *const u8, end: *const u8) -> *const u8 {
    let func = if is_x86_feature_detected!("avx2") {
        memchr2_avx2
    } else {
        memchr2_fallback
    };
    unsafe { MEMCHR2_DISPATCH = func };
    unsafe { func(needle1, needle2, beg, end) }
}

unsafe fn memchr2_raw(needle1: u8, needle2: u8, beg: *const u8, end: *const u8) -> *const u8 {
    #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
    return unsafe { MEMCHR2_DISPATCH(needle1, needle2, beg, end) };

    #[cfg(target_arch = "aarch64")]
    return unsafe { memchr2_neon(needle1, needle2, beg, end) };

    #[allow(unreachable_code)]
    return unsafe { memchr2_fallback(needle1, needle2, beg, end) };
}

unsafe fn memchr2_fallback(
    needle1: u8,
    needle2: u8,
    mut beg: *const u8,
    end: *const u8,
) -> *const u8 {
    unsafe {
        while !ptr::eq(beg, end) {
            let ch = *beg;
            if ch == needle1 || ch == needle2 {
                break;
            }
            beg = beg.add(1);
        }
        beg
    }
}

// FWIW, I found that adding support for AVX512 was not useful at the time,
// as it only marginally improved file load performance by <5%.
#[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
#[target_feature(enable = "avx2")]
unsafe fn memchr2_avx2(needle1: u8, needle2: u8, mut beg: *const u8, end: *const u8) -> *const u8 {
    unsafe {
        use std::arch::x86_64::*;

        let n1 = _mm256_set1_epi8(needle1 as i8);
        let n2 = _mm256_set1_epi8(needle2 as i8);
        let mut remaining = distance(end, beg);

        while remaining >= 32 {
            let v = _mm256_loadu_si256(beg as *const _);
            let a = _mm256_cmpeq_epi8(v, n1);
            let b = _mm256_cmpeq_epi8(v, n2);
            let c = _mm256_or_si256(a, b);
            let m = _mm256_movemask_epi8(c) as u32;

            if m != 0 {
                return beg.add(m.trailing_zeros() as usize);
            }

            beg = beg.add(32);
            remaining -= 32;
        }

        memchr2_fallback(needle1, needle2, beg, end)

        // TODO: This code probably works correctly but requires more testing.
        /*
        // Handle the remaining <32 bytes by reading 32 bytes and masking out the irrelevant data.
        // This works, because x86 does not care about slice boundaries. It does care about page boundaries, however.
        if remaining > 0 {
            // Data beyond the beg/end range may not be mapped in. As such, we need to avoid reading beyond the
            // page boundaries. This assumes 4KiB pages or larger. If we're in the lower half of the 4KiB page,
            // we load data from `end.sub(off) == end.sub(remaining) == beg`, since we know that this 32-byte read
            // can't possibly read 2KiB. Otherwise, we load from `end.sub(off) == end.sub(32)`, which essentially
            // means we read such that the end of the read is aligned with the end of the haystack. The start of the
            // SIMD register will then contain garbage we must ignore.
            let off = if ((beg as usize) & 2048) != 0 {
                32
            } else {
                remaining
            };

            let v = _mm256_loadu_si256(end.sub(off) as *const _);
            let a = _mm256_cmpeq_epi8(v, n1);
            let b = _mm256_cmpeq_epi8(v, n2);
            let c = _mm256_or_si256(a, b);
            let m = _mm256_movemask_epi8(c) as u32;

            // If we were in the upper half of the 4KiB page, we must shift the mask such that it's not aligned with
            // the end of the haystack but rather with the current `beg`: A shift of `32 - remaining` is needed,
            // which equals `off - remaining`. Otherwise, we must not shift at all. Luckily `off` will be `remaining`
            // in that case and `remaining - remaining` is 0.
            let m = m >> (off - remaining);

            // If we were in the lower half of the 4KiB page, we must mask out anything beyond the end of
            // the haystack. Here, we basically restrict the "length" if `m` to contain `remaining`-many bits.
            // In case of a read in the upper half this won't do anything, but that's fine. Branchless code is great.
            let m = m & ((1 << remaining) - 1);

            if m != 0 {
                return beg.add(m.trailing_zeros() as usize);
            }
        }

        end
        */
    }
}

#[cfg(target_arch = "aarch64")]
unsafe fn memchr2_neon(needle1: u8, needle2: u8, mut beg: *const u8, end: *const u8) -> *const u8 {
    unsafe {
        use std::arch::aarch64::*;

        if distance(end, beg) >= 16 {
            let n1 = vdupq_n_u8(needle1);
            let n2 = vdupq_n_u8(needle2);

            loop {
                let v = vld1q_u8(beg as *const _);
                let a = vceqq_u8(v, n1);
                let b = vceqq_u8(v, n2);
                let c = vorrq_u8(a, b);

                // https://community.arm.com/arm-community-blogs/b/servers-and-cloud-computing-blog/posts/porting-x86-vector-bitmask-optimizations-to-arm-neon
                let m = vreinterpretq_u16_u8(c);
                let m = vshrn_n_u16(m, 4);
                let m = vreinterpret_u64_u8(m);
                let m = vget_lane_u64(m, 0);

                if m != 0 {
                    return beg.add(m.trailing_zeros() as usize >> 2);
                }

                beg = beg.add(16);
                if distance(end, beg) < 16 {
                    break;
                }
            }
        }

        memchr2_fallback(needle1, needle2, beg, end)
    }
}

/// Same as `memchr2`, but searches from the end of the haystack.
/// If no needle is found, 0 is returned.
///
/// *NOTE: Unlike `memchr2` (or `memrchr`), an offset PAST the hit is returned.*
/// This is because this function is primarily used for `ucd::newlines_backward`,
/// which needs exactly that.
pub fn memrchr2(needle1: u8, needle2: u8, haystack: &[u8], offset: usize) -> Option<usize> {
    unsafe {
        let beg = haystack.as_ptr();
        let it = beg.add(offset.min(haystack.len()));
        let it = memrchr2_raw(needle1, needle2, beg, it);
        if it.is_null() {
            None
        } else {
            Some(distance(it, beg))
        }
    }
}

#[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
static mut MEMRCHR2_DISPATCH: unsafe fn(
    needle1: u8,
    needle2: u8,
    beg: *const u8,
    end: *const u8,
) -> *const u8 = memrchr2_dispatch;

#[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
unsafe fn memrchr2_dispatch(needle1: u8, needle2: u8, beg: *const u8, end: *const u8) -> *const u8 {
    let func = if is_x86_feature_detected!("avx2") {
        memrchr2_avx2
    } else {
        memrchr2_fallback
    };
    unsafe { MEMRCHR2_DISPATCH = func };
    unsafe { func(needle1, needle2, beg, end) }
}

unsafe fn memrchr2_raw(needle1: u8, needle2: u8, beg: *const u8, end: *const u8) -> *const u8 {
    #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
    return unsafe { MEMRCHR2_DISPATCH(needle1, needle2, beg, end) };

    #[cfg(target_arch = "aarch64")]
    return unsafe { memrchr2_neon(needle1, needle2, beg, end) };

    #[allow(unreachable_code)]
    return unsafe { memrchr2_fallback(needle1, needle2, beg, end) };
}

unsafe fn memrchr2_fallback(
    needle1: u8,
    needle2: u8,
    beg: *const u8,
    mut end: *const u8,
) -> *const u8 {
    unsafe {
        while !ptr::eq(end, beg) {
            end = end.sub(1);
            let ch = *end;
            if ch == needle1 || needle2 == ch {
                return end;
            }
        }
        null()
    }
}

#[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
#[target_feature(enable = "avx2")]
unsafe fn memrchr2_avx2(needle1: u8, needle2: u8, beg: *const u8, mut end: *const u8) -> *const u8 {
    unsafe {
        use std::arch::x86_64::*;

        if distance(end, beg) >= 32 {
            let n1 = _mm256_set1_epi8(needle1 as i8);
            let n2 = _mm256_set1_epi8(needle2 as i8);

            loop {
                end = end.sub(32);

                let v = _mm256_loadu_si256(end as *const _);
                let a = _mm256_cmpeq_epi8(v, n1);
                let b = _mm256_cmpeq_epi8(v, n2);
                let c = _mm256_or_si256(a, b);
                let m = _mm256_movemask_epi8(c) as u32;

                if m != 0 {
                    return end.add(31 - m.leading_zeros() as usize);
                }

                if distance(end, beg) < 32 {
                    break;
                }
            }
        }

        memrchr2_fallback(needle1, needle2, beg, end)
    }
}

#[cfg(target_arch = "aarch64")]
unsafe fn memrchr2_neon(needle1: u8, needle2: u8, beg: *const u8, mut end: *const u8) -> *const u8 {
    unsafe {
        use std::arch::aarch64::*;

        if distance(end, beg) >= 16 {
            let n1 = vdupq_n_u8(needle1);
            let n2 = vdupq_n_u8(needle2);

            loop {
                end = end.sub(16);

                let v = vld1q_u8(end as *const _);
                let a = vceqq_u8(v, n1);
                let b = vceqq_u8(v, n2);
                let c = vorrq_u8(a, b);

                // https://community.arm.com/arm-community-blogs/b/servers-and-cloud-computing-blog/posts/porting-x86-vector-bitmask-optimizations-to-arm-neon
                let m = vreinterpretq_u16_u8(c);
                let m = vshrn_n_u16(m, 4);
                let m = vreinterpret_u64_u8(m);
                let m = vget_lane_u64(m, 0);

                if m != 0 {
                    return end.add(15 - (m.leading_zeros() as usize >> 2));
                }

                if distance(end, beg) < 16 {
                    break;
                }
            }
        }

        memrchr2_fallback(needle1, needle2, beg, end)
    }
}

/*pub struct Memchr2<'a> {
    needle1: u8,
    needle2: u8,
    beg: *const u8,
    end: *const u8,
    it: *const u8,
    _marker: PhantomData<&'a [u8]>,
}

impl<'a> Memchr2<'a> {
    pub fn new(needle1: u8, needle2: u8, haystack: &'a [u8]) -> Self {
        Self {
            needle1,
            needle2,
            beg: haystack.as_ptr(),
            end: unsafe { haystack.as_ptr().add(haystack.len()) },
            it: haystack.as_ptr(),
            _marker: PhantomData,
        }
    }
}

impl Iterator for Memchr2<'_> {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        if self.it.is_null() {
            return None;
        }

        self.it = unsafe { memchr2_raw(self.needle1, self.needle2, self.it, self.end) };
        if self.it.is_null() {
            return None;
        }

        let idx = unsafe { distance(self.it, self.beg) };
        self.it = if self.it == self.end {
            null()
        } else {
            unsafe { self.it.add(1) }
        };
        Some(idx)
    }
}

impl FusedIterator for Memchr2<'_> {}

pub struct memrchr2<'a> {
    needle1: u8,
    needle2: u8,
    beg: *const u8,
    it: *const u8,
    _marker: PhantomData<&'a [u8]>,
}

impl<'a> memrchr2<'a> {
    pub fn new(needle1: u8, needle2: u8, haystack: &'a [u8]) -> Self {
        Self {
            needle1,
            needle2,
            beg: haystack.as_ptr(),
            it: unsafe { haystack.as_ptr().add(haystack.len()) },
            _marker: PhantomData,
        }
    }
}

impl Iterator for memrchr2<'_> {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        if self.it.is_null() {
            return None;
        }

        self.it = unsafe { memrchr2_raw(self.needle1, self.needle2, self.beg, self.it) };
        if self.it.is_null() {
            return None;
        }

        let idx = unsafe { distance(self.it, self.beg) };
        self.it = if self.it == self.beg {
            null()
        } else {
            unsafe { self.it.sub(1) }
        };
        Some(idx)
    }
}

impl FusedIterator for memrchr2<'_> {}*/

// Can be replaced with `sub_ptr` once it's stabilized.
#[inline(always)]
unsafe fn distance<T>(hi: *const T, lo: *const T) -> usize {
    unsafe { usize::try_from(hi.offset_from(lo)).unwrap_unchecked() }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::sys;
    use std::slice;

    #[test]
    fn test_memchr2_empty() {
        assert_eq!(memchr2(b'a', b'b', b"", 0), 0);
    }

    #[test]
    fn test_empty() {
        assert_eq!(memrchr2(b'a', b'b', b"", 0), None);
    }

    #[test]
    fn test_basic() {
        let haystack = b"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
        let haystack = &haystack[..43];

        assert_eq!(memchr2(b'a', b'z', haystack, 0), 0);
        assert_eq!(memchr2(b'p', b'q', haystack, 0), 15);
        assert_eq!(memchr2(b'Q', b'Z', haystack, 0), 42);
        assert_eq!(memchr2(b'0', b'9', haystack, 0), haystack.len());

        assert_eq!(memrchr2(b'Q', b'P', haystack, 43), Some(42));
        assert_eq!(memrchr2(b'p', b'o', haystack, 43), Some(15));
        assert_eq!(memrchr2(b'a', b'b', haystack, 43), Some(1));
        assert_eq!(memrchr2(b'0', b'9', haystack, 43), None);
    }

    // Test that it doesn't match before/after the start offset respectively.
    #[test]
    fn test_with_offset() {
        let haystack = b"abcdefghabcdefghabcdefghabcdefghabcdefgh";

        assert_eq!(memrchr2(b'h', b'g', haystack, 40), Some(39));
        assert_eq!(memrchr2(b'h', b'g', haystack, 39), Some(38));
        assert_eq!(memrchr2(b'a', b'b', haystack, 9), Some(8));
        assert_eq!(memrchr2(b'a', b'b', haystack, 1), Some(0));
        assert_eq!(memrchr2(b'a', b'b', haystack, 0), None);

        assert_eq!(memchr2(b'a', b'b', haystack, 0), 0);
        assert_eq!(memchr2(b'a', b'b', haystack, 1), 1);
        assert_eq!(memchr2(b'a', b'b', haystack, 2), 8);
        assert_eq!(memchr2(b'a', b'b', haystack, 9), 9);
        assert_eq!(memchr2(b'a', b'b', haystack, 16), 16);
        assert_eq!(memchr2(b'a', b'b', haystack, 41), 40);
    }

    // Test memory access safety at page boundaries.
    // The test is a success if it doesn't segfault.
    #[test]
    fn test_page_boundary() {
        let page = unsafe {
            let page_size = 4096;

            // 3 pages: uncommitted, committed, uncommitted
            let ptr = sys::virtual_reserve(page_size * 3).unwrap();
            sys::virtual_commit(ptr.add(page_size), page_size).unwrap();
            slice::from_raw_parts_mut(ptr.add(page_size).as_ptr(), page_size)
        };

        page.fill(b'a');

        // Test if it seeks beyond the page boundary.
        assert_eq!(memchr2(b'\0', b'\0', &page[page.len() - 40..], 0), 40);
        // Test if it seeks before the page boundary for the masked/partial load.
        assert_eq!(memchr2(b'\0', b'\0', &page[..10], 0), 10);

        // Same as above, but for memrchr2 (hence reversed).
        assert_eq!(memrchr2(b'\0', b'\0', &page[page.len() - 10..], 10), None);
        assert_eq!(memrchr2(b'\0', b'\0', &page[..40], 40), None);
    }
}
