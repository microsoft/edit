//! Rust has a very popular `memchr` crate. It's quite fast, so you may ask yourself
//! why we don't just use it: Simply put, this is optimized for short inputs.

use super::distance;
use std::ptr;

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
        ptr::null()
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
    let func = memrchr2_fallback;
    unsafe { MEMRCHR2_DISPATCH = func };
    unsafe { func(needle1, needle2, beg, end) }
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::sys;
    use std::slice;

    #[test]
    fn test_empty() {
        assert_eq!(memrchr2(b'a', b'b', b"", 0), None);
    }

    #[test]
    fn test_basic() {
        let haystack = b"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
        let haystack = &haystack[..43];

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

        // Same as above, but for memrchr2 (hence reversed).
        assert_eq!(memrchr2(b'\0', b'\0', &page[page.len() - 10..], 10), None);
        assert_eq!(memrchr2(b'\0', b'\0', &page[..40], 40), None);
    }
}
