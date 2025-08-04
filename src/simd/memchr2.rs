// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! `memchr`, but with two needles.

use std::ptr;

/// `memchr`, but with two needles.
///
/// Returns the index of the first occurrence of either needle in the
/// `haystack`. If no needle is found, `haystack.len()` is returned.
/// `offset` specifies the index to start searching from.
pub fn memchr2(needle1: u8, needle2: u8, haystack: &[u8], offset: usize) -> usize {
    unsafe {
        let beg = haystack.as_ptr();
        let end = beg.add(haystack.len());
        let it = beg.add(offset.min(haystack.len()));
        let it = memchr2_raw(needle1, needle2, it, end);
        it.offset_from_unsigned(beg)
    }
}

unsafe fn memchr2_raw(needle1: u8, needle2: u8, beg: *const u8, end: *const u8) -> *const u8 {
    #[cfg(any(target_arch = "x86", target_arch = "x86_64", target_arch = "loongarch64"))]
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

// In order to make `memchr2_raw` slim and fast, we use a function pointer that updates
// itself to the correct implementation on the first call. This reduces binary size.
// It would also reduce branches if we had >2 implementations (a jump still needs to be predicted).
// NOTE that this ONLY works if Control Flow Guard is disabled on Windows.
#[cfg(any(target_arch = "x86", target_arch = "x86_64", target_arch = "loongarch64"))]
static mut MEMCHR2_DISPATCH: unsafe fn(
    needle1: u8,
    needle2: u8,
    beg: *const u8,
    end: *const u8,
) -> *const u8 = memchr2_dispatch;

#[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
unsafe fn memchr2_dispatch(needle1: u8, needle2: u8, beg: *const u8, end: *const u8) -> *const u8 {
    let func = if is_x86_feature_detected!("avx2") { memchr2_avx2 } else { memchr2_fallback };
    unsafe { MEMCHR2_DISPATCH = func };
    unsafe { func(needle1, needle2, beg, end) }
}

// FWIW, I found that adding support for AVX512 was not useful at the time,
// as it only marginally improved file load performance by <5%.
#[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
#[target_feature(enable = "avx2")]
unsafe fn memchr2_avx2(needle1: u8, needle2: u8, mut beg: *const u8, end: *const u8) -> *const u8 {
    unsafe {
        #[cfg(target_arch = "x86")]
        use std::arch::x86::*;
        #[cfg(target_arch = "x86_64")]
        use std::arch::x86_64::*;

        let n1 = _mm256_set1_epi8(needle1 as i8);
        let n2 = _mm256_set1_epi8(needle2 as i8);
        let mut remaining = end.offset_from_unsigned(beg);

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
    }
}

#[cfg(target_arch = "loongarch64")]
unsafe fn memchr2_dispatch(needle1: u8, needle2: u8, beg: *const u8, end: *const u8) -> *const u8 {
    use std::arch::is_loongarch_feature_detected;

    let func = if is_loongarch_feature_detected!("lasx") {
        memchr2_lasx
    } else if is_loongarch_feature_detected!("lsx") {
        memchr2_lsx
    } else {
        memchr2_fallback
    };
    unsafe { MEMCHR2_DISPATCH = func };
    unsafe { func(needle1, needle2, beg, end) }
}

#[cfg(target_arch = "loongarch64")]
#[target_feature(enable = "lasx")]
unsafe fn memchr2_lasx(needle1: u8, needle2: u8, mut beg: *const u8, end: *const u8) -> *const u8 {
    unsafe {
        use std::arch::loongarch64::*;
        use std::mem::transmute as T;

        let n1 = lasx_xvreplgr2vr_b(needle1 as i32);
        let n2 = lasx_xvreplgr2vr_b(needle2 as i32);

        let off = beg.align_offset(32);
        if off != 0 && off < end.offset_from_unsigned(beg) {
            beg = memchr2_lsx(needle1, needle2, beg, beg.add(off));
        }

        while end.offset_from_unsigned(beg) >= 32 {
            let v = lasx_xvld::<0>(beg as *const _);
            let a = lasx_xvseq_b(v, n1);
            let b = lasx_xvseq_b(v, n2);
            let c = lasx_xvor_v(T(a), T(b));
            let m = lasx_xvmskltz_b(T(c));
            let l = lasx_xvpickve2gr_wu::<0>(T(m));
            let h = lasx_xvpickve2gr_wu::<4>(T(m));
            let m = (h << 16) | l;

            if m != 0 {
                return beg.add(m.trailing_zeros() as usize);
            }

            beg = beg.add(32);
        }

        memchr2_fallback(needle1, needle2, beg, end)
    }
}

#[cfg(target_arch = "loongarch64")]
#[target_feature(enable = "lsx")]
unsafe fn memchr2_lsx(needle1: u8, needle2: u8, mut beg: *const u8, end: *const u8) -> *const u8 {
    unsafe {
        use std::arch::loongarch64::*;
        use std::mem::transmute as T;

        let n1 = lsx_vreplgr2vr_b(needle1 as i32);
        let n2 = lsx_vreplgr2vr_b(needle2 as i32);

        let off = beg.align_offset(16);
        if off != 0 && off < end.offset_from_unsigned(beg) {
            beg = memchr2_fallback(needle1, needle2, beg, beg.add(off));
        }

        while end.offset_from_unsigned(beg) >= 16 {
            let v = lsx_vld::<0>(beg as *const _);
            let a = lsx_vseq_b(v, n1);
            let b = lsx_vseq_b(v, n2);
            let c = lsx_vor_v(T(a), T(b));
            let m = lsx_vmskltz_b(T(c));
            let m = lsx_vpickve2gr_wu::<0>(T(m));

            if m != 0 {
                return beg.add(m.trailing_zeros() as usize);
            }

            beg = beg.add(16);
        }

        memchr2_fallback(needle1, needle2, beg, end)
    }
}

#[cfg(target_arch = "aarch64")]
unsafe fn memchr2_neon(needle1: u8, needle2: u8, mut beg: *const u8, end: *const u8) -> *const u8 {
    unsafe {
        use std::arch::aarch64::*;

        if end.offset_from_unsigned(beg) >= 16 {
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
                if end.offset_from_unsigned(beg) < 16 {
                    break;
                }
            }
        }

        memchr2_fallback(needle1, needle2, beg, end)
    }
}

#[cfg(test)]
mod tests {
    use std::slice;

    use super::*;
    use crate::sys;

    #[test]
    fn test_empty() {
        assert_eq!(memchr2(b'a', b'b', b"", 0), 0);
    }

    #[test]
    fn test_basic() {
        let haystack = b"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
        let haystack = &haystack[..43];

        assert_eq!(memchr2(b'a', b'z', haystack, 0), 0);
        assert_eq!(memchr2(b'p', b'q', haystack, 0), 15);
        assert_eq!(memchr2(b'Q', b'Z', haystack, 0), 42);
        assert_eq!(memchr2(b'0', b'9', haystack, 0), haystack.len());
    }

    // Test that it doesn't match before/after the start offset respectively.
    #[test]
    fn test_with_offset() {
        let haystack = b"abcdefghabcdefghabcdefghabcdefghabcdefgh";

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
            const PAGE_SIZE: usize = 64 * 1024; // 64 KiB to cover many architectures.

            // 3 pages: uncommitted, committed, uncommitted
            let ptr = sys::virtual_reserve(PAGE_SIZE * 3).unwrap();
            sys::virtual_commit(ptr.add(PAGE_SIZE), PAGE_SIZE).unwrap();
            slice::from_raw_parts_mut(ptr.add(PAGE_SIZE).as_ptr(), PAGE_SIZE)
        };

        page.fill(b'a');

        // Test if it seeks beyond the page boundary.
        assert_eq!(memchr2(b'\0', b'\0', &page[page.len() - 40..], 0), 40);
        // Test if it seeks before the page boundary for the masked/partial load.
        assert_eq!(memchr2(b'\0', b'\0', &page[..10], 0), 10);
    }
}
