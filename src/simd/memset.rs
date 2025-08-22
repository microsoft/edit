// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! `memchr` for arbitrary sizes (1/2/4/8 bytes).
//!
//! Clang calls the C `memset` function only for byte-sized types (or 0 fills).
//! We however need to fill other types as well. For that, clang generates
//! SIMD loops under higher optimization levels. With `-Os` however, it only
//! generates a trivial loop which is too slow for our needs.
//!
//! This implementation uses SWAR to only have a single implementation for all
//! 4 sizes: By duplicating smaller types into a larger `u64` register we can
//! treat all sizes as if they were `u64`. The only thing we need to take care
//! of is the tail end of the array, which needs to write 0-7 additional bytes.

use std::mem;

/// A marker trait for types that are safe to `memset`.
///
/// # Safety
///
/// Just like with C's `memset`, bad things happen
/// if you use this with non-trivial types.
pub unsafe trait MemsetSafe: Copy {}

unsafe impl MemsetSafe for u8 {}
unsafe impl MemsetSafe for u16 {}
unsafe impl MemsetSafe for u32 {}
unsafe impl MemsetSafe for u64 {}
unsafe impl MemsetSafe for usize {}

unsafe impl MemsetSafe for i8 {}
unsafe impl MemsetSafe for i16 {}
unsafe impl MemsetSafe for i32 {}
unsafe impl MemsetSafe for i64 {}
unsafe impl MemsetSafe for isize {}

/// Fills a slice with the given value.
#[inline]
pub fn memset<T: MemsetSafe>(dst: &mut [T], val: T) {
    unsafe {
        match mem::size_of::<T>() {
            1 => {
                // LLVM will compile this to a call to `memset`,
                // which hopefully should be better optimized than my code.
                let beg = dst.as_mut_ptr();
                let val = mem::transmute_copy::<_, u8>(&val);
                beg.write_bytes(val, dst.len());
            }
            2 => {
                let beg = dst.as_mut_ptr();
                let end = beg.add(dst.len());
                let val = mem::transmute_copy::<_, u16>(&val);
                memset_raw(beg as *mut u8, end as *mut u8, val as u64 * 0x0001000100010001);
            }
            4 => {
                let beg = dst.as_mut_ptr();
                let end = beg.add(dst.len());
                let val = mem::transmute_copy::<_, u32>(&val);
                memset_raw(beg as *mut u8, end as *mut u8, val as u64 * 0x0000000100000001);
            }
            8 => {
                let beg = dst.as_mut_ptr();
                let end = beg.add(dst.len());
                let val = mem::transmute_copy::<_, u64>(&val);
                memset_raw(beg as *mut u8, end as *mut u8, val);
            }
            _ => unreachable!(),
        }
    }
}

#[inline]
fn memset_raw(beg: *mut u8, end: *mut u8, val: u64) {
    #[cfg(any(target_arch = "x86", target_arch = "x86_64", target_arch = "loongarch64"))]
    return unsafe { MEMSET_DISPATCH(beg, end, val) };

    #[cfg(target_arch = "aarch64")]
    return unsafe { memset_neon(beg, end, val) };

    #[allow(unreachable_code)]
    return unsafe { memset_fallback(beg, end, val) };
}

#[inline(never)]
unsafe fn memset_fallback(mut beg: *mut u8, end: *mut u8, val: u64) {
    unsafe {
        let mut remaining = end.offset_from_unsigned(beg);

        while remaining >= 8 {
            (beg as *mut u64).write_unaligned(val);
            beg = beg.add(8);
            remaining -= 8;
        }

        if remaining >= 4 {
            // 4-7 bytes remaining
            (beg as *mut u32).write_unaligned(val as u32);
            (end.sub(4) as *mut u32).write_unaligned(val as u32);
        } else if remaining >= 2 {
            // 2-3 bytes remaining
            (beg as *mut u16).write_unaligned(val as u16);
            (end.sub(2) as *mut u16).write_unaligned(val as u16);
        } else if remaining >= 1 {
            // 1 byte remaining
            beg.write(val as u8);
        }
    }
}

#[cfg(any(target_arch = "x86", target_arch = "x86_64", target_arch = "loongarch64"))]
static mut MEMSET_DISPATCH: unsafe fn(beg: *mut u8, end: *mut u8, val: u64) = memset_dispatch;

#[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
fn memset_dispatch(beg: *mut u8, end: *mut u8, val: u64) {
    let func = if is_x86_feature_detected!("avx2") { memset_avx2 } else { memset_sse2 };
    unsafe { MEMSET_DISPATCH = func };
    unsafe { func(beg, end, val) }
}

#[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
#[target_feature(enable = "sse2")]
unsafe fn memset_sse2(mut beg: *mut u8, end: *mut u8, val: u64) {
    unsafe {
        #[cfg(target_arch = "x86")]
        use std::arch::x86::*;
        #[cfg(target_arch = "x86_64")]
        use std::arch::x86_64::*;

        let mut remaining = end.offset_from_unsigned(beg);

        if remaining >= 16 {
            let fill = _mm_set1_epi64x(val as i64);

            while remaining >= 32 {
                _mm_storeu_si128(beg as *mut _, fill);
                _mm_storeu_si128(beg.add(16) as *mut _, fill);

                beg = beg.add(32);
                remaining -= 32;
            }

            if remaining >= 16 {
                // 16-31 bytes remaining
                _mm_storeu_si128(beg as *mut _, fill);
                _mm_storeu_si128(end.sub(16) as *mut _, fill);
                return;
            }
        }

        if remaining >= 8 {
            // 8-15 bytes remaining
            (beg as *mut u64).write_unaligned(val);
            (end.sub(8) as *mut u64).write_unaligned(val);
        } else if remaining >= 4 {
            // 4-7 bytes remaining
            (beg as *mut u32).write_unaligned(val as u32);
            (end.sub(4) as *mut u32).write_unaligned(val as u32);
        } else if remaining >= 2 {
            // 2-3 bytes remaining
            (beg as *mut u16).write_unaligned(val as u16);
            (end.sub(2) as *mut u16).write_unaligned(val as u16);
        } else if remaining >= 1 {
            // 1 byte remaining
            beg.write(val as u8);
        }
    }
}

#[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
#[target_feature(enable = "avx2")]
fn memset_avx2(mut beg: *mut u8, end: *mut u8, val: u64) {
    unsafe {
        #[cfg(target_arch = "x86")]
        use std::arch::x86::*;
        #[cfg(target_arch = "x86_64")]
        use std::arch::x86_64::*;
        use std::hint::black_box;

        let mut remaining = end.offset_from_unsigned(beg);

        if remaining >= 128 {
            let fill = _mm256_set1_epi64x(val as i64);

            loop {
                _mm256_storeu_si256(beg as *mut _, fill);
                _mm256_storeu_si256(beg.add(32) as *mut _, fill);
                _mm256_storeu_si256(beg.add(64) as *mut _, fill);
                _mm256_storeu_si256(beg.add(96) as *mut _, fill);

                beg = beg.add(128);
                remaining -= 128;
                if remaining < 128 {
                    break;
                }
            }
        }

        if remaining >= 16 {
            let fill = _mm_set1_epi64x(val as i64);

            loop {
                // LLVM is _very_ eager to unroll loops. In the absence of an unroll attribute, black_box does the job.
                // Note that this must not be applied to the intrinsic parameters, as they're otherwise misoptimized.
                #[allow(clippy::unit_arg)]
                black_box(_mm_storeu_si128(beg as *mut _, fill));

                beg = beg.add(16);
                remaining -= 16;
                if remaining < 16 {
                    break;
                }
            }
        }

        // `remaining` is between 0 and 15 at this point.
        // By overlapping the stores we can write all of them in at most 2 stores. This approach
        // can be seen in various libraries, such as wyhash which uses it for loading data in `wyr3`.
        if remaining >= 8 {
            // 8-15 bytes
            (beg as *mut u64).write_unaligned(val);
            (end.sub(8) as *mut u64).write_unaligned(val);
        } else if remaining >= 4 {
            // 4-7 bytes
            (beg as *mut u32).write_unaligned(val as u32);
            (end.sub(4) as *mut u32).write_unaligned(val as u32);
        } else if remaining >= 2 {
            // 2-3 bytes
            (beg as *mut u16).write_unaligned(val as u16);
            (end.sub(2) as *mut u16).write_unaligned(val as u16);
        } else if remaining >= 1 {
            // 1 byte
            beg.write(val as u8);
        }
    }
}

#[cfg(target_arch = "loongarch64")]
fn memset_dispatch(beg: *mut u8, end: *mut u8, val: u64) {
    use std::arch::is_loongarch_feature_detected;

    let func = if is_loongarch_feature_detected!("lasx") {
        memset_lasx
    } else if is_loongarch_feature_detected!("lsx") {
        memset_lsx
    } else {
        memset_fallback
    };
    unsafe { MEMSET_DISPATCH = func };
    unsafe { func(beg, end, val) }
}

#[cfg(target_arch = "loongarch64")]
#[target_feature(enable = "lasx")]
fn memset_lasx(mut beg: *mut u8, end: *mut u8, val: u64) {
    unsafe {
        use std::arch::loongarch64::*;

        let fill = lasx_xvreplgr2vr_d(val as i64);

        if end.offset_from_unsigned(beg) >= 32 {
            lasx_xvst::<0>(fill, beg as *mut _);
            let off = beg.align_offset(32);
            beg = beg.add(off);
        }

        if end.offset_from_unsigned(beg) >= 128 {
            loop {
                lasx_xvst::<0>(fill, beg as *mut _);
                lasx_xvst::<32>(fill, beg as *mut _);
                lasx_xvst::<64>(fill, beg as *mut _);
                lasx_xvst::<96>(fill, beg as *mut _);

                beg = beg.add(128);
                if end.offset_from_unsigned(beg) < 128 {
                    break;
                }
            }
        }

        if end.offset_from_unsigned(beg) >= 16 {
            let fill = lsx_vreplgr2vr_d(val as i64);

            loop {
                lsx_vst::<0>(fill, beg as *mut _);

                beg = beg.add(16);
                if end.offset_from_unsigned(beg) < 16 {
                    break;
                }
            }
        }

        if end.offset_from_unsigned(beg) >= 8 {
            // 8-15 bytes
            (beg as *mut u64).write_unaligned(val);
            (end.sub(8) as *mut u64).write_unaligned(val);
        } else if end.offset_from_unsigned(beg) >= 4 {
            // 4-7 bytes
            (beg as *mut u32).write_unaligned(val as u32);
            (end.sub(4) as *mut u32).write_unaligned(val as u32);
        } else if end.offset_from_unsigned(beg) >= 2 {
            // 2-3 bytes
            (beg as *mut u16).write_unaligned(val as u16);
            (end.sub(2) as *mut u16).write_unaligned(val as u16);
        } else if end.offset_from_unsigned(beg) >= 1 {
            // 1 byte
            beg.write(val as u8);
        }
    }
}

#[cfg(target_arch = "loongarch64")]
#[target_feature(enable = "lsx")]
unsafe fn memset_lsx(mut beg: *mut u8, end: *mut u8, val: u64) {
    unsafe {
        use std::arch::loongarch64::*;

        if end.offset_from_unsigned(beg) >= 16 {
            let fill = lsx_vreplgr2vr_d(val as i64);

            lsx_vst::<0>(fill, beg as *mut _);
            let off = beg.align_offset(16);
            beg = beg.add(off);

            while end.offset_from_unsigned(beg) >= 32 {
                lsx_vst::<0>(fill, beg as *mut _);
                lsx_vst::<16>(fill, beg as *mut _);

                beg = beg.add(32);
            }

            if end.offset_from_unsigned(beg) >= 16 {
                // 16-31 bytes remaining
                lsx_vst::<0>(fill, beg as *mut _);
                lsx_vst::<-16>(fill, end as *mut _);
                return;
            }
        }

        if end.offset_from_unsigned(beg) >= 8 {
            // 8-15 bytes remaining
            (beg as *mut u64).write_unaligned(val);
            (end.sub(8) as *mut u64).write_unaligned(val);
        } else if end.offset_from_unsigned(beg) >= 4 {
            // 4-7 bytes remaining
            (beg as *mut u32).write_unaligned(val as u32);
            (end.sub(4) as *mut u32).write_unaligned(val as u32);
        } else if end.offset_from_unsigned(beg) >= 2 {
            // 2-3 bytes remaining
            (beg as *mut u16).write_unaligned(val as u16);
            (end.sub(2) as *mut u16).write_unaligned(val as u16);
        } else if end.offset_from_unsigned(beg) >= 1 {
            // 1 byte remaining
            beg.write(val as u8);
        }
    }
}

#[cfg(target_arch = "aarch64")]
unsafe fn memset_neon(mut beg: *mut u8, end: *mut u8, val: u64) {
    unsafe {
        use std::arch::aarch64::*;
        let mut remaining = end.offset_from_unsigned(beg);

        if remaining >= 32 {
            let fill = vdupq_n_u64(val);

            loop {
                // Compiles to a single `stp` instruction.
                vst1q_u64(beg as *mut _, fill);
                vst1q_u64(beg.add(16) as *mut _, fill);

                beg = beg.add(32);
                remaining -= 32;
                if remaining < 32 {
                    break;
                }
            }
        }

        if remaining >= 16 {
            // 16-31 bytes remaining
            let fill = vdupq_n_u64(val);
            vst1q_u64(beg as *mut _, fill);
            vst1q_u64(end.sub(16) as *mut _, fill);
        } else if remaining >= 8 {
            // 8-15 bytes remaining
            (beg as *mut u64).write_unaligned(val);
            (end.sub(8) as *mut u64).write_unaligned(val);
        } else if remaining >= 4 {
            // 4-7 bytes remaining
            (beg as *mut u32).write_unaligned(val as u32);
            (end.sub(4) as *mut u32).write_unaligned(val as u32);
        } else if remaining >= 2 {
            // 2-3 bytes remaining
            (beg as *mut u16).write_unaligned(val as u16);
            (end.sub(2) as *mut u16).write_unaligned(val as u16);
        } else if remaining >= 1 {
            // 1 byte remaining
            beg.write(val as u8);
        }
    }
}

#[cfg(test)]
mod tests {
    use std::fmt;
    use std::ops::Not;

    use super::*;

    fn check_memset<T>(val: T, len: usize)
    where
        T: MemsetSafe + Not<Output = T> + PartialEq + fmt::Debug,
    {
        let mut buf = vec![!val; len];
        memset(&mut buf, val);
        assert!(buf.iter().all(|&x| x == val));
    }

    #[test]
    fn test_memset_empty() {
        check_memset(0u8, 0);
        check_memset(0u16, 0);
        check_memset(0u32, 0);
        check_memset(0u64, 0);
    }

    #[test]
    fn test_memset_single() {
        check_memset(0u8, 1);
        check_memset(0xFFu8, 1);
        check_memset(0xABu16, 1);
        check_memset(0x12345678u32, 1);
        check_memset(0xDEADBEEFu64, 1);
    }

    #[test]
    fn test_memset_small() {
        for &len in &[2, 3, 4, 5, 7, 8, 9] {
            check_memset(0xAAu8, len);
            check_memset(0xBEEFu16, len);
            check_memset(0xCAFEBABEu32, len);
            check_memset(0x1234567890ABCDEFu64, len);
        }
    }

    #[test]
    fn test_memset_large() {
        check_memset(0u8, 1000);
        check_memset(0xFFu8, 1024);
        check_memset(0xBEEFu16, 512);
        check_memset(0xCAFEBABEu32, 256);
        check_memset(0x1234567890ABCDEFu64, 128);
    }

    #[test]
    fn test_memset_various_values() {
        check_memset(0u8, 17);
        check_memset(0x7Fu8, 17);
        check_memset(0x8001u16, 17);
        check_memset(0xFFFFFFFFu32, 17);
        check_memset(0x8000000000000001u64, 17);
    }

    #[test]
    fn test_memset_signed_types() {
        check_memset(-1i8, 8);
        check_memset(-2i16, 8);
        check_memset(-3i32, 8);
        check_memset(-4i64, 8);
        check_memset(-5isize, 8);
    }

    #[test]
    fn test_memset_usize_isize() {
        check_memset(0usize, 4);
        check_memset(usize::MAX, 4);
        check_memset(0isize, 4);
        check_memset(isize::MIN, 4);
    }

    #[test]
    fn test_memset_alignment() {
        // Check that memset works for slices not aligned to 8 bytes
        let mut buf = [0u8; 15];
        for offset in 0..8 {
            let slice = &mut buf[offset..(offset + 7)];
            memset(slice, 0x5A);
            assert!(slice.iter().all(|&x| x == 0x5A));
        }
    }
}
