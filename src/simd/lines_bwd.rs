// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::ptr;

use crate::helpers::CoordType;

/// Starting from the `offset` in `haystack` with a current line index of
/// `line`, this seeks backwards to the `line_stop`-nth line and returns the
/// new offset and the line index at that point.
///
/// Note that this function differs from `lines_fwd` in that it
/// seeks backwards even if the `line` is already at `line_stop`.
/// This allows you to ensure (or test) whether `offset` is at a line start.
///
/// It returns an offset *past* a newline and thus at the start of a line.
pub fn lines_bwd(
    haystack: &[u8],
    offset: usize,
    line: CoordType,
    line_stop: CoordType,
) -> (usize, CoordType) {
    unsafe {
        let beg = haystack.as_ptr();
        let it = beg.add(offset.min(haystack.len()));
        let (it, line) = lines_bwd_raw(beg, it, line, line_stop);
        (it.offset_from_unsigned(beg), line)
    }
}

unsafe fn lines_bwd_raw(
    beg: *const u8,
    end: *const u8,
    line: CoordType,
    line_stop: CoordType,
) -> (*const u8, CoordType) {
    #[cfg(any(target_arch = "x86_64", target_arch = "loongarch64"))]
    return unsafe { LINES_BWD_DISPATCH(beg, end, line, line_stop) };

    #[cfg(target_arch = "aarch64")]
    return unsafe { lines_bwd_neon(beg, end, line, line_stop) };

    #[allow(unreachable_code)]
    return unsafe { lines_bwd_fallback(beg, end, line, line_stop) };
}

unsafe fn lines_bwd_fallback(
    beg: *const u8,
    mut end: *const u8,
    mut line: CoordType,
    line_stop: CoordType,
) -> (*const u8, CoordType) {
    unsafe {
        while !ptr::eq(end, beg) {
            let n = end.sub(1);
            if *n == b'\n' {
                if line <= line_stop {
                    break;
                }
                line -= 1;
            }
            end = n;
        }
        (end, line)
    }
}

#[cfg(any(target_arch = "x86_64", target_arch = "loongarch64"))]
static mut LINES_BWD_DISPATCH: unsafe fn(
    beg: *const u8,
    end: *const u8,
    line: CoordType,
    line_stop: CoordType,
) -> (*const u8, CoordType) = lines_bwd_dispatch;

#[cfg(target_arch = "x86_64")]
unsafe fn lines_bwd_dispatch(
    beg: *const u8,
    end: *const u8,
    line: CoordType,
    line_stop: CoordType,
) -> (*const u8, CoordType) {
    let func = if is_x86_feature_detected!("avx2") { lines_bwd_avx2 } else { lines_bwd_fallback };
    unsafe { LINES_BWD_DISPATCH = func };
    unsafe { func(beg, end, line, line_stop) }
}

#[cfg(target_arch = "x86_64")]
#[target_feature(enable = "avx2")]
unsafe fn lines_bwd_avx2(
    beg: *const u8,
    mut end: *const u8,
    mut line: CoordType,
    line_stop: CoordType,
) -> (*const u8, CoordType) {
    unsafe {
        use std::arch::x86_64::*;

        #[inline(always)]
        unsafe fn horizontal_sum_i64(v: __m256i) -> i64 {
            unsafe {
                let hi = _mm256_extracti128_si256::<1>(v);
                let lo = _mm256_castsi256_si128(v);
                let sum = _mm_add_epi64(lo, hi);
                let shuf = _mm_shuffle_epi32::<0b11_10_11_10>(sum);
                let sum = _mm_add_epi64(sum, shuf);
                _mm_cvtsi128_si64(sum)
            }
        }

        let lf = _mm256_set1_epi8(b'\n' as i8);
        let line_stop = line_stop.min(line);
        let off = end.addr() & 31;
        if off != 0 && off < end.offset_from_unsigned(beg) {
            (end, line) = lines_bwd_fallback(end.sub(off), end, line, line_stop);
        }

        while end.offset_from_unsigned(beg) >= 128 {
            let chunk_start = end.sub(128);

            let v1 = _mm256_loadu_si256(chunk_start.add(0) as *const _);
            let v2 = _mm256_loadu_si256(chunk_start.add(32) as *const _);
            let v3 = _mm256_loadu_si256(chunk_start.add(64) as *const _);
            let v4 = _mm256_loadu_si256(chunk_start.add(96) as *const _);

            let mut sum = _mm256_setzero_si256();
            sum = _mm256_sub_epi8(sum, _mm256_cmpeq_epi8(v1, lf));
            sum = _mm256_sub_epi8(sum, _mm256_cmpeq_epi8(v2, lf));
            sum = _mm256_sub_epi8(sum, _mm256_cmpeq_epi8(v3, lf));
            sum = _mm256_sub_epi8(sum, _mm256_cmpeq_epi8(v4, lf));

            let sum = _mm256_sad_epu8(sum, _mm256_setzero_si256());
            let sum = horizontal_sum_i64(sum);

            let line_next = line - sum as CoordType;
            if line_next <= line_stop {
                break;
            }

            end = chunk_start;
            line = line_next;
        }

        while end.offset_from_unsigned(beg) >= 32 {
            let chunk_start = end.sub(32);
            let v = _mm256_loadu_si256(chunk_start as *const _);
            let c = _mm256_cmpeq_epi8(v, lf);

            let ones = _mm256_and_si256(c, _mm256_set1_epi8(0x01));
            let sum = _mm256_sad_epu8(ones, _mm256_setzero_si256());
            let sum = horizontal_sum_i64(sum);

            let line_next = line - sum as CoordType;
            if line_next <= line_stop {
                break;
            }

            end = chunk_start;
            line = line_next;
        }

        lines_bwd_fallback(beg, end, line, line_stop)
    }
}

#[cfg(target_arch = "loongarch64")]
unsafe fn lines_bwd_dispatch(
    beg: *const u8,
    end: *const u8,
    line: CoordType,
    line_stop: CoordType,
) -> (*const u8, CoordType) {
    use std::arch::is_loongarch_feature_detected;

    let func = if is_loongarch_feature_detected!("lasx") {
        lines_bwd_lasx
    } else if is_loongarch_feature_detected!("lsx") {
        lines_bwd_lsx
    } else {
        lines_bwd_fallback
    };
    unsafe { LINES_BWD_DISPATCH = func };
    unsafe { func(beg, end, line, line_stop) }
}

#[cfg(target_arch = "loongarch64")]
#[target_feature(enable = "lasx")]
unsafe fn lines_bwd_lasx(
    beg: *const u8,
    mut end: *const u8,
    mut line: CoordType,
    line_stop: CoordType,
) -> (*const u8, CoordType) {
    unsafe {
        use std::arch::loongarch64::*;
        use std::mem::transmute as T;

        #[inline(always)]
        unsafe fn horizontal_sum(sum: v32i8) -> u32 {
            unsafe {
                let sum = lasx_xvhaddw_h_b(sum, sum);
                let sum = lasx_xvhaddw_w_h(sum, sum);
                let sum = lasx_xvhaddw_d_w(sum, sum);
                let sum = lasx_xvhaddw_q_d(sum, sum);
                let tmp = lasx_xvpermi_q::<1>(T(sum), T(sum));
                let sum = lasx_xvadd_w(T(sum), T(tmp));
                lasx_xvpickve2gr_wu::<0>(sum)
            }
        }

        let lf = lasx_xvrepli_b(b'\n' as i32);
        let line_stop = line_stop.min(line);
        let off = end.addr() & 31;
        if off != 0 && off < end.offset_from_unsigned(beg) {
            (end, line) = lines_bwd_fallback(end.sub(off), end, line, line_stop);
        }

        while end.offset_from_unsigned(beg) >= 128 {
            let chunk_start = end.sub(128);

            let v1 = lasx_xvld::<0>(chunk_start as *const _);
            let v2 = lasx_xvld::<32>(chunk_start as *const _);
            let v3 = lasx_xvld::<64>(chunk_start as *const _);
            let v4 = lasx_xvld::<96>(chunk_start as *const _);

            let mut sum = lasx_xvrepli_b(0);
            sum = lasx_xvsub_b(sum, lasx_xvseq_b(v1, lf));
            sum = lasx_xvsub_b(sum, lasx_xvseq_b(v2, lf));
            sum = lasx_xvsub_b(sum, lasx_xvseq_b(v3, lf));
            sum = lasx_xvsub_b(sum, lasx_xvseq_b(v4, lf));
            let sum = horizontal_sum(sum);

            let line_next = line - sum as CoordType;
            if line_next <= line_stop {
                break;
            }

            end = chunk_start;
            line = line_next;
        }

        while end.offset_from_unsigned(beg) >= 32 {
            let chunk_start = end.sub(32);
            let v = lasx_xvld::<0>(chunk_start as *const _);
            let c = lasx_xvseq_b(v, lf);

            let ones = lasx_xvand_v(T(c), T(lasx_xvrepli_b(1)));
            let sum = horizontal_sum(T(ones));

            let line_next = line - sum as CoordType;
            if line_next <= line_stop {
                break;
            }

            end = chunk_start;
            line = line_next;
        }

        lines_bwd_fallback(beg, end, line, line_stop)
    }
}

#[cfg(target_arch = "loongarch64")]
#[target_feature(enable = "lsx")]
unsafe fn lines_bwd_lsx(
    beg: *const u8,
    mut end: *const u8,
    mut line: CoordType,
    line_stop: CoordType,
) -> (*const u8, CoordType) {
    unsafe {
        use std::arch::loongarch64::*;
        use std::mem::transmute as T;

        #[inline(always)]
        unsafe fn horizontal_sum(sum: v16i8) -> u32 {
            unsafe {
                let sum = lsx_vhaddw_h_b(sum, sum);
                let sum = lsx_vhaddw_w_h(sum, sum);
                let sum = lsx_vhaddw_d_w(sum, sum);
                let sum = lsx_vhaddw_q_d(sum, sum);
                lsx_vpickve2gr_wu::<0>(T(sum))
            }
        }

        let lf = lsx_vrepli_b(b'\n' as i32);
        let line_stop = line_stop.min(line);
        let off = end.addr() & 15;
        if off != 0 && off < end.offset_from_unsigned(beg) {
            (end, line) = lines_bwd_fallback(end.sub(off), end, line, line_stop);
        }

        while end.offset_from_unsigned(beg) >= 64 {
            let chunk_start = end.sub(64);

            let v1 = lsx_vld::<0>(chunk_start as *const _);
            let v2 = lsx_vld::<16>(chunk_start as *const _);
            let v3 = lsx_vld::<32>(chunk_start as *const _);
            let v4 = lsx_vld::<48>(chunk_start as *const _);

            let mut sum = lsx_vrepli_b(0);
            sum = lsx_vsub_b(sum, lsx_vseq_b(v1, lf));
            sum = lsx_vsub_b(sum, lsx_vseq_b(v2, lf));
            sum = lsx_vsub_b(sum, lsx_vseq_b(v3, lf));
            sum = lsx_vsub_b(sum, lsx_vseq_b(v4, lf));
            let sum = horizontal_sum(sum);

            let line_next = line - sum as CoordType;
            if line_next <= line_stop {
                break;
            }

            end = chunk_start;
            line = line_next;
        }

        while end.offset_from_unsigned(beg) >= 16 {
            let chunk_start = end.sub(16);
            let v = lsx_vld::<0>(chunk_start as *const _);
            let c = lsx_vseq_b(v, lf);

            let ones = lsx_vand_v(T(c), T(lsx_vrepli_b(1)));
            let sum = horizontal_sum(T(ones));

            let line_next = line - sum as CoordType;
            if line_next <= line_stop {
                break;
            }

            end = chunk_start;
            line = line_next;
        }

        lines_bwd_fallback(beg, end, line, line_stop)
    }
}

#[cfg(target_arch = "aarch64")]
unsafe fn lines_bwd_neon(
    beg: *const u8,
    mut end: *const u8,
    mut line: CoordType,
    line_stop: CoordType,
) -> (*const u8, CoordType) {
    unsafe {
        use std::arch::aarch64::*;

        let lf = vdupq_n_u8(b'\n');
        let line_stop = line_stop.min(line);
        let off = end.addr() & 15;
        if off != 0 && off < end.offset_from_unsigned(beg) {
            (end, line) = lines_bwd_fallback(end.sub(off), end, line, line_stop);
        }

        while end.offset_from_unsigned(beg) >= 64 {
            let chunk_start = end.sub(64);

            let v1 = vld1q_u8(chunk_start.add(0));
            let v2 = vld1q_u8(chunk_start.add(16));
            let v3 = vld1q_u8(chunk_start.add(32));
            let v4 = vld1q_u8(chunk_start.add(48));

            let mut sum = vdupq_n_u8(0);
            sum = vsubq_u8(sum, vceqq_u8(v1, lf));
            sum = vsubq_u8(sum, vceqq_u8(v2, lf));
            sum = vsubq_u8(sum, vceqq_u8(v3, lf));
            sum = vsubq_u8(sum, vceqq_u8(v4, lf));

            let sum = vaddvq_u8(sum);

            let line_next = line - sum as CoordType;
            if line_next <= line_stop {
                break;
            }

            end = chunk_start;
            line = line_next;
        }

        while end.offset_from_unsigned(beg) >= 16 {
            let chunk_start = end.sub(16);
            let v = vld1q_u8(chunk_start);
            let c = vceqq_u8(v, lf);
            let c = vandq_u8(c, vdupq_n_u8(0x01));
            let sum = vaddvq_u8(c);

            let line_next = line - sum as CoordType;
            if line_next <= line_stop {
                break;
            }

            end = chunk_start;
            line = line_next;
        }

        lines_bwd_fallback(beg, end, line, line_stop)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::helpers::CoordType;
    use crate::simd::test::*;

    #[test]
    fn pseudo_fuzz() {
        let text = generate_random_text(1024);
        let lines = count_lines(&text);
        let mut offset_rng = make_rng();
        let mut line_rng = make_rng();
        let mut line_distance_rng = make_rng();

        for _ in 0..1000 {
            let offset = offset_rng() % (text.len() + 1);
            let line_stop = line_distance_rng() % (lines + 1);
            let line = line_stop + line_rng() % 100;

            let line = line as CoordType;
            let line_stop = line_stop as CoordType;

            let expected = reference_lines_bwd(text.as_bytes(), offset, line, line_stop);
            let actual = lines_bwd(text.as_bytes(), offset, line, line_stop);

            assert_eq!(expected, actual);
        }
    }

    fn reference_lines_bwd(
        haystack: &[u8],
        mut offset: usize,
        mut line: CoordType,
        line_stop: CoordType,
    ) -> (usize, CoordType) {
        if line >= line_stop {
            while offset > 0 {
                let c = haystack[offset - 1];
                if c == b'\n' {
                    if line == line_stop {
                        break;
                    }
                    line -= 1;
                }
                offset -= 1;
            }
        }
        (offset, line)
    }
    #[test]
    fn seeks_to_start() {
        for i in 6..=11 {
            let (off, line) = lines_bwd(b"Hello\nWorld\n", i, 123, 456);
            assert_eq!(off, 6); // After "Hello\n"
            assert_eq!(line, 123); // Still on the same line
        }
    }
}
