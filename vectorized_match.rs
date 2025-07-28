#[inline(always)]
unsafe fn vectorized_match(
    input: &[u8; 16],
    bitmap_0_7: &[u8; 16],
    bitmap_8_15: &[u8; 16],
    bitmask_lookup: &[u8; 16],
) -> __m128i {
    let input = _mm_loadu_si128(input.as_ptr() as *const __m128i);
    let bitmap_0_7 = _mm_loadu_si128(bitmap_0_7.as_ptr() as *const __m128i);
    let bitmap_8_15 = _mm_loadu_si128(bitmap_8_15.as_ptr() as *const __m128i);
    let bitmask_lookup = _mm_loadu_si128(bitmask_lookup.as_ptr() as *const __m128i);
    let higher_nibbles = _mm_and_si128(_mm_srli_epi16(input, 4), _mm_set1_epi8(0x0f));
    let indices_0_7 = _mm_and_si128(input, _mm_set1_epi8(0x8f));
    let msb = _mm_and_si128(input, _mm_set1_epi8(0x80));
    let indices_8_15 = _mm_xor_si128(indices_0_7, msb);
    let row_0_7 = _mm_shuffle_epi8(bitmap_0_7, indices_0_7);
    let row_8_15 = _mm_shuffle_epi8(bitmap_8_15, indices_8_15);
    let bitmask = _mm_shuffle_epi8(bitmask_lookup, higher_nibbles);
    let bitsets = _mm_or_si128(row_0_7, row_8_15);
    let tmp = _mm_and_si128(bitsets, bitmask);
    let result = _mm_cmpeq_epi8(tmp, bitmask);
    result
}
