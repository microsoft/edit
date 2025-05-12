use std::{hint, iter};

#[derive(Clone, Copy)]
pub struct Utf8Chars<'a> {
    source: &'a [u8],
    offset: usize,
}

impl<'a> Utf8Chars<'a> {
    pub fn new(source: &'a [u8], offset: usize) -> Self {
        Self { source, offset }
    }

    pub fn source(&self) -> &'a [u8] {
        self.source
    }

    pub fn offset(&self) -> usize {
        self.offset
    }

    pub fn is_empty(&self) -> bool {
        self.source.is_empty()
    }

    pub fn len(&self) -> usize {
        self.source.len()
    }

    pub fn seek(&mut self, offset: usize) {
        self.offset = offset;
    }

    pub fn has_next(&self) -> bool {
        self.offset < self.source.len()
    }

    // I found that on mixed 50/50 English/Non-English text,
    // performance actually suffers when this gets inlined.
    #[cold]
    fn next_slow(&mut self, c: u8) -> char {
        // See: https://datatracker.ietf.org/doc/html/rfc3629
        // as well as ICU's `utf8.h` for the bitmask approach.

        if self.offset >= self.source.len() {
            return Self::fffd();
        }

        let mut cp = c as u32;

        if cp < 0xE0 {
            // UTF8-2 = %xC2-DF UTF8-tail

            if cp < 0xC2 {
                return Self::fffd();
            }

            // The lead byte is 110xxxxx
            // -> Strip off the 110 prefix
            cp &= !0xE0;
        } else if cp < 0xF0 {
            // UTF8-3 =
            //   %xE0    %xA0-BF   UTF8-tail
            //   %xE1-EC UTF8-tail UTF8-tail
            //   %xED    %x80-9F   UTF8-tail
            //   %xEE-EF UTF8-tail UTF8-tail

            // This is a pretty neat approach seen in ICU4C, because it's a 1:1 translation of the RFC.
            // I don't understand why others don't do the same thing. It's rather performant.
            const BITS_80_9F: u8 = 1 << 0b100; // 0x80-9F, aka 0b100xxxxx
            const BITS_A0_BF: u8 = 1 << 0b101; // 0xA0-BF, aka 0b101xxxxx
            const BITS_BOTH: u8 = BITS_80_9F | BITS_A0_BF;
            const LEAD_TRAIL1_BITS: [u8; 16] = [
                //             v-- lead byte
                BITS_A0_BF, // 0xE0
                BITS_BOTH,  // 0xE1
                BITS_BOTH,  // 0xE2
                BITS_BOTH,  // 0xE3
                BITS_BOTH,  // 0xE4
                BITS_BOTH,  // 0xE5
                BITS_BOTH,  // 0xE6
                BITS_BOTH,  // 0xE7
                BITS_BOTH,  // 0xE8
                BITS_BOTH,  // 0xE9
                BITS_BOTH,  // 0xEA
                BITS_BOTH,  // 0xEB
                BITS_BOTH,  // 0xEC
                BITS_80_9F, // 0xED
                BITS_BOTH,  // 0xEE
                BITS_BOTH,  // 0xEF
            ];

            // The lead byte is 1110xxxx
            // -> Strip off the 1110 prefix
            cp &= !0xF0;

            let t = self.source[self.offset] as u32;
            if LEAD_TRAIL1_BITS[cp as usize] & (1 << (t >> 5)) == 0 {
                return Self::fffd();
            }
            cp = (cp << 6) | (t & 0x3F);

            self.offset += 1;
            if self.offset >= self.source.len() {
                return Self::fffd();
            }
        } else {
            // UTF8-4 =
            //   %xF0    %x90-BF   UTF8-tail UTF8-tail
            //   %xF1-F3 UTF8-tail UTF8-tail UTF8-tail
            //   %xF4    %x80-8F   UTF8-tail UTF8-tail

            // This is similar to the above, but with the indices flipped:
            // The trail byte is the index and the lead byte mask is the value.
            // This is because the split at 0x90 requires more bits than fit into an u8.
            const TRAIL1_LEAD_BITS: [u8; 16] = [
                // +------ 0xF4 lead
                // |+----- 0xF3 lead
                // ||+---- 0xF2 lead
                // |||+--- 0xF1 lead
                // ||||+-- 0xF0 lead
                // vvvvv
                0b_00000, //
                0b_00000, //
                0b_00000, //
                0b_00000, //
                0b_00000, //
                0b_00000, //
                0b_00000, // trail bytes:
                0b_00000, //
                0b_11110, // 0x80-8F -> 0x80-8F can be preceded by 0xF1-F4
                0b_01111, // 0x90-9F -v
                0b_01111, // 0xA0-AF -> 0x90-BF can be preceded by 0xF0-F3
                0b_01111, // 0xB0-BF -^
                0b_00000, //
                0b_00000, //
                0b_00000, //
                0b_00000, //
            ];

            // The lead byte *may* be 11110xxx, but could also be e.g. 11111xxx.
            // -> Only strip off the 1111 prefix
            cp &= !0xF0;

            // Now we can verify if it's actually <= 0xF4.
            if cp > 4 {
                return Self::fffd();
            }

            let t = self.source[self.offset] as u32;
            if TRAIL1_LEAD_BITS[(t >> 4) as usize] & (1 << cp) == 0 {
                return Self::fffd();
            }
            cp = (cp << 6) | (t & 0x3F);

            self.offset += 1;
            if self.offset >= self.source.len() {
                return Self::fffd();
            }

            // UTF8-tail = %x80-BF
            let t = (self.source[self.offset] as u32).wrapping_sub(0x80);
            if t > 0x3F {
                return Self::fffd();
            }
            cp = (cp << 6) | t;

            self.offset += 1;
            if self.offset >= self.source.len() {
                return Self::fffd();
            }
        }

        // SAFETY: All branches above check for `if self.offset >= self.source.len()`
        // one way or another. This is here because the compiler doesn't get it otherwise.
        unsafe { hint::assert_unchecked(self.offset < self.source.len()) };

        // UTF8-tail = %x80-BF
        let t = (self.source[self.offset] as u32).wrapping_sub(0x80);
        if t > 0x3F {
            return Self::fffd();
        }
        cp = (cp << 6) | t;

        self.offset += 1;

        // SAFETY: If `cp` wasn't a valid codepoint, we already returned U+FFFD above.
        #[allow(clippy::transmute_int_to_char)]
        unsafe {
            char::from_u32_unchecked(cp)
        }
    }

    // Improves performance by ~5% and reduces code size.
    #[cold]
    #[inline(always)]
    fn fffd() -> char {
        '\u{FFFD}'
    }
}

impl Iterator for Utf8Chars<'_> {
    type Item = char;

    // At opt-level="s", this function doesn't get inlined,
    // but performance greatly suffers in that case.
    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if self.offset >= self.source.len() {
            return None;
        }

        let c = self.source[self.offset];
        self.offset += 1;

        // Fast-passing ASCII allows this function to be trivially inlined everywhere,
        // as the full decoder is a little too large for that.
        if (c & 0x80) == 0 {
            // UTF8-1 = %x00-7F
            Some(c as char)
        } else {
            // Weirdly enough, adding a hint here to assert that `next_slow`
            // only returns codepoints >= 0x80 makes `ucd` ~5% slower.
            Some(self.next_slow(c))
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        // Lower bound: All remaining bytes are 4-byte sequences.
        // Upper bound: All remaining bytes are ASCII.
        let remaining = self.source.len() - self.offset;
        (remaining / 4, Some(remaining))
    }
}

impl iter::FusedIterator for Utf8Chars<'_> {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_broken_utf8() {
        let source = [b'a', 0xED, 0xA0, 0x80, b'b'];
        let mut chars = Utf8Chars::new(&source, 0);
        let mut offset = 0;
        for chunk in source.utf8_chunks() {
            for ch in chunk.valid().chars() {
                offset += ch.len_utf8();
                assert_eq!(chars.next(), Some(ch));
                assert_eq!(chars.offset(), offset);
            }
            if !chunk.invalid().is_empty() {
                offset += chunk.invalid().len();
                assert_eq!(chars.next(), Some('\u{FFFD}'));
                assert_eq!(chars.offset(), offset);
            }
        }
    }
}
