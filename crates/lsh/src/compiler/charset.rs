use std::fmt;
use std::ops::RangeInclusive;

use stdext::arena::Arena;

const WORD_BITS: usize = usize::BITS as usize;

pub type SerializedCharset = [u16; 16];

#[derive(Clone, PartialEq, Eq)]
pub struct Charset {
    bits: [usize; 256 / WORD_BITS],
}

impl Charset {
    pub const fn no() -> Self {
        Charset { bits: [usize::MIN; _] }
    }

    pub const fn yes() -> Self {
        Charset { bits: [usize::MAX; _] }
    }

    pub fn invert(&mut self) {
        for b in &mut self.bits {
            *b = !*b;
        }
    }

    pub fn get(&self, b: u8) -> bool {
        let hi = b as usize / WORD_BITS;
        let lo = b as usize % WORD_BITS;
        (self.bits[hi] & (1 << lo)) != 0
    }

    pub fn covers_none(&self) -> bool {
        self.bits.iter().all(|&b| b == usize::MIN)
    }

    pub fn covers_all(&self) -> bool {
        self.bits.iter().all(|&b| b == usize::MAX)
    }

    pub fn covers_range(&self, range: RangeInclusive<u8>) -> bool {
        range.into_iter().all(|b| self.get(b))
    }

    pub const fn set(&mut self, index: u8, value: bool) {
        let hi = index as usize / WORD_BITS;
        let lo = index as usize % WORD_BITS;
        self.bits[hi] = if value { self.bits[hi] | (1 << lo) } else { self.bits[hi] & !(1 << lo) };
    }

    pub const fn set_range(&mut self, range: RangeInclusive<u8>, value: bool) {
        let start = *range.start() as usize;
        let end = *range.end() as usize;

        let start_word = start / WORD_BITS;
        let start_bit = start % WORD_BITS;
        let end_word = end / WORD_BITS;
        let end_bit = end % WORD_BITS;

        // Write the starting bits of the first word
        let mask = usize::MAX << start_bit;
        self.bits[start_word] =
            if value { self.bits[start_word] | mask } else { self.bits[start_word] & !mask };

        // Fill the full words in between, if any
        let mut word = start_word + 1;
        while word <= end_word {
            self.bits[word] = if value { usize::MAX } else { usize::MIN };
            word += 1;
        }

        // Fix the trailing bits of the last word we wrote above
        let shift = end_bit + 1;
        let mask = if shift < WORD_BITS { usize::MAX << shift } else { 0 };
        self.bits[end_word] =
            if value { self.bits[end_word] & !mask } else { self.bits[end_word] | mask };
    }

    pub fn merge(&mut self, other: &Charset) {
        for (a, b) in self.bits.iter_mut().zip(other.bits.iter()) {
            *a |= *b;
        }
    }

    pub fn is_superset_of(&self, other: &Charset) -> bool {
        for (&s, &o) in self.bits.iter().zip(other.bits.iter()) {
            // For self to be a superset, every bit in other must be in self
            if (o & !s) != 0 {
                return false;
            }
        }
        true
    }

    pub fn serialize(&self) -> SerializedCharset {
        let mut arr = [0u16; 16];
        for lo in 0..16 {
            let mut u = 0u16;
            for hi in 0..16 {
                u |= (self.get(hi * 16 + lo) as u16) << hi;
            }
            arr[lo as usize] = u;
        }
        arr
    }
}

impl Default for Charset {
    fn default() -> Self {
        Self::no()
    }
}

impl fmt::Debug for Charset {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let show_char = |f: &mut fmt::Formatter<'_>, b: usize| {
            let b = b as u8;
            if b == b'"' {
                write!(f, "&quot;")
            } else if b.is_ascii_graphic() {
                let b = b as char;
                write!(f, "{b}")
            } else {
                write!(f, "0x{b:02X}")
            }
        };

        let get_bit = |index: usize| -> bool {
            let hi = index / WORD_BITS;
            let lo = index % WORD_BITS;
            (self.bits[hi] & (1 << lo)) != 0
        };

        let mut beg = 0;
        let mut first = true;

        write!(f, "[")?;

        while beg < 256 {
            while beg < 256 && !get_bit(beg) {
                beg += 1;
            }
            if beg >= 256 {
                break;
            }

            let mut end = beg;
            while end < 256 && get_bit(end) {
                end += 1;
            }

            if !first {
                write!(f, ", ")?;
            }
            show_char(f, beg)?;
            if end - beg > 1 {
                write!(f, "-")?;
                show_char(f, end - 1)?;
            }

            beg = end;
            first = false;
        }

        write!(f, "]")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn set_range() {
        for range in [5..=7, 10..=245, 0..=255] {
            let mut cs = Charset::no();
            cs.set_range(range.clone(), true);

            for i in 0u8..=255 {
                assert_eq!(cs.get(i), range.contains(&i), "range {range:?}, bit {i}");
            }
        }
    }
}
