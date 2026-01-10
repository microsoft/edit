// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! Variable-length `u32` encoding and decoding, with efficient storage of `u32::MAX`.
//! `u32::MAX` is a common value in Microsoft Edit's syntax highlighter bytecode.
//!
//! # Format
//!
//! ```text
//!         0-127        ( 7 bits): 0xxxxxxx
//!       128-16383      (14 bits): 10xxxxxx xyyyyyyy
//!     16384-2097151    (21 bits): 110xxxxx xxyyyyyy yzzzzzzz
//!   2097152-268435455  (28 bits): 1110xxxx xxxyyyyy yyzzzzzz zwwwwwww
//! 268435456-4294967294 (32 bits): 11110xxx xxxxyyyy yyzzzzzz zwwwwwww vvvvv...
//!           4294967295 (32 bits): 11111...
//! ```
//!
//! This format differs from LEB128 in that it doesn't use continuation bits,
//! but rather packs the length into the first byte, the way UTF8 does.
//! This allows for faster decoding using "count trailing ones" instructions.
//!
//! It also differs from Google Varint in that it doesn't store 32-bit values as
//!   11111111 xxxxxxxx yyyyyyyy zzzzzzzz wwwwwwww
//! for the same reason. It would require branches for decoding.
//!
//! For little endian architectures, this encoding is particularly efficient to decode.

pub fn encode(val: u32) -> Vec<u8> {
    let mut result = Vec::with_capacity(5);

    if val < 128 {
        // 0xxxxxxx (7 bits)
        result.push(val as u8);
    } else if val < 16384 {
        // 10xxxxxx xyyyyyyy (14 bits: 6 + 8)
        result.push(0x80 | ((val >> 8) as u8));
        result.push(val as u8);
    } else if val < 2097152 {
        // 110xxxxx xxyyyyyy yzzzzzzz (21 bits: 5 + 8 + 8)
        result.push(0xC0 | ((val >> 16) as u8));
        result.push((val >> 8) as u8);
        result.push(val as u8);
    } else if val < 268435456 {
        // 1110xxxx xxxyyyyy yyzzzzzz zwwwwwww (28 bits: 4 + 8 + 8 + 8)
        result.push(0xE0 | ((val >> 24) as u8));
        result.push((val >> 16) as u8);
        result.push((val >> 8) as u8);
        result.push(val as u8);
    } else if val != u32::MAX {
        // 1111xxxx yyyyyyyy zzzzzzzz wwwwwwww vvvvvvvv (32 bits: 0 + 32)
        // First byte is just the prefix with no data bits
        result.push(0xF0 | ((val >> 29) as u8));
        result.push((val >> 21) as u8);
        result.push((val >> 13) as u8);
        result.push((val >> 5) as u8);
        result.push((val << 3) as u8);
    } else {
        // Special case for u32::MAX: 11111111
        result.push(0xFF);
    }

    result
}

/// # Safety
///
/// The caller must ensure that `data..data+8` is valid memory.
/// It doesn't need to be a valid value, but it must be readable.
#[inline(never)]
pub unsafe fn decode(data: *const u8) -> (u32, usize) {
    // SAFETY: Caller guarantees data is valid for reads
    unsafe {
        let first = *data;

        // Count leading ones to determine the byte count
        // This uses the leading_ones intrinsic which is very fast on most architectures
        let ones = first.leading_ones() as usize;

        if ones == 0 {
            // 0xxxxxxx: 1 byte, 7 bits
            return (first as u32, 1);
        }

        if ones >= 5 {
            // 11111xxx: Special case for u32::MAX
            return (u32::MAX, 1);
        }

        // Read bytes individually for correctness
        match ones {
            1 => {
                // 10xxxxxx xyyyyyyy: 2 bytes, 14 bits
                let b0 = *data;
                let b1 = *data.add(1);
                let val = (((b0 & 0x3F) as u32) << 8) | (b1 as u32);
                (val, 2)
            }
            2 => {
                // 110xxxxx xxyyyyyy yzzzzzzz: 3 bytes, 21 bits
                let b0 = *data;
                let b1 = *data.add(1);
                let b2 = *data.add(2);
                let val = (((b0 & 0x1F) as u32) << 16) | ((b1 as u32) << 8) | (b2 as u32);
                (val, 3)
            }
            3 => {
                // 1110xxxx xxxyyyyy yyzzzzzz zwwwwwww: 4 bytes, 28 bits
                let b0 = *data;
                let b1 = *data.add(1);
                let b2 = *data.add(2);
                let b3 = *data.add(3);
                let val = (((b0 & 0x0F) as u32) << 24)
                    | ((b1 as u32) << 16)
                    | ((b2 as u32) << 8)
                    | (b3 as u32);
                (val, 4)
            }
            4 => {
                // 11110xxx xxxxyyyy yyzzzzzz zwwwwwww vvvvvvvv: 5 bytes, 32 bits
                let b0 = *data;
                let b1 = *data.add(1);
                let b2 = *data.add(2);
                let b3 = *data.add(3);
                let b4 = *data.add(4);
                let val = (((b0 & 0x0F) as u32) << 29)
                    | ((b1 as u32) << 21)
                    | ((b2 as u32) << 13)
                    | ((b3 as u32) << 5)
                    | ((b4 as u32) >> 3);
                (val, 5)
            }
            _ => unreachable!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_encode_decode_roundtrip() {
        // Test various boundary values
        let test_values = [
            0u32,
            1,
            127, // Max 1 byte
            128, // Min 2 bytes
            1234,
            16383,     // Max 2 bytes
            16384,     // Min 3 bytes
            2097151,   // Max 3 bytes
            2097152,   // Min 4 bytes
            268435455, // Max 4 bytes
            268435456, // Min 5 bytes
            u32::MAX - 1,
            u32::MAX, // Special case
        ];

        for &val in &test_values {
            let encoded = encode(val);
            println!("Value {} encoded as: {:02X?}", val, encoded);
            let (decoded, len) = unsafe { decode(encoded.as_ptr()) };
            println!("  Decoded as: {} with length {}", decoded, len);
            assert_eq!(decoded, val, "Failed roundtrip for value {}", val);
            assert_eq!(len, encoded.len(), "Length mismatch for value {}", val);
        }
    }

    #[test]
    fn test_specific_encodings() {
        // Test specific byte patterns
        unsafe {
            assert_eq!((0, 1), decode([0, 0xff, 0xff, 0xff, 0xff, 0xff].as_ptr()));
            assert_eq!((123, 1), decode([0x7b, 0xff, 0xff, 0xff, 0xff, 0xff].as_ptr()));
            assert_eq!((u32::MAX, 1), decode([0xff, 0xff, 0xff, 0xff, 0xff, 0xff].as_ptr()));

            // 1234 = 0x04D2, should be 2 bytes: 10000100 11010010
            assert_eq!((1234, 2), decode([0x84, 0xD2, 0xff, 0xff, 0xff, 0xff].as_ptr()));
        }
    }
}
