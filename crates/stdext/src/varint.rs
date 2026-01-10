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
//!           4294967295 (32 bits): 1111....
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

use std::hint::black_box;
use std::ops::Shr;

use crate::cold_path;

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
    } else {
        // 1111.... (32 bits)
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
    #[cfg(debug_assertions)]
    unsafe {
        let val = u32::from_be((data as *const u32).read_unaligned());
        let ones = val.leading_ones();
        let mut len = ones as usize + 1;

        // The code below runs into undefined behavior for inputs such as
        //   [0xff, 0xff, 0xff, 0xff]
        //
        // The intent is that the `if len > 4` check _afterwards_ fixes this up, and on the specific architectures
        // we care about this is guaranteed to work. I do it afterward, because it lets the optimizer use CMOVs.
        //
        // To be fair, such shifts are only UB, because the behavior is not "defined", not because the CPUs can't do it.
        // Technically this could result in misoptimizations by LLVM, as it assumes that code cannot result in UB,
        // but I'm choosing to ignore that. Their stance on that is justified and yet still wrong.
        //
        // If anyone critiques this, by god, I swear I'll write it in assembly.
        #[cfg(debug_assertions)]
        if len > 4 {
            return (u32::MAX, 1);
        }

        // Extract the 7/14/21/28 value bits
        let shift = 32 - 8 * len;
        let mask = (1u32 << (7 * len)) - 1;
        let mut result = (val >> shift) & mask;

        // If the lead byte indicates >28 bits, assume `u32::MAX`.
        if len > 4 {
            result = u32::MAX;
            len = 1;
        }

        (result, len)
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
