// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! Variable-length `u32` encoding and decoding, with efficient storage of `u32::MAX`.
//! `u32::MAX` is a common value in Microsoft Edit's syntax highlighter bytecode.
//!
//! # Format
//!
//! ```text
//!       0-127        ( 7 bits): xxxxxxx0
//!     128-16383      (14 bits): xxxxxx01 yyyyyyyx
//!   16384-2097151    (21 bits): xxxxx011 yyyyyyxx zzzzzzzy
//! 2097152-268435455  (28 bits): xxxx0111 yyyyyxxx zzzzzzyy wwwwwwwz
//!         4294967295 (32 bits): ....1111
//! ```
//!
//! The least significant bits indicate the length, in a format identical to UTF-8. The remaining bits store
//! the value, in little-endian order. Little endian was chosen, as most architectures today use that.
//!
//! On x86, `tzcnt` (= `trailing_ones()` = what we need) has the benefit that its encoding is identical to `rep bsf`.
//! Older CPUs without BMI1 will ignore the `rep` prefix and use `bsf`, while modern CPUs will use the faster `tzcnt`.
//! So not just can we drop the need for `bswap` on x86, but we also speed up the bit count calculation.
//! This makes this encoding faster than LEB128, Google Varint, and others.

pub fn encode(val: u32) -> Vec<u8> {
    let mut result = Vec::with_capacity(5);
    let shift = match val {
        0..0x80 => 0,
        0x80..0x4000 => 1,
        0x4000..0x200000 => 2,
        0x200000..0x10000000 => 3,
        _ => {
            result.push(0xff);
            return result;
        }
    };
    let marker = (1u32 << shift) - 1;
    let encoded = (val << (shift + 1)) | marker;
    let bytes = encoded.to_le_bytes();
    result.extend_from_slice(&bytes[..=shift]);
    result
}

/// # Safety
///
/// The caller must ensure that `data..data+4` is valid memory.
/// It doesn't need to be a valid value, but it must be readable.
pub unsafe fn decode(data: *const u8) -> (u32, usize) {
    // For inputs such as:
    //   [0xff, 0xff, 0xff, 0xff]
    // the shifts below will shift by more than 31 digits, which Rust considers undefined behavior.
    // *We explicitly want UB here*.
    //
    // If we write an if condition here (like this one), LLVM will turn that into a proper branch. Since our inputs
    // are relatively random, that branch will mispredict, hurting performance. The if condition at the end
    // gets turned into conditional moves (good!), but that only works because it comes after the shifts.
    // Unfortunately, there's no way to ask Rust for "platform-defined behavior" (`unchecked_shl/shr` is not it).
    #[cfg(debug_assertions)]
    unsafe {
        if (*data & 0x0f) == 0x0f {
            return (u32::MAX, 1);
        }
    }

    unsafe {
        // Read the following 4 bytes in a single u32 load. We need to swap to big-endian to move the lead
        // 0/10/110/1110/1111 bits to the MSB. This then allows us to do a single, quick `leading_ones` call.
        let val = u32::from_le((data as *const u32).read_unaligned());
        let ones = val.trailing_ones();

        let mut len = ones as usize + 1;
        let mut res = 'bextr: {
            // Give LLVM a helping hand for x86 CPUs with BMI1. It's not smart enough to figure out that `bextr` can
            // be used here. To be fair, it's not faster, so maybe that's why. It is _a lot_ more compact, however.
            #[cfg(target_feature = "bmi1")]
            break 'bextr std::arch::x86_64::_bextr_u32(val, len as u32, (7 * len) as u32);

            // This is where you'd put more architecture-specific optimizations.
            // In fact this is where I'd put my ARM optimizations, but it doesn't have anything like `bextr`. :(

            let mut res = val;
            // Shift out the bytes we read but don't need.
            res <<= 32 - 8 * len;
            // Shift back down and remove the trailing 0/10/110/1110/1111 length bits.
            res >>= 32 - 7 * len;
            break 'bextr res;
        };

        // If the lead byte indicates >28 bits, assume `u32::MAX`.
        // This doubles as a simple form of error correction.
        if len > 4 {
            res = u32::MAX;
            len = 1;
        }

        (res, len)
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
            123,
            127, // Max 1 byte
            128, // Min 2 bytes
            1234,
            16383,     // Max 2 bytes
            16384,     // Min 3 bytes
            2097151,   // Max 3 bytes
            2097152,   // Min 4 bytes
            268435455, // Max 4 bytes
            u32::MAX,  // Special case
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
            assert_eq!((0, 1), decode([0, 0xbb, 0xcc, 0xdd].as_ptr()));
            assert_eq!((123, 1), decode([0xf6, 0xbb, 0xcc, 0xdd].as_ptr()));
            assert_eq!((1234, 2), decode([0x49, 0x13, 0xcc, 0xdd].as_ptr()));
            assert_eq!((u32::MAX, 1), decode([0xff, 0xbb, 0xcc, 0xdd].as_ptr()));
        }
    }
}
