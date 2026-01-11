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
/// The caller must ensure that `data..data+4` is valid memory.
/// It doesn't need to be a valid value, but it must be readable.
#[inline(never)]
pub unsafe fn decode(data: *const u8) -> (u32, usize) {
    // For inputs such as:
    //   [0xff, 0xff, 0xff, 0xff]
    // the shifts below will shift by more than 31 digits, which Rust considers undefined behavior.
    // *We explicitly want UB here*.
    //
    // If we write an if condition here (like this one), LLVM will turn that into a proper branch. Since our inputs
    // are relatively random, that branch will mispredict, killing performance. The if condition at the end
    // gets turned into conditional moves (good!), but that only works because it comes after the shifts.
    // Unfortunately, there's no way to ask Rust for "platform-defined behavior" (`unchecked_shl/shr` is not it).
    //
    // If anyone critiques this, by god, I swear I'll write it in assembly.
    #[cfg(debug_assertions)]
    unsafe {
        if *data >= 0xf0 {
            return (u32::MAX, 1);
        }
    }

    // NOTE: The "reference" code is in the next `unsafe` block. This is specific to x86 without lzcnt/BMI1.
    #[cfg(all(any(target_arch = "x86", target_arch = "x86_64"), not(target_feature = "bmi1")))]
    unsafe {
        let val = u32::from_be((data as *const u32).read_unaligned());
        let ones = val.leading_ones();

        // On x86, `leading_ones()` will yield:
        //   not edi
        //   mov eax, 63
        //   bsr eax, edi
        //   xor eax, 31
        //
        // (Note that `bsr` returns a bit-index where 0 = LSB, 31 = MSB, but `leading_ones` returns the number
        // of consecutive 1s starting at the MSB. This is why it does, `xor eax, 31` = `eax = 31 - eax`.
        // This is very elegant, because if the bsr fails (input is all 1s), 63 xor 31 will yield the expected 32.)
        //
        // In any case, that's not ideal, because we don't need `eax` to be 32. It can be any indeterminate value.
        // This `assert_unchecked` will trick LLVM into removing the `mov` which boosts performance by ~10% or so.
        // This is only necessary on x86. All other architectures have lzcnt instructions.
        std::hint::assert_unchecked(ones < 16);

        let mut len = ones as usize + 1;
        let mut result = val;
        result <<= len;
        result >>= 32 - 7 * len;

        // Since we used an `assert_unchecked` on x86, we technically can't rely on the `ones` value to be
        // correct anymore. So for x86, we do what the compiler would do anyway: We check the value itself.
        if val >= 0xf0000000 {
            result = u32::MAX;
            len = 1;
        }

        return (result, len);
    }

    #[allow(unreachable_code)]
    unsafe {
        // Read the following 4 bytes in a single u32 load. We need to swap to big-endian to move the lead
        // 0/10/110/1110/1111 bits to the MSB. This then allows us to do a single, quick `leading_ones` call.
        let val = u32::from_be((data as *const u32).read_unaligned());
        let ones = val.leading_ones();

        let mut len = ones as usize + 1;
        let mut result = val;
        // Shift out the leading 0/10/110/1110/1111 length bits.
        result <<= len;
        // Shift back down to get the final value.
        result >>= 32 - 7 * len;

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
            //assert_eq!((0, 1), decode([0, 0xbb, 0xcc, 0xdd].as_ptr()));
            //assert_eq!((123, 1), decode([0x7b, 0xbb, 0xcc, 0xdd].as_ptr()));
            assert_eq!((1234, 2), decode([0x84, 0xD2, 0xcc, 0xdd].as_ptr()));
            //assert_eq!((u32::MAX, 1), decode([0xff, 0xbb, 0xcc, 0xdd].as_ptr()));
        }
    }
}
