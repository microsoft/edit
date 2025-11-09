// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! Base64 facilities.

use crate::arena::ArenaString;

const CHARSET: [u8; 64] = *b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

fn decode_value(byte: u8) -> Option<u8> {
    match byte {
        b'A'..=b'Z' => Some(byte - b'A'),
        b'a'..=b'z' => Some(byte - b'a' + 26),
        b'0'..=b'9' => Some(byte - b'0' + 52),
        b'+' => Some(62),
        b'/' => Some(63),
        b'=' => Some(64),
        b'\r' | b'\n' => None,
        _ => Some(0xff),
    }
}

/// One aspect of base64 is that the encoded length can be
/// calculated accurately in advance, which is what this returns.
#[inline]
pub fn encode_len(src_len: usize) -> usize {
    src_len.div_ceil(3) * 4
}

/// Encodes the given bytes as base64 and appends them to the destination string.
pub fn encode(dst: &mut ArenaString, src: &[u8]) {
    unsafe {
        let mut inp = src.as_ptr();
        let mut remaining = src.len();
        let dst = dst.as_mut_vec();

        let out_len = encode_len(src.len());
        // ... we can then use this fact to reserve space all at once.
        dst.reserve(out_len);

        // SAFETY: Getting a pointer to the reserved space is only safe
        // *after* calling `reserve()` as it may change the pointer.
        let mut out = dst.as_mut_ptr().add(dst.len());

        if remaining != 0 {
            // Translate chunks of 3 source bytes into 4 base64-encoded bytes.
            while remaining > 3 {
                // SAFETY: Thanks to `remaining > 3`, reading 4 bytes at once is safe.
                // This improves performance massively over a byte-by-byte approach,
                // because it allows us to byte-swap the read and use simple bit-shifts below.
                let val = u32::from_be((inp as *const u32).read_unaligned());
                inp = inp.add(3);
                remaining -= 3;

                *out = CHARSET[(val >> 26) as usize];
                out = out.add(1);
                *out = CHARSET[(val >> 20) as usize & 0x3f];
                out = out.add(1);
                *out = CHARSET[(val >> 14) as usize & 0x3f];
                out = out.add(1);
                *out = CHARSET[(val >> 8) as usize & 0x3f];
                out = out.add(1);
            }

            // Convert the remaining 1-3 bytes.
            let mut in1 = 0;
            let mut in2 = 0;

            // We can simplify the following logic by assuming that there's only 1
            // byte left. If there's >1 byte left, these two '=' will be overwritten.
            *out.add(3) = b'=';
            *out.add(2) = b'=';

            if remaining >= 3 {
                in2 = inp.add(2).read() as usize;
                *out.add(3) = CHARSET[in2 & 0x3f];
            }

            if remaining >= 2 {
                in1 = inp.add(1).read() as usize;
                *out.add(2) = CHARSET[(in1 << 2 | in2 >> 6) & 0x3f];
            }

            let in0 = inp.add(0).read() as usize;
            *out.add(1) = CHARSET[(in0 << 4 | in1 >> 4) & 0x3f];
            *out.add(0) = CHARSET[in0 >> 2];
        }

        dst.set_len(dst.len() + out_len);
    }
}

/// Decodes a base64 string into raw bytes.
pub fn decode(src: &str) -> Option<Vec<u8>> {
    let mut chunk = [0u8; 4];
    let mut chunk_len = 0;
    let mut out = Vec::with_capacity(src.len().saturating_sub(3) / 4 * 3);

    for &byte in src.as_bytes() {
        let Some(val) = decode_value(byte) else {
            continue;
        };
        if val == 0xff {
            return None;
        }
        chunk[chunk_len] = val;
        chunk_len += 1;

        if chunk_len == 4 {
            if chunk[0] == 64 || chunk[1] == 64 {
                return None;
            }

            out.push((chunk[0] << 2) | (chunk[1] >> 4));

            match chunk[2] {
                64 => {
                    if chunk[3] != 64 {
                        return None;
                    }
                }
                val => {
                    out.push((chunk[1] << 4) | (val >> 2));
                }
            }

            if let (Some(c), Some(d)) =
                ((chunk[2] != 64).then_some(chunk[2]), (chunk[3] != 64).then_some(chunk[3]))
            {
                out.push((c << 6) | d);
            } else if chunk[3] != 64 {
                return None;
            }

            if chunk[2] == 64 && chunk[3] != 64 {
                return None;
            }

            chunk_len = 0;
        }
    }

    if chunk_len != 0 {
        return None;
    }

    Some(out)
}

#[cfg(test)]
mod tests {
    use super::encode;
    use crate::arena::{Arena, ArenaString};

    #[test]
    fn test_basic() {
        let arena = Arena::new(4 * 1024).unwrap();
        let enc = |s: &[u8]| {
            let mut dst = ArenaString::new_in(&arena);
            encode(&mut dst, s);
            dst
        };
        assert_eq!(enc(b""), "");
        assert_eq!(enc(b"a"), "YQ==");
        assert_eq!(enc(b"ab"), "YWI=");
        assert_eq!(enc(b"abc"), "YWJj");
        assert_eq!(enc(b"abcd"), "YWJjZA==");
        assert_eq!(enc(b"abcde"), "YWJjZGU=");
        assert_eq!(enc(b"abcdef"), "YWJjZGVm");
        assert_eq!(enc(b"abcdefg"), "YWJjZGVmZw==");
        assert_eq!(enc(b"abcdefgh"), "YWJjZGVmZ2g=");
        assert_eq!(enc(b"abcdefghi"), "YWJjZGVmZ2hp");
        assert_eq!(enc(b"abcdefghij"), "YWJjZGVmZ2hpag==");
        assert_eq!(enc(b"abcdefghijk"), "YWJjZGVmZ2hpams=");
        assert_eq!(enc(b"abcdefghijkl"), "YWJjZGVmZ2hpamts");
        assert_eq!(enc(b"abcdefghijklm"), "YWJjZGVmZ2hpamtsbQ==");
        assert_eq!(enc(b"abcdefghijklmN"), "YWJjZGVmZ2hpamtsbU4=");
        assert_eq!(enc(b"abcdefghijklmNO"), "YWJjZGVmZ2hpamtsbU5P");
        assert_eq!(enc(b"abcdefghijklmNOP"), "YWJjZGVmZ2hpamtsbU5PUA==");
        assert_eq!(enc(b"abcdefghijklmNOPQ"), "YWJjZGVmZ2hpamtsbU5PUFE=");
        assert_eq!(enc(b"abcdefghijklmNOPQR"), "YWJjZGVmZ2hpamtsbU5PUFFS");
        assert_eq!(enc(b"abcdefghijklmNOPQRS"), "YWJjZGVmZ2hpamtsbU5PUFFSUw==");
        assert_eq!(enc(b"abcdefghijklmNOPQRST"), "YWJjZGVmZ2hpamtsbU5PUFFSU1Q=");
        assert_eq!(enc(b"abcdefghijklmNOPQRSTU"), "YWJjZGVmZ2hpamtsbU5PUFFSU1RV");
        assert_eq!(enc(b"abcdefghijklmNOPQRSTUV"), "YWJjZGVmZ2hpamtsbU5PUFFSU1RVVg==");
        assert_eq!(enc(b"abcdefghijklmNOPQRSTUVW"), "YWJjZGVmZ2hpamtsbU5PUFFSU1RVVlc=");
        assert_eq!(enc(b"abcdefghijklmNOPQRSTUVWX"), "YWJjZGVmZ2hpamtsbU5PUFFSU1RVVldY");
        assert_eq!(enc(b"abcdefghijklmNOPQRSTUVWXY"), "YWJjZGVmZ2hpamtsbU5PUFFSU1RVVldYWQ==");
        assert_eq!(enc(b"abcdefghijklmNOPQRSTUVWXYZ"), "YWJjZGVmZ2hpamtsbU5PUFFSU1RVVldYWVo=");
    }

    #[test]
    fn roundtrip_decode() {
        let arena = Arena::new(4 * 1024).unwrap();
        let mut dst = ArenaString::new_in(&arena);
        encode(&mut dst, b"hello clipboard");
        let decoded = super::decode(&dst).unwrap();
        assert_eq!(decoded, b"hello clipboard");
    }
}
