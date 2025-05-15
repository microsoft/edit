use crate::arena::ArenaString;

const CHARSET: [u8; 64] = *b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

pub fn encode(dst: &mut ArenaString, src: &[u8]) {
    unsafe {
        let mut inp = src.as_ptr();
        let mut remaining = src.len();
        let dst = dst.as_mut_vec();
        let out_len = src.len().div_ceil(3) * 4;
        dst.reserve(out_len);

        let mut out = dst.as_mut_ptr().add(dst.len());

        if remaining != 0 {
            while remaining > 3 {
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
}
