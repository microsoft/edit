use crate::arena::ArenaString;

const ENCODE_TABLE: [u8; 64] = *b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

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
                let in0 = inp.add(0).read() as usize;
                let in1 = inp.add(1).read() as usize;
                let in2 = inp.add(2).read() as usize;

                *out.add(0) = ENCODE_TABLE[in0 >> 2];
                *out.add(1) = ENCODE_TABLE[(in0 << 4 | in1 >> 4) & 0x3f];
                *out.add(2) = ENCODE_TABLE[(in1 << 2 | in2 >> 6) & 0x3f];
                *out.add(3) = ENCODE_TABLE[in2 & 0x3f];

                inp = inp.add(3);
                out = out.add(4);
                remaining -= 3;
            }

            *out.add(3) = b'=';
            *out.add(2) = b'=';

            let mut in1 = 0;
            let mut in2 = 0;
            if remaining >= 3 {
                in2 = inp.add(2).read() as usize;
                *out.add(3) = ENCODE_TABLE[in2 & 0x3f];
            }
            if remaining >= 2 {
                in1 = inp.add(1).read() as usize;
                *out.add(2) = ENCODE_TABLE[(in1 << 2 | in2 >> 6) & 0x3f];
            }
            let in0 = inp.add(0).read() as usize;
            *out.add(1) = ENCODE_TABLE[(in0 << 4 | in1 >> 4) & 0x3f];
            *out.add(0) = ENCODE_TABLE[in0 >> 2];
        }

        dst.set_len(dst.len() + out_len);
    }
}

#[cfg(test)]
mod tests {
    use super::encode;
    use crate::arena::Arena;

    #[test]
    fn test_basic() {
        let arena = Arena::new(4 * 1024).unwrap();
        let enc = |s: &[u8]| {
            let mut dst = arena.new_string();
            encode(&mut dst, s);
            dst
        };

        assert_eq!(enc(b""), "");
        assert_eq!(enc(b"f"), "Zg==");
        assert_eq!(enc(b"fo"), "Zm8=");
        assert_eq!(enc(b"foo"), "Zm9v");
        assert_eq!(enc(b"foob"), "Zm9vYg==");
        assert_eq!(enc(b"fooba"), "Zm9vYmE=");
        assert_eq!(enc(b"foobar"), "Zm9vYmFy");
    }
}
