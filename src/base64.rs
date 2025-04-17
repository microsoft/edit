use std::mem::MaybeUninit;

const ENCODE_TABLE: [u8; 64] = *b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

pub fn encode(input: &[u8]) -> String {
    unsafe {
        let mut inp = input.as_ptr();
        let mut remaining = input.len();

        let out_len = input.len().div_ceil(3) * 4;
        let mut buf: Box<[MaybeUninit<u8>]> = Box::new_uninit_slice(out_len);
        let mut out = buf.as_mut_ptr();

        if remaining != 0 {
            while remaining > 3 {
                let in0 = inp.add(0).read() as usize;
                let in1 = inp.add(1).read() as usize;
                let in2 = inp.add(2).read() as usize;

                (*out.add(0)).write(ENCODE_TABLE[in0 >> 2]);
                (*out.add(1)).write(ENCODE_TABLE[(in0 << 4 | in1 >> 4) & 0x3f]);
                (*out.add(2)).write(ENCODE_TABLE[(in1 << 2 | in2 >> 6) & 0x3f]);
                (*out.add(3)).write(ENCODE_TABLE[in2 & 0x3f]);

                inp = inp.add(3);
                out = out.add(4);
                remaining -= 3;
            }

            (*out.add(3)).write(b'=');
            (*out.add(2)).write(b'=');

            let mut in1 = 0;
            let mut in2 = 0;
            if remaining >= 3 {
                in2 = inp.add(2).read() as usize;
                (*out.add(3)).write(ENCODE_TABLE[in2 & 0x3f]);
            }
            if remaining >= 2 {
                in1 = inp.add(1).read() as usize;
                (*out.add(2)).write(ENCODE_TABLE[(in1 << 2 | in2 >> 6) & 0x3f]);
            }
            let in0 = inp.add(0).read() as usize;
            (*out.add(1)).write(ENCODE_TABLE[(in0 << 4 | in1 >> 4) & 0x3f]);
            (*out.add(0)).write(ENCODE_TABLE[in0 >> 2]);
        }

        String::from_utf8_unchecked(buf.assume_init().into_vec())
    }
}

#[cfg(test)]
mod tests {
    use super::encode;

    #[test]
    fn test_basic() {
        assert_eq!(encode(b""), "");
        assert_eq!(encode(b"f"), "Zg==");
        assert_eq!(encode(b"fo"), "Zm8=");
        assert_eq!(encode(b"foo"), "Zm9v");
        assert_eq!(encode(b"foob"), "Zm9vYg==");
        assert_eq!(encode(b"fooba"), "Zm9vYmE=");
        assert_eq!(encode(b"foobar"), "Zm9vYmFy");
    }
}
