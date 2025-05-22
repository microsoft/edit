// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! Provides fast, non-cryptographic hash functions.

/// The venerable wyhash hash function.
///
/// It's fast, has good statistical properties, and is in the public domain.
/// See: <https://github.com/wangyi-fudan/wyhash>
/// If you visit the link, you'll find that it was superseded by "rapidhash",
/// but that's not particularly interesting for this project. rapidhash results
/// in way larger assembly and isn't faster when hashing small amounts of data.
pub fn hash(mut seed: u64, mut data: &[u8]) -> u64 {
    const S0: u64 = 0xa0761d6478bd642f;
    const S1: u64 = 0xe7037ed1a0b428db;
    const S2: u64 = 0x8ebc6af09c88c6e3;
    const S3: u64 = 0x589965cc75374cc3;

    let len = data.len();
    let a;
    let b;

    seed ^= S0;

    if len <= 16 {
        if len >= 4 {
            a = (wyr4(data) << 32) | wyr4(&data[(len >> 3) << 2..]);
            b = (wyr4(&data[len - 4..]) << 32) | wyr4(&data[len - 4 - ((len >> 3) << 2)..]);
        } else if len > 0 {
            a = wyr3(data, len);
            b = 0;
        } else {
            a = 0;
            b = 0;
        }
    } else {
        let mut back_16 = data;

        if data.len() > 48 {
            let mut seed1 = seed;
            let mut seed2 = seed;
            while {
                seed = wymix(wyr8(data) ^ S1, wyr8(&data[8..]) ^ seed);
                seed1 = wymix(wyr8(&data[16..]) ^ S2, wyr8(&data[24..]) ^ seed1);
                seed2 = wymix(wyr8(&data[32..]) ^ S3, wyr8(&data[40..]) ^ seed2);
                back_16 = &data[32..];
                data = &data[48..];
                data.len() > 48
            } {}
            seed ^= seed1 ^ seed2;
        }
        while data.len() > 16 {
            seed = wymix(wyr8(data) ^ S1, wyr8(&data[8..]) ^ seed);
            back_16 = data;
            data = &data[16..];
        }
        a = wyr8(&back_16[data.len()..]);
        b = wyr8(&back_16[data.len() + 8..]);
    }

    wymix(S1 ^ (len as u64), wymix(a ^ S1, b ^ seed))
}

fn wyr3(p: &[u8], k: usize) -> u64 {
    let p0 = p[0] as u64;
    let p1 = p[k >> 1] as u64;
    let p2 = p[k - 1] as u64;
    (p0 << 16) | (p1 << 8) | p2
}

fn wyr4(p: &[u8]) -> u64 {
    let mut buf = [0; 4];
    buf.copy_from_slice(&p[..4]);
    u32::from_ne_bytes(buf) as u64
}

fn wyr8(p: &[u8]) -> u64 {
    let mut buf = [0; 8];
    buf.copy_from_slice(&p[..8]);
    u64::from_ne_bytes(buf)
}

// This is a weak mix function on its own. It may be worth considering
// replacing external uses of this function with a stronger one.
// On the other hand, it's very fast.
pub fn wymix(lhs: u64, rhs: u64) -> u64 {
    let lhs = lhs as u128;
    let rhs = rhs as u128;
    let r = lhs * rhs;
    (r >> 64) as u64 ^ (r as u64)
}

pub fn hash_str(seed: u64, s: &str) -> u64 {
    hash(seed, s.as_bytes())
}
