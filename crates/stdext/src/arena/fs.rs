use std::fs::File;
use std::io::{self, Read};
use std::mem::MaybeUninit;
use std::path::Path;
use std::slice::from_raw_parts_mut;

use crate::arena::Arena;
use crate::collections::{BString, BVec};

pub fn read_to_vec<P: AsRef<Path>>(arena: &'_ Arena, path: P) -> io::Result<BVec<'_, u8>> {
    fn inner<'a>(arena: &'a Arena, path: &Path) -> io::Result<BVec<'a, u8>> {
        let mut file = File::open(path)?;
        let mut vec = BVec::empty();

        const MIN_SIZE: usize = 1024;
        const MAX_SIZE: usize = 128 * 1024;
        let mut buf_size = MIN_SIZE;

        loop {
            vec.reserve(arena, buf_size);
            let spare = vec.spare_capacity_mut();
            let to_read = spare.len().min(buf_size);

            match file_read_uninit(&mut file, &mut spare[..to_read]) {
                Ok(0) => break,
                Ok(n) => {
                    unsafe { vec.set_len(vec.len() + n) };
                    buf_size = (buf_size * 2).min(MAX_SIZE);
                }
                Err(e) if e.kind() == io::ErrorKind::Interrupted => {}
                Err(e) => return Err(e),
            }
        }

        Ok(vec)
    }
    inner(arena, path.as_ref())
}

pub fn read_to_string<P: AsRef<Path>>(arena: &Arena, path: P) -> io::Result<BString<'_>> {
    fn inner<'a>(arena: &'a Arena, path: &Path) -> io::Result<BString<'a>> {
        let vec = read_to_vec(arena, path)?;
        BString::from_utf8(vec).map_err(|_| {
            io::Error::new(io::ErrorKind::InvalidData, "stream did not contain valid UTF-8")
        })
    }
    inner(arena, path.as_ref())
}

fn file_read_uninit<T: Read>(file: &mut T, buf: &mut [MaybeUninit<u8>]) -> io::Result<usize> {
    unsafe {
        let buf_slice = from_raw_parts_mut(buf.as_mut_ptr() as *mut u8, buf.len());
        let n = file.read(buf_slice)?;
        Ok(n)
    }
}
