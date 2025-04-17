mod memchr2;
mod memrchr2;
mod memset;

pub use memchr2::*;
pub use memrchr2::*;
pub use memset::*;

// Can be replaced with `sub_ptr` once it's stabilized.
#[inline(always)]
unsafe fn distance<T>(hi: *const T, lo: *const T) -> usize {
    unsafe { usize::try_from(hi.offset_from(lo)).unwrap_unchecked() }
}
