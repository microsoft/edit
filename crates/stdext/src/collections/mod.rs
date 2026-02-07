mod borrowed_string;
mod borrowed_vec;

use std::ptr::NonNull;

pub use borrowed_string::{BorrowedString, BorrowedStringFormatter};
pub use borrowed_vec::BorrowedVec;

/// # Safety
///
/// It's a realloc function. It's unsafe.
pub unsafe trait Allocator {
    fn realloc(
        &self,
        old_ptr: NonNull<u8>,
        old_size: usize,
        new_size: usize,
        align: usize,
    ) -> NonNull<[u8]>;
}
