use crate::apperr;
use crate::helpers;
use crate::sys;
use std::alloc::AllocError;
use std::alloc::Allocator;
use std::alloc::Layout;
use std::cell::Cell;
use std::fmt;
use std::mem;
use std::mem::MaybeUninit;
use std::ops::Deref;
use std::ops::DerefMut;
use std::ptr::{self, NonNull};
use std::slice;

const ALLOC_CHUNK_SIZE: usize = 64 * 1024;

pub struct Arena {
    base: NonNull<u8>,
    capacity: usize,
    commit: Cell<usize>,
    offset: Cell<usize>,
}

impl Arena {
    pub const fn empty() -> Self {
        Self {
            base: NonNull::dangling(),
            capacity: 0,
            commit: Cell::new(0),
            offset: Cell::new(0),
        }
    }

    pub fn new(capacity: usize) -> apperr::Result<Arena> {
        let capacity = (capacity + ALLOC_CHUNK_SIZE - 1) & !(ALLOC_CHUNK_SIZE - 1);
        let base = unsafe { sys::virtual_reserve(capacity)? };
        Ok(Arena {
            base,
            capacity,
            commit: Cell::new(0),
            offset: Cell::new(0),
        })
    }

    /// "Deallocates" the memory in the arena down to the given offset.
    ///
    /// # Safety
    ///
    /// Obviously, this is GIGA UNSAFE. It runs no destructors and does not check
    /// whether the offset is valid. You better take care when using this function.
    pub unsafe fn reset(&self, to: usize) {
        if cfg!(debug_assertions) && self.offset.get() > to {
            let commit = self.commit.get();
            let len = (self.offset.get() + 128).min(commit) - to;
            unsafe { slice::from_raw_parts_mut(self.base.add(to).as_ptr(), len).fill(0xDD) };
        }
        self.offset.replace(to);
    }

    fn alloc_raw(&self, bytes: usize, alignment: usize) -> NonNull<u8> {
        let bytes = bytes.max(1);
        let alignment = alignment.max(1);

        let commit = self.commit.get();
        let offset = self.offset.get();

        let beg = (offset + alignment - 1) & !(alignment - 1);
        let end = beg + bytes;

        if end > commit {
            return self.alloc_raw_bump(beg, end);
        }

        if cfg!(debug_assertions) {
            let ptr = unsafe { self.base.add(offset) };
            let len = (end + 128).min(self.commit.get()) - offset;
            unsafe { slice::from_raw_parts_mut(ptr.as_ptr(), len).fill(0xCD) };
        }

        self.offset.replace(end);
        unsafe { self.base.add(beg) }
    }

    // With the code in `alloc_raw_bump()` out of the way, `alloc_raw()` compiles down to some super tight assembly.
    #[cold]
    fn alloc_raw_bump(&self, beg: usize, end: usize) -> NonNull<u8> {
        let offset = self.offset.get();
        let commit_old = self.commit.get();
        let commit_new = (end + ALLOC_CHUNK_SIZE - 1) & !(ALLOC_CHUNK_SIZE - 1);

        if commit_new > self.capacity
            || unsafe {
                sys::virtual_commit(self.base.add(commit_old), commit_new - commit_old).is_err()
            }
        {
            panic!("Out of memory");
        }

        if cfg!(debug_assertions) {
            let ptr = unsafe { self.base.add(offset) };
            let len = (end + 128).min(self.commit.get()) - offset;
            unsafe { slice::from_raw_parts_mut(ptr.as_ptr(), len).fill(0xCD) };
        }

        self.commit.replace(commit_new);
        self.offset.replace(end);
        unsafe { self.base.add(beg) }
    }

    fn alloc_raw_zeroed(&self, bytes: usize, alignment: usize) -> NonNull<u8> {
        let ptr = self.alloc_raw(bytes, alignment);
        unsafe { ptr::write_bytes(ptr.as_ptr(), 0, bytes) };
        ptr
    }

    #[allow(clippy::mut_from_ref)]
    pub fn alloc_uninit<T>(&self) -> &mut MaybeUninit<T> {
        let bytes = mem::size_of::<T>();
        let alignment = mem::align_of::<T>();
        let ptr = self.alloc_raw(bytes, alignment);
        unsafe { ptr.cast().as_mut() }
    }

    #[allow(clippy::mut_from_ref)]
    pub fn alloc_uninit_slice<T>(&self, count: usize) -> &mut [MaybeUninit<T>] {
        let bytes = mem::size_of::<T>() * count;
        let alignment = mem::align_of::<T>();
        let ptr = self.alloc_raw(bytes, alignment);
        unsafe { slice::from_raw_parts_mut(ptr.cast().as_ptr(), count) }
    }

    pub fn alloc_default<T: Default>(&self) -> &mut T {
        self.alloc_uninit::<T>().write(Default::default())
    }

    pub fn new_vec<T>(&self) -> Vec<T, &Arena> {
        Vec::new_in(self)
    }

    pub fn new_string(&self) -> ArenaString<'_> {
        ArenaString::new_in(self)
    }
}

impl Drop for Arena {
    fn drop(&mut self) {
        // TODO: This is kinda dumb.
        if self.base != NonNull::dangling() {
            unsafe { sys::virtual_release(self.base, self.capacity) };
        }
    }
}

impl Default for Arena {
    fn default() -> Self {
        Arena::empty()
    }
}

unsafe impl Allocator for &Arena {
    fn allocate(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        let p = self.alloc_raw(layout.size(), layout.align());
        Ok(NonNull::slice_from_raw_parts(p, layout.size()))
    }

    fn allocate_zeroed(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        let p = self.alloc_raw_zeroed(layout.size(), layout.align());
        Ok(NonNull::slice_from_raw_parts(p, layout.size()))
    }

    // While it is possible to shrink the tail end of the arena, it is
    // not very useful given the existence of scoped scratch arenas.
    unsafe fn deallocate(&self, _: NonNull<u8>, _: Layout) {}

    unsafe fn grow(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<[u8]>, AllocError> {
        debug_assert!(new_layout.size() >= old_layout.size());
        debug_assert!(new_layout.align() <= old_layout.align());

        let new_ptr;

        // Growing the given area is possible if it is at the end of the arena.
        if unsafe { ptr.add(old_layout.size()) == self.base.add(self.offset.get()) } {
            new_ptr = ptr;
            let delta = new_layout.size() - old_layout.size();
            // Assuming that the given ptr/length area is at the end of the arena,
            // we can just push more memory to the end of the arena to grow it.
            _ = self.alloc_raw(delta, 1);
        } else {
            new_ptr = self.allocate(new_layout)?.cast();

            // SAFETY: It's weird to me that this doesn't assert new_layout.size() >= old_layout.size(),
            // but neither does the stdlib code at the time of writing.
            // So, assuming that is not needed, this code is safe since it just copies the old data over.
            unsafe {
                ptr::copy_nonoverlapping(ptr.as_ptr(), new_ptr.as_ptr(), old_layout.size());
                self.deallocate(ptr, old_layout);
            }
        }

        Ok(NonNull::slice_from_raw_parts(new_ptr, new_layout.size()))
    }

    unsafe fn grow_zeroed(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<[u8]>, AllocError> {
        unsafe {
            // SAFETY: Same as grow().
            let ptr = self.grow(ptr, old_layout, new_layout)?;

            // SAFETY: At this point, `ptr` must be valid for `new_layout.size()` bytes,
            // allowing us to safely zero out the delta since `old_layout.size()`.
            ptr.cast::<u8>()
                .add(old_layout.size())
                .write_bytes(0, new_layout.size() - old_layout.size());

            Ok(ptr)
        }
    }

    unsafe fn shrink(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<[u8]>, AllocError> {
        debug_assert!(new_layout.size() <= old_layout.size());
        debug_assert!(new_layout.align() <= old_layout.align());

        let mut len = old_layout.size();

        // Shrinking the given area is possible if it is at the end of the arena.
        if unsafe { ptr.add(len) == self.base.add(self.offset.get()) } {
            self.offset.set(self.offset.get() - len + new_layout.size());
            len = new_layout.size();
        } else {
            debug_assert!(
                false,
                "Did you call shrink_to_fit()? Only the last allocation can be shrunk!"
            );
        }

        Ok(NonNull::slice_from_raw_parts(ptr, len))
    }
}

static mut S_SCRATCH: [Arena; 2] = const { [Arena::empty(), Arena::empty()] };

#[cfg(debug_assertions)]
static mut S_SCRATCH_DEPTH: usize = 0;

// Most methods make just two kinds of allocations:
// * Interior: Temporary data that can be deallocated when the function returns.
// * Exterior: Data that is returned to the caller and must remain alive until the caller stops using it.
//
// Such methods only have two lifetimes, for which you consequently also only need two arenas.
// ...even if your method calls other methods recursively! This is because the exterior allocations
// of a callee are simply interior allocations to the caller, and so on, recursively.
//
// This works as long as the two arenas flip/flop between being used as interior/exterior allocator
// along the callstack. To ensure that is the case, we use a recursion counter in debug builds.
//
// This approach was described among others at: https://nullprogram.com/blog/2023/09/27/
pub struct ScratchArena<'a> {
    arena: &'a Arena,
    offset: usize,

    #[cfg(debug_assertions)]
    depth: usize,
}

impl Drop for ScratchArena<'_> {
    fn drop(&mut self) {
        unsafe {
            #[cfg(debug_assertions)]
            {
                debug_assert!(self.depth == S_SCRATCH_DEPTH);
                S_SCRATCH_DEPTH -= 1;
            }

            self.arena.reset(self.offset);
        }
    }
}

impl<'a> Deref for ScratchArena<'a> {
    type Target = Arena;

    fn deref(&self) -> &'a Self::Target {
        #[cfg(debug_assertions)]
        unsafe {
            debug_assert!(self.depth == S_SCRATCH_DEPTH)
        };

        self.arena
    }
}

pub fn init() -> apperr::Result<()> {
    unsafe {
        for s in &mut S_SCRATCH[..] {
            *s = Arena::new(128 * 1024)?;
        }
    }
    Ok(())
}

/// Returns a new scratch arena for temporary allocations,
/// ensuring it doesn't conflict with the provided arena.
pub fn scratch_arena(conflict: Option<&Arena>) -> ScratchArena {
    let is_first = helpers::opt_ptr_eq(conflict, Some(unsafe { &S_SCRATCH[0] }));
    let arena = unsafe { &mut S_SCRATCH[is_first as usize] };
    let offset = arena.offset.get();

    ScratchArena {
        arena,
        offset,

        #[cfg(debug_assertions)]
        depth: unsafe {
            S_SCRATCH_DEPTH += 1;
            S_SCRATCH_DEPTH
        },
    }
}

#[derive(Clone)]
pub struct ArenaString<'a> {
    vec: Vec<u8, &'a Arena>,
}

impl<'a> ArenaString<'a> {
    #[must_use]
    pub const fn new_in(arena: &'a Arena) -> Self {
        Self {
            vec: Vec::new_in(arena),
        }
    }

    #[inline]
    pub fn from_str(arena: &'a Arena, s: &str) -> Self {
        let mut res = Self::new_in(arena);
        res.push_str(s);
        res
    }

    /// # Safety
    ///
    /// It says "unchecked" right there. What did you expect?
    #[inline]
    #[must_use]
    pub unsafe fn from_utf8_unchecked(bytes: Vec<u8, &'a Arena>) -> Self {
        Self { vec: bytes }
    }

    #[must_use]
    pub fn from_utf8_lossy_owned(v: Vec<u8, &'a Arena>) -> Self {
        let mut res = Self::new_in(v.allocator());

        let mut iter = v.utf8_chunks();
        let Some(mut chunk) = iter.next() else {
            return res;
        };

        let valid = chunk.valid();
        if chunk.invalid().is_empty() {
            debug_assert_eq!(valid.len(), v.len());
            return unsafe { Self::from_utf8_unchecked(v) };
        }

        const REPLACEMENT: &str = "\u{FFFD}";

        res.reserve(v.len());
        res.push_str(chunk.valid());
        res.push_str(REPLACEMENT);

        loop {
            res.push_str(chunk.valid());
            if !chunk.invalid().is_empty() {
                res.push_str(REPLACEMENT);
            }
            chunk = match iter.next() {
                Some(chunk) => chunk,
                None => break,
            };
        }

        res
    }

    pub fn is_empty(&self) -> bool {
        self.vec.is_empty()
    }

    pub fn len(&self) -> usize {
        self.vec.len()
    }

    pub fn capacity(&self) -> usize {
        self.vec.capacity()
    }

    pub fn as_str(&self) -> &str {
        unsafe { str::from_utf8_unchecked(self.vec.as_slice()) }
    }

    pub fn as_mut_str(&mut self) -> &mut str {
        unsafe { str::from_utf8_unchecked_mut(self.vec.as_mut_slice()) }
    }

    pub fn as_bytes(&self) -> &[u8] {
        self.vec.as_slice()
    }

    /// Returns a mutable reference to the contents of this `String`.
    ///
    /// # Safety
    ///
    /// The underlying `&mut Vec` allows writing bytes which are not valid UTF-8.
    pub unsafe fn as_mut_vec(&mut self) -> &mut Vec<u8, &'a Arena> {
        &mut self.vec
    }

    pub fn reserve(&mut self, additional: usize) {
        self.vec.reserve(additional)
    }

    pub fn shrink_to_fit(&mut self) {
        self.vec.shrink_to_fit()
    }

    pub fn clear(&mut self) {
        self.vec.clear()
    }

    pub fn push_str(&mut self, string: &str) {
        self.vec.extend_from_slice(string.as_bytes())
    }

    pub fn push(&mut self, ch: char) {
        match ch.len_utf8() {
            1 => self.vec.push(ch as u8),
            _ => self
                .vec
                .extend_from_slice(ch.encode_utf8(&mut [0; 4]).as_bytes()),
        }
    }

    /// Same as `push(char)` but with a specified number of character copies.
    /// Shockingly absent from the standard library.
    pub fn push_repeat(&mut self, ch: char, total_copies: usize) {
        if total_copies == 0 {
            return;
        }

        let buf = unsafe { self.as_mut_vec() };

        if ch.is_ascii() {
            // Compiles down to `memset()`.
            buf.extend(std::iter::repeat_n(ch as u8, total_copies));
        } else {
            // Implements efficient string padding using quadratic duplication.
            let mut utf8_buf = [0; 4];
            let utf8 = ch.encode_utf8(&mut utf8_buf).as_bytes();
            let initial_len = buf.len();
            let added_len = utf8.len() * total_copies;
            let final_len = initial_len + added_len;

            buf.reserve(added_len);
            buf.extend_from_slice(utf8);

            while buf.len() != final_len {
                let end = (final_len - buf.len() + initial_len).min(buf.len());
                buf.extend_from_within(initial_len..end);
            }
        }
    }
}

impl fmt::Debug for ArenaString<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&**self, f)
    }
}

impl PartialEq<&str> for ArenaString<'_> {
    fn eq(&self, other: &&str) -> bool {
        self.as_str() == *other
    }
}

impl Deref for ArenaString<'_> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}

impl DerefMut for ArenaString<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_mut_str()
    }
}

impl fmt::Display for ArenaString<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

impl fmt::Write for ArenaString<'_> {
    #[inline]
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.push_str(s);
        Ok(())
    }

    #[inline]
    fn write_char(&mut self, c: char) -> fmt::Result {
        self.push(c);
        Ok(())
    }
}

#[macro_export]
macro_rules! arena_format {
    ($arena:expr, $($arg:tt)*) => {{
        use std::fmt::Write;
        let mut output = $arena.new_string();
        output.write_fmt(format_args!($($arg)*)).unwrap();
        output
    }}
}
