// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! Bindings to the ICU library.

use std::cmp::Ordering;
use std::ffi::{CStr, c_char};
use std::mem;
use std::mem::MaybeUninit;
use std::ops::Range;
use std::ptr::{null, null_mut};

use crate::arena::{Arena, ArenaString, scratch_arena};
use crate::buffer::TextBuffer;
use crate::unicode::Utf8Chars;
use crate::{apperr, arena_format, sys};

#[derive(Clone, Copy)]
pub struct Encoding {
    pub label: &'static str,
    pub canonical: &'static str,
}

pub struct Encodings {
    pub preferred: &'static [Encoding],
    pub all: &'static [Encoding],
}

static mut ENCODINGS: Encodings = Encodings { preferred: &[], all: &[] };

/// Returns a list of encodings ICU supports.
pub fn get_available_encodings() -> &'static Encodings {
    // OnceCell for people that want to put it into a static.
    #[allow(static_mut_refs)]
    unsafe {
        if ENCODINGS.all.is_empty() {
            let scratch = scratch_arena(None);
            let mut preferred = Vec::new_in(&*scratch);
            let mut alternative = Vec::new_in(&*scratch);

            // These encodings are always available.
            preferred.push(Encoding { label: "UTF-8", canonical: "UTF-8" });
            preferred.push(Encoding { label: "UTF-8 BOM", canonical: "UTF-8 BOM" });

            if let Ok(f) = init_if_needed() {
                let mut n = 0;
                loop {
                    let name = (f.ucnv_getAvailableName)(n);
                    if name.is_null() {
                        break;
                    }

                    n += 1;

                    let name = CStr::from_ptr(name).to_str().unwrap_unchecked();
                    // We have already pushed UTF-8 above and can skip it.
                    // There is no need to filter UTF-8 BOM here,
                    // since ICU does not distinguish it from UTF-8.
                    if name.is_empty() || name == "UTF-8" {
                        continue;
                    }

                    let mut status = icu_ffi::U_ZERO_ERROR;
                    let mime = (f.ucnv_getStandardName)(
                        name.as_ptr(),
                        c"MIME".as_ptr() as *const _,
                        &mut status,
                    );
                    if !mime.is_null() && status.is_success() {
                        let mime = CStr::from_ptr(mime).to_str().unwrap_unchecked();
                        preferred.push(Encoding { label: mime, canonical: name });
                    } else {
                        alternative.push(Encoding { label: name, canonical: name });
                    }
                }
            }

            let preferred_len = preferred.len();

            // Combine the preferred and alternative encodings into a single list.
            let mut all = Vec::with_capacity(preferred.len() + alternative.len());
            all.extend(preferred);
            all.extend(alternative);

            let all = all.leak();
            ENCODINGS.preferred = &all[..preferred_len];
            ENCODINGS.all = &all[..];
        }

        &ENCODINGS
    }
}

/// Formats the given ICU error code into a human-readable string.
pub fn apperr_format(f: &mut std::fmt::Formatter<'_>, code: u32) -> std::fmt::Result {
    fn format(code: u32) -> &'static str {
        let Ok(f) = init_if_needed() else {
            return "";
        };

        let status = icu_ffi::UErrorCode::new(code);
        let ptr = unsafe { (f.u_errorName)(status) };
        if ptr.is_null() {
            return "";
        }

        let str = unsafe { CStr::from_ptr(ptr) };
        str.to_str().unwrap_or("")
    }

    let msg = format(code);
    if !msg.is_empty() {
        write!(f, "ICU Error: {msg}")
    } else {
        write!(f, "ICU Error: {code:#08x}")
    }
}

/// Converts between two encodings using ICU.
pub struct Converter<'pivot> {
    source: *mut icu_ffi::UConverter,
    target: *mut icu_ffi::UConverter,
    pivot_buffer: &'pivot mut [MaybeUninit<u16>],
    pivot_source: *mut u16,
    pivot_target: *mut u16,
    reset: bool,
}

impl Drop for Converter<'_> {
    fn drop(&mut self) {
        let f = assume_loaded();
        unsafe { (f.ucnv_close)(self.source) };
        unsafe { (f.ucnv_close)(self.target) };
    }
}

impl<'pivot> Converter<'pivot> {
    /// Constructs a new `Converter` instance.
    ///
    /// # Parameters
    ///
    /// * `pivot_buffer`: A buffer used to cache partial conversions.
    ///   Don't make it too small.
    /// * `source_encoding`: The source encoding name (e.g., "UTF-8").
    /// * `target_encoding`: The target encoding name (e.g., "UTF-16").
    pub fn new(
        pivot_buffer: &'pivot mut [MaybeUninit<u16>],
        source_encoding: &str,
        target_encoding: &str,
    ) -> apperr::Result<Self> {
        let f = init_if_needed()?;

        let arena = scratch_arena(None);
        let source_encoding = Self::append_nul(&arena, source_encoding);
        let target_encoding = Self::append_nul(&arena, target_encoding);

        let mut status = icu_ffi::U_ZERO_ERROR;
        let source = unsafe { (f.ucnv_open)(source_encoding.as_ptr(), &mut status) };
        let target = unsafe { (f.ucnv_open)(target_encoding.as_ptr(), &mut status) };
        if status.is_failure() {
            if !source.is_null() {
                unsafe { (f.ucnv_close)(source) };
            }
            if !target.is_null() {
                unsafe { (f.ucnv_close)(target) };
            }
            return Err(status.as_error());
        }

        let pivot_source = pivot_buffer.as_mut_ptr() as *mut u16;
        let pivot_target = unsafe { pivot_source.add(pivot_buffer.len()) };

        Ok(Self { source, target, pivot_buffer, pivot_source, pivot_target, reset: true })
    }

    fn append_nul<'a>(arena: &'a Arena, input: &str) -> ArenaString<'a> {
        arena_format!(arena, "{}\0", input)
    }

    /// Performs one step of the encoding conversion.
    ///
    /// # Parameters
    ///
    /// * `input`: The input buffer to convert from.
    ///   It should be in the `source_encoding` that was previously specified.
    /// * `output`: The output buffer to convert to.
    ///   It should be in the `target_encoding` that was previously specified.
    ///
    /// # Returns
    ///
    /// A tuple containing:
    /// 1. The number of bytes read from the input buffer.
    /// 2. The number of bytes written to the output buffer.
    pub fn convert(
        &mut self,
        input: &[u8],
        output: &mut [MaybeUninit<u8>],
    ) -> apperr::Result<(usize, usize)> {
        let f = assume_loaded();

        let input_beg = input.as_ptr();
        let input_end = unsafe { input_beg.add(input.len()) };
        let mut input_ptr = input_beg;

        let output_beg = output.as_mut_ptr() as *mut u8;
        let output_end = unsafe { output_beg.add(output.len()) };
        let mut output_ptr = output_beg;

        let pivot_beg = self.pivot_buffer.as_mut_ptr() as *mut u16;
        let pivot_end = unsafe { pivot_beg.add(self.pivot_buffer.len()) };

        let flush = input.is_empty();
        let mut status = icu_ffi::U_ZERO_ERROR;

        unsafe {
            (f.ucnv_convertEx)(
                /* target_cnv   */ self.target,
                /* source_cnv   */ self.source,
                /* target       */ &mut output_ptr,
                /* target_limit */ output_end,
                /* source       */ &mut input_ptr,
                /* source_limit */ input_end,
                /* pivot_start  */ pivot_beg,
                /* pivot_source */ &mut self.pivot_source,
                /* pivot_target */ &mut self.pivot_target,
                /* pivot_limit  */ pivot_end,
                /* reset        */ self.reset,
                /* flush        */ flush,
                /* status       */ &mut status,
            );
        }

        self.reset = false;
        if status.is_failure() && status != icu_ffi::U_BUFFER_OVERFLOW_ERROR {
            return Err(status.as_error());
        }

        let input_advance = unsafe { input_ptr.offset_from(input_beg) as usize };
        let output_advance = unsafe { output_ptr.offset_from(output_beg) as usize };
        Ok((input_advance, output_advance))
    }
}

// In benchmarking, I found that the performance does not really change much by changing this value.
// I picked 64 because it seemed like a reasonable lower bound.
const CACHE_SIZE: usize = 64;

/// Caches a chunk of TextBuffer contents (UTF-8) in UTF-16 format.
#[repr(C)]
struct Cache {
    /// The translated text. Contains [`Cache::utf16_len`]-many valid items.
    utf16: [u16; CACHE_SIZE],
    /// For each character in [`Cache::utf16`] this stores the offset in the [`TextBuffer`],
    /// relative to the start offset stored in `native_beg`.
    /// This has the same length as [`Cache::utf16`].
    utf16_to_utf8_offsets: [u16; CACHE_SIZE],
    /// `utf8_to_utf16_offsets[native_offset - native_beg]` will tell you which character in
    /// [`Cache::utf16`] maps to the given `native_offset` in the underlying [`TextBuffer`].
    /// Contains `native_end - native_beg`-many valid items.
    utf8_to_utf16_offsets: [u16; CACHE_SIZE],

    /// The number of valid items in [`Cache::utf16`].
    utf16_len: usize,
    /// Offset of the first non-ASCII character.
    /// Less than or equal to [`Cache::utf16_len`].
    native_indexing_limit: usize,

    /// The range of UTF-8 text in the [`TextBuffer`] that this chunk covers.
    utf8_range: Range<usize>,
}

#[repr(C)]
struct DoubleCache {
    cache: [Cache; 2],
    /// You can consider this a 1 bit index into `cache`.
    mru: bool,
}

/// A wrapper around ICU's `UText` struct.
///
/// In our case its only purpose is to adapt a [`TextBuffer`] for ICU.
///
/// # Safety
///
/// Warning! No lifetime tracking is done here.
/// I initially did it properly with a PhantomData marker for the TextBuffer
/// lifetime, but it was a pain so now I don't. Not a big deal in our case.
pub struct Text(&'static mut icu_ffi::UText);

impl Drop for Text {
    fn drop(&mut self) {
        let f = assume_loaded();
        unsafe { (f.utext_close)(self.0) };
    }
}

impl Text {
    /// Constructs an ICU `UText` instance from a [`TextBuffer`].
    ///
    /// # Safety
    ///
    /// The caller must ensure that the given [`TextBuffer`]
    /// outlives the returned `Text` instance.
    pub unsafe fn new(tb: &TextBuffer) -> apperr::Result<Self> {
        let f = init_if_needed()?;

        let mut status = icu_ffi::U_ZERO_ERROR;
        let ptr =
            unsafe { (f.utext_setup)(null_mut(), size_of::<DoubleCache>() as i32, &mut status) };
        if status.is_failure() {
            return Err(status.as_error());
        }

        const FUNCS: icu_ffi::UTextFuncs = icu_ffi::UTextFuncs {
            table_size: size_of::<icu_ffi::UTextFuncs>() as i32,
            reserved1: 0,
            reserved2: 0,
            reserved3: 0,
            clone: Some(utext_clone),
            native_length: Some(utext_native_length),
            access: Some(utext_access),
            extract: None,
            replace: None,
            copy: None,
            map_offset_to_native: Some(utext_map_offset_to_native),
            map_native_index_to_utf16: Some(utext_map_native_index_to_utf16),
            close: None,
            spare1: None,
            spare2: None,
            spare3: None,
        };

        let ut = unsafe { &mut *ptr };
        ut.p_funcs = &FUNCS;
        ut.context = tb as *const TextBuffer as *mut _;
        ut.a = -1;

        Ok(Self(ut))
    }
}

fn text_buffer_from_utext<'a>(ut: &icu_ffi::UText) -> &'a TextBuffer {
    unsafe { &*(ut.context as *const TextBuffer) }
}

fn double_cache_from_utext<'a>(ut: &icu_ffi::UText) -> &'a mut DoubleCache {
    unsafe { &mut *(ut.p_extra as *mut DoubleCache) }
}

extern "C" fn utext_clone(
    dest: *mut icu_ffi::UText,
    src: &icu_ffi::UText,
    deep: bool,
    status: &mut icu_ffi::UErrorCode,
) -> *mut icu_ffi::UText {
    if status.is_failure() {
        return null_mut();
    }

    if deep {
        *status = icu_ffi::U_UNSUPPORTED_ERROR;
        return null_mut();
    }

    let f = assume_loaded();
    let ut_ptr = unsafe { (f.utext_setup)(dest, size_of::<DoubleCache>() as i32, status) };
    if status.is_failure() {
        return null_mut();
    }

    // TODO: I'm somewhat unsure whether we have to preserve the `chunk_offset`.
    // We can't blindly copy chunk contents and the `Cache` in `ut.p_extra`,
    // because they may contain dirty contents (different `TextBuffer` generation).
    unsafe {
        let ut = &mut *ut_ptr;
        ut.p_funcs = src.p_funcs;
        ut.context = src.context;
        ut.a = -1;
    }

    ut_ptr
}

extern "C" fn utext_native_length(ut: &mut icu_ffi::UText) -> i64 {
    let tb = text_buffer_from_utext(ut);
    tb.text_length() as i64
}

extern "C" fn utext_access(ut: &mut icu_ffi::UText, native_index: i64, forward: bool) -> bool {
    if let Some(cache) = utext_access_impl(ut, native_index, forward) {
        let native_off = native_index as usize - cache.utf8_range.start;
        ut.chunk_contents = cache.utf16.as_ptr();
        ut.chunk_length = cache.utf16_len as i32;
        ut.chunk_offset = cache.utf8_to_utf16_offsets[native_off] as i32;
        ut.chunk_native_start = cache.utf8_range.start as i64;
        ut.chunk_native_limit = cache.utf8_range.end as i64;
        ut.native_indexing_limit = cache.native_indexing_limit as i32;
        true
    } else {
        false
    }
}

fn utext_access_impl<'a>(
    ut: &mut icu_ffi::UText,
    native_index: i64,
    forward: bool,
) -> Option<&'a mut Cache> {
    let tb = text_buffer_from_utext(ut);
    let mut index_contained = native_index;

    if !forward {
        index_contained -= 1;
    }
    if index_contained < 0 || index_contained as usize >= tb.text_length() {
        return None;
    }

    let index_contained = index_contained as usize;
    let native_index = native_index as usize;
    let double_cache = double_cache_from_utext(ut);
    let dirty = ut.a != tb.generation() as i64;

    if dirty {
        // The text buffer contents have changed.
        // Invalidate both caches so that future calls don't mistakenly use them
        // when they enter the for loop in the else branch below (`dirty == false`).
        double_cache.cache[0].utf16_len = 0;
        double_cache.cache[1].utf16_len = 0;
        double_cache.cache[0].utf8_range = 0..0;
        double_cache.cache[1].utf8_range = 0..0;
        ut.a = tb.generation() as i64;
    } else {
        // Check if one of the caches already contains the requested range.
        for (i, cache) in double_cache.cache.iter_mut().enumerate() {
            if cache.utf8_range.contains(&index_contained) {
                double_cache.mru = i != 0;
                return Some(cache);
            }
        }
    }

    // Turn the least recently used cache into the most recently used one.
    let double_cache = double_cache_from_utext(ut);
    double_cache.mru = !double_cache.mru;
    let cache = &mut double_cache.cache[double_cache.mru as usize];

    // In order to safely fit any UTF-8 character into our cache,
    // we must assume the worst case of a 4-byte long encoding.
    const UTF16_LEN_LIMIT: usize = CACHE_SIZE - 4;
    let utf8_len_limit;
    let native_start;

    if forward {
        utf8_len_limit = (tb.text_length() - native_index).min(UTF16_LEN_LIMIT);
        native_start = native_index;
    } else {
        // The worst case ratio for UTF-8 to UTF-16 is 1:1, when the text is ASCII.
        // This allows us to safely subtract the UTF-16 buffer size
        // and assume that whatever we read as UTF-8 will fit.
        // TODO: Test what happens if you have lots of invalid UTF-8 text blow up to U+FFFD.
        utf8_len_limit = native_index.min(UTF16_LEN_LIMIT);

        // Since simply subtracting an offset may end up in the middle of a codepoint sequence,
        // we must align the offset to the next codepoint boundary.
        // Here we skip trail bytes until we find a lead.
        let mut beg = native_index - utf8_len_limit;
        let chunk = tb.read_forward(beg);
        for &c in chunk {
            if c & 0b1100_0000 != 0b1000_0000 {
                break;
            }
            beg += 1;
        }

        native_start = beg;
    }

    // Translate the given range from UTF-8 to UTF-16.
    // NOTE: This code makes the assumption that the `native_index` is always
    // at UTF-8 codepoint boundaries which technically isn't guaranteed.
    let mut utf16_len = 0;
    let mut utf8_len = 0;
    let mut ascii_len = 0;
    'outer: loop {
        let initial_utf8_len = utf8_len;
        let chunk = tb.read_forward(native_start + utf8_len);
        if chunk.is_empty() {
            break;
        }

        let mut it = Utf8Chars::new(chunk, 0);

        // If we've only seen ASCII so far we can fast-pass the UTF-16 translation,
        // because we can just widen from u8 -> u16.
        if utf16_len == ascii_len {
            let haystack = &chunk[..chunk.len().min(utf8_len_limit - ascii_len)];

            // When it comes to performance, and the search space is small (which it is here),
            // it's always a good idea to keep the loops small and tight...
            let len = haystack.iter().position(|&c| c >= 0x80).unwrap_or(haystack.len());

            // ...In this case it allows the compiler to vectorize this loop and double
            // the performance. Luckily, llvm doesn't unroll the loop, which is great,
            // because `len` will always be a relatively small number.
            for &c in &chunk[..len] {
                unsafe {
                    *cache.utf16.get_unchecked_mut(ascii_len) = c as u16;
                    *cache.utf16_to_utf8_offsets.get_unchecked_mut(ascii_len) = ascii_len as u16;
                    *cache.utf8_to_utf16_offsets.get_unchecked_mut(ascii_len) = ascii_len as u16;
                }
                ascii_len += 1;
            }

            utf16_len += len;
            utf8_len += len;
            it.seek(len);
            if ascii_len >= UTF16_LEN_LIMIT {
                break;
            }
        }

        loop {
            let Some(c) = it.next() else {
                break;
            };

            // Thanks to our `if utf16_len >= UTF16_LEN_LIMIT` check,
            // we can safely assume that this will fit.
            unsafe {
                let utf8_len_beg = utf8_len;
                let utf8_len_end = initial_utf8_len + it.offset();

                while utf8_len < utf8_len_end {
                    *cache.utf8_to_utf16_offsets.get_unchecked_mut(utf8_len) = utf16_len as u16;
                    utf8_len += 1;
                }

                if c <= '\u{FFFF}' {
                    *cache.utf16.get_unchecked_mut(utf16_len) = c as u16;
                    *cache.utf16_to_utf8_offsets.get_unchecked_mut(utf16_len) = utf8_len_beg as u16;
                    utf16_len += 1;
                } else {
                    let c = c as u32 - 0x10000;
                    let b = utf8_len_beg as u16;
                    *cache.utf16.get_unchecked_mut(utf16_len) = (c >> 10) as u16 | 0xD800;
                    *cache.utf16.get_unchecked_mut(utf16_len + 1) = (c & 0x3FF) as u16 | 0xDC00;
                    *cache.utf16_to_utf8_offsets.get_unchecked_mut(utf16_len) = b;
                    *cache.utf16_to_utf8_offsets.get_unchecked_mut(utf16_len + 1) = b;
                    utf16_len += 2;
                }
            }

            if utf16_len >= UTF16_LEN_LIMIT || utf8_len >= utf8_len_limit {
                break 'outer;
            }
        }
    }

    // Allow for looking up past-the-end indices via
    // `utext_map_offset_to_native` and `utext_map_native_index_to_utf16`.
    cache.utf16_to_utf8_offsets[utf16_len] = utf8_len as u16;
    cache.utf8_to_utf16_offsets[utf8_len] = utf16_len as u16;

    let native_limit = native_start + utf8_len;
    cache.utf16_len = utf16_len;
    // If parts of the UTF-8 chunk are ASCII, we can tell ICU that it doesn't need to call
    // utext_map_offset_to_native. For some reason, uregex calls that function *a lot*,
    // literally half the CPU time is spent on it.
    cache.native_indexing_limit = ascii_len;
    cache.utf8_range = native_start..native_limit;
    Some(cache)
}

extern "C" fn utext_map_offset_to_native(ut: &icu_ffi::UText) -> i64 {
    debug_assert!((0..=ut.chunk_length).contains(&ut.chunk_offset));

    let double_cache = double_cache_from_utext(ut);
    let cache = &double_cache.cache[double_cache.mru as usize];
    let off_rel = cache.utf16_to_utf8_offsets[ut.chunk_offset as usize];
    let off_abs = cache.utf8_range.start + off_rel as usize;
    off_abs as i64
}

extern "C" fn utext_map_native_index_to_utf16(ut: &icu_ffi::UText, native_index: i64) -> i32 {
    debug_assert!((ut.chunk_native_start..=ut.chunk_native_limit).contains(&native_index));

    let double_cache = double_cache_from_utext(ut);
    let cache = &double_cache.cache[double_cache.mru as usize];
    let off_rel = cache.utf8_to_utf16_offsets[(native_index - ut.chunk_native_start) as usize];
    off_rel as i32
}

/// A wrapper around ICU's `URegularExpression` struct.
///
/// # Safety
///
/// Warning! No lifetime tracking is done here.
pub struct Regex(&'static mut icu_ffi::URegularExpression);

impl Drop for Regex {
    fn drop(&mut self) {
        let f = assume_loaded();
        unsafe { (f.uregex_close)(self.0) };
    }
}

impl Regex {
    /// Enable case-insensitive matching.
    pub const CASE_INSENSITIVE: i32 = icu_ffi::UREGEX_CASE_INSENSITIVE;

    /// If set, ^ and $ match the start and end of each line.
    /// Otherwise, they match the start and end of the entire string.
    pub const MULTILINE: i32 = icu_ffi::UREGEX_MULTILINE;

    /// Treat the given pattern as a literal string.
    pub const LITERAL: i32 = icu_ffi::UREGEX_LITERAL;

    /// Constructs a regex, plain and simple. Read `uregex_open` docs.
    ///
    /// # Safety
    ///
    /// The caller must ensure that the given `Text` outlives the returned `Regex` instance.
    pub unsafe fn new(pattern: &str, flags: i32, text: &Text) -> apperr::Result<Self> {
        let f = init_if_needed()?;
        unsafe {
            let scratch = scratch_arena(None);
            let mut utf16 = Vec::new_in(&*scratch);
            let mut status = icu_ffi::U_ZERO_ERROR;

            utf16.extend(pattern.encode_utf16());

            let ptr = (f.uregex_open)(
                utf16.as_ptr(),
                utf16.len() as i32,
                icu_ffi::UREGEX_MULTILINE | icu_ffi::UREGEX_ERROR_ON_UNKNOWN_ESCAPES | flags,
                None,
                &mut status,
            );
            // ICU describes the time unit as being dependent on CPU performance
            // and "typically [in] the order of milliseconds", but this claim seems
            // highly outdated. On my CPU from 2021, a limit of 4096 equals roughly 600ms.
            (f.uregex_setTimeLimit)(ptr, 4096, &mut status);
            (f.uregex_setUText)(ptr, text.0 as *const _ as *mut _, &mut status);
            if status.is_failure() {
                return Err(status.as_error());
            }

            Ok(Self(&mut *ptr))
        }
    }

    /// Updates the regex pattern with the given text.
    /// If the text contents have changed, you can pass the same text as you used
    /// initially and it'll trigger ICU to reload the text and invalidate its caches.
    ///
    /// # Safety
    ///
    /// The caller must ensure that the given `Text` outlives the `Regex` instance.
    pub unsafe fn set_text(&mut self, text: &mut Text, offset: usize) {
        // Get `utext_access_impl` to detect the `TextBuffer::generation` change,
        // and refresh its contents. This ensures that ICU doesn't reuse
        // stale `UText::chunk_contents`, as it has no way tell that it's stale.
        utext_access(text.0, offset as i64, true);

        let f = assume_loaded();
        let mut status = icu_ffi::U_ZERO_ERROR;
        unsafe { (f.uregex_setUText)(self.0, text.0 as *const _ as *mut _, &mut status) };
        // `uregex_setUText` resets the regex to the start of the text.
        // Because of this, we must also call `uregex_reset64`.
        unsafe { (f.uregex_reset64)(self.0, offset as i64, &mut status) };
    }

    /// Sets the regex to the absolute offset in the underlying text.
    pub fn reset(&mut self, offset: usize) {
        let f = assume_loaded();
        let mut status = icu_ffi::U_ZERO_ERROR;
        unsafe { (f.uregex_reset64)(self.0, offset as i64, &mut status) };
    }

    /// Gets captured group count.
    pub fn group_count(&mut self) -> i32 {
        let f = assume_loaded();

        let mut status = icu_ffi::U_ZERO_ERROR;
        let count = unsafe { (f.uregex_groupCount)(self.0, &mut status) };
        if status.is_failure() { 0 } else { count }
    }

    /// Gets the text range of a captured group by index.
    pub fn group(&mut self, group: i32) -> Option<Range<usize>> {
        let f = assume_loaded();

        let mut status = icu_ffi::U_ZERO_ERROR;
        let start = unsafe { (f.uregex_start64)(self.0, group, &mut status) };
        let end = unsafe { (f.uregex_end64)(self.0, group, &mut status) };
        if status.is_failure() {
            None
        } else {
            let start = start.max(0);
            let end = end.max(start);
            Some(start as usize..end as usize)
        }
    }
}

impl Iterator for Regex {
    type Item = Range<usize>;

    fn next(&mut self) -> Option<Self::Item> {
        let f = assume_loaded();

        let mut status = icu_ffi::U_ZERO_ERROR;
        let ok = unsafe { (f.uregex_findNext)(self.0, &mut status) };
        if !ok {
            return None;
        }

        self.group(0)
    }
}

static mut ROOT_COLLATOR: Option<*mut icu_ffi::UCollator> = None;

/// Compares two UTF-8 strings for sorting using ICU's collation algorithm.
pub fn compare_strings(a: &[u8], b: &[u8]) -> Ordering {
    #[cold]
    fn init() {
        unsafe {
            let mut coll = null_mut();

            if let Ok(f) = init_if_needed() {
                let mut status = icu_ffi::U_ZERO_ERROR;
                coll = (f.ucol_open)(c"".as_ptr(), &mut status);
            }

            ROOT_COLLATOR = Some(coll);
        }
    }

    // OnceCell for people that want to put it into a static.
    #[allow(static_mut_refs)]
    let coll = unsafe {
        if ROOT_COLLATOR.is_none() {
            init();
        }
        ROOT_COLLATOR.unwrap_unchecked()
    };

    if coll.is_null() {
        compare_strings_ascii(a, b)
    } else {
        let f = assume_loaded();
        let mut status = icu_ffi::U_ZERO_ERROR;
        let res = unsafe {
            (f.ucol_strcollUTF8)(
                coll,
                a.as_ptr(),
                a.len() as i32,
                b.as_ptr(),
                b.len() as i32,
                &mut status,
            )
        };

        match res {
            icu_ffi::UCollationResult::UCOL_EQUAL => Ordering::Equal,
            icu_ffi::UCollationResult::UCOL_GREATER => Ordering::Greater,
            icu_ffi::UCollationResult::UCOL_LESS => Ordering::Less,
        }
    }
}

/// Unicode collation via `ucol_strcollUTF8`, now for ASCII!
fn compare_strings_ascii(a: &[u8], b: &[u8]) -> Ordering {
    let mut iter = a.iter().zip(b.iter());

    // Low weight: Find the first character which differs.
    //
    // Remember that result in case all remaining characters are
    // case-insensitive equal, because then we use that as a fallback.
    while let Some((&a, &b)) = iter.next() {
        if a != b {
            let mut order = a.cmp(&b);
            let la = a.to_ascii_lowercase();
            let lb = b.to_ascii_lowercase();

            if la == lb {
                // High weight: Find the first character which
                // differs case-insensitively.
                for (a, b) in iter {
                    let la = a.to_ascii_lowercase();
                    let lb = b.to_ascii_lowercase();

                    if la != lb {
                        order = la.cmp(&lb);
                        break;
                    }
                }
            }

            return order;
        }
    }

    // Fallback: The shorter string wins.
    a.len().cmp(&b.len())
}

static mut ROOT_CASEMAP: Option<*mut icu_ffi::UCaseMap> = None;

/// Converts the given UTF-8 string to lower case.
///
/// Case folding differs from lower case in that the output is primarily useful
/// to machines for comparisons. It's like applying Unicode normalization.
pub fn fold_case<'a>(arena: &'a Arena, input: &str) -> ArenaString<'a> {
    // OnceCell for people that want to put it into a static.
    #[allow(static_mut_refs)]
    let casemap = unsafe {
        if ROOT_CASEMAP.is_none() {
            ROOT_CASEMAP = Some(if let Ok(f) = init_if_needed() {
                let mut status = icu_ffi::U_ZERO_ERROR;
                (f.ucasemap_open)(null(), 0, &mut status)
            } else {
                null_mut()
            })
        }
        ROOT_CASEMAP.unwrap_unchecked()
    };

    if !casemap.is_null() {
        let f = assume_loaded();
        let mut status = icu_ffi::U_ZERO_ERROR;
        let mut output = Vec::new_in(arena);
        let mut output_len;

        // First, guess the output length:
        // TODO: What's a good heuristic here?
        {
            output.reserve_exact(input.len() + 16);
            let output = output.spare_capacity_mut();
            output_len = unsafe {
                (f.ucasemap_utf8FoldCase)(
                    casemap,
                    output.as_mut_ptr() as *mut _,
                    output.len() as i32,
                    input.as_ptr() as *const _,
                    input.len() as i32,
                    &mut status,
                )
            };
        }

        // If that failed to fit, retry with the correct length.
        if status == icu_ffi::U_BUFFER_OVERFLOW_ERROR && output_len > 0 {
            output.reserve_exact(output_len as usize);
            let output = output.spare_capacity_mut();
            output_len = unsafe {
                (f.ucasemap_utf8FoldCase)(
                    casemap,
                    output.as_mut_ptr() as *mut _,
                    output.len() as i32,
                    input.as_ptr() as *const _,
                    input.len() as i32,
                    &mut status,
                )
            };
        }

        if status.is_success() && output_len > 0 {
            unsafe {
                output.set_len(output_len as usize);
            }
            return unsafe { ArenaString::from_utf8_unchecked(output) };
        }
    }

    let mut result = ArenaString::from_str(arena, input);
    for b in unsafe { result.as_bytes_mut() } {
        b.make_ascii_lowercase();
    }
    result
}

// NOTE:
// To keep this neat, fields are ordered by prefix (= `ucol_` before `uregex_`),
// followed by functions in this order:
// * Static methods (e.g. `ucnv_getAvailableName`)
// * Constructors (e.g. `ucnv_open`)
// * Destructors (e.g. `ucnv_close`)
// * Methods, grouped by relationship
//   (e.g. `uregex_start64` and `uregex_end64` are near each other)
//
// WARNING:
// The order of the fields MUST match the order of strings in the following two arrays.
#[allow(non_snake_case)]
#[repr(C)]
struct LibraryFunctions {
    // LIBICUUC_PROC_NAMES
    u_errorName: icu_ffi::u_errorName,
    ucasemap_open: icu_ffi::ucasemap_open,
    ucasemap_utf8FoldCase: icu_ffi::ucasemap_utf8FoldCase,
    ucnv_getAvailableName: icu_ffi::ucnv_getAvailableName,
    ucnv_getStandardName: icu_ffi::ucnv_getStandardName,
    ucnv_open: icu_ffi::ucnv_open,
    ucnv_close: icu_ffi::ucnv_close,
    ucnv_convertEx: icu_ffi::ucnv_convertEx,
    utext_setup: icu_ffi::utext_setup,
    utext_close: icu_ffi::utext_close,

    // LIBICUI18N_PROC_NAMES
    ucol_open: icu_ffi::ucol_open,
    ucol_strcollUTF8: icu_ffi::ucol_strcollUTF8,
    uregex_open: icu_ffi::uregex_open,
    uregex_close: icu_ffi::uregex_close,
    uregex_setTimeLimit: icu_ffi::uregex_setTimeLimit,
    uregex_setUText: icu_ffi::uregex_setUText,
    uregex_reset64: icu_ffi::uregex_reset64,
    uregex_findNext: icu_ffi::uregex_findNext,
    uregex_groupCount: icu_ffi::uregex_groupCount,
    uregex_start64: icu_ffi::uregex_start64,
    uregex_end64: icu_ffi::uregex_end64,
}

macro_rules! proc_name {
    ($s:literal) => {
        concat!(env!("EDIT_CFG_ICU_EXPORT_PREFIX"), $s, env!("EDIT_CFG_ICU_EXPORT_SUFFIX"), "\0")
            .as_ptr() as *const c_char
    };
}

// Found in libicuuc.so on UNIX, icuuc.dll/icu.dll on Windows.
const LIBICUUC_PROC_NAMES: [*const c_char; 10] = [
    proc_name!("u_errorName"),
    proc_name!("ucasemap_open"),
    proc_name!("ucasemap_utf8FoldCase"),
    proc_name!("ucnv_getAvailableName"),
    proc_name!("ucnv_getStandardName"),
    proc_name!("ucnv_open"),
    proc_name!("ucnv_close"),
    proc_name!("ucnv_convertEx"),
    proc_name!("utext_setup"),
    proc_name!("utext_close"),
];

// Found in libicui18n.so on UNIX, icuin.dll/icu.dll on Windows.
const LIBICUI18N_PROC_NAMES: [*const c_char; 11] = [
    proc_name!("ucol_open"),
    proc_name!("ucol_strcollUTF8"),
    proc_name!("uregex_open"),
    proc_name!("uregex_close"),
    proc_name!("uregex_setTimeLimit"),
    proc_name!("uregex_setUText"),
    proc_name!("uregex_reset64"),
    proc_name!("uregex_findNext"),
    proc_name!("uregex_groupCount"),
    proc_name!("uregex_start64"),
    proc_name!("uregex_end64"),
];

enum LibraryFunctionsState {
    Uninitialized,
    Failed,
    Loaded(LibraryFunctions),
}

static mut LIBRARY_FUNCTIONS: LibraryFunctionsState = LibraryFunctionsState::Uninitialized;

pub fn init() -> apperr::Result<()> {
    init_if_needed()?;
    Ok(())
}

#[allow(static_mut_refs)]
fn init_if_needed() -> apperr::Result<&'static LibraryFunctions> {
    #[cold]
    fn load() {
        unsafe {
            LIBRARY_FUNCTIONS = LibraryFunctionsState::Failed;

            let Ok(icu) = sys::load_icu() else {
                return;
            };

            type TransparentFunction = unsafe extern "C" fn() -> *const ();

            // OH NO I'M DOING A BAD THING
            //
            // If this assertion hits, you either forgot to update `LIBRARY_PROC_NAMES`
            // or you're on a platform where `dlsym` behaves different from classic UNIX and Windows.
            //
            // This code assumes that we can treat the `LibraryFunctions` struct containing various different function
            // pointers as an array of `TransparentFunction` pointers. In C, this works on any platform that supports
            // POSIX `dlsym` or equivalent, but I suspect Rust is once again being extra about it. In any case, that's
            // still better than loading every function one by one, just to blow up our binary size for no reason.
            const _: () = assert!(
                mem::size_of::<LibraryFunctions>()
                    == mem::size_of::<TransparentFunction>()
                        * (LIBICUUC_PROC_NAMES.len() + LIBICUI18N_PROC_NAMES.len())
            );

            let mut funcs = MaybeUninit::<LibraryFunctions>::uninit();
            let mut ptr = funcs.as_mut_ptr() as *mut TransparentFunction;

            #[cfg(edit_icu_renaming_auto_detect)]
            let scratch_outer = scratch_arena(None);
            #[cfg(edit_icu_renaming_auto_detect)]
            let suffix = sys::icu_detect_renaming_suffix(&scratch_outer, icu.libicuuc);

            for (handle, names) in [
                (icu.libicuuc, &LIBICUUC_PROC_NAMES[..]),
                (icu.libicui18n, &LIBICUI18N_PROC_NAMES[..]),
            ] {
                for &name in names {
                    #[cfg(edit_icu_renaming_auto_detect)]
                    let scratch = scratch_arena(Some(&scratch_outer));
                    #[cfg(edit_icu_renaming_auto_detect)]
                    let name = sys::icu_add_renaming_suffix(&scratch, name, &suffix);

                    let Ok(func) = sys::get_proc_address(handle, name) else {
                        debug_assert!(
                            false,
                            "Failed to load ICU function: {:?}",
                            CStr::from_ptr(name)
                        );
                        return;
                    };

                    ptr.write(func);
                    ptr = ptr.add(1);
                }
            }

            LIBRARY_FUNCTIONS = LibraryFunctionsState::Loaded(funcs.assume_init());
        }
    }

    unsafe {
        if matches!(&LIBRARY_FUNCTIONS, LibraryFunctionsState::Uninitialized) {
            load();
        }
    }

    match unsafe { &LIBRARY_FUNCTIONS } {
        LibraryFunctionsState::Loaded(f) => Ok(f),
        _ => Err(apperr::APP_ICU_MISSING),
    }
}

#[allow(static_mut_refs)]
fn assume_loaded() -> &'static LibraryFunctions {
    match unsafe { &LIBRARY_FUNCTIONS } {
        LibraryFunctionsState::Loaded(f) => f,
        _ => unreachable!(),
    }
}

mod icu_ffi {
    #![allow(dead_code, non_camel_case_types)]

    use std::ffi::{c_char, c_int, c_void};

    use crate::apperr;

    #[derive(Copy, Clone, Eq, PartialEq)]
    #[repr(transparent)]
    pub struct UErrorCode(c_int);

    impl UErrorCode {
        pub const fn new(code: u32) -> Self {
            Self(code as c_int)
        }

        pub fn is_success(&self) -> bool {
            self.0 <= 0
        }

        pub fn is_failure(&self) -> bool {
            self.0 > 0
        }

        pub fn as_error(&self) -> apperr::Error {
            debug_assert!(self.0 > 0);
            apperr::Error::new_icu(self.0 as u32)
        }
    }

    pub const U_ZERO_ERROR: UErrorCode = UErrorCode(0);
    pub const U_BUFFER_OVERFLOW_ERROR: UErrorCode = UErrorCode(15);
    pub const U_UNSUPPORTED_ERROR: UErrorCode = UErrorCode(16);

    pub type u_errorName = unsafe extern "C" fn(code: UErrorCode) -> *const c_char;

    pub struct UConverter;

    pub type ucnv_getAvailableName = unsafe extern "C" fn(n: i32) -> *const c_char;

    pub type ucnv_getStandardName = unsafe extern "C" fn(
        name: *const u8,
        standard: *const u8,
        status: &mut UErrorCode,
    ) -> *const c_char;

    pub type ucnv_open =
        unsafe extern "C" fn(converter_name: *const u8, status: &mut UErrorCode) -> *mut UConverter;

    pub type ucnv_close = unsafe extern "C" fn(converter: *mut UConverter);

    pub type ucnv_convertEx = unsafe extern "C" fn(
        target_cnv: *mut UConverter,
        source_cnv: *mut UConverter,
        target: *mut *mut u8,
        target_limit: *const u8,
        source: *mut *const u8,
        source_limit: *const u8,
        pivot_start: *mut u16,
        pivot_source: *mut *mut u16,
        pivot_target: *mut *mut u16,
        pivot_limit: *const u16,
        reset: bool,
        flush: bool,
        status: &mut UErrorCode,
    );

    pub struct UCaseMap;

    pub type ucasemap_open = unsafe extern "C" fn(
        locale: *const c_char,
        options: u32,
        status: &mut UErrorCode,
    ) -> *mut UCaseMap;

    pub type ucasemap_utf8FoldCase = unsafe extern "C" fn(
        csm: *const UCaseMap,
        dest: *mut c_char,
        dest_capacity: i32,
        src: *const c_char,
        src_length: i32,
        status: &mut UErrorCode,
    ) -> i32;

    #[repr(C)]
    pub enum UCollationResult {
        UCOL_EQUAL = 0,
        UCOL_GREATER = 1,
        UCOL_LESS = -1,
    }

    #[repr(C)]
    pub struct UCollator;

    pub type ucol_open =
        unsafe extern "C" fn(loc: *const c_char, status: &mut UErrorCode) -> *mut UCollator;

    pub type ucol_strcollUTF8 = unsafe extern "C" fn(
        coll: *mut UCollator,
        source: *const u8,
        source_length: i32,
        target: *const u8,
        target_length: i32,
        status: &mut UErrorCode,
    ) -> UCollationResult;

    // UText callback functions
    pub type UTextClone = unsafe extern "C" fn(
        dest: *mut UText,
        src: &UText,
        deep: bool,
        status: &mut UErrorCode,
    ) -> *mut UText;
    pub type UTextNativeLength = unsafe extern "C" fn(ut: &mut UText) -> i64;
    pub type UTextAccess =
        unsafe extern "C" fn(ut: &mut UText, native_index: i64, forward: bool) -> bool;
    pub type UTextExtract = unsafe extern "C" fn(
        ut: &mut UText,
        native_start: i64,
        native_limit: i64,
        dest: *mut u16,
        dest_capacity: i32,
        status: &mut UErrorCode,
    ) -> i32;
    pub type UTextReplace = unsafe extern "C" fn(
        ut: &mut UText,
        native_start: i64,
        native_limit: i64,
        replacement_text: *const u16,
        replacement_length: i32,
        status: &mut UErrorCode,
    ) -> i32;
    pub type UTextCopy = unsafe extern "C" fn(
        ut: &mut UText,
        native_start: i64,
        native_limit: i64,
        native_dest: i64,
        move_text: bool,
        status: &mut UErrorCode,
    );
    pub type UTextMapOffsetToNative = unsafe extern "C" fn(ut: &UText) -> i64;
    pub type UTextMapNativeIndexToUTF16 =
        unsafe extern "C" fn(ut: &UText, native_index: i64) -> i32;
    pub type UTextClose = unsafe extern "C" fn(ut: &mut UText);

    #[repr(C)]
    pub struct UTextFuncs {
        pub table_size: i32,
        pub reserved1: i32,
        pub reserved2: i32,
        pub reserved3: i32,
        pub clone: Option<UTextClone>,
        pub native_length: Option<UTextNativeLength>,
        pub access: Option<UTextAccess>,
        pub extract: Option<UTextExtract>,
        pub replace: Option<UTextReplace>,
        pub copy: Option<UTextCopy>,
        pub map_offset_to_native: Option<UTextMapOffsetToNative>,
        pub map_native_index_to_utf16: Option<UTextMapNativeIndexToUTF16>,
        pub close: Option<UTextClose>,
        pub spare1: Option<UTextClose>,
        pub spare2: Option<UTextClose>,
        pub spare3: Option<UTextClose>,
    }

    #[repr(C)]
    pub struct UText {
        pub magic: u32,
        pub flags: i32,
        pub provider_properties: i32,
        pub size_of_struct: i32,
        pub chunk_native_limit: i64,
        pub extra_size: i32,
        pub native_indexing_limit: i32,
        pub chunk_native_start: i64,
        pub chunk_offset: i32,
        pub chunk_length: i32,
        pub chunk_contents: *const u16,
        pub p_funcs: &'static UTextFuncs,
        pub p_extra: *mut c_void,
        pub context: *mut c_void,
        pub p: *mut c_void,
        pub q: *mut c_void,
        pub r: *mut c_void,
        pub priv_p: *mut c_void,
        pub a: i64,
        pub b: i32,
        pub c: i32,
        pub priv_a: i64,
        pub priv_b: i32,
        pub priv_c: i32,
    }

    pub const UTEXT_MAGIC: u32 = 0x345ad82c;
    pub const UTEXT_PROVIDER_LENGTH_IS_EXPENSIVE: i32 = 1;
    pub const UTEXT_PROVIDER_STABLE_CHUNKS: i32 = 2;
    pub const UTEXT_PROVIDER_WRITABLE: i32 = 3;
    pub const UTEXT_PROVIDER_HAS_META_DATA: i32 = 4;
    pub const UTEXT_PROVIDER_OWNS_TEXT: i32 = 5;

    pub type utext_setup = unsafe extern "C" fn(
        ut: *mut UText,
        extra_space: i32,
        status: &mut UErrorCode,
    ) -> *mut UText;
    pub type utext_close = unsafe extern "C" fn(ut: *mut UText) -> *mut UText;

    #[repr(C)]
    pub struct UParseError {
        pub line: i32,
        pub offset: i32,
        pub pre_context: [u16; 16],
        pub post_context: [u16; 16],
    }

    #[repr(C)]
    pub struct URegularExpression;

    pub const UREGEX_UNIX_LINES: i32 = 1;
    pub const UREGEX_CASE_INSENSITIVE: i32 = 2;
    pub const UREGEX_COMMENTS: i32 = 4;
    pub const UREGEX_MULTILINE: i32 = 8;
    pub const UREGEX_LITERAL: i32 = 16;
    pub const UREGEX_DOTALL: i32 = 32;
    pub const UREGEX_UWORD: i32 = 256;
    pub const UREGEX_ERROR_ON_UNKNOWN_ESCAPES: i32 = 512;

    pub type uregex_open = unsafe extern "C" fn(
        pattern: *const u16,
        pattern_length: i32,
        flags: i32,
        pe: Option<&mut UParseError>,
        status: &mut UErrorCode,
    ) -> *mut URegularExpression;
    pub type uregex_close = unsafe extern "C" fn(regexp: *mut URegularExpression);
    pub type uregex_setTimeLimit =
        unsafe extern "C" fn(regexp: *mut URegularExpression, limit: i32, status: &mut UErrorCode);
    pub type uregex_setUText = unsafe extern "C" fn(
        regexp: *mut URegularExpression,
        text: *mut UText,
        status: &mut UErrorCode,
    );
    pub type uregex_reset64 =
        unsafe extern "C" fn(regexp: *mut URegularExpression, index: i64, status: &mut UErrorCode);
    pub type uregex_findNext =
        unsafe extern "C" fn(regexp: *mut URegularExpression, status: &mut UErrorCode) -> bool;
    pub type uregex_groupCount =
        unsafe extern "C" fn(regexp: *mut URegularExpression, status: &mut UErrorCode) -> i32;
    pub type uregex_start64 = unsafe extern "C" fn(
        regexp: *mut URegularExpression,
        group_num: i32,
        status: &mut UErrorCode,
    ) -> i64;
    pub type uregex_end64 = unsafe extern "C" fn(
        regexp: *mut URegularExpression,
        group_num: i32,
        status: &mut UErrorCode,
    ) -> i64;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[ignore]
    #[test]
    fn init() {
        assert!(init_if_needed().is_ok());
    }

    #[test]
    fn test_compare_strings_ascii() {
        // Empty strings
        assert_eq!(compare_strings_ascii(b"", b""), Ordering::Equal);
        // Equal strings
        assert_eq!(compare_strings_ascii(b"hello", b"hello"), Ordering::Equal);
        // Different lengths
        assert_eq!(compare_strings_ascii(b"abc", b"abcd"), Ordering::Less);
        assert_eq!(compare_strings_ascii(b"abcd", b"abc"), Ordering::Greater);
        // Same chars, different cases - 1st char wins
        assert_eq!(compare_strings_ascii(b"AbC", b"aBc"), Ordering::Less);
        // Different chars, different cases - 2nd char wins, because it differs
        assert_eq!(compare_strings_ascii(b"hallo", b"Hello"), Ordering::Less);
        assert_eq!(compare_strings_ascii(b"Hello", b"hallo"), Ordering::Greater);
    }
}
