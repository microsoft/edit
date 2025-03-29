use crate::{icu, sys};
use std::num::NonZeroU32;
use std::{fmt, io, result};

// Remember to add an entry to `Error::message()` for each new error.
pub const APP_ICU_MISSING: Error = Error::new_app(1);
pub const APP_FILE_NOT_FOUND: Error = Error::new_app(2);

pub type Result<T> = result::Result<T, Error>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Error(NonZeroU32);

impl Error {
    #[allow(dead_code)]
    const FLAGS_MASK: u32 = 0xF8000000; // Top 5 bits
    const FLAGS_CUSTOM_FAILURE: u32 = 0xA0000000;

    const TAG_APP: u32 = Self::FLAGS_CUSTOM_FAILURE | (1 << 16);
    const TAG_ICU: u32 = Self::FLAGS_CUSTOM_FAILURE | (2 << 16);

    /// Creates a new error code.
    ///
    /// # Safety
    ///
    /// The caller must ensure that the code is non-zero and correctly tagged up.
    pub const unsafe fn new(code: u32) -> Self {
        Error(unsafe { NonZeroU32::new_unchecked(code) })
    }

    pub const fn new_app(code: u32) -> Self {
        debug_assert!(code > 0 && code <= 0xFFFF);
        unsafe { Self::new(Self::TAG_APP | code) }
    }

    pub const fn new_icu(code: u32) -> Self {
        debug_assert!(code > 0 && code <= 0xFFFF);
        unsafe { Self::new(Self::TAG_ICU | code) }
    }

    pub fn is_app(&self) -> bool {
        (self.0.get() & 0xFFFF0000) == Self::TAG_APP
    }

    pub fn is_icu(&self) -> bool {
        (self.0.get() & 0xFFFF0000) == Self::TAG_ICU
    }

    pub fn code(&self) -> u32 {
        self.0.get() & 0xFFFF
    }

    pub fn value(&self) -> u32 {
        self.0.get()
    }

    pub fn message(self) -> String {
        match self {
            APP_ICU_MISSING => "ICU not found".to_string(),
            APP_FILE_NOT_FOUND => "File not found".to_string(),
            _ => {
                debug_assert!(!self.is_app());
                if self.is_icu() {
                    icu::format_error(self)
                } else {
                    sys::format_error(self)
                }
            }
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:#08x}", self.0)
    }
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        match err.kind() {
            io::ErrorKind::NotFound => APP_FILE_NOT_FOUND,
            _ => sys::io_error_to_apperr(err),
        }
    }
}
