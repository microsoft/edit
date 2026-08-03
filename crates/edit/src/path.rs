// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! Path related helpers.

use std::ffi::{OsStr, OsString};
use std::path::{Component, MAIN_SEPARATOR_STR, Path, PathBuf};

/// Normalizes a given path by removing redundant components.
/// The given path must be absolute (e.g. by joining it with the current working directory).
pub fn normalize(path: &Path) -> PathBuf {
    let mut res = PathBuf::with_capacity(path.as_os_str().as_encoded_bytes().len());
    let mut root_len = 0;

    for component in path.components() {
        match component {
            Component::Prefix(p) => res.push(p.as_os_str()),
            Component::RootDir => {
                res.push(OsStr::new(MAIN_SEPARATOR_STR));
                root_len = res.as_os_str().as_encoded_bytes().len();
            }
            Component::CurDir => {}
            Component::ParentDir => {
                // Get the length up to the parent directory
                if let Some(len) = res
                    .parent()
                    .map(|p| p.as_os_str().as_encoded_bytes().len())
                    // Ensure we don't pop the root directory
                    && len >= root_len
                {
                    // Pop the last component from `res`.
                    //
                    // This can be replaced with a plain `res.as_mut_os_string().truncate(len)`
                    // once `os_string_truncate` is stabilized (#133262).
                    let mut bytes = res.into_os_string().into_encoded_bytes();
                    bytes.truncate(len);
                    res = PathBuf::from(unsafe { OsString::from_encoded_bytes_unchecked(bytes) });
                }
            }
            Component::Normal(p) => res.push(p),
        }
    }

    res
}

#[cfg(test)]
mod tests {
    use std::ffi::OsString;
    use std::path::Path;

    use super::*;

    fn norm(s: &str) -> OsString {
        normalize(Path::new(s)).into_os_string()
    }

    #[cfg(unix)]
    #[test]
    fn test_unix() {
        assert_eq!(norm("/a/b/c"), "/a/b/c");
        assert_eq!(norm("/a/b/c/"), "/a/b/c");
        assert_eq!(norm("/a/./b"), "/a/b");
        assert_eq!(norm("/a/b/../c"), "/a/c");
        assert_eq!(norm("/../../a"), "/a");
        assert_eq!(norm("/../"), "/");
        assert_eq!(norm("/a//b/c"), "/a/b/c");
        assert_eq!(norm("/a/b/c/../../../../d"), "/d");
        assert_eq!(norm("//"), "/");
    }

    #[cfg(windows)]
    #[test]
    fn test_windows() {
        assert_eq!(norm(r"C:\a\b\c"), r"C:\a\b\c");
        assert_eq!(norm(r"C:\a\b\c\"), r"C:\a\b\c");
        assert_eq!(norm(r"C:\a\.\b"), r"C:\a\b");
        assert_eq!(norm(r"C:\a\b\..\c"), r"C:\a\c");
        assert_eq!(norm(r"C:\..\..\a"), r"C:\a");
        assert_eq!(norm(r"C:\..\"), r"C:\");
        assert_eq!(norm(r"C:\a\\b\c"), r"C:\a\b\c");
        assert_eq!(norm(r"C:/a\b/c"), r"C:\a\b\c");
        assert_eq!(norm(r"C:\a\b\c\..\..\..\..\d"), r"C:\d");
        assert_eq!(norm(r"\\server\share\path"), r"\\server\share\path");
    }
}
