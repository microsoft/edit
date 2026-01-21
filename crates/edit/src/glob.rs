// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! Simple glob matching.
//!
//! Supported patterns:
//! - `*` matches any characters except for path separators, including an empty string.
//! - `**` matches any characters, including an empty string.
//!   For convenience, `/**/` also matches `/`.

use std::path::is_separator;

#[inline]
pub fn glob_match<P: AsRef<[u8]>, N: AsRef<[u8]>>(pattern: P, needle: N) -> bool {
    glob(pattern.as_ref(), needle.as_ref())
}

fn glob(pattern: &[u8], needle: &[u8]) -> bool {
    fast_path(pattern, needle).unwrap_or_else(|| slow_path(pattern, needle))
}

// Fast-pass for the most common patterns:
// * Matching files by extension (e.g., **/*.rs)
// * Matching files by name (e.g., **/Cargo.toml)
fn fast_path(pattern: &[u8], needle: &[u8]) -> Option<bool> {
    // In either case, the glob must start with "**/".
    let mut suffix = pattern.strip_prefix(b"**/")?;
    if suffix.is_empty() {
        return None;
    }

    // Determine whether it's "**/" or "**/*".
    let mut needs_dir_anchor = true;
    if let Some(s) = suffix.strip_prefix(b"*") {
        suffix = s;
        needs_dir_anchor = false;
    }

    // Restrict down to anything we can handle with a suffix check.
    if suffix.is_empty() || contains_magic(suffix) {
        return None;
    }

    Some(
        match_path_suffix(needle, suffix)
            && (
                // In case of "**/*extension" a simple suffix match is sufficient.
                !needs_dir_anchor
                // But for "**/filename" we need to ensure that path is either "filename"...
                || needle.len() == suffix.len()
                // ...or that it is ".../filename".
                || is_separator(needle[needle.len() - suffix.len() - 1] as char)
            ),
    )
}

fn contains_magic(pattern: &[u8]) -> bool {
    pattern.contains(&b'*')
}

fn match_path_suffix(path: &[u8], suffix: &[u8]) -> bool {
    if path.len() < suffix.len() {
        return false;
    }

    let path = &path[path.len() - suffix.len()..];

    #[cfg(windows)]
    {
        path.iter().zip(suffix.iter()).all(|(a, b)| {
            let a = if *a == b'\\' { b'/' } else { *a };
            let b = if *b == b'\\' { b'/' } else { *b };
            a.eq_ignore_ascii_case(&b)
        })
    }

    #[cfg(not(windows))]
    path.eq_ignore_ascii_case(suffix)
}

// This code is based on https://research.swtch.com/glob.go
// It's not particularly fast, but it doesn't need to be. It doesn't run often.
#[cold]
fn slow_path(pattern: &[u8], needle: &[u8]) -> bool {
    let mut px = 0;
    let mut nx = 0;
    let mut next_px = 0;
    let mut next_nx = 0;
    let mut next_double_px = 0;
    let mut next_double_nx = 0;

    while px < pattern.len() || nx < needle.len() {
        if px < pattern.len() {
            match pattern[px] {
                b'*' => {
                    if pattern.get(px + 1) == Some(&b'*') {
                        // doublestar - matches any characters including /
                        next_double_px = px;
                        next_double_nx = nx + 1;
                        px += 2;

                        // For convenience, /**/  also matches /.
                        if px >= 3
                            && px < pattern.len()
                            && pattern[px] == b'/'
                            && pattern[px - 3] == b'/'
                        {
                            px += 1;
                        }
                    } else {
                        // single star - does not match path separators
                        // Try to match at nx. If that doesn't work out, restart at nx+1 next.
                        next_px = px;
                        next_nx = nx + 1;
                        px += 1;
                    }
                    continue;
                }
                c => {
                    // ordinary character
                    if nx < needle.len() && needle[nx].eq_ignore_ascii_case(&c) {
                        px += 1;
                        nx += 1;
                        continue;
                    }
                }
            }
        }

        // Mismatch. Maybe restart.
        // Try single-star backtracking first, but only if we don't cross a separator.
        if next_nx > 0 && next_nx <= needle.len() && !is_separator(needle[next_nx - 1] as char) {
            px = next_px;
            nx = next_nx;
            continue;
        }

        // Try doublestar backtracking
        if next_double_nx > 0 && next_double_nx <= needle.len() {
            px = next_double_px;
            nx = next_double_nx;
            continue;
        }

        return false;
    }

    true
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_glob_match() {
        let tests = [
            // Test cases from https://research.swtch.com/glob.go
            ("", "", true),
            ("x", "", false),
            ("", "x", false),
            ("abc", "abc", true),
            ("*", "abc", true),
            ("*c", "abc", true),
            ("*b", "abc", false),
            ("a*", "abc", true),
            ("b*", "abc", false),
            ("a*", "a", true),
            ("*a", "a", true),
            ("a*b*c*d*e*", "axbxcxdxe", true),
            ("a*b*c*d*e*", "axbxcxdxexxx", true),
            ("*x", "xxx", true),
            // Test cases from https://github.com/golang/go/blob/master/src/path/filepath/match_test.go
            ("a*", "ab/c", false),
            ("a*b", "a/b", false),
            ("a*/b", "abc/b", true),
            ("a*/b", "a/c/b", false),
            ("a*b*c*d*e*/f", "axbxcxdxe/f", true),
            ("a*b*c*d*e*/f", "axbxcxdxexxx/f", true),
            ("a*b*c*d*e*/f", "axbxcxdxe/xxx/f", false),
            ("a*b*c*d*e*/f", "axbxcxdxexxx/fff", false),
            // Single star (*)
            ("*", "", true),
            ("foo/*/bar", "foo/bar", false),
            ("foo/*/bar", "foo/baz/bar", true),
            ("foo/*/bar", "foo/baz/qux/bar", false),
            // Double star (**)
            ("**", "", true),
            ("**", "foo", true),
            ("**", "foo/bar", true),
            ("**", "foo/bar/baz", true),
            ("**/foo", "foo", true),
            ("**/foo", "bar/foo", true),
            ("**/foo", "bar/baz/foo", true),
            ("**/foo", "bar", false),
            ("foo/**", "foo/bar", true),
            ("foo/**", "foo/bar/baz", true),
            ("foo/**", "bar/baz", false),
            ("foo/**/baz", "foo/baz", true),
            ("foo/**/baz", "foo/bar/baz", true),
            ("foo/**/baz", "foo/bar/qux/baz", true),
            ("foo/**/baz", "foo/bar/qux", false),
            ("**/**", "foo/bar", true),
            ("foo**bar", "foobar", true),
            ("foo**bar", "fooxbar", true),
            ("foo**bar", "foo/bar", true),
            ("foo**/bar", "foobar", false),
            ("foo/**bar", "foobar", false),
            ("**/", "foo/", true),
            ("/**", "/", true),
            ("/**", "/foo", true),
            ("foo/**", "foo/", true),
            ("a/**/b/**/c", "a/b/c", true),
            ("a/**/b/**/c", "a/x/b/y/c", true),
            ("a/**/b/**/c", "a/x/y/b/z/w/c", true),
            // Mix of * and **
            ("foo/*/baz/**", "foo/bar/baz/qux", true),
            ("foo/*/baz/**", "foo/bar/qux/baz/test", false),
            ("**/*/foo", "bar/baz/foo", true),
            ("**/*/foo", "bar/foo", true),
            // Case insensitivity
            ("*.txt", "file.TXT", true),
            ("**/*.rs", "dir/file.RS", true),
            // Optimized patterns: **/*.ext and **/name
            ("**/*.rs", "foo.rs", true),
            ("**/*.rs", "dir/foo.rs", true),
            ("**/*.rs", "dir/sub/foo.rs", true),
            ("**/*.rs", "foo.txt", false),
            ("**/*.rs", "dir/foo.txt", false),
            ("**/Cargo.toml", "Cargo.toml", true),
            ("**/Cargo.toml", "dir/Cargo.toml", true),
            ("**/Cargo.toml", "dir/sub/Cargo.toml", true),
            ("**/Cargo.toml", "Cargo.lock", false),
            ("**/Cargo.toml", "dir/Cargo.lock", false),
        ];

        for (pattern, name, expected) in tests {
            let result = glob_match(pattern, name);
            assert_eq!(
                result, expected,
                "glob_match({:?}, {:?}), got {}, expected {}",
                pattern, name, result, expected
            );
        }
    }
}
