// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::path::is_separator;

pub fn glob_match(glob: &[u8], path: &[u8]) -> bool {
    fast_path(glob, path).unwrap_or_else(|| slow_path(glob, path))
}

// Fast-pass for the most common patterns:
// * Matching files by extension (e.g., **/*.rs)
// * Matching files by name (e.g., **/Cargo.toml)
fn fast_path(glob: &[u8], path: &[u8]) -> Option<bool> {
    // In either case, the glob must start with "**/".
    let mut suffix = glob.strip_prefix(b"**/")?;
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
        match_path_suffix(path, suffix)
            && (
                // In case of "**/*extension" a simple suffix match is sufficient.
                !needs_dir_anchor
                // But for "**/filename" we need to ensure that path is either "filename"...
                || path.len() == suffix.len()
                // ...or that it is ".../filename".
                || is_separator(path[path.len() - suffix.len() - 1] as char)
            ),
    )
}

fn contains_magic(glob: &[u8]) -> bool {
    glob.iter().any(|&b| b == b'*' || b == b'?')
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
fn slow_path(glob: &[u8], path: &[u8]) -> bool {
    let mut px = 0;
    let mut nx = 0;
    let mut next_px = 0;
    let mut next_nx = 0;
    let mut next_double_px = 0;
    let mut next_double_nx = 0;

    while px < glob.len() || nx < path.len() {
        if px < glob.len() {
            match glob[px] {
                b'?' => {
                    // single-character wildcard
                    if nx < path.len() && !is_separator(path[nx] as char) {
                        px += 1;
                        nx += 1;
                        continue;
                    }
                }
                b'*' => {
                    // Check for doublestar
                    if px + 1 < glob.len() && glob[px + 1] == b'*' {
                        // doublestar - matches across path separators
                        // Handle trailing slash after ** (e.g., **/ should skip the slash)
                        let skip = if px + 2 < glob.len() && glob[px + 2] == b'/' { 3 } else { 2 };
                        // Try to match at nx first (zero-length match). If that doesn't work, restart at nx+1.
                        next_double_px = px;
                        next_double_nx = nx + 1;
                        px += skip;
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
                    if nx < path.len() && path[nx].eq_ignore_ascii_case(&c) {
                        px += 1;
                        nx += 1;
                        continue;
                    }
                }
            }
        }

        // Mismatch. Maybe restart.
        // Try single-star backtracking first, but only if we don't cross a separator
        if 0 < next_nx && next_nx <= path.len() && !is_separator(path[next_nx - 1] as char) {
            px = next_px;
            nx = next_nx;
            continue;
        }

        // Try doublestar backtracking
        if 0 < next_double_nx && next_double_nx <= path.len() {
            px = next_double_px;
            nx = next_double_nx;
            continue;
        }

        return false;
    }

    // Matched all of pattern to all of name. Success.
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
            ("a*b?c*x", "abxbbxdbxebxczzx", true),
            ("a*b?c*x", "abxbbxdbxebxczzy", false),
            ("*x", "xxx", true),
            // Test cases from https://github.com/golang/go/blob/master/src/path/filepath/match_test.go
            ("abc", "abc", true),
            ("*", "abc", true),
            ("*c", "abc", true),
            ("a*", "a", true),
            ("a*", "abc", true),
            ("a*", "ab/c", false),
            ("a*/b", "abc/b", true),
            ("a*/b", "a/c/b", false),
            ("a*b*c*d*e*/f", "axbxcxdxe/f", true),
            ("a*b*c*d*e*/f", "axbxcxdxexxx/f", true),
            ("a*b*c*d*e*/f", "axbxcxdxe/xxx/f", false),
            ("a*b*c*d*e*/f", "axbxcxdxexxx/fff", false),
            ("a*b?c*x", "abxbbxdbxebxczzx", true),
            ("a*b?c*x", "abxbbxdbxebxczzy", false),
            ("a?b", "a/b", false),
            ("a*b", "a/b", false),
            ("*x", "xxx", true),
            // Basic doublestar tests
            ("**", "foo", true),
            ("**", "foo/bar", true),
            ("**", "foo/bar/baz", true),
            ("**/foo", "foo", true),
            ("**/foo", "bar/foo", true),
            ("**/foo", "bar/baz/foo", true),
            ("foo/**", "foo/bar", true),
            ("foo/**", "foo/bar/baz", true),
            ("foo/**/baz", "foo/baz", true),
            ("foo/**/baz", "foo/bar/baz", true),
            ("foo/**/baz", "foo/bar/qux/baz", true),
            // Doublestar should not match if literal parts don't match
            ("**/foo", "bar", false),
            ("foo/**", "bar/baz", false),
            ("foo/**/baz", "foo/bar/qux", false),
            // Single star should NOT match separators
            ("foo/*/bar", "foo/bar", false),
            ("foo/*/bar", "foo/baz/bar", true),
            ("foo/*/bar", "foo/baz/qux/bar", false),
            // Mix of single and double star
            ("foo/*/baz/**", "foo/bar/baz/qux", true),
            ("foo/*/baz/**", "foo/bar/qux/baz/test", false),
            // Edge cases
            ("**/**", "foo/bar", true),
            ("**/*/foo", "bar/baz/foo", true),
            ("**/*/foo", "bar/foo", true),
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
            let result = glob_match(pattern.as_bytes(), name.as_bytes());
            assert_eq!(
                result, expected,
                "glob_match({:?}, {:?}) = {}, want {}",
                pattern, name, result, expected
            );
        }
    }
}
