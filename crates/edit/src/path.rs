// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! Path related helpers.

use std::ffi::{OsStr, OsString};
use std::path::{Component, MAIN_SEPARATOR_STR, Path, PathBuf};

#[derive(Clone, Copy)]
enum EnvVarSyntax {
    Percent,
    Dollar,
}

/// Expands environment variables and a leading home-directory marker.
///
/// Windows paths use `%NAME%` variables (excluding `=` and NUL); other platforms
/// use `$NAME` with an ASCII identifier.
pub fn expand(path: &Path) -> PathBuf {
    let syntax = if cfg!(windows) { EnvVarSyntax::Percent } else { EnvVarSyntax::Dollar };
    let home =
        if cfg!(windows) { std::env::var_os("USERPROFILE") } else { std::env::var_os("HOME") };
    expand_with(path, syntax, home, |name| std::env::var_os(name))
}

fn expand_with(
    path: &Path,
    syntax: EnvVarSyntax,
    home: Option<OsString>,
    mut get_var: impl FnMut(&OsStr) -> Option<OsString>,
) -> PathBuf {
    let bytes = path.as_os_str().as_encoded_bytes();
    let mut expanded = Vec::with_capacity(bytes.len());
    let mut i = 0;
    let mut changed = false;

    if bytes.first() == Some(&b'~')
        && (bytes.len() == 1
            || bytes.get(1) == Some(&b'/')
            || (matches!(syntax, EnvVarSyntax::Percent) && bytes.get(1) == Some(&b'\\')))
        && let Some(home) = home.filter(|home| !home.is_empty())
    {
        expanded.extend_from_slice(home.as_encoded_bytes());
        i = 1;
        changed = true;
    }

    while i < bytes.len() {
        let delimiter = match syntax {
            EnvVarSyntax::Percent => b'%',
            EnvVarSyntax::Dollar => b'$',
        };
        if bytes[i] != delimiter {
            expanded.push(bytes[i]);
            i += 1;
            continue;
        }

        let name_start = i + 1;
        let name_end = match syntax {
            EnvVarSyntax::Percent => bytes[name_start..]
                .iter()
                .position(|&b| b == b'%')
                .map(|offset| name_start + offset)
                .filter(|&end| valid_percent_env_name(&bytes[name_start..end])),
            EnvVarSyntax::Dollar => {
                if bytes.get(name_start).is_some_and(|&b| is_env_name_start(b)) {
                    let mut end = name_start + 1;
                    while bytes.get(end).is_some_and(|&b| is_env_name_continue(b)) {
                        end += 1;
                    }
                    Some(end)
                } else {
                    None
                }
            }
        };

        if let Some(name_end) = name_end {
            // Both bounds follow ASCII syntax bytes in the original encoded path.
            let name = unsafe { OsStr::from_encoded_bytes_unchecked(&bytes[name_start..name_end]) };
            if let Some(value) = get_var(name).filter(|value| !value.is_empty()) {
                expanded.extend_from_slice(value.as_encoded_bytes());
                i = name_end + usize::from(matches!(syntax, EnvVarSyntax::Percent));
                changed = true;
                continue;
            }
        }

        expanded.push(bytes[i]);
        i += 1;
    }

    if !changed {
        return path.to_path_buf();
    }

    // Original path fragments and replacement values are valid encoded strings.
    // Splitting the original only at ASCII syntax bytes preserves encoding
    // boundaries on both UTF-8 and Windows WTF-8.
    PathBuf::from(unsafe { OsString::from_encoded_bytes_unchecked(expanded) })
}

fn valid_percent_env_name(name: &[u8]) -> bool {
    !name.is_empty() && !name.contains(&b'=') && !name.contains(&0)
}

fn is_env_name_start(byte: u8) -> bool {
    byte.is_ascii_alphabetic() || byte == b'_'
}

fn is_env_name_continue(byte: u8) -> bool {
    is_env_name_start(byte) || byte.is_ascii_digit()
}

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

    fn expand_as(path: &str, syntax: EnvVarSyntax, home: Option<&str>) -> PathBuf {
        expand_with(Path::new(path), syntax, home.map(OsString::from), |name| match name.to_str() {
            Some("HOME") => Some(OsString::from("/test/home")),
            Some("WINDIR") => Some(OsString::from(r"C:\Windows")),
            Some("USERPROFILE") => Some(OsString::from(r"C:\Users\test user")),
            Some("ProgramFiles(x86)") => Some(OsString::from(r"C:\Program Files (x86)")),
            Some("RELATIVE") => Some(OsString::from("a relative directory")),
            Some("EMPTY") => Some(OsString::new()),
            _ => None,
        })
    }

    #[test]
    fn test_expand_percent_variables() {
        assert_eq!(
            expand_as(r"%WINDIR%\System32", EnvVarSyntax::Percent, None),
            PathBuf::from(r"C:\Windows\System32")
        );
        assert_eq!(
            expand_as(r"%USERPROFILE%\My Documents", EnvVarSyntax::Percent, None),
            PathBuf::from(r"C:\Users\test user\My Documents")
        );
        assert_eq!(
            expand_as(r"%ProgramFiles(x86)%\Edit", EnvVarSyntax::Percent, None),
            PathBuf::from(r"C:\Program Files (x86)\Edit")
        );
    }

    #[test]
    fn test_expand_dollar_variables() {
        assert_eq!(
            expand_as("$HOME/.ssh/config", EnvVarSyntax::Dollar, None),
            PathBuf::from("/test/home/.ssh/config")
        );
        assert_eq!(
            expand_as("$RELATIVE/file", EnvVarSyntax::Dollar, None),
            PathBuf::from("a relative directory/file")
        );
        assert_eq!(
            expand_as("$HOME/My Documents", EnvVarSyntax::Dollar, None),
            PathBuf::from("/test/home/My Documents")
        );
    }

    #[test]
    fn test_expand_home_directory() {
        assert_eq!(
            expand_as("~/.ssh/config", EnvVarSyntax::Dollar, Some("/test/home")),
            PathBuf::from("/test/home/.ssh/config")
        );
        assert_eq!(
            expand_as("~\\Documents", EnvVarSyntax::Percent, Some(r"C:\Users\test user")),
            PathBuf::from(r"C:\Users\test user\Documents")
        );
        assert_eq!(
            expand_as("~user/file", EnvVarSyntax::Dollar, Some("/home")),
            Path::new("~user/file")
        );
    }

    #[test]
    fn test_expansion_preserves_unexpanded_paths_and_tokens() {
        for path in [
            "ordinary/path",
            "/ordinary/absolute path",
            "a path/with spaces",
            "literal$5",
            "$",
            "${HOME}",
            "%",
            "%BAD-NAME%",
        ] {
            assert_eq!(expand_as(path, EnvVarSyntax::Dollar, None), PathBuf::from(path));
        }
        for path in [
            r"ordinary\path",
            r"C:\ordinary\absolute path",
            r"a path\with spaces",
            "literal%5",
            "%",
            "%MISSING%",
            "%EMPTY%",
            "%BAD=NAME%",
        ] {
            assert_eq!(expand_as(path, EnvVarSyntax::Percent, None), PathBuf::from(path));
        }
        assert_eq!(
            expand_as("$MISSING/file", EnvVarSyntax::Dollar, None),
            PathBuf::from("$MISSING/file")
        );
        assert_eq!(
            expand_as("$EMPTY/file", EnvVarSyntax::Dollar, None),
            PathBuf::from("$EMPTY/file")
        );
        assert_eq!(expand_as("~/file", EnvVarSyntax::Dollar, Some("")), PathBuf::from("~/file"));
    }

    #[test]
    fn test_expansion_is_not_recursive_or_cross_platform() {
        let result =
            expand_with(Path::new("$HOME/%WINDIR%"), EnvVarSyntax::Dollar, None, |name| match name
                .to_str()
            {
                Some("HOME") => Some(OsString::from("$OTHER")),
                _ => None,
            });
        assert_eq!(result, PathBuf::from("$OTHER/%WINDIR%"));
    }

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
