// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! Platform abstractions.

#[cfg(unix)]
mod unix;
#[cfg(windows)]
mod windows;

#[cfg(not(windows))]
pub use std::fs::canonicalize;

#[cfg(unix)]
pub use unix::*;
#[cfg(windows)]
pub use windows::*;

/// EN: Returns the user's desktop directory through the platform-neutral directory provider.
/// 中文：透過跨平台目錄提供者取得使用者的桌面資料夾。
pub fn desktop_dir() -> std::io::Result<std::path::PathBuf> {
    dirs::desktop_dir().ok_or_else(|| {
        std::io::Error::new(std::io::ErrorKind::NotFound, "desktop directory is unavailable")
    })
}
