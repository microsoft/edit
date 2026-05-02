// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::ffi::OsStr;
use std::fs;
use std::io;
use std::os::windows::ffi::OsStrExt;
use std::path::{Path, PathBuf};
use std::ptr::{null, null_mut};
use std::{env, mem};

use windows_sys::Win32::Foundation::{ERROR_FILE_NOT_FOUND, ERROR_SUCCESS};
use windows_sys::Win32::Storage::FileSystem;
use windows_sys::Win32::System::Registry;
use windows_sys::Win32::UI::WindowsAndMessaging;
use windows_sys::core::w;

const INSTALL_DIR_NAME: &str = "Microsoft\\Edit";
const ENVIRONMENT_KEY: windows_sys::core::PCWSTR =
    w!("SYSTEM\\CurrentControlSet\\Control\\Session Manager\\Environment");
const PATH_VALUE: windows_sys::core::PCWSTR = w!("Path");

pub fn install() -> io::Result<PathBuf> {
    let install_dir = install_dir()?;
    let source =
        env::current_exe().map_err(|err| with_context(err, "failed to locate edit.exe"))?;
    let target = install_dir.join("edit.exe");

    fs::create_dir_all(&install_dir)
        .map_err(|err| with_context(err, "failed to create the install directory"))?;

    if !same_path(&source, &target) {
        fs::copy(&source, &target).map_err(|err| {
            with_context(err, "failed to copy edit.exe into the install directory")
        })?;
    }

    let (path, value_type) = read_machine_path()?;
    let updated_path = add_to_path_before_system32(&path, &install_dir);
    if updated_path != path {
        write_machine_path(&updated_path, value_type)?;
        notify_environment_changed();
    }

    Ok(install_dir)
}

pub fn uninstall() -> io::Result<PathBuf> {
    let install_dir = install_dir()?;
    let target = install_dir.join("edit.exe");

    let (path, value_type) = read_machine_path()?;
    let updated_path = remove_from_path(&path, &install_dir);
    if updated_path != path {
        write_machine_path(&updated_path, value_type)?;
        notify_environment_changed();
    }

    remove_file_or_schedule_delete(&target)?;
    match fs::remove_dir(&install_dir) {
        Ok(()) => {}
        Err(err)
            if matches!(err.kind(), io::ErrorKind::NotFound | io::ErrorKind::DirectoryNotEmpty) => {
        }
        Err(err) => return Err(with_context(err, "failed to remove the install directory")),
    }

    Ok(install_dir)
}

fn install_dir() -> io::Result<PathBuf> {
    let program_files = env::var_os("ProgramFiles").ok_or_else(|| {
        io::Error::new(io::ErrorKind::NotFound, "ProgramFiles environment variable is not set")
    })?;
    Ok(PathBuf::from(program_files).join(INSTALL_DIR_NAME))
}

fn read_machine_path() -> io::Result<(String, Registry::REG_VALUE_TYPE)> {
    let key = RegKey::open(Registry::KEY_QUERY_VALUE)?;
    let mut value_type = 0;
    let mut byte_len = 0;

    let res = unsafe {
        Registry::RegQueryValueExW(
            key.raw(),
            PATH_VALUE,
            null(),
            &mut value_type,
            null_mut(),
            &mut byte_len,
        )
    };
    if res == ERROR_FILE_NOT_FOUND {
        return Ok((String::new(), Registry::REG_EXPAND_SZ));
    }
    win32_result(res)?;

    if value_type != Registry::REG_SZ && value_type != Registry::REG_EXPAND_SZ {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "machine Path registry value is not a string",
        ));
    }

    let mut buffer = vec![0u16; byte_len.div_ceil(2) as usize];
    let res = unsafe {
        Registry::RegQueryValueExW(
            key.raw(),
            PATH_VALUE,
            null(),
            &mut value_type,
            buffer.as_mut_ptr().cast(),
            &mut byte_len,
        )
    };
    win32_result(res)?;

    buffer.truncate(byte_len as usize / mem::size_of::<u16>());
    while buffer.last() == Some(&0) {
        buffer.pop();
    }

    Ok((String::from_utf16_lossy(&buffer), value_type))
}

fn write_machine_path(path: &str, value_type: Registry::REG_VALUE_TYPE) -> io::Result<()> {
    let key = RegKey::open(Registry::KEY_QUERY_VALUE | Registry::KEY_SET_VALUE)?;
    let path = to_wide(path);
    let byte_len = path
        .len()
        .checked_mul(mem::size_of::<u16>())
        .and_then(|len| u32::try_from(len).ok())
        .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, "Path value is too long"))?;

    let res = unsafe {
        Registry::RegSetValueExW(
            key.raw(),
            PATH_VALUE,
            0,
            value_type,
            path.as_ptr().cast(),
            byte_len,
        )
    };
    win32_result(res).map_err(|err| with_context(err, "failed to update the machine Path"))
}

fn add_to_path_before_system32(path: &str, install_dir: &Path) -> String {
    let install_dir = path_to_string(install_dir);
    let mut entries = path_entries_without(path, &install_dir);
    let insert_at = entries.iter().position(|entry| is_system32_path(entry)).unwrap_or(0);
    entries.insert(insert_at, install_dir);
    entries.join(";")
}

fn remove_from_path(path: &str, install_dir: &Path) -> String {
    let install_dir = path_to_string(install_dir);
    path_entries_without(path, &install_dir).join(";")
}

fn path_entries_without(path: &str, install_dir: &str) -> Vec<String> {
    path.split(';')
        .map(str::trim)
        .filter(|entry| !entry.is_empty())
        .filter(|entry| !same_path_string(entry, install_dir))
        .map(ToOwned::to_owned)
        .collect()
}

fn same_path_string(left: &str, right: &str) -> bool {
    normalize_path(left) == normalize_path(right)
}

fn same_path(left: &Path, right: &Path) -> bool {
    let left = fs::canonicalize(left).unwrap_or_else(|_| left.to_path_buf());
    let right = fs::canonicalize(right).unwrap_or_else(|_| right.to_path_buf());
    same_path_string(&path_to_string(&left), &path_to_string(&right))
}

fn is_system32_path(path: &str) -> bool {
    let path = normalize_path(path);
    if path == r"%systemroot%\system32" || path == r"%windir%\system32" {
        return true;
    }

    system_root().map(|root| path == normalize_path(&format!("{root}\\System32"))).unwrap_or(false)
}

fn system_root() -> Option<String> {
    env::var("SystemRoot").ok().or_else(|| env::var("windir").ok())
}

fn normalize_path(path: &str) -> String {
    let mut path = path.trim().trim_matches('"').replace('/', "\\");
    while path.ends_with('\\') {
        path.pop();
    }
    path.to_ascii_lowercase()
}

fn path_to_string(path: &Path) -> String {
    path.as_os_str().to_string_lossy().into_owned()
}

fn remove_file_or_schedule_delete(path: &Path) -> io::Result<()> {
    match fs::remove_file(path) {
        Ok(()) => Ok(()),
        Err(err) if err.kind() == io::ErrorKind::NotFound => Ok(()),
        Err(err) => schedule_delete_on_reboot(path)
            .map_err(|schedule_err| with_context(schedule_err, &format!("{err}"))),
    }
}

fn schedule_delete_on_reboot(path: &Path) -> io::Result<()> {
    let path = to_wide(path.as_os_str());
    let ok = unsafe {
        FileSystem::MoveFileExW(path.as_ptr(), null(), FileSystem::MOVEFILE_DELAY_UNTIL_REBOOT)
    };
    if ok == 0 { Err(io::Error::last_os_error()) } else { Ok(()) }
}

fn notify_environment_changed() {
    let environment = to_wide("Environment");
    unsafe {
        WindowsAndMessaging::SendMessageTimeoutW(
            WindowsAndMessaging::HWND_BROADCAST,
            WindowsAndMessaging::WM_SETTINGCHANGE,
            0,
            environment.as_ptr() as isize,
            WindowsAndMessaging::SMTO_ABORTIFHUNG,
            5000,
            null_mut(),
        );
    }
}

fn to_wide(s: impl AsRef<OsStr>) -> Vec<u16> {
    s.as_ref().encode_wide().chain(Some(0)).collect()
}

fn win32_result(res: u32) -> io::Result<()> {
    if res == ERROR_SUCCESS { Ok(()) } else { Err(io::Error::from_raw_os_error(res as i32)) }
}

fn with_context(err: io::Error, context: &str) -> io::Error {
    io::Error::new(err.kind(), format!("{context}: {err}"))
}

struct RegKey(Registry::HKEY);

impl RegKey {
    fn open(access: Registry::REG_SAM_FLAGS) -> io::Result<Self> {
        let mut key = null_mut();
        let res = unsafe {
            Registry::RegOpenKeyExW(
                Registry::HKEY_LOCAL_MACHINE,
                ENVIRONMENT_KEY,
                0,
                access,
                &mut key,
            )
        };
        win32_result(res).map(|()| Self(key))
    }

    fn raw(&self) -> Registry::HKEY {
        self.0
    }
}

impl Drop for RegKey {
    fn drop(&mut self) {
        unsafe {
            Registry::RegCloseKey(self.0);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn install_dir_is_inserted_before_system32() {
        let install_dir = Path::new(r"C:\Program Files\Microsoft\Edit");
        let path = r"C:\Windows\System32;C:\Windows;C:\Tools";

        assert_eq!(
            add_to_path_before_system32(path, install_dir),
            r"C:\Program Files\Microsoft\Edit;C:\Windows\System32;C:\Windows;C:\Tools"
        );
    }

    #[test]
    fn existing_install_dir_is_moved_before_system32() {
        let install_dir = Path::new(r"C:\Program Files\Microsoft\Edit");
        let path = r"C:\Windows\System32;C:\Program Files\Microsoft\Edit;C:\Windows";

        assert_eq!(
            add_to_path_before_system32(path, install_dir),
            r"C:\Program Files\Microsoft\Edit;C:\Windows\System32;C:\Windows"
        );
    }

    #[test]
    fn uninstall_removes_install_dir_from_path() {
        let install_dir = Path::new(r"C:\Program Files\Microsoft\Edit");
        let path = r"C:\Program Files\Microsoft\Edit;C:\Windows\System32;C:\Windows";

        assert_eq!(remove_from_path(path, install_dir), r"C:\Windows\System32;C:\Windows");
    }
}
