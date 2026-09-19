use std::fmt::Write as _;
use std::path::{Path, PathBuf};

use edit::buffer::TextBuffer;
use edit::cell::{Ref, SemiRefCell};
use edit::lsh::{LANGUAGES, Language};
use edit::{json, path as edit_path};
use stdext::arena::{read_to_string, scratch_arena};
use stdext::arena_format;

use crate::apperr;

pub struct Settings {
    pub path: PathBuf,
    pub file_associations: Vec<(String, &'static Language)>,
    pub recent_files: Vec<PathBuf>,
}

struct SettingsCell(SemiRefCell<Settings>);
unsafe impl Sync for SettingsCell {}
static SETTINGS: SettingsCell = SettingsCell(SemiRefCell::new(Settings::new()));

impl Settings {
    /// Fills the given settings.json text buffer with some initial contents for convenience.
    pub fn bootstrap(tb: &mut TextBuffer) {
        tb.set_crlf(false);
        let contents = Self::borrow().to_json();
        tb.write_raw(contents.as_bytes());
        tb.cursor_move_to_logical(Default::default());
        tb.mark_as_clean();
    }

    const fn new() -> Self {
        Settings { path: PathBuf::new(), file_associations: Vec::new(), recent_files: Vec::new() }
    }

    pub fn borrow() -> Ref<'static, Settings> {
        SETTINGS.0.borrow()
    }

    pub fn reload() -> apperr::Result<()> {
        let s = &mut *SETTINGS.0.borrow_mut();

        // Reset all members if we had been loaded previously.
        if !s.path.as_os_str().is_empty() {
            *s = Settings::new();
        }

        s.load()
    }

    fn load(&mut self) -> apperr::Result<()> {
        self.path = match settings_json_path() {
            Some(p) => p,
            None => return Ok(()),
        };

        let scratch = scratch_arena(None);
        let str = match read_to_string(&scratch, &self.path) {
            Err(err) if err.kind() == std::io::ErrorKind::NotFound => return Ok(()),
            Err(err) => return Err(err.into()),
            Ok(str) => str,
        };
        let Ok(json) = json::parse(&scratch, &str) else {
            return Err(apperr::Error::SettingsInvalid("Invalid JSON"));
        };
        let Some(root) = json.as_object() else {
            return Err(apperr::Error::SettingsInvalid("Non-object root"));
        };

        if let Some(f) = root.get_object("files.associations") {
            for &(mut key, ref value) in f.iter() {
                if !key.contains('/') {
                    key = arena_format!(&*scratch, "**/{key}").leak();
                }

                let Some(id) = value.as_str() else {
                    return Err(apperr::Error::SettingsInvalid("files.associations"));
                };
                let Some(language) = LANGUAGES.iter().find(|lang| lang.id == id) else {
                    return Err(apperr::Error::SettingsInvalid("language ID"));
                };

                self.file_associations.push((key.to_string(), language));
            }
        }

        // EN: Persist at most five unique absolute paths, newest first.
        // 中文：最近開啟檔案最多保存五筆不重複的絕對路徑，最新項目在前。
        if let Some(value) = root.get("files.recent") {
            let Some(values) = value.as_array() else {
                return Err(apperr::Error::SettingsInvalid("files.recent"));
            };
            for value in values.iter().take(5) {
                let Some(value) = value.as_str() else {
                    return Err(apperr::Error::SettingsInvalid("files.recent"));
                };
                let path = PathBuf::from(value);
                if !path.is_absolute() {
                    return Err(apperr::Error::SettingsInvalid("files.recent"));
                }
                let path = edit_path::normalize(&path);
                if !self.recent_files.contains(&path) {
                    self.recent_files.push(path);
                }
            }
        }

        Ok(())
    }

    /// EN: Moves a successfully opened file to the front of the recent-file list.
    /// 中文：將成功開啟的檔案移至最近檔案清單最前方。
    pub fn record_recent_file(path: &Path) -> apperr::Result<()> {
        let absolute = if path.is_absolute() {
            path.to_path_buf()
        } else {
            std::env::current_dir()?.join(path)
        };
        let absolute = edit_path::normalize(&absolute);
        let settings = &mut *SETTINGS.0.borrow_mut();
        settings.remember_recent_file(absolute);
        settings.save()
    }

    fn remember_recent_file(&mut self, path: PathBuf) {
        self.recent_files.retain(|recent| recent != &path);
        self.recent_files.insert(0, path);
        self.recent_files.truncate(5);
    }

    fn save(&self) -> apperr::Result<()> {
        if self.path.as_os_str().is_empty() {
            return Ok(());
        }
        if let Some(parent) = self.path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        std::fs::write(&self.path, self.to_json())?;
        Ok(())
    }

    fn to_json(&self) -> String {
        // EN: Serialize generated settings with LF on every supported platform.
        // 中文：自行序列化設定，確保所有支援平台皆固定使用 LF。
        let mut contents = String::from("{\n  \"files.associations\": {");
        for (index, (pattern, language)) in self.file_associations.iter().enumerate() {
            contents.push_str(if index == 0 { "\n    " } else { ",\n    " });
            write_json_string(&mut contents, pattern);
            contents.push_str(": ");
            write_json_string(&mut contents, language.id);
        }
        if !self.file_associations.is_empty() {
            contents.push_str("\n  ");
        }
        contents.push_str("},\n  \"files.recent\": [");
        for (index, path) in self.recent_files.iter().enumerate() {
            contents.push_str(if index == 0 { "\n    " } else { ",\n    " });
            write_json_string(&mut contents, &path.to_string_lossy());
        }
        if !self.recent_files.is_empty() {
            contents.push_str("\n  ");
        }
        contents.push_str("]\n}\n");
        contents
    }
}

fn write_json_string(output: &mut String, value: &str) {
    output.push('"');
    for ch in value.chars() {
        match ch {
            '"' => output.push_str("\\\""),
            '\\' => output.push_str("\\\\"),
            '\u{08}' => output.push_str("\\b"),
            '\u{0c}' => output.push_str("\\f"),
            '\n' => output.push_str("\\n"),
            '\r' => output.push_str("\\r"),
            '\t' => output.push_str("\\t"),
            '\0'..='\u{1f}' => _ = write!(output, "\\u{:04x}", ch as u32),
            _ => output.push(ch),
        }
    }
    output.push('"');
}

fn settings_json_path() -> Option<PathBuf> {
    let mut config_dir = config_dir()?;
    config_dir.push("settings.json");
    Some(config_dir)
}

fn config_dir() -> Option<PathBuf> {
    fn var_path(key: &str) -> Option<PathBuf> {
        std::env::var_os(key).map(PathBuf::from)
    }

    fn push(mut path: PathBuf, suffix: &str) -> PathBuf {
        path.push(suffix);
        path
    }

    #[cfg(target_os = "windows")]
    {
        var_path("APPDATA").map(|p| push(p, "Microsoft\\Edit"))
    }
    #[cfg(any(target_os = "macos", target_os = "ios"))]
    {
        var_path("HOME").map(|p| push(p, "Library/Application Support/com.microsoft.edit"))
    }
    #[cfg(not(any(target_os = "windows", target_os = "macos", target_os = "ios")))]
    {
        var_path("XDG_CONFIG_HOME")
            .or_else(|| var_path("HOME").map(|p| push(p, ".config")))
            .map(|p| push(p, "msedit"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn recent_files_keep_the_newest_unique_five_paths() {
        let mut settings = Settings::new();
        for name in ["one", "two", "three", "four", "five", "six", "four"] {
            settings.remember_recent_file(PathBuf::from(name));
        }
        assert_eq!(
            settings.recent_files,
            ["four", "six", "five", "three", "two"].map(PathBuf::from)
        );
    }

    #[test]
    fn recent_file_json_uses_lf() {
        let mut settings = Settings::new();
        settings.recent_files.push(PathBuf::from("C:/notes/readme.md"));
        let json = settings.to_json();
        assert!(json.contains("\"files.recent\""));
        assert!(!json.contains("\r\n"));
    }
}
