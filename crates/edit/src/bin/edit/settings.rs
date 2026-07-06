use std::path::PathBuf;

use edit::buffer::TextBuffer;
use edit::cell::{Ref, SemiRefCell};
use edit::framebuffer::{
    CATPPUCCIN_FRAPPE, CATPPUCCIN_LATTE, CATPPUCCIN_MACCHIATO, CATPPUCCIN_MOCHA,
    INDEXED_COLORS_COUNT,
};
use edit::json;
use edit::lsh::{LANGUAGES, Language};
use edit::oklab::StraightRgba;
use stdext::arena::{read_to_string, scratch_arena};
use stdext::arena_format;

use crate::apperr;

/// The color theme to use for the editor.
#[derive(Clone, Copy, PartialEq, Eq, Default)]
pub enum Theme {
    /// Follow the terminal's own color palette (queried via OSC). The default.
    #[default]
    System,
    CatppuccinLatte,
    CatppuccinFrappe,
    CatppuccinMacchiato,
    CatppuccinMocha,
}

impl Theme {
    fn from_id(id: &str) -> Option<Self> {
        Some(match id {
            "system" => Theme::System,
            "catppuccin-latte" => Theme::CatppuccinLatte,
            "catppuccin-frappe" => Theme::CatppuccinFrappe,
            "catppuccin-macchiato" => Theme::CatppuccinMacchiato,
            "catppuccin-mocha" => Theme::CatppuccinMocha,
            _ => return None,
        })
    }

    /// The fixed palette for this theme, or `None` when the terminal's
    /// own palette should be used ([`Theme::System`]).
    pub fn palette(self) -> Option<[StraightRgba; INDEXED_COLORS_COUNT]> {
        Some(match self {
            Theme::System => return None,
            Theme::CatppuccinLatte => CATPPUCCIN_LATTE,
            Theme::CatppuccinFrappe => CATPPUCCIN_FRAPPE,
            Theme::CatppuccinMacchiato => CATPPUCCIN_MACCHIATO,
            Theme::CatppuccinMocha => CATPPUCCIN_MOCHA,
        })
    }
}

pub struct Settings {
    pub path: PathBuf,
    pub file_associations: Vec<(String, &'static Language)>,
    pub theme: Theme,
}

struct SettingsCell(SemiRefCell<Settings>);
unsafe impl Sync for SettingsCell {}
static SETTINGS: SettingsCell = SettingsCell(SemiRefCell::new(Settings::new()));

impl Settings {
    /// Fills the given settings.json text buffer with some initial contents for convenience.
    pub fn bootstrap(tb: &mut TextBuffer) {
        tb.set_crlf(false);
        tb.write_raw(
            concat!(
                "{\n",
                "    // Color theme. One of: \"system\" (follow the terminal palette),\n",
                "    // \"catppuccin-latte\", \"catppuccin-frappe\", \"catppuccin-macchiato\",\n",
                "    // \"catppuccin-mocha\".\n",
                "    // \"theme\": \"system\",\n",
                "\n",
                "    // Maps file name globs to language IDs for syntax highlighting.\n",
                "    // The default is empty (associations are inferred automatically).\n",
                "    // \"files.associations\": {\n",
                "    //     \"*.txt\": \"plaintext\"\n",
                "    // }\n",
                "}\n",
            )
            .as_bytes(),
        );
        tb.cursor_move_to_logical(Default::default());
        tb.mark_as_clean();
    }

    const fn new() -> Self {
        Settings { path: PathBuf::new(), file_associations: Vec::new(), theme: Theme::System }
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

        if let Some(value) = root.get("theme") {
            let Some(id) = value.as_str() else {
                return Err(apperr::Error::SettingsInvalid("theme"));
            };
            let Some(theme) = Theme::from_id(id) else {
                return Err(apperr::Error::SettingsInvalid("theme"));
            };
            self.theme = theme;
        }

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

        Ok(())
    }
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
