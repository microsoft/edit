use std::path::PathBuf;

use edit::buffer::TextBuffer;
use edit::cell::{Ref, SemiRefCell};
use edit::json;
use edit::lsh::{LANGUAGES, Language};
use edit::oklab::StraightRgba;
use stdext::arena::{read_to_string, scratch_arena};
use stdext::arena_format;

use crate::apperr;

/// Theme configuration with user-defined colors.
/// All colors are in RRGGBBAA format (same as framebuffer).
#[derive(Clone, Copy, Debug)]
pub struct ThemeConfig {
    // Standard 16 terminal colors
    pub black: Option<StraightRgba>,
    pub red: Option<StraightRgba>,
    pub green: Option<StraightRgba>,
    pub yellow: Option<StraightRgba>,
    pub blue: Option<StraightRgba>,
    pub magenta: Option<StraightRgba>,
    pub cyan: Option<StraightRgba>,
    pub white: Option<StraightRgba>,
    pub bright_black: Option<StraightRgba>,
    pub bright_red: Option<StraightRgba>,
    pub bright_green: Option<StraightRgba>,
    pub bright_yellow: Option<StraightRgba>,
    pub bright_blue: Option<StraightRgba>,
    pub bright_magenta: Option<StraightRgba>,
    pub bright_cyan: Option<StraightRgba>,
    pub bright_white: Option<StraightRgba>,
    // Special colors
    pub background: Option<StraightRgba>,
    pub foreground: Option<StraightRgba>,
    // UI colors
    pub line_number: Option<StraightRgba>,
    pub line_highlight: Option<StraightRgba>,
}

impl ThemeConfig {
    /// Create an empty theme config (all colors are None)
    pub const fn empty() -> Self {
        Self {
            black: None,
            red: None,
            green: None,
            yellow: None,
            blue: None,
            magenta: None,
            cyan: None,
            white: None,
            bright_black: None,
            bright_red: None,
            bright_green: None,
            bright_yellow: None,
            bright_blue: None,
            bright_magenta: None,
            bright_cyan: None,
            bright_white: None,
            background: None,
            foreground: None,
            line_number: None,
            line_highlight: None,
        }
    }

}

pub struct Settings {
    pub path: PathBuf,
    pub file_associations: Vec<(String, &'static Language)>,
    pub theme: ThemeConfig,
}

struct SettingsCell(SemiRefCell<Settings>);
unsafe impl Sync for SettingsCell {}
static SETTINGS: SettingsCell = SettingsCell(SemiRefCell::new(Settings::new()));

impl Settings {
    /// Fills the given settings.json text buffer with some initial contents for convenience.
    pub fn bootstrap(tb: &mut TextBuffer) {
        tb.set_crlf(false);
        tb.write_raw(b"{\n}\n");
        tb.cursor_move_to_logical(Default::default());
        tb.mark_as_clean();
    }

    const fn new() -> Self {
        Settings {
            path: PathBuf::new(),
            file_associations: Vec::new(),
            theme: ThemeConfig::empty(),
        }
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

        // Parse theme configuration
        if let Some(theme_obj) = root.get_object("theme") {
            self.theme = parse_theme_config(theme_obj)?;
        }

        Ok(())
    }
}

/// Parse a hex color string (with or without # prefix) into StraightRgba
/// Supports: RRGGBB, #RRGGBB, RRGGBBAA, #RRGGBBAA
/// Format matches the framebuffer's DEFAULT_THEME (RRGGBBAA in big-endian)
fn parse_hex_color(s: &str) -> Option<StraightRgba> {
    let s = s.trim();
    let s = s.strip_prefix('#').unwrap_or(s);

    let val = u32::from_str_radix(s, 16).ok()?;

    // Convert to RRGGBBAA format (same as DEFAULT_THEME in framebuffer.rs)
    let rgba = match s.len() {
        6 => StraightRgba::from_be(val << 8 | 0xFF), // RRGGBB -> RRGGBBFF
        8 => StraightRgba::from_be(val),             // RRGGBBAA
        _ => return None,
    };

    Some(rgba)
}

/// Parse theme configuration from JSON object
fn parse_theme_config(obj: edit::json::Object) -> apperr::Result<ThemeConfig> {
    let mut theme = ThemeConfig::empty();

    for &(key, ref value) in obj.iter() {
        let Some(color_str) = value.as_str() else {
            return Err(apperr::Error::SettingsInvalid("theme color value must be a string"));
        };

        let Some(color) = parse_hex_color(color_str) else {
            return Err(apperr::Error::SettingsInvalid("invalid color format"));
        };

        match key {
            "black" => theme.black = Some(color),
            "red" => theme.red = Some(color),
            "green" => theme.green = Some(color),
            "yellow" => theme.yellow = Some(color),
            "blue" => theme.blue = Some(color),
            "magenta" => theme.magenta = Some(color),
            "cyan" => theme.cyan = Some(color),
            "white" => theme.white = Some(color),
            "brightBlack" | "bright_black" => theme.bright_black = Some(color),
            "brightRed" | "bright_red" => theme.bright_red = Some(color),
            "brightGreen" | "bright_green" => theme.bright_green = Some(color),
            "brightYellow" | "bright_yellow" => theme.bright_yellow = Some(color),
            "brightBlue" | "bright_blue" => theme.bright_blue = Some(color),
            "brightMagenta" | "bright_magenta" => theme.bright_magenta = Some(color),
            "brightCyan" | "bright_cyan" => theme.bright_cyan = Some(color),
            "brightWhite" | "bright_white" => theme.bright_white = Some(color),
            "background" | "bg" => theme.background = Some(color),
            "foreground" | "fg" => theme.foreground = Some(color),
            "lineNumber" | "line_number" => theme.line_number = Some(color),
            "lineHighlight" | "line_highlight" => theme.line_highlight = Some(color),
            _ => {} // Unknown theme key, ignore
        }
    }

    Ok(theme)
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
