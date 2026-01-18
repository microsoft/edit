use std::fs::read_to_string;
use std::path::PathBuf;

use edit::cell::{Ref, SemiRefCell};
use edit::lsh::{LANGUAGES, Language};
use edit::{apperr, json};
use stdext::arena::scratch_arena;
use stdext::arena_format;

#[derive(Default)]
pub struct Settings {
    pub file_associations: Vec<(String, &'static Language)>,
}

#[derive(Default)]
struct SettingsCell(SemiRefCell<Settings>);

unsafe impl Sync for SettingsCell {}

static SETTINGS: SettingsCell =
    SettingsCell(SemiRefCell::new(Settings { file_associations: Vec::new() }));

impl Settings {
    pub fn borrow() -> Ref<'static, Settings> {
        SETTINGS.0.borrow()
    }

    pub fn reload() -> apperr::Result<()> {
        let s = Self::load()?;
        *SETTINGS.0.borrow_mut() = s;
        Ok(())
    }

    fn load() -> apperr::Result<Self> {
        let mut settings = Self { file_associations: Vec::new() };

        let Some(mut config_dir) = config_dir() else {
            return Ok(settings);
        };

        config_dir.push("settings.json");

        let str = match read_to_string(config_dir) {
            Err(err) if err.kind() == std::io::ErrorKind::NotFound => return Ok(settings),
            Err(err) => return Err(err.into()),
            Ok(str) => str,
        };

        let scratch = scratch_arena(None);
        let json: json::Value = json::parse(&scratch, &str).unwrap(); // TODO
        let root: json::Object = json.as_object().unwrap();

        if let Some(f) = root.get_object("fileAssociations") {
            for item in f.iter() {
                let mut pattern = item.key;
                if !pattern.contains('/') {
                    pattern = arena_format!(&scratch, "**/{pattern}").leak();
                }

                let id = item.value.as_str().unwrap(); // TODO
                let language = LANGUAGES.iter().find(|lang| lang.id == id).unwrap(); // TODO

                settings.file_associations.push((pattern.to_string(), language));
            }
        }

        Ok(settings)
    }
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
        var_path("APPDATA").map(|p| push(p, "Microsoft/Edit"))
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
