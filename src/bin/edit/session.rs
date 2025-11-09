// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::{fs, io};

use serde::{Deserialize, Serialize};

use crate::state;

const SESSION_FILE_NAME: &str = "session.json";
pub(crate) const SESSION_VERSION: u32 = 1;

#[derive(Default, Serialize, Deserialize)]
pub struct SessionFile {
    pub version: u32,
    #[serde(default)]
    pub open_documents: Vec<SessionDocument>,
    #[serde(default)]
    pub recent_files: Vec<String>,
}

#[derive(Clone, Serialize, Deserialize)]
pub struct SessionDocument {
    pub path: String,
    pub line: i64,
    pub column: i64,
}

pub fn load() -> Option<SessionFile> {
    let mut path = state::config_dir()?;
    path.push(SESSION_FILE_NAME);

    let text = fs::read_to_string(path).ok()?;
    let session: SessionFile = serde_json::from_str(&text).ok()?;
    if session.version != SESSION_VERSION {
        return None;
    }
    Some(session)
}

pub fn save(data: &SessionFile) -> io::Result<()> {
    let mut dir = match state::config_dir() {
        Some(dir) => dir,
        None => return Ok(()),
    };
    fs::create_dir_all(&dir)?;

    dir.push(SESSION_FILE_NAME);
    let text = serde_json::to_string_pretty(data)?;
    fs::write(dir, text)
}
