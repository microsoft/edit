// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::collections::HashSet;

use edit::arena::scratch_arena;
use edit::helpers::AsciiStringHelpers;
use edit::sys;

include!(concat!(env!("OUT_DIR"), "/i18n_edit.rs"));

static mut S_LANG: LangId = LangId::en;
static mut DEFAULT_LANG: LangId = LangId::en;

pub fn init() {
    let scratch = scratch_arena(None);
    let langs = sys::preferred_languages(&scratch);
    let mut lang = LangId::en;

    'outer: for l in langs {
        for (prefix, id) in LANGUAGES {
            if l.starts_with_ignore_ascii_case(prefix) {
                lang = *id;
                break 'outer;
            }
        }
    }

    unsafe {
        S_LANG = lang;
        DEFAULT_LANG = lang;
    }
}

pub fn loc(id: LocId) -> &'static str {
    TRANSLATIONS[unsafe { S_LANG as usize }][id as usize]
}

pub fn unique_languages() -> Vec<(&'static str, LangId)> {
    let mut seen = HashSet::new();
    let mut result = Vec::new();
    for &(tag, id) in LANGUAGES {
        if seen.insert(id as u32) {
            result.push((tag, id));
        }
    }
    result
}

pub fn reset_language() {
    unsafe {
        S_LANG = DEFAULT_LANG;
    }
}

pub fn set_language_tag(tag: &str) -> Option<LangId> {
    let normalized = normalize_lang_tag(tag);
    for &(lang_tag, id) in LANGUAGES {
        if normalize_lang_tag(lang_tag) == normalized {
            unsafe { S_LANG = id };
            return Some(id);
        }
    }
    None
}

pub fn language_display_name(tag: &str) -> &'static str {
    let normalized = normalize_lang_tag(tag);
    match normalized.as_str() {
        "en" => "English",
        "de" => "Deutsch",
        "es" => "Español",
        "fr" => "Français",
        "it" => "Italiano",
        "ja" => "日本語",
        "ko" => "한국어",
        "pl" => "Polski",
        "pt_br" => "Português (Brasil)",
        "ro" => "Română",
        "ru" => "Русский",
        "zh_hans" => "中文（简体）",
        "zh_hant" => "中文（繁體）",
        _ => "Unknown",
    }
}

fn normalize_lang_tag(tag: &str) -> String {
    tag.chars()
        .map(|c| match c {
            'A'..='Z' => c.to_ascii_lowercase(),
            '-' => '_',
            _ => c,
        })
        .collect()
}
