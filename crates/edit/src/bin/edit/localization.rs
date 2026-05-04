// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use edit::sys;
use stdext::AsciiStringHelpers as _;
use stdext::arena::scratch_arena;

include!(concat!(env!("OUT_DIR"), "/i18n_edit.rs"));

static mut S_LANG: LangId = LangId::en;

pub fn init() {
    let scratch = scratch_arena(None);
    let langs = sys::preferred_languages(&scratch);
    let lang = select_language(langs);

    unsafe {
        S_LANG = lang;
    }
}

fn select_language<'a>(langs: impl IntoIterator<Item = &'a str>) -> LangId {
    let mut lang = LangId::en;

    'outer: for l in langs {
        for (prefix, id) in LANGUAGES {
            if l.starts_with_ignore_ascii_case(prefix) {
                lang = *id;
                break 'outer;
            }
        }
    }

    lang
}

pub fn loc(id: LocId) -> &'static str {
    TRANSLATIONS[unsafe { S_LANG as usize }][id as usize]
}

#[cfg(test)]
mod tests {
    use super::*;

    // Regression test for https://github.com/microsoft/edit/issues/832.
    #[test]
    fn chinese_region_aliases_select_expected_script() {
        for (actual, expected) in [
            ("zh-CN.UTF-8", "zh-hans"),
            ("zh-SG.UTF-8", "zh-hans"),
            ("zh-TW.UTF-8", "zh-hant"),
            ("zh-HK.UTF-8", "zh-hant"),
            ("zh-MO.UTF-8", "zh-hant"),
            ("zh-Hant.UTF-8", "zh-hant"),
            ("zh", "zh-hans"),
        ] {
            let Some(&(_, expected)) = LANGUAGES.iter().find(|(l, _)| expected == *l) else {
                continue; // Disabled by EDIT_CFG_LANGUAGES
            };

            assert!(select_language(std::iter::once(actual)) == expected);
        }
    }
}
