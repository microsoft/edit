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

    fn assert_lang(langs: &[&str], expected: LangId) {
        assert!(select_language(langs.iter().copied()) == expected);
    }

    // Regression test for https://github.com/microsoft/edit/issues/832.
    #[test]
    fn chinese_region_aliases_select_expected_script() {
        assert_lang(&["zh-CN.UTF-8"], LangId::zh_hans);
        assert_lang(&["zh-SG.UTF-8"], LangId::zh_hans);
        assert_lang(&["zh-TW.UTF-8"], LangId::zh_hant);
        assert_lang(&["zh-HK.UTF-8"], LangId::zh_hant);
        assert_lang(&["zh-MO.UTF-8"], LangId::zh_hant);
        assert_lang(&["zh-Hant.UTF-8"], LangId::zh_hant);
        assert_lang(&["zh"], LangId::zh_hans);
    }
}
