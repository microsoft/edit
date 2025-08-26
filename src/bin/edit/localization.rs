// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use edit::arena::scratch_arena;
use edit::helpers::AsciiStringHelpers;
use edit::sys;

include!(concat!(env!("OUT_DIR"), "/i18n_edit.rs"));

static mut S_LANG: LangId = LangId::en;

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
    }
}

pub fn loc(id: LocId) -> &'static str {
    TRANSLATIONS[unsafe { S_LANG as usize }][id as usize]
}
