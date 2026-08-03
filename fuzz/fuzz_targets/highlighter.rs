#![no_main]

use edit::lsh::{Highlighter, LANGUAGES};
use libfuzzer_sys::fuzz_target;
use stdext::arena::scratch_arena;

fuzz_target!(|data: &[u8]| {
    if data.is_empty() {
        return;
    }

    for lang in LANGUAGES {
        let mut highlighter = Highlighter::new(&data, lang);

        loop {
            let arena = scratch_arena(None);
            let highlights = highlighter.parse_next_line(&arena);
            if highlights.is_empty() {
                break;
            }
        }
    }
});
