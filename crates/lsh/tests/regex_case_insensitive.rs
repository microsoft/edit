// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! Regression test for the `(?i:...)` literal bug.
//!
//! `inlined_memicmp` (in `runtime.rs`) requires the needle for
//! `Condition::PrefixInsensitive` to be ASCII-lowercase: it lowercases each
//! haystack byte and compares it raw against the needle. Before the fix in
//! `emit_literal`, the literal was interned verbatim, so any uppercase byte
//! made the prefix check mismatch unconditionally and the `if` branch
//! silently never fired.

use lsh::compiler::Compiler;
use lsh::runtime::Runtime;
use stdext::arena::{self, Arena, scratch_arena};

fn ensure_scratch() {
    use std::sync::Once;
    static ONCE: Once = Once::new();
    ONCE.call_once(|| {
        arena::init(1 << 20).unwrap();
    });
}

fn compile_and_run(src: &str, line: &[u8]) -> (Vec<(usize, u32)>, Vec<String>) {
    ensure_scratch();
    let arena = Arena::new(1 << 20).unwrap();
    let mut compiler = Compiler::new(&arena);
    compiler.parse("test.lsh", src).unwrap();
    let assembly = compiler.assemble().unwrap();

    let charsets: Vec<[u16; 16]> = assembly.charsets.iter().map(|c| c.serialize()).collect();
    let kind_names: Vec<String> = {
        let max = assembly.highlight_kinds.iter().map(|hk| hk.value).max().unwrap_or(0);
        let mut names = vec![String::new(); max as usize + 1];
        for hk in &assembly.highlight_kinds {
            names[hk.value as usize] = hk.identifier.to_string();
        }
        names
    };
    let entry = assembly.entrypoints[0].address as u32;
    let mut runtime =
        Runtime::new(&assembly.instructions, &assembly.strings, &charsets, entry);

    let scratch = scratch_arena(None);
    let highlights = runtime.parse_next_line::<u32>(&scratch, line);
    let pairs = highlights.iter().map(|h| (h.start, h.kind)).collect();
    (pairs, kind_names)
}

fn assert_kind_present(src: &str, line: &[u8], expected_kind: &str) {
    let (highlights, kind_names) = compile_and_run(src, line);
    let present: Vec<&str> = highlights
        .iter()
        .filter_map(|(_, k)| kind_names.get(*k as usize).map(String::as_str))
        .collect();
    assert!(
        present.contains(&expected_kind),
        "expected `{expected_kind}` span on input {:?}, got kinds: {:?} (raw: {:?})",
        std::str::from_utf8(line).unwrap_or("<non-utf8>"),
        present,
        highlights,
    );
}

const GRAMMAR: &str = r#"
#[display_name = "Test"]
#[path = "**/test.txt"]
pub fn test() {
    if /(?i:FOO)\>/ {
        yield keyword.control;
    }
}
"#;

#[test]
fn case_insensitive_uppercase_literal_matches_uppercase_input() {
    assert_kind_present(GRAMMAR, b"FOO", "keyword.control");
}

#[test]
fn case_insensitive_uppercase_literal_matches_lowercase_input() {
    assert_kind_present(GRAMMAR, b"foo", "keyword.control");
}

#[test]
fn case_insensitive_alternation_with_uppercase_alternatives() {
    let src = r#"
#[display_name = "Test"]
#[path = "**/test.txt"]
pub fn test() {
    if /(?i:FROM|RUN|CMD)\>/ {
        yield keyword.control;
    }
}
"#;
    assert_kind_present(src, b"FROM", "keyword.control");
    assert_kind_present(src, b"RUN", "keyword.control");
    assert_kind_present(src, b"cmd", "keyword.control");
}
