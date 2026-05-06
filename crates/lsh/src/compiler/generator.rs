// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! Code generators for different output formats.
//!
//! ## TODO
//!
//! - The label lookup (with a `HashMap`) is $NOT_GREAT.
//!   The backend should emit metadata, I think.

use std::collections::HashMap;
use std::ffi::OsStr;
use std::fs::read_dir;
use std::io;
use std::path::PathBuf;

use stdext::arena::scratch_arena;

use super::*;
use crate::runtime::{Instruction, MnemonicFormattingConfig};

const JS_RUNTIME: &str = include_str!("runtime.js");

pub struct Generator<'a> {
    compiler: Compiler<'a>,
}

impl<'a> Generator<'a> {
    pub fn new(arena: &'a Arena) -> Self {
        Self { compiler: Compiler::new(arena) }
    }

    pub fn read_file(&mut self, path: &Path) -> CompileResult<()> {
        let path_str = path.display().to_string();
        match std::fs::read_to_string(path) {
            Ok(src) => self.compiler.parse(&path_str, &src),
            Err(e) => {
                Err(CompileError { path: path_str, line: 0, column: 0, message: e.to_string() })
            }
        }
    }

    pub fn read_directory(&mut self, path: &Path) -> CompileResult<()> {
        let files = Self::read_dir_to_vec(path).map_err(|e| CompileError {
            path: path.display().to_string(),
            line: 0,
            column: 0,
            message: e.to_string(),
        })?;

        for path in files {
            if path.extension() == Some(OsStr::new("lsh")) {
                self.read_file(&path)?;
            }
        }
        Ok(())
    }

    fn read_dir_to_vec(path: &Path) -> io::Result<Vec<PathBuf>> {
        let mut paths = Vec::new();

        for entry in read_dir(path)? {
            let entry = entry?;
            if entry.metadata().is_ok_and(|f| f.is_file())
                && entry.file_name().as_encoded_bytes().ends_with(b".lsh")
            {
                paths.push(entry.path());
            }
        }

        paths.sort_unstable();
        Ok(paths)
    }

    pub fn assemble(mut self) -> CompileResult<Assembly<'a>> {
        self.compiler.assemble()
    }

    pub fn generate_assembly(mut self, vt: bool) -> CompileResult<String> {
        let mut output = String::new();
        let assembly = self.compiler.assemble()?;
        let line_num_width = assembly.instructions.len().checked_ilog10().unwrap_or(0) as usize + 1;
        let mnemonic_config = if vt {
            MnemonicFormattingConfig {
                instruction_prefix: "\x1b[33m", // yellow
                instruction_suffix: "\x1b[39m", // default

                register_prefix: "\x1b[32m", // green
                register_suffix: "\x1b[39m",

                address_prefix: "\x1b[90m", // bright black
                address_suffix: "\x1b[39m",

                numeric_prefix: "\x1b[36m", // cyan
                numeric_suffix: "\x1b[39m",
            }
        } else {
            Default::default()
        };
        let label_prefix = if vt { "\x1b[4;94m" } else { "" }; // underlined & bright blue
        let label_suffix = if vt { "\x1b[m" } else { "" };
        let line_prefix = if vt { "\x1b[90m" } else { "" }; // bright black
        let comment_prefix = if vt { "\x1b[32m" } else { "" }; // green
        let comment_suffix = if vt { "\x1b[39m" } else { "" };

        // TODO: This is kind of stupid. There should be per-instruction annotations.
        let labels: HashMap<usize, &str> =
            assembly.entrypoints.iter().map(|ep| (ep.address, ep.name.as_str())).collect();

        let mut off = 0;
        while off < assembly.instructions.len() {
            if let Some(label) = labels.get(&off) {
                if off != 0 {
                    output.push('\n');
                }
                _ = writeln!(output, "{label_prefix}{}:{label_suffix}", label);
            }

            let (Some(instr), len) = Instruction::decode(&assembly.instructions[off..]) else {
                break;
            };

            let scratch = scratch_arena(None);
            let mnemonic = instr.mnemonic(&scratch, &mnemonic_config);
            _ = write!(output, "{line_prefix}{off:>line_num_width$}:  {mnemonic}");

            let text_chars = {
                let mut count = 0;
                let mut in_escape = false;
                for c in mnemonic.bytes() {
                    if in_escape {
                        if c.is_ascii_alphabetic() {
                            in_escape = false;
                        }
                    } else if c == b'\x1b' {
                        in_escape = true;
                    } else {
                        count += 1;
                    }
                }
                count
            };
            let padding_width = 40usize.saturating_sub(text_chars);
            _ = write!(output, "{:<padding_width$}", "");

            match instr {
                Instruction::JumpIfMatchCharset { idx, .. } => {
                    _ = write!(
                        output,
                        " {comment_prefix}// {:?}{comment_suffix}",
                        assembly.charsets[idx as usize]
                    )
                }
                Instruction::JumpIfMatchPrefix { idx, .. }
                | Instruction::JumpIfMatchPrefixInsensitive { idx, .. } => {
                    _ = write!(
                        output,
                        " {comment_prefix}// {:?}{comment_suffix}",
                        assembly.strings[idx as usize]
                    )
                }
                _ => {}
            }

            output.push('\n');
            off += len;
        }

        Ok(output)
    }

    pub fn generate_rust(mut self) -> CompileResult<String> {
        let assembly = self.compiler.assemble()?;

        let mut output = String::new();
        output.push_str("// This file is auto-generated. Do not edit it manually.\n\n");
        output.push_str("use lsh::runtime::Language;\n\n");

        output.push_str("#[derive(Debug, Clone, Copy, PartialEq, Eq)]\npub enum HighlightKind {\n");
        let members: Vec<_> = assembly
            .highlight_kinds
            .iter()
            .map(|hk| (hk, format!("{} = {},", hk.fmt_camelcase(), hk.value)))
            .collect();
        let width = members.iter().map(|s| s.1.len()).max().unwrap_or(0);
        for (hk, member) in members {
            _ = writeln!(output, "    {member:<width$} // {}", hk.identifier);
        }
        output.push_str("}\n");

        if let Some(last) = assembly.highlight_kinds.last() {
            _ = write!(
                output,
                "
impl TryFrom<u32> for HighlightKind {{
    type Error = ();

    #[inline]
    fn try_from(value: u32) -> Result<Self, Self::Error> {{
        if value <= Self::{} as u32 {{
            Ok(unsafe {{ std::mem::transmute::<u8, Self>(value as u8) }})
        }} else {{
            Err(())
        }}
    }}
}}
",
                last.fmt_camelcase()
            );
        }

        output.push_str("/*\n");
        output.push_str(&self.compiler.as_mermaid());
        output.push_str("*/\n");

        output.push_str("\n#[rustfmt::skip] pub const LANGUAGES: &[Language] = &[\n");
        for ep in &assembly.entrypoints {
            _ = writeln!(
                output,
                "    Language {{ id: {:?}, name: {:?}, entrypoint: {} }},",
                ep.name.replace('_', "-"),
                ep.display_name,
                ep.address,
            );
        }
        output.push_str("];\n");

        output.push_str(
            "\n#[rustfmt::skip] pub const FILE_ASSOCIATIONS: &[(&str, &Language)] = &[\n",
        );
        for (idx, ep) in assembly.entrypoints.iter().enumerate() {
            for path in &ep.paths {
                _ = writeln!(output, "    ({path:?}, &LANGUAGES[{idx}]),");
            }
        }
        output.push_str("];\n");

        _ = writeln!(
            output,
            "\n#[rustfmt::skip] pub const ASSEMBLY: [u8; {len}] = [",
            len = assembly.instructions.len() + Instruction::MAX_ENCODED_SIZE,
        );
        let line_num_width = assembly.instructions.len().checked_ilog10().unwrap_or(0) as usize + 1;

        // TODO: This is kind of stupid. There should be per-instruction annotations.
        let labels: HashMap<usize, &str> =
            assembly.entrypoints.iter().map(|ep| (ep.address, ep.name.as_str())).collect();

        let mut off = 0;
        while off < assembly.instructions.len() {
            if let Some(label) = labels.get(&off) {
                if off != 0 {
                    output.push('\n');
                }
                _ = writeln!(output, "    // {}:", label);
            }

            output.push_str("    ");

            let (instr, len) = Instruction::decode(&assembly.instructions[off..]);
            let scratch = scratch_arena(None);
            for i in 0..len {
                _ = write!(output, "0x{:02x}, ", assembly.instructions[off + i]);
            }

            if let Some(instr) = instr {
                _ = writeln!(
                    output,
                    "{:<padding_width$}// {off:>line_num_width$}:  {mnemonic}",
                    "",
                    padding_width = Instruction::MAX_ENCODED_SIZE.saturating_sub(len) * 6,
                    mnemonic = instr.mnemonic(&scratch, &Default::default())
                );
            } else {
                output.push('\n');
            }

            off += len;
        }
        // Normally the runtime would need to do bounds checks at all times to be safe,
        // since there may be malformed bytecode (e.g. a bug in this compiler).
        // We can fix that by padding the instruction stream with invalid opcodes at the end.
        // This works as long as the runtime checks for valid opcodes. Even if the last valid
        // opcode is chopped off (due to a bug above), the runtime can do an unchecked read
        // of `MAX_ENCODED_SIZE` bytes without risking OOB access.
        output.push_str("\n    // padding\n");
        for _ in 0..Instruction::MAX_ENCODED_SIZE {
            _ = writeln!(output, "    0xff,");
        }
        output.push_str("];\n");

        _ = writeln!(
            output,
            "\n#[rustfmt::skip] pub const CHARSETS: [[u16; 16]; {len}] = [",
            len = assembly.charsets.len(),
        );
        for cs in assembly.charsets {
            let cs = cs.serialize();
            output.push_str("    [");
            for (i, &v) in cs.iter().enumerate() {
                if i != 0 {
                    _ = write!(output, ", ");
                }
                _ = write!(output, "0x{v:04x}");
            }
            output.push_str("],\n");
        }
        output.push_str("];\n");

        _ = writeln!(
            output,
            "\n#[rustfmt::skip] pub const STRINGS: [&str; {len}] = [",
            len = assembly.strings.len(),
        );
        for s in assembly.strings {
            _ = writeln!(output, "    {s:?},");
        }
        output.push_str("];\n");

        Ok(output)
    }

    pub fn generate_js(mut self) -> CompileResult<String> {
        let assembly = self.compiler.assemble()?;

        fn push_js_string(output: &mut String, value: &str) {
            output.push('"');
            let mut prev = '\0';
            for c in value.chars() {
                match c {
                    '\\' => output.push_str("\\\\"),
                    '"' => output.push_str("\\\""),
                    '\n' => output.push_str("\\n"),
                    '\r' => output.push_str("\\r"),
                    '\t' => output.push_str("\\t"),
                    '\u{08}' => output.push_str("\\b"),
                    '\u{0c}' => output.push_str("\\f"),
                    '\u{2028}' => output.push_str("\\u2028"),
                    '\u{2029}' => output.push_str("\\u2029"),
                    '/' if prev == '<' => output.push_str("\\/"),
                    c if c.is_control() => _ = write!(output, "\\u{:04x}", c as u32),
                    c => output.push(c),
                }
                prev = c;
            }
            output.push('"');
        }

        fn push_js_string_array(output: &mut String, values: &[&str], indent: &str) {
            for value in values {
                output.push_str(indent);
                push_js_string(output, value);
                output.push_str(",\n");
            }
        }

        fn push_js_u8_array(output: &mut String, values: &[u8], indent: &str) {
            const VALUES_PER_LINE: usize = 16;

            for chunk in values.chunks(VALUES_PER_LINE) {
                output.push_str(indent);
                for value in chunk {
                    _ = write!(output, "0x{value:02x}, ");
                }
                output.push('\n');
            }
        }

        let mut output = String::new();
        output.push_str("// This file is auto-generated. Do not edit it manually.\n");
        output.push_str("const LSH = (() => {\n");
        output.push_str(JS_RUNTIME);
        output.push('\n');

        output.push_str("    const LANGUAGES = [\n");
        for ep in &assembly.entrypoints {
            output.push_str("        { id: ");
            push_js_string(&mut output, &ep.name.replace('_', "-"));
            output.push_str(", name: ");
            push_js_string(&mut output, &ep.display_name);
            _ = writeln!(output, ", entrypoint: {} }},", ep.address);
        }
        output.push_str("    ];\n\n");

        output.push_str("    const FILE_ASSOCIATIONS = [\n");
        for (idx, ep) in assembly.entrypoints.iter().enumerate() {
            for path in &ep.paths {
                output.push_str("        [");
                push_js_string(&mut output, path);
                _ = writeln!(output, ", LANGUAGES[{idx}]],");
            }
        }
        output.push_str("    ];\n\n");

        output.push_str("    const HIGHLIGHT_KINDS = [\n");
        for hk in &assembly.highlight_kinds {
            output.push_str("        { identifier: ");
            push_js_string(&mut output, hk.identifier);
            _ = writeln!(output, ", value: {} }},", hk.value);
        }
        output.push_str("    ];\n\n");

        output.push_str("    const ASSEMBLY = new Uint8Array([\n");
        push_js_u8_array(&mut output, &assembly.instructions, "        ");
        output.push_str("\n        // padding\n");
        for _ in 0..Instruction::MAX_ENCODED_SIZE {
            output.push_str("        0xff,\n");
        }
        output.push_str("    ]);\n\n");

        output.push_str("    const CHARSETS = [\n");
        for cs in assembly.charsets {
            let cs = cs.serialize();
            output.push_str("        [");
            for (i, &v) in cs.iter().enumerate() {
                if i != 0 {
                    output.push_str(", ");
                }
                _ = write!(output, "0x{v:04x}");
            }
            output.push_str("],\n");
        }
        output.push_str("    ];\n\n");

        output.push_str("    const STRINGS = [\n");
        push_js_string_array(&mut output, &assembly.strings, "        ");
        output.push_str("    ].map(encodeString);\n\n");

        output.push_str(
            r#"    return {
        Runtime,
        LANGUAGES,
        FILE_ASSOCIATIONS,
        HIGHLIGHT_KINDS,
        ASSEMBLY,
        CHARSETS,
        STRINGS,
    };
})();
"#,
        );

        Ok(output)
    }
}
