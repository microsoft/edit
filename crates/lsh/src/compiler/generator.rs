// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! Code generators for different output formats.
//!
//! ## TODO
//!
//! - The label lookup (with a `HashMap`) is just plain dumb.
//!   The backend should emit metadata, I think.

use std::collections::HashMap;
use std::ffi::OsStr;
use std::fmt::format;
use std::fs::read_dir;
use std::io;
use std::path::PathBuf;

use stdext::arena::scratch_arena;

use super::*;

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

            let (instr, len) = Instruction::decode(&assembly.instructions[off..]);
            let mnemonic_width = match instr {
                Some(Instruction::JumpIfMatchCharset { .. })
                | Some(Instruction::JumpIfMatchPrefix { .. })
                | Some(Instruction::JumpIfMatchPrefixInsensitive { .. }) => {
                    if vt {
                        60
                    } else {
                        30
                    }
                }
                _ => 0,
            };

            let scratch = scratch_arena(None);
            if let Some(instr) = instr {
                _ = write!(
                    output,
                    "{line_prefix}{off:>line_num_width$}:  {mnemonic:mnemonic_width$}",
                    mnemonic = instr.mnemonic(&scratch, &mnemonic_config),
                );
            } else {
                _ = write!(
                    output,
                    "{line_prefix}{off:>line_num_width$}:  {_n}{opcode:#02x}{n_}",
                    opcode = assembly.instructions[off],
                    _n = mnemonic_config.numeric_prefix,
                    n_ = mnemonic_config.numeric_suffix,
                );
            }

            match instr {
                Some(Instruction::JumpIfMatchCharset { idx, .. }) => {
                    _ = write!(
                        output,
                        " {comment_prefix}// {:?}{comment_suffix}",
                        assembly.charsets[idx as usize]
                    )
                }
                Some(Instruction::JumpIfMatchPrefix { idx, .. })
                | Some(Instruction::JumpIfMatchPrefixInsensitive { idx, .. }) => {
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
        output.push_str("use lsh::engine::Language;\n\n");

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
                ep.name, ep.display_name, ep.address
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
            len = assembly.instructions.len(),
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
                    padding_width = 9usize.saturating_sub(len) * 6,
                    mnemonic = instr.mnemonic(&scratch, &Default::default())
                );
            }

            off += len;
        }
        output.push_str("];\n");

        _ = writeln!(
            output,
            "\n#[rustfmt::skip] pub const CHARSETS: [[u16; 16]; {len}] = [",
            len = assembly.charsets.len(),
        );
        for cs in assembly.charsets {
            output.push_str("    [");
            for lo in 0..16 {
                if lo > 0 {
                    output.push_str(", ");
                }
                let mut u = 0u16;
                for hi in 0..16 {
                    u |= (cs[hi * 16 + lo] as u16) << hi;
                }
                _ = write!(output, "0x{u:04x}");
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
}
