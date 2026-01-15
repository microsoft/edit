// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

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
                Instruction::JumpIfMatchCharset { .. }
                | Instruction::JumpIfMatchPrefix { .. }
                | Instruction::JumpIfMatchPrefixInsensitive { .. } => {
                    if vt {
                        60
                    } else {
                        30
                    }
                }
                _ => 0,
            };

            let scratch = scratch_arena(None);
            _ = write!(
                output,
                "{line_prefix}{off:>line_num_width$}:  {mnemonic:mnemonic_width$}",
                mnemonic = instr.mnemonic(&scratch, &mnemonic_config)
            );

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

        output.push_str(
            "
#[repr(C)]
#[derive(Default, Clone, Copy)]
pub struct Registers {
    pub zero: u32, // x0 = Zero
    pub pc: u32,   // x1 = ProgramCounter
    pub off: u32,  // x2 = InputOffset
    pub hs: u32,   // z3 = HighlightStart
    pub hk: u32,   // x4 = HighlightKind
    pub x5: u32,
    pub x6: u32,
    pub x7: u32,
    pub x8: u32,
    pub x9: u32,
    pub x10: u32,
    pub x11: u32,
    pub x12: u32,
    pub x13: u32,
    pub x14: u32,
    pub x15: u32,
}

impl Registers {
    #[inline(always)]
    pub fn get(&self, reg: usize) -> u32 {
        debug_assert!(reg < 16);
        unsafe { (self as *const _ as *const u32).add(reg).read() }
    }

    #[inline(always)]
    pub fn set(&mut self, reg: usize, val: u32) {
        debug_assert!(reg < 16);
        unsafe { (self as *mut _ as *mut u32).add(reg).write(val) }
    }
}

",
        );

        output.push_str(
            "
pub struct Language {
    pub name: &'static str,
    pub filenames: &'static [&'static str],
    pub entrypoint: u32,
}

impl Language {
    const fn new(name: &'static str, filenames: &'static [&'static str], entrypoint: u32) -> Self {
        Self { name, filenames, entrypoint }
    }
}

impl PartialEq for &Language {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(*self, *other)
    }
}
",
        );

        output.push_str("/**\n");
        output.push_str(&self.compiler.as_mermaid());
        output.push_str("**/\n");

        output.push_str("\n#[rustfmt::skip] pub const LANGUAGES: &[Language] = &[\n");
        for ep in &assembly.entrypoints {
            _ = writeln!(output, "    Language::new({:?}, &[", ep.display_name);
            for (idx, f) in ep.filenames.iter().enumerate() {
                if idx != 0 {
                    _ = write!(output, ", ");
                }
                _ = writeln!(output, "{f:?}");
            }
            _ = writeln!(output, "], {}),", ep.address);
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

            _ = writeln!(
                output,
                "{:<padding_width$}// {off:>line_num_width$}:  {mnemonic}",
                "",
                padding_width = 9usize.saturating_sub(len) * 6,
                mnemonic = instr.mnemonic(&scratch, &Default::default())
            );

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
