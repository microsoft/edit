// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::fs::File;
use std::io::{BufRead, BufReader, IsTerminal, Write as _, stdout};
use std::path::{Path, PathBuf};
use std::process::exit;

use anyhow::bail;
use argh::FromArgs;
use lsh::engine::Engine;
use stdext::arena::scratch_arena;
use stdext::glob::glob_match;

#[derive(FromArgs, PartialEq, Debug)]
#[argh(description = "Debug and test frontend for Leonard's Shitty Highlighter")]
struct Command {
    #[argh(subcommand)]
    sub: SubCommands,
}

#[derive(FromArgs, PartialEq, Debug)]
#[argh(subcommand)]
enum SubCommands {
    Compile(SubCommandOneCompile),
    Assembly(SubCommandAssembly),
    Render(SubCommandRender),
}

#[derive(FromArgs, PartialEq, Debug)]
#[argh(subcommand, name = "compile", description = "Generate Rust code from .lsh files")]
struct SubCommandOneCompile {
    #[argh(positional, description = "source .lsh file or directory")]
    lsh: PathBuf,
}

#[derive(FromArgs, PartialEq, Debug)]
#[argh(subcommand, name = "assembly", description = "Generate assembly from .lsh files")]
struct SubCommandAssembly {
    #[argh(positional, description = "source .lsh file or directory")]
    lsh: PathBuf,
}

#[derive(FromArgs, PartialEq, Debug)]
#[argh(subcommand, name = "render", description = "Highlight text files")]
struct SubCommandRender {
    #[argh(positional, description = "source .lsh file or directory")]
    lsh: PathBuf,
    #[argh(positional, description = "source text file")]
    input: PathBuf,
}

pub fn main() {
    if let Err(e) = run() {
        eprintln!("{e}");
        exit(1);
    }
}

fn run() -> anyhow::Result<()> {
    stdext::arena::init(128 * 1024 * 1024).unwrap();

    let command: Command = argh::from_env();
    let scratch = scratch_arena(None);
    let mut generator = lsh::compiler::Generator::new(&scratch);
    let mut read_lsh = |path: &Path| {
        if path.is_dir() { generator.read_directory(path) } else { generator.read_file(path) }
    };

    match &command.sub {
        SubCommands::Compile(cmd) => {
            read_lsh(&cmd.lsh)?;
            let output = generator.generate_rust()?;
            _ = stdout().write_all(output.as_bytes());
        }
        SubCommands::Assembly(cmd) => {
            read_lsh(&cmd.lsh)?;
            let vt = stdout().is_terminal();
            let output = generator.generate_assembly(vt)?;
            _ = stdout().write_all(output.as_bytes());
        }
        SubCommands::Render(cmd) => {
            read_lsh(&cmd.lsh)?;
            run_render(generator, &cmd.input)?;
        }
    }

    Ok(())
}

fn run_render(generator: lsh::compiler::Generator, path: &Path) -> anyhow::Result<()> {
    let assembly = generator.assemble()?;

    let Some(entrypoint) = assembly.entrypoints.iter().find(|ep| {
        ep.paths
            .iter()
            .any(|pattern| glob_match(pattern.as_bytes(), path.as_os_str().as_encoded_bytes()))
    }) else {
        bail!("No matching highlighting definition found");
    };

    let mut color_map = Vec::new();
    let mut unknown_kinds = Vec::new();
    for hk in &assembly.highlight_kinds {
        let color = match hk.identifier {
            "other" => "",

            "comment" => "\x1b[32m",  // Green
            "method" => "\x1b[93m",   // Bright Yellow
            "string" => "\x1b[91m",   // Bright Red
            "variable" => "\x1b[96m", // Bright Cyan

            "constant.language" => "\x1b[94m",   // Bright Blue
            "constant.numeric" => "\x1b[92m",    // Bright Green
            "keyword.control" => "\x1b[95m",     // Bright Magenta
            "keyword.other" => "\x1b[94m",       // Bright Blue
            "markup.bold" => "\x1b[1m",          // Bold
            "markup.changed" => "\x1b[94m",      // Bright Blue
            "markup.deleted" => "\x1b[91m",      // Bright Red
            "markup.heading" => "\x1b[94m",      // Bright Blue
            "markup.inserted" => "\x1b[92m",     // Bright Green
            "markup.italic" => "\x1b[3m",        // Italic
            "markup.link" => "\x1b[4m",          // Underlined
            "markup.list" => "\x1b[94m",         // Bright Blue
            "markup.strikethrough" => "\x1b[9m", // Strikethrough
            "meta.header" => "\x1b[94m",         // Bright Blue

            _ => {
                unknown_kinds.push(hk.identifier.to_string());
                ""
            }
        };

        if !color.is_empty() {
            if color_map.len() <= hk.value as usize {
                color_map.resize(hk.value as usize + 1, "");
            }
            color_map[hk.value as usize] = color;
        }
    }
    if !unknown_kinds.is_empty() {
        eprintln!("\x1b[33mWarning: Unknown highlight kinds:");
        for kind in &unknown_kinds {
            eprintln!("  - {}", kind);
        }
        eprintln!("\x1b[m");
    }

    // Convert Assembly data to static references by leaking memory
    // This is fine for a CLI tool that runs once and exits
    let charsets: Vec<[u16; 16]> = assembly
        .charsets
        .into_iter()
        .map(|cs| {
            let mut arr = [0u16; 16];
            for lo in 0..16 {
                let mut u = 0u16;
                for hi in 0..16 {
                    u |= (cs[hi * 16 + lo] as u16) << hi;
                }
                arr[lo] = u;
            }
            arr
        })
        .collect();

    let mut engine = Engine::new(
        &assembly.instructions,
        &assembly.strings,
        &charsets,
        entrypoint.address as u32,
    );

    let reader = BufReader::new(File::open(path)?);
    let mut stdout = stdout();

    for line in reader.lines() {
        let line = line?;
        let scratch = scratch_arena(None);
        let highlights = engine.parse_next_line::<u32>(&scratch, line.as_bytes());

        for w in highlights.windows(2) {
            let curr = &w[0];
            let next = &w[1];
            let start = curr.start;
            let end = next.start;
            let kind = curr.kind;
            let text = &line[start..end];

            if let Some(color) = color_map.get(kind as usize) {
                write!(stdout, "{color}{text}\x1b[m")?;
            } else {
                stdout.write_all(text.as_bytes())?;
            }
        }
        writeln!(stdout)?;
    }

    Ok(())
}
