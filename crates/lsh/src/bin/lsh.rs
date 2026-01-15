// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::env::args_os;
use std::io::{IsTerminal, Write as _, stdout};
use std::path::Path;
use std::process::exit;

use lsh::CompileResult;
use stdext::arena::scratch_arena;

pub fn main() {
    if let Err(e) = run() {
        eprintln!("{e}");
        exit(1);
    }
}

fn run() -> CompileResult<()> {
    stdext::arena::init(128 * 1024 * 1024).unwrap();

    let scratch = scratch_arena(None);
    let mut generator = lsh::compiler::Generator::new(&scratch);
    let mut has_path = false;
    let mut assembly = false;

    for arg in args_os().skip(1) {
        let s = arg.to_string_lossy();
        if s == "-h" || s == "--help" {
            help();
        } else if s == "--assembly" {
            assembly = true;
        } else if s.starts_with('-') {
            eprintln!("Error: Unknown option '{s}'");
            help();
        } else {
            let path = Path::new(&arg);
            if path.is_dir() {
                generator.read_directory(path)?;
            } else {
                generator.read_file(path)?;
            }
            has_path = true;
        }
    }

    if !has_path {
        help();
    }

    let output = if assembly {
        let vt = stdout().is_terminal();
        generator.generate_assembly(vt)?
    } else {
        generator.generate_rust()?
    };
    _ = stdout().write_all(output.as_bytes());
    Ok(())
}

#[allow(clippy::print_with_newline)]
fn help() -> ! {
    eprint!(
        "\
Usage: lsh [options...] <paths...>
  <paths...>    One or more *.lsh files or directories to process.
  -h, --help    Show this help message.
  --assembly    Output only the LSH assembly.
"
    );
    exit(1);
}
