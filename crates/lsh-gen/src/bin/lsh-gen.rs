use std::env::args_os;
use std::io::Write as _;
use std::path::Path;
use std::process::exit;

use lsh_gen::CompileResult;
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
    let mut generator = lsh_gen::Generator::new(&scratch);
    let mut has_path = false;

    for arg in args_os().skip(1) {
        let path = Path::new(&arg);
        if path.is_dir() {
            generator.read_directory(path)?;
        } else {
            generator.read_file(path)?;
        }
        has_path = true;
    }

    if !has_path {
        help();
    }

    let output = generator.generate_rust()?;
    _ = std::io::stdout().write_all(output.as_bytes());
    Ok(())
}

fn help() -> ! {
    eprintln!("Usage: lsh-gen <input files>");
    exit(1);
}
