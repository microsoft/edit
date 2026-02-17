# iEdit (ie)

A simple, fast terminal text editor for macOS.

iEdit is a fork of [Microsoft Edit](https://github.com/microsoft/edit), a terminal text editor inspired by the classic MS-DOS Editor. This fork was created due to the slow pace of adding key features like syntax highlighting in the original project, and its primary focus on Windows. iEdit is dedicated to macOS and aims to deliver a lightweight, modern editing experience with features that matter.

## Features

- Lightweight and fast — single binary under 500KB
- Syntax highlighting (YAML, conf/INI, and more coming)
- Modern keyboard shortcuts (VS Code-like)
- Mouse support
- Search and replace with ICU regex
- Word wrap
- Large file support
- macOS native (ARM and Intel)

## Installation

### From source

* [Install Rust](https://www.rust-lang.org/tools/install)
* Install the nightly toolchain: `rustup install nightly`
  * Alternatively, set the environment variable `RUSTC_BOOTSTRAP=1`
* Clone the repository
* For a release build, run:
  * Rust 1.90 or earlier: `cargo build --config .cargo/release.toml --release`
  * Otherwise: `cargo build --config .cargo/release-nightly.toml --release`

The binary will be at `target/release/ie`.

### From GitHub Releases

Download pre-built macOS binaries (ARM and Intel universal binary) from the [Releases page](https://github.com/personal/iedit/releases/latest).

## Usage

```sh
ie [OPTIONS] [FILE[:LINE[:COLUMN]]]...
```

### Options

| Option | Description |
| --- | --- |
| `-h`, `--help` | Print the help message |
| `-v`, `--version` | Print the version number |

### Keyboard Shortcuts

Press `F10` to access the menu bar. The editor uses standard VS Code-like shortcuts:

| Shortcut | Action |
| --- | --- |
| `Ctrl+N` | New file |
| `Ctrl+O` | Open file |
| `Ctrl+S` | Save |
| `Ctrl+W` | Close file |
| `Ctrl+Q` | Quit |
| `Ctrl+F` | Find |
| `Ctrl+R` | Replace |
| `Ctrl+G` | Go to line |
| `Ctrl+P` | Go to file |
| `Alt+Z` | Toggle word wrap |

## Syntax Highlighting

iEdit includes built-in syntax highlighting for the following file types:

| Language | Extensions / Files |
| --- | --- |
| YAML | `.yml`, `.yaml` |
| Conf/INI | `.conf`, `.ini`, `.cfg`, `.cnf`, `.properties`, `.env`, `.toml`, and well-known config files (`config`, `credentials`, `.gitconfig`, `.editorconfig`, etc.) |

More languages are planned: Python, Rust, Go, and HCL/Terraform.

## Build Configuration

During compilation you can set various environment variables to configure the build:

| Environment variable | Description |
| --- | --- |
| `EDIT_CFG_ICU*` | See [ICU library name](#icu-library-name) for details. |
| `EDIT_CFG_LANGUAGES` | A comma-separated list of languages to include in the build. See [i18n/ie.toml](i18n/ie.toml) for available languages. |

### ICU library name

This project _optionally_ depends on the ICU library for its Search and Replace functionality.
On macOS, the project uses `libicucore.dylib` which is bundled with the system.

To test your ICU settings, run:
```sh
cargo test -- --ignored
```

## Origin

iEdit is a fork of [Microsoft Edit](https://github.com/microsoft/edit), originally created by Microsoft Corporation under the MIT License. This fork diverges from the original with a macOS-only focus and the addition of features like syntax highlighting that were not prioritized in the upstream project.

## License

MIT License. See [LICENSE](LICENSE) for details.
