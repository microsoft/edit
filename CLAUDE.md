# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Microsoft Edit — a terminal-based text editor written in Rust, inspired by MS-DOS Editor. Workspace has three crates: `edit` (main editor), `stdext` (stdlib extensions with arena allocator and custom collections), and `unicode-gen` (Unicode data code generator).

## Build & Development

Requires **Rust nightly** toolchain (see `rust-toolchain.toml`). MSRV: 1.93, edition 2024.

```bash
# Build
cargo build

# Run
cargo run

# Test (all crates, all features)
cargo test --all-features --all-targets

# Run ignored tests (e.g. ICU config validation)
cargo test -- --ignored

# Format check
cargo fmt --all -- --check

# Lint (matches CI strictness)
cargo clippy --all-features --all-targets -- --no-deps --deny warnings

# Benchmarks (criterion)
cargo bench
```

CI runs on both Ubuntu and Windows: fmt check → tests → clippy with `--deny warnings`.

## Code Style

Formatting is configured in `rustfmt.toml`: style edition 2024, `StdExternalCrate` import grouping, `Module` import granularity, Unix newlines, field init shorthand.

## Architecture

All source paths below are relative to `crates/edit/src/`.

**Text buffer** (`buffer/`): Gap buffer that intentionally does **not** track line breaks. The only persistent state is the cursor position. Line navigation is O(n) seeking through the document for line breaks. This is a fundamental design decision that permeates the entire codebase.

Supporting this design:
- `simd/` — `memchr2` functions for finding line breaks at >100GB/s
- `unicode/` — `Utf8Chars` iterator with transparent U+FFFD replacement (~4GB/s), grapheme cluster segmentation and width measurement via `MeasurementConfig` (~600MB/s)

**Framebuffer** (`framebuffer.rs`): Video-game-style intermediate buffer that accumulates UI output, handles color blending, then diffs against the previous frame to send only necessary terminal changes.

**Immediate mode UI** (`tui.rs`): ~4000 LOC immediate mode UI system. Read the module doc for an overview.

**VT parser** (`vt.rs`): Terminal escape sequence parser. **Input parser** (`input.rs`): Keyboard/mouse input parsing.

**Platform abstractions** (`sys/`): OS-specific code for Windows and Unix.

**Main application** (`bin/edit/`): Entry point in `main.rs`. Roughly 90% UI code and business logic. Key files: `state.rs` (editor state), `documents.rs` (document handling), `draw_*.rs` (UI rendering), `apperr.rs` (error handling), `localization.rs` (i18n).

**i18n**: Translations in `i18n/edit.toml`, code generated at build time by `build/i18n.rs`. Language selection controlled by `EDIT_CFG_LANGUAGES` env var.

## Build Configuration

The build script (`build/main.rs`) generates i18n code, configures ICU library linking, and embeds Windows resources. Key env vars:
- `EDIT_CFG_LANGUAGES` — comma-separated language list
- `EDIT_CFG_ICUUC_SONAME` / `EDIT_CFG_ICUI18N_SONAME` — ICU library SONAME overrides

## Design Principles

- **Small binary size** is a priority — dependencies are generally not accepted
- Many components are custom-built (JSON parser, glob matcher, base64, VT parser) to avoid external dependencies
- Performance focus: SIMD optimizations, arena allocator, careful memory management
- Release builds use aggressive size optimizations (LTO, panic=abort, symbol stripping)
