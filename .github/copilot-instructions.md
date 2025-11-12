## Purpose
Short, actionable notes to help AI coding agents be productive in this repository (microsoft/edit).

## Big picture (what to change and where)
- Single crate Rust editor. Core library code lives in `src/` and the CLI/UI binary is `src/bin/edit/`.
- UI loop and terminal handling are centered in `src/bin/edit/main.rs` (vt parser, input parser, TUI render loop).
- Platform abstractions and low-level IO live in `src/sys/` (`unix.rs`, `windows.rs`). Prefer changes here for platform behavior.
- Memory allocation / temporary buffers use the project's arena allocator in `src/arena/` (look for `Arena`, `ArenaString`, `scratch_arena`).
- Internationalization is generated at build time from `i18n/edit.toml` by the build script `build/main.rs` and included via `include!(concat!(env!("OUT_DIR"), "/i18n_edit.rs"));` in `src/bin/edit/localization.rs`.

## Important developer workflows & commands
- Recommended toolchain: Rust stable per `rust-toolchain.toml` but README suggests nightly for some builds. If you can't use nightly, `RUSTC_BOOTSTRAP=1` is an alternative.
- Debug (local):
  - cargo build (normal)
  - cargo run -p edit -- <args> (run the editor binary)
  - To enable latency instrumentation: `cargo build --package edit --features debug-latency` (this is used in the TUI loop).
- Release: follow README — either
  - `cargo build --config .cargo/release.toml --release` (older rust) or
  - `cargo build --config .cargo/release-nightly.toml --release` (when README instructs nightly-specific config).
- Tests and ignored tests: `cargo test`. Some tests are intentionally marked ignored and require environment tweaks — run `cargo test -- --ignored` to exercise them.
- Benchmarks: `cargo bench` (project uses `criterion`).

## Build-time environment and integration points
- i18n: `build/main.rs` reads `i18n/edit.toml` and writes `OUT_DIR/i18n_edit.rs`. Rebuild on changes to `i18n/edit.toml`.
- ICU (optional): the build script emits env vars consumed by code. Important envs:
  - `EDIT_CFG_ICUUC_SONAME`, `EDIT_CFG_ICUI18N_SONAME`, `EDIT_CFG_ICU_CPP_EXPORTS`, `EDIT_CFG_ICU_RENAMING_VERSION`, `EDIT_CFG_ICU_RENAMING_AUTO_DETECT`.
  - These affect `src/sys/*.rs` and `src/icu.rs` where dynamic symbol names are composed using `env!("EDIT_CFG_...")`.
  - Example (Linux): `EDIT_CFG_ICUUC_SONAME=libicuuc.so EDIT_CFG_ICUI18N_SONAME=libicui18n.so cargo build`.

## Project-specific patterns and conventions
- Arena-first memory: many data structures use `Arena` and `ArenaString` for short-lived allocations. Prefer these for UI paths and avoid heap-allocating large temporaries.
- `scratch_arena(None)` is commonly used to create ephemeral buffers inside hot loops — be careful when refactoring to not extend lifetimes.
- UI code is modularized as `draw_*` modules under `src/bin/edit/` (e.g., `draw_editor.rs`, `draw_menubar.rs`) — add UI elements by following the existing `draw_*` patterns.
- Buffer logic is in `buffer/` (gap buffer, line cache, navigation). Changes that affect on-disk or undo behavior likely live there.
- Platform-specific code in `src/sys` is authoritative for terminal and file IO. Changes to terminal modes, raw input, or clipboard should be made there.
- Use existing macro helpers: `arena_format!`, `KIBI`, `MEBI`, and `MetricFormatter` when producing formatted strings to match style and memory usage.

## Where to look for common tasks (quick map)
- Add a UI control/widget: `src/bin/edit/draw_*.rs` + `src/bin/edit/state.rs`.
- Change buffer semantics: `buffer/gap_buffer.rs`, `buffer/line_cache.rs`, `buffer/navigation.rs`.
- Localization strings: `i18n/edit.toml` -> `build/main.rs` generates `OUT_DIR/i18n_edit.rs` -> included in `src/bin/edit/localization.rs`.
- Terminal/TTY behavior: `src/sys/unix.rs` and `src/sys/windows.rs` and `src/bin/edit/main.rs` (setup_terminal, RestoreModes).
- Low-level unicode/width handling: `src/unicode/*` and `src/icu.rs`.

## Safety and testing notes for agents
- Avoid modifying unsafe, low-level allocator and lifetime-sensitive code (arena, buffer internals) without running quick checks — these are delicate and rely on invariants across many modules.
- Many behaviors are environment-dependent (terminals, ICU libs). When adding tests, prefer unit tests in `src/` that don't rely on terminal IO; integration tests that require terminal emulation are flakier.

## Example quick tasks and references
- Run editor on a file: `cargo run -p edit -- README.md` — useful to manually validate UI changes.
- Find localization usage: `src/bin/edit/localization.rs` (includes generated file) and `i18n/edit.toml` (source).
- See platform wrappers: `src/sys/mod.rs`, `src/sys/unix.rs`, `src/sys/windows.rs`.

If anything here is unclear or you'd like additional examples (e.g., small code-change + test cycle for a UI change), tell me which area to expand and I will iterate.
