# Repository Guidelines

## Project Structure & Module Organization
- `src/`: core Rust crates for the editor (`bin/edit` for the terminal app, `buffer`, `tui`, `framebuffer`, etc.). Start from `src/bin/edit/main.rs` for entry flow, and `src/bin/edit/state.rs` for global state/preferences.
- `i18n/`: localization sources (`edit.toml`) compiled into `src/bin/edit/localization.rs`.
- `assets/`: icons, branding, and misc resources bundled with releases.
- `scripts/`: helper executables; prefer `scripts/*.sh` over raw cargo commands to match CI settings.

## Build, Test, and Development Commands
- `scripts/build-debug.sh` – debug build (`cargo build`).
- `scripts/build-release.sh` – release build with nightly config (`cargo build --config .cargo/release-nightly.toml --release`).
- `scripts/test.sh [-- <args>]` – run `cargo test`.
- `scripts/check.sh` – `cargo check` (fast validation).
- `scripts/install.sh` – `cargo install --path . --locked`; installs `edit` into `~/.cargo/bin`.

## Coding Style & Naming Conventions
- Use `cargo fmt` (already configured via `rustfmt.toml`); check formatting before committing.
- Modules follow snake_case; types use UpperCamelCase; constants use SCREAMING_SNAKE.
- Prefer `arena_format!` for UI strings needing formatting; keep UI `classname`s stable for the TUI diffing algorithm.

## Testing Guidelines
- Tests leverage Rust’s standard `cargo test` runner; integration tests live alongside modules.
- Add `#[test]` functions near the functionality they cover; name as `test_<feature>_<case>`.
- For rendering or UI changes, add snapshot/debug logs where possible and describe manual test steps in PRs.

## Commit & Pull Request Guidelines
- Commit messages mirror existing history: concise imperative summary (e.g., “Add preferences dialog with colorschemes”).
- PRs should include: purpose/summary, key changes, testing performed (`cargo check`, `scripts/test.sh`, manual steps), and screenshots for UI shifts.
- Reference related issues (`Fixes #123`) when applicable, and keep PRs focused; split large features into iterative commits when possible.
