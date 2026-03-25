#!/usr/bin/env bash
set -Eeuo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

bash -n "$repo_root/tools/install.sh"
bash -n "$repo_root/tools/uninstall.sh"

source "$repo_root/tools/install.sh"
source "$repo_root/tools/uninstall.sh"

fail() {
  printf 'test failure: %s\n' "$*" >&2
  exit 1
}

assert_eq() {
  local actual="$1" expected="$2" message="$3"
  [ "$actual" = "$expected" ] || fail "$message (expected '$expected', got '$actual')"
}

assert_exists() {
  [ -e "$1" ] || fail "expected path to exist: $1"
}

assert_not_exists() {
  [ ! -e "$1" ] || fail "expected path to be absent: $1"
}

assert_manifest_contains() {
  local manifest="$1" path="$2"
  grep -Fxq "$path" "$manifest" || fail "expected manifest $manifest to contain $path"
}

run_unit_tests() {
  local actual tmp compat older release_cfg

  actual="$(cd "$repo_root" && find_local_edit_checkout)"
  assert_eq "$actual" "$repo_root" "find_local_edit_checkout should detect the repository root"

  tmp="$(mktemp -d)"
  if (cd "$tmp" && find_local_edit_checkout >/dev/null 2>&1); then
    fail "find_local_edit_checkout should reject unrelated repositories"
  fi
  rm -rf "$tmp"

  tmp="$(mktemp -d)"
  mkdir -p "$tmp/a" "$tmp/b"
  : > "$tmp/a/libicutest.so.1"
  : > "$tmp/b/libicutest.so.3"
  actual="$(find_latest_icu_lib_in_dirs "libicutest" "$tmp/a:$tmp/b")"
  assert_eq "$actual" "$tmp/b/libicutest.so.3" "find_latest_icu_lib_in_dirs should choose the newest candidate"
  : > "$tmp/a/libicutest.so"
  icu_has_unversioned_lib_in_dirs "libicutest" "$tmp/a:$tmp/b" || fail "icu_has_unversioned_lib_in_dirs should find unversioned libraries"
  rm -rf "$tmp"

  compat="$(mktemp)"
  older="$(mktemp -d)"
  mkdir -p "$older/.cargo"
  : > "$older/.cargo/release.toml"
  release_cfg="$(select_release_config "$older" "$HOME/.cargo/bin/cargo" "$compat")"
  if nightly_uses_real_immediate_abort "$HOME/.cargo/bin/cargo"; then
    assert_eq "$release_cfg" "$compat" "older source trees should use a generated compatibility config on newer nightly toolchains"
    grep -Fq 'panic = "immediate-abort"' "$compat" || fail "compatibility config should set panic = immediate-abort"
  else
    assert_eq "$release_cfg" ".cargo/release.toml" "older source trees should keep release.toml on older nightly toolchains"
  fi
  rm -f "$compat"
  rm -rf "$older"

  if [ -f "$repo_root/.cargo/release-nightly.toml" ] && nightly_uses_real_immediate_abort "$HOME/.cargo/bin/cargo"; then
    compat="$(mktemp)"
    release_cfg="$(select_release_config "$repo_root" "$HOME/.cargo/bin/cargo" "$compat")"
    assert_eq "$release_cfg" ".cargo/release-nightly.toml" "modern source trees should prefer release-nightly.toml"
    rm -f "$compat"
  fi

  tmp="$(mktemp -d)"
  mkdir -p "$tmp/prefix/bin" "$tmp/prefix/share/edit"
  : > "$tmp/prefix/bin/edit"
  ln -s "$tmp/prefix/bin/edit" "$tmp/prefix/bin/msedit"
  printf '%s\n%s\n' "$tmp/prefix/bin/edit" "$tmp/prefix/bin/msedit" > "$tmp/prefix/share/edit/install-manifest"
  remove_manifest_entries "$tmp/prefix/share/edit/install-manifest" 0
  assert_not_exists "$tmp/prefix/bin/edit"
  assert_not_exists "$tmp/prefix/bin/msedit"
  assert_not_exists "$tmp/prefix/share/edit/install-manifest"
  rm -rf "$tmp"
}

assert_user_install_tree() {
  local prefix="$1"
  local manifest="$prefix/share/edit/install-manifest"

  assert_exists "$prefix/bin/edit"
  assert_exists "$prefix/bin/msedit"
  assert_exists "$manifest"
  assert_manifest_contains "$manifest" "$prefix/bin/edit"
  assert_manifest_contains "$manifest" "$prefix/bin/msedit"
}

run_local_checkout_install_cycle() {
  local tmp prefix
  tmp="$(mktemp -d)"
  prefix="$tmp/prefix"

  (
    cd "$repo_root"
    EDIT_ALLOW_ELEVATION=0 \
    EDIT_SKIP_DEPS=1 \
    EDIT_USER_PREFIX="$prefix" \
    bash "$repo_root/tools/install.sh"
  )

  assert_user_install_tree "$prefix"

  EDIT_USER_PREFIX="$prefix" bash "$repo_root/tools/uninstall.sh" --user-only
  assert_not_exists "$prefix/bin/edit"
  assert_not_exists "$prefix/bin/msedit"
  assert_not_exists "$prefix/share/edit/install-manifest"

  rm -rf "$tmp"
}

run_fake_repo_install_cycle() {
  local tmp fake prefix source_ref
  tmp="$(mktemp -d)"
  fake="$tmp/fake"
  prefix="$tmp/prefix"
  source_ref="$(git -C "$repo_root" rev-parse HEAD)"

  mkdir -p "$fake"
  (
    cd "$fake"
    git init -q
    cat > Cargo.toml <<'EOF'
[package]
name = "fake"
version = "0.1.0"
edition = "2021"
EOF
    EDIT_ALLOW_ELEVATION=0 \
    EDIT_SKIP_DEPS=1 \
    EDIT_SOURCE_URL="$repo_root" \
    EDIT_SOURCE_REF="$source_ref" \
    EDIT_USER_PREFIX="$prefix" \
    bash "$repo_root/tools/install.sh"
  )

  assert_user_install_tree "$prefix"

  EDIT_USER_PREFIX="$prefix" bash "$repo_root/tools/uninstall.sh" --user-only
  assert_not_exists "$prefix/bin/edit"
  assert_not_exists "$prefix/bin/msedit"
  assert_not_exists "$prefix/share/edit/install-manifest"

  rm -rf "$tmp"
}

main() {
  run_unit_tests
  run_local_checkout_install_cycle
  run_fake_repo_install_cycle
  printf 'installer tests passed\n'
}

main "$@"
