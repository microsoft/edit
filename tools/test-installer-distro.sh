#!/usr/bin/env bash
set -Eeuo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
prefix="${EDIT_PREFIX:-/usr/local}"
bin_dir="$prefix/bin"
libexec_dir="$prefix/libexec"
manifest="$prefix/share/edit/install-manifest"

fail() {
  printf 'test failure: %s\n' "$*" >&2
  exit 1
}

assert_exists() {
  [ -e "$1" ] || fail "expected path to exist: $1"
}

assert_not_exists() {
  [ ! -e "$1" ] || fail "expected path to be absent: $1"
}

assert_manifest_contains() {
  local path="$1"
  grep -Fxq "$path" "$manifest" || fail "expected manifest $manifest to contain $path"
}

cleanup() {
  bash "$repo_root/tools/uninstall.sh" --system-only >/dev/null 2>&1 || true
}

main() {
  trap cleanup EXIT

  bash -n "$repo_root/tools/install.sh"
  bash -n "$repo_root/tools/uninstall.sh"

  EDIT_SOURCE_DIR="$repo_root" bash "$repo_root/tools/install.sh"

  assert_exists "$bin_dir/edit"
  assert_exists "$bin_dir/msedit"
  assert_exists "$manifest"
  assert_manifest_contains "$bin_dir/edit"
  assert_manifest_contains "$bin_dir/msedit"

  if [ -e "$libexec_dir/edit-real" ]; then
    assert_manifest_contains "$libexec_dir/edit-real"
  fi

  bash "$repo_root/tools/uninstall.sh" --system-only

  assert_not_exists "$bin_dir/edit"
  assert_not_exists "$bin_dir/msedit"
  assert_not_exists "$libexec_dir/edit-real"
  assert_not_exists "$manifest"

  trap - EXIT
  printf 'distro installer smoke test passed\n'
}

main "$@"
