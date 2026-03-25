#!/usr/bin/env bash
set -Eeuo pipefail
umask 022

log(){ printf '\033[1;34m==>\033[0m %s\n' "$*"; }
warn(){ printf '\033[1;33m!!\033[0m %s\n' "$*"; }
is_root(){ [ "${EUID:-$(id -u)}" -eq 0 ]; }
: "${EDIT_PREFIX:=/usr/local}"
: "${EDIT_USER_PREFIX:=$HOME/.local}"

# ----- args -----
MODE="all"   # all | user | system
DRYRUN=0
for a in "$@"; do
  case "$a" in
    --user-only) MODE="user" ;;
    --system-only) MODE="system" ;;
    --dry-run) DRYRUN=1 ;;
    -h|--help)
      cat <<'EOF'
Usage: uninstall.sh [--user-only|--system-only] [--dry-run]
  --user-only     Remove only ~/.local installs
  --system-only   Remove only /usr/local installs (requires root/sudo/doas)
  --dry-run       Show what would be removed, without removing
EOF
      exit 0
      ;;
    *) warn "Ignoring unknown argument: $a" ;;
  esac
done

# ----- elevation helper (sudo/doas if available) -----
SUDO=""
if ! is_root; then
  if command -v sudo >/dev/null 2>&1; then
    SUDO="sudo"
  elif command -v doas >/dev/null 2>&1; then
    SUDO="doas"
  fi
fi

run_rm() {
  # rm path... (with optional sudo)
  if [ "$DRYRUN" -eq 1 ]; then
    printf '[dry-run] rm -f %s\n' "$*" ; return 0
  fi
  rm -f "$@" 2>/dev/null || true
}

run_rm_root() {
  # rm path... as root (if possible)
  if [ "$DRYRUN" -eq 1 ]; then
    printf '[dry-run] %s rm -f %s\n' "${SUDO:-(no-sudo)}" "$*"
    return 0
  fi
  if is_root; then
    rm -f "$@" 2>/dev/null || true
  elif [ -n "$SUDO" ]; then
    $SUDO rm -f "$@" 2>/dev/null || true
  else
    warn "No sudo/doas; cannot remove: $*"
  fi
}

remove_manifest_entries() {
  local manifest="$1" use_root="$2"
  [ -f "$manifest" ] || return 1

  while IFS= read -r path; do
    [ -n "$path" ] || continue
    if [ "$use_root" -eq 1 ]; then
      run_rm_root "$path"
    else
      run_rm "$path"
    fi
  done < "$manifest"

  if [ "$use_root" -eq 1 ]; then
    run_rm_root "$manifest"
  else
    run_rm "$manifest"
  fi
}

# ----- user-local -----
if [ "$MODE" = "all" ] || [ "$MODE" = "user" ]; then
  log "Removing user-local binaries"
  if ! remove_manifest_entries "$EDIT_USER_PREFIX/share/edit/install-manifest" 0; then
    warn "No user install manifest found; falling back to legacy path cleanup"
    run_rm "$EDIT_USER_PREFIX/bin/edit" \
           "$EDIT_USER_PREFIX/bin/msedit" \
           "$EDIT_USER_PREFIX/bin/.edit-real"
  fi
fi

# ----- system-wide -----
if [ "$MODE" = "all" ] || [ "$MODE" = "system" ]; then
  if ! is_root && [ -z "$SUDO" ]; then
    warn "Skipping system-wide removal: need root, sudo, or doas"
  else
    log "Removing system-wide binaries"
    if ! remove_manifest_entries "$EDIT_PREFIX/share/edit/install-manifest" 1; then
      warn "No system install manifest found; falling back to legacy binary cleanup only"
      run_rm_root "$EDIT_PREFIX/bin/edit" "$EDIT_PREFIX/bin/msedit"
      run_rm_root "$EDIT_PREFIX/libexec/edit-real"
    fi

    if command -v ldconfig >/dev/null 2>&1; then
      if [ "$DRYRUN" -eq 1 ]; then
        printf '[dry-run] %s ldconfig\n' "${SUDO:-(no-sudo)}"
      else
        if is_root; then ldconfig || true
        else $SUDO ldconfig || true
        fi
      fi
    fi
  fi
fi

log "Done."
