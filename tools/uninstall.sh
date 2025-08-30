#!/usr/bin/env bash
set -Eeuo pipefail
umask 022

log(){ printf '\033[1;34m==>\033[0m %s\n' "$*"; }
warn(){ printf '\033[1;33m!!\033[0m %s\n' "$*"; }
is_root(){ [ "${EUID:-$(id -u)}" -eq 0 ]; }

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

# ----- user-local -----
if [ "$MODE" = "all" ] || [ "$MODE" = "user" ]; then
  log "Removing user-local binaries"
  run_rm "$HOME/.local/bin/edit" \
         "$HOME/.local/bin/msedit" \
         "$HOME/.local/bin/.edit-real"
fi

# ----- system-wide -----
if [ "$MODE" = "all" ] || [ "$MODE" = "system" ]; then
  if ! is_root && [ -z "$SUDO" ]; then
    warn "Skipping system-wide removal: need root, sudo, or doas"
  else
    log "Removing system-wide binaries"
    run_rm_root /usr/local/bin/edit /usr/local/bin/msedit
    run_rm_root /usr/local/libexec/edit-real

    log "Removing ICU helper symlinks (if we created them)"
    for lib in libicuuc libicui18n libicudata; do
      if [ -L "/usr/local/lib/$lib.so" ]; then
        run_rm_root "/usr/local/lib/$lib.so"
      fi
    done

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
