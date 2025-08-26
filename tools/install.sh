set -euo pipefail

# Edit (Microsoft.Edit) Linux installer
# - Installs deps (build + ICU), ensures unversioned ICU symlinks exist,
# - builds Edit with nightly, and installs to /usr/local/bin or ~/.local/bin.

need_cmd() { command -v "$1" >/dev/null 2>&1; }
log() { printf '\033[1;34m==>\033[0m %s\n' "$*"; }
warn() { printf '\033[1;33m!!\033[0m %s\n' "$*"; }
die() { printf '\033[1;31mxx\033[0m %s\n' "$*"; exit 1; }

SUDO=""
if [ "${EUID:-$(id -u)}" -ne 0 ]; then
  if need_cmd sudo; then SUDO="sudo"; elif need_cmd doas; then SUDO="doas"; else SUDO=""; fi
fi

PM=""
if need_cmd apt-get; then PM=apt
elif need_cmd dnf; then PM=dnf
elif need_cmd yum; then PM=yum
elif need_cmd zypper; then PM=zypper
elif need_cmd pacman; then PM=pacman
elif need_cmd apk; then PM=apk
elif need_cmd xbps-install; then PM=xbps
else
  warn "Unknown distro. Attempting best-effort build if prerequisites exist."
fi

install_pkgs() {
  case "$PM" in
    apt)
      $SUDO apt-get update -y
      $SUDO apt-get install -y --no-install-recommends \
        build-essential pkg-config curl ca-certificates git \
        libicu-dev
      ;;
    dnf)
      $SUDO dnf -y install @development-tools gcc gcc-c++ make \
        pkgconfig curl ca-certificates git libicu-devel
      ;;
    yum)
      $SUDO yum -y groupinstall "Development Tools" || true
      $SUDO yum -y install gcc gcc-c++ make pkgconfig curl ca-certificates git libicu-devel
      ;;
    zypper)
      $SUDO zypper --non-interactive ref
      $SUDO zypper --non-interactive install -t pattern devel_basis || true
      $SUDO zypper --non-interactive install gcc gcc-c++ make \
        pkg-config curl ca-certificates git libicu-devel
      ;;
    pacman)
      $SUDO pacman -Syu --noconfirm --needed base-devel icu curl ca-certificates git
      ;;
    apk)
      $SUDO apk add --no-cache build-base pkgconfig curl ca-certificates git icu-dev
      ;;
    xbps)
      $SUDO xbps-install -Sy gcc clang make pkg-config curl ca-certificates git icu-devel
      ;;
    *)
      warn "Skipping package installation; please ensure build tools + ICU dev libs are installed."
      ;;
  esac
}

ensure_unversioned_icu_symlinks() {
  # If libicuuc.so / libicui18n.so are missing, create them pointing to the newest versioned .so
  local libdirs="/usr/lib /usr/lib64 /lib /lib64 /usr/local/lib"
  find_latest() {
    local stem="$1"
    local best=""
    for d in $libdirs; do
      [ -d "$d" ] || continue
      # shellcheck disable=SC2010
      for f in $(ls "$d"/"$stem".so.* 2>/dev/null | sort -V); do best="$f"; done
    done
    printf '%s' "$best"
  }

  for lib in libicuuc libicui18n; do
    local_unver=""
    for d in $libdirs; do
      if [ -e "$d/$lib.so" ]; then local_unver="$d/$lib.so"; break; fi
    done
    if [ -z "$local_unver" ]; then
      latest="$(find_latest "$lib")"
      if [ -n "$latest" ]; then
        log "Creating unversioned symlink for $lib → $latest"
        $SUDO ln -sf "$latest" "/usr/local/lib/$lib.so"
        if need_cmd ldconfig; then $SUDO ldconfig; fi
      else
        warn "Could not find versioned $lib.so.* — Search/Replace may fail. Install ICU dev/runtime."
      fi
    fi
  done
}

install_rust() {
  # Ensure we can install rustup even if distro rust/cargo exist
  export RUSTUP_INIT_SKIP_PATH_CHECK=yes

  # Install rustup if missing
  if ! need_cmd rustup; then
    log "Installing Rust (rustup)"
    curl --proto '=https' --tlsv1.2 -fsSL https://sh.rustup.rs | sh -s -- -y --profile minimal
  fi

  # Make sure current shell sees $HOME/.cargo/bin first (before /usr/bin cargo)
  if [ -f "$HOME/.cargo/env" ]; then
    # shellcheck disable=SC1091
    . "$HOME/.cargo/env"
  fi
  # If shell caches 'cargo' location, refresh it
  hash -r 2>/dev/null || true

  # Confirm we're using rustup's cargo; warn if not
  if command -v cargo >/dev/null 2>&1 && ! command -v cargo | grep -q "$HOME/.cargo/bin/cargo"; then
    warn "Using cargo from: $(command -v cargo) (not rustup). Build will still work, but +nightly may not."
    warn "Temporarily preferring rustup cargo for this script run."
    if [ -x "$HOME/.cargo/bin/cargo" ]; then
      export PATH="$HOME/.cargo/bin:$PATH"
      hash -r 2>/dev/null || true
    fi
  fi

  # Ensure nightly is available (the build uses 'cargo +nightly')
  if ! rustup toolchain list 2>/dev/null | grep -q '^nightly'; then
    log "Installing Rust nightly toolchain"
    rustup toolchain install nightly --no-self-update --profile minimal --component rust-src
  fi

  # Optional: set default to stable (not required for build), keep nightly available
  if ! rustup default 2>/dev/null | grep -q stable; then
    rustup default stable >/dev/null 2>&1 || true
  fi

  # Final sanity print
  log "Rustup OK: $(rustup --version 2>/dev/null || echo 'not found'), cargo: $(command -v cargo || echo 'missing')"
}


build_and_install() {
  local SRC_DIR
  if [ -d .git ] && [ -f Cargo.toml ]; then
    SRC_DIR="$(pwd)"
  else
    SRC_DIR="$(mktemp -d)"
    log "Cloning microsoft/edit into $SRC_DIR"
    git clone --depth=1 https://github.com/microsoft/edit.git "$SRC_DIR"
  fi

  log "Building Edit (release)"
  CARGO_BIN="${HOME}/.cargo/bin/cargo"
  if [ ! -x "$CARGO_BIN" ]; then CARGO_BIN="$(command -v cargo)"; fi
  (cd "$SRC_DIR" && RUSTC_BOOTSTRAP=1 "$CARGO_BIN" +nightly build --config .cargo/release.toml --release)

  local BIN="$SRC_DIR/target/release/edit"
  [ -x "$BIN" ] || die "Build failed: $BIN not found"

  local DEST="/usr/local/bin"
  local DEST_USER="$HOME/.local/bin"
  if [ -n "$SUDO" ]; then
    log "Installing to $DEST"
    $SUDO install -Dm755 "$BIN" "$DEST/edit"
    $SUDO ln -sf "$DEST/edit" "$DEST/msedit"
  else
    mkdir -p "$DEST_USER"
    log "Installing to $DEST_USER (no sudo)"
    install -Dm755 "$BIN" "$DEST_USER/edit"
    ln -sf "$DEST_USER/edit" "$DEST_USER/msedit"
    if ! printf '%s' "$PATH" | tr ':' '\n' | grep -qx "$DEST_USER"; then
      warn "Add $DEST_USER to your PATH to run 'edit' or 'msedit' globally."
    fi
  fi

  log "Installed: $(command -v edit || true)  |  Version: $(edit --version 2>/dev/null || true)"
}

main() {
  log "Installing dependencies"
  install_pkgs
  log "Ensuring ICU unversioned symlinks exist"
  ensure_unversioned_icu_symlinks
  log "Ensuring Rust toolchain"
  install_rust
  log "Building and installing Edit"
  build_and_install
  log "Done. Run:  edit    (alias: msedit)"
}
main "$@"
