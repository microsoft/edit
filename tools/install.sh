#!/usr/bin/env bash

# guard: ensure Linux + bash
[ "$(uname -s 2>/dev/null)" = "Linux" ] || {
  printf '\033[1;31mxx\033[0m This installer targets Linux.\n' >&2; exit 1; }
command -v bash >/dev/null 2>&1 || {
  printf '\033[1;31mxx\033[0m bash not found. Please install bash.\n' >&2; exit 1; }


set -Eeuo pipefail
umask 022
export LC_ALL=C

trap 'code=$?; line=${BASH_LINENO[0]:-}; cmd=${BASH_COMMAND:-}; printf "\033[1;31mxx\033[0m failed (exit %s) at line %s: %s\n" "$code" "$line" "$cmd" >&2' ERR

# Edit (Microsoft.Edit) Linux installer
# - Installs deps (build + ICU)
# - Ensures ICU can be loaded (system-wide symlinks when possible; user wrapper otherwise)
# - Installs rustup + nightly, builds, and installs to /usr/local/bin or ~/.local/bin

need_cmd() { command -v "$1" >/dev/null 2>&1; }
log() { printf '\033[1;34m==>\033[0m %s\n' "$*"; }
warn() { printf '\033[1;33m!!\033[0m %s\n' "$*"; }
die() { printf '\033[1;31mxx\033[0m %s\n' "$*"; exit 1; }
is_root() { [ "${EUID:-$(id -u)}" -eq 0 ]; }
run_root() {
  if is_root; then
    "$@"
  elif [ -n "${SUDO:-}" ]; then
    $SUDO "$@"
  else
    die "This step requires root. Re-run as root, with sudo/doas, or set EDIT_SKIP_DEPS=1 after installing dependencies manually."
  fi
}

SUDO=""
if [ "${EUID:-$(id -u)}" -ne 0 ]; then
  if need_cmd sudo; then SUDO="sudo"
  elif need_cmd doas; then SUDO="doas"
  else SUDO=""
  fi
fi

HAVE_ROOT=0
if is_root || [ -n "$SUDO" ]; then HAVE_ROOT=1; fi

PM=""
USE_COLOR=1
[ -t 1 ] && [ -z "${NO_COLOR:-}" ] || USE_COLOR=0
if [ "$USE_COLOR" -eq 0 ]; then
  log(){ printf '==> %s\n' "$*"; }
  warn(){ printf '!! %s\n' "$*"; }
  die(){ printf 'xx %s\n' "$*"; exit 1; }
fi

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

apt_update_if_stale() {
  if [ -d /var/lib/apt/lists ]; then
    local now=$(date +%s) newest=0 count=0 m
    while IFS= read -r -d '' f; do
      count=$((count+1))
      m=$(stat -c %Y "$f" 2>/dev/null || echo 0)
      [ "$m" -gt "$newest" ] && newest="$m"
    done < <(find /var/lib/apt/lists -type f -print0 2>/dev/null || true)
    if [ "$count" -eq 0 ] || [ "$newest" -lt $(( now - 21600 )) ]; then
      run_root env DEBIAN_FRONTEND=noninteractive apt-get update -y
    fi
  else
    run_root env DEBIAN_FRONTEND=noninteractive apt-get update -y
  fi
}

install_pkgs() {
  case "$PM" in
    apt)
      apt_update_if_stale
      run_root env DEBIAN_FRONTEND=noninteractive apt-get install -y -qq --no-install-recommends \
        build-essential pkg-config curl ca-certificates git libicu-dev
      ;;
    dnf)
      run_root dnf -y install @development-tools gcc gcc-c++ make \
        pkgconf-pkg-config curl ca-certificates git libicu-devel
      ;;
    yum)
      run_root yum -y groupinstall "Development Tools" || true
      run_root yum -y install gcc gcc-c++ make pkgconfig curl ca-certificates git libicu-devel
      ;;
    zypper)
      run_root zypper --non-interactive ref
      run_root zypper --non-interactive install -t pattern devel_basis || true
      run_root zypper --non-interactive install --no-recommends \
        gcc gcc-c++ make pkg-config curl ca-certificates git libicu-devel
      ;;
    pacman)
      # Full sync to avoid partial upgrades in scripted installs
      run_root pacman -Syu --noconfirm --needed --noprogressbar \
        base-devel icu curl ca-certificates git pkgconf
      ;;
    apk)
      # Alpine: icu-dev provides unversioned .so symlinks; keep it
      run_root apk add --no-cache \
        build-base pkgconf curl ca-certificates git icu-dev
      ;;
    xbps)
      run_root xbps-install -Sy -y \
        gcc clang make pkgconf curl ca-certificates git icu-devel
      ;;
    *)
      warn "Unknown or unsupported package manager. Skipping dependency installation."
      warn "Please ensure build tools, pkg-config, git, curl, and ICU dev/runtime are installed."
      ;;
  esac
}


# -------- ICU discovery helpers --------
# Return the directory containing the newest versioned lib for a given stem, or empty.
find_icu_libdir_for() {
  local stem="$1"
  if need_cmd ldconfig; then
    local p
    p="$(ldconfig -p 2>/dev/null | awk '/'"$stem"'\.so\./{print $NF}' | sort -V | tail -1 || true)"
    [ -n "$p" ] && { dirname -- "$p"; return 0; }
  fi
  local d
  for d in /usr/local/lib /usr/local/lib64 /usr/lib /usr/lib64 /lib /lib64 /usr/lib/*-linux-gnu /lib/*-linux-gnu /usr/lib32; do
    ls "$d/$stem.so."* >/dev/null 2>&1 && { printf '%s' "$d"; return 0; }
  done
  printf ''
}

# Build a colon-joined LD_LIBRARY_PATH fragment with unique dirs for uc/i18n/data.
build_icu_ldpath() {
  local dirs=() d seen=""
  for stem in libicuuc libicui18n libicudata; do
    d="$(find_icu_libdir_for "$stem")"
    [ -z "$d" ] && continue
    case ":$seen:" in *":$d:"*) : ;; *) dirs+=("$d"); seen="$seen:$d";; esac
  done
  (IFS=:; printf '%s' "${dirs[*]:-}")
}

# Create unversioned symlinks system-wide if allowed; return 0 on success, 1 otherwise.
ensure_system_icu_symlinks() {
  local icudir="$1" ok_all=0
  [ -n "$icudir" ] || return 1
  for lib in libicuuc libicui18n libicudata; do
    # Already present?
    if [ -e "$icudir/$lib.so" ] || [ -e "/usr/local/lib/$lib.so" ]; then
      continue
    fi
    # Find latest version
    local latest
    latest="$(ls "$icudir/$lib.so."* 2>/dev/null | sort -V | tail -1 || true)"
    if [ -n "$latest" ]; then
      if [ "$HAVE_ROOT" -eq 1 ]; then
        log "Creating unversioned symlink for $lib → $latest"
        target="$(readlink -f "$latest" 2>/dev/null || echo "$latest")"
        run_root install -d -m 0755 /usr/local/lib
        run_root ln -sf "$target" "/usr/local/lib/$lib.so"
        if need_cmd ldconfig && [ -z "${_EDIT_LDCONFIG_DONE:-}" ]; then
          run_root ldconfig; _EDIT_LDCONFIG_DONE=1
        fi
      else
        ok_all=1
      fi
    else
      ok_all=1
    fi
  done
  return $ok_all
}

# Create a user-local wrapper that exports LD_LIBRARY_PATH to ICU dir, then execs the binary
install_user_wrapper() {
  local bin="$1" icudir="$2" dst="$3"
  mkdir -p "$(dirname "$dst")"
  cat > "$dst" <<EOF
#!/usr/bin/env bash
set -euo pipefail
export LD_LIBRARY_PATH="${icudir}:\${LD_LIBRARY_PATH:-}"
exec -a edit "$bin" "\$@"
EOF
  chmod +x "$dst"
}

# -------- Rust setup --------
install_rust() {
  : "${CARGO_HOME:=$HOME/.cargo}"
  : "${RUSTUP_HOME:=$HOME/.rustup}"
  export CARGO_HOME RUSTUP_HOME
  export RUSTUP_INIT_SKIP_PATH_CHECK=yes
  if ! need_cmd rustup; then
    log "Installing Rust (rustup)"
    curl --proto '=https' --tlsv1.2 --retry 5 --retry-delay 2 -fsSL https://sh.rustup.rs | sh -s -- -y --profile minimal
  fi
  if [ -f "$HOME/.cargo/env" ]; then . "$HOME/.cargo/env"; fi
  hash -r 2>/dev/null || true

  # Prefer rustup's cargo for +nightly
  if [ -x "$HOME/.cargo/bin/cargo" ] && [ "$(command -v cargo)" != "$HOME/.cargo/bin/cargo" ]; then
    export PATH="$HOME/.cargo/bin:$PATH"
    hash -r 2>/dev/null || true
  fi

  if ! rustup toolchain list 2>/dev/null | grep -q '^nightly'; then
    log "Installing Rust nightly toolchain"
    rustup toolchain install nightly --no-self-update --profile minimal --component rust-src
  fi

  # Keep stable default (optional)
  rustup default stable >/dev/null 2>&1 || true

  # final check: ensure '+nightly' actually resolves
  if ! "$HOME/.cargo/bin/cargo" +nightly -V >/dev/null 2>&1; then
    warn "cargo (+nightly) resolution failed; diagnostics:"
    "$HOME/.cargo/bin/rustup" show 2>&1 | sed 's/^/    /'
    die "rustup cargo +nightly not usable; check PATH and rustup installation"
  fi
}

# -------- Build & install --------
build_and_install() {
  : "${EDIT_FORCE_WRAPPER:=0}"          # 1 = force user wrapper even with sudo
  : "${EDIT_SOURCE_URL:=https://github.com/microsoft/edit.git}"  # allow testing forks

  local SRC_DIR
  local CLEANUP=0
  _cleanup() {
    # safe under `set -u`
    if [ "${CLEANUP:-0}" -eq 1 ] && [ -n "${SRC_DIR:-}" ]; then
      rm -rf "$SRC_DIR"
    fi
  }
  trap _cleanup EXIT

  if [ -d .git ] && [ -f Cargo.toml ]; then
    SRC_DIR="$(pwd)"
  else
    SRC_DIR="$(mktemp -d)"
    CLEANUP=1
    log "Cloning microsoft/edit into $SRC_DIR"
    : "${EDIT_SOURCE_REF:=}"   # can be a tag, branch, or commit SHA
    export GIT_TERMINAL_PROMPT=0
    if [ -n "${EDIT_SOURCE_REF:-}" ]; then
      git -c http.lowSpeedLimit=1 -c http.lowSpeedTime=30 \
        clone --filter=blob:none --depth=1 --branch "$EDIT_SOURCE_REF" \
        "$EDIT_SOURCE_URL" "$SRC_DIR" || {
          log "Ref not a branch/tag; doing full clone to fetch commit"
          git -c http.lowSpeedLimit=1 -c http.lowSpeedTime=30 \
            clone --filter=blob:none "$EDIT_SOURCE_URL" "$SRC_DIR"
          (cd "$SRC_DIR" && git checkout --detach "$EDIT_SOURCE_REF")
        }
    else
      git -c http.lowSpeedLimit=1 -c http.lowSpeedTime=30 \
        clone --filter=blob:none --depth=1 "$EDIT_SOURCE_URL" "$SRC_DIR"
    fi
  fi

  log "Building Edit (release)"
  local CARGO_BIN="${HOME}/.cargo/bin/cargo"
  [ -x "$CARGO_BIN" ] || CARGO_BIN="$(command -v cargo || true)"
  [ -x "$CARGO_BIN" ] || die "cargo not found"
  (cd "$SRC_DIR" && RUSTC_BOOTSTRAP=1 "$CARGO_BIN" +nightly \
    build --config .cargo/release.toml --release ${EDIT_CARGO_ARGS:-})

  local BIN="$SRC_DIR/target/release/edit"
  [ -x "$BIN" ] || die "Build failed: $BIN not found"

  local DEST_SYS="${EDIT_PREFIX:-/usr/local}/bin"
  local DEST_USER="${EDIT_USER_PREFIX:-$HOME/.local}/bin"
  local OUT_BIN="" WRAPPER_NEEDED=0 ICU_DIR="" ICU_DIR_FIRST=""

  # Prefer build.rs artifact if present, else fall back to shell discovery
  local LDPATH_FILE=""
  LDPATH_FILE="$(find "$SRC_DIR/target" -type f -name '.edit.ldpath' | head -n1 || true)"
  if [ -n "$LDPATH_FILE" ] && [ -s "$LDPATH_FILE" ]; then
    ICU_DIR="$(tr -d '\n' < "$LDPATH_FILE" || true)"
    log "ICU (from build.rs): ${ICU_DIR:-<empty>}"
  else
    ICU_DIR="$(build_icu_ldpath || true)"
    [ -n "$ICU_DIR" ] && log "ICU (shell fallback): $ICU_DIR"
  fi

  if [ -z "$ICU_DIR" ]; then
    warn "ICU libraries not found; install ICU dev/runtime packages. Proceeding; wrapper will not help."
  else
    # First dir for symlink shim; keep full list for LD_LIBRARY_PATH wrappers
    ICU_DIR_FIRST="${ICU_DIR%%:*}"
  fi


  # Try to make system-wide ICU symlinks if we can
  if [ "$HAVE_ROOT" -eq 1 ] && [ -n "$ICU_DIR_FIRST" ]; then
    if ensure_system_icu_symlinks "$ICU_DIR_FIRST"; then
      log "System ICU symlinks OK."
    else
      warn "Could not create system ICU symlinks; will use user wrapper if installing locally."
      WRAPPER_NEEDED=1
    fi
  elif [ "$HAVE_ROOT" -eq 0 ] && [ -n "$ICU_DIR" ]; then
    WRAPPER_NEEDED=1
  fi

  if [ "${EDIT_FORCE_WRAPPER:-0}" -eq 1 ] && [ -n "$ICU_DIR" ]; then
    WRAPPER_NEEDED=1
  fi

  local DEST_SYS_DIR; DEST_SYS_DIR="$(dirname "$DEST_SYS")"
  if [ "$HAVE_ROOT" -eq 1 ] && run_root sh -lc "test -w '$DEST_SYS_DIR' -a -d '$DEST_SYS_DIR'"; then
    log "Installing to $DEST_SYS"
    if [ "$WRAPPER_NEEDED" -eq 1 ] && [ -n "$ICU_DIR" ]; then
      # Could not install the system-wide ICU shim; use a system-wide wrapper
      log "System ICU symlinks unavailable; installing wrapper that sets LD_LIBRARY_PATH"
      run_root install -Dm755 "$BIN" "${EDIT_PREFIX:-/usr/local}/libexec/edit-real"
      run_root bash -lc "cat > '$DEST_SYS/edit' <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
export LD_LIBRARY_PATH='"$ICU_DIR"':\${LD_LIBRARY_PATH:-}
exec -a edit '"${EDIT_PREFIX:-/usr/local}"'/libexec/edit-real "\$@"
EOF
chmod +x '$DEST_SYS/edit'"
      run_root ln -sf "$DEST_SYS/edit" "$DEST_SYS/msedit"
      OUT_BIN="$DEST_SYS/edit"
    else
      # Normal case: direct binary install (symlink shim present or not needed)
      run_root install -Dm755 "$BIN" "$DEST_SYS/edit"
      run_root ln -sf "$DEST_SYS/edit" "$DEST_SYS/msedit"
      OUT_BIN="$DEST_SYS/edit"
    fi
  else
    mkdir -p "$DEST_USER"
    if [ "$WRAPPER_NEEDED" -eq 1 ] && [ -n "$ICU_DIR" ]; then
      log "Installing user-local wrapper due to missing privileges for ICU shim"
      install -Dm755 "$BIN" "$DEST_USER/.edit-real"
      install_user_wrapper "$DEST_USER/.edit-real" "$ICU_DIR" "$DEST_USER/edit"
      ln -sf "$DEST_USER/edit" "$DEST_USER/msedit"
      OUT_BIN="$DEST_USER/edit"
    else
      log "Installing to $DEST_USER (no sudo)"
      install -Dm755 "$BIN" "$DEST_USER/edit"
      ln -sf "$DEST_USER/edit" "$DEST_USER/msedit"
      OUT_BIN="$DEST_USER/edit"
    fi
    if ! printf '%s' "$PATH" | tr ':' '\n' | grep -qx "$DEST_USER"; then
      warn "Add $DEST_USER to your PATH to run 'edit' globally."
    fi
  fi

  CLEANUP=0
  trap - EXIT

  log "Installed: $OUT_BIN"
  if [ -n "$OUT_BIN" ]; then
    log "Version: $("$OUT_BIN" --version 2>/dev/null || true)"
  else
    log "Version: $(edit --version 2>/dev/null || true)"
  fi

  # PATH check hint
  if [ -n "$OUT_BIN" ]; then
    case ":$PATH:" in
      *":$(dirname "$OUT_BIN"):"*) : ;;
      *) warn "Note: $(dirname "$OUT_BIN") is not in PATH for non-login shells." ;;
    esac
  fi
}

main() {
  if [ "${EDIT_SKIP_DEPS:-0}" != "1" ]; then
    log "Installing dependencies"
    install_pkgs
  else
    log "Skipping dependency installation (EDIT_SKIP_DEPS=1)"
    need_cmd curl || die "curl is required when EDIT_SKIP_DEPS=1"
    if [ ! -d .git ] || [ ! -f Cargo.toml ]; then
      need_cmd git || die "git is required to clone the source when EDIT_SKIP_DEPS=1"
    fi
  fi
  log "Ensuring Rust toolchain"
  install_rust
  log "Building and installing Edit"
  build_and_install
  log "Done. Run:  edit    (alias: msedit)"
}
main "$@"
