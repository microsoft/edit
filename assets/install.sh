#!/bin/sh
# shellcheck shell=dash

set -eu

log() { printf '\033[1;34m==>\033[0m %s\n' "$*"; }
warn() { printf '\033[1;33mwarning:\033[0m %s\n' "$*" >&2; }
die() { printf '\033[1;31merror:\033[0m %s\n' "$*" >&2; exit 1; }

usage() {
    cat <<'EOF'
Usage: install.sh [--dev] [--system]
  --dev     Build from the main branch instead of the latest release
  --system  Install to /usr/local/bin (requires sudo)

Without --system, installs to ~/.local/bin.
EOF
    exit 1
}

#### Parse arguments

dev=0
system=0
for arg in "$@"; do
    case "$arg" in
        --dev) dev=1 ;;
        --system) system=1 ;;
        -h|--help) usage ;;
        *) usage ;;
    esac
done

if [ "$system" = 1 ]; then
    command -v sudo >/dev/null 2>&1 || die "sudo is required for --system installs."
fi

#### Check prerequisites

command -v cargo >/dev/null 2>&1 || die "cargo not found. Install Rust via rustup (https://rustup.rs) or your OS package manager."

if command -v curl >/dev/null 2>&1; then
    download() { curl --proto '=https' --tlsv1.2 --retry 3 -fsSL -o "$1" "$2"; }
elif command -v wget >/dev/null 2>&1; then
    download() { wget --https-only --secure-protocol=TLSv1_2 -qO "$1" "$2"; }
else
    die "curl or wget not found. Install either via your OS package manager."
fi

tmpdir=$(mktemp -d)
trap 'rm -rf "$tmpdir"' EXIT

#### Find ICU SONAME

icuuc_soname=""
icui18n_soname=""
icu_cpp_exports=""
icu_renaming_version=""

case "$(uname -s)" in
    Darwin)
        ;;
    *)
        # Pick the best candidate SONAME
        # Preference: libicuuc.so.78 > libicuuc.so > libicuuc.so.78.1
        # (Symbols are usually suffixed with the major version, so that's preferred.)

        icu_ranked_paths=$tmpdir/icu_ranked_paths

        if command -v ldconfig >/dev/null 2>&1; then
            ldconfig -p 2>/dev/null | grep -o '/.*libicuuc\.so.*$'
        else
            find /usr/local/lib /usr/local/lib64 /usr/lib /usr/lib64 /lib /lib64 -maxdepth 2 -name 'libicuuc.so*' 2>/dev/null
        fi \
        | while IFS= read -r icuuc_path; do
            printf '%s %s\n' "${icuuc_path##*/}" "$icuuc_path"
        done \
        | sort -t. -k3,3n -k4,4n > "$icu_ranked_paths"

        major_entry=$(grep -E '^libicuuc\.so\.[0-9]+ ' "$icu_ranked_paths" | tail -n1 || true)
        bare_entry=$(grep -E '^libicuuc\.so ' "$icu_ranked_paths" | tail -n1 || true)
        full_entry=$(grep -E '^libicuuc\.so\.[0-9]+\.[0-9]+ ' "$icu_ranked_paths" | tail -n1 || true)

        if [ -n "$major_entry" ]; then icu_entry=$major_entry
        elif [ -n "$bare_entry" ]; then icu_entry=$bare_entry
        elif [ -n "$full_entry" ]; then icu_entry=$full_entry
        else die "libicuuc not found. Install ICU via your OS package manager (e.g. libicu78, libicu, icu)."
        fi

        icuuc_soname=${icu_entry%% *}
        icuuc_path=${icu_entry#* }
        icui18n_path="${icuuc_path%/*}/libicui18n.so${icuuc_soname#libicuuc.so}"
        [ -e "$icui18n_path" ] || die "libicui18n not found. Install ICU via your OS package manager (e.g. libicu78, libicu, icu)."
        icui18n_soname=${icui18n_path##*/}

        # Figure out the symbol naming scheme / renaming version

        if command -v readelf >/dev/null 2>&1; then
            icu_probe_symbol=$(readelf -Ws "$icuuc_path" 2>/dev/null | grep -Eo '_?u_errorName(_[0-9]+)?' | tail -n1 || true)
        elif command -v nm >/dev/null 2>&1; then
            icu_probe_symbol=$(nm -D "$icuuc_path" 2>/dev/null | grep -Eo '_?u_errorName(_[0-9]+)?' | tail -n1 || true)
        else
            icu_probe_symbol=
        fi

        case "$icu_probe_symbol" in
            _u_errorName|_u_errorName_[0-9]*) icu_cpp_exports=true ;;
        esac
        case "$icu_probe_symbol" in
            u_errorName_[0-9]*|_u_errorName_[0-9]*) icu_renaming_version=${icu_probe_symbol##*_} ;;
            *) ;;
        esac

        log_renaming=""
        log_cpp=""
        if [ -n "$icu_renaming_version" ]; then
            log_renaming=", renaming version $icu_renaming_version"
        fi
        if [ -n "$icu_cpp_exports" ]; then
            log_cpp=", C++ symbol exports"
        fi
        log "Found $icuuc_soname, $icui18n_soname$log_renaming$log_cpp"
        ;;
esac

#### Download source

if [ "$dev" = 1 ]; then
    log "Downloading main branch"
    download "$tmpdir/edit.tar.gz" 'https://github.com/microsoft/edit/archive/refs/heads/main.tar.gz'
else
    log "Fetching latest release tag"
    download "$tmpdir/latest.json" 'https://api.github.com/repos/microsoft/edit/releases/latest'
    tag=$(grep -oE '"tag_name": *"[^"]+"' "$tmpdir/latest.json" | grep -oE 'v[^"]+')
    [ -n "$tag" ] || die "Could not determine latest release tag."
    log "Latest release: $tag"
    download "$tmpdir/edit.tar.gz" "https://github.com/microsoft/edit/archive/refs/tags/$tag.tar.gz"
fi

srcdir="$tmpdir/edit-src"
mkdir -p "$srcdir"
log "Extracting"
tar xf "$tmpdir/edit.tar.gz" -C "$srcdir" --strip-components=1

#### Build

log "Building"
[ -z "$icuuc_soname" ] || export EDIT_CFG_ICUUC_SONAME="$icuuc_soname"
[ -z "$icui18n_soname" ] || export EDIT_CFG_ICUI18N_SONAME="$icui18n_soname"
[ -z "$icu_cpp_exports" ] || export EDIT_CFG_ICU_CPP_EXPORTS="$icu_cpp_exports"
[ -z "$icu_renaming_version" ] || export EDIT_CFG_ICU_RENAMING_VERSION="$icu_renaming_version"

if rustup component list --installed 2>/dev/null | grep -q rust-src; then
    (cd "$srcdir" && RUSTC_BOOTSTRAP=1 cargo build -p edit --release --config .cargo/release.toml)
else
    warn "rust-src component not found; building without size optimizations"
    (cd "$srcdir" && cargo build -p edit --release)
fi

bin="$srcdir/target/release/edit"
[ -x "$bin" ] || die "Build failed: binary not found."

#### Install

if [ "$system" = 1 ]; then
    dest=/usr/local/bin
    run="sudo"
else
    dest="$HOME/.local/bin"
    run=""
fi

log "Installing to $dest"
$run mkdir -p "$dest"
$run cp "$bin" "$dest/msedit"
$run chmod 755 "$dest/msedit"
if [ ! -e "$dest/edit" ] || [ "$(readlink "$dest/edit" 2>/dev/null)" = "msedit" ]; then
    $run ln -sf msedit "$dest/edit"
    edit_linked=1
else
    edit_linked=0
fi

#### Summary

case ":$PATH:" in
    *":$dest:"*)
        if [ "$edit_linked" = 1 ]; then
            echo "✅ Done. Run 'edit' or 'msedit' to start."
        else
            echo "✅ Done. Run 'msedit' to start."
        fi
        ;;
    *)
        echo "⚠️ Done. $dest is not in PATH; you may need to add it."
        if [ "$edit_linked" = 1 ]; then
            echo "Run '$dest/edit' or '$dest/msedit' to start."
        else
            echo "Run '$dest/msedit' to start."
        fi
        ;;
esac
