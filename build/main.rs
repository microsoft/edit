// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#![allow(irrefutable_let_patterns)]

use crate::helpers::env_opt;
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};

mod helpers;
mod i18n;

#[derive(Clone, Copy, PartialEq, Eq)]
enum TargetOs {
    Windows,
    MacOS,
    Unix,
}

// ---- ICU discovery for installer & source builds ---------------------------
fn dedup_join(mut v: Vec<PathBuf>) -> String {
    v.sort();
    v.dedup();
    let parts: Vec<String> = v.into_iter().map(|p| p.display().to_string()).collect();
    parts.join(":")
}

fn try_pkg_config() -> Vec<PathBuf> {
    let mut dirs = Vec::new();
    for name in ["icu-uc", "icu-i18n", "icu-data"] {
        match pkg_config::Config::new().print_system_libs(false).probe(name) {
            Ok(lib) => dirs.extend(lib.link_paths.clone()),
            Err(_) => {}
        }
    }
    dirs
}

fn try_fs_latest_for(stem: &str, roots: &[&str]) -> Option<PathBuf> {
    // Find lib<stem>.so.* and return its parent dir
    for d in roots {
        let dir = Path::new(d);
        if !dir.is_dir() { continue; }
        // A simple lexicographic sort is good enough for .so.N versions
        let mut candidates: Vec<PathBuf> = match fs::read_dir(dir) {
            Ok(it) => it.filter_map(|e| e.ok().map(|e| e.path()))
                        .filter(|p| p.file_name()
                                     .and_then(|s| s.to_str())
                                     .map(|n| n.starts_with(&format!("lib{stem}.so.")))
                                     .unwrap_or(false))
                        .collect(),
            Err(_) => continue,
        };
        candidates.sort();
        if let Some(path) = candidates.last() {
            return path.parent().map(|p| p.to_path_buf());
        }
    }
    None
}

fn try_fs_scan() -> Vec<PathBuf> {
    let roots = [
        "/usr/local/lib", "/usr/local/lib64",
        "/usr/lib", "/usr/lib64", "/lib", "/lib64",
        "/usr/lib32",
        "/usr/lib/x86_64-linux-gnu", "/lib/x86_64-linux-gnu",
        "/usr/lib/aarch64-linux-gnu", "/lib/aarch64-linux-gnu",
        "/usr/lib/arm-linux-gnueabihf", "/lib/arm-linux-gnueabihf",
    ];
    let mut dirs = Vec::new();
    for stem in ["icuuc", "icui18n", "icudata"] {
        if let Some(d) = try_fs_latest_for(stem, &roots) {
            dirs.push(d);
        }
    }
    dirs
}

fn write_icu_ldpath_artifact() {
    // 1) gather ICU dirs (prefer pkg-config)
    let mut dirs = try_pkg_config();
    if dirs.is_empty() {
        dirs = try_fs_scan();
    }

    // 2) write ${OUT_DIR}/.edit.ldpath (empty file if not found)
    let out_dir = std::env::var("OUT_DIR").expect("OUT_DIR not set");
    let ldfile = Path::new(&out_dir).join(".edit.ldpath");
    let joined = dedup_join(dirs);
    // Create the file regardless (lets the installer detect the “not found” case)
    let mut f = fs::File::create(&ldfile).expect("create .edit.ldpath");
    if !joined.is_empty() {
        let _ = writeln!(f, "{}", joined);
        // Also export for optional runtime hints
        println!("cargo:rustc-env=EDIT_BUILD_ICU_LDPATH={}", joined);
        println!("cargo:warning=edit: using ICU from {}", joined);
    } else {
        // Leave it empty; installer will fall back to its own detection
        println!("cargo:warning=edit: ICU not found by build script");
    }
    // Re-run if we change this file
    println!("cargo:rerun-if-changed=build/main.rs");
    println!("cargo:rerun-if-env-changed=PKG_CONFIG_PATH");
}


fn main() {
    let target_os = match env_opt("CARGO_CFG_TARGET_OS").as_str() {
        "windows" => TargetOs::Windows,
        "macos" | "ios" => TargetOs::MacOS,
        _ => TargetOs::Unix,
    };

    // Always produce ICU ldpath artifact for installer & source builds
    write_icu_ldpath_artifact();
    compile_i18n();
    configure_icu(target_os);
    #[cfg(windows)]
    configure_windows_binary(target_os);
}

fn compile_i18n() {
    const PATH: &str = "i18n/edit.toml";

    let i18n = std::fs::read_to_string(PATH).unwrap();
    let contents = i18n::generate(&i18n);
    let out_dir = env_opt("OUT_DIR");
    let path = format!("{out_dir}/i18n_edit.rs");
    std::fs::write(path, contents).unwrap();

    println!("cargo::rerun-if-env-changed=EDIT_CFG_LANGUAGES");
    println!("cargo::rerun-if-changed={PATH}");
}

fn configure_icu(target_os: TargetOs) {
    let icuuc_soname = env_opt("EDIT_CFG_ICUUC_SONAME");
    let icui18n_soname = env_opt("EDIT_CFG_ICUI18N_SONAME");
    let cpp_exports = env_opt("EDIT_CFG_ICU_CPP_EXPORTS");
    let renaming_version = env_opt("EDIT_CFG_ICU_RENAMING_VERSION");
    let renaming_auto_detect = env_opt("EDIT_CFG_ICU_RENAMING_AUTO_DETECT");

    // If none of the `EDIT_CFG_ICU*` environment variables are set,
    // we default to enabling `EDIT_CFG_ICU_RENAMING_AUTO_DETECT` on UNIX.
    // This slightly improves portability at least in the cases where the SONAMEs match our defaults.
    let renaming_auto_detect = if !renaming_auto_detect.is_empty() {
        renaming_auto_detect.parse::<bool>().unwrap()
    } else {
        target_os == TargetOs::Unix
            && icuuc_soname.is_empty()
            && icui18n_soname.is_empty()
            && cpp_exports.is_empty()
            && renaming_version.is_empty()
    };
    if renaming_auto_detect && !renaming_version.is_empty() {
        // It makes no sense to specify an explicit version and also ask for auto-detection.
        panic!(
            "Either `EDIT_CFG_ICU_RENAMING_AUTO_DETECT` or `EDIT_CFG_ICU_RENAMING_VERSION` must be set, but not both"
        );
    }

    let icuuc_soname = if !icuuc_soname.is_empty() {
        &icuuc_soname
    } else {
        match target_os {
            TargetOs::Windows => "icuuc.dll",
            TargetOs::MacOS => "libicucore.dylib",
            TargetOs::Unix => "libicuuc.so",
        }
    };
    let icui18n_soname = if !icui18n_soname.is_empty() {
        &icui18n_soname
    } else {
        match target_os {
            TargetOs::Windows => "icuin.dll",
            TargetOs::MacOS => "libicucore.dylib",
            TargetOs::Unix => "libicui18n.so",
        }
    };
    let icu_export_prefix =
        if !cpp_exports.is_empty() && cpp_exports.parse::<bool>().unwrap() { "_" } else { "" };
    let icu_export_suffix =
        if !renaming_version.is_empty() { format!("_{renaming_version}") } else { String::new() };

    println!("cargo::rerun-if-env-changed=EDIT_CFG_ICUUC_SONAME");
    println!("cargo::rustc-env=EDIT_CFG_ICUUC_SONAME={icuuc_soname}");
    println!("cargo::rerun-if-env-changed=EDIT_CFG_ICUI18N_SONAME");
    println!("cargo::rustc-env=EDIT_CFG_ICUI18N_SONAME={icui18n_soname}");
    println!("cargo::rerun-if-env-changed=EDIT_CFG_ICU_EXPORT_PREFIX");
    println!("cargo::rustc-env=EDIT_CFG_ICU_EXPORT_PREFIX={icu_export_prefix}");
    println!("cargo::rerun-if-env-changed=EDIT_CFG_ICU_EXPORT_SUFFIX");
    println!("cargo::rustc-env=EDIT_CFG_ICU_EXPORT_SUFFIX={icu_export_suffix}");
    println!("cargo::rerun-if-env-changed=EDIT_CFG_ICU_RENAMING_AUTO_DETECT");
    println!("cargo::rustc-check-cfg=cfg(edit_icu_renaming_auto_detect)");
    if renaming_auto_detect {
        println!("cargo::rustc-cfg=edit_icu_renaming_auto_detect");
    }
}

#[cfg(windows)]
fn configure_windows_binary(target_os: TargetOs) {
    if target_os != TargetOs::Windows {
        return;
    }

    const PATH: &str = "src/bin/edit/edit.exe.manifest";
    println!("cargo::rerun-if-changed={PATH}");
    winresource::WindowsResource::new()
        .set_manifest_file(PATH)
        .set("FileDescription", "Microsoft Edit")
        .set("LegalCopyright", "© Microsoft Corporation. All rights reserved.")
        .set_icon("assets/edit.ico")
        .compile()
        .unwrap();
}
