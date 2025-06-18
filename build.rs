// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use core::panic;
use std::env::VarError;

#[derive(PartialEq, Eq)]
enum TargetOs {
    Windows,
    MacOS,
    Unix,
}

fn main() {
    let target_os = match env_opt("CARGO_CFG_TARGET_OS").as_str() {
        "windows" => TargetOs::Windows,
        "macos" | "ios" => TargetOs::MacOS,
        _ => TargetOs::Unix,
    };
    let icuuc_soname = env_opt("EDIT_CFG_ICUUC_SONAME");
    let icui18n_soname = env_opt("EDIT_CFG_ICUI18N_SONAME");
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
            && renaming_version.is_empty()
    };
    if renaming_auto_detect && !renaming_version.is_empty() {
        panic!(
            "Either `EDIT_CFG_ICU_RENAMING_AUTO_DETECT` or `EDIT_CFG_ICU_RENAMING_VERSION` must be set, but not both"
        );
    }

    let icuuc_soname = if !icuuc_soname.is_empty() {
        &icuuc_soname
    } else {
        match target_os {
            TargetOs::Windows => "icuuc.dll",
            TargetOs::MacOS => "libicuuc.dylib",
            TargetOs::Unix => "libicuuc.so",
        }
    };
    let icui18n_soname = if !icui18n_soname.is_empty() {
        &icui18n_soname
    } else {
        match target_os {
            TargetOs::Windows => "icuin.dll",
            TargetOs::MacOS => "libicui18n.dylib",
            TargetOs::Unix => "libicui18n.so",
        }
    };
    let renaming_version =
        if renaming_version.is_empty() { String::new() } else { format!("_{renaming_version}") };

    println!("cargo::rerun-if-env-changed=EDIT_CFG_ICUUC_SONAME");
    println!("cargo::rustc-env=EDIT_CFG_ICUUC_SONAME={icuuc_soname}");
    println!("cargo::rerun-if-env-changed=EDIT_CFG_ICUI18N_SONAME");
    println!("cargo::rustc-env=EDIT_CFG_ICUI18N_SONAME={icui18n_soname}");
    println!("cargo::rerun-if-env-changed=EDIT_CFG_ICU_RENAMING_VERSION");
    println!("cargo::rustc-env=EDIT_CFG_ICU_RENAMING_VERSION={renaming_version}");
    println!("cargo::rerun-if-env-changed=EDIT_CFG_ICU_RENAMING_AUTO_DETECT");
    println!("cargo::rustc-check-cfg=cfg(edit_icu_renaming_auto_detect)");
    if renaming_auto_detect {
        println!("cargo::rustc-cfg=edit_icu_renaming_auto_detect");
    }

    #[cfg(windows)]
    if target_os == TargetOs::Windows {
        winresource::WindowsResource::new()
            .set_manifest_file("src/bin/edit/edit.exe.manifest")
            .set("FileDescription", "Microsoft Edit")
            .set("LegalCopyright", "© Microsoft Corporation. All rights reserved.")
            .set_icon("assets/edit.ico")
            .compile()
            .unwrap();
    }
}

fn env_opt(name: &str) -> String {
    match std::env::var(name) {
        Ok(value) => value,
        Err(VarError::NotPresent) => String::new(),
        Err(VarError::NotUnicode(_)) => {
            panic!("Environment variable `{name}` is not valid Unicode")
        }
    }
}
