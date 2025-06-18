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
    let target_os = match env_fallback("CARGO_CFG_TARGET_OS", "").as_str() {
        "windows" => TargetOs::Windows,
        "macos" | "ios" => TargetOs::MacOS,
        _ => TargetOs::Unix,
    };
    let icuuc_soname = env_fallback(
        "EDIT_CFG_ICUUC_SONAME",
        match target_os {
            TargetOs::Windows => "icuuc.dll",
            TargetOs::MacOS => "libicuuc.dylib",
            TargetOs::Unix => "libicuuc.so",
        },
    );
    let icui18n_soname = env_fallback(
        "EDIT_CFG_ICUI18N_SONAME",
        match target_os {
            TargetOs::Windows => "icuin.dll",
            TargetOs::MacOS => "libicui18n.dylib",
            TargetOs::Unix => "libicui18n.so",
        },
    );
    let renaming_auto_detect =
        env_fallback("EDIT_CFG_ICU_RENAMING_AUTO_DETECT", "false").parse::<bool>().unwrap();
    let renaming_version = {
        let mut v = env_fallback("EDIT_CFG_ICU_RENAMING_VERSION", "");
        if v.parse::<i32>().unwrap_or(0) > 0 {
            v.insert(0, '_');
        }
        v
    };

    if renaming_auto_detect && !renaming_version.is_empty() {
        panic!(
            "Either `EDIT_CFG_ICU_RENAMING_AUTO_DETECT` or `EDIT_CFG_ICU_RENAMING_VERSION` must be set, but not both"
        );
    }

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

fn env_fallback(name: &str, fallback: &str) -> String {
    match std::env::var(name) {
        Ok(value) => value,
        Err(VarError::NotPresent) => fallback.to_string(),
        Err(VarError::NotUnicode(_)) => {
            panic!("Environment variable `{name}` is not valid Unicode")
        }
    }
}
