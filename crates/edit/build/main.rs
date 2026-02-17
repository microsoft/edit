// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#![allow(irrefutable_let_patterns)]

use crate::helpers::env_opt;

mod helpers;
mod i18n;

fn main() {
    stdext::arena::init(128 * 1024 * 1024).unwrap();

    compile_i18n();
    configure_icu();
}

fn compile_i18n() {
    let i18n_path = "../../i18n/ie.toml";

    let i18n = std::fs::read_to_string(i18n_path).unwrap();
    let contents = i18n::generate(&i18n);
    let out_dir = env_opt("OUT_DIR");
    let path = format!("{out_dir}/i18n_ie.rs");
    std::fs::write(&path, contents).unwrap();

    println!("cargo::rerun-if-env-changed=EDIT_CFG_LANGUAGES");
    println!("cargo::rerun-if-changed={i18n_path}");
}

fn configure_icu() {
    let icuuc_soname = env_opt("EDIT_CFG_ICUUC_SONAME");
    let icui18n_soname = env_opt("EDIT_CFG_ICUI18N_SONAME");
    let cpp_exports = env_opt("EDIT_CFG_ICU_CPP_EXPORTS");
    let renaming_version = env_opt("EDIT_CFG_ICU_RENAMING_VERSION");

    let icuuc_soname = if !icuuc_soname.is_empty() {
        &icuuc_soname
    } else {
        "libicucore.dylib"
    };
    let icui18n_soname = if !icui18n_soname.is_empty() {
        &icui18n_soname
    } else {
        "libicucore.dylib"
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
}
