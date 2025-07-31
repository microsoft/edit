// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#![allow(irrefutable_let_patterns)]

use std::collections::{BTreeMap, HashMap, HashSet};
use std::env::VarError;
use std::io::Write as _;

#[derive(Clone, Copy, PartialEq, Eq)]
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

    compile_i18n();
    configure_icu(target_os);
    #[cfg(windows)]
    configure_windows_binary(target_os);
}

fn compile_i18n() {
    const PATH: &str = "i18n/edit.toml";

    let i18n = std::fs::read_to_string(PATH).unwrap();
    let i18n = toml_span::parse(&i18n).expect("Failed to parse i18n file");
    let root = i18n.as_table().unwrap();
    let mut languages = Vec::new();
    let mut aliases = Vec::new();
    let mut translations: BTreeMap<String, HashMap<String, String>> = BTreeMap::new();

    for (k, v) in root.iter() {
        match &k.name[..] {
            "__default__" => {
                const ERROR: &str = "i18n: __default__ must be [str]";
                languages = Vec::from_iter(
                    v.as_array()
                        .expect(ERROR)
                        .iter()
                        .map(|lang| lang.as_str().expect(ERROR).to_string()),
                );
            }
            "__alias__" => {
                const ERROR: &str = "i18n: __alias__ must be str->str";
                aliases.extend(v.as_table().expect(ERROR).iter().map(|(alias, lang)| {
                    (alias.to_string(), lang.as_str().expect(ERROR).to_string())
                }));
            }
            _ => {
                const ERROR: &str = "i18n: LocId must be str->str";
                translations.insert(
                    k.name.to_string(),
                    HashMap::from_iter(
                        v.as_table().expect(ERROR).iter().map(|(k, v)| {
                            (k.name.to_string(), v.as_str().expect(ERROR).to_string())
                        }),
                    ),
                );
            }
        }
    }

    // Use EDIT_CFG_LANGUAGES for the language list if it is set.
    if let cfg_languages = env_opt("EDIT_CFG_LANGUAGES")
        && !cfg_languages.is_empty()
    {
        languages = cfg_languages.split(',').map(|lang| lang.to_string()).collect();
    }

    // Ensure English as the fallback language is always present.
    if !languages.iter().any(|l| l == "en") {
        languages.push("en".to_string());
    }

    // Normalize language tags for use in source code (i.e. no "-").
    for lang in &mut languages {
        if lang.is_empty() {
            panic!("i18n: empty language tag");
        }
        for c in unsafe { lang.as_bytes_mut() } {
            *c = match *c {
                b'A'..=b'Z' | b'a'..=b'z' => c.to_ascii_lowercase(),
                b'-' => b'_',
                b'_' => b'_',
                _ => panic!("i18n: language tag \"{lang}\" must be [a-zA-Z_-]"),
            }
        }
    }

    // * Validate that there are no duplicate language tags.
    // * Validate that all language tags are valid.
    // * Merge the aliases into the languages list.
    let mut languages_with_aliases: Vec<_>;
    {
        let mut specified = HashSet::new();
        for lang in &languages {
            if !specified.insert(lang.as_str()) {
                panic!("i18n: duplicate language tag \"{lang}\"");
            }
        }

        let mut available = HashSet::new();
        for v in translations.values() {
            for lang in v.keys() {
                available.insert(lang.as_str());
            }
        }

        let mut invalid = Vec::new();
        for lang in &languages {
            if !available.contains(lang.as_str()) {
                invalid.push(lang.as_str());
            }
        }
        if !invalid.is_empty() {
            panic!("i18n: invalid language tags {invalid:?}");
        }

        languages_with_aliases = languages.iter().map(|l| (l.clone(), l.clone())).collect();
        for (alias, lang) in aliases {
            if specified.contains(lang.as_str()) && !specified.contains(alias.as_str()) {
                languages_with_aliases.push((alias, lang));
            }
        }
    }

    // Sort languages by:
    // - "en" first, because it'll map to `LangId::en == 0`, which is the default.
    // - then alphabetically
    // - but tags with subtags (e.g. "zh_hans") before those without (e.g. "zh").
    {
        fn sort(a: &String, b: &String) -> std::cmp::Ordering {
            match (a == "en", b == "en") {
                (true, false) => std::cmp::Ordering::Less,
                (false, true) => std::cmp::Ordering::Greater,
                _ => {
                    let (a0, a1) = a.split_once('_').unwrap_or((a, "xxxxxx"));
                    let (b0, b1) = b.split_once('_').unwrap_or((b, "xxxxxx"));
                    match a0.cmp(b0) {
                        std::cmp::Ordering::Equal => a1.cmp(b1),
                        ord => ord,
                    }
                }
            }
        }
        languages.sort_unstable_by(sort);
        languages_with_aliases.sort_unstable_by(|a, b| sort(&a.0, &b.0));
    }

    // Generate the source code for the i18n data.
    {
        let out_dir = env_opt("OUT_DIR");
        let mut out = std::fs::File::create(format!("{out_dir}/i18n_edit.rs")).unwrap();
        let mut writer = std::io::BufWriter::new(&mut out);

        _ = write!(
            writer,
            "// This file is generated by build.rs. Do not edit it manually.\n\
        \n\
        #[derive(Clone, Copy, PartialEq, Eq)]\n\
        pub enum LocId {{\n",
        );

        for (k, _) in translations.iter() {
            _ = writeln!(writer, "    {k},");
        }

        _ = write!(
            writer,
            "}}\n\
        \n\
        #[allow(non_camel_case_types)]\n\
        #[derive(Clone, Copy, PartialEq, Eq)]\n\
        pub enum LangId {{\n",
        );

        for lang in &languages {
            _ = writeln!(writer, "    {lang},");
        }

        _ = write!(
            writer,
            "}}\n\
        \n\
        const LANGUAGES: &[(&str, LangId)] = &[\n"
        );

        for (alias, lang) in &languages_with_aliases {
            _ = writeln!(writer, "    ({alias:?}, LangId::{lang}),");
        }

        _ = write!(
            writer,
            "];\n\
        \n\
        const TRANSLATIONS: [[&str; {}]; {}] = [\n",
            translations.len(),
            languages.len(),
        );

        for lang in &languages {
            _ = writeln!(writer, "    [");
            for (_, v) in translations.iter() {
                const DEFAULT: &String = &String::new();
                let v = v.get(lang).or_else(|| v.get("en")).unwrap_or(DEFAULT);
                _ = writeln!(writer, "        {v:?},");
            }
            _ = writeln!(writer, "    ],");
        }

        _ = writeln!(writer, "];");
    }

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

fn env_opt(name: &str) -> String {
    match std::env::var(name) {
        Ok(value) => value,
        Err(VarError::NotPresent) => String::new(),
        Err(VarError::NotUnicode(_)) => {
            panic!("Environment variable `{name}` is not valid Unicode")
        }
    }
}
