// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

fn main() {
    #[cfg(windows)]
    if std::env::var("CARGO_CFG_TARGET_OS").unwrap_or_default() == "windows" {
        winres::WindowsResource::new()
            .set_manifest_file("src/bin/edit/edit.exe.manifest")
            .set("FileDescription", "Microsoft Edit")
            .set("LegalCopyright", "© Microsoft Corporation. All rights reserved.")
            .compile()
            .unwrap();
    }
}
