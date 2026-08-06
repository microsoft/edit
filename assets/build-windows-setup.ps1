$ErrorActionPreference = "Stop"
$PSNativeCommandUseErrorActionPreference = $true

Set-Location (Split-Path -Parent $PSScriptRoot)

$env:RUSTC_BOOTSTRAP = 1
$config = "./.cargo/release.toml"

if (Get-Command "msrustup" -ErrorAction SilentlyContinue) {
    # The default C2/MSVC toolchain cannot compile this project.
    $env:MSRUSTUP_TOOLCHAIN = "ms-prod@llvm"
    $config = "./.cargo/release-windows-ms.toml"
}

# Extract the package version from Cargo.toml so we can stamp it into the installer.
$cargoToml = Get-Content -Raw -LiteralPath "crates/edit/Cargo.toml"
$versionMatch = [regex]::Match($cargoToml, '(?m)^version\s*=\s*"([^"]+)"')
if (!$versionMatch.Success) {
    throw "Failed to extract version from crates/edit/Cargo.toml"
}
$version = $versionMatch.Groups[1].Value

cargo build --config $config --release --target aarch64-pc-windows-msvc
cargo build --config $config --release --target x86_64-pc-windows-msvc

$iscc = "C:\Program Files (x86)\Inno Setup 6\ISCC.exe"
if (!(Test-Path $iscc)) {
    $iscc = "$env:LocalAppData\Programs\Inno Setup 6\ISCC.exe"
    if (!(Test-Path $iscc)) {
        throw "Please install Inno Setup 6: https://jrsoftware.org/isdl.php"
    }
}

& $iscc /DAppVersion=$version /DArchitecturesAllowed=arm64 /DSource=target\aarch64-pc-windows-msvc\release\edit.exe /Otarget /Fedit-$version-aarch64-windows-setup assets\edit.iss
& $iscc /DAppVersion=$version /DArchitecturesAllowed=x64os /DSource=target\x86_64-pc-windows-msvc\release\edit.exe  /Otarget /Fedit-$version-x86_64-windows-setup assets\edit.iss
