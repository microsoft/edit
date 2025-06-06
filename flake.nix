{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    rust-overlay,
  }: let
    system = "x86_64-linux";
    pkgs = import nixpkgs {
      inherit system;
      overlays = [rust-overlay.overlays.default];
    };

    nightlyToolchain = pkgs.rust-bin.selectLatestNightlyWith (
      toolchain:
        toolchain.default.override {
          extensions = [
            "rust-src"
            "clippy"
          ];
        }
    );

    rustPlatform = pkgs.makeRustPlatform {
      cargo = nightlyToolchain;
      rustc = nightlyToolchain;
    };
  in {
    devShells.default = pkgs.mkShell {
      buildInputs = with pkgs; [
        rustToolchain
        rustup
        pkg-config
        # Add other dependencies your project needs
      ];

      shellHook = ''
        echo "Rust nightly development environment"
        echo "Rust version: $(rustc --version)"
        echo "Cargo version: $(cargo --version)"
      '';
    };
    packages.${system}.default = rustPlatform.buildRustPackage {
      pname = "edit";
      version = "0.1.0";
      src = ./.;

      cargoLock = {
        lockFile = ./Cargo.lock;
      };

      nativeBuildInputs = with pkgs; [
        pkg-config
      ];

      meta = with pkgs.lib; {
        description = "Microsoft Edit - vi Mode?";
        license = licenses.mit;
        maintainers = ["sahilkmishra"];
      };
    };
  };
}
