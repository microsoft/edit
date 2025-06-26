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
    supportedSystems = ["x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin"];
    forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

    nixpkgsFor = forAllSystems (system:
      import nixpkgs {
        inherit system;
        overlays = [rust-overlay.overlays.default];
      }
    );
  in {
    devShells = forAllSystems (system: let
      pkgs = nixpkgsFor.${system};

      nightlyToolchain = pkgs.rust-bin.selectLatestNightlyWith (
        toolchain:
          toolchain.default.override {
            extensions = [
              "rust-src"
              "clippy"
            ];
          }
      );
    in {
      default = pkgs.mkShell {
        buildInputs = with pkgs; [
          nightlyToolchain
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
    });

    packages = forAllSystems (system: let
      pkgs = nixpkgsFor.${system};

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
      default = rustPlatform.buildRustPackage {
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
          description = "Microsoft Edit - A TUI editor";
          license = licenses.mit;
          maintainers = ["microsoft" "pmarreck"];
        };
      };
    });
  };
}
