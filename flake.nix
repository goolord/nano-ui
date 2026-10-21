{
  description = "nano-ui: immediate-mode GUI toolkit for Haskell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };

  outputs = inputs @ { self, flake-parts, haskell-flake, nixpkgs, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      imports = [
        haskell-flake.flakeModule
      ];

      perSystem = { config, pkgs, ... }: {
        formatter = pkgs.nixpkgs-fmt;

        haskellProjects.default = {
          projectRoot = self;

          # Match the GHC 9.14 toolchain used in local cabal builds.
          basePackages = pkgs.haskell.packages.ghc914;

          # haskell-flake's default tools include cabal-install and
          # haskell-language-server.
          devShell.mkShellArgs = {
            nativeBuildInputs = with pkgs; [ pkg-config ];
            buildInputs = with pkgs; [ sdl3 sdl3-ttf ];
          };

          # packages, apps (demos), and checks (test suite)
          autoWire = [ "packages" "apps" "checks" ];
        };

        devShells.default = pkgs.mkShell {
          name = "nano-ui";
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
          ];
        };
      };
    };
}
