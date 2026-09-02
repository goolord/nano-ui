{
  description = "nano-ui: purely functional immediate-mode GUI for Haskell";

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

      perSystem = { config, pkgs, lib, ... }: {
        formatter = pkgs.nixpkgs-fmt;

        haskellProjects.default = {
          projectRoot = self;

          # Match the GHC 9.14 toolchain used in local cabal builds.
          compiler = "ghc914";
          basePackages = pkgs.haskell.packages.ghc914;

          defaults.settings = {
            imports = [ haskell-flake.modules.haskellProjects.default ];
            nano-ui-sdl.flags.sdl = lib.mkDefault true;
            nano-ui-term.flags.notcurses = lib.mkDefault true;
          };

          devShell = {
            tools = {
              cabal = "recommended";
              haskell-language-server = "recommended";
            };
            nativeBuildInputs = with pkgs; [
              pkg-config
            ];
            shellFor = {
              extraPackages = _hpkgs: with pkgs; [
                SDL3
                SDL3_ttf
                fontconfig
                notcurses # provides notcurses-core.pc for cabal pkg-config
              ];
            };
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
