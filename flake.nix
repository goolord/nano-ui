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

      flake.templates = {
        default = self.templates.sdl;
        sdl = {
          path = ./templates/sdl;
          description = "nano-ui desktop app with the SDL3 backend (TrueType fonts, dialogs)";
        };
        rgfw = {
          path = ./templates/rgfw;
          description = "nano-ui desktop app with the RGFW + OpenGL backend (no SDL3 required)";
        };
      };

      perSystem = { config, pkgs, ... }: {
        formatter = pkgs.nixpkgs-fmt;

        haskellProjects.default = {
          projectRoot = self;

          # Match the GHC 9.14 toolchain used in local cabal builds.
          basePackages = pkgs.haskell.packages.ghc914;

          # haskell-flake's default tools include cabal-install and
          # haskell-language-server.
          # mdbook builds the book (nano-ui-book-test runs it); python3 runs
          # the scripts under scripts/.
          devShell.mkShellArgs = {
            nativeBuildInputs = with pkgs; [ pkg-config mdbook python3 ];
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
