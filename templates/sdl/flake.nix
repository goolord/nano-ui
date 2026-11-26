{
  description = "nano-ui desktop application (SDL3 backend)";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { nixpkgs, ... }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (s: f nixpkgs.legacyPackages.${s});
    in
    {
      devShells = forAllSystems (pkgs: {
        default = pkgs.mkShell {
          nativeBuildInputs = with pkgs; [
            haskell.compiler.ghc914
            cabal-install
            pkg-config
          ];
          buildInputs = with pkgs; [
            sdl3
            sdl3-ttf
          ];
        };
      });
    };
}
