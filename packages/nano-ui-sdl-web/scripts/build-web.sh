#!/usr/bin/env bash
# Build the browser demo with GHC's wasm backend and lay out a directory to
# serve: the page, the runtime, the wasm module and its JSFFI glue.
#
#   source ~/.ghc-wasm/env
#   export PKG_CONFIG_PATH=~/.nano-ui-wasi/lib/pkgconfig   # build-wasi-deps.sh
#   packages/nano-ui-sdl-web/scripts/build-web.sh [out-dir]
#   python3 -m http.server -d <out-dir> 8000
#
# Run from anywhere in the repository. The out-dir defaults to
# dist-web/ at the repository root.
set -euo pipefail

root="$(git -C "$(dirname "$0")" rev-parse --show-toplevel)"
pkg="$root/packages/nano-ui-sdl-web"
out="${1:-$root/dist-web}"
target="nano-ui-sdl-web:exe:nano-ui-sdl-web-demo"
project="$root/cabal.project.wasm"

command -v wasm32-wasi-cabal >/dev/null || { echo "no wasm32-wasi-cabal: source ~/.ghc-wasm/env first" >&2; exit 1; }

(cd "$root" && wasm32-wasi-cabal build --project-file="$project" "$target")
wasm="$(cd "$root" && wasm32-wasi-cabal list-bin --project-file="$project" "$target")"

mkdir -p "$out"
cp "$pkg"/web/index.html "$pkg"/web/main.js "$pkg"/web/nano-ui-sdl-web.js "$out"/
cp "$wasm" "$out/nano-ui-sdl-web-demo.wasm"
# The glue the module's JavaScript imports need, generated from the module.
"$(wasm32-wasi-ghc --print-libdir)/post-link.mjs" -i "$wasm" -o "$out/ghc_wasm_jsffi.js"

echo "Built $out. Serve it with:"
echo "  python3 -m http.server -d $out 8000"
