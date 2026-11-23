#!/usr/bin/env bash
# Build a nano-ui-sdl-web program with GHC's wasm backend and lay out a
# directory to serve: the page, the runtime, the wasm module, its JSFFI glue,
# the WASI shim the page loads it with, and the fallback font.
#
#   source ~/.ghc-wasm/env
#   export PKG_CONFIG_PATH=~/.nano-ui-wasi/lib/pkgconfig   # build-wasi-deps.sh
#   packages/nano-ui-sdl-web/scripts/build-web.sh [out-dir [cabal-target]]
#   python3 -m http.server -d <out-dir> 8000
#
# Run from anywhere in the repository. The out-dir defaults to dist-web/ at
# the repository root, and the target to the web backend's own demo. The
# target must be an executable built as a reactor module that exports
# nanoMain (see the nano-ui-sdl-web-demo stanza). Needs node and npm (both
# come with ghc-wasm-meta) for the JSFFI glue and the shim.
set -euo pipefail

root="$(git -C "$(dirname "$0")" rev-parse --show-toplevel)"
pkg="$root/packages/nano-ui-sdl-web"
out="${1:-$root/dist-web}"
target="${2:-nano-ui-sdl-web:exe:nano-ui-sdl-web-demo}"
project="$root/cabal.project.wasm"
shim_version="0.4.1"

command -v wasm32-wasi-cabal >/dev/null || { echo "no wasm32-wasi-cabal: source ~/.ghc-wasm/env first" >&2; exit 1; }

# -j1 keeps one GHC process at a time, as the native CI build does.
(cd "$root" && wasm32-wasi-cabal build -j1 --project-file="$project" "$target")
wasm="$(cd "$root" && wasm32-wasi-cabal list-bin --project-file="$project" "$target")"

mkdir -p "$out"
out="$(cd "$out" && pwd)"
cp "$pkg"/web/index.html "$pkg"/web/main.js "$pkg"/web/nano-ui-sdl-web.js "$out"/
# The fallback font the default options load, with its licence.
mkdir -p "$out/fonts"
cp "$pkg"/web/fonts/*.ttf "$out/fonts/"
cp "$pkg"/LICENSE-Inter "$out/fonts/"
# The glue the module's JavaScript imports need, generated from the module
# before wasm-opt drops the custom section it is read from.
"$(wasm32-wasi-ghc --print-libdir)/post-link.mjs" -i "$wasm" -o "$out/ghc_wasm_jsffi.js"
if command -v wasm-opt >/dev/null; then
  wasm-opt --strip-debug -O2 "$wasm" -o "$out/main.wasm"
else
  cp "$wasm" "$out/main.wasm"
fi

# The WASI shim main.js imports, from npm, with its licences.
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT
(cd "$tmp" && npm pack --silent "@bjorn3/browser_wasi_shim@$shim_version" >/dev/null && tar xzf ./*.tgz)
rm -rf "$out/browser_wasi_shim"
mkdir -p "$out/browser_wasi_shim"
cp "$tmp"/package/dist/*.js "$tmp"/package/LICENSE-* "$out/browser_wasi_shim/"

echo "Built $out. Serve it with:"
echo "  python3 -m http.server -d $out 8000"
