#!/usr/bin/env bash
# Build FreeType, HarfBuzz, SDL3 and SDL3_ttf for wasm32-wasi with the C
# toolchain of GHC's wasm backend, and install them with pkg-config files
# that cabal can link statically.
#
#   source ~/.ghc-wasm/env          # from ghc-wasm-meta
#   packages/nano-ui-sdl-web/scripts/build-wasi-deps.sh [prefix]
#
# The prefix defaults to ~/.nano-ui-wasi. Sources are downloaded into
# $prefix/src. Needs git, cmake, ninja and pkg-config.
#
# SDL has no WASI platform. SDL3 is configured as a console Unix build with
# every subsystem SDL_ttf does not use turned off, and no threads: SDL_ttf
# needs only SDL's surfaces, IO streams and allocator. Whether SDL builds so
# is what this script finds out first; see the README's "Status".
set -euo pipefail

prefix="${1:-$HOME/.nano-ui-wasi}"
src="$prefix/src"
build="$prefix/build"
mkdir -p "$src" "$build"

cc="${CC:-wasm32-wasi-clang}"
cxx="${CXX:-wasm32-wasi-clang++}"
ar="${AR:-wasm32-wasi-ar}"
ranlib="${RANLIB:-wasm32-wasi-ranlib}"
command -v "$cc" >/dev/null || { echo "no $cc: source ~/.ghc-wasm/env first" >&2; exit 1; }

# wasi-libc keeps signals, mmap, getpid and process clocks behind these.
emulated="-D_WASI_EMULATED_SIGNAL -D_WASI_EMULATED_MMAN -D_WASI_EMULATED_GETPID -D_WASI_EMULATED_PROCESS_CLOCKS"
emulated_libs="-lwasi-emulated-signal -lwasi-emulated-mman -lwasi-emulated-getpid -lwasi-emulated-process-clocks"

fetch() { # name url tag
  if [ ! -d "$src/$1" ]; then
    git clone --depth 1 --branch "$3" "$2" "$src/$1"
  fi
}

cmake_wasi() { # name source-dir extra-args...
  local name="$1" dir="$2"
  shift 2
  cmake -S "$dir" -B "$build/$name" -G Ninja \
    -DCMAKE_SYSTEM_NAME=WASI \
    -DCMAKE_SYSTEM_PROCESSOR=wasm32 \
    -DCMAKE_C_COMPILER="$cc" \
    -DCMAKE_CXX_COMPILER="$cxx" \
    -DCMAKE_AR="$(command -v "$ar")" \
    -DCMAKE_RANLIB="$(command -v "$ranlib")" \
    -DCMAKE_C_FLAGS="$emulated" \
    -DCMAKE_CXX_FLAGS="$emulated -fno-exceptions -DHB_NO_MT" \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_INSTALL_PREFIX="$prefix" \
    -DCMAKE_PREFIX_PATH="$prefix" \
    -DCMAKE_FIND_ROOT_PATH="$prefix" \
    -DBUILD_SHARED_LIBS=OFF \
    "$@"
  cmake --build "$build/$name"
  cmake --install "$build/$name"
}

fetch freetype https://gitlab.freedesktop.org/freetype/freetype.git VER-2-13-3
fetch harfbuzz https://github.com/harfbuzz/harfbuzz.git 10.4.0
fetch SDL https://github.com/libsdl-org/SDL.git release-3.2.26
fetch SDL_ttf https://github.com/libsdl-org/SDL_ttf.git release-3.2.2

cmake_wasi freetype "$src/freetype" \
  -DFT_DISABLE_ZLIB=ON -DFT_DISABLE_BZIP2=ON -DFT_DISABLE_PNG=ON \
  -DFT_DISABLE_HARFBUZZ=ON -DFT_DISABLE_BROTLI=ON

cmake_wasi harfbuzz "$src/harfbuzz" \
  -DHB_HAVE_FREETYPE=ON -DHB_BUILD_SUBSET=OFF -DHB_BUILD_UTILS=OFF -DHB_HAVE_GLIB=OFF \
  -DHB_HAVE_ICU=OFF -DHB_HAVE_GOBJECT=OFF -DHB_BUILD_TESTS=OFF

cmake_wasi SDL "$src/SDL" \
  -DSDL_STATIC=ON -DSDL_SHARED=OFF -DSDL_TEST_LIBRARY=OFF -DSDL_TESTS=OFF -DSDL_EXAMPLES=OFF \
  -DSDL_UNIX_CONSOLE_BUILD=ON -DSDL_THREADS=OFF \
  -DSDL_AUDIO=OFF -DSDL_JOYSTICK=OFF -DSDL_HAPTIC=OFF -DSDL_HIDAPI=OFF -DSDL_SENSOR=OFF \
  -DSDL_POWER=OFF -DSDL_CAMERA=OFF -DSDL_DIALOG=OFF -DSDL_GPU=OFF -DSDL_TRAY=OFF \
  -DSDL_DUMMYVIDEO=ON -DSDL_OFFSCREEN=OFF -DSDL_OPENGL=OFF -DSDL_OPENGLES=OFF -DSDL_VULKAN=OFF \
  -DSDL_LIBC=ON

cmake_wasi SDL_ttf "$src/SDL_ttf" \
  -DSDLTTF_VENDORED=OFF -DSDLTTF_HARFBUZZ=ON -DSDLTTF_PLUTOSVG=OFF -DSDLTTF_SAMPLES=OFF

# cabal asks pkg-config for --libs without --static, so fold each file's
# private requirements and libraries into its public ones, and add what a
# static link of these needs from wasi-sdk: the WASI emulation libraries and
# HarfBuzz's C++ runtime.
for pc in "$prefix"/lib/pkgconfig/*.pc; do
  awk -v extra_sdl="$emulated_libs" -v name="$(basename "$pc" .pc)" '
    /^Requires\.private:/ { sub(/^Requires\.private:/, ""); reqp = reqp " " $0; next }
    /^Libs\.private:/ { sub(/^Libs\.private:/, ""); libp = libp " " $0; next }
    { lines[++n] = $0 }
    END {
      for (i = 1; i <= n; i++) {
        line = lines[i]
        if (line ~ /^Requires:/) { line = line reqp; reqp = "" }
        if (line ~ /^Libs:/) {
          line = line libp
          if (name == "sdl3") line = line " " extra_sdl
          if (name == "harfbuzz") line = line " -lc++ -lc++abi"
          libp = ""
        }
        print line
      }
      if (reqp != "") print "Requires:" reqp
    }' "$pc" > "$pc.tmp"
  mv "$pc.tmp" "$pc"
done

echo
echo "Installed in $prefix. Build the demo with:"
echo "  PKG_CONFIG_PATH=$prefix/lib/pkgconfig packages/nano-ui-sdl-web/scripts/build-web.sh"
