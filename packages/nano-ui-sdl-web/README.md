# nano-ui-sdl-web

Browser backend for [nano-ui](https://github.com/goolord/nano-ui), built with
GHC's WebAssembly backend. Text goes through the same SDL_ttf font engine as
[nano-ui-sdl](../nano-ui-sdl): SDL_ttf shapes lines with HarfBuzz and
rasterises glyphs with FreeType into a glyph atlas laid out as on the desktop.
WebGL2 draws the frame's triangles with the blending and linear filtering that
`SDL_RenderGeometry` uses in nano-ui-sdl. The two backends' pixels have not
been compared yet.

```haskell
{-# LANGUAGE CPP, OverloadedStrings #-}

import NanoUI
import NanoUI.Backend.SdlWeb (defaultSdlWebOptions, runSdlWebApp)

main :: IO ()
main = runSdlWebApp defaultSdlWebOptions (label "Hello")

#if defined(wasm32_HOST_ARCH)
foreign export javascript "nanoMain" main :: IO ()
#endif
```

`runSdlWebAppReduce` takes a `model -> NanoUIE msg ()` view and an update
function, as `runSdlAppReduce` does. `SdlWebOptions` sets the title
(`webWindowSettings`), the theme or a theme that follows the page's
`prefers-color-scheme` (`webThemeFor`), fonts, the base font size, and a UI
scale on top of the device pixel ratio.

## Fonts

The bundled Inter is the default (`WebFontInter`). A page cannot read font
files from disk, so other fonts come three ways:

- `WebFontInstalled "Noto Sans"`: an installed family, which the browser
  hands over through the Local Font Access API. Chromium-based browsers have
  it; Firefox and Safari do not. `listWebFontFamilies` lists the families.
  The browser asks the user the first time, and shows that question only
  right after a click or key press, so call it from a task a button starts.
- `WebFontUrl "fonts/Literata.ttf"`: a TrueType or OpenType file the page
  fetches. A file on another origin needs CORS headers that allow the page.
- `WebFontBytes bytes "label"`: a file the program embeds.

Set them in `SdlWebOptions` (`webFont`, `webMonoFont`), or change the UI font
from the view with `setSdlWebUiFont`, which loads it before the next frame as
`setSdlUiFont` does on the desktop. A font that will not load is the bundled
Inter, with a warning on the browser console.

## Debugging

`askSdlWebDebug` returns the session's frame timings (the core's
`CoreDebugSnapshot`, as `askSdlDebug` has), the canvas size, pixel ratio,
scale, UI font and the WebGL renderer's name. It refreshes at most four
times a second while a view asks, so ask only while the numbers are on
screen. `formatFpsRows`, `formatDrawRows` and `formatExplainRows` lay them
out as the SDL backend's Debug window does. WebGL calls return before the GPU
runs them, so the present time is zero and the render time is the whole of a
frame's cost. The runtime rows need GHC's `-T`: a reactor module takes no
command line, so `start` in `web/main.js` passes `rtsFlags` (the demo page's
`"-T"`) through `GHCRTS`.

## The page

The page provides a `<canvas>` whose CSS box (its `width` and `height`
in CSS pixels) the page sets: a stylesheet rule, an inline style, or a
container it fills. The session makes the drawing buffer that box times
`devicePixelRatio` and follows the box as it changes. `web/index.html`
fills the viewport with `canvas { display: block; width: 100vw; height:
100vh; }`.

A canvas with no CSS size takes its box from its drawing buffer, which the
session sets, so its size could not follow the page. When the session
starts, the runtime fixes such a canvas's CSS width or height at the size
it has then: the `width` and `height` attributes, or the browser's 300x150
default. It keeps that size whatever the window does.

## How it runs

The program is a WebAssembly reactor module. `web/main.js` instantiates it
with a WASI shim and the JSFFI glue GHC generates, then calls its `nanoMain`
export. The session is `NanoUI.Runner.runSessionLoop`, the loop the desktop
backends use. Its wait for events awaits a JavaScript promise, which
suspends the Haskell thread and returns control to the browser until the
canvas queues an event or a background thread wakes the loop.

`web/nano-ui-sdl-web.js` holds the page's side:

- WebGL2 draws from the module's memory in place: the 32-byte vertices, the
  32-bit indices, and one draw per merged command, scissored to the frame's
  damage. The drawing buffer is preserved between frames, so a frame repaints
  only its damage, as nano-ui-sdl's retained texture allows.
- The glyph atlas pages are 2048x2048 RGBA in the module's memory. Only the
  rect a frame added to goes to the GPU.
- Pointer, wheel and key events become fixed-size records the session reads
  (`NanoUI.SdlWeb.Internal.Input` lists them). A hidden textarea receives
  typed text, input method compositions and pastes. While a text field has
  focus, the textarea sits over its caret, so the input method's window and
  the on-screen keyboard open there.
- The cursor is the canvas's CSS `cursor`. Browsers have every cursor kind,
  so none falls back.

## Building for the browser

On Linux or macOS with [ghc-wasm-meta](https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta)
(its default 9.14 flavour, with the cabal 3.14 it installs):

```bash
source ~/.ghc-wasm/env
```

```bash
packages/nano-ui-sdl-web/scripts/build-wasi-deps.sh
```

```bash
PKG_CONFIG_PATH=~/.nano-ui-wasi/lib/pkgconfig packages/nano-ui-sdl-web/scripts/build-web.sh
```

```bash
python3 -m http.server -d dist-web 8000
```

`build-wasi-deps.sh` builds FreeType, HarfBuzz, SDL3 and SDL3_ttf with the
wasm toolchain's clang into `~/.nano-ui-wasi`. SDL has no WASI platform, so
the script patches three spots in SDL's sources (the threadless build CMake
allows only for Emscripten, the dynamic API, the Unix URL opener) and builds
it as a console Unix build. FreeType's setjmp needs WebAssembly exception
handling (`-mllvm -wasm-enable-sjlj`), which every current browser has.
Everything is built with `-fPIC`, since ghc-wasm-meta's Cabal config links
each Haskell library as a shared module for Template Haskell.

`build-web.sh [out-dir [cabal-target]]` builds a program with
`cabal.project.wasm` (by default this package's `nano-ui-sdl-web-demo`; the
widget cookbook is `nano-ui-demo:exe:nano-ui-web-cookbook`), shrinks it with
`wasm-opt`, and lays out `out-dir` (default `dist-web/`): the page, the
runtime, the module as `main.wasm`, GHC's JSFFI glue, and the
[browser_wasi_shim](https://github.com/bjorn3/browser_wasi_shim) it is loaded
with, fetched from npm.

The bundled Inter is embedded with Template Haskell, which GHC's wasm
backend runs through Node.js, so `node` must be on `PATH` while building.

`scripts/smoke-test.cjs <dir> <x> <y>` loads a built directory in headless
Chromium with Playwright: the program must set the page title, draw a frame
with text, and redraw after a click at the point given, with no errors
logged. CI's Web workflow (`.github/workflows/web.yml`) runs all of this and
publishes the cookbook with the book.

## Building natively

With SDL3 and SDL3_ttf installed, as for nano-ui-sdl, the package builds
with the rest of the repository. A native `runSdlWebApp` throws, since there
is no page to run in (`browserAvailable` is `False`). The test suite runs
sessions against a scripted host instead of a browser: it queues events and
records each texture upload and draw.

## Status

Built and run in headless Chromium (WebGL2 on SwiftShader) with GHC 9.14's
wasm backend: this package's demo and the widget cookbook draw their text and
widgets, and take clicks, drags and typing. Natively, with GHC 9.10.3 and
9.14.1, the test suite checks the font engine (SDL_ttf shaping, glyph
rasterisation into the in-memory atlas), event decoding, damage clipping and
command merging, and whole sessions on the scripted host (opening frame, a
click on a button, a pixel ratio change reopening the fonts).

In Chromium, the cookbook's Diagnostics tab shows the session's frame
statistics, and its font picker lists the installed families and switches
the UI to one of them.

Not yet checked: pixel comparison with nano-ui-sdl, input methods, and
browsers other than Chromium.

## Differences from nano-ui-sdl

- There are no font directories. Installed families come only through the
  Local Font Access API (see [Fonts](#fonts)), and there are no fallback
  fonts for characters the UI font lacks, and no bold face found beside a
  font file: bold and italic are drawn synthetically.
- Pasting reads the text of the page's `paste` event, and copying writes
  with `navigator.clipboard`. A view's clipboard read sees only what was last
  pasted or copied on the page.
- The window is the canvas: only the title (`setWindowTitleUi`) is
  supported. There are no file dialogs, file drops, further windows
  (`nativeWindow`), or screenshots.
