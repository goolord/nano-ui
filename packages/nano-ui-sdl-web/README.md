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
(its default 9.14 flavour, with cabal 3.14 or earlier):

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
wasm toolchain's clang into `~/.nano-ui-wasi`. `build-web.sh` builds the demo
with `cabal.project.wasm` and copies the page, the runtime, the module and its
glue into `dist-web/`.

The bundled Inter is embedded with Template Haskell, which GHC's wasm
backend runs through Node.js, so `node` must be on `PATH` while building.

## Building natively

With SDL3 and SDL3_ttf installed, as for nano-ui-sdl, the package builds
with the rest of the repository. A native `runSdlWebApp` throws, since there
is no page to run in (`browserAvailable` is `False`). The test suite runs
sessions against a scripted host instead of a browser: it queues events and
records each texture upload and draw.

## Status

Verified natively with GHC 9.10.3 and 9.14.1: the font engine (SDL_ttf
shaping, glyph rasterisation into the in-memory atlas), event decoding,
damage clipping and command merging, and whole sessions on the scripted host
(opening frame, a click on a button, a pixel ratio change reopening the
fonts).

Not yet run:

- the wasm build: SDL has no WASI platform, so `build-wasi-deps.sh`
  configures SDL3 as a console Unix build without threads, and whether it
  builds that way is untested
- the browser runtime, `web/nano-ui-sdl-web.js`, and the promise-based wait
- `ghc-compact` and `effectful-core` under wasm32-wasi

## Differences from nano-ui-sdl

- The browser has no font directories: there is no installed-font search,
  no fallback fonts for characters Inter lacks, and no bold face found beside
  a font file. Bold and italic are drawn synthetically. Pass other fonts as
  bytes with `WebFontBytes`.
- Pasting reads the text of the page's `paste` event, and copying writes
  with `navigator.clipboard`. A view's clipboard read sees only what was last
  pasted or copied on the page.
- The window is the canvas: only the title (`setWindowTitleUi`) is
  supported. There are no file dialogs, file drops, further windows
  (`nativeWindow`), or screenshots.
