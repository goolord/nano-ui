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

Fonts work as on the desktop. `webFontFamilies` lists the families there
are, as `listFontFamilies` does, and `WebFontSearch ["Noto Sans", "sans-serif"]`
uses the first that can be had, as `FontSearch` does, else the bundled Inter
(`WebFontInter`, the default). Names compare ignoring case, spaces, `-` and
`_`, and `sans-serif`, `serif` and `monospace` stand for the desktop's lists
of common families. Pass a search as `webFont` or `webMonoFont`, or change the
UI font from the view with `setSdlWebUiFont`, which loads it before the next
frame as `setSdlUiFont` does.

A page cannot read font files from disk, so the families come from two
places:

- Installed families, through the Local Font Access API, in Chromium-based
  browsers. The first time `webFontFamilies` is asked, the browser has not
  asked the user yet, and it can ask only during a click or key press, so
  the session asks at the user's next press on the page; that press works
  as usual. The browser keeps the answer for the site, so on later visits
  the installed families are there at once.
- A font service (`webFontService = Just Fontsource`): about 2,100
  open-licence families from fontsource.org, loaded by name as TrueType from
  its CDN. This is what Firefox and Safari have, since they give a page no
  installed fonts, and what Chromium has until the user allows installed
  ones. It is off by default, since the page then fetches the catalogue and
  fonts from a third party.

A font can also be named by where it is: `WebFontInstalled "Noto Sans"` (an
installed family only), `WebFontUrl "fonts/Literata.ttf"` (a TrueType or
OpenType file the page fetches; another origin needs CORS headers that allow
the page), or `WebFontBytes bytes "label"` (a file the program embeds).
`listWebFontFamilies` asks for the installed families at once, from a task
a press starts, for a page that wants to ask at a moment of its choosing.

The bundled Inter covers printable ASCII only. For everything else (accented
Latin, Greek, Cyrillic, the ellipsis, dashes and curly quotes, currency,
arrows, common symbols) the session attaches fallback fonts, as the desktop
attaches system fonts: `webFallbackFonts`, by default
`fonts/inter-extended.ttf`, a wider subset of the same Inter that
`build-web.sh` serves beside the page (154 KB, made with
`scripts/prune_inter.py --web`). A page served some other way serves that
file too, or sets `webFallbackFonts`. Combining accents typed apart from
their letter still have no font that places them.

A font that will not load is the bundled Inter, with a warning on the
browser console.

## File dialogs

A page sees no paths, so the dialogs trade in contents:

- `openWebFiles opts` lets the user choose files and answers each one's
  name and bytes (`WebFile`), or `Nothing` when they cancel. `opts` is a
  `FileDialogOptions` with nano-ui-sdl's `dialogFilters` and
  `dialogAllowMany`.
- `openWebFolder` reads every file in a folder the user chooses, named by its
  path inside the folder.
- `saveWebFile opts name bytes` shows the browser's save dialog where it has
  one and downloads the file under `name` elsewhere. It answers the name the
  file was saved under. The dialog is the File System Access API's
  `showSaveFilePicker`, which only Chromium-based browsers implement; Firefox
  and Safari have no API that lets a page open their save dialog. Firefox
  asks where to save a download when its "Always ask you where to save
  files" setting is on, and otherwise saves to the Downloads folder.

Browsers show these only right after a click or key press on the page, so
run them from a task the press starts (`useTask`), as the cookbook's File
Dialogs section does. Opening reads whole files into memory.

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
statistics; its font picker lists the installed families and Fontsource's,
and switches the UI to either (and, with the Local Font Access API removed
as Firefox and Safari lack it, lists and uses Fontsource's); and its File
Dialogs section opens files and folders and
saves (checked with Playwright supplying the files and a stand-in save
dialog, and with the download a browser without one falls back to).

Not yet checked: pixel comparison with nano-ui-sdl, input methods, and
browsers other than Chromium.

## Differences from nano-ui-sdl

- There are no font directories. Installed families come only through the
  Local Font Access API, in Chromium-based browsers after the user allows
  it, and a font service stands in elsewhere (see [Fonts](#fonts)). Fallback
  fonts are the ones
  the session loads (the wider Inter by default) rather than the system's,
  and no bold face is found beside a font file: bold and italic are drawn
  synthetically.
- Pasting reads the text of the page's `paste` event, and copying writes
  with `navigator.clipboard`. A view's clipboard read sees only what was last
  pasted or copied on the page.
- The window is the canvas: only the title (`setWindowTitleUi`) is
  supported. There are no file drops, further windows (`nativeWindow`), or
  screenshots, and the file dialogs work on contents, not paths (see
  [File dialogs](#file-dialogs)).
