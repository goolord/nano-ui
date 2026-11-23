# nano-ui-rgfw

Lightweight desktop backend for [nano-ui](https://github.com/goolord/nano-ui) built on [RGFW](https://github.com/ColleagueRiley/RGFW) and OpenGL 3.2. RGFW's C source compiles directly with the package and text renders with the bundled [Cozette](https://github.com/slavfox/Cozette) bitmap font, so you need no libraries beyond native OS windowing and OpenGL.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import NanoUI
import NanoUI.Backend.Rgfw (RgfwOptions (..), defaultRgfwOptions, runRgfwApp)

main :: IO ()
main = runRgfwApp defaultRgfwOptions {optScale = 2} (label "Hello")
```

For Elm-style views, `runRgfwAppReduce` pairs a `model -> NanoUIE msg ()` view with an update function `msg -> model -> model` (add `nano-ui-emit` and import `NanoUI.Emit`). Configure the window (`optWindow`, using the same `WindowSettings` as `nano-ui-sdl`), theme, UI scale, and animation refresh rate through `RgfwOptions`. RGFW centers windows that leave `wsPosition` at its default, and all RGFW windows are opaque (`wsTransparent`, `wsOpacity`, and `setWindowOpacityUi` are no-ops; use `nano-ui-sdl` if you need translucent windows).

Call `askRgfwDebug` inside a view to inspect frame timings and display stats, or render them directly with `debugWindowBody`. To run custom initialization after window creation and before frame 1, use `runRgfwAppWith` or `runRgfwAppReduceCustomWith`.

Any core `Theme` works here, adjusted automatically by `applyRgfwTheme` to square corners and 1 px borders to fit flat-quad rendering. Cozette renders at 7x13 px at 1x scale, EPX-upscaled at 2x and 4x, and box-filtered at intermediate scales (set `optScale = 0` to follow the monitor scale). Because Cozette is a single fixed-size bitmap font, `fontSize` and `fontMono` do not change glyph size, and layout measures every string using that exact bitmap size.

## Running examples

```sh
cabal run nano-ui-rgfw-demo     # widget demo
cabal run nano-ui-rgfw-profile  # hidden-window benchmark for +RTS -p
```

## System requirements

- **Linux:** X11, Xcursor, Xrandr, and Xi development headers, plus OpenGL 3.2
- **macOS:** Cocoa and OpenGL 3.2
- **Windows:** `gdi32`, `user32`, `shell32`, and OpenGL 3.2

Target GHC 9.10 through 9.14 with `GHC2024` and `OverloadedStrings`, add `nano-ui` and `nano-ui-rgfw` to `build-depends`, and compile executables with `-threaded` so background tasks keep running while the event loop sleeps. RGFW itself is bundled inside `nano-ui-rgfw-bindings`.

## Sessions and limitations

`runRgfwApp` owns the native window and OpenGL context until exit, and `runRgfwAppReduceCustom` lets you derive theme and scale dynamically from your model. For custom event loops, `NanoUI.Rgfw.Internal.Context` and `NanoUI.Rgfw.Internal.Gl` expose lower-level setup. Keep all OpenGL calls and cleanup on the bound OS thread that created the context, and reuse that same OS thread across sessions (on Windows, background task wakes only reach the original thread).

Coordinates are in logical pixels scaled by `optScale`. Note a few intentional simplifications compared to `nano-ui-sdl`:

- **No OS IME pre-edit or file drops:** Input methods draw their own system composition UI, and `dropZone`/`useDrop` receive no external drops.
- **Bitmap text only:** There is no system font discovery, HarfBuzz shaping, or variable font sizes/weights/italics. Text drawn inside canvases (`richText`, Markdown, chart labels, `drawText`, pane drag titles) renders in Cozette above vector shapes in its layer (`richText` and Markdown still draw underlines and strikethroughs as vector lines). Choose `nano-ui-sdl` whenever rich TrueType typography is needed.

## Documentation

- [The nano-ui Book](https://goolord.github.io/nano-ui/book/): Tutorials, widget gallery, and backend overview.
- [Development Setup](https://goolord.github.io/nano-ui/book/getting-started/development.html): Running headless and native rendering tests.

## License

MIT. The bundled Cozette bitmap font is MIT-licensed by its author (`LICENSE-Cozette`); include that notice with applications built against `nano-ui-rgfw`.
