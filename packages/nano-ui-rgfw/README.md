# nano-ui-rgfw

Window backend for [nano-ui](https://github.com/goolord/nano-ui) on
[RGFW](https://github.com/ColleagueRiley/RGFW) and OpenGL 3.2. RGFW is compiled
with the package, and text uses the bundled
[Cozette](https://github.com/slavfox/Cozette) bitmap font, so it needs no
libraries beyond the platform's windowing and OpenGL.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import NanoUI
import NanoUI.Backend.Rgfw (RgfwOptions (..), defaultRgfwOptions, runRgfwApp)

main :: IO ()
main = runRgfwApp defaultRgfwOptions {optScale = 2} (label "Hello")
```

`runRgfwAppReduce` takes a model and an update function, for use with
`NanoUI.Emit`. `RgfwOptions` sets the window title, size and centering, the
theme, the UI scale, and the refresh rate used for pacing animations.

The theme is any core `Theme`. The backend draws it with square corners and 1px
borders (`applyRgfwTheme`), since geometry is drawn as flat quads. The font is
drawn from its 7x13 bitmap at 1x, from EPX-scaled bitmaps at 2x and 4x, and
box-averaged from those at other scales. A scale of `0` follows the monitor.

## Running

```sh
cabal run nano-ui-rgfw-demo     # widget demo
cabal run nano-ui-rgfw-profile  # demo frame loop in a hidden window, for +RTS -p
```

## Requirements

X11, Xcursor, Xrandr, and Xi on Linux; Cocoa on macOS; gdi32, user32, and
shell32 on Windows. All need OpenGL 3.2.

Use GHC 9.14 and add `nano-ui` and `nano-ui-rgfw` to your application's
`build-depends`. The example uses `GHC2024` and `OverloadedStrings`.
On Linux, install the development packages for the listed X11 libraries;
RGFW itself is bundled by `nano-ui-rgfw-bindings`.

## Sessions and rendering

`runRgfwApp` owns the window and OpenGL context until the window closes.
`runRgfwAppReduceCustom` can derive the theme and scale from your model.
For a custom event loop, the internal modules `NanoUI.Rgfw.Internal.Context`
and `NanoUI.Rgfw.Internal.Gl`, with the RGFW bindings, expose context setup and
rendering. OpenGL operations must run on the OS thread where the context is
current, and native resources must be closed on that thread.

Layout and input use logical coordinates; `optScale` controls their mapping
to physical pixels. RGFW reports no input-method composition: an input
method draws its own composition.
The bitmap font does not provide the SDL backend's installed-font lookup or
HarfBuzz shaping. Choose the SDL backend when those text features are
required.

See the [development guide](https://github.com/goolord/nano-ui/blob/main/docs/development.md)
for headless tests and native rendering checks.
