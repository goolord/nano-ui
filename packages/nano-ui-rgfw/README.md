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
