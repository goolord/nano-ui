# nano-ui-rgfw-bindings

Haskell bindings to [RGFW](https://github.com/ColleagueRiley/RGFW), a
single-header C library for windows, input, and OpenGL contexts. The C source
is bundled and compiled with the package.

`RGFW` is the wrapped API that `nano-ui-rgfw` uses; `RGFW.Raw` is the direct
FFI layer.

Linked system libraries: X11, Xcursor, Xrandr, and Xi on Linux; Cocoa on
macOS; gdi32, user32, and shell32 on Windows.
