# nano-ui-rgfw

A small window backend for [nano-ui](https://github.com/goolord/nano-ui), built
on [RGFW](https://github.com/ColleagueRiley/RGFW) and OpenGL 3.2. RGFW is
compiled with the package and text uses the bundled Cozette bitmap font, so
there is nothing to install beyond the platform's own windowing libraries.

```haskell
import NanoUI
import NanoUI.Backend.Rgfw (defaultRgfwOptions, runRgfwApp)

main :: IO ()
main = runRgfwApp defaultRgfwOptions (label "Hello")
```

`runRgfwAppReduce` takes a model and an update function, for use with
`NanoUI.Emit`. `RgfwOptions` sets the window title, size, theme, and scale.
