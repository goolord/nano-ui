# nano-ui-rgfw: RGFW Backend

`nano-ui-rgfw` is a lightweight, zero-system-dependency backend powered by [RGFW](https://github.com/ColleagueRiley/RGFW) and OpenGL 3.2.

## Why RGFW?

While `nano-ui-sdl` is rich in features (TrueType fonts, native file dialogs), it requires SDL3 shared libraries. In contrast, `nano-ui-rgfw`:
- Links statically with bundled C source code.
- Has **zero external system C dependencies**.
- Includes a bundled bitmap font (`Cozette`).
- Compiles in seconds and produces small, completely standalone binaries.

## Usage

```haskell
import NanoUI
import NanoUI.Backend.Rgfw

main :: IO ()
main = runRgfwApp defaultRgfwOptions appView
```

Ideal for quick tools, game overlays, embedded environments, or minimal deployment scenarios.
