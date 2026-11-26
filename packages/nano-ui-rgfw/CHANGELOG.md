# Changelog

## 0.2.0.0 -- Unreleased

Initial release.

- `NanoUI.Backend.Rgfw` runs `nano-ui` views in a native OpenGL 3.2 window using bundled RGFW (`nano-ui-rgfw-bindings`) and the bundled Cozette bitmap font (`LICENSE-Cozette`), requiring no external SDL or FreeType C libraries.
- Supports `nano-ui-shader` GLSL 150 surface shaders and offscreen subtree effects, retained damage-clipped framebuffer rendering, image atlas textures, multi-thread wakeups, clipboard, cursor shapes, full keyboard/mouse input, and `WindowSettings` window commands (`askWindow`, `quit`, `requestScreenshot`, excluding transparency/opacity).
- Supports Elm-style reducer apps (`NanoUIE msg ()` from `nano-ui-emit`), `optTheme` / `optThemeFor`, `optExplainLayout`, and `askRgfwDebug` / `debugWindowBody`.
- Builds with GHC 9.10 through 9.14 (`base >=4.20 && <4.23`).
