# Changelog

## 0.2.0.0 -- Unreleased

Initial release.

- Extracted the shared SDL3_ttf text engine from `nano-ui-sdl` so `nano-ui-sdl` and `nano-ui-sdl-web` share line shaping, glyph atlas bookkeeping, shaped-line and font caches, and the bundled Inter font (SIL OFL 1.1 in `LICENSE-Inter`).
- Exposes internal modules (`NanoUI.Ttf.Internal.*`) for backend integration, including fallback font attachment (`attachFallback`, `openFontBytes`) and SDL error reporting.
