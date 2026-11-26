# Changelog

## 0.2.0.0 -- Unreleased

Initial release.

- `NanoUI.Backend.SdlWeb` runs `nano-ui` views in a browser `<canvas>` using GHC's `wasm32` backend, WebGL2 rendering (including `nano-ui-shader` GLSL ES 300 surface shaders and subtree effects), and `nano-ui-ttf` text shaping with bundled Inter (`LICENSE-Inter`).
- Supports `WebFontInstalled`, `WebFontUrl`, `WebFontSearch` (with optional Fontsource loading), runtime font switching (`setSdlWebUiFont`), and extended fallback fonts (`webFallbackFonts`).
- Browser file dialogs (`openWebFiles`, `openWebFolder`, `saveWebFile`) returning `WebFile` names and bytes.
- `requestAnimationFrame`-paced animation frames, viewport-aware wake pausing, pointer capture/touch handling, and `askSdlWebDebug` / `SdlWebDebugSnapshot` telemetry (`formatFpsRows`, `formatDrawRows`, `formatExplainRows`).
- Includes the browser loader (`web/`), `scripts/build-wasi-deps.sh`, `scripts/build-web.sh`, and headless Chromium smoke testing (`scripts/smoke-test.cjs`).
- Builds with GHC 9.10 through 9.14 (`base >=4.20 && <4.23`).
