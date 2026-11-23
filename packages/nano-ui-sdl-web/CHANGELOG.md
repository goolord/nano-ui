# Changelog

## 0.1.0.0 -- Unreleased

### Added

- First release: `NanoUI.Backend.SdlWeb` runs nano-ui views in a browser
  canvas with GHC's WebAssembly backend. Text uses nano-ui-sdl's SDL_ttf font
  engine with the bundled Inter, whose OFL notice ships in `LICENSE-Inter`;
  WebGL2 draws the frames. Builds with GHC 9.10 through 9.14
  (`base >=4.20 && <4.23`).
- `web/` holds the page's runtime, a loader and a demo page;
  `scripts/build-wasi-deps.sh` builds SDL3, SDL3_ttf, FreeType and
  HarfBuzz for wasm32-wasi, and `scripts/build-web.sh` builds the demo, or
  any program given as its cabal target. `scripts/smoke-test.cjs` loads a
  built page in headless Chromium and checks it draws and takes a click.
- `formatExplainRows`, for showing the layout overlay's node, as
  nano-ui-sdl exports it.
- Installed fonts and fonts by URL: `WebFontInstalled` and `WebFontUrl`,
  `listWebFontFamilies` (the Local Font Access API, in Chromium-based
  browsers), and `setSdlWebUiFont` to change the UI font from the view. A
  font that will not load falls back to the bundled Inter.
- `askSdlWebDebug` and `SdlWebDebugSnapshot`: frame timings, canvas, pixel
  ratio, font and WebGL renderer, with `formatFpsRows` and `formatDrawRows`
  re-exported, as nano-ui-sdl has `askSdlDebug`.
