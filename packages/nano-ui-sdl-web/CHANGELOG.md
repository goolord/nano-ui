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
