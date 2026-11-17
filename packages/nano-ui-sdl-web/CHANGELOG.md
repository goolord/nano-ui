# Changelog

## 0.1.0.0 -- Unreleased

### Added

- First release: `NanoUI.Backend.SdlWeb` runs nano-ui views in a browser
  canvas with GHC's WebAssembly backend. Text uses nano-ui-sdl's SDL_ttf font
  engine with the bundled Inter, whose OFL notice ships in `LICENSE-Inter`;
  WebGL2 draws the frames. Builds with GHC 9.10 through 9.14
  (`base >=4.20 && <4.23`). It imports `NanoUI.Internal` modules, so it
  allows only nano-ui 0.2.0.x (`>=0.2.0 && <0.2.1`).
- `web/` holds the page's runtime, a loader and a demo page;
  `scripts/build-wasi-deps.sh` builds SDL3, SDL3_ttf, FreeType and
  HarfBuzz for wasm32-wasi, and `scripts/build-web.sh` builds the demo.
