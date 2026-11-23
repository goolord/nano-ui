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
- For a canvas inside a page: `start` in `web/main.js` takes `env`, the
  program's environment variables; an inline `touch-action` on the canvas
  is kept, and a `pointercancel` releases the press away from the canvas;
  timed waits are held while the canvas is scrolled out of the viewport.
- Animation frames are paced by `requestAnimationFrame` where they were
  paced by `setTimeout`, whose 15.6 ms steps on Windows dropped about
  every other frame.
- `cabal.project.wasm` builds with `split-sections`; `build-web.sh` takes
  `WASM_OPT_FLAGS` and prints the module's sizes; the demo page preloads
  the module and its loader.
- Installed fonts and fonts by URL: `WebFontInstalled` and `WebFontUrl`,
  `listWebFontFamilies` (the Local Font Access API, in Chromium-based
  browsers), and `setSdlWebUiFont` to change the UI font from the view. A
  font that will not load falls back to the bundled Inter.
- Fonts by name, as on the desktop: `WebFontSearch` uses the first family
  that can be had (installed, else from the font service, else Inter), and
  `webFontFamilies` lists them. Installed families are asked for at the
  user's next press on the page, which is when browsers allow the question.
  `webFontService = Just Fontsource` lists and loads Fontsource's families
  in every browser, Firefox and Safari included.
- Fallback fonts for the characters the UI font lacks: `webFallbackFonts`,
  by default the wider Inter subset (`web/fonts/inter-extended.ttf`,
  Latin beyond ASCII, Greek, Cyrillic, punctuation and common symbols) that
  `build-web.sh` serves beside the page.
- File dialogs on contents: `openWebFiles`, `openWebFolder` and
  `saveWebFile`, with `FileDialogOptions` and `FileFilter` as nano-ui-sdl
  names them, and `WebFile` for a chosen file's name and bytes. Saving uses
  the browser's save dialog where it has one and downloads elsewhere.
- `askSdlWebDebug` and `SdlWebDebugSnapshot`: frame timings, canvas, pixel
  ratio, font and WebGL renderer, with `formatFpsRows` and `formatDrawRows`
  re-exported, as nano-ui-sdl has `askSdlDebug`.
