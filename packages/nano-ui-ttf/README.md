# nano-ui-ttf

Shared SDL3_ttf text rendering layer used internally by both [nano-ui-sdl](../nano-ui-sdl) and [nano-ui-sdl-web](../nano-ui-sdl-web). It handles HarfBuzz line shaping via SDL_ttf, glyph atlas packing, shaped-line and font handle caching, and embeds the default Inter font (`NanoUI.Ttf.Internal.Inter`).

Applications should not depend on `nano-ui-ttf` directly; depend on a backend package instead. All exposed modules are `Internal` and may change alongside the backends between minor releases.

Building requires SDL3, SDL3_ttf 3.2+, and `pkg-config`. The `sdl` Cabal flag is enabled by default; disabling it marks the library unbuildable so solver plans without SDL can still resolve.

Code is MIT-licensed (`LICENSE`); the bundled Inter font is under the SIL Open Font License 1.1 (`LICENSE-Inter`).
