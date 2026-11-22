# nano-ui-ttf

The SDL3_ttf text layer that [nano-ui-sdl](../nano-ui-sdl) and
[nano-ui-sdl-web](../nano-ui-sdl-web) share: shaping a line with SDL_ttf,
placing glyphs in a texture atlas, caching shaped lines and opened fonts, and
the bundled Inter font (`NanoUI.Ttf.Internal.Inter`).

Applications do not depend on this package. Use a backend, which picks fonts
and draws text through it. Both modules are `Internal` and change between
minor releases along with the backends.

Building it needs SDL3, SDL3_ttf 3.2 or newer, and pkg-config. The `sdl`
flag is on by default; turning it off marks the library unbuildable, so a
package set without SDL can still resolve.

Inter is distributed under the SIL Open Font License 1.1 (`LICENSE-Inter`);
the code is MIT (`LICENSE`).
