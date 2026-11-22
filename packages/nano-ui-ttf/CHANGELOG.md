# Changelog

## 0.1.0.0 -- Unreleased

- First release: SDL3_ttf line shaping, glyph atlas bookkeeping, shaped-line
  and font caches, and the bundled Inter font, extracted from nano-ui-sdl so
  nano-ui-sdl-web shares them. `TTF_Init` and `TTF_OpenFont` failures carry
  SDL's error text.
