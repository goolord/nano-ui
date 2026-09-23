#ifndef NANO_UI_TEXT_ATLAS_H
#define NANO_UI_TEXT_ATLAS_H

#include <SDL3/SDL.h>
#include <stdbool.h>

/* Pages the atlas opens before it is out of space. Mirrors glyphAtlasPages
 * in NanoUI.Internal.Draw.Types. */
#define NANO_UI_TEXT_ATLAS_PAGES 4

typedef struct NanoUiTextAtlas NanoUiTextAtlas;

NanoUiTextAtlas *nano_ui_text_atlas_create(SDL_Renderer *renderer);
void nano_ui_text_atlas_destroy(NanoUiTextAtlas *atlas);

/* The texture of page `page`, or NULL for a page never opened. */
SDL_Texture *nano_ui_text_atlas_texture(NanoUiTextAtlas *atlas, int page);

/* Copy a glyph surface into the atlas, on a new page once the last one is
 * full, and write its u0, v0, u1 and v1: normalised to [0,1] within the page,
 * plus the page's number in u. False when no page has room or the copy
 * fails. */
bool nano_ui_text_atlas_insert_surface(
    NanoUiTextAtlas *atlas,
    SDL_Surface *surface,
    float uv[4]);

void nano_ui_text_atlas_reset(NanoUiTextAtlas *atlas);

#endif
