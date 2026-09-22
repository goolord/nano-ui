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

bool nano_ui_text_atlas_insert_surface(
    NanoUiTextAtlas *atlas,
    SDL_Surface *surface,
    int *out_page,
    float *out_x,
    float *out_y,
    float *out_w,
    float *out_h);

void nano_ui_text_atlas_reset(NanoUiTextAtlas *atlas);

#endif
