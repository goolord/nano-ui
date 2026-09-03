#ifndef NANO_UI_TEXT_ATLAS_H
#define NANO_UI_TEXT_ATLAS_H

#include <SDL3/SDL.h>
#include <stdbool.h>

typedef struct NanoUiTextAtlas NanoUiTextAtlas;

NanoUiTextAtlas *nano_ui_text_atlas_create(SDL_Renderer *renderer);
void nano_ui_text_atlas_destroy(NanoUiTextAtlas *atlas);

SDL_Texture *nano_ui_text_atlas_texture(NanoUiTextAtlas *atlas);
bool nano_ui_text_atlas_size(NanoUiTextAtlas *atlas, float *out_w, float *out_h);

bool nano_ui_text_atlas_insert_surface(
    NanoUiTextAtlas *atlas,
    SDL_Surface *surface,
    float *out_x,
    float *out_y,
    float *out_w,
    float *out_h);

void nano_ui_text_atlas_reset(NanoUiTextAtlas *atlas);

#endif
