#ifndef NANO_UI_WEB_ATLAS_H
#define NANO_UI_WEB_ATLAS_H

#include <SDL3/SDL.h>
#include <stdbool.h>
#include <stdint.h>

/* Pages the atlas opens before it is out of space. Mirrors glyphAtlasPages
 * in NanoUI.Internal.Draw.Types. */
#define NANO_UI_WEB_ATLAS_PAGES 4

/* Width and height of a page in pixels. Mirrors glyphAtlasSize in
 * NanoUI.SdlWeb.Internal.Font. */
#define NANO_UI_WEB_ATLAS_SIZE 2048

/* The glyph atlas of nano_ui_text_atlas.c in nano-ui-sdl, kept in memory
 * instead of SDL textures: each page is RGBA32 pixels, and the area written
 * since the page was last uploaded is kept so only that goes to the GPU. */
typedef struct NanoUiWebAtlas NanoUiWebAtlas;

NanoUiWebAtlas *nano_ui_web_atlas_create(void);
void nano_ui_web_atlas_destroy(NanoUiWebAtlas *atlas);

/* The pixels of page `page`, NANO_UI_WEB_ATLAS_SIZE * 4 bytes a row, or NULL
 * for a page never opened. */
uint8_t *nano_ui_web_atlas_pixels(NanoUiWebAtlas *atlas, int page);

/* Write the rect of page `page` changed since the last call (x, y, width,
 * height) and forget it. False when nothing changed. */
bool nano_ui_web_atlas_take_dirty(NanoUiWebAtlas *atlas, int page, int out[4]);

/* Mark every open page changed in full, after the GPU lost its textures. */
void nano_ui_web_atlas_mark_all_dirty(NanoUiWebAtlas *atlas);

/* Copy an RGBA32 glyph surface into the atlas, on a new page once the last
 * one is full, and write its u0, v0, u1 and v1: normalised to [0,1] within
 * the page, plus the page's number in u. False when no page has room. */
bool nano_ui_web_atlas_insert_surface(
    NanoUiWebAtlas *atlas,
    SDL_Surface *surface,
    float uv[4]);

void nano_ui_web_atlas_reset(NanoUiWebAtlas *atlas);

#endif
