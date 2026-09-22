#include "nano_ui_text_atlas.h"

#include <SDL3/SDL.h>
#include <stdlib.h>

enum {
    NANO_UI_TEXT_ATLAS_SIZE = 2048,
    NANO_UI_TEXT_ATLAS_PAD = 1,
    NANO_UI_WHITE_PATCH_SIZE = 4
};

/* Glyphs fill page 0, then each later page in turn. A reset empties every
 * page but keeps their textures, and a page is cleared when it is opened
 * again. */
struct NanoUiTextAtlas {
    SDL_Renderer *renderer;
    SDL_Texture *tex[NANO_UI_TEXT_ATLAS_PAGES];
    int pages; /* pages holding glyphs; the cursor is on the last */
    int x;
    int y;
    int row_h;
};

/* SDL owns the streaming storage. A lock is write-only, so initialize the
 * complete surface on creation/reset, including transparent glyph padding.
 * Every page gets the white patch, so each starts its cursor the same way;
 * shapes sample the one on page 0. */
static bool clear_texture(SDL_Texture *tex)
{
    SDL_Surface *surface = NULL;
    if (!SDL_LockTextureToSurface(tex, NULL, &surface)) {
        return false;
    }
    SDL_Rect white = {0, 0, NANO_UI_WHITE_PATCH_SIZE, NANO_UI_WHITE_PATCH_SIZE};
    bool ok = SDL_FillSurfaceRect(surface, NULL, 0) &&
              SDL_FillSurfaceRect(surface, &white, 0xffffffffu);
    SDL_UnlockTexture(tex);
    return ok;
}

static SDL_Texture *create_texture(NanoUiTextAtlas *atlas)
{
    SDL_Texture *tex = SDL_CreateTexture(
        atlas->renderer,
        SDL_PIXELFORMAT_RGBA32,
        SDL_TEXTUREACCESS_STREAMING,
        NANO_UI_TEXT_ATLAS_SIZE,
        NANO_UI_TEXT_ATLAS_SIZE);
    if (!tex) {
        return NULL;
    }
    SDL_SetTextureBlendMode(tex, SDL_BLENDMODE_BLEND);
    /* Bilinear filtering keeps glyph quads smooth when a quad lands off a
     * whole texel boundary (fractional display scale, sub-pixel pen nudge,
     * shaped-run placement). NEAREST snaps to the closest texel and makes
     * scaled/slightly-misaligned text look blocky and pixelated. */
    SDL_SetTextureScaleMode(tex, SDL_SCALEMODE_LINEAR);
    return tex;
}

/* Start glyphs on page `page`: make or clear its texture and put the cursor
 * after its white patch. */
static bool open_page(NanoUiTextAtlas *atlas, int page)
{
    if (!atlas->tex[page]) {
        atlas->tex[page] = create_texture(atlas);
        if (!atlas->tex[page]) {
            return false;
        }
    }
    if (!clear_texture(atlas->tex[page])) {
        return false;
    }
    atlas->pages = page + 1;
    atlas->x = NANO_UI_WHITE_PATCH_SIZE + NANO_UI_TEXT_ATLAS_PAD;
    atlas->y = NANO_UI_TEXT_ATLAS_PAD;
    atlas->row_h = NANO_UI_WHITE_PATCH_SIZE;
    return true;
}

/* A place for a gw by gh glyph on the current page, moving to a new row or
 * a new page when it does not fit. */
static bool slot_for(NanoUiTextAtlas *atlas, int gw, int gh, int *out_page, int *out_x, int *out_y)
{
    int pad = NANO_UI_TEXT_ATLAS_PAD;
    int size = NANO_UI_TEXT_ATLAS_SIZE;
    if (gw + 2 * pad > size || gh + 2 * pad > size) {
        return false;
    }
    if (atlas->pages == 0 && !open_page(atlas, 0)) {
        return false;
    }
    for (;;) {
        if (atlas->x + gw + pad <= size && atlas->y + gh + pad <= size) {
            break;
        }
        int next_y = atlas->y + (atlas->row_h > 0 ? atlas->row_h + pad : pad);
        if (next_y + gh + pad <= size) {
            atlas->y = next_y;
            atlas->x = pad;
            atlas->row_h = 0;
            break;
        }
        if (atlas->pages >= NANO_UI_TEXT_ATLAS_PAGES || !open_page(atlas, atlas->pages)) {
            return false;
        }
    }
    *out_page = atlas->pages - 1;
    *out_x = atlas->x;
    *out_y = atlas->y;
    return true;
}

NanoUiTextAtlas *nano_ui_text_atlas_create(SDL_Renderer *renderer)
{
    if (!renderer) {
        return NULL;
    }
    NanoUiTextAtlas *atlas = (NanoUiTextAtlas *)calloc(1, sizeof(NanoUiTextAtlas));
    if (!atlas) {
        return NULL;
    }
    atlas->renderer = renderer;
    return atlas;
}

void nano_ui_text_atlas_destroy(NanoUiTextAtlas *atlas)
{
    if (!atlas) {
        return;
    }
    for (int i = 0; i < NANO_UI_TEXT_ATLAS_PAGES; i++) {
        if (atlas->tex[i]) {
            SDL_DestroyTexture(atlas->tex[i]);
        }
    }
    free(atlas);
}

SDL_Texture *nano_ui_text_atlas_texture(NanoUiTextAtlas *atlas, int page)
{
    if (!atlas || page < 0 || page >= NANO_UI_TEXT_ATLAS_PAGES) {
        return NULL;
    }
    return atlas->tex[page];
}

bool nano_ui_text_atlas_insert_surface(
    NanoUiTextAtlas *atlas,
    SDL_Surface *surface,
    int *out_page,
    float *out_x,
    float *out_y,
    float *out_w,
    float *out_h)
{
    if (!atlas || !surface) {
        return false;
    }
    int gw = surface->w;
    int gh = surface->h;
    if (gw <= 0 || gh <= 0) {
        return false;
    }
    int page = 0;
    int x = 0;
    int y = 0;
    if (!slot_for(atlas, gw, gh, &page, &x, &y)) {
        return false;
    }
    /* Glyph surfaces already have the atlas's RGBA32 format. Preserve their
     * pitch instead of copying each row into a second full-size CPU atlas. */
    SDL_Rect rect = {x, y, gw, gh};
    if (!SDL_UpdateTexture(atlas->tex[page], &rect, surface->pixels, surface->pitch)) {
        return false;
    }
    atlas->x = x + gw + NANO_UI_TEXT_ATLAS_PAD;
    if (gh > atlas->row_h) {
        atlas->row_h = gh;
    }
    if (out_page) {
        *out_page = page;
    }
    if (out_x) {
        *out_x = (float)x;
    }
    if (out_y) {
        *out_y = (float)y;
    }
    if (out_w) {
        *out_w = (float)gw;
    }
    if (out_h) {
        *out_h = (float)gh;
    }
    return true;
}

void nano_ui_text_atlas_reset(NanoUiTextAtlas *atlas)
{
    if (!atlas) {
        return;
    }
    /* Page 0 keeps its texture and is cleared now; the others are cleared
     * when glyphs reach them again. */
    if (atlas->tex[0]) {
        open_page(atlas, 0);
    } else {
        atlas->pages = 0;
    }
}
