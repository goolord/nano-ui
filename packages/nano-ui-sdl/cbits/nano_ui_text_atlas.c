#include "nano_ui_text_atlas.h"

#include <SDL3/SDL.h>
#include <stdlib.h>

enum {
    NANO_UI_TEXT_ATLAS_SIZE = 2048,
    NANO_UI_TEXT_ATLAS_PAD = 1,
    NANO_UI_WHITE_PATCH_SIZE = 4
};

struct NanoUiTextAtlas {
    SDL_Renderer *renderer;
    SDL_Texture *tex;
    int w;
    int h;
    int x;
    int y;
    int row_h;
};

/* SDL owns the streaming storage. A lock is write-only, so initialize the
 * complete surface on creation/reset, including transparent glyph padding. */
static bool clear_texture(NanoUiTextAtlas *atlas)
{
    SDL_Surface *surface = NULL;
    if (!SDL_LockTextureToSurface(atlas->tex, NULL, &surface)) {
        return false;
    }
    SDL_Rect white = {0, 0, NANO_UI_WHITE_PATCH_SIZE, NANO_UI_WHITE_PATCH_SIZE};
    bool ok = SDL_FillSurfaceRect(surface, NULL, 0) &&
              SDL_FillSurfaceRect(surface, &white, 0xffffffffu);
    SDL_UnlockTexture(atlas->tex);
    return ok;
}

static bool create_texture(NanoUiTextAtlas *atlas, int w, int h)
{
    SDL_Texture *tex =
        SDL_CreateTexture(atlas->renderer, SDL_PIXELFORMAT_RGBA32, SDL_TEXTUREACCESS_STREAMING, w, h);
    if (!tex) {
        return false;
    }
    SDL_SetTextureBlendMode(tex, SDL_BLENDMODE_BLEND);
    /* Bilinear filtering keeps glyph quads smooth when a quad lands off a
     * whole texel boundary (fractional display scale, sub-pixel pen nudge,
     * shaped-run placement). NEAREST snaps to the closest texel and makes
     * scaled/slightly-misaligned text look blocky and pixelated. */
    SDL_SetTextureScaleMode(tex, SDL_SCALEMODE_LINEAR);
    atlas->tex = tex;
    if (!clear_texture(atlas)) {
        SDL_DestroyTexture(tex);
        atlas->tex = NULL;
        return false;
    }
    atlas->w = w;
    atlas->h = h;
    return true;
}

static bool slot_for(NanoUiTextAtlas *atlas, int gw, int gh, int *out_x, int *out_y)
{
    int pad = NANO_UI_TEXT_ATLAS_PAD;
    if (gw + 2 * pad > NANO_UI_TEXT_ATLAS_SIZE || gh + 2 * pad > NANO_UI_TEXT_ATLAS_SIZE) {
        return false;
    }
    if (!atlas->tex) {
        if (!create_texture(atlas, NANO_UI_TEXT_ATLAS_SIZE, NANO_UI_TEXT_ATLAS_SIZE)) {
            return false;
        }
        atlas->x = NANO_UI_WHITE_PATCH_SIZE + pad;
        atlas->y = pad;
        atlas->row_h = NANO_UI_WHITE_PATCH_SIZE;
    }
    if (atlas->x + gw + pad <= atlas->w && atlas->y + gh + pad <= atlas->h) {
        *out_x = atlas->x;
        *out_y = atlas->y;
        return true;
    }
    int next_y = atlas->y + (atlas->row_h > 0 ? atlas->row_h + pad : pad);
    if (next_y + gh + pad <= atlas->h && gw + 2 * pad <= atlas->w) {
        atlas->y = next_y;
        atlas->x = pad;
        atlas->row_h = 0;
        *out_x = atlas->x;
        *out_y = atlas->y;
        return true;
    }
    return false;
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
    if (atlas->tex) {
        SDL_DestroyTexture(atlas->tex);
    }
    free(atlas);
}

SDL_Texture *nano_ui_text_atlas_texture(NanoUiTextAtlas *atlas)
{
    return atlas ? atlas->tex : NULL;
}

bool nano_ui_text_atlas_insert_surface(
    NanoUiTextAtlas *atlas,
    SDL_Surface *surface,
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
    int x = 0;
    int y = 0;
    if (!slot_for(atlas, gw, gh, &x, &y)) {
        return false;
    }
    /* Glyph surfaces already have the atlas's RGBA32 format. Preserve their
     * pitch instead of copying each row into a second full-size CPU atlas. */
    SDL_Rect rect = {x, y, gw, gh};
    if (!SDL_UpdateTexture(atlas->tex, &rect, surface->pixels, surface->pitch)) {
        return false;
    }
    atlas->x = x + gw + NANO_UI_TEXT_ATLAS_PAD;
    if (gh > atlas->row_h) {
        atlas->row_h = gh;
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
    atlas->x = NANO_UI_WHITE_PATCH_SIZE + NANO_UI_TEXT_ATLAS_PAD;
    atlas->y = NANO_UI_TEXT_ATLAS_PAD;
    atlas->row_h = NANO_UI_WHITE_PATCH_SIZE;
    if (atlas->tex) {
        clear_texture(atlas);
    }
}
