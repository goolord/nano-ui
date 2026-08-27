#include "nano_ui_opt.h"
#include "nano_ui_text_atlas.h"

#include <SDL3/SDL.h>
#include <stdlib.h>
#include <string.h>

enum {
    NANO_UI_TEXT_ATLAS_SIZE = 1024,
    NANO_UI_TEXT_ATLAS_PAD = 1
};

struct NanoUiTextAtlas {
    SDL_Renderer *renderer;
    SDL_Texture *tex;
    Uint8 *pixels;
    int w;
    int h;
    int x;
    int y;
    int row_h;
};

static bool upload_all(NanoUiTextAtlas *atlas)
{
    if (!atlas->tex || !atlas->pixels || atlas->w <= 0 || atlas->h <= 0) {
        return false;
    }
    return SDL_UpdateTexture(atlas->tex, NULL, atlas->pixels, atlas->w * 4);
}

static bool create_texture(NanoUiTextAtlas *atlas, int w, int h)
{
    SDL_Texture *tex =
        SDL_CreateTexture(atlas->renderer, SDL_PIXELFORMAT_RGBA32, SDL_TEXTUREACCESS_STREAMING, w, h);
    if (!tex) {
        return false;
    }
    SDL_SetTextureBlendMode(tex, SDL_BLENDMODE_BLEND);
    SDL_SetTextureScaleMode(tex, SDL_SCALEMODE_NEAREST);
    size_t bytes = (size_t)w * (size_t)h * 4;
    Uint8 *px = (Uint8 *)malloc(bytes);
    if (!px) {
        SDL_DestroyTexture(tex);
        return false;
    }
    memset(px, 0, bytes);
    if (atlas->pixels && atlas->w > 0 && atlas->h > 0) {
        int copy_w = atlas->w < w ? atlas->w : w;
        int copy_h = atlas->h < h ? atlas->h : h;
        for (int row = 0; row < copy_h; row++) {
            memcpy(
                px + (size_t)row * (size_t)w * 4,
                atlas->pixels + (size_t)row * (size_t)atlas->w * 4,
                (size_t)copy_w * 4);
        }
    }
    free(atlas->pixels);
    if (atlas->tex) {
        SDL_DestroyTexture(atlas->tex);
    }
    atlas->tex = tex;
    atlas->pixels = px;
    atlas->w = w;
    atlas->h = h;
    return upload_all(atlas);
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
        atlas->x = pad;
        atlas->y = pad;
        atlas->row_h = 0;
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

static bool blit_surface(NanoUiTextAtlas *atlas, SDL_Surface *surface, int x, int y)
{
    SDL_Surface *converted = SDL_ConvertSurface(surface, SDL_PIXELFORMAT_RGBA32);
    if (!converted) {
        return false;
    }
    Uint8 *src = (Uint8 *)converted->pixels;
    int src_pitch = converted->pitch;
    for (int row = 0; row < converted->h; row++) {
        Uint8 *dst = atlas->pixels + ((y + row) * atlas->w + x) * 4;
        memcpy(dst, src + (size_t)row * (size_t)src_pitch, (size_t)converted->w * 4);
    }
    SDL_Rect rect = {x, y, converted->w, converted->h};
    bool ok = SDL_UpdateTexture(atlas->tex, &rect, atlas->pixels + (y * atlas->w + x) * 4, atlas->w * 4);
    SDL_DestroySurface(converted);
    return ok;
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
    free(atlas->pixels);
    free(atlas);
}

SDL_Texture *nano_ui_text_atlas_texture(NanoUiTextAtlas *atlas)
{
    return atlas ? atlas->tex : NULL;
}

bool nano_ui_text_atlas_size(NanoUiTextAtlas *atlas, float *out_w, float *out_h)
{
    if (!atlas || !atlas->tex) {
        return false;
    }
    if (out_w) {
        *out_w = (float)atlas->w;
    }
    if (out_h) {
        *out_h = (float)atlas->h;
    }
    return true;
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
    if (!blit_surface(atlas, surface, x, y)) {
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
    atlas->x = NANO_UI_TEXT_ATLAS_PAD;
    atlas->y = NANO_UI_TEXT_ATLAS_PAD;
    atlas->row_h = 0;
    if (atlas->pixels && atlas->w > 0 && atlas->h > 0) {
        memset(atlas->pixels, 0, (size_t)atlas->w * (size_t)atlas->h * 4);
        upload_all(atlas);
    }
}
