#include "nano_ui_opt.h"
#include <SDL3/SDL.h>
#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>

bool nano_ui_create_rgba_texture(
    SDL_Renderer *renderer,
    const void *pixels,
    int w,
    int h,
    SDL_Texture **out_texture)
{
    if (!renderer || !pixels || !out_texture || w <= 0 || h <= 0) {
        return false;
    }
    SDL_Texture *texture =
        SDL_CreateTexture(renderer, SDL_PIXELFORMAT_RGBA32, SDL_TEXTUREACCESS_STATIC, w, h);
    if (!texture) {
        return false;
    }
    SDL_SetTextureBlendMode(texture, SDL_BLENDMODE_BLEND);
    if (!SDL_UpdateTexture(texture, NULL, pixels, w * 4)) {
        SDL_DestroyTexture(texture);
        return false;
    }
    *out_texture = texture;
    return true;
}

bool nano_ui_render_texture_dst(
    SDL_Renderer *renderer,
    SDL_Texture *texture,
    float tex_w,
    float tex_h,
    float x,
    float y,
    float w,
    float h,
    float u0,
    float v0,
    float u1,
    float v1,
    uint8_t r,
    uint8_t g,
    uint8_t b,
    uint8_t a)
{
    if (!renderer || !texture) {
        return false;
    }
    float tw = tex_w;
    float th = tex_h;
    if (tw <= 0.f || th <= 0.f) {
        if (!SDL_GetTextureSize(texture, &tw, &th)) {
            return false;
        }
    }
    SDL_FRect src = {u0 * tw, v0 * th, (u1 - u0) * tw, (v1 - v0) * th};
    SDL_FRect dst = {x, y, w, h};
    if (src.w <= 0.f || src.h <= 0.f) {
        return false;
    }
    bool tinted = (r != 255) || (g != 255) || (b != 255) || (a != 255);
    if (tinted) {
        SDL_SetTextureColorMod(texture, r, g, b);
        SDL_SetTextureAlphaMod(texture, a);
    }
    bool ok = SDL_RenderTexture(renderer, texture, &src, &dst);
    if (tinted) {
        SDL_SetTextureColorMod(texture, 255, 255, 255);
        SDL_SetTextureAlphaMod(texture, 255);
    }
    return ok;
}
