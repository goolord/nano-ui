#include <SDL3/SDL.h>
#include <stdbool.h>

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
