#include <SDL3/SDL.h>
#include <SDL3_ttf/SDL_ttf.h>
#include <math.h>
#include <stddef.h>
#include <stdbool.h>

bool nano_ui_ttf_init(void)
{
    return TTF_Init();
}

void nano_ui_ttf_quit(void)
{
    TTF_Quit();
}

TTF_Font *nano_ui_ttf_open_font(const char *path, float ptsize)
{
    return TTF_OpenFont(path, ptsize);
}

void nano_ui_ttf_close_font(TTF_Font *font)
{
    if (font) {
        TTF_CloseFont(font);
    }
}

float nano_ui_ttf_line_skip(TTF_Font *font)
{
    return (float)TTF_GetFontLineSkip(font);
}

float nano_ui_ttf_ascent(TTF_Font *font)
{
    return (float)TTF_GetFontAscent(font);
}

float nano_ui_ttf_space_advance(TTF_Font *font)
{
    int w = 0;
    int h = 0;
    if (!TTF_GetStringSize(font, " ", 1, &w, &h)) {
        return 0.f;
    }
    return (float)w;
}

bool nano_ui_ttf_string_size(
    TTF_Font *font,
    const char *text,
    size_t len,
    float *out_w,
    float *out_h)
{
    int w = 0;
    int h = 0;
    if (!TTF_GetStringSize(font, text, len, &w, &h)) {
        return false;
    }
    if (out_w) {
        *out_w = (float)w;
    }
    if (out_h) {
        *out_h = (float)h;
    }
    return true;
}

static float corner_inset(float rad, float dist)
{
    if (dist >= rad) {
        return 0.f;
    }
    return rad - sqrtf(rad * rad - dist * dist);
}

bool nano_ui_fill_rounded_rect(
    SDL_Renderer *renderer,
    Uint8 r,
    Uint8 g,
    Uint8 b,
    Uint8 a,
    float x,
    float y,
    float w,
    float h,
    float radius)
{
    if (w <= 0.f || h <= 0.f) {
        return true;
    }

    SDL_SetRenderDrawColor(renderer, r, g, b, a);

    if (radius <= 0.5f) {
        SDL_FRect rect = {x, y, w, h};
        return SDL_RenderFillRect(renderer, &rect);
    }

    float rad = radius;
    if (rad > w / 2.f) {
        rad = w / 2.f;
    }
    if (rad > h / 2.f) {
        rad = h / 2.f;
    }

    int y0 = (int)floorf(y);
    int y1 = (int)ceilf(y + h);
    for (int py = y0; py < y1; py++) {
        float cy = (float)py + 0.5f;
        float left = x;
        float right = x + w;

        if (cy < y + rad) {
            float dy = (y + rad) - cy;
            float inset = corner_inset(rad, dy);
            left = x + inset;
            right = x + w - inset;
        } else if (cy >= y + h - rad) {
            float dy = cy - (y + h - rad);
            float inset = corner_inset(rad, dy);
            left = x + inset;
            right = x + w - inset;
        }

        if (right > left) {
            SDL_FRect row = {left, (float)py, right - left, 1.f};
            if (!SDL_RenderFillRect(renderer, &row)) {
                return false;
            }
        }
    }
    return true;
}

bool nano_ui_ttf_create_texture(
    SDL_Renderer *renderer,
    TTF_Font *font,
    const char *text,
    size_t len,
    Uint8 r,
    Uint8 g,
    Uint8 b,
    Uint8 a,
    SDL_Texture **out_texture,
    float *out_w,
    float *out_h)
{
    SDL_Color fg = {r, g, b, a};
    SDL_Surface *surface = TTF_RenderText_Blended(font, text, len, fg);
    if (!surface) {
        return false;
    }

    SDL_Texture *texture = SDL_CreateTextureFromSurface(renderer, surface);
    SDL_DestroySurface(surface);
    if (!texture) {
        return false;
    }

    float tw = 0.f;
    float th = 0.f;
    if (!SDL_GetTextureSize(texture, &tw, &th)) {
        SDL_DestroyTexture(texture);
        return false;
    }

    if (out_texture) {
        *out_texture = texture;
    } else {
        SDL_DestroyTexture(texture);
    }
    if (out_w) {
        *out_w = tw;
    }
    if (out_h) {
        *out_h = th;
    }
    return true;
}

bool nano_ui_render_texture(
    SDL_Renderer *renderer,
    SDL_Texture *texture,
    float x,
    float y)
{
    float tw = 0.f;
    float th = 0.f;
    if (!SDL_GetTextureSize(texture, &tw, &th)) {
        return false;
    }
    SDL_FRect dst = {x, y, tw, th};
    return SDL_RenderTexture(renderer, texture, NULL, &dst);
}

bool nano_ui_ttf_render_blended(
    SDL_Renderer *renderer,
    TTF_Font *font,
    const char *text,
    size_t len,
    Uint8 r,
    Uint8 g,
    Uint8 b,
    Uint8 a,
    float x,
    float y,
    SDL_Texture **out_texture,
    float *out_w,
    float *out_h)
{
    if (!nano_ui_ttf_create_texture(renderer, font, text, len, r, g, b, a, out_texture, out_w, out_h)) {
        return false;
    }
    if (out_texture && *out_texture) {
        return nano_ui_render_texture(renderer, *out_texture, x, y);
    }
    return true;
}

void nano_ui_destroy_texture(SDL_Texture *texture)
{
    if (texture) {
        SDL_DestroyTexture(texture);
    }
}
