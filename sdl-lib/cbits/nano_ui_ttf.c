#include "nano_ui_opt.h"
#include <SDL3/SDL.h>
#include <SDL3_ttf/SDL_ttf.h>
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
    /* TTF_GetStringSize: length 0 means NUL-terminated. Haskell empty Text
     * is not a C string, so pass a real empty literal instead of strlen. */
    if (len == 0) {
        text = "";
    }
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

bool nano_ui_ttf_glyph_metrics(
    TTF_Font *font,
    Uint32 codepoint,
    int *out_minx,
    int *out_maxx,
    int *out_miny,
    int *out_maxy,
    int *out_advance)
{
    if (!font) {
        return false;
    }
    int minx = 0, maxx = 0, miny = 0, maxy = 0, advance = 0;
    if (!TTF_GetGlyphMetrics(font, codepoint, &minx, &maxx, &miny, &maxy, &advance)) {
        return false;
    }
    if (out_minx)   *out_minx   = minx;
    if (out_maxx)   *out_maxx   = maxx;
    if (out_miny)   *out_miny   = miny;
    if (out_maxy)   *out_maxy   = maxy;
    if (out_advance) *out_advance = advance;
    return true;
}

/* Render a single glyph as a white-on-alpha surface so vertex color can tint it.
   The surface uses SDL_PIXELFORMAT_RGBA32 with R=G=B=255, A=coverage. */
bool nano_ui_ttf_render_glyph_surface(
    TTF_Font *font,
    Uint32 codepoint,
    SDL_Surface **out_surface)
{
    if (!font || !out_surface) {
        return false;
    }
    SDL_Color white = {255, 255, 255, 255};
    SDL_Surface *raw = TTF_RenderGlyph_Blended(font, codepoint, white);
    if (!raw) {
        return false;
    }
    /* Convert to RGBA32 to ensure consistent pixel layout for the atlas blitter. */
    SDL_Surface *converted = SDL_ConvertSurface(raw, SDL_PIXELFORMAT_RGBA32);
    SDL_DestroySurface(raw);
    if (!converted) {
        return false;
    }
    *out_surface = converted;
    return true;
}

void nano_ui_destroy_texture(SDL_Texture *texture)
{
    if (texture) {
        SDL_DestroyTexture(texture);
    }
}
