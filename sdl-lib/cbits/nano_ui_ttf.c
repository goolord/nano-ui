#include "nano_ui_opt.h"
#include <SDL3/SDL.h>
#include <SDL3_ttf/SDL_ttf.h>
#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>

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

TTF_Font *nano_ui_ttf_open_font_memory(const void *data, size_t size, float ptsize)
{
    SDL_IOStream *stream = SDL_IOFromConstMem(data, size);
    if (!stream) {
        return NULL;
    }
    SDL_PropertiesID props = SDL_CreateProperties();
    if (!props) {
        SDL_CloseIO(stream);
        return NULL;
    }
    SDL_SetPointerProperty(props, TTF_PROP_FONT_CREATE_IOSTREAM_POINTER, stream);
    SDL_SetBooleanProperty(props, TTF_PROP_FONT_CREATE_IOSTREAM_AUTOCLOSE_BOOLEAN, true);
    SDL_SetFloatProperty(props, TTF_PROP_FONT_CREATE_SIZE_FLOAT, ptsize);
    TTF_Font *font = TTF_OpenFontWithProperties(props);
    SDL_DestroyProperties(props);
    return font;
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
    if (TTF_GetStringSize(font, " ", 1, &w, &h) && w > 0) {
        return (float)w;
    }
    int advance = 0;
    if (TTF_GetGlyphMetrics(font, ' ', NULL, NULL, NULL, NULL, &advance) && advance > 0) {
        return (float)advance;
    }
    return 0.f;
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

static Uint8 glyph_channel_max(Uint8 r, Uint8 g, Uint8 b)
{
    Uint8 m = r;
    if (g > m) {
        m = g;
    }
    if (b > m) {
        m = b;
    }
    return m;
}

static void force_white_rgb(SDL_Surface *surf)
{
    if (!surf || !surf->pixels || surf->format != SDL_PIXELFORMAT_RGBA32) {
        return;
    }
    Uint8 *base = (Uint8 *)surf->pixels;
    int pitch = surf->pitch;
    for (int y = 0; y < surf->h; y++) {
        Uint8 *row = base + y * pitch;
        for (int x = 0; x < surf->w; x++) {
            Uint8 *px = row + x * 4;
            px[0] = 255;
            px[1] = 255;
            px[2] = 255;
        }
    }
}

static void invert_glyph_alpha(SDL_Surface *surf)
{
    if (!surf || !surf->pixels || surf->format != SDL_PIXELFORMAT_RGBA32) {
        return;
    }
    Uint8 *base = (Uint8 *)surf->pixels;
    int pitch = surf->pitch;
    for (int y = 0; y < surf->h; y++) {
        Uint8 *row = base + y * pitch;
        for (int x = 0; x < surf->w; x++) {
            row[x * 4 + 3] = (Uint8)(255 - row[x * 4 + 3]);
        }
    }
}

static SDL_Surface *glyph_image_to_rgba(SDL_Surface *raw, TTF_ImageType image_type)
{
    if (!raw) {
        return NULL;
    }

    SDL_Surface *out = SDL_ConvertSurface(raw, SDL_PIXELFORMAT_RGBA32);
    if (!out) {
        return NULL;
    }

    if (image_type == TTF_IMAGE_ALPHA || image_type == TTF_IMAGE_SDF) {
        /* Spec: color channels are white, alpha is coverage. Transparent
         * white (a=0, rgb=255) must stay transparent. Do not use luma. */
        force_white_rgb(out);
        return out;
    }

    Uint8 *base = (Uint8 *)out->pixels;
    int pitch = out->pitch;
    Uint32 opaque_sum = 0;
    Uint32 count = 0;
    for (int y = 0; y < out->h; y++) {
        Uint8 *row = base + y * pitch;
        for (int x = 0; x < out->w; x++) {
            Uint8 *px = row + x * 4;
            Uint8 luma = glyph_channel_max(px[0], px[1], px[2]);
            Uint8 a = px[3];
            Uint8 cov = a > luma ? a : luma;
            px[0] = 255;
            px[1] = 255;
            px[2] = 255;
            px[3] = cov;
            opaque_sum += cov;
            count += 1;
        }
    }
    if (count > 0 && opaque_sum > (255u * count) / 2u) {
        invert_glyph_alpha(out);
    }
    return out;
}

bool nano_ui_ttf_render_glyph_surface(
    TTF_Font *font,
    Uint32 codepoint,
    SDL_Surface **out_surface)
{
    if (!font || !out_surface) {
        return false;
    }

    TTF_ImageType image_type = TTF_IMAGE_INVALID;
    SDL_Surface *raw = TTF_GetGlyphImage(font, codepoint, &image_type);
    if (!raw) {
        SDL_Color white = {255, 255, 255, 255};
        raw = TTF_RenderGlyph_Blended(font, codepoint, white);
        image_type = TTF_IMAGE_ALPHA;
    }
    if (!raw) {
        return false;
    }

    SDL_Surface *converted = glyph_image_to_rgba(raw, image_type);
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
