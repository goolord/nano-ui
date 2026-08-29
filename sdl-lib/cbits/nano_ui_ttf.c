#include "nano_ui_opt.h"
#include <SDL3/SDL.h>
#include <SDL3_ttf/SDL_ttf.h>
#include <stdio.h>
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

static float rounded_rect_sdf(
    float px,
    float py,
    float x,
    float y,
    float w,
    float h,
    float rad)
{
    float cx = x + w * 0.5f;
    float cy = y + h * 0.5f;
    float hx = w * 0.5f - rad;
    float hy = h * 0.5f - rad;
    float dx = fabsf(px - cx) - hx;
    float dy = fabsf(py - cy) - hy;
    float ax = fmaxf(dx, 0.f);
    float ay = fmaxf(dy, 0.f);
    return sqrtf(ax * ax + ay * ay) + fminf(fmaxf(dx, dy), 0.f) - rad;
}

static bool fill_scanline_interior(
    SDL_Renderer *renderer,
    Uint8 r,
    Uint8 g,
    Uint8 b,
    Uint8 a,
    float x,
    float y,
    float w,
    float h,
    float rad)
{
    SDL_SetRenderDrawColor(renderer, r, g, b, a);

    int y0 = (int)floorf(y + 0.5f);
    int y1 = (int)floorf(y + h - 0.5f);
    float corner_top = y + rad;
    float corner_bot = y + h - rad;

    for (int py = y0; py <= y1; py++) {
        float cy = (float)py + 0.5f;
        float left = x;
        float right = x + w;

        if (cy < corner_top) {
            float dy = corner_top - cy;
            float inset = corner_inset(rad, dy);
            left = x + inset;
            right = x + w - inset;
        } else if (cy >= corner_bot) {
            float dy = cy - corner_bot;
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

static bool fill_edge_aa(
    SDL_Renderer *renderer,
    Uint8 r,
    Uint8 g,
    Uint8 b,
    Uint8 a,
    float x,
    float y,
    float w,
    float h,
    float rad)
{
    int x0 = (int)floorf(x - 1.f);
    int x1 = (int)ceilf(x + w + 1.f);
    int y0 = (int)floorf(y - 1.f);
    int y1 = (int)ceilf(y + h + 1.f);

    for (int py = y0; py < y1; py++) {
        for (int px = x0; px < x1; px++) {
            float sx = (float)px + 0.5f;
            float sy = (float)py + 0.5f;
            float dist = rounded_rect_sdf(sx, sy, x, y, w, h, rad);
            if (dist <= 0.f) {
                continue;
            }
            if (dist >= 1.f) {
                continue;
            }
            float coverage = 1.f - dist;
            if (coverage <= 0.f) {
                continue;
            }
            Uint8 aa = (Uint8)fminf(255.f, (float)a * coverage + 0.5f);
            SDL_SetRenderDrawColor(renderer, r, g, b, aa);
            SDL_FRect pixel = {(float)px, (float)py, 1.f, 1.f};
            if (!SDL_RenderFillRect(renderer, &pixel)) {
                return false;
            }
        }
    }
    return true;
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

    if (!fill_scanline_interior(renderer, r, g, b, a, x, y, w, h, rad)) {
        return false;
    }
    return fill_edge_aa(renderer, r, g, b, a, x, y, w, h, rad);
}

static float clampf01(float v)
{
    if (v < 0.f) {
        return 0.f;
    }
    if (v > 1.f) {
        return 1.f;
    }
    return v;
}

bool nano_ui_stroke_rounded_rect(
    SDL_Renderer *renderer,
    Uint8 r,
    Uint8 g,
    Uint8 b,
    Uint8 a,
    float x,
    float y,
    float w,
    float h,
    float radius,
    float bw)
{
    if (w <= 0.f || h <= 0.f || bw <= 0.f) {
        return true;
    }

    float rad = radius;
    if (rad > w / 2.f) {
        rad = w / 2.f;
    }
    if (rad > h / 2.f) {
        rad = h / 2.f;
    }

    float ibw = fminf(bw, fminf(w, h) * 0.5f);
    float ix = x + ibw;
    float iy = y + ibw;
    float iw = w - 2.f * ibw;
    float ih = h - 2.f * ibw;
    float ir = fmaxf(0.f, rad - ibw);
    bool has_inner = iw > 0.f && ih > 0.f;

    int x0 = (int)floorf(x - 1.f);
    int x1 = (int)ceilf(x + w + 1.f);
    int y0 = (int)floorf(y - 1.f);
    int y1 = (int)ceilf(y + h + 1.f);
    float skip_l = ix + 1.f;
    float skip_r = ix + iw - 1.f;
    float skip_t = iy + 1.f;
    float skip_b = iy + ih - 1.f;
    bool skip_ok = has_inner && iw > 2.f && ih > 2.f;

    for (int py = y0; py < y1; py++) {
        for (int px = x0; px < x1; px++) {
            float sx = (float)px + 0.5f;
            float sy = (float)py + 0.5f;
            if (skip_ok && sx > skip_l && sx < skip_r && sy > skip_t && sy < skip_b) {
                continue;
            }
            float d_out = rounded_rect_sdf(sx, sy, x, y, w, h, rad);
            float d_in = has_inner ? rounded_rect_sdf(sx, sy, ix, iy, iw, ih, ir) : 1.f;
            float coverage = (1.f - clampf01(d_out)) - (1.f - clampf01(d_in));
            if (coverage <= 0.f) {
                continue;
            }
            Uint8 aa = (Uint8)fminf(255.f, (float)a * coverage + 0.5f);
            SDL_SetRenderDrawColor(renderer, r, g, b, aa);
            SDL_FRect pixel = {(float)px, (float)py, 1.f, 1.f};
            if (!SDL_RenderFillRect(renderer, &pixel)) {
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
    SDL_SetTextureBlendMode(texture, SDL_BLENDMODE_BLEND);
    SDL_SetTextureScaleMode(texture, SDL_SCALEMODE_NEAREST);

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

bool nano_ui_ttf_render_surface(
    TTF_Font *font,
    const char *text,
    size_t len,
    Uint8 r,
    Uint8 g,
    Uint8 b,
    Uint8 a,
    SDL_Surface **out_surface,
    float *out_w,
    float *out_h)
{
    if (!font || !text || !out_surface) {
        return false;
    }
    SDL_Color fg = {r, g, b, a};
    SDL_Surface *surface = TTF_RenderText_Blended(font, text, len, fg);
    if (!surface) {
        return false;
    }
    *out_surface = surface;
    if (out_w) {
        *out_w = (float)surface->w;
    }
    if (out_h) {
        *out_h = (float)surface->h;
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

bool nano_ui_render_texture_sized(
    SDL_Renderer *renderer,
    SDL_Texture *texture,
    float x,
    float y,
    float w,
    float h)
{
    if (!renderer || !texture || w <= 0.f || h <= 0.f) {
        return false;
    }
    SDL_FRect dst = {x, y, w, h};
    return SDL_RenderTexture(renderer, texture, NULL, &dst);
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
    return nano_ui_render_texture_sized(renderer, texture, x, y, tw, th);
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

static float edge_x_at_y(float y, float x0, float y0, float x1, float y1)
{
    if (fabsf(y1 - y0) < 1e-6f) {
        return x0;
    }
    return x0 + (y - y0) * (x1 - x0) / (y1 - y0);
}

static void add_edge_hits(
    float y,
    float x0,
    float y0,
    float x1,
    float y1,
    float *xs,
    int *count)
{
    float lo = fminf(y0, y1);
    float hi = fmaxf(y0, y1);
    if (y < lo - 0.5f || y > hi + 0.5f) {
        return;
    }
    if (fabsf(y1 - y0) < 1e-6f) {
        xs[(*count)++] = fminf(x0, x1);
        xs[(*count)++] = fmaxf(x0, x1);
        return;
    }
    xs[(*count)++] = edge_x_at_y(y, x0, y0, x1, y1);
}

static float dist_to_segment(
    float px,
    float py,
    float x0,
    float y0,
    float x1,
    float y1)
{
    float dx = x1 - x0;
    float dy = y1 - y0;
    float len2 = dx * dx + dy * dy;
    if (len2 < 1e-6f) {
        float qx = px - x0;
        float qy = py - y0;
        return sqrtf(qx * qx + qy * qy);
    }
    float t = fmaxf(0.f, fminf(1.f, ((px - x0) * dx + (py - y0) * dy) / len2));
    float qx = x0 + t * dx - px;
    float qy = y0 + t * dy - py;
    return sqrtf(qx * qx + qy * qy);
}

static bool point_in_triangle(
    float px,
    float py,
    float x0,
    float y0,
    float x1,
    float y1,
    float x2,
    float y2)
{
    float d0 = (x1 - x0) * (py - y0) - (y1 - y0) * (px - x0);
    float d1 = (x2 - x1) * (py - y1) - (y2 - y1) * (px - x1);
    float d2 = (x0 - x2) * (py - y2) - (y0 - y2) * (px - x2);
    bool has_neg = (d0 < 0.f) || (d1 < 0.f) || (d2 < 0.f);
    bool has_pos = (d0 > 0.f) || (d1 > 0.f) || (d2 > 0.f);
    return !(has_neg && has_pos);
}

static float triangle_edge_dist(
    float px,
    float py,
    float x0,
    float y0,
    float x1,
    float y1,
    float x2,
    float y2)
{
    float d0 = dist_to_segment(px, py, x0, y0, x1, y1);
    float d1 = dist_to_segment(px, py, x1, y1, x2, y2);
    float d2 = dist_to_segment(px, py, x2, y2, x0, y0);
    float d = fminf(d0, fminf(d1, d2));
    if (point_in_triangle(px, py, x0, y0, x1, y1, x2, y2)) {
        return -d;
    }
    return d;
}

static bool fill_triangle_edge_aa(
    SDL_Renderer *renderer,
    Uint8 r,
    Uint8 g,
    Uint8 b,
    Uint8 a,
    float x0,
    float y0,
    float x1,
    float y1,
    float x2,
    float y2)
{
    float min_x = fminf(x0, fminf(x1, x2));
    float max_x = fmaxf(x0, fmaxf(x1, x2));
    float min_y = fminf(y0, fminf(y1, y2));
    float max_y = fmaxf(y0, fmaxf(y1, y2));

    int x_start = (int)floorf(min_x - 1.f);
    int x_end = (int)ceilf(max_x + 1.f);
    int y_start = (int)floorf(min_y - 1.f);
    int y_end = (int)ceilf(max_y + 1.f);

    for (int py = y_start; py < y_end; py++) {
        for (int px = x_start; px < x_end; px++) {
            float sx = (float)px + 0.5f;
            float sy = (float)py + 0.5f;
            float dist = triangle_edge_dist(sx, sy, x0, y0, x1, y1, x2, y2);
            if (dist <= 0.f) {
                continue;
            }
            if (dist >= 0.5f) {
                continue;
            }
            float coverage = 0.5f - dist;
            if (coverage <= 0.f) {
                continue;
            }
            Uint8 aa = (Uint8)fminf(255.f, (float)a * coverage + 0.5f);
            SDL_SetRenderDrawColor(renderer, r, g, b, aa);
            SDL_FRect pixel = {(float)px, (float)py, 1.f, 1.f};
            if (!SDL_RenderFillRect(renderer, &pixel)) {
                return false;
            }
        }
    }
    return true;
}

bool nano_ui_fill_triangle(
    SDL_Renderer *renderer,
    Uint8 r,
    Uint8 g,
    Uint8 b,
    Uint8 a,
    float x0,
    float y0,
    float x1,
    float y1,
    float x2,
    float y2)
{
    if (!renderer) {
        return false;
    }

    SDL_SetRenderDrawColor(renderer, r, g, b, a);

    float min_y = fminf(y0, fminf(y1, y2));
    float max_y = fmaxf(y0, fmaxf(y1, y2));
    int y_start = (int)floorf(min_y);
    int y_end = (int)ceilf(max_y);

    for (int py = y_start; py <= y_end; py++) {
        float y = (float)py + 0.5f;
        float xs[6];
        int count = 0;
        add_edge_hits(y, x0, y0, x1, y1, xs, &count);
        add_edge_hits(y, x1, y1, x2, y2, xs, &count);
        add_edge_hits(y, x2, y2, x0, y0, xs, &count);
        if (count < 2) {
            continue;
        }
        float left = xs[0];
        float right = xs[0];
        for (int i = 1; i < count; i++) {
            if (xs[i] < left) {
                left = xs[i];
            }
            if (xs[i] > right) {
                right = xs[i];
            }
        }
        if (right > left) {
            SDL_FRect row = {left, (float)py, right - left, 1.f};
            if (!SDL_RenderFillRect(renderer, &row)) {
                return false;
            }
        }
    }
    return fill_triangle_edge_aa(renderer, r, g, b, a, x0, y0, x1, y1, x2, y2);
}
