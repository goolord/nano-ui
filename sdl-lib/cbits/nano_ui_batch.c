#include "nano_ui_opt.h"
#include "nano_ui_batch.h"

#include <stdlib.h>
#include <string.h>

enum {
    NANO_UI_SOLID_BATCH = 512,
    NANO_UI_TEX_BATCH = 512
};

struct NanoUiBatch {
    SDL_Renderer *renderer;
    SDL_FRect *solid_rects;
    int solid_count;
    int solid_cap;
    uint8_t solid_r;
    uint8_t solid_g;
    uint8_t solid_b;
    uint8_t solid_a;
    bool solid_active;

    SDL_Texture *tex;
    float tex_w;
    float tex_h;
    SDL_FRect *src_rects;
    SDL_FRect *dst_rects;
    uint8_t tint_r;
    uint8_t tint_g;
    uint8_t tint_b;
    uint8_t tint_a;
    bool tint_active;
    int tex_count;
    int tex_cap;
};

static bool grow_solid(NanoUiBatch *batch)
{
    int cap = batch->solid_cap ? batch->solid_cap * 2 : NANO_UI_SOLID_BATCH;
    SDL_FRect *next = (SDL_FRect *)realloc(batch->solid_rects, (size_t)cap * sizeof(SDL_FRect));
    if (!next) {
        return false;
    }
    batch->solid_rects = next;
    batch->solid_cap = cap;
    return true;
}

static bool grow_tex(NanoUiBatch *batch)
{
    int cap = batch->tex_cap ? batch->tex_cap * 2 : NANO_UI_TEX_BATCH;
    SDL_FRect *next_src = (SDL_FRect *)realloc(batch->src_rects, (size_t)cap * sizeof(SDL_FRect));
    SDL_FRect *next_dst = (SDL_FRect *)realloc(batch->dst_rects, (size_t)cap * sizeof(SDL_FRect));
    if (!next_src || !next_dst) {
        free(next_src);
        return false;
    }
    batch->src_rects = next_src;
    batch->dst_rects = next_dst;
    batch->tex_cap = cap;
    return true;
}

static void flush_solid(NanoUiBatch *batch)
{
    if (!batch || batch->solid_count <= 0) {
        return;
    }
    SDL_SetRenderDrawColor(batch->renderer, batch->solid_r, batch->solid_g, batch->solid_b, batch->solid_a);
    SDL_RenderFillRects(batch->renderer, batch->solid_rects, batch->solid_count);
    batch->solid_count = 0;
    batch->solid_active = false;
}

static void flush_tex(NanoUiBatch *batch)
{
    if (!batch || batch->tex_count <= 0 || !batch->tex) {
        return;
    }
    if (batch->tint_active) {
        SDL_SetTextureColorMod(batch->tex, batch->tint_r, batch->tint_g, batch->tint_b);
        SDL_SetTextureAlphaMod(batch->tex, batch->tint_a);
    }
    for (int i = 0; i < batch->tex_count; i++) {
        SDL_RenderTexture(batch->renderer, batch->tex, &batch->src_rects[i], &batch->dst_rects[i]);
    }
    if (batch->tint_active) {
        SDL_SetTextureColorMod(batch->tex, 255, 255, 255);
        SDL_SetTextureAlphaMod(batch->tex, 255);
    }
    batch->tex_count = 0;
    batch->tex = NULL;
    batch->tint_active = false;
}

NanoUiBatch *nano_ui_batch_create(SDL_Renderer *renderer)
{
    if (!renderer) {
        return NULL;
    }
    NanoUiBatch *batch = (NanoUiBatch *)calloc(1, sizeof(NanoUiBatch));
    if (!batch) {
        return NULL;
    }
    batch->renderer = renderer;
    return batch;
}

void nano_ui_batch_destroy(NanoUiBatch *batch)
{
    if (!batch) {
        return;
    }
    nano_ui_batch_flush(batch);
    free(batch->solid_rects);
    free(batch->src_rects);
    free(batch->dst_rects);
    free(batch);
}

void nano_ui_batch_flush(NanoUiBatch *batch)
{
    if (!batch) {
        return;
    }
    flush_tex(batch);
    flush_solid(batch);
}

void nano_ui_batch_fill_solid(
    NanoUiBatch *batch,
    uint8_t r,
    uint8_t g,
    uint8_t b,
    uint8_t a,
    float x,
    float y,
    float w,
    float h)
{
    if (!batch || w <= 0.f || h <= 0.f) {
        return;
    }
    if (batch->solid_active
        && (batch->solid_r != r || batch->solid_g != g || batch->solid_b != b || batch->solid_a != a)) {
        flush_solid(batch);
    }
    flush_tex(batch);
    if (batch->solid_count >= batch->solid_cap && !grow_solid(batch)) {
        flush_solid(batch);
        SDL_SetRenderDrawColor(batch->renderer, r, g, b, a);
        SDL_FRect one = {x, y, w, h};
        SDL_RenderFillRect(batch->renderer, &one);
        return;
    }
    batch->solid_rects[batch->solid_count++] = (SDL_FRect){x, y, w, h};
    batch->solid_r = r;
    batch->solid_g = g;
    batch->solid_b = b;
    batch->solid_a = a;
    batch->solid_active = true;
}

bool nano_ui_batch_texture_dst(
    NanoUiBatch *batch,
    SDL_Texture *texture,
    float tex_w,
    float tex_h,
    float dst_x,
    float dst_y,
    float dst_w,
    float dst_h,
    float u0,
    float v0,
    float u1,
    float v1,
    uint8_t r,
    uint8_t g,
    uint8_t b,
    uint8_t a)
{
    if (!batch || !texture || dst_w <= 0.f || dst_h <= 0.f) {
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
    SDL_FRect dst = {dst_x, dst_y, dst_w, dst_h};
    if (src.w <= 0.f || src.h <= 0.f) {
        return false;
    }
    bool tinted = (r != 255) || (g != 255) || (b != 255) || (a != 255);
    if (batch->tex_count > 0
        && (batch->tex != texture || batch->tint_r != r || batch->tint_g != g || batch->tint_b != b
            || batch->tint_a != a || batch->tint_active != tinted)) {
        flush_tex(batch);
    }
    flush_solid(batch);
    if (batch->tex_count >= batch->tex_cap && !grow_tex(batch)) {
        if (tinted) {
            SDL_SetTextureColorMod(texture, r, g, b);
            SDL_SetTextureAlphaMod(texture, a);
        }
        bool ok = SDL_RenderTexture(batch->renderer, texture, &src, &dst);
        if (tinted) {
            SDL_SetTextureColorMod(texture, 255, 255, 255);
            SDL_SetTextureAlphaMod(texture, 255);
        }
        return ok;
    }
    batch->tex = texture;
    batch->tex_w = tw;
    batch->tex_h = th;
    batch->tint_r = r;
    batch->tint_g = g;
    batch->tint_b = b;
    batch->tint_a = a;
    batch->tint_active = tinted;
    batch->src_rects[batch->tex_count] = src;
    batch->dst_rects[batch->tex_count] = dst;
    batch->tex_count++;
    return true;
}

bool nano_ui_batch_texture_sized(
    NanoUiBatch *batch,
    SDL_Texture *texture,
    float x,
    float y,
    float w,
    float h)
{
    return nano_ui_batch_texture_dst(batch, texture, w, h, x, y, w, h, 0.f, 0.f, 1.f, 1.f, 255, 255, 255, 255);
}
