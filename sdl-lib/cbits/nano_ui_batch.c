#include "nano_ui_opt.h"
#include "nano_ui_batch.h"

#include <math.h>
#include <stdlib.h>
#include <string.h>

enum { NANO_UI_VTX_STRIDE = 20 };

bool nano_ui_fill_rounded_rect(
    SDL_Renderer *renderer,
    uint8_t r,
    uint8_t g,
    uint8_t b,
    uint8_t a,
    float x,
    float y,
    float w,
    float h,
    float radius);
bool nano_ui_fill_triangle(
    SDL_Renderer *renderer,
    uint8_t r,
    uint8_t g,
    uint8_t b,
    uint8_t a,
    float x0,
    float y0,
    float x1,
    float y1,
    float x2,
    float y2);

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
    SDL_Vertex *verts;
    int *indices;
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
    size_t vbytes = (size_t)cap * 4 * sizeof(SDL_Vertex);
    size_t ibytes = (size_t)cap * 6 * sizeof(int);
    SDL_Vertex *verts = (SDL_Vertex *)malloc(vbytes);
    int *idx = (int *)malloc(ibytes);
    if (!verts || !idx) {
        free(verts);
        free(idx);
        return false;
    }
    if (batch->tex_count > 0) {
        memcpy(verts, batch->verts, (size_t)batch->tex_count * 4 * sizeof(SDL_Vertex));
        memcpy(idx, batch->indices, (size_t)batch->tex_count * 6 * sizeof(int));
    }
    free(batch->verts);
    free(batch->indices);
    batch->verts = verts;
    batch->indices = idx;
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

static void write_quad(
    SDL_Vertex *verts,
    int *indices,
    int base,
    SDL_FRect dst,
    float u0,
    float v0,
    float u1,
    float v1,
    uint8_t r,
    uint8_t g,
    uint8_t b,
    uint8_t a)
{
    SDL_FColor col = {(float)r / 255.f, (float)g / 255.f, (float)b / 255.f, (float)a / 255.f};
    verts[0] = (SDL_Vertex){{dst.x, dst.y}, col, {u0, v0}};
    verts[1] = (SDL_Vertex){{dst.x + dst.w, dst.y}, col, {u1, v0}};
    verts[2] = (SDL_Vertex){{dst.x + dst.w, dst.y + dst.h}, col, {u1, v1}};
    verts[3] = (SDL_Vertex){{dst.x, dst.y + dst.h}, col, {u0, v1}};
    indices[0] = base;
    indices[1] = base + 1;
    indices[2] = base + 2;
    indices[3] = base;
    indices[4] = base + 2;
    indices[5] = base + 3;
}

static bool draw_one_quad(
    SDL_Renderer *ren,
    SDL_Texture *texture,
    SDL_FRect dst,
    float u0,
    float v0,
    float u1,
    float v1,
    uint8_t r,
    uint8_t g,
    uint8_t b,
    uint8_t a)
{
    SDL_Vertex verts[4];
    int indices[6];
    write_quad(verts, indices, 0, dst, u0, v0, u1, v1, r, g, b, a);
    return SDL_RenderGeometry(ren, texture, verts, 4, indices, 6);
}

static void flush_tex(NanoUiBatch *batch)
{
    if (!batch || batch->tex_count <= 0 || !batch->tex) {
        return;
    }
    int n = batch->tex_count;
    bool ok = SDL_RenderGeometry(
        batch->renderer,
        batch->tex,
        batch->verts,
        n * 4,
        batch->indices,
        n * 6);
    if (!ok) {
        int one[6] = {0, 1, 2, 0, 2, 3};
        for (int i = 0; i < n; i++) {
            SDL_RenderGeometry(batch->renderer, batch->tex, &batch->verts[i * 4], 4, one, 6);
        }
    }
    batch->tex_count = 0;
    batch->tex = NULL;
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
    free(batch->verts);
    free(batch->indices);
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
    if (!batch || !texture || dst_w <= 0.f || dst_h <= 0.f || u1 <= u0 || v1 <= v0) {
        return false;
    }
    (void)tex_w;
    (void)tex_h;
    SDL_FRect dst = {dst_x, dst_y, dst_w, dst_h};
    if (batch->tex_count > 0 && batch->tex != texture) {
        flush_tex(batch);
    }
    flush_solid(batch);
    if (batch->tex_count >= batch->tex_cap && !grow_tex(batch)) {
        flush_tex(batch);
        if (batch->tex_count >= batch->tex_cap && !grow_tex(batch)) {
            return draw_one_quad(batch->renderer, texture, dst, u0, v0, u1, v1, r, g, b, a);
        }
    }
    int i = batch->tex_count;
    write_quad(&batch->verts[i * 4], &batch->indices[i * 6], i * 4, dst, u0, v0, u1, v1, r, g, b, a);
    batch->tex = texture;
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

static bool load_vtx(const uint8_t *verts, int vert_count, uint32_t idx, float *x, float *y, float *u, float *v, uint32_t *rgba)
{
    if ((int)idx < 0 || (int)idx >= vert_count) {
        return false;
    }
    const uint8_t *p = verts + (size_t)idx * (size_t)NANO_UI_VTX_STRIDE;
    memcpy(x, p, 4);
    memcpy(y, p + 4, 4);
    memcpy(u, p + 8, 4);
    memcpy(v, p + 12, 4);
    memcpy(rgba, p + 16, 4);
    return true;
}

static bool hits_damage(
    int has_damage,
    float dx,
    float dy,
    float dw,
    float dh,
    float x0,
    float y0,
    float x1,
    float y1,
    float x2,
    float y2)
{
    if (!has_damage) {
        return true;
    }
    float minx = fminf(x0, fminf(x1, x2));
    float maxx = fmaxf(x0, fmaxf(x1, x2));
    float miny = fminf(y0, fminf(y1, y2));
    float maxy = fmaxf(y0, fmaxf(y1, y2));
    float ix = fmaxf(minx, dx);
    float iy = fmaxf(miny, dy);
    float ix1 = fminf(maxx, dx + dw);
    float iy1 = fminf(maxy, dy + dh);
    return ix < ix1 && iy < iy1;
}

void nano_ui_batch_draw_range(
    NanoUiBatch *batch,
    const uint8_t *verts,
    int vert_count,
    const uint8_t *indices,
    int index_count,
    int index_start,
    int index_n,
    int tex_id,
    SDL_Texture *texture,
    float tex_w,
    float tex_h,
    float scale,
    int has_damage,
    float dmg_x,
    float dmg_y,
    float dmg_w,
    float dmg_h)
{
    if (!batch || !verts || !indices || vert_count <= 0 || index_n < 3 || scale <= 0.f) {
        return;
    }
    int end = index_start + index_n;
    if (index_start < 0) {
        index_start = 0;
    }
    if (end > index_count) {
        end = index_count;
    }
    const uint32_t *idx = (const uint32_t *)indices;
    SDL_Renderer *ren = batch->renderer;
    for (int i = index_start; i + 2 < end; i += 3) {
        float x0, y0, u0, v0, x1, y1, u1, v1, x2, y2, u2, v2;
        uint32_t rgba0, rgba1, rgba2;
        if (!load_vtx(verts, vert_count, idx[i], &x0, &y0, &u0, &v0, &rgba0)
            || !load_vtx(verts, vert_count, idx[i + 1], &x1, &y1, &u1, &v1, &rgba1)
            || !load_vtx(verts, vert_count, idx[i + 2], &x2, &y2, &u2, &v2, &rgba2)) {
            continue;
        }
        (void)u1;
        (void)v1;
        (void)u2;
        (void)v2;
        (void)rgba1;
        (void)rgba2;
        if (!hits_damage(has_damage, dmg_x, dmg_y, dmg_w, dmg_h, x0, y0, x1, y1, x2, y2)) {
            continue;
        }
        uint8_t r = (uint8_t)((rgba0 >> 24) & 0xFFu);
        uint8_t g = (uint8_t)((rgba0 >> 16) & 0xFFu);
        uint8_t b = (uint8_t)((rgba0 >> 8) & 0xFFu);
        uint8_t a = (uint8_t)(rgba0 & 0xFFu);
        if (tex_id > 0) {
            float w = x2 - x0;
            float h = y2 - y0;
            if (w <= 0.f || h <= 0.f) {
                continue;
            }
            float px = x0 * scale;
            float py = y0 * scale;
            float pw = w * scale;
            float ph = h * scale;
            if (texture) {
                nano_ui_batch_texture_dst(batch, texture, tex_w, tex_h, px, py, pw, ph, u0, v0, u2, v2, r, g, b, a);
            } else {
                nano_ui_batch_fill_solid(batch, r, g, b, a, px, py, pw, ph);
            }
        } else if (u0 <= -1.5f) {
            nano_ui_batch_flush(batch);
            nano_ui_fill_triangle(
                ren,
                r,
                g,
                b,
                a,
                x0 * scale,
                y0 * scale,
                x1 * scale,
                y1 * scale,
                x2 * scale,
                y2 * scale);
        } else {
            float w = x2 - x0;
            float h = y2 - y0;
            if (w <= 0.f || h <= 0.f) {
                continue;
            }
            float px = x0 * scale;
            float py = y0 * scale;
            float pw = w * scale;
            float ph = h * scale;
            if (v0 < 0.f) {
                nano_ui_batch_flush(batch);
                nano_ui_fill_rounded_rect(ren, r, g, b, a, px, py, pw, ph, u0 * scale);
            } else {
                nano_ui_batch_fill_solid(batch, r, g, b, a, px, py, pw, ph);
            }
        }
    }
}
