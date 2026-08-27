#include "nano_ui_opt.h"
#include "nano_ui_batch.h"

#include <math.h>
#include <stdlib.h>
#include <string.h>

enum { NANO_UI_VTX_STRIDE = 20 };
enum { NANO_UI_CORNER_SEGS = 8 };
enum {
    NANO_UI_SOLID_BATCH = 512,
    NANO_UI_GEOM_VERTS = 2048,
    NANO_UI_GEOM_IDX = 3072
};

#ifndef NANO_UI_PI
#define NANO_UI_PI 3.14159265358979323846f
#endif

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
bool nano_ui_stroke_rounded_rect(
    SDL_Renderer *renderer,
    uint8_t r,
    uint8_t g,
    uint8_t b,
    uint8_t a,
    float x,
    float y,
    float w,
    float h,
    float radius,
    float bw);
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
    bool tex_set;
    SDL_Vertex *verts;
    int *indices;
    int vert_count;
    int index_count;
    int vert_cap;
    int index_cap;
};

static SDL_FColor color_f(uint8_t r, uint8_t g, uint8_t b, uint8_t a)
{
    return (SDL_FColor){(float)r / 255.f, (float)g / 255.f, (float)b / 255.f, (float)a / 255.f};
}

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

static bool grow_geom(NanoUiBatch *batch)
{
    int vcap = batch->vert_cap ? batch->vert_cap * 2 : NANO_UI_GEOM_VERTS;
    int icap = batch->index_cap ? batch->index_cap * 2 : NANO_UI_GEOM_IDX;
    SDL_Vertex *verts = (SDL_Vertex *)malloc((size_t)vcap * sizeof(SDL_Vertex));
    int *idx = (int *)malloc((size_t)icap * sizeof(int));
    if (!verts || !idx) {
        free(verts);
        free(idx);
        return false;
    }
    if (batch->vert_count > 0) {
        memcpy(verts, batch->verts, (size_t)batch->vert_count * sizeof(SDL_Vertex));
    }
    if (batch->index_count > 0) {
        memcpy(idx, batch->indices, (size_t)batch->index_count * sizeof(int));
    }
    free(batch->verts);
    free(batch->indices);
    batch->verts = verts;
    batch->indices = idx;
    batch->vert_cap = vcap;
    batch->index_cap = icap;
    return true;
}

static bool ensure_geom(NanoUiBatch *batch, int need_v, int need_i)
{
    while (batch->vert_count + need_v > batch->vert_cap || batch->index_count + need_i > batch->index_cap) {
        if (!grow_geom(batch)) {
            return false;
        }
    }
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
    SDL_FColor col)
{
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
    write_quad(verts, indices, 0, dst, u0, v0, u1, v1, color_f(r, g, b, a));
    return SDL_RenderGeometry(ren, texture, verts, 4, indices, 6);
}

static void flush_geom(NanoUiBatch *batch)
{
    if (!batch || batch->vert_count <= 0 || batch->index_count <= 0) {
        batch->vert_count = 0;
        batch->index_count = 0;
        batch->tex = NULL;
        batch->tex_set = false;
        return;
    }
    bool ok = SDL_RenderGeometry(
        batch->renderer,
        batch->tex,
        batch->verts,
        batch->vert_count,
        batch->indices,
        batch->index_count);
    if (!ok) {
        int one[3] = {0, 1, 2};
        for (int i = 0; i + 2 < batch->index_count; i += 3) {
            SDL_Vertex tri[3] = {
                batch->verts[batch->indices[i]],
                batch->verts[batch->indices[i + 1]],
                batch->verts[batch->indices[i + 2]]
            };
            SDL_RenderGeometry(batch->renderer, batch->tex, tri, 3, one, 3);
        }
    }
    batch->vert_count = 0;
    batch->index_count = 0;
    batch->tex = NULL;
    batch->tex_set = false;
}

static void begin_geom(NanoUiBatch *batch, SDL_Texture *texture)
{
    flush_solid(batch);
    if (batch->tex_set && batch->tex != texture) {
        flush_geom(batch);
    }
    batch->tex = texture;
    batch->tex_set = true;
}

static bool push_quad(NanoUiBatch *batch, SDL_FRect dst, float u0, float v0, float u1, float v1, SDL_FColor col)
{
    if (dst.w <= 0.f || dst.h <= 0.f) {
        return true;
    }
    if (!ensure_geom(batch, 4, 6)) {
        return false;
    }
    int base = batch->vert_count;
    write_quad(&batch->verts[base], &batch->indices[batch->index_count], base, dst, u0, v0, u1, v1, col);
    batch->vert_count += 4;
    batch->index_count += 6;
    return true;
}

static bool push_tri(
    NanoUiBatch *batch,
    float x0,
    float y0,
    float x1,
    float y1,
    float x2,
    float y2,
    SDL_FColor col)
{
    if (!ensure_geom(batch, 3, 3)) {
        return false;
    }
    int base = batch->vert_count;
    batch->verts[base] = (SDL_Vertex){{x0, y0}, col, {0.f, 0.f}};
    batch->verts[base + 1] = (SDL_Vertex){{x1, y1}, col, {0.f, 0.f}};
    batch->verts[base + 2] = (SDL_Vertex){{x2, y2}, col, {0.f, 0.f}};
    batch->indices[batch->index_count] = base;
    batch->indices[batch->index_count + 1] = base + 1;
    batch->indices[batch->index_count + 2] = base + 2;
    batch->vert_count += 3;
    batch->index_count += 3;
    return true;
}

static bool push_corner(
    NanoUiBatch *batch,
    float cx,
    float cy,
    float rad,
    float a0,
    float a1,
    SDL_FColor col)
{
    int segs = NANO_UI_CORNER_SEGS;
    if (!ensure_geom(batch, segs + 2, segs * 3)) {
        return false;
    }
    int center = batch->vert_count;
    batch->verts[center] = (SDL_Vertex){{cx, cy}, col, {0.f, 0.f}};
    batch->vert_count++;
    for (int i = 0; i <= segs; i++) {
        float t = (float)i / (float)segs;
        float a = a0 + (a1 - a0) * t;
        float x = cx + cosf(a) * rad;
        float y = cy + sinf(a) * rad;
        batch->verts[batch->vert_count] = (SDL_Vertex){{x, y}, col, {0.f, 0.f}};
        if (i > 0) {
            int rim1 = batch->vert_count - 1;
            int rim0 = batch->vert_count;
            batch->indices[batch->index_count] = center;
            batch->indices[batch->index_count + 1] = rim1;
            batch->indices[batch->index_count + 2] = rim0;
            batch->index_count += 3;
        }
        batch->vert_count++;
    }
    return true;
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
    flush_geom(batch);
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
    flush_geom(batch);
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
    begin_geom(batch, texture);
    if (!push_quad(batch, dst, u0, v0, u1, v1, color_f(r, g, b, a))) {
        flush_geom(batch);
        if (!push_quad(batch, dst, u0, v0, u1, v1, color_f(r, g, b, a))) {
            return draw_one_quad(batch->renderer, texture, dst, u0, v0, u1, v1, r, g, b, a);
        }
    }
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

void nano_ui_batch_triangle(
    NanoUiBatch *batch,
    uint8_t r,
    uint8_t g,
    uint8_t b,
    uint8_t a,
    float x0,
    float y0,
    float x1,
    float y1,
    float x2,
    float y2)
{
    if (!batch) {
        return;
    }
    begin_geom(batch, NULL);
    if (!push_tri(batch, x0, y0, x1, y1, x2, y2, color_f(r, g, b, a))) {
        flush_geom(batch);
        if (!push_tri(batch, x0, y0, x1, y1, x2, y2, color_f(r, g, b, a))) {
            nano_ui_fill_triangle(batch->renderer, r, g, b, a, x0, y0, x1, y1, x2, y2);
        }
    }
}

void nano_ui_batch_rounded_rect(
    NanoUiBatch *batch,
    uint8_t r,
    uint8_t g,
    uint8_t b,
    uint8_t a,
    float x,
    float y,
    float w,
    float h,
    float radius)
{
    if (!batch || w <= 0.f || h <= 0.f) {
        return;
    }
    float rad = radius;
    if (rad > w / 2.f) {
        rad = w / 2.f;
    }
    if (rad > h / 2.f) {
        rad = h / 2.f;
    }
    if (rad <= 0.5f) {
        nano_ui_batch_fill_solid(batch, r, g, b, a, x, y, w, h);
        return;
    }
    SDL_FColor col = color_f(r, g, b, a);
    int segs = NANO_UI_CORNER_SEGS;
    int need_v = 20 + 4 * (segs + 2);
    int need_i = 30 + 4 * segs * 3;
    begin_geom(batch, NULL);
    if (!ensure_geom(batch, need_v, need_i)) {
        flush_geom(batch);
        if (!ensure_geom(batch, need_v, need_i)) {
            nano_ui_batch_flush(batch);
            nano_ui_fill_rounded_rect(batch->renderer, r, g, b, a, x, y, w, h, rad);
            return;
        }
    }
    float mid_w = w - 2.f * rad;
    float mid_h = h - 2.f * rad;
    if (!push_quad(batch, (SDL_FRect){x + rad, y + rad, mid_w, mid_h}, 0.f, 0.f, 1.f, 1.f, col)
        || !push_quad(batch, (SDL_FRect){x + rad, y, mid_w, rad}, 0.f, 0.f, 1.f, 1.f, col)
        || !push_quad(batch, (SDL_FRect){x + rad, y + h - rad, mid_w, rad}, 0.f, 0.f, 1.f, 1.f, col)
        || !push_quad(batch, (SDL_FRect){x, y + rad, rad, mid_h}, 0.f, 0.f, 1.f, 1.f, col)
        || !push_quad(batch, (SDL_FRect){x + w - rad, y + rad, rad, mid_h}, 0.f, 0.f, 1.f, 1.f, col)
        || !push_corner(batch, x + rad, y + rad, rad, NANO_UI_PI, NANO_UI_PI * 1.5f, col)
        || !push_corner(batch, x + w - rad, y + rad, rad, NANO_UI_PI * 1.5f, NANO_UI_PI * 2.f, col)
        || !push_corner(batch, x + w - rad, y + h - rad, rad, 0.f, NANO_UI_PI * 0.5f, col)
        || !push_corner(batch, x + rad, y + h - rad, rad, NANO_UI_PI * 0.5f, NANO_UI_PI, col)) {
        nano_ui_batch_flush(batch);
        nano_ui_fill_rounded_rect(batch->renderer, r, g, b, a, x, y, w, h, rad);
    }
}

void nano_ui_batch_stroke_rounded_rect(
    NanoUiBatch *batch,
    uint8_t r,
    uint8_t g,
    uint8_t b,
    uint8_t a,
    float x,
    float y,
    float w,
    float h,
    float radius,
    float bw)
{
    if (!batch || w <= 0.f || h <= 0.f || bw <= 0.f) {
        return;
    }
    if (batch->vert_count > 0 || batch->solid_count > 0) {
        nano_ui_batch_flush(batch);
    }
    nano_ui_stroke_rounded_rect(batch->renderer, r, g, b, a, x, y, w, h, radius, bw);
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
            nano_ui_batch_triangle(
                batch,
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
                float rad = u0 * scale;
                if (v0 <= -2.f) {
                    float bw = (-v0 - 1.f) * scale;
                    if (bw < 1.f) {
                        bw = 1.f;
                    }
                    nano_ui_batch_stroke_rounded_rect(batch, r, g, b, a, px, py, pw, ph, rad, bw);
                } else {
                    nano_ui_batch_rounded_rect(batch, r, g, b, a, px, py, pw, ph, rad);
                }
                /* Quads emit two tris. Skip the paired tri so we stroke once. */
                i += 3;
            } else {
                nano_ui_batch_fill_solid(batch, r, g, b, a, px, py, pw, ph);
            }
        }
    }
}
