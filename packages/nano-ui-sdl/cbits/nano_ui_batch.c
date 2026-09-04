#include "nano_ui_opt.h"
#include "nano_ui_batch.h"
#include "nano_ui_simd.h"

#include <stdlib.h>

struct NanoUiBatch {
    SDL_Renderer *renderer;
    const uint8_t *verts;
    int vert_count;
    const uint8_t *indices;
    int index_count;
    SDL_Texture *pending_texture;
    int pending_start;
    int pending_n;
};

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
    if (batch) {
        nano_ui_batch_flush(batch);
        free(batch);
    }
}

void nano_ui_batch_flush(NanoUiBatch *batch)
{
    if (!batch || !batch->renderer || batch->pending_n < 3) {
        if (batch) {
            batch->pending_n = 0;
            batch->pending_start = 0;
            batch->pending_texture = NULL;
        }
        return;
    }
    const SDL_Vertex *sdl_verts = (const SDL_Vertex *)batch->verts;
    const int *idx = (const int *)batch->indices + batch->pending_start;
    SDL_RenderGeometry(batch->renderer, batch->pending_texture, sdl_verts, batch->vert_count, idx, batch->pending_n);
    batch->pending_n = 0;
    batch->pending_start = 0;
    batch->pending_texture = NULL;
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
    (void)tex_id;
    (void)tex_w;
    (void)tex_h;
    (void)scale;

    if (!batch || !verts || !indices || vert_count <= 0 || index_n < 3) {
        return;
    }
    if (index_start < 0) {
        index_start = 0;
    }

    if (has_damage && dmg_w > 0.f && dmg_h > 0.f && index_n >= 6) {
        const SDL_Vertex *sdl_verts = (const SDL_Vertex *)verts;
        const int *idx = (const int *)indices + index_start;
        float dx0 = dmg_x;
        float dy0 = dmg_y;
        float dx1 = dmg_x + dmg_w;
        float dy1 = dmg_y + dmg_h;

        bool any_visible = false;
        int q = 0;
        for (; q + 6 <= index_n; q += 6) {
            int i0 = idx[q];
            int i2 = idx[q + 2];
            if (i0 >= 0 && i0 < vert_count && i2 >= 0 && i2 < vert_count) {
                float x0 = sdl_verts[i0].position.x;
                float y0 = sdl_verts[i0].position.y;
                float x1 = sdl_verts[i2].position.x;
                float y1 = sdl_verts[i2].position.y;
                float qx0 = x0 < x1 ? x0 : x1;
                float qx1 = x0 > x1 ? x0 : x1;
                float qy0 = y0 < y1 ? y0 : y1;
                float qy1 = y0 > y1 ? y0 : y1;
                if (nano_ui_aabb_intersects(qx0, qy0, qx1, qy1, dx0, dy0, dx1, dy1)) {
                    any_visible = true;
                    break;
                }
            } else {
                any_visible = true;
                break;
            }
        }
        if (!any_visible && q > 0) {
            return;
        }
    }

    if (batch->pending_n > 0 &&
        batch->verts == verts &&
        batch->indices == indices &&
        batch->pending_texture == texture &&
        batch->pending_start + batch->pending_n == index_start)
    {
        batch->pending_n += index_n;
        if (vert_count > batch->vert_count) {
            batch->vert_count = vert_count;
        }
        return;
    }

    nano_ui_batch_flush(batch);

    batch->verts = verts;
    batch->vert_count = vert_count;
    batch->indices = indices;
    batch->index_count = index_count;
    batch->pending_texture = texture;
    batch->pending_start = index_start;
    batch->pending_n = index_n;
}

void nano_ui_set_clip_rect(SDL_Renderer *renderer, int x, int y, int w, int h)
{
    SDL_Rect r = {x, y, w, h};
    SDL_SetRenderClipRect(renderer, &r);
}

void nano_ui_clear_clip_rect(SDL_Renderer *renderer)
{
    SDL_SetRenderClipRect(renderer, NULL);
}
