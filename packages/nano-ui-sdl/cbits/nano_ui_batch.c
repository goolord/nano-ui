#include "nano_ui_opt.h"
#include "nano_ui_batch.h"

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
    (void)has_damage;
    (void)dmg_x;
    (void)dmg_y;
    (void)dmg_w;
    (void)dmg_h;

    if (!batch || !verts || !indices || vert_count <= 0 || index_n < 3) {
        return;
    }
    if (index_start < 0) {
        index_start = 0;
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
