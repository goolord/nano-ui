#include "nano_ui_opt.h"
#include "nano_ui_batch.h"

#include <stdlib.h>

struct NanoUiBatch {
    SDL_Renderer *renderer;
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
    free(batch);
}

void nano_ui_batch_flush(NanoUiBatch *batch)
{
    (void)batch;
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
    (void)index_count;
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
    nano_ui_batch_flush(batch);
    const SDL_Vertex *sdl_verts = (const SDL_Vertex *)verts;
    const int *idx = (const int *)indices + index_start;
    SDL_RenderGeometry(batch->renderer, texture, sdl_verts, vert_count, idx, index_n);
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
