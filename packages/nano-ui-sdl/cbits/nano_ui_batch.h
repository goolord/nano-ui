#ifndef NANO_UI_BATCH_H
#define NANO_UI_BATCH_H

#include <SDL3/SDL.h>
#include <stdint.h>

typedef struct NanoUiBatch NanoUiBatch;

NanoUiBatch *nano_ui_batch_create(SDL_Renderer *renderer);
void nano_ui_batch_destroy(NanoUiBatch *batch);
void nano_ui_batch_flush(NanoUiBatch *batch);

void nano_ui_batch_draw_range(
    NanoUiBatch *batch,
    const uint8_t *verts,
    int vert_count,
    const uint8_t *indices,
    int index_start,
    int index_n,
    SDL_Texture *texture,
    int has_damage,
    float dmg_x,
    float dmg_y,
    float dmg_w,
    float dmg_h);

#endif
