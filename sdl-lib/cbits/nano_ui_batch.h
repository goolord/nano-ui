#ifndef NANO_UI_BATCH_H
#define NANO_UI_BATCH_H

#include <SDL3/SDL.h>
#include <stdbool.h>
#include <stdint.h>

typedef struct NanoUiBatch NanoUiBatch;

NanoUiBatch *nano_ui_batch_create(SDL_Renderer *renderer);
void nano_ui_batch_destroy(NanoUiBatch *batch);
void nano_ui_batch_flush(NanoUiBatch *batch);

void nano_ui_batch_fill_solid(
    NanoUiBatch *batch,
    uint8_t r,
    uint8_t g,
    uint8_t b,
    uint8_t a,
    float x,
    float y,
    float w,
    float h);

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
    uint8_t a);

bool nano_ui_batch_texture_sized(
    NanoUiBatch *batch,
    SDL_Texture *texture,
    float x,
    float y,
    float w,
    float h);

#endif
