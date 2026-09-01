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
    float y2);

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
    float radius);

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
    float bw);

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
    float dmg_h);

void nano_ui_set_clip_rect(SDL_Renderer *renderer, int x, int y, int w, int h);
void nano_ui_clear_clip_rect(SDL_Renderer *renderer);

#endif
