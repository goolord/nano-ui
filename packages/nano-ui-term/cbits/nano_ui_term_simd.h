#ifndef NANO_UI_TERM_SIMD_H
#define NANO_UI_TERM_SIMD_H

#include <stdint.h>
#include <stdbool.h>

void nano_ui_term_fill_blanks_simd(uint32_t *cells, int len);

void nano_ui_term_stamp_quad_cells_simd(
    uint32_t *cells,
    int grid_w,
    int ix,
    int iy,
    int iw,
    int ih,
    uint32_t rgba);

void nano_ui_term_blend_dim_simd(
    uint32_t *cells,
    int grid_w,
    int ix,
    int iy,
    int iw,
    int ih,
    uint32_t dim_rgba);

#endif // NANO_UI_TERM_SIMD_H
