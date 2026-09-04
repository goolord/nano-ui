#include "nano_ui_term_simd.h"
#include <string.h>

#if defined(__AVX2__)
#include <immintrin.h>
#define HAS_AVX2 1
#elif defined(__SSE2__) || defined(_M_X64)
#include <emmintrin.h>
#define HAS_SSE 1
#endif

void nano_ui_term_fill_blanks_simd(uint32_t *cells, int len)
{
    if (!cells || len <= 0) {
        return;
    }

#if defined(HAS_AVX2)
    // Repeating 8-word chunks representing cells (32, 0, 0):
    // 24 words = exactly 8 cells (each cell is 3 uint32_t)
    __m256i v0 = _mm256_setr_epi32(32, 0, 0, 32, 0, 0, 32, 0);
    __m256i v1 = _mm256_setr_epi32(0, 32, 0, 0, 32, 0, 0, 32);
    __m256i v2 = _mm256_setr_epi32(0, 0, 32, 0, 0, 32, 0, 0);

    int i = 0;
    for (; i + 24 <= len; i += 24) {
        _mm256_storeu_si256((__m256i *)(cells + i + 0), v0);
        _mm256_storeu_si256((__m256i *)(cells + i + 8), v1);
        _mm256_storeu_si256((__m256i *)(cells + i + 16), v2);
    }
    for (; i < len; i += 3) {
        cells[i] = 32;
        if (i + 1 < len) cells[i + 1] = 0;
        if (i + 2 < len) cells[i + 2] = 0;
    }
#else
    memset(cells, 0, (size_t)len * sizeof(uint32_t));
    for (int i = 0; i < len; i += 3) {
        cells[i] = 32;
    }
#endif
}

void nano_ui_term_stamp_quad_cells_simd(
    uint32_t *cells,
    int grid_w,
    int ix,
    int iy,
    int iw,
    int ih,
    uint32_t rgba)
{
    if (!cells || grid_w <= 0 || iw <= 0 || ih <= 0) {
        return;
    }

#if defined(HAS_AVX2)
    // 8 cells = 24 words with repeating pattern (32, rgba, rgba)
    __m256i v0 = _mm256_setr_epi32(32, (int)rgba, (int)rgba, 32, (int)rgba, (int)rgba, 32, (int)rgba);
    __m256i v1 = _mm256_setr_epi32((int)rgba, 32, (int)rgba, (int)rgba, 32, (int)rgba, (int)rgba, 32);
    __m256i v2 = _mm256_setr_epi32((int)rgba, (int)rgba, 32, (int)rgba, (int)rgba, 32, (int)rgba, (int)rgba);

    for (int dy = 0; dy < ih; dy++) {
        uint32_t *row = cells + ((iy + dy) * grid_w + ix) * 3;
        int dx = 0;
        for (; dx + 8 <= iw; dx += 8) {
            _mm256_storeu_si256((__m256i *)(row + dx * 3 + 0), v0);
            _mm256_storeu_si256((__m256i *)(row + dx * 3 + 8), v1);
            _mm256_storeu_si256((__m256i *)(row + dx * 3 + 16), v2);
        }
        for (; dx < iw; dx++) {
            uint32_t *cell = row + dx * 3;
            cell[0] = 32;
            cell[1] = rgba;
            cell[2] = rgba;
        }
    }
#else
    for (int dy = 0; dy < ih; dy++) {
        uint32_t *row = cells + ((iy + dy) * grid_w + ix) * 3;
        for (int dx = 0; dx < iw; dx++) {
            uint32_t *cell = row + dx * 3;
            cell[0] = 32;
            cell[1] = rgba;
            cell[2] = rgba;
        }
    }
#endif
}

static inline uint32_t lerp_color_u32(uint32_t w, uint32_t dim_rgba, float t)
{
    if ((w & 0xFF) < 32) {
        return dim_rgba;
    }
    float inv_t = 1.0f - t;
    float r = (float)((w >> 24) & 0xFF) * inv_t + (float)((dim_rgba >> 24) & 0xFF) * t;
    float g = (float)((w >> 16) & 0xFF) * inv_t + (float)((dim_rgba >> 16) & 0xFF) * t;
    float b = (float)((w >> 8) & 0xFF) * inv_t + (float)((dim_rgba >> 8) & 0xFF) * t;
    float a = (float)(w & 0xFF) * inv_t + (float)(dim_rgba & 0xFF) * t;

    uint32_t ur = (uint32_t)(r + 0.5f);
    uint32_t ug = (uint32_t)(g + 0.5f);
    uint32_t ub = (uint32_t)(b + 0.5f);
    uint32_t ua = (uint32_t)(a + 0.5f);

    if (ur > 255) ur = 255;
    if (ug > 255) ug = 255;
    if (ub > 255) ub = 255;
    if (ua > 255) ua = 255;

    return (ur << 24) | (ug << 16) | (ub << 8) | ua;
}

void nano_ui_term_blend_dim_simd(
    uint32_t *cells,
    int grid_w,
    int ix,
    int iy,
    int iw,
    int ih,
    uint32_t rgba)
{
    if (!cells || grid_w <= 0 || iw <= 0 || ih <= 0) {
        return;
    }
    uint32_t dim = rgba | 0xFF;
    float t = (float)(rgba & 0xFF) / 255.0f;

    for (int dy = 0; dy < ih; dy++) {
        uint32_t *row = cells + ((iy + dy) * grid_w + ix) * 3;
        for (int dx = 0; dx < iw; dx++) {
            uint32_t *cell = row + dx * 3;
            cell[1] = lerp_color_u32(cell[1], dim, t);
            cell[2] = lerp_color_u32(cell[2], dim, t);
        }
    }
}
