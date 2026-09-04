#ifndef NANO_UI_SIMD_H
#define NANO_UI_SIMD_H

#include <stdint.h>
#include <stdbool.h>

#if defined(__AVX2__)
#include <immintrin.h>
#define NANO_UI_HAS_AVX2 1
#elif defined(__SSE4_1__) || defined(__SSE2__) || defined(_M_X64) || (defined(_M_IX86_FP) && _M_IX86_FP >= 2)
#include <smmintrin.h>
#define NANO_UI_HAS_SSE 1
#elif defined(__ARM_NEON) || defined(__ARM_NEON__)
#include <arm_neon.h>
#define NANO_UI_HAS_NEON 1
#endif

// Returns true if quad AABB [qx0, qy0, qx1, qy1] intersects [dx0, dy0, dx1, dy1]
static inline bool nano_ui_aabb_intersects(
    float qx0, float qy0, float qx1, float qy1,
    float dx0, float dy0, float dx1, float dy1)
{
    return !(qx1 < dx0 || qx0 > dx1 || qy1 < dy0 || qy0 > dy1);
}

#if defined(NANO_UI_HAS_AVX2)

// Tests 8 quad bounding boxes against damage rect [dx0, dy0, dx1, dy1].
// Returns an 8-bit mask where bit i is 1 if quad i intersects the damage rectangle.
static inline uint32_t nano_ui_cull_8_quads_avx2(
    __m256 qx0, __m256 qy0, __m256 qx1, __m256 qy1,
    __m256 dx0, __m256 dy0, __m256 dx1, __m256 dy1)
{
    __m256 outside = _mm256_or_ps(
        _mm256_cmp_ps(qx1, dx0, _CMP_LT_OQ),
        _mm256_or_ps(
            _mm256_cmp_ps(qx0, dx1, _CMP_GT_OQ),
            _mm256_or_ps(
                _mm256_cmp_ps(qy1, dy0, _CMP_LT_OQ),
                _mm256_cmp_ps(qy0, dy1, _CMP_GT_OQ)
            )
        )
    );
    uint32_t out_mask = (uint32_t)_mm256_movemask_ps(outside);
    return (~out_mask) & 0xFF;
}

// SIMD vector scale for float coordinates: out = in * scale
static inline void nano_ui_scale_floats_avx2(float *dst, const float *src, int count, float scale)
{
    __m256 s = _mm256_set1_ps(scale);
    int i = 0;
    for (; i + 8 <= count; i += 8) {
        __m256 v = _mm256_loadu_ps(src + i);
        _mm256_storeu_ps(dst + i, _mm256_mul_ps(v, s));
    }
    for (; i < count; i++) {
        dst[i] = src[i] * scale;
    }
}

#endif

#endif // NANO_UI_SIMD_H
