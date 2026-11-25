#include <SDL3/SDL.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "nano_ui_ttf_shared.h"

/* The batch reads the core's draw buffer in place as SDL_Vertex: x and y,
 * then red, green, blue and alpha, then u and v, as 32-bit floats. */
_Static_assert(sizeof(SDL_Vertex) == NANO_UI_VERTEX_BYTES, "SDL_Vertex must be the core's 32-byte vertex");
_Static_assert(offsetof(SDL_Vertex, position) == 0, "SDL_Vertex starts with its position");
_Static_assert(offsetof(SDL_Vertex, color) == 8, "SDL_Vertex colour follows the 2-float position");
_Static_assert(offsetof(SDL_Vertex, tex_coord) == 24, "SDL_Vertex UV follows the 4-float colour");
_Static_assert(sizeof(SDL_FColor) == 16, "SDL_Vertex colour is four floats");

#if SDL_VERSION_ATLEAST(3, 4, 0)
typedef struct {
    uint64_t hash;
    SDL_GPUShader *shader;
    SDL_GPURenderState *state;
    int failed;
} NanoUiGpuShaderEntry;

typedef struct {
    SDL_Texture *tex;
    int cap_w;
    int cap_h;
    int in_use;
} NanoUiTargetSlot;
#endif

typedef struct {
    SDL_Renderer *renderer;
    const uint8_t *verts;
    int vert_count;
    const uint8_t *indices;
    SDL_Texture *pending_texture;
    int pending_start;
    int pending_n;
    /* The pending indices less the first vertex they use (see narrow). */
    int *rebased;
    int rebased_cap;
    /* Device pixels per layout unit for the pass (nano_ui_batch_begin). The
     * renderer draws at scale 1, so clip rects are whole device pixels, and
     * positions are multiplied here instead of by SDL's render scale. */
    float scale;
    /* Logical origin subtracted from vertex positions when drawing into a
     * Tier 2 offscreen effect target (0, 0 on the main target). */
    float origin_x;
    float origin_y;
    /* Scaled x, y pairs of the pass's vertices, by vertex index. Only the
     * vertices [xy_lo, xy_hi) of xy_verts are filled: a flush extends the
     * range to the vertices its indices use. */
    float *xy;
    int xy_cap;
    int xy_lo;
    int xy_hi;
    const uint8_t *xy_verts;
#if SDL_VERSION_ATLEAST(3, 4, 0)
    SDL_Texture *white_tex;
    NanoUiGpuShaderEntry *shaders;
    int shader_count;
    int shader_cap;
    NanoUiTargetSlot *targets;
    int target_count;
    int target_cap;
#endif
} NanoUiBatch;

/* Create a window paired with SDL 3.4's "gpu" renderer supporting SPIR-V and
 * MSL custom fragment shaders. Returns 1 on success; on failure frees any
 * partial resources and returns 0 so RenderDriverAuto can fall back cleanly. */
int nano_ui_create_window_and_gpu_renderer(
    const char *title, int w, int h, uint64_t flags,
    SDL_Window **out_win, SDL_Renderer **out_ren)
{
    if (!out_win || !out_ren) {
        return 0;
    }
    *out_win = NULL;
    *out_ren = NULL;
#if SDL_VERSION_ATLEAST(3, 4, 0)
    SDL_Window *win = SDL_CreateWindow(title, w, h, (SDL_WindowFlags)flags);
    if (!win) {
        return 0;
    }
    SDL_PropertiesID props = SDL_CreateProperties();
    if (!props) {
        SDL_DestroyWindow(win);
        return 0;
    }
    SDL_SetPointerProperty(props, SDL_PROP_RENDERER_CREATE_WINDOW_POINTER, win);
    SDL_SetStringProperty(props, SDL_PROP_RENDERER_CREATE_NAME_STRING, "gpu");
    SDL_SetBooleanProperty(props, SDL_PROP_RENDERER_CREATE_GPU_SHADERS_SPIRV_BOOLEAN, true);
    SDL_SetBooleanProperty(props, SDL_PROP_RENDERER_CREATE_GPU_SHADERS_MSL_BOOLEAN, true);
    SDL_Renderer *ren = SDL_CreateRendererWithProperties(props);
    SDL_DestroyProperties(props);
    if (!ren) {
        SDL_DestroyWindow(win);
        return 0;
    }
    *out_win = win;
    *out_ren = ren;
    return 1;
#else
    (void)title; (void)w; (void)h; (void)flags;
    return 0;
#endif
}

/* Report whether renderer is backed by an SDL 3.4 GPU device supporting
 * SDL_GPURenderState custom fragment shaders. */
int nano_ui_renderer_has_gpu(SDL_Renderer *renderer)
{
#if SDL_VERSION_ATLEAST(3, 4, 0)
    return (renderer && SDL_GetGPURendererDevice(renderer) != NULL) ? 1 : 0;
#else
    (void)renderer;
    return 0;
#endif
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
    batch->scale = 1.f;
#if SDL_VERSION_ATLEAST(3, 4, 0)
    /* Every UV the batches submit lies in [0, 1]. Left on AUTO, SDL scans all
     * of a call's vertices on every textured call to choose between clamp
     * and wrap, which comes to clamp anyway. SDL 3.2 has no setting for it,
     * so there a call passes only the vertices it uses (see narrow). */
    SDL_SetRenderTextureAddressMode(renderer, SDL_TEXTURE_ADDRESS_CLAMP, SDL_TEXTURE_ADDRESS_CLAMP);
#endif
    return batch;
}

#if !SDL_VERSION_ATLEAST(3, 4, 0)
/* Narrow a batch's vertices to the range its indices use, rebasing the
 * indices to its start. SDL 3.2 scans every vertex a textured call is given,
 * and the batches all share the frame's buffer, so passing all of it made a
 * frame cost draw calls times vertices. Indices out of the buffer are left
 * for SDL to reject, and a batch whose indices cannot be copied passes the
 * vertices up to the last it uses. */
static void narrow(NanoUiBatch *batch, const SDL_Vertex **v, const int **idx, int *nv)
{
    const int *ix = *idx;
    const int n = batch->pending_n;
    int lo = ix[0], hi = ix[0];
    for (int k = 1; k < n; k++) {
        lo = ix[k] < lo ? ix[k] : lo;
        hi = ix[k] > hi ? ix[k] : hi;
    }
    if (lo < 0 || hi >= *nv) {
        return;
    }
    *nv = hi + 1;
    if (lo == 0) {
        return;
    }
    if (batch->rebased_cap < n) {
        int *grown = (int *)realloc(batch->rebased, sizeof(int) * (size_t)n * 2);
        if (!grown) {
            return;
        }
        batch->rebased = grown;
        batch->rebased_cap = n * 2;
    }
    for (int k = 0; k < n; k++) {
        batch->rebased[k] = ix[k] - lo;
    }
    *idx = batch->rebased;
    *v += lo;
    *nv = hi - lo + 1;
}
#endif

/* Start a pass at a scale: forget the positions scaled for an earlier pass,
 * whose buffer may hold other vertices now. */
void nano_ui_batch_begin(NanoUiBatch *batch, float scale)
{
    if (batch) {
        batch->scale = scale > 0.f ? scale : 1.f;
        batch->origin_x = 0.f;
        batch->origin_y = 0.f;
        batch->xy_verts = NULL;
        batch->xy_lo = batch->xy_hi = 0;
    }
}

void nano_ui_batch_flush(NanoUiBatch *batch);

/* Set the logical origin subtracted from vertex coordinates when redirecting
 * drawing into an offscreen Tier 2 effect target. */
void nano_ui_batch_set_origin(NanoUiBatch *batch, float ox, float oy)
{
    if (batch) {
        nano_ui_batch_flush(batch);
        batch->origin_x = ox;
        batch->origin_y = oy;
        batch->xy_verts = NULL;
        batch->xy_lo = batch->xy_hi = 0;
    }
}

static void scale_range(NanoUiBatch *batch, const SDL_Vertex *v, int from, int to)
{
    const float s = batch->scale;
    const float ox = batch->origin_x;
    const float oy = batch->origin_y;
    for (int i = from; i < to; i++) {
        batch->xy[2 * i] = (v[i].position.x - ox) * s;
        batch->xy[2 * i + 1] = (v[i].position.y - oy) * s;
    }
}

/* Scaled positions covering the vertices the n indices idx use, which count
 * from vertex off of the batch's buffer, as a pointer for vertex off; NULL
 * when there is no memory for them. Indices outside the buffer are left for
 * SDL to reject. */
static const float *scaled_positions(NanoUiBatch *batch, int off, const int *idx, int n)
{
    const int total = batch->vert_count;
    if (batch->xy_cap < total) {
        float *grown = (float *)realloc(batch->xy, sizeof(float) * 2 * (size_t)total);
        if (!grown) {
            return NULL;
        }
        batch->xy = grown;
        batch->xy_cap = total;
    }
    int lo = idx[0], hi = idx[0];
    for (int k = 1; k < n; k++) {
        lo = idx[k] < lo ? idx[k] : lo;
        hi = idx[k] > hi ? idx[k] : hi;
    }
    lo += off;
    hi += off + 1;
    lo = lo < 0 ? 0 : lo;
    hi = hi > total ? total : hi;
    const SDL_Vertex *all = (const SDL_Vertex *)batch->verts;
    if (batch->xy_verts != batch->verts || batch->xy_lo >= batch->xy_hi) {
        scale_range(batch, all, lo, hi);
        batch->xy_verts = batch->verts;
        batch->xy_lo = lo;
        batch->xy_hi = hi;
    } else {
        if (lo < batch->xy_lo) {
            scale_range(batch, all, lo, batch->xy_lo);
            batch->xy_lo = lo;
        }
        if (hi > batch->xy_hi) {
            scale_range(batch, all, batch->xy_hi, hi);
            batch->xy_hi = hi;
        }
    }
    return batch->xy + 2 * (size_t)off;
}

/* Drop the pending batch without drawing it: a pass that failed part way. */
void nano_ui_batch_discard(NanoUiBatch *batch)
{
    if (batch) {
        batch->pending_n = 0;
        batch->pending_start = 0;
        batch->pending_texture = NULL;
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
    /* The interleaved SDL_Vertex buffer read in place through strides; an
     * untextured batch passes no UVs. */
    const SDL_Vertex *v = (const SDL_Vertex *)batch->verts;
    const int *idx = (const int *)batch->indices + batch->pending_start;
    int nv = batch->vert_count;
#if !SDL_VERSION_ATLEAST(3, 4, 0)
    narrow(batch, &v, &idx, &nv);
#endif
    const int stride = (int)sizeof(SDL_Vertex);
    const float *xy = &v->position.x;
    int xy_stride = stride;
    if (batch->scale != 1.f || batch->origin_x != 0.f || batch->origin_y != 0.f) {
        xy = scaled_positions(batch, (int)(v - (const SDL_Vertex *)batch->verts), idx, batch->pending_n);
        xy_stride = (int)(2 * sizeof(float));
        if (!xy) {
            nano_ui_batch_discard(batch);
            return;
        }
    }
    SDL_RenderGeometryRaw(batch->renderer, batch->pending_texture,
                          xy, xy_stride, &v->color, stride,
                          batch->pending_texture ? &v->tex_coord.x : NULL, stride,
                          nv, idx, batch->pending_n, (int)sizeof(int));
    batch->pending_n = 0;
    batch->pending_start = 0;
    batch->pending_texture = NULL;
}

void nano_ui_batch_destroy(NanoUiBatch *batch)
{
    if (batch) {
        nano_ui_batch_flush(batch);
#if SDL_VERSION_ATLEAST(3, 4, 0)
        if (batch->shaders) {
            SDL_GPUDevice *dev = batch->renderer ? SDL_GetGPURendererDevice(batch->renderer) : NULL;
            for (int i = 0; i < batch->shader_count; i++) {
                if (batch->shaders[i].state) {
                    SDL_DestroyGPURenderState(batch->shaders[i].state);
                }
                if (batch->shaders[i].shader && dev) {
                    SDL_ReleaseGPUShader(dev, batch->shaders[i].shader);
                }
            }
            free(batch->shaders);
        }
        if (batch->targets) {
            for (int i = 0; i < batch->target_count; i++) {
                if (batch->targets[i].tex) {
                    SDL_DestroyTexture(batch->targets[i].tex);
                }
            }
            free(batch->targets);
        }
        if (batch->white_tex) {
            SDL_DestroyTexture(batch->white_tex);
        }
#endif
        free(batch->rebased);
        free(batch->xy);
        free(batch);
    }
}

/* Commands may contain independent triangles, not just axis-aligned quads.
 * Reject a range only when every triangle is outside the damage bounds. SDL
 * clips intersecting triangles; this just avoids submitting invisible ranges. */
static bool visible(const SDL_Vertex *v, int vertex_count, const int *indices, int count,
                    float x, float y, float w, float h)
{
    for (int i = 0; i + 2 < count; i += 3) {
        if ((unsigned)indices[i] >= (unsigned)vertex_count ||
            (unsigned)indices[i + 1] >= (unsigned)vertex_count ||
            (unsigned)indices[i + 2] >= (unsigned)vertex_count) return true;
        const SDL_FPoint a = v[indices[i]].position;
        const SDL_FPoint b = v[indices[i + 1]].position;
        const SDL_FPoint c = v[indices[i + 2]].position;
        if (!((a.x < x && b.x < x && c.x < x) ||
              (a.x >= x + w && b.x >= x + w && c.x >= x + w) ||
              (a.y < y && b.y < y && c.y < y) ||
              (a.y >= y + h && b.y >= y + h && c.y >= y + h))) {
            return true;
        }
    }
    return false;
}

void nano_ui_batch_draw_range(
    NanoUiBatch *batch, const uint8_t *verts, int vert_count,
    const uint8_t *indices, int index_start, int index_n,
    SDL_Texture *texture, int has_damage,
    float dmg_x, float dmg_y, float dmg_w, float dmg_h)
{
    if (!batch || !verts || !indices || vert_count <= 0 || index_n < 3) {
        return;
    }
    if (index_start < 0) {
        index_start = 0;
    }
    if (has_damage && dmg_w > 0.f && dmg_h > 0.f &&
        !visible((const SDL_Vertex *)verts, vert_count, (const int *)indices + index_start,
                 index_n, dmg_x, dmg_y, dmg_w, dmg_h)) {
        return;
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
    batch->indices = indices;
    batch->pending_texture = texture;
    batch->pending_start = index_start;
    batch->pending_n = index_n;
    batch->vert_count = vert_count;
}

#if SDL_VERSION_ATLEAST(3, 4, 0)
static SDL_Texture *ensure_white_tex(NanoUiBatch *batch)
{
    if (batch->white_tex) {
        return batch->white_tex;
    }
    SDL_Texture *tex = SDL_CreateTexture(
        batch->renderer, SDL_PIXELFORMAT_RGBA32, SDL_TEXTUREACCESS_STATIC, 1, 1);
    if (!tex) {
        return NULL;
    }
    const uint32_t white = 0xFFFFFFFFu;
    SDL_UpdateTexture(tex, NULL, &white, 4);
    SDL_SetTextureBlendMode(tex, SDL_BLENDMODE_BLEND);
    batch->white_tex = tex;
    return tex;
}

static NanoUiGpuShaderEntry *get_gpu_shader(
    NanoUiBatch *batch, SDL_GPUDevice *dev, uint64_t hash,
    const uint8_t *spirv, int spirv_len,
    const uint8_t *msl, int msl_len,
    int needs_tex)
{
    for (int i = 0; i < batch->shader_count; i++) {
        if (batch->shaders[i].hash == hash) {
            return &batch->shaders[i];
        }
    }
    if (batch->shader_count == batch->shader_cap) {
        int new_cap = batch->shader_cap > 0 ? batch->shader_cap * 2 : 16;
        NanoUiGpuShaderEntry *grown = (NanoUiGpuShaderEntry *)realloc(
            batch->shaders, (size_t)new_cap * sizeof(NanoUiGpuShaderEntry));
        if (!grown) {
            return NULL;
        }
        batch->shaders = grown;
        batch->shader_cap = new_cap;
    }
    NanoUiGpuShaderEntry *entry = &batch->shaders[batch->shader_count++];
    memset(entry, 0, sizeof(*entry));
    entry->hash = hash;

    SDL_GPUShaderFormat fmts = SDL_GetGPUShaderFormats(dev);
    SDL_GPUShaderCreateInfo sci;
    memset(&sci, 0, sizeof(sci));
    sci.stage = SDL_GPU_SHADERSTAGE_FRAGMENT;
    sci.num_samplers = needs_tex ? 1u : 0u;
    sci.num_uniform_buffers = 1u;

    if ((fmts & SDL_GPU_SHADERFORMAT_SPIRV) && spirv && spirv_len > 0) {
        sci.format = SDL_GPU_SHADERFORMAT_SPIRV;
        sci.code = spirv;
        sci.code_size = (size_t)spirv_len;
        sci.entrypoint = "main";
    } else if ((fmts & SDL_GPU_SHADERFORMAT_MSL) && msl && msl_len > 0) {
        sci.format = SDL_GPU_SHADERFORMAT_MSL;
        sci.code = msl;
        sci.code_size = (size_t)msl_len;
        sci.entrypoint = "main0";
    } else {
        entry->failed = 1;
        return entry;
    }

    SDL_GPUShader *shader = SDL_CreateGPUShader(dev, &sci);
    if (!shader) {
        fprintf(stderr, "nano-ui-sdl: SDL_CreateGPUShader failed: %s\n", SDL_GetError());
        entry->failed = 1;
        return entry;
    }

    SDL_GPURenderStateCreateInfo rsci;
    memset(&rsci, 0, sizeof(rsci));
    rsci.fragment_shader = shader;
    SDL_GPURenderState *state = SDL_CreateGPURenderState(batch->renderer, &rsci);
    if (!state) {
        fprintf(stderr, "nano-ui-sdl: SDL_CreateGPURenderState failed: %s\n", SDL_GetError());
        SDL_ReleaseGPUShader(dev, shader);
        entry->failed = 1;
        return entry;
    }
    entry->shader = shader;
    entry->state = state;
    return entry;
}
#endif

/* Flush pending geometry and draw one shader-backed index range via SDL 3.4's
 * SDL_GPURenderState, pushing its std140 uniform block at fragment slot 0. */
int nano_ui_batch_draw_shader(
    NanoUiBatch *batch, const uint8_t *verts, int vert_count,
    const uint8_t *indices, int index_start, int index_n,
    SDL_Texture *texture, int has_damage,
    float dmg_x, float dmg_y, float dmg_w, float dmg_h,
    uint64_t hash,
    const uint8_t *spirv, int spirv_len,
    const uint8_t *msl, int msl_len,
    int needs_tex,
    const uint8_t *uniform_bytes, int uniform_len)
{
    if (!batch || !batch->renderer || !verts || !indices || vert_count <= 0 || index_n < 3) {
        return 0;
    }
    if (index_start < 0) {
        index_start = 0;
    }
    if (has_damage && dmg_w > 0.f && dmg_h > 0.f &&
        !visible((const SDL_Vertex *)verts, vert_count, (const int *)indices + index_start,
                 index_n, dmg_x, dmg_y, dmg_w, dmg_h)) {
        return 1;
    }
    nano_ui_batch_flush(batch);
#if SDL_VERSION_ATLEAST(3, 4, 0)
    SDL_GPUDevice *dev = SDL_GetGPURendererDevice(batch->renderer);
    if (!dev) {
        return 0;
    }
    NanoUiGpuShaderEntry *entry = get_gpu_shader(
        batch, dev, hash, spirv, spirv_len, msl, msl_len, needs_tex);
    if (!entry || entry->failed || !entry->state) {
        return 0;
    }
    if (uniform_len > 0 && uniform_bytes != NULL) {
        if (!SDL_SetGPURenderStateFragmentUniforms(entry->state, 0u, uniform_bytes, (Uint32)uniform_len)) {
            fprintf(stderr, "nano-ui-sdl: SDL_SetGPURenderStateFragmentUniforms failed: %s\n", SDL_GetError());
            return 0;
        }
    }
    /* Always pass a valid texture so SDL's GPU renderer selects its textured
     * vertex pipeline (providing both location 0 v_color and location 1 v_uv). */
    SDL_Texture *tex = texture ? texture : ensure_white_tex(batch);
    if (!tex) {
        return 0;
    }
    batch->verts = verts;
    batch->vert_count = vert_count;
    const SDL_Vertex *v = (const SDL_Vertex *)verts;
    const int *idx = (const int *)indices + index_start;
    const int stride = (int)sizeof(SDL_Vertex);
    const float *xy = &v->position.x;
    int xy_stride = stride;
    if (batch->scale != 1.f || batch->origin_x != 0.f || batch->origin_y != 0.f) {
        xy = scaled_positions(batch, 0, idx, index_n);
        xy_stride = (int)(2 * sizeof(float));
        if (!xy) {
            return 0;
        }
    }
    if (!SDL_SetGPURenderState(batch->renderer, entry->state)) {
        fprintf(stderr, "nano-ui-sdl: SDL_SetGPURenderState failed: %s\n", SDL_GetError());
        return 0;
    }
    bool ok = SDL_RenderGeometryRaw(
        batch->renderer, tex,
        xy, xy_stride, &v->color, stride,
        &v->tex_coord.x, stride,
        vert_count, idx, index_n, (int)sizeof(int));
    SDL_SetGPURenderState(batch->renderer, NULL);
    return ok ? 1 : 0;
#else
    (void)texture; (void)hash; (void)spirv; (void)spirv_len;
    (void)msl; (void)msl_len; (void)needs_tex;
    (void)uniform_bytes; (void)uniform_len;
    return 0;
#endif
}

/* Return the renderer's current render target (may be NULL for the window). */
SDL_Texture *nano_ui_batch_current_target(NanoUiBatch *batch)
{
    return (batch && batch->renderer) ? SDL_GetRenderTarget(batch->renderer) : NULL;
}

/* Acquire a pooled 64px-bucketed RGBA32 offscreen render target texture.
 * Returns a non-negative slot index, or -1 on failure. */
int nano_ui_batch_acquire_target(
    NanoUiBatch *batch, int min_w, int min_h, int *out_cap_w, int *out_cap_h)
{
#if SDL_VERSION_ATLEAST(3, 4, 0)
    if (!batch || !batch->renderer) return -1;
    int need_w = min_w > 0 ? ((min_w + 63) & ~63) : 64;
    int need_h = min_h > 0 ? ((min_h + 63) & ~63) : 64;
    int best = -1;
    for (int i = 0; i < batch->target_count; i++) {
        NanoUiTargetSlot *s = &batch->targets[i];
        if (!s->in_use && s->tex &&
            s->cap_w >= need_w && s->cap_h >= need_h &&
            s->cap_w <= need_w * 2 && s->cap_h <= need_h * 2) {
            best = i;
            break;
        }
    }
    if (best < 0) {
        if (batch->target_count == batch->target_cap) {
            int new_cap = batch->target_cap > 0 ? batch->target_cap * 2 : 8;
            NanoUiTargetSlot *grown = (NanoUiTargetSlot *)realloc(
                batch->targets, (size_t)new_cap * sizeof(NanoUiTargetSlot));
            if (!grown) return -1;
            batch->targets = grown;
            batch->target_cap = new_cap;
        }
        SDL_Texture *tex = SDL_CreateTexture(
            batch->renderer, SDL_PIXELFORMAT_RGBA32, SDL_TEXTUREACCESS_TARGET, need_w, need_h);
        if (!tex) {
            fprintf(stderr, "nano-ui-sdl: offscreen target creation failed: %s\n", SDL_GetError());
            return -1;
        }
        SDL_SetTextureBlendMode(tex, SDL_BLENDMODE_BLEND);
        SDL_SetTextureScaleMode(tex, SDL_SCALEMODE_LINEAR);
        best = batch->target_count++;
        batch->targets[best].tex = tex;
        batch->targets[best].cap_w = need_w;
        batch->targets[best].cap_h = need_h;
    }
    batch->targets[best].in_use = 1;
    if (out_cap_w) *out_cap_w = batch->targets[best].cap_w;
    if (out_cap_h) *out_cap_h = batch->targets[best].cap_h;
    return best;
#else
    (void)batch; (void)min_w; (void)min_h; (void)out_cap_w; (void)out_cap_h;
    return -1;
#endif
}

void nano_ui_batch_release_target(NanoUiBatch *batch, int slot)
{
#if SDL_VERSION_ATLEAST(3, 4, 0)
    if (batch && slot >= 0 && slot < batch->target_count) {
        batch->targets[slot].in_use = 0;
    }
#else
    (void)batch; (void)slot;
#endif
}

/* Bind pooled target slot (>= 0), or fallback_target (NULL = default backbuffer)
 * when slot < 0. Clears to transparent black when clear_transparent != 0. */
int nano_ui_batch_bind_target(
    NanoUiBatch *batch, int slot, SDL_Texture *fallback_target, int clear_transparent)
{
    if (!batch || !batch->renderer) return 0;
    nano_ui_batch_flush(batch);
    SDL_Texture *target = fallback_target;
#if SDL_VERSION_ATLEAST(3, 4, 0)
    if (slot >= 0 && slot < batch->target_count) {
        target = batch->targets[slot].tex;
    }
#else
    (void)slot;
#endif
    if (!SDL_SetRenderTarget(batch->renderer, target)) {
        return 0;
    }
    if (clear_transparent) {
        SDL_SetRenderClipRect(batch->renderer, NULL);
        SDL_SetRenderDrawColor(batch->renderer, 0, 0, 0, 0);
        SDL_RenderClear(batch->renderer);
    }
    return 1;
}

/* Composite pooled source texture src_slot over the currently bound render
 * target at physical pixel rect (px, py, pw, ph) with texture UVs (u0,v0)..(u1,v1)
 * through a cached SDL 3.4 GPU fragment shader. */
int nano_ui_batch_composite_effect(
    NanoUiBatch *batch, int src_slot,
    float px, float py, float pw, float ph,
    float u0, float v0, float u1, float v1,
    int blend_copy,
    uint64_t hash,
    const uint8_t *spirv, int spirv_len,
    const uint8_t *msl, int msl_len,
    const uint8_t *uniform_bytes, int uniform_len)
{
    if (!batch || !batch->renderer) return 0;
    nano_ui_batch_flush(batch);
#if SDL_VERSION_ATLEAST(3, 4, 0)
    if (src_slot < 0 || src_slot >= batch->target_count || !batch->targets[src_slot].tex) {
        return 0;
    }
    (void)blend_copy;
    SDL_Texture *src_tex = batch->targets[src_slot].tex;

    SDL_Vertex quad[4];
    quad[0].position.x = px;      quad[0].position.y = py;
    quad[1].position.x = px + pw; quad[1].position.y = py;
    quad[2].position.x = px + pw; quad[2].position.y = py + ph;
    quad[3].position.x = px;      quad[3].position.y = py + ph;
    quad[0].tex_coord.x = u0;     quad[0].tex_coord.y = v0;
    quad[1].tex_coord.x = u1;     quad[1].tex_coord.y = v0;
    quad[2].tex_coord.x = u1;     quad[2].tex_coord.y = v1;
    quad[3].tex_coord.x = u0;     quad[3].tex_coord.y = v1;
    for (int i = 0; i < 4; i++) {
        quad[i].color.r = 1.f;
        quad[i].color.g = 1.f;
        quad[i].color.b = 1.f;
        quad[i].color.a = 1.f;
    }
    static const int indices[6] = { 0, 1, 2, 0, 2, 3 };

    SDL_GPUDevice *dev = SDL_GetGPURendererDevice(batch->renderer);
    NanoUiGpuShaderEntry *entry = dev ? get_gpu_shader(
        batch, dev, hash, spirv, spirv_len, msl, msl_len, 1) : NULL;
    if (!entry || entry->failed || !entry->state) {
        /* Fallback: blit the offscreen premultiplied target unshaded so the
         * wrapped subtree never disappears on shader compile failure. */
        SDL_SetTextureBlendMode(src_tex, SDL_BLENDMODE_BLEND_PREMULTIPLIED);
        bool ok = SDL_RenderGeometry(batch->renderer, src_tex, quad, 4, indices, 6);
        SDL_SetTextureBlendMode(src_tex, SDL_BLENDMODE_BLEND);
        return ok ? 1 : 0;
    }
    if (uniform_len > 0 && uniform_bytes != NULL) {
        if (!SDL_SetGPURenderStateFragmentUniforms(entry->state, 0u, uniform_bytes, (Uint32)uniform_len)) {
            fprintf(stderr, "nano-ui-sdl: effect uniform upload failed: %s\n", SDL_GetError());
            return 0;
        }
    }
    /* Always blend with straight-alpha-to-premultiplied SDL_BLENDMODE_BLEND
     * so both intermediate offscreen targets (cleared to 0,0,0,0) and the
     * final composite stay in premultiplied offscreen space. */
    SDL_SetTextureBlendMode(src_tex, SDL_BLENDMODE_BLEND);
    if (!SDL_SetGPURenderState(batch->renderer, entry->state)) {
        fprintf(stderr, "nano-ui-sdl: SDL_SetGPURenderState failed: %s\n", SDL_GetError());
        return 0;
    }
    bool ok = SDL_RenderGeometry(batch->renderer, src_tex, quad, 4, indices, 6);
    SDL_SetGPURenderState(batch->renderer, NULL);
    return ok ? 1 : 0;
#else
    (void)src_slot; (void)px; (void)py; (void)pw; (void)ph;
    (void)u0; (void)v0; (void)u1; (void)v1; (void)blend_copy;
    (void)hash; (void)spirv; (void)spirv_len; (void)msl; (void)msl_len;
    (void)uniform_bytes; (void)uniform_len;
    return 0;
#endif
}
