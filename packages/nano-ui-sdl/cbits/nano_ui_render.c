#include <SDL3/SDL.h>
#include <stdlib.h>

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
    /* Scaled x, y pairs of the pass's vertices, by vertex index. Only the
     * vertices [xy_lo, xy_hi) of xy_verts are filled: a flush extends the
     * range to the vertices its indices use. */
    float *xy;
    int xy_cap;
    int xy_lo;
    int xy_hi;
    const uint8_t *xy_verts;
} NanoUiBatch;

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
        batch->xy_verts = NULL;
        batch->xy_lo = batch->xy_hi = 0;
    }
}

static void scale_range(NanoUiBatch *batch, const SDL_Vertex *v, int from, int to)
{
    const float s = batch->scale;
    for (int i = from; i < to; i++) {
        batch->xy[2 * i] = v[i].position.x * s;
        batch->xy[2 * i + 1] = v[i].position.y * s;
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
    if (batch->scale != 1.f) {
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
