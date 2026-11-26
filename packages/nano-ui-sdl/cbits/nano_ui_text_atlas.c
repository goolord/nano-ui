#include "nano_ui_text_atlas.h"

#include <SDL3/SDL.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

/* Glyphs fill page 0, then each later page in turn, placed by
 * nano_ui_ttf_shared.h as nano-ui-sdl-web's atlas places them. Each opened
 * page keeps a CPU RGBA32 backing buffer and a dirty bounding rectangle so
 * bursts of glyph insertions copy into RAM and flush to the GPU texture in a
 * single SDL_UpdateTexture when nano_ui_text_atlas_texture is queried. A
 * reset clears only the previously used rows of page 0 and leaves the rest
 * untouched. */
typedef struct {
    SDL_Texture *tex;
    uint8_t *pixels;
    int used_h;
    int dirty_x0;
    int dirty_y0;
    int dirty_x1;
    int dirty_y1;
    bool dirty;
} NanoUiAtlasPage;

struct NanoUiTextAtlas {
    SDL_Renderer *renderer;
    NanoUiAtlasPage pages[NANO_UI_TEXT_ATLAS_PAGES];
    NanoUiAtlasCursor cur;
};

static void mark_dirty(NanoUiAtlasPage *p, int x0, int y0, int x1, int y1)
{
    if (x0 >= x1 || y0 >= y1) {
        return;
    }
    if (!p->dirty) {
        p->dirty_x0 = x0;
        p->dirty_y0 = y0;
        p->dirty_x1 = x1;
        p->dirty_y1 = y1;
        p->dirty = true;
    } else {
        if (x0 < p->dirty_x0) p->dirty_x0 = x0;
        if (y0 < p->dirty_y0) p->dirty_y0 = y0;
        if (x1 > p->dirty_x1) p->dirty_x1 = x1;
        if (y1 > p->dirty_y1) p->dirty_y1 = y1;
    }
}

static void write_white_patch(uint8_t *pixels)
{
    const int pitch = NANO_UI_GLYPH_ATLAS_SIZE * 4;
    for (int y = 0; y < NANO_UI_WHITE_PATCH_SIZE; y++) {
        memset(pixels + (size_t)y * (size_t)pitch, 0xff, (size_t)NANO_UI_WHITE_PATCH_SIZE * 4);
    }
}

static SDL_Texture *create_texture(NanoUiTextAtlas *atlas)
{
    SDL_Texture *tex = SDL_CreateTexture(
        atlas->renderer,
        SDL_PIXELFORMAT_RGBA32,
        SDL_TEXTUREACCESS_STREAMING,
        NANO_UI_GLYPH_ATLAS_SIZE,
        NANO_UI_GLYPH_ATLAS_SIZE);
    if (!tex) {
        return NULL;
    }
    SDL_SetTextureBlendMode(tex, SDL_BLENDMODE_BLEND);
    /* Bilinear filtering keeps glyph quads smooth when a quad lands off a
     * whole texel boundary (fractional display scale, sub-pixel pen nudge,
     * shaped-run placement). NEAREST snaps to the closest texel and makes
     * scaled/slightly-misaligned text look blocky and pixelated. */
    SDL_SetTextureScaleMode(tex, SDL_SCALEMODE_LINEAR);
    return tex;
}

static bool flush_page(NanoUiAtlasPage *p)
{
    if (!p->dirty || !p->tex || !p->pixels) {
        return true;
    }
    const int size = NANO_UI_GLYPH_ATLAS_SIZE;
    const int pitch = size * 4;
    int x0 = p->dirty_x0 < 0 ? 0 : p->dirty_x0;
    int y0 = p->dirty_y0 < 0 ? 0 : p->dirty_y0;
    int x1 = p->dirty_x1 > size ? size : p->dirty_x1;
    int y1 = p->dirty_y1 > size ? size : p->dirty_y1;
    if (x0 < x1 && y0 < y1) {
        SDL_Rect rect = {x0, y0, x1 - x0, y1 - y0};
        const uint8_t *src = p->pixels + (size_t)y0 * (size_t)pitch + (size_t)x0 * 4;
        if (!SDL_UpdateTexture(p->tex, &rect, src, pitch)) {
            return false;
        }
    }
    p->dirty = false;
    return true;
}

/* Start glyphs on page `page`: allocate or clear only the previously used
 * rows of its CPU buffer, ensure its GPU texture exists, and put the cursor
 * after its white patch. The atlas is a void pointer for
 * nano_ui_atlas_slot_for. */
static bool open_page(void *self, int page)
{
    NanoUiTextAtlas *atlas = (NanoUiTextAtlas *)self;
    NanoUiAtlasPage *p = &atlas->pages[page];
    const int size = NANO_UI_GLYPH_ATLAS_SIZE;
    const int pitch = size * 4;
    bool fresh_tex = false;
    if (!p->tex) {
        p->tex = create_texture(atlas);
        if (!p->tex) {
            return false;
        }
        fresh_tex = true;
    }
    int clear_h = p->used_h;
    if (!p->pixels) {
        p->pixels = (uint8_t *)calloc((size_t)size * (size_t)size, 4);
        if (!p->pixels) {
            return false;
        }
        clear_h = size;
    } else if (clear_h > 0) {
        if (clear_h > size) clear_h = size;
        memset(p->pixels, 0, (size_t)clear_h * (size_t)pitch);
    }
    write_white_patch(p->pixels);
    const int min_h = NANO_UI_WHITE_PATCH_SIZE + NANO_UI_GLYPH_ATLAS_PAD;
    p->used_h = min_h;
    if (fresh_tex) {
        /* A newly created GPU texture has undefined contents everywhere,
         * including the bottom-row padding texel sampled for missing glyphs. */
        mark_dirty(p, 0, 0, size, size);
    } else {
        int dirty_h = clear_h > min_h ? clear_h : min_h;
        mark_dirty(p, 0, 0, size, dirty_h);
    }
    nano_ui_atlas_cursor_open(&atlas->cur, page);
    return true;
}

NanoUiTextAtlas *nano_ui_text_atlas_create(SDL_Renderer *renderer)
{
    if (!renderer) {
        return NULL;
    }
    NanoUiTextAtlas *atlas = (NanoUiTextAtlas *)calloc(1, sizeof(NanoUiTextAtlas));
    if (!atlas) {
        return NULL;
    }
    atlas->renderer = renderer;
    return atlas;
}

void nano_ui_text_atlas_destroy(NanoUiTextAtlas *atlas)
{
    if (!atlas) {
        return;
    }
    nano_ui_text_atlas_drop_textures(atlas);
    for (int i = 0; i < NANO_UI_TEXT_ATLAS_PAGES; i++) {
        free(atlas->pages[i].pixels);
        atlas->pages[i].pixels = NULL;
    }
    free(atlas);
}

SDL_Texture *nano_ui_text_atlas_texture(NanoUiTextAtlas *atlas, int page)
{
    if (!atlas || page < 0 || page >= NANO_UI_TEXT_ATLAS_PAGES) {
        return NULL;
    }
    NanoUiAtlasPage *p = &atlas->pages[page];
    if (p->tex && p->dirty) {
        flush_page(p);
    }
    return p->tex;
}

bool nano_ui_text_atlas_insert_surface(
    NanoUiTextAtlas *atlas,
    SDL_Surface *surface,
    float uv[4])
{
    if (!atlas || !surface || !uv) {
        return false;
    }
    int gw = surface->w;
    int gh = surface->h;
    if (gw <= 0 || gh <= 0) {
        return false;
    }
    int page = 0;
    int x = 0;
    int y = 0;
    if (!nano_ui_atlas_slot_for(&atlas->cur, atlas, open_page, gw, gh, &page, &x, &y)) {
        return false;
    }
    NanoUiAtlasPage *p = &atlas->pages[page];
    const int dst_pitch = NANO_UI_GLYPH_ATLAS_SIZE * 4;
    const size_t row_bytes = (size_t)gw * 4;
    const uint8_t *src = (const uint8_t *)surface->pixels;
    uint8_t *dst = p->pixels + (size_t)y * (size_t)dst_pitch + (size_t)x * 4;
    for (int row = 0; row < gh; row++) {
        memcpy(dst + (size_t)row * (size_t)dst_pitch, src + (size_t)row * (size_t)surface->pitch, row_bytes);
    }
    int bottom = y + gh + NANO_UI_GLYPH_ATLAS_PAD;
    if (bottom > p->used_h) {
        p->used_h = bottom;
    }
    mark_dirty(p, x, y, x + gw, y + gh);
    nano_ui_atlas_commit(&atlas->cur, page, x, y, gw, gh, uv);
    return true;
}

void nano_ui_text_atlas_drop_textures(NanoUiTextAtlas *atlas)
{
    if (!atlas) {
        return;
    }
    for (int i = 0; i < NANO_UI_TEXT_ATLAS_PAGES; i++) {
        NanoUiAtlasPage *p = &atlas->pages[i];
        if (p->tex) {
            SDL_DestroyTexture(p->tex);
            p->tex = NULL;
        }
        p->dirty = false;
    }
    atlas->cur.pages = 0;
}

void nano_ui_text_atlas_reset(NanoUiTextAtlas *atlas)
{
    if (!atlas) {
        return;
    }
    /* Page 0 keeps its texture and clears its previously used rows now; the
     * others clear lazily when glyphs reach them again. */
    if (atlas->pages[0].tex) {
        open_page(atlas, 0);
    } else {
        atlas->cur.pages = 0;
    }
}
