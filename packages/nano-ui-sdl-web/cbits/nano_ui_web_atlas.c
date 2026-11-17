#include "nano_ui_web_atlas.h"

#include <stdlib.h>
#include <string.h>

enum {
    NANO_UI_WEB_ATLAS_PAD = 1,
    NANO_UI_WHITE_PATCH_SIZE = 4
};

/* Glyphs fill page 0, then each later page in turn. A reset empties every
 * page but keeps their memory, and a page is cleared when it is opened
 * again. Placement is nano_ui_text_atlas.c's, so a glyph lands at the same
 * UVs on both backends. */
struct NanoUiWebAtlas {
    uint8_t *pixels[NANO_UI_WEB_ATLAS_PAGES];
    /* The changed rect of each page as x0, y0, x1, y1; empty when x1 <= x0. */
    int dirty[NANO_UI_WEB_ATLAS_PAGES][4];
    int pages; /* pages holding glyphs; the cursor is on the last */
    int x;
    int y;
    int row_h;
};

static void mark_dirty(NanoUiWebAtlas *atlas, int page, int x, int y, int w, int h)
{
    int *d = atlas->dirty[page];
    if (d[2] <= d[0]) {
        d[0] = x;
        d[1] = y;
        d[2] = x + w;
        d[3] = y + h;
        return;
    }
    d[0] = x < d[0] ? x : d[0];
    d[1] = y < d[1] ? y : d[1];
    d[2] = x + w > d[2] ? x + w : d[2];
    d[3] = y + h > d[3] ? y + h : d[3];
}

/* Every page gets the white patch, so each starts its cursor the same way;
 * shapes sample the one on page 0. */
static void clear_page(NanoUiWebAtlas *atlas, int page)
{
    const int size = NANO_UI_WEB_ATLAS_SIZE;
    uint8_t *px = atlas->pixels[page];
    memset(px, 0, (size_t)size * size * 4);
    for (int y = 0; y < NANO_UI_WHITE_PATCH_SIZE; y++) {
        memset(px + (size_t)y * size * 4, 0xff, NANO_UI_WHITE_PATCH_SIZE * 4);
    }
    mark_dirty(atlas, page, 0, 0, size, size);
}

/* Start glyphs on page `page`: allocate or clear it and put the cursor
 * after its white patch. */
static bool open_page(NanoUiWebAtlas *atlas, int page)
{
    if (!atlas->pixels[page]) {
        const size_t size = NANO_UI_WEB_ATLAS_SIZE;
        atlas->pixels[page] = (uint8_t *)malloc(size * size * 4);
        if (!atlas->pixels[page]) {
            return false;
        }
    }
    clear_page(atlas, page);
    atlas->pages = page + 1;
    atlas->x = NANO_UI_WHITE_PATCH_SIZE + NANO_UI_WEB_ATLAS_PAD;
    atlas->y = NANO_UI_WEB_ATLAS_PAD;
    atlas->row_h = NANO_UI_WHITE_PATCH_SIZE;
    return true;
}

/* A place for a gw by gh glyph on the current page, moving to a new row or
 * a new page when it does not fit. */
static bool slot_for(NanoUiWebAtlas *atlas, int gw, int gh, int *out_page, int *out_x, int *out_y)
{
    int pad = NANO_UI_WEB_ATLAS_PAD;
    int size = NANO_UI_WEB_ATLAS_SIZE;
    if (gw + 2 * pad > size || gh + 2 * pad > size) {
        return false;
    }
    if (atlas->pages == 0 && !open_page(atlas, 0)) {
        return false;
    }
    for (;;) {
        if (atlas->x + gw + pad <= size && atlas->y + gh + pad <= size) {
            break;
        }
        int next_y = atlas->y + (atlas->row_h > 0 ? atlas->row_h + pad : pad);
        if (next_y + gh + pad <= size) {
            atlas->y = next_y;
            atlas->x = pad;
            atlas->row_h = 0;
            break;
        }
        if (atlas->pages >= NANO_UI_WEB_ATLAS_PAGES || !open_page(atlas, atlas->pages)) {
            return false;
        }
    }
    *out_page = atlas->pages - 1;
    *out_x = atlas->x;
    *out_y = atlas->y;
    return true;
}

NanoUiWebAtlas *nano_ui_web_atlas_create(void)
{
    return (NanoUiWebAtlas *)calloc(1, sizeof(NanoUiWebAtlas));
}

void nano_ui_web_atlas_destroy(NanoUiWebAtlas *atlas)
{
    if (!atlas) {
        return;
    }
    for (int i = 0; i < NANO_UI_WEB_ATLAS_PAGES; i++) {
        free(atlas->pixels[i]);
    }
    free(atlas);
}

uint8_t *nano_ui_web_atlas_pixels(NanoUiWebAtlas *atlas, int page)
{
    if (!atlas || page < 0 || page >= NANO_UI_WEB_ATLAS_PAGES) {
        return NULL;
    }
    return atlas->pixels[page];
}

bool nano_ui_web_atlas_take_dirty(NanoUiWebAtlas *atlas, int page, int out[4])
{
    if (!atlas || page < 0 || page >= NANO_UI_WEB_ATLAS_PAGES || !atlas->pixels[page]) {
        return false;
    }
    int *d = atlas->dirty[page];
    if (d[2] <= d[0] || d[3] <= d[1]) {
        return false;
    }
    out[0] = d[0];
    out[1] = d[1];
    out[2] = d[2] - d[0];
    out[3] = d[3] - d[1];
    d[0] = d[1] = d[2] = d[3] = 0;
    return true;
}

void nano_ui_web_atlas_mark_all_dirty(NanoUiWebAtlas *atlas)
{
    if (!atlas) {
        return;
    }
    for (int i = 0; i < NANO_UI_WEB_ATLAS_PAGES; i++) {
        if (atlas->pixels[i]) {
            mark_dirty(atlas, i, 0, 0, NANO_UI_WEB_ATLAS_SIZE, NANO_UI_WEB_ATLAS_SIZE);
        }
    }
}

bool nano_ui_web_atlas_insert_surface(
    NanoUiWebAtlas *atlas,
    SDL_Surface *surface,
    float uv[4])
{
    if (!atlas || !surface || !uv || surface->format != SDL_PIXELFORMAT_RGBA32) {
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
    if (!slot_for(atlas, gw, gh, &page, &x, &y)) {
        return false;
    }
    const size_t stride = (size_t)NANO_UI_WEB_ATLAS_SIZE * 4;
    uint8_t *dst = atlas->pixels[page] + (size_t)y * stride + (size_t)x * 4;
    const uint8_t *src = (const uint8_t *)surface->pixels;
    for (int row = 0; row < gh; row++) {
        memcpy(dst + (size_t)row * stride, src + (size_t)row * surface->pitch, (size_t)gw * 4);
    }
    mark_dirty(atlas, page, x, y, gw, gh);
    atlas->x = x + gw + NANO_UI_WEB_ATLAS_PAD;
    if (gh > atlas->row_h) {
        atlas->row_h = gh;
    }
    float s = (float)NANO_UI_WEB_ATLAS_SIZE;
    uv[0] = (float)page + (float)x / s;
    uv[1] = (float)y / s;
    uv[2] = uv[0] + (float)gw / s;
    uv[3] = uv[1] + (float)gh / s;
    return true;
}

void nano_ui_web_atlas_reset(NanoUiWebAtlas *atlas)
{
    if (!atlas) {
        return;
    }
    /* Page 0 is cleared now; the others are cleared when glyphs reach them
     * again. */
    if (atlas->pixels[0]) {
        open_page(atlas, 0);
    } else {
        atlas->pages = 0;
    }
}
