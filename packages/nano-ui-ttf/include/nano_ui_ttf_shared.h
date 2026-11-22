#ifndef NANO_UI_TTF_SHARED_H
#define NANO_UI_TTF_SHARED_H

/* Constants and glyph-atlas placement shared by nano-ui-ttf's shaping
 * (cbits/nano_ui_ttf.c), nano-ui-sdl's texture atlas and vertex batch
 * (nano_ui_text_atlas.c, nano_ui_render.c) and nano-ui-sdl-web's in-memory
 * atlas (nano_ui_web_atlas.c). The package installs this header, so the
 * other two include it through their dependency on nano-ui-ttf.
 *
 * NanoUI.Ttf.Internal.Header reads these values with hsc2hs. Two have a
 * Haskell twin in the core package, which cannot include C:
 * NANO_UI_VERTEX_BYTES is NanoUI.Internal.Draw.Types.vertexSize and
 * NANO_UI_GLYPH_ATLAS_PAGES is glyphAtlasPages. nano-ui-sdl-web-test checks
 * that each pair is equal.
 *
 * The header needs no SDL include, so hsc2hs compiles it without SDL's
 * flags. */

#include <stdbool.h>

/* Bytes in one vertex of the core's draw buffer: x, y, red, green, blue,
 * alpha, u, v, each a 32-bit float. SDL_Vertex has the same layout, which
 * nano_ui_render.c asserts. */
#define NANO_UI_VERTEX_BYTES 32

/* Width and height of a glyph atlas page in pixels. */
#define NANO_UI_GLYPH_ATLAS_SIZE 2048

/* Pages the atlas opens before it is out of space. */
#define NANO_UI_GLYPH_ATLAS_PAGES 4

/* Transparent pixels around each glyph, so bilinear sampling at a glyph's
 * edge reads nothing from its neighbour. */
#define NANO_UI_GLYPH_ATLAS_PAD 1

/* The opaque white square at the top left of every page, which untextured
 * shapes sample on page 0. The texel just right of it is padding, which
 * NanoUI.Ttf.Internal.Font samples for a glyph that has no slot. */
#define NANO_UI_WHITE_PATCH_SIZE 4

/* ------------------------------------------------------------------------ */
/* Shaped lines (nano_ui_ttf_shape)                                         */
/* ------------------------------------------------------------------------ */

/* The `field` argument of nano_ui_ttf_shaped_int. */
enum {
    NANO_UI_SHAPED_WIDTH = 0,
    NANO_UI_SHAPED_HEIGHT = 1,
    NANO_UI_SHAPED_GLYPH_COUNT = 2,
    NANO_UI_SHAPED_CLUSTER_COUNT = 3
};

/* The `field` argument of nano_ui_ttf_shaped_ptr. */
enum {
    NANO_UI_SHAPED_GLYPHS = 0,
    NANO_UI_SHAPED_GLYPH_FONTS = 1,
    NANO_UI_SHAPED_CLUSTERS = 2
};

/* The ints of one glyph in the NANO_UI_SHAPED_GLYPHS array, in order:
 * NANO_UI_SHAPED_GLYPH_INTS of them a glyph. dst is where the glyph image
 * lands on the line, in pixels from its top left; src is the part of the
 * glyph image to draw. */
enum {
    NANO_UI_GLYPH_TEXT_OFFSET = 0,
    NANO_UI_GLYPH_INDEX = 1,
    NANO_UI_GLYPH_DST_X = 2,
    NANO_UI_GLYPH_DST_Y = 3,
    NANO_UI_GLYPH_DST_W = 4,
    NANO_UI_GLYPH_DST_H = 5,
    NANO_UI_GLYPH_SRC_X = 6,
    NANO_UI_GLYPH_SRC_Y = 7,
    NANO_UI_GLYPH_SRC_W = 8,
    NANO_UI_GLYPH_SRC_H = 9,
    NANO_UI_SHAPED_GLYPH_INTS = 10
};

/* The ints of one cluster in the NANO_UI_SHAPED_CLUSTERS array, in order:
 * NANO_UI_SHAPED_CLUSTER_INTS of them a cluster. Offset and length are in
 * bytes of the shaped text; x and width in pixels. */
enum {
    NANO_UI_CLUSTER_OFFSET = 0,
    NANO_UI_CLUSTER_LENGTH = 1,
    NANO_UI_CLUSTER_X = 2,
    NANO_UI_CLUSTER_WIDTH = 3,
    NANO_UI_CLUSTER_FLAGS = 4,
    NANO_UI_SHAPED_CLUSTER_INTS = 5
};

/* The `direction` argument of nano_ui_ttf_shape, and a cluster's flags
 * masked with NANO_UI_DIRECTION_MASK: SDL_ttf's TTF_Direction values and
 * TTF_SUBSTRING_DIRECTION_MASK, which nano_ui_ttf.c asserts. */
enum {
    NANO_UI_DIRECTION_AUTO = 0,
    NANO_UI_DIRECTION_LTR = 4,
    NANO_UI_DIRECTION_RTL = 5
};
#define NANO_UI_DIRECTION_MASK 0xFF

/* ------------------------------------------------------------------------ */
/* Glyph atlas placement                                                    */
/* ------------------------------------------------------------------------ */

/* Where the next glyph goes. Glyphs fill page 0 in rows, then each later
 * page in turn; a glyph lands at the same UVs whichever backend stores the
 * pages. */
typedef struct NanoUiAtlasCursor {
    int pages; /* pages holding glyphs; the cursor is on the last */
    int x;
    int y;
    int row_h;
} NanoUiAtlasCursor;

/* Put the cursor on page `page`, after its white patch. */
static inline void nano_ui_atlas_cursor_open(NanoUiAtlasCursor *cur, int page)
{
    cur->pages = page + 1;
    cur->x = NANO_UI_WHITE_PATCH_SIZE + NANO_UI_GLYPH_ATLAS_PAD;
    cur->y = NANO_UI_GLYPH_ATLAS_PAD;
    cur->row_h = NANO_UI_WHITE_PATCH_SIZE;
}

/* A place for a gw by gh glyph on the current page, moving to a new row or
 * a new page when it does not fit. `open_page(atlas, page)` makes page
 * `page` ready for glyphs and calls nano_ui_atlas_cursor_open; it answers
 * false when it cannot. False when the glyph is larger than a page, or no
 * page is left with room. */
static inline bool nano_ui_atlas_slot_for(
    NanoUiAtlasCursor *cur,
    void *atlas,
    bool (*open_page)(void *atlas, int page),
    int gw,
    int gh,
    int *out_page,
    int *out_x,
    int *out_y)
{
    const int pad = NANO_UI_GLYPH_ATLAS_PAD;
    const int size = NANO_UI_GLYPH_ATLAS_SIZE;
    if (gw + 2 * pad > size || gh + 2 * pad > size) {
        return false;
    }
    if (cur->pages == 0 && !open_page(atlas, 0)) {
        return false;
    }
    for (;;) {
        if (cur->x + gw + pad <= size && cur->y + gh + pad <= size) {
            break;
        }
        int next_y = cur->y + (cur->row_h > 0 ? cur->row_h + pad : pad);
        if (next_y + gh + pad <= size) {
            cur->y = next_y;
            cur->x = pad;
            cur->row_h = 0;
            break;
        }
        if (cur->pages >= NANO_UI_GLYPH_ATLAS_PAGES || !open_page(atlas, cur->pages)) {
            return false;
        }
    }
    *out_page = cur->pages - 1;
    *out_x = cur->x;
    *out_y = cur->y;
    return true;
}

/* Move the cursor past a gw by gh glyph copied to (x, y) on page `page`,
 * and write its u0, v0, u1 and v1: normalised to [0,1] within the page,
 * plus the page's number in u. */
static inline void nano_ui_atlas_commit(
    NanoUiAtlasCursor *cur, int page, int x, int y, int gw, int gh, float uv[4])
{
    cur->x = x + gw + NANO_UI_GLYPH_ATLAS_PAD;
    if (gh > cur->row_h) {
        cur->row_h = gh;
    }
    float s = (float)NANO_UI_GLYPH_ATLAS_SIZE;
    uv[0] = (float)page + (float)x / s;
    uv[1] = (float)y / s;
    uv[2] = uv[0] + (float)gw / s;
    uv[3] = uv[1] + (float)gh / s;
}

#endif
