#include "nano_ui_text_atlas.h"

#include <SDL3/SDL.h>
#include <stdlib.h>

/* Glyphs fill page 0, then each later page in turn, placed by
 * nano_ui_ttf_shared.h as nano-ui-sdl-web's atlas places them. A reset
 * empties every page but keeps their textures, and a page is cleared when
 * it is opened again. */
struct NanoUiTextAtlas {
    SDL_Renderer *renderer;
    SDL_Texture *tex[NANO_UI_TEXT_ATLAS_PAGES];
    NanoUiAtlasCursor cur;
};

/* SDL owns the streaming storage. A lock is write-only, so initialize the
 * complete surface on creation/reset, including transparent glyph padding.
 * Every page gets the white patch, so each starts its cursor the same way;
 * shapes sample the one on page 0. */
static bool clear_texture(SDL_Texture *tex)
{
    SDL_Surface *surface = NULL;
    if (!SDL_LockTextureToSurface(tex, NULL, &surface)) {
        return false;
    }
    SDL_Rect white = {0, 0, NANO_UI_WHITE_PATCH_SIZE, NANO_UI_WHITE_PATCH_SIZE};
    bool ok = SDL_FillSurfaceRect(surface, NULL, 0) &&
              SDL_FillSurfaceRect(surface, &white, 0xffffffffu);
    SDL_UnlockTexture(tex);
    return ok;
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

/* Start glyphs on page `page`: make or clear its texture and put the cursor
 * after its white patch. The atlas is a void pointer for
 * nano_ui_atlas_slot_for. */
static bool open_page(void *self, int page)
{
    NanoUiTextAtlas *atlas = (NanoUiTextAtlas *)self;
    if (!atlas->tex[page]) {
        atlas->tex[page] = create_texture(atlas);
        if (!atlas->tex[page]) {
            return false;
        }
    }
    if (!clear_texture(atlas->tex[page])) {
        return false;
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
    free(atlas);
}

SDL_Texture *nano_ui_text_atlas_texture(NanoUiTextAtlas *atlas, int page)
{
    if (!atlas || page < 0 || page >= NANO_UI_TEXT_ATLAS_PAGES) {
        return NULL;
    }
    return atlas->tex[page];
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
    /* Glyph surfaces already have the atlas's RGBA32 format. Preserve their
     * pitch instead of copying each row into a second full-size CPU atlas. */
    SDL_Rect rect = {x, y, gw, gh};
    if (!SDL_UpdateTexture(atlas->tex[page], &rect, surface->pixels, surface->pitch)) {
        return false;
    }
    nano_ui_atlas_commit(&atlas->cur, page, x, y, gw, gh, uv);
    return true;
}

void nano_ui_text_atlas_drop_textures(NanoUiTextAtlas *atlas)
{
    if (!atlas) {
        return;
    }
    for (int i = 0; i < NANO_UI_TEXT_ATLAS_PAGES; i++) {
        if (atlas->tex[i]) {
            SDL_DestroyTexture(atlas->tex[i]);
            atlas->tex[i] = NULL;
        }
    }
    atlas->cur.pages = 0;
}

void nano_ui_text_atlas_reset(NanoUiTextAtlas *atlas)
{
    if (!atlas) {
        return;
    }
    /* Page 0 keeps its texture and is cleared now; the others are cleared
     * when glyphs reach them again. */
    if (atlas->tex[0]) {
        open_page(atlas, 0);
    } else {
        atlas->cur.pages = 0;
    }
}
