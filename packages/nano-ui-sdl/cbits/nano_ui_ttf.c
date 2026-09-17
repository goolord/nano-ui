#include <SDL3/SDL.h>
#include <SDL3_ttf/SDL_ttf.h>
#include <SDL3_ttf/SDL_textengine.h>
#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>

bool nano_ui_ttf_init(void)
{
    return TTF_Init();
}

void nano_ui_ttf_quit(void)
{
    TTF_Quit();
}

/* Kerning is on by default; pin it so shaping survives defaults changing.
 * Direction and script stay unset: shaping detects them per run, so
 * right-to-left and complex scripts shape as themselves. Light grid-fitting
 * snaps stems to whole pixels the way terminals (alacritty/kitty) rasterize,
 * giving crisp edges instead of soft, grey antialiased outlines. NORMAL keeps
 * fractional outlines. */
static TTF_Font *pin_font_rendering(TTF_Font *font)
{
    if (font) {
        TTF_SetFontKerning(font, true);
        TTF_SetFontHinting(font, TTF_HINTING_LIGHT);
    }
    return font;
}

TTF_Font *nano_ui_ttf_open_font(const char *path, float ptsize)
{
    return pin_font_rendering(TTF_OpenFont(path, ptsize));
}

/* A font at another size over the same source as @font@: the copy shares
 * its file or memory stream, so any number of sizes read one source. */
TTF_Font *nano_ui_ttf_copy_font(TTF_Font *font, float ptsize)
{
    TTF_Font *copy = font ? TTF_CopyFont(font) : NULL;
    if (copy) {
        TTF_SetFontSize(copy, ptsize);
    }
    return pin_font_rendering(copy);
}

void nano_ui_ttf_remove_fallback(TTF_Font *font, TTF_Font *fallback)
{
    if (font && fallback) {
        TTF_RemoveFallbackFont(font, fallback);
    }
}

TTF_Font *nano_ui_ttf_open_font_memory(const void *data, size_t size, float ptsize)
{
    /* The font reads this stream after the Haskell ByteString callback ends.
     * Give the stream its own storage, released by its autoclose lifetime. */
    SDL_IOStream *stream = SDL_IOFromDynamicMem();
    if (!stream) {
        return NULL;
    }
    if (SDL_WriteIO(stream, data, size) != size ||
        SDL_SeekIO(stream, 0, SDL_IO_SEEK_SET) < 0) {
        SDL_CloseIO(stream);
        return NULL;
    }
    SDL_PropertiesID props = SDL_CreateProperties();
    if (!props) {
        SDL_CloseIO(stream);
        return NULL;
    }
    SDL_SetPointerProperty(props, TTF_PROP_FONT_CREATE_IOSTREAM_POINTER, stream);
    SDL_SetBooleanProperty(props, TTF_PROP_FONT_CREATE_IOSTREAM_AUTOCLOSE_BOOLEAN, true);
    SDL_SetFloatProperty(props, TTF_PROP_FONT_CREATE_SIZE_FLOAT, ptsize);
    TTF_Font *font = TTF_OpenFontWithProperties(props);
    SDL_DestroyProperties(props);
    return pin_font_rendering(font);
}

void nano_ui_ttf_close_font(TTF_Font *font)
{
    if (font) {
        TTF_CloseFont(font);
    }
}

float nano_ui_ttf_line_skip(TTF_Font *font)
{
    return (float)TTF_GetFontLineSkip(font);
}

float nano_ui_ttf_ascent(TTF_Font *font)
{
    return (float)TTF_GetFontAscent(font);
}

float nano_ui_ttf_space_advance(TTF_Font *font)
{
    int w = 0;
    int h = 0;
    if (TTF_GetStringSize(font, " ", 1, &w, &h) && w > 0) {
        return (float)w;
    }
    int advance = 0;
    if (TTF_GetGlyphMetrics(font, ' ', NULL, NULL, NULL, NULL, &advance) && advance > 0) {
        return (float)advance;
    }
    return 0.f;
}

bool nano_ui_ttf_glyph_metrics(
    TTF_Font *font,
    Uint32 codepoint,
    int *out_minx,
    int *out_maxx,
    int *out_miny,
    int *out_maxy,
    int *out_advance)
{
    if (!font) {
        return false;
    }
    int minx = 0, maxx = 0, miny = 0, maxy = 0, advance = 0;
    if (!TTF_GetGlyphMetrics(font, codepoint, &minx, &maxx, &miny, &maxy, &advance)) {
        return false;
    }
    if (out_minx)   *out_minx   = minx;
    if (out_maxx)   *out_maxx   = maxx;
    if (out_miny)   *out_miny   = miny;
    if (out_maxy)   *out_maxy   = maxy;
    if (out_advance) *out_advance = advance;
    return true;
}

static Uint8 glyph_channel_max(Uint8 r, Uint8 g, Uint8 b)
{
    Uint8 m = r;
    if (g > m) {
        m = g;
    }
    if (b > m) {
        m = b;
    }
    return m;
}

static void force_white_rgb(SDL_Surface *surf)
{
    if (!surf || !surf->pixels || surf->format != SDL_PIXELFORMAT_RGBA32) {
        return;
    }
    Uint8 *base = (Uint8 *)surf->pixels;
    int pitch = surf->pitch;
    for (int y = 0; y < surf->h; y++) {
        Uint8 *row = base + y * pitch;
        for (int x = 0; x < surf->w; x++) {
            Uint8 *px = row + x * 4;
            px[0] = 255;
            px[1] = 255;
            px[2] = 255;
        }
    }
}

static void invert_glyph_alpha(SDL_Surface *surf)
{
    if (!surf || !surf->pixels || surf->format != SDL_PIXELFORMAT_RGBA32) {
        return;
    }
    Uint8 *base = (Uint8 *)surf->pixels;
    int pitch = surf->pitch;
    for (int y = 0; y < surf->h; y++) {
        Uint8 *row = base + y * pitch;
        for (int x = 0; x < surf->w; x++) {
            row[x * 4 + 3] = (Uint8)(255 - row[x * 4 + 3]);
        }
    }
}

static SDL_Surface *glyph_image_to_rgba(SDL_Surface *raw, TTF_ImageType image_type)
{
    if (!raw) {
        return NULL;
    }

    SDL_Surface *out = SDL_ConvertSurface(raw, SDL_PIXELFORMAT_RGBA32);
    if (!out) {
        return NULL;
    }

    if (image_type == TTF_IMAGE_ALPHA || image_type == TTF_IMAGE_SDF) {
        /* Spec: color channels are white, alpha is coverage. Transparent
         * white (a=0, rgb=255) must stay transparent. Do not use luma. */
        force_white_rgb(out);
        return out;
    }

    Uint8 *base = (Uint8 *)out->pixels;
    int pitch = out->pitch;
    Uint32 opaque_sum = 0;
    Uint32 count = 0;
    for (int y = 0; y < out->h; y++) {
        Uint8 *row = base + y * pitch;
        for (int x = 0; x < out->w; x++) {
            Uint8 *px = row + x * 4;
            Uint8 luma = glyph_channel_max(px[0], px[1], px[2]);
            Uint8 a = px[3];
            Uint8 cov = a > luma ? a : luma;
            px[0] = 255;
            px[1] = 255;
            px[2] = 255;
            px[3] = cov;
            opaque_sum += cov;
            count += 1;
        }
    }
    if (count > 0 && opaque_sum > (255u * count) / 2u) {
        invert_glyph_alpha(out);
    }
    return out;
}

bool nano_ui_ttf_render_glyph_surface(
    TTF_Font *font,
    Uint32 codepoint,
    SDL_Surface **out_surface)
{
    if (!font || !out_surface) {
        return false;
    }

    TTF_ImageType image_type = TTF_IMAGE_INVALID;
    SDL_Surface *raw = TTF_GetGlyphImage(font, codepoint, &image_type);
    if (!raw) {
        SDL_Color white = {255, 255, 255, 255};
        raw = TTF_RenderGlyph_Blended(font, codepoint, white);
        image_type = TTF_IMAGE_ALPHA;
    }
    if (!raw) {
        return false;
    }

    SDL_Surface *converted = glyph_image_to_rgba(raw, image_type);
    SDL_DestroySurface(raw);
    if (!converted) {
        return false;
    }

    *out_surface = converted;
    return true;
}

void nano_ui_destroy_texture(SDL_Texture *texture)
{
    if (texture) {
        SDL_DestroyTexture(texture);
    }
}

int nano_ui_ttf_get_kerning(TTF_Font *font, Uint32 prev_cp, Uint32 cp)
{
    if (!font) {
        return 0;
    }
    int k = 0;
    TTF_GetGlyphKerning(font, prev_cp, cp, &k);
    return k;
}

/* ------------------------------------------------------------------------ */
/* Shaping                                                                  */
/* ------------------------------------------------------------------------ */

/* One shaped line, copied out of SDL_ttf's text layout: per glyph
 * (text offset, glyph index, dst x y w h, src x y w h) and its font; per
 * cluster (byte offset, byte length, x, width, flags). Coordinates are
 * pixels from the top left of the line. */
typedef struct NanoUIShaped {
    int w;
    int h;
    int num_glyphs;
    int *glyphs;
    TTF_Font **glyph_fonts;
    int num_clusters;
    int *clusters;
    /* Bytes of the caller's text; anything after is the sentinel. */
    int text_len;
} NanoUIShaped;

static bool SDLCALL nano_ui_capture_text(void *userdata, TTF_Text *text)
{
    NanoUIShaped *out = (NanoUIShaped *)userdata;
    TTF_TextData *d = text->internal;
    int copies = 0;
    for (int i = 0; i < d->num_ops; i++) {
        if (d->ops[i].cmd == TTF_DRAW_COMMAND_COPY) {
            copies++;
        }
    }
    out->w = d->w;
    out->h = d->h;
    out->glyphs = (int *)SDL_calloc(copies > 0 ? copies : 1, 10 * sizeof(int));
    out->glyph_fonts = (TTF_Font **)SDL_calloc(copies > 0 ? copies : 1, sizeof(TTF_Font *));
    out->clusters = (int *)SDL_calloc(d->num_clusters > 0 ? d->num_clusters : 1, 5 * sizeof(int));
    if (!out->glyphs || !out->glyph_fonts || !out->clusters) {
        return false;
    }
    /* The sentinel ends a left-to-right line and starts a right-to-left
     * one, which then shifts back by its advance. */
    int shift = 0;
    for (int i = 0; i < d->num_clusters; i++) {
        TTF_SubString *c = &d->clusters[i];
        if (c->offset >= out->text_len && c->length > 0) {
            int advance = 0;
            TTF_GetGlyphMetrics(d->font, '|', NULL, NULL, NULL, NULL, &advance);
            out->w = d->w - advance;
            if ((c->flags & TTF_SUBSTRING_DIRECTION_MASK) == TTF_DIRECTION_RTL) {
                shift = advance;
            }
        }
    }
    int g = 0;
    for (int i = 0; i < d->num_ops; i++) {
        TTF_DrawOperation *op = &d->ops[i];
        if (op->cmd != TTF_DRAW_COMMAND_COPY || op->copy.text_offset >= out->text_len) {
            continue;
        }
        int *slot = out->glyphs + g * 10;
        slot[0] = op->copy.text_offset;
        slot[1] = (int)op->copy.glyph_index;
        slot[2] = op->copy.dst.x - shift;
        /* SDL_ttf places a fallback font's glyphs from that font's own
         * ascent; align them to the line's font's baseline instead. */
        slot[3] = op->copy.dst.y - (TTF_GetFontAscent(op->copy.glyph_font) - TTF_GetFontAscent(d->font));
        slot[4] = op->copy.dst.w;
        slot[5] = op->copy.dst.h;
        slot[6] = op->copy.src.x;
        slot[7] = op->copy.src.y;
        slot[8] = op->copy.src.w;
        slot[9] = op->copy.src.h;
        out->glyph_fonts[g] = op->copy.glyph_font;
        g++;
    }
    out->num_glyphs = g;
    int k = 0;
    for (int i = 0; i < d->num_clusters; i++) {
        TTF_SubString *c = &d->clusters[i];
        if (c->offset >= out->text_len) {
            continue;
        }
        int *slot = out->clusters + k * 5;
        slot[0] = c->offset;
        slot[1] = c->length;
        slot[2] = c->rect.x - shift;
        slot[3] = c->rect.w;
        slot[4] = (int)c->flags;
        k++;
    }
    out->num_clusters = k;
    d->engine_text = out;
    return true;
}

static void SDLCALL nano_ui_release_text(void *userdata, TTF_Text *text)
{
    (void)userdata;
    (void)text;
}

/* Shape one line with the font and its fallbacks, in a direction (0 lets
 * SDL_ttf pick, TTF_DIRECTION_LTR or TTF_DIRECTION_RTL). The caller frees
 * the result with nano_ui_ttf_shaped_free.
 *
 * The line is shaped with a '|' after it, whose glyph and cluster are then
 * dropped. SDL_ttf 3.2 cuts a fallback span that ends the text one character
 * past its last cluster, losing a vowel sign merged into that cluster, and
 * trims the width of trailing spaces. */
bool nano_ui_ttf_shape(TTF_Font *font, const char *text, size_t len, int direction, NanoUIShaped *out)
{
    SDL_zerop(out);
    if (!font || !text || len == 0) {
        return font != NULL;
    }
    TTF_TextEngine engine;
    SDL_INIT_INTERFACE(&engine);
    engine.userdata = out;
    engine.CreateText = nano_ui_capture_text;
    engine.DestroyText = nano_ui_release_text;
    out->text_len = (int)len;
    bool sentinel = TTF_FontHasGlyph(font, '|');
    char *padded = SDL_malloc(len + 1);
    if (!padded) {
        return false;
    }
    SDL_memcpy(padded, text, len);
    padded[len] = '|';
    TTF_Text *t = TTF_CreateText(&engine, font, padded, sentinel ? len + 1 : len);
    SDL_free(padded);
    if (!t) {
        return false;
    }
    if (direction != 0) {
        TTF_SetTextDirection(t, (TTF_Direction)direction);
    }
    /* Laying the text out hands it to the engine, which copies it. */
    int w = 0, h = 0;
    bool ok = TTF_GetTextSize(t, &w, &h);
    if (ok && out->glyphs == NULL) {
        ok = TTF_UpdateText(t) && out->glyphs != NULL;
    }
    TTF_DestroyText(t);
    return ok;
}

void nano_ui_ttf_shaped_free(NanoUIShaped *shaped)
{
    if (shaped) {
        SDL_free(shaped->glyphs);
        SDL_free(shaped->glyph_fonts);
        SDL_free(shaped->clusters);
        SDL_zerop(shaped);
    }
}

size_t nano_ui_ttf_shaped_size(void)
{
    return sizeof(NanoUIShaped);
}

/* Fields of a shaped line: 0 width, 1 height, 2 glyph count, 3 cluster count. */
int nano_ui_ttf_shaped_int(const NanoUIShaped *shaped, int field)
{
    switch (field) {
    case 0: return shaped->w;
    case 1: return shaped->h;
    case 2: return shaped->num_glyphs;
    case 3: return shaped->num_clusters;
    default: return 0;
    }
}

/* 0 glyphs, 1 glyph fonts, 2 clusters. */
void *nano_ui_ttf_shaped_ptr(const NanoUIShaped *shaped, int field)
{
    switch (field) {
    case 0: return shaped->glyphs;
    case 1: return (void *)shaped->glyph_fonts;
    case 2: return shaped->clusters;
    default: return NULL;
    }
}

bool nano_ui_ttf_render_glyph_index_surface(TTF_Font *font, Uint32 glyph_index, SDL_Surface **out_surface)
{
    if (!font || !out_surface) {
        return false;
    }
    TTF_ImageType image_type = TTF_IMAGE_INVALID;
    SDL_Surface *raw = TTF_GetGlyphImageForIndex(font, glyph_index, &image_type);
    if (!raw) {
        return false;
    }
    SDL_Surface *converted = glyph_image_to_rgba(raw, image_type);
    SDL_DestroySurface(raw);
    if (!converted) {
        return false;
    }
    *out_surface = converted;
    return true;
}

bool nano_ui_ttf_has_glyph(TTF_Font *font, Uint32 ch)
{
    return font && TTF_FontHasGlyph(font, ch);
}

bool nano_ui_ttf_add_fallback(TTF_Font *font, TTF_Font *fallback)
{
    return font && fallback && TTF_AddFallbackFont(font, fallback);
}
