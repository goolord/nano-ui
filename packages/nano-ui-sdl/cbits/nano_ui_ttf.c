#include "nano_ui_opt.h"
#include <SDL3/SDL.h>
#include <SDL3_ttf/SDL_ttf.h>
#include <hb.h>
#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

bool nano_ui_ttf_init(void)
{
    return TTF_Init();
}

void nano_ui_ttf_quit(void)
{
    TTF_Quit();
}

TTF_Font *nano_ui_ttf_open_font(const char *path, float ptsize)
{
    TTF_Font *font = TTF_OpenFont(path, ptsize);
    /* Kerning is on by default; pin it so shaping survives defaults changing. */
    if (font) {
        TTF_SetFontKerning(font, true);
        TTF_SetFontDirection(font, TTF_DIRECTION_LTR);
        TTF_SetFontScript(font, TTF_StringToTag("Latn"));
    }
    return font;
}

TTF_Font *nano_ui_ttf_open_font_memory(const void *data, size_t size, float ptsize)
{
    SDL_IOStream *stream = SDL_IOFromConstMem(data, size);
    if (!stream) {
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
    if (font) {
        TTF_SetFontKerning(font, true);
        TTF_SetFontDirection(font, TTF_DIRECTION_LTR);
        TTF_SetFontScript(font, TTF_StringToTag("Latn"));
    }
    return font;
}

void nano_ui_ttf_close_font(TTF_Font *font)
{
    if (font) {
        TTF_CloseFont(font);
    }
}

void nano_ui_ttf_set_font_style(TTF_Font *font, int style)
{
    if (font) {
        TTF_SetFontStyle(font, (TTF_FontStyleFlags)style);
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

bool nano_ui_ttf_string_size(
    TTF_Font *font,
    const char *text,
    size_t len,
    float *out_w,
    float *out_h)
{
    int w = 0;
    int h = 0;
    if (len == 0) {
        text = "";
    }
    if (!TTF_GetStringSize(font, text, len, &w, &h)) {
        return false;
    }
    if (out_w) {
        *out_w = (float)w;
    }
    if (out_h) {
        *out_h = (float)h;
    }
    return true;
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

bool nano_ui_ttf_render_text_surface(
    TTF_Font *font,
    const char *text,
    size_t len,
    SDL_Surface **out_surface)
{
    if (!font || !text || !out_surface) {
        return false;
    }
    if (len == 0) {
        text = "";
    }

    SDL_Color white = {255, 255, 255, 255};
    SDL_Surface *raw = TTF_RenderText_Blended(font, text, len, white);
    if (!raw) {
        return false;
    }

    SDL_Surface *converted = glyph_image_to_rgba(raw, TTF_IMAGE_ALPHA);
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

bool nano_ui_ttf_save_render_text(
    TTF_Font *font,
    const char *text,
    const char *bmp_path)
{
    if (!font || !text || !bmp_path) {
        return false;
    }
    SDL_Color white = {255, 255, 255, 255};
    SDL_Surface *surf = TTF_RenderText_Blended(font, text, 0, white);
    if (!surf) {
        return false;
    }
    bool ok = SDL_SaveBMP(surf, bmp_path);
    SDL_DestroySurface(surf);

    TTF_TextEngine *engine = TTF_CreateSurfaceTextEngine();
    if (engine) {
        TTF_Text *t = TTF_CreateText(engine, font, text, 0);
        if (t) {
            int len = (int)strlen(text);
            printf("SDL3_ttf character layout for \"%s\":\n", text);
            for (int i = 0; i < len; i++) {
                TTF_SubString sub;
                if (TTF_GetTextSubString(t, i, &sub)) {
                    printf("  [%d] '%c': x=%d, y=%d, w=%d, h=%d\n",
                           i, text[i], sub.rect.x, sub.rect.y, sub.rect.w, sub.rect.h);
                }
            }
            int w = 0, h = 0;
            TTF_GetTextSize(t, &w, &h);
            printf("Total text size: w=%d, h=%d\n", w, h);
            TTF_DestroyText(t);
        }
        TTF_DestroySurfaceTextEngine(engine);
    }

    return ok;
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

static int utf8_encode_cp(char *out, Uint32 cp)
{
    if (cp < 0x80) {
        out[0] = (char)cp;
        return 1;
    }
    if (cp < 0x800) {
        out[0] = (char)(0xC0 | (cp >> 6));
        out[1] = (char)(0x80 | (cp & 0x3F));
        return 2;
    }
    if (cp < 0x10000) {
        out[0] = (char)(0xE0 | (cp >> 12));
        out[1] = (char)(0x80 | ((cp >> 6) & 0x3F));
        out[2] = (char)(0x80 | (cp & 0x3F));
        return 3;
    }
    out[0] = (char)(0xF0 | (cp >> 18));
    out[1] = (char)(0x80 | ((cp >> 12) & 0x3F));
    out[2] = (char)(0x80 | ((cp >> 6) & 0x3F));
    out[3] = (char)(0x80 | (cp & 0x3F));
    return 4;
}

static TTF_TextEngine *pair_kerning_engine(void)
{
    static TTF_TextEngine *engine = NULL;
    if (!engine) {
        engine = TTF_CreateSurfaceTextEngine();
    }
    return engine;
}

/* Measure the shaped 2-glyph layout of "pc" and diff it against the raw
   glyph advances: sub-strings report pen + left bearing, so the bearing
   difference cancels and what remains is the GPOS pair adjustment.  A
   ligature (or any nonsense beyond half an advance) yields 0, which the
   per-char pen model represents as "no kerning" anyway. */
int nano_ui_ttf_get_pair_kerning(TTF_Font *font, Uint32 prev_cp, Uint32 cp)
{
    if (!font) {
        return 0;
    }
    TTF_TextEngine *engine = pair_kerning_engine();
    if (!engine) {
        return 0;
    }
    char buf[8];
    int len = utf8_encode_cp(buf, prev_cp);
    len += utf8_encode_cp(buf + len, cp);
    TTF_Text *t = TTF_CreateText(engine, font, buf, len);
    if (!t) {
        return 0;
    }
    int kern = 0;
    TTF_SubString s0, s1;
    if (TTF_GetTextSubString(t, 0, &s0) && TTF_GetTextSubString(t, 1, &s1)) {
        int minx_p = 0, maxx = 0, miny = 0, maxy = 0, adv_p = 0;
        int minx_c = 0, adv_c = 0;
        if (TTF_GetGlyphMetrics(font, prev_cp, &minx_p, &maxx, &miny, &maxy, &adv_p) &&
            TTF_GetGlyphMetrics(font, cp, &minx_c, &maxx, &miny, &maxy, &adv_c)) {
            kern = (s1.rect.x - s0.rect.x) - (adv_p + minx_c - minx_p);
            if (kern < -adv_p / 2 || kern > adv_p / 2) {
                kern = 0;
            }
        }
    }
    TTF_DestroyText(t);
    return kern;
}

void nano_ui_ttf_debug_pair(TTF_Font *font, Uint32 prev_cp, Uint32 cp)
{
    if (!font) {
        return;
    }
    TTF_TextEngine *engine = pair_kerning_engine();
    if (!engine) {
        printf("  debug: no engine\n");
        return;
    }
    char buf[8];
    int len = utf8_encode_cp(buf, prev_cp);
    len += utf8_encode_cp(buf + len, cp);
    printf("  debug pair cp %u-%u len=%d\n", prev_cp, cp, len);
    TTF_Text *t = TTF_CreateText(engine, font, buf, len);
    if (!t) {
        printf("  debug: TTF_CreateText failed\n");
        return;
    }
    TTF_SubString s0, s1, sAll;
    if (TTF_GetTextSubString(t, 0, &s0)) {
        printf("  s0: x=%d y=%d w=%d\n", s0.rect.x, s0.rect.y, s0.rect.w);
    } else {
        printf("  s0 failed\n");
    }
    if (TTF_GetTextSubString(t, 1, &s1)) {
        printf("  s1: x=%d y=%d w=%d\n", s1.rect.x, s1.rect.y, s1.rect.w);
    } else {
        printf("  s1 failed\n");
    }
    if (TTF_GetTextSubString(t, 0, &sAll)) { }
    int w = 0, h = 0;
    TTF_GetTextSize(t, &w, &h);
    printf("  text size: %dx%d\n", w, h);
    int sw = 0, sh = 0;
    if (TTF_GetStringSize(font, buf, len, &sw, &sh)) {
        printf("  TTF_GetStringSize: %d (expect kerned ~%d for To/AV)\n", sw, w - 3);
    }
    int minx_p = 0, maxx = 0, miny = 0, maxy = 0, adv_p = 0;
    int minx_c = 0, adv_c = 0;
    TTF_GetGlyphMetrics(font, prev_cp, &minx_p, &maxx, &miny, &maxy, &adv_p);
    TTF_GetGlyphMetrics(font, cp, &minx_c, &maxx, &miny, &maxy, &adv_c);
    printf("  metrics p: adv=%d minx=%d | c: adv=%d minx=%d\n", adv_p, minx_p, adv_c, minx_c);
    int kern = (s1.rect.x - s0.rect.x) - (adv_p + minx_c - minx_p);
    printf("  computed kern=%d\n", kern);
    TTF_DestroyText(t);
}

void nano_ui_ttf_dump_layout(TTF_Font *font, const char *text)
{
    if (!font || !text) {
        return;
    }
    const char *family = TTF_GetFontFamilyName(font);
    const char *styleName = TTF_GetFontStyleName(font);
    printf("== layout dump '%s' family='%s' style='%s' ==\n", text,
           family ? family : "?", styleName ? styleName : "?");
    printf("  font dir=%d script=%u kern=%d\n",
           (int)TTF_GetFontDirection(font),
           TTF_GetFontScript(font),
           TTF_GetFontKerning(font) ? 1 : 0);
    /* Sweep the knobs that might gate hb kerning and print the T->o sub-delta. */
    int tw = 0, th = 0;
    TTF_GetStringSize(font, "To", 2, &tw, &th);
    printf("  sweep base:      To width=%d (unkerned ref, kern would shrink ~2-3)\n", tw);
    bool okLang = TTF_SetFontLanguage(font, "en");
    TTF_GetStringSize(font, "To", 2, &tw, &th);
    printf("  sweep lang=en:   ok=%d To width=%d\n", okLang, tw);
    bool reScript = TTF_SetFontScript(font, TTF_StringToTag("Latn"));
    bool reDir = TTF_SetFontDirection(font, TTF_DIRECTION_LTR);
    TTF_GetStringSize(font, "To", 2, &tw, &th);
    printf("  sweep re-set:    script=%d dir=%d To width=%d\n", reScript, reDir, tw);
    TTF_TextEngine *engine = TTF_CreateSurfaceTextEngine();
    TTF_Text *t = engine ? TTF_CreateText(engine, font, text, 0) : NULL;
    int len = (int)strlen(text);
    for (int i = 0; i < len; i++) {
        Uint32 cp = (Uint32)(unsigned char)text[i];
        int minx = 0, maxx = 0, miny = 0, maxy = 0, adv = 0;
        bool gm = TTF_GetGlyphMetrics(font, cp, &minx, &maxx, &miny, &maxy, &adv);
        int kern = 0;
        if (i > 0) {
            TTF_GetGlyphKerning(font, (Uint32)(unsigned char)text[i - 1], cp, &kern);
        }
        int sx = 0, sw = 0;
        TTF_SubString sub;
        if (t && TTF_GetTextSubString(t, i, &sub)) {
            sx = sub.rect.x;
            sw = sub.rect.w;
        }
        printf("  [%2d] '%c' adv=%4d minx=%3d kern(prev)=%4d sub.x=%4d sub.w=%4d%s\n",
               i, text[i], adv, minx, kern, sx, sw, gm ? "" : "  (no glyph)");
    }
    if (t) {
        int w = 0, h = 0;
        TTF_GetTextSize(t, &w, &h);
        printf("  TTF_GetTextSize: %dx%d\n", w, h);
        TTF_DestroyText(t);
    }
    if (engine) {
        TTF_DestroySurfaceTextEngine(engine);
    }
    int sw2 = 0, sh2 = 0;
    if (TTF_GetStringSize(font, text, 0, &sw2, &sh2)) {
        printf("  TTF_GetStringSize: %dx%d\n", sw2, sh2);
    }
}
