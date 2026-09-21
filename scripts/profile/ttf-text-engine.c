/* Feasibility probe for replacing the glyph backend with SDL_ttf's renderer
 * text engine. Pass the same font file the existing backend uses. nano-ui's
 * synthetic weight/slant preserve regular-face layout, so native styling must
 * preserve those dimensions before it can replace that path transparently. */
#include <SDL3/SDL.h>
#include <SDL3_ttf/SDL_ttf.h>
#include <inttypes.h>
#include <stdio.h>

static int failure(void)
{
    fprintf(stderr, "text engine probe failed: %s\n", SDL_GetError());
    return 1;
}

static uint64_t pixel_hash(SDL_Renderer *renderer)
{
    SDL_Surface *raw = SDL_RenderReadPixels(renderer, NULL);
    if (!raw) return 0;
    SDL_Surface *rgba = SDL_ConvertSurface(raw, SDL_PIXELFORMAT_RGBA32);
    SDL_DestroySurface(raw);
    if (!rgba) return 0;
    uint64_t hash = UINT64_C(14695981039346656037);
    for (int y = 0; y < rgba->h; ++y) {
        const unsigned char *row = (const unsigned char *)rgba->pixels + y * rgba->pitch;
        for (int x = 0; x < rgba->w * 4; ++x) hash = (hash ^ row[x]) * UINT64_C(1099511628211);
    }
    SDL_DestroySurface(rgba);
    return hash;
}

int main(int argc, char **argv)
{
    if (argc != 2) {
        fprintf(stderr, "usage: ttf-text-engine FONT.ttf\n");
        return 1;
    }
    SDL_SetHint(SDL_HINT_VIDEO_DRIVER, "dummy");
    SDL_SetHint(SDL_HINT_RENDER_DRIVER, "software");
    if (!SDL_Init(SDL_INIT_VIDEO) || !TTF_Init()) return failure();
    SDL_Window *window = NULL;
    SDL_Renderer *renderer = NULL;
    if (!SDL_CreateWindowAndRenderer("text engine probe", 640, 100, SDL_WINDOW_HIDDEN, &window, &renderer)) return failure();
    TTF_Font *font = TTF_OpenFont(argv[1], 16);
    if (!font) return failure();
    TTF_SetFontKerning(font, true);
    TTF_SetFontHinting(font, TTF_HINTING_LIGHT);
    TTF_TextEngine *engine = TTF_CreateRendererTextEngine(renderer);
    if (!engine) return failure();
    TTF_Text *text = TTF_CreateText(engine, font, "AV To fi cafe -- synthetic styles  ", 0);
    if (!text) return failure();
    TTF_SetTextColor(text, 255, 255, 255, 255);
    const int styles[] = {TTF_STYLE_NORMAL, TTF_STYLE_BOLD, TTF_STYLE_ITALIC, TTF_STYLE_BOLD | TTF_STYLE_ITALIC};
    const char *names[] = {"regular", "bold", "italic", "bold-italic"};
    int base_w = 0, base_h = 0;
    for (int i = 0; i < 4; ++i) {
        TTF_SetFontStyle(font, styles[i]);
        int w = 0, h = 0;
        if (!TTF_UpdateText(text) || !TTF_GetTextSize(text, &w, &h) || w <= 0 || h <= 0) return failure();
        if (i == 0) { base_w = w; base_h = h; }
        SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
        SDL_RenderClear(renderer);
        if (!TTF_DrawRendererText(text, 16, 16)) return failure();
        const uint64_t hash = pixel_hash(renderer);
        if (!hash) return failure();
        printf("%s: %dx%d, regular-layout=%s, pixels=%016" PRIx64 "\n",
               names[i], w, h, w == base_w && h == base_h ? "yes" : "no", hash);
    }
    TTF_DestroyText(text);
    TTF_DestroyRendererTextEngine(engine);
    TTF_CloseFont(font);
    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    TTF_Quit();
    SDL_Quit();
    return 0;
}
