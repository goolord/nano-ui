#include <SDL3/SDL.h>
#include <stddef.h>
#include <stdbool.h>

void nano_ui_sdl_init_hints(void)
{
    SDL_SetHint(SDL_HINT_RENDER_VSYNC, "1");
}

void nano_ui_sdl_init_bench_hints(void)
{
    SDL_SetHint(SDL_HINT_ASSERT, "always_ignore");
    SDL_SetHint(SDL_HINT_RENDER_VSYNC, "0");
}

float nano_ui_window_display_scale(SDL_Window *window)
{
    if (!window) {
        return 1.f;
    }
    float scale = SDL_GetWindowDisplayScale(window);
    if (scale <= 0.f) {
        return 1.f;
    }
    return scale;
}

bool nano_ui_window_logical_size(
    SDL_Window *window,
    float scale,
    float *out_w,
    float *out_h)
{
    if (!window) {
        return false;
    }
    int w = 0;
    int h = 0;
    if (!SDL_GetWindowSize(window, &w, &h)) {
        return false;
    }
    if (scale <= 0.f) {
        scale = 1.f;
    }
    if (out_w) {
        *out_w = (float)w / scale;
    }
    if (out_h) {
        *out_h = (float)h / scale;
    }
    return true;
}

bool nano_ui_mouse_window_pos(float *out_x, float *out_y)
{
    float x = 0.f;
    float y = 0.f;
    (void)SDL_GetMouseState(&x, &y);
    if (out_x) {
        *out_x = x;
    }
    if (out_y) {
        *out_y = y;
    }
    return true;
}

typedef void (*nano_ui_resize_cb)(void);

static nano_ui_resize_cb g_resize_cb = NULL;

static bool nano_ui_resize_watch(void *userdata, SDL_Event *event)
{
    (void)userdata;
    if (!g_resize_cb || !event) {
        return true;
    }
    if (event->type == SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED
        || event->type == SDL_EVENT_WINDOW_RESIZED) {
        g_resize_cb();
    }
    return true;
}

bool nano_ui_install_resize_watch(nano_ui_resize_cb cb)
{
    if (!SDL_AddEventWatch(nano_ui_resize_watch, NULL)) {
        g_resize_cb = NULL;
        return false;
    }
    g_resize_cb = cb;
    return true;
}

void nano_ui_remove_resize_watch(void)
{
    SDL_RemoveEventWatch(nano_ui_resize_watch, NULL);
    g_resize_cb = NULL;
}

static Uint32 g_refresh_event_type = 0;

bool nano_ui_register_refresh_event(void)
{
    if (g_refresh_event_type != 0) {
        return true;
    }
    g_refresh_event_type = SDL_RegisterEvents(1);
    return g_refresh_event_type != 0;
}

Uint32 nano_ui_refresh_event_type(void)
{
    return g_refresh_event_type;
}

bool nano_ui_push_refresh_event(void)
{
    if (g_refresh_event_type == 0) {
        return false;
    }
    SDL_Event ev;
    SDL_zero(ev);
    ev.type = g_refresh_event_type;
    return SDL_PushEvent(&ev);
}

bool nano_ui_set_render_scale(SDL_Renderer *renderer, float scale)
{
    if (!renderer || scale <= 0.f) {
        return false;
    }
    return SDL_SetRenderScale(renderer, scale, scale);
}

bool nano_ui_renderer_name(SDL_Renderer *renderer, char *buf, size_t cap)
{
    if (!renderer || !buf || cap == 0) {
        return false;
    }
    const char *name = SDL_GetRendererName(renderer);
    if (!name) {
        buf[0] = '\0';
        return false;
    }
    size_t i = 0;
    for (; i + 1 < cap && name[i]; i++) {
        buf[i] = name[i];
    }
    buf[i] = '\0';
    return true;
}

SDL_Texture *nano_ui_retain_create(SDL_Renderer *renderer, int w, int h)
{
    if (!renderer || w <= 0 || h <= 0) {
        return NULL;
    }
    SDL_Texture *tex =
        SDL_CreateTexture(renderer, SDL_PIXELFORMAT_RGBA32, SDL_TEXTUREACCESS_TARGET, w, h);
    if (!tex) {
        return NULL;
    }
    SDL_SetTextureBlendMode(tex, SDL_BLENDMODE_NONE);
    return tex;
}

bool nano_ui_retain_begin(SDL_Renderer *renderer, SDL_Texture *tex)
{
    if (!renderer || !tex) {
        return false;
    }
    return SDL_SetRenderTarget(renderer, tex);
}

bool nano_ui_retain_blit(SDL_Renderer *renderer, SDL_Texture *tex)
{
    if (!renderer || !tex) {
        return false;
    }
    if (!SDL_SetRenderTarget(renderer, NULL)) {
        return false;
    }
    if (!SDL_SetRenderClipRect(renderer, NULL)) {
        return false;
    }
    return SDL_RenderTexture(renderer, tex, NULL, NULL);
}

bool nano_ui_retain_blit_rect(
    SDL_Renderer *renderer,
    SDL_Texture *tex,
    float src_x,
    float src_y,
    float src_w,
    float src_h,
    float dst_x,
    float dst_y)
{
    if (!renderer || !tex || src_w <= 0.f || src_h <= 0.f) {
        return false;
    }
    if (!SDL_SetRenderTarget(renderer, NULL)) {
        return false;
    }
    if (!SDL_SetRenderClipRect(renderer, NULL)) {
        return false;
    }
    SDL_FRect src = {src_x, src_y, src_w, src_h};
    SDL_FRect dst = {dst_x, dst_y, src_w, src_h};
    return SDL_RenderTexture(renderer, tex, &src, &dst);
}

bool nano_ui_render_coords_from_window(
    SDL_Renderer *renderer,
    float window_x,
    float window_y,
    float *out_x,
    float *out_y)
{
    if (!renderer) {
        return false;
    }
    return SDL_RenderCoordinatesFromWindow(renderer, window_x, window_y, out_x, out_y);
}
