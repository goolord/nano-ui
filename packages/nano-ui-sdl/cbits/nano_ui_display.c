#include <SDL3/SDL.h>
#include <stddef.h>
#include <stdbool.h>

void nano_ui_sdl_init_hints(bool vsync)
{
    SDL_SetHint(SDL_HINT_RENDER_VSYNC, vsync ? "1" : "0");
    /* SDL3 only auto-picks Wayland when the compositor has the fifo-v1 /
     * commit-timing-v1 protocols. Without them (sway, wlroots, many others
     * today) it silently selects X11/XWayland, giving a scale-1 window on a
     * scale-2 (or fractional) output: the compositor upscales the whole
     * window and text looks blurred, "like it's upscaled". Native Wayland
     * + SDL_WINDOW_HIGH_PIXEL_DENSITY makes the window rasterize at the real
     * output scale, which is what alacritty/kitty do. Honour an explicit
     * SDL_VIDEO_DRIVER override, and keep pure-X11 sessions untouched. */
    if (SDL_getenv("WAYLAND_DISPLAY") && !SDL_getenv("SDL_VIDEO_DRIVER")) {
        SDL_SetHint(SDL_HINT_VIDEO_DRIVER, "wayland");
    }
}

void nano_ui_sdl_init_bench_hints(void)
{
    SDL_SetHint(SDL_HINT_ASSERT, "always_ignore");
    SDL_SetHint(SDL_HINT_RENDER_VSYNC, "0");
}

bool nano_ui_set_render_vsync(SDL_Renderer *renderer, bool vsync)
{
    if (!renderer) {
        return false;
    }
    return SDL_SetRenderVSync(renderer, vsync ? 1 : 0);
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

int nano_ui_window_refresh_rate(SDL_Window *window)
{
    if (!window) {
        return 0;
    }
    SDL_DisplayID id = SDL_GetDisplayForWindow(window);
    if (!id) {
        return 0;
    }
    const SDL_DisplayMode *current = SDL_GetCurrentDisplayMode(id);
    if (current && current->refresh_rate > 0) {
        return current->refresh_rate;
    }
    /* Some drivers leave the current mode's refresh at 0 (variable-refresh
     * panels, compositors that report a base rate). Fall back to the highest
     * refresh among display modes at the current size so pacing still
     * targets the panel's real cadence instead of the 60 Hz default. */
    int best = 0;
    int cw = current ? current->w : 0;
    int ch = current ? current->h : 0;
    if (cw <= 0 || ch <= 0) {
        SDL_GetWindowSize(window, &cw, &ch);
    }
    int count = 0;
    SDL_DisplayMode **modes = SDL_GetFullscreenDisplayModes(id, &count);
    for (int i = 0; i < count && modes; i++) {
        if (modes[i] && modes[i]->w == cw && modes[i]->h == ch && modes[i]->refresh_rate > best) {
            best = modes[i]->refresh_rate;
        }
    }
    if (modes) {
        SDL_free(modes);
    }
    return best;
}

/* SDL_GetWindowSize already returns the window-coordinate (logical) size, not
 * pixels. Dividing by the display scale would shrink the logical size on
 * DPI-scaled displays (fullscreen), which made the retained framebuffer too
 * small and sheared NEAREST-sampled text during the blit. */
bool nano_ui_window_logical_size(SDL_Window *window, float *out_w, float *out_h)
{
    if (!window) {
        return false;
    }
    int w = 0;
    int h = 0;
    if (!SDL_GetWindowSize(window, &w, &h)) {
        return false;
    }
    *out_w = (float)w;
    *out_h = (float)h;
    return true;
}

void nano_ui_mouse_window_pos(float *out_x, float *out_y)
{
    (void)SDL_GetMouseState(out_x, out_y);
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
        || event->type == SDL_EVENT_WINDOW_RESIZED
        || event->type == SDL_EVENT_WINDOW_DISPLAY_SCALE_CHANGED) {
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

bool nano_ui_window_target_matches_size(SDL_Renderer *renderer, int w, int h)
{
    int output_w, output_h;
    return SDL_GetRenderOutputSize(renderer, &output_w, &output_h)
        && output_w == w && output_h == h;
}

bool nano_ui_retain_begin(SDL_Renderer *renderer, SDL_Texture *tex, float scale)
{
    /* A NULL texture draws full-repaint sessions straight to the window. */
    if (!renderer) {
        return false;
    }
    if (!SDL_SetRenderTarget(renderer, tex)) {
        return false;
    }
    return scale <= 0.f || SDL_SetRenderScale(renderer, scale, scale);
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
    (void)SDL_SetRenderScale(renderer, 1.f, 1.f);
    return SDL_RenderTexture(renderer, tex, NULL, NULL);
}

bool nano_ui_save_screenshot(SDL_Renderer *renderer, const char *path)
{
    if (!renderer || !path) {
        return false;
    }
    SDL_Surface *surface = SDL_RenderReadPixels(renderer, NULL);
    if (!surface) {
        return false;
    }
    bool ok = SDL_SaveBMP(surface, path);
    SDL_DestroySurface(surface);
    return ok;
}
