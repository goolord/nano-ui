#include <SDL3/SDL.h>
#include <stddef.h>
#include <stdbool.h>

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

typedef void (*nano_ui_resize_cb)(void);

static nano_ui_resize_cb g_resize_cb = NULL;

static bool nano_ui_resize_watch(void *userdata, SDL_Event *event)
{
    (void)userdata;
    if (!g_resize_cb || !event) {
        return true;
    }
    /* Not SDL_EVENT_WINDOW_RESIZED: SDL sends it just before the pixel size
     * change, which is what tells the renderer to resize its swap chain. A
     * frame drawn on RESIZED goes to the old-size backbuffer, shown cropped
     * or with a bare strip, and the size then counts as drawn, so the window
     * trails the drag by a step. */
    if (event->type == SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED
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
