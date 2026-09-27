#include <SDL3/SDL.h>
#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>
#if !defined(_WIN32) && !defined(__APPLE__)
#include <dlfcn.h>
#endif

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

/* Size limits for a Wayland window, sent to the compositor by nano-ui rather
 * than by SDL.
 *
 * SDL both hints its minimum and maximum to the compositor and clamps every
 * configure to them. A compositor that ignores the hint (sway's tiling does)
 * then configures a size SDL turns back into the current one: SDL acks the
 * configure and reports nothing, so no frame is drawn and nothing commits the
 * ack. sway holds the resize for its 200 ms transaction timeout, on every step
 * of a drag past the limit. With the limits kept from SDL, such a configure
 * is a real resize, drawn and committed like any other; the compositor still
 * gets the hint, which a floating window's resize honours.
 *
 * SDL loads libwayland-client itself, so the copy it loaded is used rather
 * than linking one. */

#if !defined(_WIN32) && !defined(__APPLE__)
struct wl_proxy;

static struct {
    bool loaded;
    struct wl_proxy *(*marshal_flags)(struct wl_proxy *, uint32_t, const void *, uint32_t, uint32_t, ...);
    uint32_t (*get_version)(struct wl_proxy *);
    void *(*get_user_data)(struct wl_proxy *);
} g_wl;

/* xdg_toplevel request opcodes, from xdg-shell.xml. */
enum { XDG_TOPLEVEL_SET_MAX_SIZE = 7, XDG_TOPLEVEL_SET_MIN_SIZE = 8 };

/* The window's xdg_toplevel when SDL made it itself, or NULL: not Wayland, not
 * shown yet, or a libdecor frame's, whose limits libdecor sets with its
 * decorations added. SDL listens on its own toplevel with the same window data
 * it keeps on the surface. */
static struct wl_proxy *sdl_toplevel(SDL_Window *window)
{
    if (!g_wl.loaded) {
        g_wl.loaded = true;
        void *lib = dlopen("libwayland-client.so.0", RTLD_NOW | RTLD_NOLOAD);
        if (lib) {
            *(void **)&g_wl.marshal_flags = dlsym(lib, "wl_proxy_marshal_flags");
            *(void **)&g_wl.get_version = dlsym(lib, "wl_proxy_get_version");
            *(void **)&g_wl.get_user_data = dlsym(lib, "wl_proxy_get_user_data");
        }
    }
    if (!window || !g_wl.marshal_flags || !g_wl.get_version || !g_wl.get_user_data) {
        return NULL;
    }
    SDL_PropertiesID props = SDL_GetWindowProperties(window);
    struct wl_proxy *toplevel = SDL_GetPointerProperty(props, SDL_PROP_WINDOW_WAYLAND_XDG_TOPLEVEL_POINTER, NULL);
    struct wl_proxy *surface = SDL_GetPointerProperty(props, SDL_PROP_WINDOW_WAYLAND_SURFACE_POINTER, NULL);
    if (!toplevel || !surface || g_wl.get_user_data(toplevel) != g_wl.get_user_data(surface)) {
        return NULL;
    }
    return toplevel;
}

bool nano_ui_wayland_toplevel(SDL_Window *window)
{
    return sdl_toplevel(window) != NULL;
}

/* SDL sends its own limits (none) with every configure, so these are sent
 * again before each commit. Zero is no limit. A window that cannot be resized
 * or is fullscreen keeps what SDL sends: its size, or nothing. */
void nano_ui_wayland_size_limits(SDL_Window *window, int min_w, int min_h, int max_w, int max_h)
{
    struct wl_proxy *toplevel = sdl_toplevel(window);
    SDL_WindowFlags flags = SDL_GetWindowFlags(window);
    if (!toplevel || !(flags & SDL_WINDOW_RESIZABLE) || (flags & SDL_WINDOW_FULLSCREEN)) {
        return;
    }
    uint32_t version = g_wl.get_version(toplevel);
    g_wl.marshal_flags(toplevel, XDG_TOPLEVEL_SET_MIN_SIZE, NULL, version, 0, min_w, min_h);
    g_wl.marshal_flags(toplevel, XDG_TOPLEVEL_SET_MAX_SIZE, NULL, version, 0, max_w, max_h);
}
#else
bool nano_ui_wayland_toplevel(SDL_Window *window)
{
    (void)window;
    return false;
}

void nano_ui_wayland_size_limits(SDL_Window *window, int min_w, int min_h, int max_w, int max_h)
{
    (void)window; (void)min_w; (void)min_h; (void)max_w; (void)max_h;
}
#endif
