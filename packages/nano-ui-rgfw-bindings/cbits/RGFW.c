#ifndef RGFW_EXPORT
#define RGFW_EXPORT
#endif
#ifndef RGFW_IMPLEMENTATION
#define RGFW_IMPLEMENTATION
#endif
#ifndef RGFW_OPENGL
#define RGFW_OPENGL
#endif
#include "RGFW.h"

static RGFW_info s_rgfw_info;
static int s_rgfw_initialized = 0;

static int32_t rgfw_init(const char* name) {
    if (!s_rgfw_initialized) {
        int32_t res = RGFW_init_ptr(name ? name : "nano-ui", 0, &s_rgfw_info);
        if (res == 0) s_rgfw_initialized = 1;
#ifdef RGFW_UNIX
        /* The pipe RGFW_stopCheckEvents writes to. RGFW_waitForEvent makes it
           on its first wait, and a wake from another thread before that
           would write to descriptor 0. */
        if (res == 0 && _RGFW->eventWait_forceStop[1] == 0) {
            if (pipe(_RGFW->eventWait_forceStop) == -1) {
                _RGFW->eventWait_forceStop[0] = 0;
                _RGFW->eventWait_forceStop[1] = 0;
            } else {
                /* Non-blocking: a stop never blocks its thread on a full pipe,
                   and the wait's drain never blocks on an empty one. */
                for (int i = 0; i < 2; i++)
                    fcntl(_RGFW->eventWait_forceStop[i], F_SETFL, fcntl(_RGFW->eventWait_forceStop[i], F_GETFL, 0) | O_NONBLOCK);
            }
        }
#endif
        return res;
    }
    return 0;
}

/* Create a window with a native core-profile OpenGL context of at least
   major.minor, current on the calling thread. NULL if either fails. */
RGFW_window* rgfw_create_window_gl(const char* name, int32_t x, int32_t y, int32_t w, int32_t h, uint32_t flags, int32_t major, int32_t minor) {
    if (!s_rgfw_initialized && rgfw_init(name ? name : "nano-ui") != 0) return NULL;
    /* rgfw_init does not request RGFW_initOpenGL, so load the GL library here. */
    if (RGFW_loadGL() == RGFW_FALSE) return NULL;
    RGFW_glHints* hints = RGFW_getGlobalHints_OpenGL();
    hints->profile = RGFW_glCore;
    hints->major = major;
    hints->minor = minor;
    hints->depth = 0;
    hints->stencil = 0;
    RGFW_window* win = RGFW_createWindow(name, x, y, w, h, (RGFW_windowFlags)flags | RGFW_windowOpenGL);
    if (win != NULL && RGFW_window_getContext_OpenGL(win) == NULL) {
        RGFW_window_close(win);
        return NULL;
    }
    return win;
}

int32_t rgfw_window_w(const RGFW_window* win) { return win->w; }
int32_t rgfw_window_h(const RGFW_window* win) { return win->h; }

float rgfw_window_scale(RGFW_window* win) {
    if (!win) return 1.0f;
    RGFW_monitor* mon = RGFW_window_getMonitor(win);
    if (!mon) {
        mon = RGFW_getPrimaryMonitor();
    }
    if (mon) {
        float x = 1.0f, y = 1.0f;
        if (RGFW_monitor_getScale(mon, &x, &y) && x > 0.0f) {
            return x;
        }
    }
    return 1.0f;
}

uint8_t rgfw_window_set_mouse_standard(RGFW_window* win, uint8_t icon) {
    if (!win) return 0;
    return (uint8_t)RGFW_window_setMouseStandard(win, (RGFW_mouseIcon)icon);
}

uint8_t rgfw_window_set_mouse_default(RGFW_window* win) {
    if (!win) return 0;
    return (uint8_t)RGFW_window_setMouseDefault(win);
}

/* Clipboard text, owned by RGFW and valid until the next read. NULL with
   *len 0 when the clipboard holds no text; *len leaves out RGFW's NUL
   terminator. */
const char* rgfw_read_clipboard_text(size_t* len) {
    const RGFW_dataTransfer* data = RGFW_readClipboardString();
    if (data == NULL || data->data == NULL) {
        *len = 0;
        return NULL;
    }
    size_t n = data->length;
    while (n > 0 && data->data[n - 1] == '\0') {
        n--;
    }
    *len = n;
    return data->data;
}

uint8_t rgfw_write_clipboard_text(const char* text, size_t len) {
    /* RGFW inspects data[length - 1], so empty text goes out as a lone NUL. */
    RGFW_dataTransfer data = { len > 0 ? text : "", len > 0 ? len : 1, RGFW_dataText };
    return (uint8_t)RGFW_writeClipboard(&data);
}
