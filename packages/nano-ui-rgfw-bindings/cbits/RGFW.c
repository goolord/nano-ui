#ifndef RGFW_EXPORT
#define RGFW_EXPORT
#endif
#ifndef RGFW_IMPLEMENTATION
#define RGFW_IMPLEMENTATION
#endif
#include "RGFW.h"

static RGFW_info s_rgfw_info;
static int s_rgfw_initialized = 0;

int32_t rgfw_init(const char* name) {
    if (!s_rgfw_initialized) {
        int32_t res = RGFW_init_ptr(name ? name : "nano-ui", 0, &s_rgfw_info);
        if (res == 0) s_rgfw_initialized = 1;
        return res;
    }
    return 0;
}

void rgfw_deinit(void) {
    if (s_rgfw_initialized) {
        RGFW_deinit_ptr(&s_rgfw_info);
        s_rgfw_initialized = 0;
    }
}

RGFW_window* rgfw_create_window(const char* name, int32_t x, int32_t y, int32_t w, int32_t h, uint32_t flags) {
    if (!s_rgfw_initialized) {
        rgfw_init(name ? name : "nano-ui");
    }
    return RGFW_createWindow(name, x, y, w, h, (RGFW_windowFlags)flags);
}

RGFW_surface* rgfw_create_surface(RGFW_window* win, uint8_t* data, int32_t w, int32_t h, uint8_t format) {
    return RGFW_window_createSurface(win, data, w, h, (RGFW_format)format);
}

uint8_t rgfw_event_type(const RGFW_event* e) { return e->type; }
int32_t rgfw_event_mouse_x(const RGFW_event* e) { return e->mouse.x; }
int32_t rgfw_event_mouse_y(const RGFW_event* e) { return e->mouse.y; }
uint8_t rgfw_event_button_value(const RGFW_event* e) { return e->button.value; }
uint8_t rgfw_event_button_state(const RGFW_event* e) { return e->button.state; }
float rgfw_event_delta_x(const RGFW_event* e) { return e->delta.x; }
float rgfw_event_delta_y(const RGFW_event* e) { return e->delta.y; }
uint32_t rgfw_event_key_value(const RGFW_event* e) { return e->key.value; }
uint8_t rgfw_event_key_state(const RGFW_event* e) { return e->key.state; }
uint8_t rgfw_event_key_mod(const RGFW_event* e) { return e->key.mod; }
uint32_t rgfw_event_keyChar_value(const RGFW_event* e) { return e->keyChar.value; }
int32_t rgfw_event_update_w(const RGFW_event* e) { return e->update.w; }
int32_t rgfw_event_update_h(const RGFW_event* e) { return e->update.h; }
size_t rgfw_event_size(void) { return sizeof(RGFW_event); }

int32_t rgfw_window_w(const RGFW_window* win) { return win->w; }
int32_t rgfw_window_h(const RGFW_window* win) { return win->h; }
