#include "RGFW.h"

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

float rgfw_event_scale_x(const RGFW_event* e) { return e->scale.x; }
float rgfw_event_scale_y(const RGFW_event* e) { return e->scale.y; }

uint8_t rgfw_window_set_mouse_standard(RGFW_window* win, uint8_t icon) {
    if (!win) return 0;
    return (uint8_t)RGFW_window_setMouseStandard(win, (RGFW_mouseIcon)icon);
}

uint8_t rgfw_window_set_mouse_default(RGFW_window* win) {
    if (!win) return 0;
    return (uint8_t)RGFW_window_setMouseDefault(win);
}

