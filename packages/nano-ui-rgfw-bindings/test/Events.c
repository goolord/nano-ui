/* RGFW's functions are defined in RGFW.c: declare them plain, not inline. */
#define RGFWDEF
#include "RGFW.h"
#include <string.h>
#ifdef _WIN32
#include <windows.h>
#endif

size_t rgfw_test_event_size(void) { return sizeof(RGFW_event); }

/* RGFW.c defines it; RGFW.h declares it only for its implementation. */
void RGFW_mouseNotifyCallback(RGFW_window *win, i32 x, i32 y, RGFW_bool status);

/* Queue the pointer entering the window at (x, y), as the platforms do. */
void rgfw_test_enter(RGFW_window *win, int x, int y) { RGFW_mouseNotifyCallback(win, x, y, RGFW_TRUE); }

/* Press the left button at (5, 5) through the window procedure, as Windows
   delivers it, then either release it at (x, y) or, when lose is set, take
   the capture away as another window would. 1 when the press captured the
   mouse and nothing holds it after, 0 when not, -1 off Windows. */
int rgfw_test_win32_drag(RGFW_window *win, int x, int y, int lose)
{
#ifdef _WIN32
    HWND hwnd = (HWND)RGFW_window_getHWND(win);
    SendMessageW(hwnd, WM_LBUTTONDOWN, MK_LBUTTON, MAKELPARAM(5, 5));
    int captured = GetCapture() == hwnd;
    if (lose)
        ReleaseCapture();
    else
        SendMessageW(hwnd, WM_LBUTTONUP, 0, MAKELPARAM(x, y));
    return captured && GetCapture() == NULL;
#else
    (void)win; (void)x; (void)y; (void)lose;
    return -1;
#endif
}

/* Write through the native types, independently of the Haskell offsets. */
void rgfw_test_event(RGFW_event *e, int kind)
{
    memset(e, 0, sizeof(*e));
    switch (kind) {
    case 0:
        e->key.type = RGFW_keyPressed;
        e->key.value = RGFW_keyHome;
        e->key.repeat = 1;
        e->key.mod = RGFW_modControl | RGFW_modShift;
        e->key.state = 1;
        break;
    case 1:
        e->keyChar.type = RGFW_keyChar;
        e->keyChar.value = 0x1f600;
        break;
    case 2:
        e->mouse.type = RGFW_mouseMotion;
        e->mouse.x = -130; e->mouse.y = 245;
        break;
    case 3:
        e->delta.type = RGFW_mouseScroll;
        e->delta.x = 1.25f; e->delta.y = -2.5f;
        break;
    case 4:
        e->button.type = RGFW_mouseButtonPressed;
        e->button.value = RGFW_mouseRight;
        break;
    case 5:
        e->update.type = RGFW_windowResized;
        e->update.w = 643; e->update.h = 481;
        break;
    case 6:
        e->scale.type = RGFW_scaleUpdated;
        e->scale.x = 1.25f; e->scale.y = 1.5f;
        break;
    }
}
