/* The desktop's own frame under a window that draws its own chrome.
 *
 * A borderless window on Windows is a popup: it has no frame, so there is
 * nothing outside its edges for the pointer to find and the application has
 * to spend its own chrome on saying where it can be resized. Answering
 * WM_NCCALCSIZE with the sizing frame left outside the client area puts the
 * frame back where it always is: invisible, outside the window you can see,
 * and what the desktop resizes the window by. Nothing is drawn in it and
 * nothing of the view gives way to it -- the window is made that much larger
 * instead.
 *
 * The window procedure is here rather than in Haskell because Windows calls
 * it from inside SetWindowPos, DefWindowProc and the modal loop a border
 * drag runs in, and a Haskell callback reached that way re-enters the
 * runtime from a foreign call that is already running.
 *
 * Only Windows has any of this; the Haskell side does not call in elsewhere.
 */

#ifdef _WIN32

#include <windows.h>

/* The procedure this one stands in front of, kept on the window itself so
 * that more than one window can have a frame at a time. */
static const WCHAR *NANO_UI_OLD_PROC = L"NanoUIOldWndProc";

/* How far the frame reaches beyond the window you can see, across and down.
 * The per-monitor calls are Windows 10 and later, so they are looked up
 * rather than linked against; the desktop-wide ones stand in for them. */
static void nano_ui_frame_metrics(HWND hwnd, int *across, int *down)
{
    typedef UINT(WINAPI * dpi_for_window_fn)(HWND);
    typedef int(WINAPI * metrics_for_dpi_fn)(int, UINT);
    static dpi_for_window_fn dpi_for_window = NULL;
    static metrics_for_dpi_fn metrics_for_dpi = NULL;
    static int looked_up = 0;

    if (!looked_up) {
        HMODULE user32 = GetModuleHandleW(L"user32.dll");
        if (user32) {
            dpi_for_window = (dpi_for_window_fn)(void *)GetProcAddress(user32, "GetDpiForWindow");
            metrics_for_dpi = (metrics_for_dpi_fn)(void *)GetProcAddress(user32, "GetSystemMetricsForDpi");
        }
        looked_up = 1;
    }

    if (dpi_for_window && metrics_for_dpi) {
        UINT dpi = dpi_for_window(hwnd);
        if (dpi == 0) {
            dpi = 96;
        }
        *across = metrics_for_dpi(SM_CXFRAME, dpi) + metrics_for_dpi(SM_CXPADDEDBORDER, dpi);
        *down = metrics_for_dpi(SM_CYFRAME, dpi) + metrics_for_dpi(SM_CXPADDEDBORDER, dpi);
    } else {
        *across = GetSystemMetrics(SM_CXFRAME) + GetSystemMetrics(SM_CXPADDEDBORDER);
        *down = GetSystemMetrics(SM_CYFRAME) + GetSystemMetrics(SM_CXPADDEDBORDER);
    }
}

/* Whether the window has a sizing frame at all. SDL gives a borderless
 * window one only while it is resizable and not fullscreen, so this is also
 * what leaves a fullscreen window covering the whole of its screen: there the
 * frame is not asked for and SDL answers for the client area itself. */
static int nano_ui_has_sizing_frame(HWND hwnd)
{
    return (GetWindowLongPtrW(hwnd, GWL_STYLE) & WS_THICKFRAME) != 0;
}

static LRESULT CALLBACK nano_ui_frame_proc(HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam)
{
    WNDPROC old = (WNDPROC)GetPropW(hwnd, NANO_UI_OLD_PROC);

    /* Where the client area goes: the sizing frame's width in on every side,
     * and nothing else. The desktop would take a caption's height off the
     * top as well, which is a title bar this window draws for itself and has
     * no use for; SDL, at the other extreme, answers this one itself for a
     * borderless window and says the client covers the whole of it, frame
     * and all, which leaves nothing outside the window to take hold of.
     *
     * Three sides and not four: the sides and the bottom of the frame are
     * left unpainted by the desktop, and the view shows through them to
     * whatever is behind the window, but the top of one is a caption, and a
     * caption is painted whether the window has a title bar in it or not. So
     * the top of the client stays where the top of the window is, and the
     * strip that would have been a caption is the view's.
     *
     * A maximized window is placed with its frame off the edges of the
     * screen, so for that one case the top comes back inside, or the top of
     * the view would go off the screen with it. */
    if (msg == WM_NCCALCSIZE && wparam == TRUE && nano_ui_has_sizing_frame(hwnd)) {
        NCCALCSIZE_PARAMS *params = (NCCALCSIZE_PARAMS *)lparam;
        int across = 0;
        int down = 0;
        nano_ui_frame_metrics(hwnd, &across, &down);
        params->rgrc[0].left += across;
        params->rgrc[0].right -= across;
        params->rgrc[0].bottom -= down;
        if (IsZoomed(hwnd)) {
            params->rgrc[0].top += down;
        }
        return 0;
    }

    /* The window is going: give it back the procedure it had and take the
     * property off, which the desktop asks of whoever put it there. */
    if (msg == WM_NCDESTROY && old) {
        SetWindowLongPtrW(hwnd, GWLP_WNDPROC, (LONG_PTR)old);
        RemovePropW(hwnd, NANO_UI_OLD_PROC);
    }

    if (old) {
        return CallWindowProcW(old, hwnd, msg, wparam, lparam);
    }
    return DefWindowProcW(hwnd, msg, wparam, lparam);
}

/* Grow the window by the frame, or shrink it back, and have the desktop work
 * the frame out again either way, which is what SWP_FRAMECHANGED asks for.
 * A window with no sizing frame is given none by the procedure above, so it
 * keeps its size and only has the frame worked out again. */
static void nano_ui_refit(HWND hwnd, int sign)
{
    RECT r;
    int across = 0;
    int down = 0;
    if (!GetWindowRect(hwnd, &r)) {
        return;
    }
    if (nano_ui_has_sizing_frame(hwnd) && !IsZoomed(hwnd)) {
        nano_ui_frame_metrics(hwnd, &across, &down);
    }
    SetWindowPos(
        hwnd,
        NULL,
        0,
        0,
        (r.right - r.left) + sign * 2 * across,
        (r.bottom - r.top) + sign * down,
        SWP_NOMOVE | SWP_NOZORDER | SWP_NOACTIVATE | SWP_FRAMECHANGED);
}

void nano_ui_set_native_frame(void *window, int on)
{
    HWND hwnd = (HWND)window;
    WNDPROC old;
    if (!hwnd) {
        return;
    }
    old = (WNDPROC)GetPropW(hwnd, NANO_UI_OLD_PROC);
    if (on && !old) {
        old = (WNDPROC)(LONG_PTR)SetWindowLongPtrW(hwnd, GWLP_WNDPROC, (LONG_PTR)nano_ui_frame_proc);
        if (!old) {
            return;
        }
        SetPropW(hwnd, NANO_UI_OLD_PROC, (HANDLE)old);
        nano_ui_refit(hwnd, 1);
    } else if (!on && old) {
        SetWindowLongPtrW(hwnd, GWLP_WNDPROC, (LONG_PTR)old);
        RemovePropW(hwnd, NANO_UI_OLD_PROC);
        nano_ui_refit(hwnd, -1);
    }
}

/* How much larger than its view the frame makes the window right now, in
 * window coordinates: the frame twice across and once down, or nothing for a
 * window without it, or one maximized or fullscreen, where it is off the
 * screen or not there. SDL sizes a borderless window as though its client
 * area were the whole of it, so whatever asks SDL for a size adds this. */
void nano_ui_native_frame_outset(void *window, int *across, int *down)
{
    HWND hwnd = (HWND)window;
    *across = 0;
    *down = 0;
    if (!hwnd || !GetPropW(hwnd, NANO_UI_OLD_PROC) || !nano_ui_has_sizing_frame(hwnd) || IsZoomed(hwnd)) {
        return;
    }
    nano_ui_frame_metrics(hwnd, across, down);
    *across *= 2;
}

#endif
