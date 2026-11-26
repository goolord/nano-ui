/* Win32 window subclass for live-resize smoothness and optional native sizing
 * frame on windows that draw their own chrome.
 *
 * Every Windows window installs this procedure so WM_WINDOWPOSCHANGING sets
 * SWP_NOCOPYBITS on size changes (stopping Windows from BitBlt-shifting stale
 * client pixels to the new top-left corner during border drags), WM_ERASEBKGND
 * suppresses GDI background flashes, and WM_ENTERSIZEMOVE / WM_EXITSIZEMOVE
 * track active modal resize drags so retained textures do not shrink mid-drag.
 *
 * When DecorationsFrame is enabled on a borderless resizable window, the same
 * procedure answers WM_NCCALCSIZE with the sizing frame left outside the client
 * area, restoring the invisible outer resize border and DWM drop shadow without
 * shrinking the client area (the outer window rectangle is expanded instead).
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

/* Properties stored on each subclassed HWND so multiple windows work
 * independently: the previous WndProc, whether DecorationsFrame WM_NCCALCSIZE
 * is active, and whether Win32's modal size/move loop is currently running. */
static const WCHAR *NANO_UI_OLD_PROC = L"NanoUIOldWndProc";
static const WCHAR *NANO_UI_NATIVE_FRAME = L"NanoUINativeFrame";
static const WCHAR *NANO_UI_IN_SIZEMOVE = L"NanoUIInSizeMove";

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
 * window WS_THICKFRAME only while it is resizable and not fullscreen, so
 * fullscreen windows skip the custom WM_NCCALCSIZE adjustment and cover the
 * entire screen. */
static int nano_ui_has_sizing_frame(HWND hwnd)
{
    return (GetWindowLongPtrW(hwnd, GWL_STYLE) & WS_THICKFRAME) != 0;
}

static LRESULT CALLBACK nano_ui_frame_proc(HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam)
{
    WNDPROC old = (WNDPROC)GetPropW(hwnd, NANO_UI_OLD_PROC);

    /* Suppress GDI background erasure during resizes and exposes: nano-ui
     * paints the entire client area itself. */
    if (msg == WM_ERASEBKGND) {
        return 1;
    }

    if (msg == WM_ENTERSIZEMOVE) {
        SetPropW(hwnd, NANO_UI_IN_SIZEMOVE, (HANDLE)(INT_PTR)1);
    } else if (msg == WM_EXITSIZEMOVE) {
        RemovePropW(hwnd, NANO_UI_IN_SIZEMOVE);
    }

    /* Stop Windows from BitBlt-copying stale client bits when a resize also
     * moves the window origin (dragging the left or top edge), which shifts
     * old pixels across the client area before the new frame swaps. Resizes
     * that keep the top-left corner fixed (SWP_NOMOVE, right/bottom drags)
     * keep existing top-left pixels intact without flashing. */
    if (msg == WM_WINDOWPOSCHANGING && lparam) {
        LRESULT res = old ? CallWindowProcW(old, hwnd, msg, wparam, lparam)
                          : DefWindowProcW(hwnd, msg, wparam, lparam);
        WINDOWPOS *wp = (WINDOWPOS *)lparam;
        if (!(wp->flags & SWP_NOSIZE) && !(wp->flags & SWP_NOMOVE)) {
            RECT cur;
            if (!GetWindowRect(hwnd, &cur) || wp->x != cur.left || wp->y != cur.top) {
                wp->flags |= SWP_NOCOPYBITS;
            }
        }
        return res;
    }

    /* Inset the client area by the sizing frame on the left, right, and bottom
     * only when DecorationsFrame is active. The default handler also insets
     * the top by the caption height, and Windows paints that top strip even
     * when the window has no title bar; SDL's borderless handler instead makes
     * the client area cover the entire window rectangle, leaving no outer
     * resize border. Leaving the top edge at the top of the window rectangle
     * gives the top strip to the client area while keeping native resize
     * borders on the other three sides.
     *
     * A maximized window positions its sizing frame outside the monitor bounds,
     * so in that case the top must also be inset by the frame height to keep
     * the client area on screen. */
    if (msg == WM_NCCALCSIZE && wparam == TRUE
        && GetPropW(hwnd, NANO_UI_NATIVE_FRAME)
        && nano_ui_has_sizing_frame(hwnd)) {
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

    /* Restore the previous window procedure and remove window properties
     * before destruction. */
    if (msg == WM_NCDESTROY && old) {
        SetWindowLongPtrW(hwnd, GWLP_WNDPROC, (LONG_PTR)old);
        RemovePropW(hwnd, NANO_UI_OLD_PROC);
        RemovePropW(hwnd, NANO_UI_NATIVE_FRAME);
        RemovePropW(hwnd, NANO_UI_IN_SIZEMOVE);
    }

    if (old) {
        return CallWindowProcW(old, hwnd, msg, wparam, lparam);
    }
    return DefWindowProcW(hwnd, msg, wparam, lparam);
}

static int nano_ui_ensure_subclass(HWND hwnd)
{
    if (!hwnd) {
        return 0;
    }
    if (GetPropW(hwnd, NANO_UI_OLD_PROC)) {
        return 1;
    }
    WNDPROC old = (WNDPROC)(LONG_PTR)SetWindowLongPtrW(hwnd, GWLP_WNDPROC, (LONG_PTR)nano_ui_frame_proc);
    if (!old) {
        return 0;
    }
    SetPropW(hwnd, NANO_UI_OLD_PROC, (HANDLE)old);
    return 1;
}

void nano_ui_install_win32_resize_hook(void *window)
{
    nano_ui_ensure_subclass((HWND)window);
}

int nano_ui_window_in_sizemove(void *window)
{
    HWND hwnd = (HWND)window;
    return (hwnd && GetPropW(hwnd, NANO_UI_IN_SIZEMOVE) != NULL) ? 1 : 0;
}

/* Expand or shrink the outer window rectangle by the frame thickness and
 * trigger a frame recalculation via SWP_FRAMECHANGED. */
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
    int was_on;
    if (!hwnd) {
        return;
    }
    was_on = GetPropW(hwnd, NANO_UI_NATIVE_FRAME) != NULL;
    if (on && !was_on) {
        if (!nano_ui_ensure_subclass(hwnd)) {
            return;
        }
        SetPropW(hwnd, NANO_UI_NATIVE_FRAME, (HANDLE)(INT_PTR)1);
        nano_ui_refit(hwnd, 1);
    } else if (!on && was_on) {
        RemovePropW(hwnd, NANO_UI_NATIVE_FRAME);
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
    if (!hwnd || !GetPropW(hwnd, NANO_UI_NATIVE_FRAME) || !nano_ui_has_sizing_frame(hwnd) || IsZoomed(hwnd)) {
        return;
    }
    nano_ui_frame_metrics(hwnd, across, down);
    *across *= 2;
}

#endif
