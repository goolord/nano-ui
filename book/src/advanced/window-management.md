# Desktop Windows

This chapter covers configuring native windows, reacting to window events, opening secondary OS windows, building frameless custom title bars, and capturing screenshots.

## Configuring and running a window

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Windows.hs:settings}}
```

`runSdlApp :: SdlOptions -> NanoUI () -> IO ()` opens a native window using `opts` and runs the event loop until the window closes. On each frame, it runs your view on the main display thread, solves layout, diffs the draw commands against the previous frame, and repaints only the changed rectangles. (If your view modifies a `Var` it already read earlier in the same pass, it runs a second pass before layout.)

Frames only run when an input event, window event, timer, animation, or background task wakes the loop. When a window sits idle, no frames run at all.

`NanoUI` wraps an `effectful` `Eff` stack carrying frame input, the layout tree, and window handles. Because only a backend runner (or `NanoUI.Testing`) executes it, allocate long-lived state like `docName` in `IO` before starting the loop and pass it into your view.

All window dimensions use logical layout units. Physical pixel dimensions are logical units multiplied by `winScale`, which the backend queries from the OS display. Leaving `wsPosition` at `WindowPositionDefault` lets the window manager choose the initial placement. On SDL, `wsOpacity` controls overall window translucency.

## Controlling the window from a view

Because the OS window persists across the entire session while your view runs every frame, window actions fall into two groups: **declarative setters** and **imperative commands**.

### Declarative setters

Setters remember the last value they sent to the OS and skip the backend call when nothing changed. You can call them unconditionally on every frame (as `editor` above does with `setWindowTitleUi`), and skipping a setter simply leaves the current value in place:

- `setWindowTitleUi`
- `setWindowModeUi`
- `setWindowMinSizeUi` / `setWindowMaxSizeUi`
- `setWindowOpacityUi`
- `setWindowIconUi`

A repeated setter call costs only an `Eq` check. For `setWindowIconUi`, build the `RgbaPixels` once at startup and reuse that same value every frame: `ByteString` equality returns immediately when both sides share the same buffer, whereas freshly decoded icons compare byte by byte.

### Imperative commands

Commands forward directly to the OS on every call, because the user can also move, resize, or maximize the window independently and there is no single desired value to cache. Always guard commands behind one-shot events such as `button`:

- `moveWindowUi` (takes desktop coordinates on SDL, physical pixels on RGFW)
- `centerWindowUi`
- `resizeWindowUi`
- `minimizeWindowUi`, `maximizeWindowUi`, `restoreWindowUi`, `toggleMaximizedUi`

Keep in mind that SDL applies commands asynchronously and reports the updated geometry back through an OS event, so `askWindow` still sees the old size during the frame the command ran and updates on the following frame.

Call `quitUi` to close the window after the current frame finishes painting (`runSdlApp` then returns). Inside a secondary `nativeWindow`, `quitUi` closes only that child window.

### Reading window state and updating from worker threads

Window operations read the active session from `NanoUI`, which is only live on the UI thread during a frame. To update a window from a background thread, write to a `Var` with `writeVarIO`, wake the loop with `askWake`, and let the view apply the setter on the next frame (see [State & `Var`](../state/index.md)).

`askWindow :: NanoUI WindowState` returns the current window metrics captured at the start of the frame: logical size, display scale, focus, maximized/fullscreen flags, and `winCloseRequested`. Calling `askWindow` opts your view into redrawing whenever window metadata changes (focus, moves, maximize toggles). Views that never call `askWindow` skip those redraws completely.

## Confirming before closing

By default, clicking the OS close button ends the session immediately. To show an "unsaved changes" prompt instead, set `wsExitOnCloseRequest = False`.

When the user requests a close, the backend keeps the window open, sets `winCloseRequested` to `True` for a single frame, and then clears it. Copy that signal into a `Var` so your confirmation modal stays open across subsequent frames:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Windows.hs:close_request}}
```

For a full example that only prompts when the buffer is dirty, see [Tutorial 2: Build a Notepad](../tutorials/notepad.md).

## Multiple native windows

`nativeWindow :: Var Bool -> WindowSettings -> NanoUI () -> NanoUI ()` opens a second OS window whenever its `Var Bool` is `True` and runs `body` as that window's view. Just like `modal`, call `nativeWindow` unconditionally on every frame. The backend creates the OS window on the first frame the flag turns `True` and destroys it when the flag turns `False` (or if a frame skips calling it). Clicking the secondary window's close button sets the `Var` back to `False`.

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Windows.hs:native_window}}
```

A few important details about secondary windows:

- **Independent focus and IDs.** Each native window owns its own keyboard focus, text carets, scroll positions, and hover state. `useVar` calls inside `body` count from zero independently of the parent window.
- **Initial settings apply once.** `nativeWindow` reads `WindowSettings` only when opening the window. To update the title or mode afterward, call setters like `setWindowTitleUi` inside `body`.
- **Closures stay fresh.** Every frame of the parent view also re-evaluates each open child `body`, so any local values captured from the parent stay up to date.
- **Watch parent animation rates.** Because child views re-evaluate when the parent renders, a parent animating at 60 fps also lays out open child windows at 60 fps (though a child only repaints pixels when its draw list actually changes).
- **Nesting and shared state.** Child windows can open further windows of their own, and all windows share state cleanly through `Var`s (a write in one window shows up in the others on their next frame).
- **Elm-style usage.** With `NanoUIE` ([Elm-Style State](../state/elm.md)), child windows emit to the same reducer. To store the open flag in your model, pass `constVar model.inspectorOpen`, set `wsExitOnCloseRequest = False`, and emit a close message when `winCloseRequested` fires.
- **Platform notes.** Both SDL and RGFW support `nativeWindow`, while headless `NanoUI.Testing` ignores it without running `body`. On Windows, Win32 blocks the message pump during a window resize drag, so sibling windows pause redrawing until the drag ends.

Run `cabal run nano-ui-example-windows` to try a three-window example with shared state.

## Custom title bars

On SDL, `sdlWindowDecorations` hides the default OS title bar so you can draw your own. Pair it with `windowCaption :: [Rect] -> NanoUI Bool` (exported from `nano-ui-sdl`), which renders minimize, maximize, and close buttons and returns `True` when the close button is clicked (minimize and maximize are handled automatically):

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Windows.hs:title_bar}}
```

Any remaining strip across the top of the window acts as a native drag handle. Specifically, `windowCaption` starts with the full width of the caption row, subtracts the caption buttons, any open floating panels (menus or modals), and the interactive exclusion rectangles you pass in (`[respRect menuResp]` above), and registers the leftover spans with SDL's hit-test callback. When the user drags one of those spans, the OS moves the window natively without sending a press to the view.

Pass the `respRect` of every clickable control sitting in your custom title bar. `windowCaption` scales those logical rectangles by `winScale` automatically. Note that because `respRect` reflects the previous frame's layout, exclusion zones trail a moving widget by one frame, and any frame that omits `windowCaption` clears the drag spans.

Choose between two decoration modes:

- `DecorationsFrame` hides the title bar while preserving the native OS resize border.
- `DecorationsNone` removes both the title bar and OS border (on resizable windows, the outer edges still act as resize borders).

Use `windowCaptionWith` to customize button appearance or resize border thickness. `windowFrame` wraps your view in a full-window panel with a rounded border `frameWidth` thick and insets the view by `frameWidth`:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Windows.hs:window_frame}}
```

## Screenshots

`nano-ui` gives you three ways to capture rendered frames, depending on where you handle the pixels.

### Fire-and-forget callback (`requestScreenshot`)

`requestScreenshot :: (Maybe Screenshot -> IO ()) -> NanoUI ()` schedules a pixel readback for the frame currently being built. Right after presenting the frame to the screen, the backend copies the pixels and invokes your callback (passing `Nothing` on headless or unsupported backends):

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Windows.hs:request_screenshot}}
```

Reading back a full GPU frame copies 4 bytes per physical pixel (around 16 MB for a 1280x800 window at 2x scale). Because the callback runs on the display thread before the next frame begins, keep it brief: stash the pixels in an `IORef` or `forkIO` the image encoding.

### Background capture (`askScreenshot`)

`askScreenshot :: NanoUI (IO (Maybe Screenshot))` returns a blocking `IO` action bound to the current window. Call that action from a **background thread**: it wakes the loop and waits until the next frame is presented. Never invoke it on the display thread, or the loop will deadlock waiting for itself.

### Declarative capture (`useScreenshot`)

`useScreenshot :: Eq k => Task k (Maybe Screenshot) -> k -> NanoUI (Maybe Screenshot)` captures one screenshot per key and hands the result back inside your view, using a `Task` allocated during setup. When the key changes, it kicks off a capture and returns the previous screenshot until the new one finishes a frame or two later:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Windows.hs:use_screenshot}}
```

If you are working directly with `SdlEnv` (from `askSdlEnv`), `nano-ui-sdl` also exposes `saveScreenshot env path` (writes a BMP file) and `captureScreenshot env` (returns raw pixels) for the last presented frame. Call both on the display thread, and encode formats like PNG with `JuicyPixels` on a worker thread.

## Testing views that use window APIs

Window setters and commands become no-ops when no OS window is attached, so views that call them still run cleanly under headless `NanoUI.Testing`. Testing screenshots or custom title bars requires a real SDL window (see the hidden-window tests in `nano-ui-demo`).
