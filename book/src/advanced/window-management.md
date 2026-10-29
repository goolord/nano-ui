# Desktop Windows

## A window program

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Windows.hs:settings}}
```

`runSdlApp :: SdlOptions -> NanoUI () -> IO ()` creates the window from `opts`, then runs frames until the window closes. A frame evaluates the view (`editor`) on the display thread, the OS thread that runs SDL; lays out the widgets the view declared; compares the resulting draw commands with the previous frame's; and repaints the rectangles that differ. The view is evaluated a second time in the same frame when it writes a `Var` it already read.

A frame runs when SDL delivers an input or window event, or a timer or task wakes the loop. An idle window runs none. `button :: Text -> NanoUI Bool` reads the input events SDL delivered since the previous frame and returns `True` on the frame that delivers its click. `whenM :: Monad m => m Bool -> m () -> m ()` runs the second action on that frame only.

`NanoUI` is a newtype over an `effectful` `Eff` stack that carries the frame's input, layout tree and window. Only a backend runner or `NanoUI.Testing` can run it, so state that must outlive a frame lives outside it: `main` allocates `docName :: Var Text` with `newVar` in `IO` and passes it to `editor`.

Sizes are in layout units. Physical pixels are layout units times `winScale`, which the backend reads from the display. With the default `wsPosition` (`WindowPositionDefault`) the OS places the window. `wsOpacity` (SDL only) sets the opacity of the whole window.

## Changing the window from a view

The OS window lasts for the whole run, and the view is evaluated again on every frame, so window calls differ in what a repeated call does. `editor` above uses one of each kind.

Setters store the value they last applied and call SDL only when the new value differs. A frame that skips a setter leaves the last value in place. `editor` calls `setWindowTitleUi` every frame with a title computed from `docName`, so the title changes on the same frame as each edit (`textInput` writes `docName` before `readVar` reads it): `setWindowTitleUi`, `setWindowModeUi`, `setWindowMinSizeUi`, `setWindowMaxSizeUi`, `setWindowOpacityUi`, `setWindowIconUi`.

A repeated setter call costs one `Eq` comparison. For `setWindowIconUi`, build the `RgbaPixels` once in setup and pass the same value each frame: `ByteString` equality returns at once for the same buffer, and compares every byte of a freshly decoded one.

Commands call SDL every time they are evaluated, because the user can also move, resize or maximize the window and there is no last value to compare against. Guard them with a widget result that is `True` on one frame only, such as `button`: `moveWindowUi`, `centerWindowUi`, `resizeWindowUi`, `minimizeWindowUi`, `maximizeWindowUi`, `restoreWindowUi`, `toggleMaximizedUi`.

SDL applies a command during the view, and the new window state comes back as an SDL event. `askWindow` in the same frame still returns the old size; a later frame returns the new one. `moveWindowUi` takes desktop coordinates on SDL and pixels on RGFW.

`quitUi` closes the window after the current frame is painted, and `runSdlApp` returns. Inside a `nativeWindow` body it closes only that window.

Window calls read the window from the `NanoUI` context, which exists only while the backend evaluates the view. To change the window from a worker thread, write a `Var` that the view passes to a setter with `writeVarIO`, then run the action `askWake` returns so a frame runs ([State & `Var`](../state/index.md)).

`askWindow :: NanoUI WindowState` returns the record the backend fills before the view runs: size, scale, focus, maximized and fullscreen flags, and `winCloseRequested`. The first call marks the window as read. From then on, every change to the record (focus, a move, maximizing) runs a frame that repaints the whole window instead of the differing rectangles. A view that never calls `askWindow` pays nothing.

## Confirming before close

Set `wsExitOnCloseRequest = False`. On a close request the backend leaves the window open, sets `winCloseRequested`, runs one frame, and clears the flag after it. The confirmation has to outlive that frame, so the view copies the flag into a `Var`:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Windows.hs:close_request}}
```

`useVar False` allocates the `Var` on the first frame and returns the same `Var` on later frames, matched by its call position in the view. `modal confirming title body` evaluates `body` as a modal dialog on every frame where `confirming` holds `True`.

[Build a Notepad](../tutorials/notepad.md) asks only when the document has unsaved changes.

## More windows

`nativeWindow :: Var Bool -> WindowSettings -> NanoUI () -> NanoUI ()` shows a second window while the `Var` holds `True`. `body` is an unevaluated `NanoUI ()` action; the backend evaluates it as the view of the second window's frames. Call `nativeWindow` every frame, like `modal`: the backend opens an OS window on the first frame that calls it with the `Var` set, and destroys it on the first frame that does not. The close button writes `False` to the `Var`.

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Windows.hs:native_window}}
```

- Each window has its own keyboard focus and widget state (text cursors, scroll offsets, hover). `useVar` calls in `body` are numbered from the start of `body`, so they do not shift ids in the parent view.
- `nativeWindow` reads `settings` once, when it opens the window. To change the title later, call `setWindowTitleUi` inside `body`.
- `body` is a closure over the parent view's bindings. Each open window evaluates it again after every frame of the parent view, so captured values are those of the parent's latest frame.
- Child cost scales with the parent's frame rate: a parent animating at 60 fps evaluates and lays out every open `body` 60 times a second. A child repaints only when its draw commands differ from its previous frame's.
- `body` can open more windows.
- Windows share state through `Var`s. A `Var` written in one window shows in the others on their next frame.
- With `NanoUIE` ([Elm-Style State](../state/elm.md)), `body` emits to the same reducer. To keep the open flag in your model, pass `constVar model.inspectorOpen`, set `wsExitOnCloseRequest = False`, and emit a close message when `winCloseRequested` is set.
- The SDL and RGFW backends open the window. Under `NanoUI.Testing` no window opens and `body` does not run.
- On Windows, other windows stop redrawing while one is being resized, because Win32 blocks the event loop during a resize drag.

`nano-ui-example-windows` has three windows sharing state.

## Custom title bars

`sdlWindowDecorations` removes the OS title bar. `windowCaption :: [Rect] -> NanoUI Bool` (from `nano-ui-sdl`) draws minimize, maximize, and close buttons and returns `True` on the frame close is clicked. Minimize and maximize act inside `windowCaption`.

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Windows.hs:title_bar}}
```

The rest of the bar drags the window. `windowCaption` takes the row at the caption buttons' height across the full window width, removes the rects you pass, the buttons and any floating panels (an open menu, a modal), and stores the remaining spans. SDL calls a hit-test callback with the pointer position on each press; the callback returns "drag" inside a stored span, and the OS then moves the window without delivering the press to the view. Pass the rect of every clickable widget in the bar. `button'` returns a `Response` instead of a `Bool`, and `respRect` takes its rectangle.

The rects are in layout units with the origin at the window's top-left corner. `windowCaption` multiplies them by the window's scale before storing them. The callback runs during SDL's event handling, outside any frame, against the spans the last frame stored. `respRect` returns the previous frame's rectangle, so the exclusions trail a moving widget by one frame. A frame that does not call `windowCaption` removes the spans.

`DecorationsFrame` removes the title bar but keeps the OS resize border. `DecorationsNone` removes both. On a resizable window, the outer edges resize instead of drag. `windowCaptionWith` customizes the buttons and resize border.

`windowFrame` wraps the view in a panel that fills the window, draws a rounded border `frameWidth` thick, and insets the view by `frameWidth`:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Windows.hs:window_frame}}
```

## Screenshots

`requestScreenshot :: (Maybe Screenshot -> IO ()) -> NanoUI ()` queues a callback for the frame the view is building. After the backend presents that frame to the screen, it reads the pixels back and calls the callback, with `Nothing` if the backend can't capture. The callback is `IO`, since it runs after the view has finished:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Windows.hs:request_screenshot}}
```

A capture copies the whole frame from the GPU at 4 bytes per physical pixel, about 16 MB for a 1280 by 800 window at scale 2. The callback runs on the display thread before the next frame starts, and that frame waits for it. Keep it short: write an `IORef` or `forkIO` the encoding.

`askScreenshot :: NanoUI (IO (Maybe Screenshot))` reads the current window in the view and returns an `IO` action bound to it. Run that action on a background thread: it wakes the loop and blocks until the next frame is presented. Running it on the display thread deadlocks.

`useScreenshot :: Eq k => Task k (Maybe Screenshot) -> k -> NanoUI (Maybe Screenshot)` takes one screenshot per key and returns it to the view. The `Task` holds the result across frames, so `newApp` allocates it in `IO`. On a frame with a new key `useScreenshot` starts a capture and returns the previous key's result until the capture arrives a frame or two later, as `useTask` does (see [Async Work & Animation](async.md)):

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Windows.hs:use_screenshot}}
```

`nano-ui-sdl` also has `saveScreenshot env path` (BMP) and `captureScreenshot env` (pixels), which read the last presented frame. `env` is the `SdlEnv` that `askSdlEnv` returns. Call both on the display thread. For PNG, encode with `JuicyPixels` on another thread.

## Testing

Window actions do nothing when no backend window is attached, so views that use them run under `NanoUI.Testing`. Screenshots and custom title bars need a real SDL window, as in `nano-ui-demo`'s hidden-window tests.
