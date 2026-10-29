# Desktop Windows

## Window settings

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Windows.hs:settings}}
```

Sizes are in logical units. The backend scales them for the display. Leave `wsPosition` unset to let the OS place the window. `wsOpacity` (SDL only) sets the opacity of the whole window.

## Changing the window from a view

Setters do nothing when the value is unchanged, so call them every frame: `setWindowTitleUi`, `setWindowModeUi`, `setWindowMinSizeUi`, `setWindowMaxSizeUi`, `setWindowOpacityUi`, `setWindowIconUi`.

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Windows.hs:title}}
```

Commands run on every call, so only call them on an event: `moveWindowUi`, `centerWindowUi`, `resizeWindowUi`, `minimizeWindowUi`, `maximizeWindowUi`, `restoreWindowUi`, `toggleMaximizedUi`.

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Windows.hs:maximize}}
```

`quitUi` quits the app. Inside a `nativeWindow` body it closes only that window.

`askWindow` returns the window's size, scale, maximized and fullscreen flags, and whether a close was requested.

## Confirming before close

Set `wsExitOnCloseRequest = False`, then check `winCloseRequested`:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Windows.hs:close_request}}
```

[Build a Notepad](../tutorials/notepad.md) shows this with a confirmation modal.

## More windows

`nativeWindow open settings body` shows a second window while `open` is `True`. Call it every frame, like `modal`. Closing the window sets `open` to `False`.

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Windows.hs:native_window}}
```

- Each window has its own focus, widget state, and `useVar` slots.
- `settings` are read once, when the window opens. Change the title later with `setWindowTitleUi` inside `body`.
- `body` can use values from the surrounding view, and can open more windows.
- Windows share state through `Var`s. A `Var` written in one window updates the others.
- Each open window re-runs `body` on every `Var` write and every frame of the parent view. It only repaints if its output changed.
- With `NanoUIE`, `body` emits to the same reducer. To keep the open flag in your model, pass `constVar model.inspectorOpen`, set `wsExitOnCloseRequest = False`, and emit a close message when `winCloseRequested` is set.
- Works with SDL and RGFW. Does nothing in tests.
- On Windows, other windows stop redrawing while one is being resized.

`nano-ui-example-windows` has three windows sharing state.

## Custom title bars

Remove the OS title bar with `sdlWindowDecorations`, then draw your own. `windowCaption` (from `nano-ui-sdl`) draws minimize, maximize, and close buttons and returns `True` on the frame close is clicked. Pass it the rects of clickable widgets in the bar; the rest of the bar drags the window.

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Windows.hs:title_bar}}
```

`DecorationsFrame` removes the title bar but keeps the OS resize border. `DecorationsNone` removes both. On a resizable window, the outer edges resize instead of drag. `windowCaptionWith` customizes the buttons and resize border.

`windowFrame` draws a rounded border around the whole view without taking layout space:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Windows.hs:window_frame}}
```

## Screenshots

`requestScreenshot` passes the next frame to a callback, or `Nothing` if the backend can't capture:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Windows.hs:request_screenshot}}
```

The next frame waits for the callback, so keep it short: write an `IORef` or `forkIO` the encoding.

From a background thread, use `askScreenshot`. To take a screenshot per key in a task, use `useScreenshot`:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Windows.hs:use_screenshot}}
```

`nano-ui-sdl` also has `saveScreenshot env path` (BMP) and `captureScreenshot env` (pixels). Call both on the display thread. For PNG, encode with `JuicyPixels` on another thread.

## Testing

Window actions do nothing without a session, so views that use them run fine under `NanoUI.Testing`. Screenshots and custom title bars need a real SDL window, as in `nano-ui-demo`'s hidden-window tests.
