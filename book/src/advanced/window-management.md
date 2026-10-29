# Desktop Windows

The core library has window settings and view actions; `nano-ui-sdl` adds custom chrome and native screenshots.

## Window settings

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Windows.hs:settings}}
```

Sizes are logical units, not pixels; the backend applies DPI scaling. `wsPosition` defaults to the desktop's placement. `wsOpacity` (SDL) sets whole-window opacity.

## Declarative and imperative actions

Inside the view:

- **Declarative setters** (`setWindowTitleUi`, `setWindowModeUi`, `setWindowMinSizeUi`, `setWindowMaxSizeUi`, `setWindowOpacityUi`, `setWindowIconUi`) are no-ops when the value has not changed. Call them every frame:

  ```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Windows.hs:title}}
```

- **Imperative actions** (`moveWindowUi`, `centerWindowUi`, `resizeWindowUi`, `minimizeWindowUi`, `maximizeWindowUi`, `restoreWindowUi`, `toggleMaximizedUi`) run on every call. Guard them with an event:

  ```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Windows.hs:maximize}}
```

`quitUi` ends the session; there is no separate "close window" command. In a window opened with `nativeWindow` (below), it closes that window instead.

## Close requests

By default the window closes as soon as the platform asks it to. To confirm unsaved work, set `wsExitOnCloseRequest = False` and handle the request in the view:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Windows.hs:close_request}}
```

`askWindow` reports the window's current state, including size, scale, maximized/fullscreen flags, and the close request. [Build a Notepad](../tutorials/notepad.md) wires this to a confirmation modal.

## More windows

`nativeWindow` opens another native window, with its own view, while a `Var Bool` holds `True`. Like `modal`, call it every frame, and the window's close button writes `False`:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Windows.hs:native_window}}
```

The new window has its own focus, widget state, `useVar` slots and frames. It starts with the declaring view's theme; the settings take effect when it opens, so change its title or theme later from its own view. A window's view can open further windows the same way.

The body runs in the new window's own frames, and after each frame of the declaring view, so it can use that view's values directly. Windows also share state through `Var`s: when a `Var` changes, every open window runs its view again on its next frame, so typing in one window shows in another.

In an Elm-style view (`NanoUIE`), the body emits to the same reducer as the rest of the view. To keep a window's open state in the model, pass `constVar model.inspectorOpen`, set `wsExitOnCloseRequest = False` in its settings, and emit a close message from the body when `winCloseRequested <$> askWindow` is true.

An app that never calls `nativeWindow` pays nothing for it. With several windows open, a `Var` write or a frame of the declaring view costs each window one run of its view (with no repaint if nothing it shows changed), and every window keeps its own glyph and image textures. The SDL and RGFW backends open these windows; in tests the call does nothing. On Windows, a window being resized does not redraw the others until the drag ends. `nano-ui-example-windows` shows three windows sharing state.

## Custom title bars

For a frameless window, set the decorations and draw the bar yourself. `windowCaption` from `nano-ui-sdl` draws the minimize, maximize, and close buttons, registers the drag region, and reports a close click:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Windows.hs:title_bar}}
```

`windowCaption` takes the rectangles of interactive widgets in the bar (menu buttons, tabs, search fields); the spans between them become draggable. The returned `Bool` is true on the frame the close button is clicked. `windowCaptionWith` customizes the buttons and resize border.

`DecorationsFrame` keeps the platform's resize frame while removing the title bar; `DecorationsNone` removes both. On a resizable frameless window, edge resizing takes precedence over the drag region along the outer edges.

An optional rounded border around the whole view comes from `windowFrame`:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Windows.hs:window_frame}}
```

It draws a border and insets the view; the border does not consume layout space.

## Screenshots

`requestScreenshot` asks the backend for the next presented frame and delivers it to a callback:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Windows.hs:request_screenshot}}
```

The callback runs in `IO` after the frame is presented, and the next frame waits on it, so keep it short: an `IORef` write, or a `forkIO` to encode. Each request is answered once, with `Nothing` when the backend cannot capture.

For thread-safe capture use `askScreenshot`, which returns an `IO` action for a background thread, or `useScreenshot` for a per-key screenshot owned by a task:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Windows.hs:use_screenshot}}
```

The `nano-ui-sdl` backend also has `saveScreenshot env path` (a BMP of the current frame) and `captureScreenshot env` (pixels), both on the display thread. For PNG encoding, use `JuicyPixels` or a similar library in a background thread.

## Testing window behavior

Window operations are no-ops without a session, so views that set titles or read `askWindow` run under `NanoUI.Testing`. Native screenshots and chrome need a real SDL session, as in `nano-ui-demo`'s hidden-window tests.
