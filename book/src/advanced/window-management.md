# Window Chrome & Screenshots

`nano-ui` provides high-level declarative and imperative controls for desktop windows.

## Window Settings

Configure startup window properties in `WindowSettings`:

```haskell
opts = defaultSdlOptions
  { sdlWindowSettings = defaultWindowSettings
      { wsTitle       = "Main Window"
      , wsSize        = Size 1280 800
      , wsMinSize     = Just (Size 640 480)
      , wsMode        = Normal  -- Maximized, Fullscreen, Hidden
      , wsTransparent = False
      }
  }
```

## Declarative vs Imperative Window APIs

Inside the view:
- **Declarative Setters** (`setWindowTitleUi`, `setWindowModeUi`): No-ops when the value has not changed. Safe to call unconditionally every frame.
- **Imperative Actions** (`resizeWindowUi`, `maximizeWindowUi`, `closeWindowUi`): Run on every call. Always guard behind an event:
  ```haskell
  whenM (button "Maximize") maximizeWindowUi
  ```

---

## Custom Title Bars & Window Chrome

For modern frameless desktop apps (like VS Code or `ned`):
1. Set `sdlWindowDecorations = DecorationsNone` or `DecorationsFrame` in `SdlOptions`.
2. Render your own title bar in the view with `windowFrame`, `captionButtons`, and `dragSpans`:

```haskell
customTitleBar :: NanoUI ()
customTitleBar = rowWith (fillW . fixedH 32 . alignMid) $ do
  -- Draggable area for moving the window
  dragSpans $ label "My Application"
  spacer
  captionButtons  -- Minimise, Maximise, Close
```

---

## Taking Screenshots

Capture window contents programmatically:

```haskell
whenM (button "Take Screenshot") $ do
  requestScreenshot
```

Consume the resulting raw pixels with `useScreenshot` or save them to disk with `saveScreenshot`.
