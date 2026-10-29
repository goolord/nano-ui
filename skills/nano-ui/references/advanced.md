# Custom drawing, images, text editing and windows

## Custom drawing

`customWidget` takes a `CustomWidgetSpec` (`widgetLayout`, `widgetDraw`,
`widgetCursor`, `widgetContent` key); `canvas`/`canvasConfigured` are the
helpers for most cases; `drawing` takes a pure `DrawOp` list. Inside, use
`runCanvasFor dc` so tessellation follows the display scale; `drawContext`
gives the theme, font, hover and press state. The controls in
`NanoUI.Widgets.Custom` (knobs, switches, progress bars and rings, spinners,
sparklines) are reference implementations for your own.

Vector paths come from `NanoUI.Path`, imported qualified as `P`: `P.moveTo`,
`P.lineTo`, `P.cubicTo`, `P.arc`, `P.circle`. Render with `drawPath` /
`drawPathWith` (fill rule `P.NonZero`/`P.EvenOdd`, paint `P.Solid` or a
`P.Linear` gradient) and `drawStrokePathWith` (a `P.Stroke` record: width,
caps, joins, dash). `withTransform` (translate, rotate, scale) and `withClip`.
Text: `drawText pos alignX alignY txt colour`; measure with `lineWidth` and
`uiFontMetrics`. Shapes: `drawRect`, `drawRoundedRect`, and
`drawStrokeRoundedRect` (its border is inset inside the bounds, unlike a
centred path stroke).

**Content keys.** Without `widgetContent`, a custom widget's ops are rebuilt
and diffed every frame. With a key, unchanged frames skip both the rebuild and
the repaint, but the key must cover **everything the drawing reads**, or it
draws stale pixels. `contentKey [numbers]` hashes numbers; `contentKeyOf
[keyPart a, keyPart b]` hashes anything `Hashable`. Fold in colours, text,
animation state and viewport widths.

**Moving overlays.** A drawing whose layout rect never changes (a drag ghost
under the pointer, positioned with `pinAt`) gets no layout change when it
moves. Give it a version that includes what moves it,
`drawingVersioned (contentKey [mx, my]) ...`, or it keeps the ops of the frame
it first appeared in.

Custom widgets that take keyboard input declare their keys with `widgetKeys`,
read them with `focusedKeyPressed` or `useKeyNav`, and call `useInputMethod`
each focused frame if they read `inputChars`.

## Images and SVG

- Scoped (usual): `owner <- newImageHandle` at setup, then
  `useImageRgba owner key w h pixels` each frame. The texture is freed when a
  frame ends without the call, or the key changes.
- Session-wide: `iid <- freshImageId` and `registerImageRgba iid w h bytes`
  once; `releaseImageRgba` frees it.
- `image mods iid` stretches to its bounds. `imageConfigured` adds CSS-like
  `ContentFit` (`FitContain`, `FitCover`, `FitFill`, `FitScaleDown`,
  `FitNone`), alignment, crop, zoom, opacity and rotation.
- `svgIcon`/`svgIconConfigured` render a parsed SVG (`parseSvg`, `loadSvg`)
  with the same fitting. A one-colour icon (`stroke="currentColor"`) takes the
  text colour, so one 24 px-grid icon serves every tint.
- Lazy loading: `sensorConfigured` with `sensorAnticipate` reports when a
  thumbnail is about to scroll into view.

## Text editing

`textArea` edits a `Var Text` and rejoins lines on each edit. For large
documents use `textAreaDocument` with a `Var TextDocument`, which shares
unchanged lines; convert with `documentText` only when saving. Edits are
`TextCommand`s, so the keyboard, the right-click menu and
`runTextCommand wid cmd` (`Undo`, `InsertText`, ...) share one undo history.
`textCanUndo (respId resp)` drives menu state. Writing the `Var` from outside
the widget clears its undo history.

## Window management

- `WindowSettings` in the backend options: `wsTitle`, `wsSize` (logical
  units), `wsMinSize`, mode, transparency.
- In a view: `askWindow` reads the `WindowState` and re-frames on change.
  Declarative setters (`setWindowTitleUi`, `setWindowModeUi`, ...) do nothing
  when the value is unchanged, so call them every frame. Commands
  (`resizeWindowUi`, `maximizeWindowUi`, ...) run on every call, so trigger
  them from events.
- Confirm before closing: `wsExitOnCloseRequest = False`, check
  `winCloseRequested <$> askWindow`, and call `quitUi` once confirmed.
- More native windows: `nativeWindow openVar settings body`, called every frame
  like `modal`. Each window has its own context and frames, and re-runs its
  view after any `Var` change and after each frame of the view that declared
  it, so the body may close over that view's values. `quitUi` in the body
  closes just that window.
- Pixels: `requestScreenshot`, `askScreenshot`, `useScreenshot`.
- Custom title bars (SDL): `sdlWindowDecorations = DecorationsNone` or
  `DecorationsFrame`, with `captionButtons`, `dragSpans` and `windowFrame`
  (ned builds its whole title bar this way).

## SDL specifics

- Native file dialogs never block. `askOpenFileDialog`/`askSaveFileDialog`
  return a `FileDialogId`; keep it in a `Var (Maybe FileDialogId)` made at
  setup, and `useFileDialog dialogVar onPaths` handles the result once and
  restores focus.

  ```haskell
  useFileDialog openDlg (traverse_ openFile . listToMaybe)
  whenM (button "Open...") $
    writeVar openDlg . Just =<< askOpenFileDialog defaultFileDialogOptions
  ```

- `openUrl` opens a link or file in the system handler (in `IO`). Check the
  scheme of URLs from content you do not control.
- `askSdlEnv` gives IO-level session operations (keep them on the display
  thread). Most apps never need it: `setSdlUiScale`, `setSdlUiFont` and
  `windowCaption` are safe view actions and no-ops without a session.
