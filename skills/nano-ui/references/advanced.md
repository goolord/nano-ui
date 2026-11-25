# Custom drawing, images, text editing, and windows

## Custom drawing

Use `customWidget` with a `CustomWidgetSpec` (`widgetLayout`, `widgetDraw`, `widgetCursor`, `widgetContent`) for full control, `canvas` / `canvasConfigured` for most drawings, or `drawing` for a pure `DrawOp` list. Inside `widgetDraw`, call `runCanvasFor dc` so curve tessellation adapts to display scale; `drawContext` supplies the active theme, font metrics, and hover/press state. The six controls in `NanoUI.Widgets.Custom` (`knob`, `toggleSwitch`, `progressBar`, `circularProgress`, `spinner`, `sparkline`) serve as clean reference implementations.

Build vector paths with `NanoUI.Path` (imported qualified as `P`): `P.moveTo`, `P.lineTo`, `P.cubicTo`, `P.arc`, `P.circle`. Fill with `drawPath` / `drawPathWith` (`P.NonZero` or `P.EvenOdd`, painted with `P.Solid` or `P.Linear` gradients) and stroke with `drawStrokePathWith` (`P.Stroke` record for width, caps, joins, and dash patterns). Apply `withTransform` (translate, rotate, scale) and `withClip` to scope transforms and clips. Draw text with `drawText pos alignX alignY txt color`, measuring runs in the view with `lineWidthUi` and `uiFontMetrics` (pure `lineWidth` is only exact for text already shaped). Basic shape helpers include `drawRect`, `drawRoundedRect`, and `drawStrokeRoundedRect` (whose stroke is inset inside the bounds, unlike a centered path stroke).

**Content keys.** Without `widgetContent`, custom widgets rebuild and diff their draw ops every frame. Adding a key skips both rebuild and repaint on unchanged frames, provided the key includes **everything the drawing reads**. Hash numbers with `contentKey [xs]` and arbitrary `Hashable` values with `contentKeyOf [keyPart a, keyPart b]`, folding in colors, text, animation progress, and scroll viewport widths.

**Moving overlays.** A drawing whose layout rectangle stays fixed (such as a drag ghost moved with `pinAt`) receives no layout diff when it moves. Version it explicitly with `drawingVersioned (contentKey [mx, my]) ...` so it does not stay stuck on frame 1.

Custom widgets that accept keyboard input declare their claimed keys via `widgetKeys`, read them with `focusedKeyPressed` or `useKeyNav`, and call `useInputMethod` on every focused frame if they read `inputChars`.

## Images and SVG

- **Scoped lifetimes (preferred):** Allocate `owner <- newImageHandle` during setup and call `useImageRgba owner key w h pixels` inside the view. The atlas slot frees automatically when the key changes or a frame completes without calling `useImageRgba`.
- **Session-wide images:** Allocate an ID with `freshImageId`, upload once via `registerImageRgba iid w h bytes`, and free manually with `releaseImageRgba`.
- **Layout and fitting:** `image mods iid` stretches to its bounds. `imageConfigured` supports CSS-style `ContentFit` (`FitContain`, `FitCover`, `FitFill`, `FitScaleDown`, `FitNone`), alignment, sub-rectangle crop, zoom, opacity, and rotation.
- **SVG icons:** Parse once at setup with `parseSvg` or `loadSvg` and render via `svgIcon` / `svgIconConfigured`. Single-color SVGs using `stroke="currentColor"` or `fill="currentColor"` inherit the current text color, so one 24 px icon adapts cleanly to any theme or tone.
- **Lazy loading:** Wrap thumbnails in `sensorConfigured` with `sensorAnticipate` to load images shortly before they scroll into view.

## Multi-line text editing

`textArea` edits a `Var Text` by rejoining lines on every edit (fine for short text). For files and code buffers, use `textAreaDocument` with a `Var TextDocument`, which shares unchanged lines across edits; flatten to `Text` with `documentText` only when saving. Keyboard edits, right-click menus, and programmatic `runTextCommand wid cmd` calls (`Undo`, `InsertText`, and so on) all share a single undo stack, and `textCanUndo (respId resp)` drives Edit menu state. Overwriting the `Var` directly clears the undo history.

## Window management

- Configure initial windows with `WindowSettings` (`wsTitle`, logical `wsSize`, `wsMinSize`, window mode, transparency).
- Inside a view, `askWindow` reads live `WindowState` and triggers redraws whenever window metadata changes. Declarative setters (`setWindowTitleUi`, `setWindowModeUi`, and so on) are no-ops when unchanged and safe to call every frame; imperative commands (`resizeWindowUi`, `maximizeWindowUi`, and so on) run every time and belong behind events.
- **Close confirmation:** Set `wsExitOnCloseRequest = False`, watch `winCloseRequested <$> askWindow` (true for one frame only), copy it into a `Var` to open a confirmation `modal`, and call `quitUi` once confirmed.
- **Multiple windows:** Call `nativeWindow openVar settings body` unconditionally every frame like `modal`. Each window has independent focus and widget state, re-evaluates its `body` after `Var` changes and parent frames (so closures over parent bindings stay fresh), and closes only itself on `quitUi`.
- **Screenshots:** `requestScreenshot` (callback after present), `askScreenshot` (blocking `IO` trigger for worker threads), and `useScreenshot` (keyed `Task` hook).
- **Custom title bars (SDL):** Set `sdlWindowDecorations = DecorationsNone` or `DecorationsFrame`, and pair with `windowCaption` (or `captionButtons`, `dragSpans`, and `windowFrame`).

## SDL specifics

- **Non-blocking native file dialogs:** `askOpenFileDialog` and `askSaveFileDialog` return a `FileDialogId` immediately. Keep it in a `Var (Maybe FileDialogId)` allocated at setup, and poll it with `useFileDialog dialogVar onPaths` (which delivers results once and restores focus):

  ```haskell
  useFileDialog openDlg (traverse_ openFile . listToMaybe)
  whenM (button "Open...") $
    writeVar openDlg . Just =<< askOpenFileDialog defaultFileDialogOptions
  ```

- **External links:** `openUrl` opens a URL or path via the OS default handler (in `IO`). Always validate the URL scheme when opening untrusted content.
- **Low-level session access:** `askSdlEnv` returns the `SdlEnv` needed for direct `IO` operations (always call on the display thread). Most apps never need it because `setSdlUiScale`, `setSdlUiFont`, and `windowCaption` work directly as safe view actions.

## Fragment shaders

Use `shaderBox` / `shaderConfigured` (from `NanoUI`) for Tier 1 leaf surface fills (zero offscreen targets) and `withShader` for Tier 2 subtree effects (pooled offscreen targets, interactive children preserved). Define custom shaders at top level using `nano-ui-shader` (`import NanoUI.Shader`) so compilation happens once:

- **Uniforms:** Derive `Generic` and `Uniforms` on a product record (`Float`, `Int32`, `Word32`, `Bool`, `V2`, `V3`, `V4`, `Color`, `Rect`, `Size`, tuples). Packing uses std140 alignment and drives damage diffing automatically (unchanged uniforms produce zero damage).
- **EDSL authoring:** Enable `DataKinds` and `OverloadedRecordDot` to write `surfaceShader`, `colorEffect`, `distortionEffect`, or `layerEffect` (and `*M` variants with `bind` and bounded `forRange` loops). Project uniform fields (`u.color.rgb`) and vector swizzles (`ctx.uv.yx`) directly with dot syntax; vector-scalar operators are `(*^)`, `(^*)`, `(/^)`, `(+^)`, `(-^)`.
- **Composition:** Fuse compatible effects into one GPU pass with `composeColor`, `composeDistort`, and `distortThenColor`, or chain multi-pass effects with `chainLayer`.
- **Built-in library:** `meshGradient4`, `radialGlow`, `sdfRoundedBox`, `colorWheelHsv`, `noiseGrain`, `checkerboard`, `groupOpacity` (also exports SVG `<g opacity="...">`), `grayscale`, `tint`, `brightnessContrast`, `colorMatrix`, `vignette`, `scanlines`, `waveDistort`, `rippleDistort`, `pixelate`, `boxBlur`, `gaussianBlur`, `dropShadow`, `outerGlow`, `chromaticAberration`.
- **Fallback:** Query live support via `askShaderCapabilities`. Without GPU shader support (such as SDL's software renderer), `shaderConfigured` draws `scFallback` and `withShader` renders its children directly. On SDL 3.4+, `RenderDriverAuto` selects `"gpu"` automatically when available.
