# Migrating from 0.1 to 0.2

Version 0.2 brings a concrete `NanoUI` monad, `Var`-based state, an opaque
`Layout` API, typed fragment shaders, and cleaner package boundaries. This
guide lists the breaking changes encountered when porting real applications
from 0.1 to 0.2.

## Cabal dependencies and project setup

All `nano-ui*` packages now version in lockstep at `0.2.0.0` with `^>=0.2`
bounds.

```cabal
build-depends:
  , nano-ui      ^>=0.2
  , nano-ui-sdl  ^>=0.2
  -- optional, if you use NanoUI.Emit or runSdlAppReduce:
  , nano-ui-emit ^>=0.2
```

If you pin this repository from Git in a `cabal.project`, add
`packages/nano-ui-ttf` to your `subdir:` list because `nano-ui-sdl` now shares
its SDL3_ttf text layer through `nano-ui-ttf` (its `+sdl` flag defaults to
true). If you have `tests: True` globally in `cabal.project`, also include
`packages/nano-ui-shader` (or set `tests: False` on `package nano-ui-sdl`).
Remove any stale `+simd` or `-simd` flag on `nano-ui-sdl`.

## Monad and IO

Views no longer carry an `effectful` constraint.

| 0.1 | 0.2 |
| --- | --- |
| `Ui :> es => Eff es a` | `NanoUI a` |
| `uiIO action` | `liftIO action` |
| Mixing other `effectful` effects directly | Import `NanoUI.Effectful` (`NanoUIEs`, `Ui`, `embedNanoUI`, `withRunInNanoUI`) |
| `runUi` from `NanoUI` / `NanoUI.Backend` | `runUi` from `NanoUI.Effectful`, or `runNanoUI` from `NanoUI.Backend` |

## State and `Var`

Hooks (`useState`, `useInt`, `useFloat`, `useFlag`, `useText`, `useEnum`,
`useToggle`, `useTableSort`, `useContextMenu`) and `StateCell` are replaced
by `Var`. Interactive controls and overlays take a `Var` directly and write
edits back to it; primed variants return `Response`.

| 0.1 | 0.2 |
| --- | --- |
| `StateCell a`, `newState x` | `Var a`, `newVar x` |
| `(v, setV) <- useState cell` | `v <- readVar var`, `writeVar var v'` |
| `modifyState cell f` | `modifyVar var f` |
| `(t, setT) <- useText x; setT =<< textInput t` | `t <- useVar x; textInput t` |
| `useInt`, `useFloat`, `useFlag`, `useText` | `useVar` |
| `useEnum` | `useEnumVar` |
| `useToggle` | `useVar False` with `modifyVar v not` |
| `useTableSort` | `useVar (SortCol 0 SortAsc)` |
| `useContextMenu` | `popup` with a `Var (Maybe V2)` or `Var Bool` |
| Value-in, value-out control (`x' <- slider 0 1 x`) | `(_, x') <- withValue x (slider 0 1)` |
| Read-only derived value in a control | `constVar x` |
| `(r, a) <- modal open title body` | `modal openVar title body` (or `(r, a) <- modal' (constVar open) title body`) |
| `window open title body` | `floatingWindow openVar title body` (or `floatingWindow'` for `Response`) |
| `popup open anchor body` | `popup openVar anchor body` (or `popup'` for `Response`) |
| `table` returning `TableResponse` | `table'` / `tableWith'` / `tableConfigured'` |
| `useTask key action`, `useStream`, `useImageRgba`, `useDrag*` | Allocate a handle once (`newTask`, `newStream`, `newImageHandle`, `newDrag`) and pass it first |

Keyed scopes (`withKey`, `forEachVar`, `PaneGrid` panes) no longer consume a
positional sibling id, so widgets declared after a keyed list keep their
focus and state when the list changes.

## Typed emission (`nano-ui-emit`)

`NanoUI.Emit`, reducer runners (`runSdlAppReduce`, `reduceMessages`,
`reduceUpdates`, `runFrameE`), and reducer test helpers moved to the
`nano-ui-emit` package using `NanoUIE msg a` without `Typeable`.

| 0.1 | 0.2 |
| --- | --- |
| `Emit.button "Save" Save` | `Emit.emitWhen (button "Save") Save` |
| `Emit.slider 0 1 val SetVal` | `Emit.emitChanged (slider 0 1) val SetVal` |
| `Emit.textArea doc SetDoc` | `Emit.emitEdited textArea' doc SetDoc` |
| `runFrame` returning `(a, dd, dirty, msgs)` | `runFrame` returns `(a, dd, dirty)`; use `runFrameE` for typed messages |

## Module splits

Top-level `NanoUI` now focuses on view code. Specialized widgets, themes,
custom drawing, and backend helpers live in dedicated modules:

| Symbols | 0.2 module |
| --- | --- |
| Built-in themes (`tomorrowNightMinDarkTheme`, `nordTheme`, ...) | `NanoUI.Themes` |
| `paneGrid`, `GridNode`, `PaneGridConfig`, `PaneGridCtx`, `PaneDropLocation`, `paneDragHandle` | `NanoUI.Widgets.PaneGrid` |
| `customWidget`, `CustomWidgetSpec`, `defaultCustomWidgetSpec`, `DrawOp`, `KeyClaim`, `runCanvasFor`, `bindVar` | `NanoUI.Widgets.Custom` |
| `captionButtons`, `dragSpans`, `windowFrame`, `WindowFrame` | `NanoUI.Chrome` |
| `TextDocument`, `replaceDocumentRange`, `editDocument` | `NanoUI.Widgets.TextDocument` |
| `colorToWord32`, `emptyInput`, `buttonsFromList`, `inputKeysFromList`, `setThemeIO`, `withTheme`, `mix64`, `Layout (..)` | `NanoUI.Backend` |
| `shortcut`, `shortcutOnce`, `ctrl`, `shift`, `alt`, `super`, `cmdOrCtrl`, `key`, `parseShortcut` | `NanoUI.Shortcut` |
| `NanoUI.Context`, `NanoUI.Store`, `NanoUI.Layout.*`, `NanoUI.Id`, `NanoUI.Debug` | `NanoUI.Internal.*` |

`NanoUI` also exports `selectableRow`, `collapsing`, `SortKey`, and
`textCell`. If your view defines helpers with those names, hide them on the
`import NanoUI` line or switch to the built-ins.

## Layout and widget configs

`Layout` constructors and fields are no longer exported from `NanoUI`. Use
layout modifiers instead, and pass `LayoutModifier` (`Layout -> Layout`)
directly to widget config layout fields without applying `defaultLayout`.

| 0.1 | 0.2 |
| --- | --- |
| `defaultLayout { layoutPadding = Padding l r t b }` | `padLRTB l r t b` (or `padLeft`, `padRight`, `padTop`, `padBottom`, `padAll`, `padXY`) |
| `defaultLayout { layoutDir = Row }` | `direction Row` |
| `defaultLayout { layoutWidth = Grow }` | `growW` (or `fillW`) |
| `ticLayout = fillW defaultLayout` (same for `sicLayout`, `bcLayout`, `canvasLayout`, `widgetLayout`, `scLayout`, `pgLayout`) | `ticLayout = fillW` (a `LayoutModifier`, not a `Layout`) |
| `alignMid` | `alignMiddle` |
| `fixedWH w h` allowing `minW` overrides | `fixedWH w h` sets `minW`/`maxW` to `w` and `minH`/`maxH` to `h` |
| `muted` / `danger` filling row width | `muted` / `danger` no longer imply `fillW`; add `fillW` where needed |
| `scrollArea` / `scrollArea2D` / `scrollAreaConfigured` | `scrollWith'` / `scroll2DWith'` / `scrollConfigured'` (return `(a, Response)`) |
| `defaultScrollConfig` allowing 2D scroll | `defaultScrollConfig` is vertical-only; use `scroll2DConfig` for 2D |

## Widget and input renames

| 0.1 | 0.2 |
| --- | --- |
| `searchField*`, `SearchFieldConfig`, `sfc*` | `searchInput*`, `SearchInputConfig`, `sic*` |
| `boundedSelect`, `boundedRadio`, `boundedTabs*`, `boundedTabBar*` | `enumSelectBy`, `enumRadioBy`, `enumTabsBy*`, `enumTabBarBy*` |
| `PopupConfig`: `cfgPlacement`, `cfgOffset`, `cfgDismissable` | `popupPlacement`, `popupOffset`, `popupDismissable` |
| `Cell` width override `withText` | `cellWidthText` |
| `getScrollMetricsUi`, `scrollToUi`, `setScrollOffsetUi`, `scrollPagesUi`, `scrollToEndUi`, `scrollRectIntoViewUi`, `setScrollStepUi` | Same names without the `Ui` suffix (`getScrollMetrics`, `scrollTo`, ...) |
| `setWindowTitleUi`, `resizeWindowUi`, `moveWindowUi`, `maximizeWindowUi`, `quitUi`, `setUiTheme` | Same names without `Ui` (`setWindowTitle`, `resizeWindow`, `moveWindow`, `maximizeWindow`, `quit`, `setTheme`) |
| `inputMousePressed` / `inputMouseReleased` fields | `mousePressed MouseLeft` / `mouseReleased MouseLeft`, or `pressedIn` / `releasedIn` on `Input` |
| `textFieldActive` / `floatingPanelActive` shortcut guards | Use `shortcut` / `shortcutOnce` / `keyPressed`, which respect modals, `disabledWhen`, and focused widget `KeyClaim`s |
| `runCanvas` inside `CustomWidgetSpec` | `runCanvasFor dc` |
| `widgetCursor :: CustomDrawContext -> UiCursorKind` | `widgetCursor :: CustomDrawContext -> Rect -> V2 -> UiCursorKind` |
| `parseSvg :: Text -> Either Text Svg` | `parseSvg :: ByteString -> Either Text Svg` (UTF-8) |
| `Theme { ... }` record construction | Update `defaultTheme { ... }` or `defaultLightTheme { ... }` (`themePage`, `themePopup`, `themeSuccess`, `themeWarning`, `themeDanger` were added) |
| `Style { ... }` record construction | Update `defaultStyle` or an existing theme style (`styleBorderSides` was added) |
| `TextFont` positional construction | Gained a trailing `Bool` field (`textFontTabular`, pass `False` for proportional digits) |
| `foldInputKeys` / `inputKeysElem` from `NanoUI` | Import from `NanoUI.Input` |
| `nextId` from `NanoUI` | Import from `NanoUI.Monad` |
| `getScrollMetrics ctx wid` in `IO` | `getScrollMetricsIO ctx wid` from `NanoUI.Testing` / `NanoUI.Backend` |
| `tabKey` requiring `Eq` only | `tabKey` requires `Hashable` |
| `pvDragPick` in `PaneGrid` | Wrap the pane header in `paneDragHandle` |
| `useListNav` index (`Int` with `-1` sentinel) | `Maybe Int` |

## SDL backend (`nano-ui-sdl`)

Views no longer thread an `SdlEnv` handle.

| 0.1 | 0.2 |
| --- | --- |
| `runSdlAppWith setup view` | Allocate state in `IO` (`s <- setup`) and call `runSdlApp opts (view s)` |
| `sdlWindowTitle`, `sdlWindowSize`, `sdlWindowResizable`, `sdlWindowFullscreen`, `sdlWindowHidden` | `sdlWindowSettings = defaultWindowSettings { wsTitle = ..., wsSize = ..., wsResizable = ..., wsMode = ... }` |
| `sdlWindowBorderless = True` | `sdlWindowDecorations = DecorationsNone` (or `DecorationsFrame` for custom title bars with native resize edges) |
| `askOpenFileDialog env opts` returning `Maybe FileDialogId` | `askOpenFileDialog opts` returning `FileDialogId` directly (same for save/folder dialogs) |
| `setSdlUiFont env ...`, `setSdlUiScale env ...`, `askSdlDebug env` | Same functions without the `env` argument (`askSdlDebug` returns `Maybe SdlDebugSnapshot`) |

## Companion packages

- **nano-ui-diagrams:** Pass a `PlotCache` allocated once with `newPlotCache`
  to plotting functions.
- **nano-ui-form:** Pass a `FormState` allocated once with `newFormState`.
  `NanoUI.Form.Named` and `NanoUI.Form.Unnamed` merged into
  `NanoUI.Form.Input` using `FieldName`, and `label` / `separator` became
  `formLabel` / `formSeparator`.
- **nano-ui-markdown:** Use `markdownWith` for custom layout modifiers.
- **nano-ui-shader:** Renamed `select` to `selectE` to avoid clashing with
  `NanoUI.select`.
