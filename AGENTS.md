# AGENTS.md

Guidance for AI coding agents working with **nano-ui**, an immediate-mode GUI
toolkit for Haskell. This file teaches the mental model, API conventions,
styling system, and the pitfalls that trip up code written against it —
including a section of recurring bugs and their fixes, mined from the git
histories of real applications built on nano-ui: `ned` (a vim-like editor),
`stronghead` (a profiling debugger), and `winget-gui` (a Windows package
manager front end).

## What nano-ui is

A view is a `NanoUI ()` action the backend re-runs every frame. Widgets return
values directly (`button` returns `Bool`), input widgets take a mutable `Var`
holding their state, and layout is a composable modifier argument. You write
UIs with ordinary functions, `do` notation, and `if`/`case`/`for_`.

```haskell
counter :: NanoUI ()
counter = row $ do
  n <- useVar (0 :: Int)
  whenM (button "-") (modifyVar n (subtract 1))
  label . T.pack . show =<< readVar n
  whenM (button "+") (modifyVar n (+ 1))
```

Monorepo layout (this repository):

| Package | What it is |
| --- | --- |
| `packages/nano-ui` | Core: widgets, layout, input, theming, draw list. `NanoUI.hs` is the full public export list |
| `packages/nano-ui-sdl` | SDL3 backend: TrueType fonts, installed-font lookup, native file dialogs, custom title bars |
| `packages/nano-ui-rgfw` | RGFW + OpenGL 3.2 backend with a bundled bitmap font (no system deps) |
| `packages/nano-ui-emit` | Elm-style typed messages: `NanoUIE msg a` views + reducer runners |
| `packages/nano-ui-diagrams` | Line/bar/scatter/area charts (`NanoUI.Plot`) and diagrams-lib drawing |
| `packages/nano-ui-form` | Validated applicative forms on ditto |
| `packages/nano-ui-markdown` | Markdown rendering, incrementally appendable for streamed chat replies |
| `packages/nano-ui-svg` | `DrawOp`-to-SVG export (headless screenshots of any view) |
| `packages/nano-ui-demo` | Example apps + single-topic tutorials |

`nano-ui` is on Hackage, but the current source may be ahead of the released API and some companion packages are unpublished. Consumers that need the current revision pin it as a Cabal `source-repository-package`
(github.com/goolord/nano-ui) with `subdir: packages/nano-ui packages/nano-ui-emit packages/nano-ui-sdl ...`
and `package nano-ui-sdl flags: +sdl`. Cabal fetches the pinned source into
`dist-newstyle/src/nano-ui-<hash>/`; read library source or the export list
there when working in a consumer repo.

Requires GHC 9.10–9.14 (dev uses 9.14), `default-language: GHC2024`,
`OverloadedStrings` for text literals, and `-threaded` on executables.
The SDL backend needs SDL3 + SDL3_ttf 3.2+ + pkg-config (`nix develop` in this
repo provides them; on Windows install them through MSYS2 UCRT64 and point
`PKG_CONFIG_PATH` at its `ucrt64\lib\pkgconfig` — `C:\msys64\ucrt64\lib\pkgconfig`
on a default install). Build this repository
with `cabal build -j1 all`.

## The frame model (read this first)

Each frame: the UI action runs (appending layout nodes, reading input, reading
or writing `Var`s) → layout is solved → pointer/keyboard/focus resolve against
the new geometry → damage is computed against the previous frame → the damaged
region is painted.

Consequences that shape everything else:

- **The view action can run more than once per frame.** Writing a `Var` that
  was already read earlier in the pass re-runs the action once with one-shot
  input events cleared (clicks/keypresses don't fire twice). So: never allocate
  per-frame resources inside the view (`liftIO (newVar x)` inside a view is a
  bug — it allocates every pass), and always guard side effects behind event
  checks: `whenM (button "Save") saveDocument`.
- **`readVar` placement matters.** Reading a `Var` *below* the input widget
  that edits it keeps typing to a single pass; reading it *above* costs a
  second pass on change. Only widgets bound to the `Var` (no `readVar`
  anywhere) repaint just that widget; any `readVar` makes the write a
  full-window repaint.
- **Geometry lags one frame.** Layout is solved after the view runs, so
  `respRect` returns the *previous* frame's rectangle (empty on first
  appearance). Warm up a frame before targeting widgets by rect in tests.
- **Idle costs nothing** — the loop blocks on events unless animation, a timed
  wake, or input is pending. Don't force continuous redraws.

## Widget API conventions

Naming scheme, applied consistently across ~every widget:

| Form | Meaning |
| --- | --- |
| `button "Save"` | Base: returns `Bool` (activated) or `()` |
| `button'` | Primed: also/instead returns a `Response` |
| `buttonWith (fixedW 120)` | Takes a `Layout -> Layout` modifier first |
| `buttonConfigured cfg` | Takes a config record (`ButtonConfig` etc., always has a `default...Config`) |

Verified signatures of the core widgets:

```haskell
button   :: Text -> NanoUI Bool
label    :: Text -> NanoUI ()
heading  :: Text -> NanoUI ()
checkbox :: Text -> Var Bool -> NanoUI ()
slider   :: Float -> Float -> Var Float -> NanoUI ()   -- min max
textInput :: Var Text -> NanoUI ()
select   :: Foldable f => f Text -> Var Int -> NanoUI ()
menuItem :: Text -> NanoUI Bool
modal    :: Var Bool -> Text -> NanoUI () -> NanoUI () -- open state, title, body
popup    :: Var Bool -> PopupConfig -> NanoUI () -> NanoUI ()
table    :: Foldable f => Text -> Colonnade Headed row Text -> f row -> Var SortCol -> NanoUI ()
tabs     :: (Foldable f, Hashable a) => Var a -> f (Tab a (NanoUI ())) -> NanoUI ()
tree     :: Foldable f => Text -> f TreeItem -> Var Int -> NanoUI ()
```

**Input widgets take the `Var` they display and mutate.** No callback, no
on-change handler: `textInput query`, `slider 0 100 settings.volume`. To react
to edits, use the primed variant's `Response`.

`Response` fields and helpers: `respId` (the widget's `WidgetId`),
`respRect`, `respHovered`, `respPressed` (held down, not clicked),
`respClicked` (activated: left click or Enter), `respChanged` (value edited),
`respSubmitted` (committed, e.g. Enter in a text field), `respHeldWith` /
`respClickedWith` (which `MouseButton`: left/right/middle/back/...),
`respRightPressed` / `respRightClicked`. `searchInput'` debounces
`respChanged` (fires per keystroke on the `Var`, but `respChanged` only after
a pause) — trigger expensive work on it.

Inventory (all re-exported from `NanoUI`): containers (`row`, `column`,
`grid`, `panel`, `card`, `callout`, `toolbar`, `center`, `responsive`,
`scroll`, `scroll2D`, `layers`, `mouseArea`, `separator`, `spacer`), text
(`label`, `heading`, `muted`, `mono`, `danger`, `kv`, `selectableText`),
buttons and menus (`button`, `iconButton`, `buttonContent`, `menuButton`,
`menuItem`, `menuSeparator`, `contextMenu`), inputs (`checkbox`,
`toggleButton`, `radio`/`enumRadio`, `select`/`enumSelect`/`boundedSelect`,
`slider`, `textInput`, `searchInput`, `numericInput`, `comboBox`,
`textArea`, `textAreaDocument`, `colorPicker`), tabs (`tabs`, `tabBar`,
`closableTab`, enum/bounded variants), `tree`, `table`/`simpleTable`
(Colonnade-based, sortable via `Var SortCol`), overlays (`modal`, `window`,
`popup`, `tooltip`, `contextMenu`), pane grids (`paneGrid`), rich text
(`NanoUI.Widgets.RichText`), images and SVG icons, drag and drop (`useDrag`,
`useDrop`, `dropZone`, `useReorder`, `insertionIndex`), and canvas-drawn
reference widgets in `NanoUI.Widgets.Custom` (switches, knobs, progress bars,
spinners, sparklines — which serve as reference implementations for custom
widgets users may want to implement themselves to accompany the canvas API).

## State: `Var`

Typed mutable references; no `Dynamic`, no runtime type lookup.

- `useVar initial` — positional, allocated inside `NanoUI`, zero setup. Only
  `Int`, `Double`, `Float`, `Bool`, `Text`, `SortCol` (`useEnumVar` for any
  `Enum`). Initial value used only on first init; reset later with `writeVar`.
- `newVar initial` — in `IO`, any type. Allocate during setup, store in an app
  record, pass into the view. Passing the same `Var` to two widgets shares
  state.
- `VarMap` + `varAt` / `dropVar` / `retainVars` — one `Var` per key for
  dynamic collections (chat logs, rows that come and go).
- `constVar x` — read-only `Var` (writes are no-ops): display a derived value
  in an input and handle edits via the primed `Response`.
- `withValue v widget` — run a `Var` widget against a plain value, get both
  result and final value back.
- Record fields: with `Generic` + `OverloadedRecordDot`, `settings.volume` is
  a `Var Float` projecting one field (tracked independently of
  `settings.title`, so editing one doesn't repaint what reads the other).
  Custom projections: `zoomVar` (getter/setter) or `zoomVarL` (lens).
- Lists: `forEachVar key list body` gives per-element `Var`s that track their
  keys across reorders; `withElemVars` when you filter/sort yourself.
- `readVar` / `writeVar` / `modifyVar` in the view (`modifyVar` composes
  multiple same-frame updates; equal writes are no-ops). `peekVar` reads
  without recording a dependency. From `IO`/threads: `readVarIO`,
  `writeVarIO`, `modifyVarIO`, then call the `askWake` callback to schedule a
  frame.

Elm-style alternative: `nano-ui-emit`'s `NanoUIE msg a`, a `NanoUI` action
with the view's message sink in scope as the implicit parameter `?emit`. Core
widgets run in it unchanged (`NanoUI.Emit` re-exports `NanoUI`); emit with
`emit`, `emitWhen (button "Save") Save`, `emitChanged (slider 0 100) v SetV`,
and `mapMessages` for child components. Give views a `Model -> NanoUIE Msg ()`
signature. Run with `runSdlAppReduce options update model view`.

## Stable widget identity

Widget IDs count up in call order among siblings; internal state (caret,
selection, open dropdown) is keyed by them.

- Keep call order consistent across frames.
- Wrap **conditional subtrees** in `scope` — it reserves one slot whether or
  not its body runs, so later siblings keep their IDs. (`useVar` has its own
  counter, so conditional *widgets* alone don't shift `useVar` slots, but
  conditional `useVar` calls still need `scope`.)
- Wrap **dynamic/reorderable list items** in `withKey` with a stable,
  sibling-unique key — never the list index.
- Task/stream/image calls do **not** consume widget IDs; calling them
  conditionally is safe for identity.
- Hidden content keeps its state. For ever-growing keyed collections, keep row
  state in a `VarMap` and `retainVars` what's gone.

## Layout

Logical pixels, origin top-left, `y` downward. Backends handle DPI scaling
(custom widgets work in logical coordinates).

Modifiers are `Layout -> Layout` functions composed with `(.)`; when two set
the same property, **the leftmost wins**:

```haskell
columnWith (gap 8 . padAll 12 . fillW) $ do
  label "Settings"
  buttonWith (fixedW 120) "Apply"
```

Common modifiers: `padAll`/`padXY`/`padLeft`.../`padLRTB`, `gap`, `fillW`,
`fillH`, `grow`, `minW`/`maxW`/`fixedW`, `minH`/`maxH`/`fixedH`/`fixedWH`,
`percent 50`, `alignMid`/`alignStart`/`alignEnd`/`alignCenter`/`alignTop`/
`alignBottom`/`alignBaseline`, `tight` (zero padding, keeps gap), `wrap`
(wrap onto lines; add `lineGap`, `lineAlign`), `layered`, `pinAt x y`
(absolutely position over siblings — `pinAt 0 0 . grow` covers the parent for
scrims), `aspect r`, `gridCols`. (Font size and friends are text style, not
layout.)

- `layers` stacks children (later on top); each aligns within the shared box.
- Scroll containers need a bounded viewport: `scrollWith (fixedH 240 . fillW) body`.
  `scrollArea` returns the scroller's `WidgetId` for programmatic control.
- Virtualize huge lists: `sid <- currentId` before the container, then
  `getScrollMetricsUi sid` for offset/viewport, build only visible rows
  between spacer nodes, and `scrollRectIntoViewUi sid rect ...` to keep the
  selection in view (see `nano-ui-example-long-list`, 100k rows).
- Overlapping nodes: interactive widgets get pointer events, panels/labels/
  containers don't. Override with `pointer PointerBlock` (a scrim that eats
  clicks) or `pointer PointerPass` (decorative drawing that doesn't).
- `mouseArea` wraps a subtree and reports hover/clicks for the whole region
  (unless an interactive child handled it) — the tool for list rows and cards.
- A widget's natural size comes from its content, so text that changes length
  moves its siblings unless its width is pinned. Give changing labels
  (`fmtInt` counts, timers, byte totals) a `fixedW` sized to their widest
  value; inside a scrolling list, make cells `fillW` so a long value cannot
  widen the list itself. See "Recurring bugs" below for both fixes as they
  were actually made.

## Theming and styling

A `Theme` holds a `Style` per surface kind (`themeButton`, `themeInput`,
`themePanel`, `themeFloatingWindow`, `themePopup`) plus semantic colors
(`themeAccent`, `themeMuted`, `themeSelection`, `themeFocusRing`,
`themeWindow`, `themeSeparator`, success/warning/danger, etc.). A `Style` is
bg/fg/border/borderWidth/cornerRadius/hoverBg/activeBg.

The page behind a view is `pageBackground`: `themeWindow` by default, unless
`themePage` is set. `pageColor` sets one; `panelPage` gives a view that draws
no panel the panel surface. The page is laid over the window background, so
`themeWindow` remains the window; a page that uses it shows it in the gaps
between panels. A full-window scroller (`scrollWith grow`) fills the page
colour.

- Built-ins: `defaultTheme` (dark), `defaultLightTheme`, Tomorrow variants,
  or `themeFromBase16`/`themeFromBase16Dark`/`themeFromBase16Light` from any
  Base16 palette.
- Set once at startup: `sdlAppTheme = Just appTheme` in `SdlOptions`, or
  per-frame with `setUiTheme`. Follow the OS:
  `setUiTheme . lightDark defaultLightTheme defaultTheme =<< systemAppearance`
  or `followSystemTheme ctx`.
- Scope a modification: `styled (Theme -> Theme) subtree` — modifiers compose
  with `(.)`, nested scopes adapt to the active base theme. Replace outright
  with `themed theme subtree`. Theme scopes affect painting only, never
  layout metrics.
- Scope modifiers: style-level (`background`, `foreground`, `borderColor`,
  `borderWidth`, `borderSides`, `cornerRadius`, `hoverBackground`,
  `pressBackground`, `fillColor`), theme-level (`buttonStyle`, `inputStyle`,
  `panelStyle`, `everyStyle`, `accentColor`, `textColor`, `pageColor`,
  `panelPage`, `rounded`, `subtle`, `tinted`, `readableOn`), semantic
  (`primary`, `destructive`, `success`, `warning`, `tone Danger`,
  `toneColor Warning` for custom drawing). Text style modifiers on any text
  widget: `fontMono`, `fontTone Warning`, `fontSize`, `fontWeight`,
  `fontItalic`, `fontUnderline`.
- `disabledWhen cond subtree` disables interaction and dims rendering while
  preserving layout and state.
- `uiTheme` reads the active theme in the view (for custom drawing); `drawContext`
  supplies it inside canvas code.

The winget-gui pattern for a fully custom look: define a `Palette` record of
literals, build a complete `Theme` from it, then derive scoped helpers —
`quiet = subtle . buttonStyle (foreground muted)` for borderless buttons, and
`surface bg border radius = styled (panelStyle (background bg . ...)) . panelWith`
for chrome panels. Keep 3 surface levels (well/shelf/controls) and reserve
color for meaning.

Fonts: SDL backend defaults to the bundled Inter; select installed families
with `sdlAppFont = FontSearch ["Segoe UI"]`, `sdlAppMonoFont = FontSearch
["Cascadia Mono"]`, `sdlAppFontSize` (and `setSdlUiFont` mid-session). On
Windows the density is 1; `sdlAppUiScale = 0` follows the desktop scaling.

## Input, keyboard, focus

- Chords from `NanoUI.Shortcut`: `shortcut (ctrl <> key 's')`,
  `shortcutOnce (cmdOrCtrl <> shift <> key 'p')` (once = ignore key repeat),
  `parseShortcut "C-s"` for user config. Only the first matching `shortcut`
  call per frame fires.
- Suppression is automatic and correct: shortcuts and key queries are blocked
  behind open modals, inside `disabledWhen`, and when the focused widget
  claims the key (typing in a text field, arrows on a slider). Modified chords
  (Ctrl+Enter, Alt+Left) still pass through. `menuItemShortcut` binds only
  while its menu is open — add a top-level `shortcut` too for closed-menu
  binding.
- Focus: clicking focuses fields/selects; Tab/Shift+Tab cycle. Programmatic:
  `requestFocus (respId resp)` (or `currentId` just before the call),
  `focusNext`, `clearFocus`, `isFocused`. Focus takes effect at end of frame.
- Focus-dependent decisions need care about *when* they're made: the node
  arena is reset before the view runs, so asking "is a text field active?"
  at the top of the view always answers no — read such state at the **end**
  of the view, after the widgets have run (see "Recurring bugs").
- Keys on `Input`: `inputKeys` (pressed incl. repeats), `inputKeysNew`
  (initial presses), `inputKeysHeld`, `inputKeysReleased`, `inputChars`
  (committed text). Predicates: `pressedIn`, `pressedOnceIn`, `heldIn`,
  `releasedIn` work on keys and mouse buttons.
- Mouse in scope: `mousePressed`/`mouseHeld`/`mouseReleased` (suppressed
  behind modals/disabled). `mousePressed MouseBack` = back navigation.
- IME: built-in text fields handle composition. Any custom widget reading
  `inputChars` must call `useInputMethod` each focused frame (see the
  terminal demo).
- Cursors: built-ins set their own; `withCursorShape UiCursorCrosshair subtree`
  for regions, `widgetCursor` per custom widget.

## Overlays

`modal`, `window`, and `popup` take a `Var Bool` for open state; Escape,
outside-click, and the close button write `False`. **Call them unconditionally
every frame** (inside `when` shifts sibling IDs) — they draw nothing while
closed:

```haskell
confirm <- useVar False
whenM (button "Delete...") (writeVar confirm True)
modal confirm "Delete the file?" $
  whenM (button "Delete") (deleteFile >> writeVar confirm False)
```

For derived open state or dialogs that must not auto-dismiss, pass `constVar`
and use the primed variants (`modal'`, `window'`, `popup'`) + `respClicked`.
Tooltips are stateless: `tooltip resp "text"` or `withTooltip`, with
`tooltipConfigured` for delay/placement.

## Background work

- `task <- newTask` in setup, then `useTaskStatus task key action` in the view
  (`TaskRunning`/`TaskDone`/`TaskFailed`) or `useTask task key action`
  (`Maybe a`, keeps the previous result while a new key runs so lists don't
  flash empty). A task runs once per key, restarts when the key changes
  (cancel via async exception — use `bracket` inside for resources), and is
  **cancelled if the call is skipped for a full frame**: keep `useTask` calls
  outside tabs/conditionals when work should continue while hidden. Add a
  retry counter to the key to retrigger.
- `stream <- newStream` + `useStream stream key initial producer` for pushes
  (sensors, download progress, token streams); updates between frames
  coalesce. Fold items into the state to accumulate a list.
- External threads outside these handles: write state, then call the callback
  from `askWake`. Never touch `Context` or damage from another thread.
- Compile with `-threaded`.

## Animation

`animateTo`/`animateToA` tween (`Tween` + `Ease`) or spring (`Spring`,
`presetBouncy`/`presetSmooth`/`presetStiff`) a value toward a target with
mid-flight retargeting. `animate`/`pulse` for continuous oscillation. Call
`keepAnimating` **inside the animating widget** (per-frame, only while on
screen). Timers/clocks/debounces: call `wakeAfter` on every frame that still
needs a future wake (pending wakes reset each frame). Never force redraws by
dirtying the context every frame — that's an unpaced busy loop.

## Custom drawing

`customWidget` (a `CustomWidgetSpec` with `widgetLayout`, `widgetDraw`,
`widgetCursor`, `widgetContent` key) or the `canvas`/`canvasConfigured`
helpers; `drawing` for pure `DrawOp` lists. Inside, use `runCanvasFor dc` so
tessellation adapts to display scale; `drawContext` gives theme, font,
hover, and press state. Reference widgets in `NanoUI.Widgets.Custom` (knobs,
switches, progress rings/bars, spinners, sparklines) accompany this canvas
API as reference implementations for users building their own custom controls.

Vector paths via `NanoUI.Path` imported qualified as `P`: `P.moveTo`,
`P.lineTo`, `P.cubicTo`, `P.arc`, `P.circle`; render with `drawPath` /
`drawPathWith` (fill rule `P.NonZero`/`P.EvenOdd`, paint `P.Solid` or
`P.Linear` gradient) and `drawStrokePathWith` (a `P.Stroke` record: width,
caps, joins, dash). `withTransform` (translate/rotate/scale) and `withClip`.
Text: `drawText pos alignX alignY txt color`; measure with `lineWidth`/
`uiFontMetrics`. Shapes: `drawRoundedRect`, `drawStrokeRoundedRect` (border
inset inside bounds, unlike centred path strokes).

**Content keys**: without `widgetContent`, a custom widget's ops are rebuilt
and diffed every frame. With a key, unchanged frames skip rebuild *and*
repaint — but the key must cover **everything the drawing reads** (a stale
key draws stale pixels). `contentKey [numbers]` hashes numbers; fold in
colors and text yourself (`mix64` from `NanoUI.Backend`, as winget-gui's
`drawKey` does).

**Moving overlays**: a drawing whose layout rect never changes (a drag ghost
pinned under the pointer, `pinAt`-positioned at the cursor) gets no layout
change when it moves, so give it an explicit version that includes what
moves it — `drawingVersioned (contentKey [round mx, round my, ...]) ...` —
or it keeps the ops of the frame it first appeared in (ned's "tab ghost").

## Images and SVG

- Scoped (usual): `owner <- newImageHandle` in setup, then `useImageRgba
  owner key w h pixels` each frame — texture freed when a frame ends without
  the call, or the key changes.
- Session-static: `iid <- freshImageId` + `registerImageRgba iid w h bytes`
  once; `releaseImageRgba` to free.
- `image` stretches to bounds; `imageConfigured` adds CSS-like `ContentFit`
  (`FitContain`, `FitCover`, `FitFill`, `FitScaleDown`, `FitNone`),
  alignment, crop, zoom, opacity, rotation. `svgIcon`/`svgIconConfigured`
  render parsed SVGs (`parseSvg`) with the same fitting, tintable via
  `fontColor`. One stroked 24px-grid icon raster serves every tint
  (`stroke="currentColor"`), as in winget-gui's `icon` helper.

## Text editing

`textArea` works on a `Var Text` (rejoins lines per edit). For large
documents use `textAreaDocument` with a `Var TextDocument` (shares unchanged
lines); convert with `documentText` only when saving. Edits flow through
`TextCommand`s, so keyboard, the right-click menu, and `runTextCommand wid
cmd` (Undo, `InsertText`, ...) share one undo history; `textCanUndo (respId
resp)` for menu state; writing the `Var` from outside clears undo.

## Window management

`WindowSettings` in options (`wsTitle`, `wsSize` in logical units, `wsMinSize`,
mode, transparency...). In the view: `askWindow` (auto re-frame on change);
declarative setters (`setWindowTitleUi`, `setWindowModeUi`, ...) are no-ops
when unchanged — call them unconditionally; imperative commands
(`resizeWindowUi`, `maximizeWindowUi`, ...) run per call — trigger from
events. `wsExitOnCloseRequest = False` + `winCloseRequested` + `quitUi` for
close confirmation. More native windows: `nativeWindow open settings body`
(SDL only), called every frame like `modal`; each window has its own
context and frames, shares state through `Var`s (any `Var` change reruns
every window's view), and `quitUi` in its body closes just that window. `requestScreenshot`/`askScreenshot`/`useScreenshot` for
pixels. Custom title bars: `sdlWindowDecorations = DecorationsNone`/
`DecorationsFrame` + `captionButtons`, `dragSpans`, `windowFrame` (ned builds
its whole own title bar this way).

## SDL specifics

Native file dialogs are non-blocking: `askOpenFileDialog`/`askSaveFileDialog`
return a `FileDialogId` to keep in a `Var (Maybe FileDialogId)`;
`pollFileDialogUi` consumes the finished result once and restores focus;
`useFileDialog` wraps the whole dance. `askSdlEnv` for IO-level session ops
(keep them on the display thread). `openUrl` opens links. High-level session
ops (`setSdlUiScale`, `setSdlUiFont`, `windowCaption`) are safe view actions
and no-ops without a session.

## Application structure (patterns that work)

**Setup once in IO, then run** — everything frame-lived is either a `Var` or
inside the view:

```haskell
main :: IO ()
main = do
  cache <- newViewCache                    -- winget-gui: a record of newVar calls + IORef caches
  env <- newEnv                           -- app state outside nano-ui's knowledge
  runSdlApp sdlOptions {sdlWindowSettings = ..., sdlAppTheme = Just appTheme} (appView env cache)
```

- **winget-gui**: `Env { envState :: IORef AppState, envWake :: IORef (IO ()) }`.
  The view installs `askWake`'s callback into `envWake` on its first frame;
  worker threads `modifyState` (atomic IORef write) then `notify` (the wake).
  Each frame gathers every read into a `Frame` record (`prepareFrame`) before
  building the window top-to-bottom, with keyboard shortcuts last. Derived
  collections are memoized in `IORef (Maybe (key, value))` caches keyed by a
  revision counter.
- **stronghead**: a large `App` record of `Var`s and state components; panel
  modules each own a state type; a command palette and floating panes.
- **ned**: state in a plain `IORef App` the toolkit knows nothing of; the
  frame re-reads after each part that may change it and calls `requestFrame`
  when what was drawn no longer matches state.
- Elm-style (`nano-ui-emit`) when a pure model + update fits better.

Headless modes are cheap to add: winget-gui runs the real view against real
data with `--screenshot path` / `--selftest dir` (SDL in a hidden window);
stronghead records demo videos by driving the real view with scripted input.

## Testing

`NanoUI.Testing.Sim` is the default for anything a user does: `runSim ctx
failed input view script` settles the view, then `click "Save"` (string
literals aim at visible text; `At p`, `Within rect`, `On resp`, `OnWidget
wid` otherwise), `typeText`, `pressKey`, `chord`, `wheel`, `drag`, `settle`,
`expect "text"`, `checkEq`. In this repo's integration suite, `simSpec name w
h view script` wraps it; import `NanoUI.Testing.Sim` explicitly (its short
names clash with locals under `-Wname-shadowing`). Merge checks that share a
setup into one script rather than adding a spec per assertion.

`NanoUI.Testing`: `newContext` + `runFrame ctx size dt input view` → result,
`DrawData`, and whether a follow-up frame was requested; `runFrameE` for emit
views. Harness (`NanoUI.Testing.Harness`): `warmup2`, `clickPair` /
`clickPairWith MouseMiddle ...`, `runClick`, `held var widget` (widget + new
value), `lastFramePasses`, span lookup (`spanRect`, `spanRectOf`). Rules:
warm up before targeting `respRect`; set `tooltipDelay = 0`; use
`newWakeSignal` for deterministic task wake-ups; `cancelTasks` in teardown.
Headless tests cover layout/state/input — run the native suites and demos too
when touching rendering, fonts, dialogs, or DPI.

## Recurring bugs and their fixes

Bugs that were actually made and fixed in ned, stronghead, and winget-gui.
Check here before debugging a similar symptom; the patterns repeat.

### Layout shifting and jitter

- **A label whose text changes length moves its siblings every frame.** A
  marker count going 9 → 10 → 100, a timer, a byte total: each redraw
  re-flows the row. Fix: pin the label to the width of its widest value —
  stronghead's `paneMetaWith (fixedW 32) (fmtInt nMarkers)` and its
  `readoutDeltaWith` subtitles ("Fix markers count to fixed width",
  "Fix collections subtitle to fixed width").
- **A long value inside a scrolling list widens the whole list**, which then
  scrolls sideways, and the horizontal bar comes and goes as wide rows are
  built and dropped. Fix: the cell fills its column instead of contributing
  natural width, and the text is ellipsized — winget-gui:
  ```haskell
  -- In a row, a label too narrow for its text cuts it off rather than wrapping.
  nameLine f = rowWith (fillW . tight) . labelWith (fillW . f)
  ```
- **A drop-down clips its longest option.** A fixed guess for the width
  breaks when options are long ("Unknown disk · 113 apps · 9.24 GB"). Fix:
  measure the options with the font and add insets and the arrow:
  `fixedW (maximum (0 : map (lineWidth fm) options) + 56)`.
- **Content-derived grid sizes recompute per frame and jitter.** Lock the
  geometry (ned keeps its tree/editor column widths in saved layout state,
  "grid size locking"; winget-gui keys row-height constants, not text).
- `flex` inside a row still works, but prefer making the one growable child
  `fillW` directly (stronghead replaced `fixedW 90` + `flex` around the
  filter box with `fillW`, which also removed dead space).

### Double borders and boxes-in-boxes

nano-ui fills padded containers with the panel colour, and `scrollArea`,
`panel`, and `card` each draw their own background, border, and radius.
Nesting them inside popups, dialogs, and panes draws a box inside a box:

- A `scrollArea` in a popup showed a bordered well inside the dialog.
  stronghead's fix: give the popup's layout `padAll 0`, move the padding onto
  the inner `columnWith (fillW . padAll 12 ...)`, and use a borderless
  scroller so only one level owns chrome ("Remove duplicate borders from
  workspace popovers and notifications"). The same applies to dialogs and
  floating windows: winget-gui sets `themeFloatingWindow`/`themePopup` to
  the panel surface precisely so a padded body doesn't show a panel box
  inside the dialog.
- To strip chrome from an inner surface:
  `styled (inputStyle (background colorTransparent . borderWidth 0)) ...`.
- Between two chrome regions, draw one `separator` at the seam in the parent
  — not a border on the bottom of one and the top of the other.

### Text metrics and the pixel grid

- **`fmAdvance` per glyph × count ≠ a shaped run's width.** A shaping backend
  lays a run out by unrounded advances (8 vs 8.25 px per glyph at size 15),
  so per-glyph arithmetic drifts a cell every ~32 characters and long runs
  overdraw what follows. Fix: measure the whole run —
  `lineWidth fm run` over its length (ned, "text stays on the cell grid
  along a long run").
- **Rows must be a whole number of device pixels tall.** At a fractional
  scale (125%) rows of 22.4 px stack 22, 23, 22, 23 apart and text shimmers.
  Snap positions with the font's snap scale, as winget-gui's `rowButton`
  does:
  ```haskell
  snap v = let k = max 1 (fmSnapScale f) in fromIntegral (round (v * k) :: Int) / k
  ```
- **Box-centred is not optically centred.** A two-line block of a big label
  over a small one centres by line boxes, which leave more room under the
  smaller line — it reads ~4 px high (measured from screenshots; fixed with
  a 4 px `padT` nudge in winget-gui). For exact optical centring, centre on
  the ink: winget-gui's `rowButton` measures the ink span and the
  capital/x-height midpoints from `FontMetrics` glyphs.
- **`alignMid` on a row positions the row itself** in its parent; centering
  the row's *contents* takes the modifier on each child. The same for a
  header band: centre each child, or the row's own padding dominates what
  the eye measures ("Eliminate header dead space", "balance the status
  line").

### Cached drawings that don't update

- **A keyed custom widget freezes on an input the key missed.** winget-gui's
  sort arrow stayed on the old column until the key covered everything the
  drawing reads ("key custom widgets on their content"). If a keyed drawing
  shows stale pixels, the key is missing an input — not the widget broken.
- **A pinned overlay whose rect never changes keeps its first frame's ops.**
  A drag ghost following the pointer never triggers a layout change, so an
  unversioned drawing caches the frame it appeared in. Version it by what
  moves it (`drawingVersioned`, ned's "tab ghost").
- **Widths derived from the scroll viewport belong in the key too**: ned's
  finder rows clamp to `rectW (scrollViewport metrics)` and key on it, so
  the drawing follows the viewport rather than going stale when the
  scrollbar appears.

### Scroll details

- **Content can run under the scrollbar.** The scrollbar keeps a lane out of
  the viewport; a wide row drawn to the widget's full width runs beneath the
  bar. Clamp content and hit targets to the viewport width, but keep the
  backdrop filling the widget or its edge shows as a seam down the lane
  (ned, "hold the finder's rows out of its scrollbar's lane").
- **Tune the wheel step to the row height.** The default notch (~60 px)
  jumps ~1 row of 54 px or 3 rows of smaller ones; winget-gui sets
  `setScrollTuning` so a notch is exactly one row, eased.

### Input ordering and focus

- **Ask focus questions at the end of the view.** The node arena is reset
  before the view runs, so a focus query at the top of `appView` always
  answered "no": winget-gui's Ctrl+A selected every app while the search box
  was being typed into, and Delete opened the uninstall dialog. The fix
  moved the guards below the widgets. Keyboard shortcuts that need to know
  what has focus belong after the widgets have run.
- **Escape is not claimed by text inputs**, so Escape shortcuts must keep
  working while a field is focused. Don't share one "a field is editing"
  guard between Ctrl+A, Delete, *and* Escape — winget-gui's Escape could
  never clear the selection once the search box had focus until it got its
  own condition.
- **A focused widget that reads typed text must call `useInputMethod` every
  focused frame** — even when it isn't a text field. ned's file tree takes
  vim's j/k as typed text; only the editor asked, so the keys never reached
  the tree ("the file tree asks the window for typed text while vim's keys
  are on").
- **Keep a drag's anchor from its press**, not by re-deriving it from the
  selection each frame: a one-character selection's anchor sits on its
  left, so leftward drags carried the anchor along and selected one
  character (ned, "slow leftward drags").

### SVG export (nano-ui-svg)

- Several canvas drawings in one SVG frame emit colliding `clip_`/`grad_`
  element IDs — scope them per drawing (stronghead prefixes each
  `<g><defs>` block with an index; "Scope canvas clip and gradient IDs
  across multiple drawings").
- Right-aligned numerals lose their anchor: `text-anchor`/`textLength` may
  need post-processing, and tabular numbers help readouts
  (`font-variant-numeric: tabular-nums` at the root).

### Testing custom-drawn widgets

A custom-drawn widget has no text spans, so span-based assertions see
nothing. Click it and screenshot, then assert on the image — winget-gui's
self-test clicks a column header and checks the screenshot for the sort
arrow.

## Pitfalls checklist

Quick one-liners; the section above has the full story and real fixes.

- `liftIO (newVar ...)` inside a view — allocates every pass. Move to setup.
- Unguarded side effects in views — the view re-runs. Wrap in `whenM (button ...)`.
- Conditional widgets/`useVar`s without `scope`; keyed lists using indices.
- Overlays called conditionally instead of unconditionally with a `Var Bool`.
- `useTask` inside a tab/branch — cancelled while hidden.
- `keepAnimating` from an always-run top level — busy loop. Call it in the
  animating widget. Timers via `wakeAfter`, re-requested each frame.
- Writing a freshly computed `NaN` to a `Var` after reading it — continuous
  redraw (`NaN /= NaN`). Guard the write.
- Custom widget content key missing an input (animation state, color, text,
  position of a moving overlay, scroll viewport width) — stale pixels or a
  frozen drawing. Include *everything* the drawing reads.
- A changing label without `fixedW` — its siblings shift every frame; a wide
  cell in a scrolling list without `fillW` — the list scrolls sideways.
- A `scrollArea`/`panel`/`card` nested in a popup or dialog — a box inside
  the box. One level owns chrome; strip or flatten the rest.
- Focus queries at the top of the view — the arena was reset; they always
  say no. Read them at the end, after the widgets ran.
- Escape behind the same guard as Ctrl+A/Delete — text inputs don't claim
  Escape, so it must keep working while a field is focused.
- A focused widget reading `inputChars` without `useInputMethod` — typed
  text never reaches it.
- Per-glyph advance × count for a long run — drifts off the shaped width;
  measure the run with `lineWidth`.
- Parsing/building per frame — parse documents once, keep chart values
  outside the view when data is unchanged.
- `readVar` above the widget that edits the `Var` — extra pass per change;
  move reads below when order is free.
- Missing `-threaded`.
- Touching `Context` from a worker thread — wake with `askWake` instead.

## Book code

Every Haskell snippet in `book/src` is an `{{#include path:anchor}}` of
compiled code, never written inline; `nano-ui-book-test` fails on inline
Haskell. Chapter snippets live in `packages/nano-ui-demo/book/Book/*.hs` (the
`book-examples` library), and code drawn to a picture lives in
`packages/nano-ui-svg/app/*Examples.hs`, which the glossary renders. mdBook
does not dedent an anchored region, so anchor whole top-level definitions;
put stand-ins for application code below the anchors.

## Writing style

The book and UI copy read as if a person wrote them. Generated prose has a
recognizable accent; do not reproduce it.

- No em-dashes. Use a colon, a comma, parentheses, or a second sentence. This
  applies to `book/src` and to the book strings in `Glossary.hs`, which
  generate `book/theme/nano-ui-banks.js`. A book edit is not done until
  `rg '\x{2014}' book/src` finds nothing.
- Drop the "not X, but Y" and "isn't just X, it's Y" cadence, rhetorical
  question openings, and a closing paragraph that restates the section.
- Avoid the stock vocabulary: delve, leverage, seamless, robust, landscape,
  realm, tapestry, testament, elevate, unlock, streamline, game changer,
  cutting edge, at its core, it's worth noting, here's the thing, let's dive
  in.
- Do not pad with three-item lists of abstractions or stack parallel clauses
  to sound thorough. One concrete example beats three vague ones.
- Prefer short sentences, active voice, measurable claims and code. Let a
  section end when its last point is made.
- Vary paragraph and sentence length. Uniform rhythm reads as generated.

## Where to look things up

- `packages/nano-ui/lib/NanoUI.hs` — the entire public API, grouped and
  documented (Haddock overview). First place to find what exists.
- `book/src/` (or [online](https://goolord.github.io/nano-ui/)) — full documentation book: quick start, tutorials, state (including the Elm-style reducer), keyboard/focus, layout, widget gallery, styling, troubleshooting, and architecture notes.
- `book/src/state/index.md` — `Var` semantics, write costs, resource handles.
- `book/src/state/identity.md` — widget ids, `scope`, `withKey`, and keyed lists.
- `book/src/widgets/index.md` — the widget gallery and map to every widget chapter.
- `book/src/getting-started/development.md` — building, testing, profiling, conventions.
- `packages/nano-ui-demo/examples/` — one concept per file (`Hello`, `State`,
  `Layout`, `Components`, `Forms`, `TodoList`, `Keyboard`, `Overlays`,
  `Theming`, `Animation`, `Images`, `Canvas`, `Tasks`, `Files`, `LongList`,
  `LiveChart`), copyable as starting points. Full apps: `Demo.hs` (widget
  tour), `Notepad.hs` (menus + dialogs), `Logs.hs` (streaming), `Terminal.hs`.
- Real consumers: [ned](https://github.com/goolord/ned) (editor),
  [stronghead](https://github.com/goolord/stronghead) (GHC diagnostics GUI),
  [winget-gui](https://github.com/goolord/winget-gui) (Windows package
  manager front end).
- In consumer repos, the pinned library source is under
  `dist-newstyle/src/nano-ui-<hash>/`.
