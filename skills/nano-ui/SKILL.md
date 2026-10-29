---
name: nano-ui
description: Build desktop user interfaces in Haskell with nano-ui, an immediate-mode GUI toolkit (nano-ui-sdl, nano-ui-rgfw, nano-ui-emit). Use when writing, reviewing, or debugging nano-ui views, widgets, layout, styling, Var state, keyboard handling, overlays, background tasks, or custom drawing, or when translating an HTML/CSS/React design or web habits into nano-ui.
---

# Writing user interfaces with nano-ui

A view is a `NanoUI ()` action the backend re-runs every frame. Widgets return
what the user did this frame (`button` returns `Bool`), input widgets take a
mutable `Var` holding their state, and layout is a modifier argument. Views are
ordinary Haskell: functions, `do` notation, `if`/`case`/`for_`.

```haskell
counter :: NanoUI ()
counter = row $ do
  n <- useVar (0 :: Int)
  whenM (button "-") (modifyVar n (subtract 1))
  label . T.pack . show =<< readVar n
  whenM (button "+") (modifyVar n (+ 1))
```

Reference files in this skill, read when the task needs them:

- `references/html-css-mapping.md`: HTML elements, CSS properties, DOM events
  and React habits mapped to nano-ui. Read it before porting a web design or
  when reaching for a web idiom (margins, `display: none`, `z-index`).
- `references/recurring-bugs.md`: bugs real nano-ui apps made and how each was
  fixed (layout jitter, boxes in boxes, stale drawings, focus ordering). Check
  it before debugging a visual or input symptom.
- `references/advanced.md`: custom drawing and content keys, images and SVG,
  text editing, native windows, SDL specifics.

## Finding the API

- `NanoUI.hs` is the whole public view API with Haddock overviews per group.
  Read it first to learn what exists; do not guess names.
- A consumer project pins the library as a Cabal `source-repository-package`
  (github.com/goolord/nano-ui, `subdir: packages/nano-ui packages/nano-ui-sdl ...`,
  plus `package nano-ui-sdl` with `flags: +sdl`). Cabal fetches it into
  `dist-newstyle/src/nano-ui-<hash>/`; read the source and export lists there.
  The Hackage release may lag the source.
- The book (https://goolord.github.io/nano-ui/, or `book/src` in the repo)
  covers state, layout, every widget family, styling, input and async work.
  `packages/nano-ui-demo/examples/` has one concept per file (`Hello`,
  `State`, `Layout`, `Components`, `Forms`, `TodoList`, `Keyboard`,
  `Overlays`, `Theming`, `Animation`, `Images`, `Canvas`, `Tasks`, `Files`,
  `LongList`, `LiveChart`) and full apps (`Demo`, `Notepad`, `Logs`,
  `Terminal`), all usable as starting points.

## Project setup

GHC 9.10 to 9.14, `default-language: GHC2024`, `OverloadedStrings`, and
`-threaded` on executables. The SDL backend needs SDL3 and SDL3_ttf 3.2+ found
through pkg-config (on Windows: MSYS2 UCRT64, with `PKG_CONFIG_PATH` pointing at
`C:\msys64\ucrt64\lib\pkgconfig`). The RGFW backend needs no system libraries
and bundles a bitmap font.

```haskell
import NanoUI
import NanoUI.Backend.Sdl (SdlOptions (..), defaultSdlOptions, runSdlApp)

main :: IO ()
main = do
  app <- newAppState -- every newVar / newTask / newImageHandle happens here
  runSdlApp
    defaultSdlOptions
      { sdlWindowSettings = defaultWindowSettings {wsTitle = "Inbox", wsSize = Size 960 640}
      , sdlAppTheme = Just appTheme
      }
    (appView app)
```

Packages: `nano-ui` (core), `nano-ui-sdl` (TrueType fonts, installed-font
lookup, file dialogs, custom title bars), `nano-ui-rgfw` (OpenGL, no system
deps), `nano-ui-emit` (Elm-style messages), `nano-ui-diagrams` (charts in
`NanoUI.Plot`), `nano-ui-form` (validated forms), `nano-ui-markdown`
(streamable Markdown), `nano-ui-svg` (export any view to SVG).

## The frame model

Each frame: the view runs (adding layout nodes, reading input, reading and
writing `Var`s), layout is solved, pointer/keyboard/focus resolve against the
new geometry, damage is computed against the previous frame, and the damaged
region is painted. Consequences:

- **The view can run more than once per frame.** Writing a `Var` that was
  already read earlier in the pass re-runs the view once with one-shot input
  cleared, so clicks and key presses don't fire twice. Never allocate inside a
  view (`liftIO (newVar x)` allocates every pass), and put every side effect
  behind an event: `whenM (button "Save") saveDocument`.
- **Where `readVar` sits matters.** Read a `Var` below the input that edits
  it and typing costs one pass; read it above and each change costs two. A
  `Var` bound only to widgets (no `readVar`) repaints just those widgets; any
  `readVar` makes a write repaint the window.
- **Geometry lags one frame.** Layout is solved after the view runs, so
  `respRect` is the previous frame's rectangle (empty on first appearance).
- **Idle costs nothing.** The loop blocks on events unless animation, a timed
  wake, or input is pending. Never force continuous redraws.

## Widget naming

| Form | Meaning |
| --- | --- |
| `button "Save"` | Base: returns `Bool` (activated) or `()` |
| `button'` | Primed: also or instead returns a `Response` |
| `buttonWith (fixedW 120)` | Takes a `Layout -> Layout` modifier first |
| `buttonConfigured cfg` | Takes a config record; each has a `default...Config` |

```haskell
button    :: Text -> NanoUI Bool
label     :: Text -> NanoUI ()
checkbox  :: Text -> Var Bool -> NanoUI ()
slider    :: Float -> Float -> Var Float -> NanoUI ()   -- min max
textInput :: Var Text -> NanoUI ()
select    :: Foldable f => f Text -> Var Int -> NanoUI ()
modal     :: Var Bool -> Text -> NanoUI () -> NanoUI ()  -- open state, title, body
tabs      :: (Foldable f, Hashable a) => Var a -> f (Tab a (NanoUI ())) -> NanoUI ()
```

Input widgets take the `Var` they show and edit; there are no callbacks. To
react to an edit, use the primed variant's `Response`: `respClicked` (left
click or Enter), `respChanged` (value edited), `respSubmitted` (Enter in a
field), `respHovered`, `respPressed` (held down), `respRightClicked`,
`respClickedWith`/`respHeldWith` (which `MouseButton`), `respRect`, `respId`.
`searchInput'` debounces `respChanged`, so trigger expensive work on it.

Inventory, all from `NanoUI`: containers (`row`, `column`, `grid`, `panel`,
`card`, `callout`, `toolbar`, `center`, `responsive`, `scroll`, `scroll2D`,
`layers`, `mouseArea`, `separator`, `spacer`), text (`label`, `heading`,
`muted`, `mono`, `danger`, `kv`, `selectableText`, `richText`), buttons and
menus (`button`, `iconButton`, `buttonContent`, `menuButton`, `menuItem`,
`menuSeparator`, `contextMenu`), inputs (`checkbox`, `toggleButton`,
`radio`/`enumRadio`, `select`/`enumSelect`, `slider`, `textInput`,
`searchInput`, `numericInput`, `comboBox`, `textArea`, `textAreaDocument`,
`colorPicker`), `tabs`/`tabBar`, `tree`, `table`/`simpleTable` (Colonnade,
sortable by `Var SortCol`), overlays (`modal`, `window`, `popup`, `tooltip`),
`paneGrid`, images and SVG icons, drag and drop (`useDrag`, `useDrop`,
`dropZone`, `useReorder`), and canvas-drawn controls in
`NanoUI.Widgets.Custom` (`toggleSwitch`, `knob`, `progressBar`,
`circularProgress`, `spinner`, `sparkline`), which double as reference
implementations for your own custom widgets.

## State: `Var`

- `useVar initial`: positional, allocated inside the view, no setup. Only
  `Int`, `Double`, `Float`, `Bool`, `Text`, `SortCol` (`useEnumVar` for any
  `Enum`). The initial value is used once; reset later with `writeVar`.
- `newVar initial`: in `IO`, any type. Allocate at setup, keep it in an app
  record, pass it to the view. Two widgets given the same `Var` share state.
- `VarMap` with `varAt`/`dropVar`/`retainVars`: one `Var` per key for dynamic
  collections.
- `constVar x`: a read-only `Var`; show a derived value in an input and handle
  edits through the primed `Response`.
- `withValue v widget`: run a `Var` widget against a plain value and get the
  result and the edited value back.
- Record fields: with `Generic` and `OverloadedRecordDot`, `settings.volume` is
  a `Var Float` for one field, tracked apart from the others. `zoomVar`
  (getter/setter) and `zoomVarL` (lens) for other projections.
- Lists: `forEachVar key list body` gives per-element `Var`s that follow their
  keys across reorders; `withElemVars` when you filter or sort yourself.
- In the view: `readVar`, `writeVar`, `modifyVar` (equal writes are no-ops),
  `peekVar` from `NanoUI.Var` (read without a dependency). From `IO` or another thread:
  `readVarIO`, `writeVarIO`, `modifyVarIO`, then call the callback from
  `askWake` to schedule a frame.

Elm-style alternative: `nano-ui-emit`'s `NanoUIE msg a` is a `NanoUI` action
with the message sink in scope as `?emit`. Core widgets work unchanged
(`NanoUI.Emit` re-exports `NanoUI`). Emit with `emit`,
`emitWhen (button "Save") Save`, `emitChanged (slider 0 100) v SetV`;
`mapMessages` for child components. Views are `Model -> NanoUIE Msg ()`; run
with `runSdlAppReduce options update model view`.

## Stable widget identity

Widget ids count up in call order among siblings, and internal state (caret,
selection, open dropdown) is keyed by them.

- Keep call order the same from frame to frame.
- Wrap a **conditional subtree** in `scope`: it takes one slot whether or not
  its body runs, so later siblings keep their ids. `useVar` has its own counter,
  but a conditional `useVar` still needs `scope`.
- Wrap **items of a dynamic or reorderable list** in `withKey` with a stable,
  sibling-unique key. Never the list index.
- Task, stream and image calls take no widget ids; calling them conditionally
  is safe for identity.
- Hidden content keeps its state. For ever-growing keyed collections keep row
  state in a `VarMap` and `retainVars` what is gone.

## Layout

Logical pixels, origin top-left, `y` down; backends handle DPI. Modifiers are
`Layout -> Layout` functions composed with `(.)`, and **the leftmost wins**
when two set the same property.

```haskell
columnWith (gap 8 . padAll 12 . fillW) $ do
  label "Settings"
  buttonWith (fixedW 120) "Apply"
```

- Sizing: `fillW`, `fillH`, `grow` (both), `fixedW`/`fixedH`/`fixedWH`,
  `minW`/`maxW`/`minH`/`maxH`, `percent 50` (of the parent's width),
  `aspect r`. Everything else is fit to content.
- Spacing: `padAll`, `padXY`, `padLeft`/`padRight`/`padTop`/`padBottom`,
  `padLRTB`, `gap`, `tight` (zero padding, keeps the gap). Containers default
  to 3 px padding and an 8 px gap.
- Alignment: `alignStart`/`alignCenter`/`alignEnd` across a column,
  `alignTop`/`alignMid`/`alignBottom`/`alignBaseline` down a row. A child
  aligns itself: `alignMid` on a row positions the row, not its contents.
- `wrap` (with `lineGap`, `lineAlign`), `gridCols`, `layered`.
- `spacer (Grow 1) Fit` or `flex` pushes siblings apart.
- `layers` stacks children in one box, later on top. `pinAt x y` takes a child
  out of flow; `pinAt 0 0 . grow` covers the parent (scrims, overlays).
- Scrollers need a bounded viewport: `scrollWith (fixedH 240 . fillW) body`.
  `scrollArea` also returns the scroller's `WidgetId` for programmatic control.
- Huge lists: `sid <- currentId` before the scroller, `getScrollMetricsUi sid`
  for offset and viewport, build only visible rows between spacers, and
  `scrollRectIntoViewUi` to follow the selection (`examples/LongList.hs`).
- Pointer: interactive widgets take pointer events, panels and labels don't.
  `pointer PointerBlock` makes a region eat clicks; `pointer PointerPass` makes
  decoration click-through. `mouseArea` reports hover and clicks for a whole
  region (list rows, cards).
- Natural size follows content, so a label whose text changes length moves its
  siblings. Pin changing labels (counts, timers, byte totals) with a `fixedW`
  sized to the widest value; in a scrolling list make cells `fillW` so a long
  value cannot widen the list.
- `explainLayout True` outlines every node and reports the one under the pointer
  when a layout surprises you.

## Theming and styling

A `Theme` holds a `Style` per surface (`themeButton`, `themeInput`,
`themePanel`, `themeFloatingWindow`, `themePopup`) and semantic colours
(`themeAccent`, `themeMuted`, `themeSelection`, `themeFocusRing`,
`themeWindow`, `themeSeparator`, success/warning/danger). A `Style` is
bg/fg/border/borderWidth/cornerRadius/hoverBg/activeBg.

- Built-in themes: `defaultTheme` (dark), `defaultLightTheme`, Tomorrow
  variants, `themeFromBase16`/`themeFromBase16Dark`/`themeFromBase16Light`.
- Set once with `sdlAppTheme = Just appTheme`, or per frame with `setUiTheme`.
  Follow the OS: `setUiTheme . lightDark defaultLightTheme defaultTheme =<< systemAppearance`.
- Scope a change with `styled (Theme -> Theme) subtree`; modifiers compose with
  `(.)` and nest. `themed theme subtree` replaces the theme. Theme scopes change
  painting only, never layout.
- Style modifiers: `background`, `foreground`, `borderColor`, `borderWidth`,
  `borderSides`, `cornerRadius`, `hoverBackground`, `pressBackground`,
  `fillColor`. Theme modifiers: `buttonStyle`, `inputStyle`, `panelStyle`,
  `everyStyle`, `accentColor`, `textColor`, `pageColor`, `panelPage`,
  `rounded`, `subtle`, `tinted`, `readableOn`. Semantic: `primary`,
  `destructive`, `success`, `warning`, `tone Danger`; `toneColor Warning` for
  custom drawing.
- Text style modifiers go on the text widget itself: `labelWith (fontSize 20 . fontBold)`,
  `fontMono`, `fontTone Warning`, `fontColor c`, `fontItalic`,
  `fontUnderline`, `fontStrike`.
- The page behind a view is `themeWindow` unless `themePage` is set
  (`pageColor`). `panelPage` gives a view with no panel of its own the panel
  surface. A full-window scroller (`scrollWith grow`) paints the page colour.
- `disabledWhen cond subtree` disables input and dims it, keeping layout and
  state.
- `uiTheme` reads the active theme in a view.

For a fully custom look: a `Palette` record of literal colours, a complete
`Theme` built from it, then small helpers, e.g.
`quiet = subtle . buttonStyle (foreground muted)` for borderless buttons and
`surface bg border radius = styled (panelStyle (background bg . ...)) . panelWith`
for chrome. Three surface levels (well, shelf, controls) and colour kept for
meaning go a long way.

Fonts (SDL): bundled Inter by default; `sdlAppFont = FontSearch ["Segoe UI"]`,
`sdlAppMonoFont = FontSearch ["Cascadia Mono"]`, `sdlAppFontSize`, and
`setSdlUiFont` mid-session. `sdlAppUiScale = 0` follows desktop scaling.

## Keyboard, mouse and focus

- Chords from `NanoUI.Shortcut`: `shortcut (ctrl <> key 's')`,
  `shortcutOnce (cmdOrCtrl <> shift <> key 'p')` (ignores key repeat),
  `parseShortcut "C-s"` for user config. Only the first matching `shortcut`
  call in a frame fires.
- Suppression is automatic: shortcuts and key queries are blocked behind an
  open modal, inside `disabledWhen`, and when the focused widget claims the key
  (typing in a field, arrows on a slider). Modified chords (Ctrl+Enter,
  Alt+Left) still pass. `menuItemShortcut` binds only while its menu is open,
  so add a top-level `shortcut` for the closed-menu binding.
- Focus: clicking focuses fields and selects, Tab and Shift+Tab cycle.
  `requestFocus (respId resp)` (or `currentId` just before the widget),
  `focusNext`, `clearFocus`, `isFocused`. Focus changes apply at the end of the
  frame.
- **Ask focus questions at the end of the view**, after the widgets have run.
  At the top of the view the node arena is empty and "is a field focused?"
  always answers no. Keyboard shortcuts that depend on focus go last.
- Text inputs do not claim Escape. Give Escape its own guard rather than
  sharing the "a field is editing" guard used for Ctrl+A or Delete.
- Raw input: `keyPressed`, `keyPressedOnce`, `keyHeld`, `keyReleased`;
  `mousePressed MouseBack` and friends; `askInput` for the whole `Input`
  record (`inputKeys`, `inputKeysNew`, `inputChars`, ...).
- A custom widget that reads `inputChars` must call `useInputMethod` on every
  focused frame or typed text never reaches it.
- Cursors: built-ins set their own; `withCursorShape UiCursorCrosshair subtree`
  for a region, `widgetCursor` in a custom widget spec.
- Clipboard: `getClipboard`, `setClipboard`.

## Overlays

`modal`, `window` and `popup` take a `Var Bool` for open state; Escape, an
outside click and the close button write `False`. Call them **every frame,
unconditionally** (inside `when` they shift sibling ids); they draw nothing
while closed.

```haskell
confirm <- useVar False
whenM (button "Delete...") (writeVar confirm True)
modal confirm "Delete the file?" $
  whenM (button "Delete") (deleteFile >> writeVar confirm False)
```

For derived open state or dialogs that must not auto-dismiss, pass `constVar`
and use the primed variants (`modal'`, `window'`, `popup'`) with
`respClicked`. Tooltips are stateless: `tooltip resp "text"`, `withTooltip`,
`tooltipConfigured` for delay and placement. Right-click menus:
`contextMenu resp body` or `contextMenuArea`.

## Background work

- `task <- newTask` at setup, then `useTaskStatus task key action`
  (`TaskRunning`/`TaskDone`/`TaskFailed`) or `useTask task key action` (`Maybe a`,
  keeps the previous result while a new key runs). A task runs once per key,
  restarts when the key changes (cancelled by async exception, so use `bracket`
  inside for resources), and **is cancelled when its call is skipped for a
  frame**. Keep `useTask` outside tabs and conditionals when work must continue
  while hidden. Put a retry counter in the key to run it again.
- `stream <- newStream` and `useStream stream key initial producer` for pushed
  updates (progress, sensors, token streams); updates between frames coalesce.
- Other threads: write state with `writeVarIO`/`modifyVarIO` or your own
  `IORef`, then call the `askWake` callback. Never touch the `Context` from
  another thread.

## Animation and time

`animateTo`/`animateToA` move a value toward a target with a `Tween ease
duration delay` or a `Spring` (`presetBouncy`, `presetSmooth`, `presetStiff`),
retargeting mid-flight. `animate`/`pulse` oscillate. Call `keepAnimating`
inside the animating widget, only while it is on screen. Timers, clocks and
debounces call `wakeAfter seconds` on every frame that still needs a future
wake (pending wakes reset each frame) and read `uiTime`. Never mark the
window dirty every frame to keep it moving: that is an unpaced busy loop.

## Application structure

Set up once in `IO`, then run. Everything frame-lived is a `Var` or inside the
view.

- Setup returns a plain state record (`newApp :: IO App`) of `Var`s, tasks,
  image handles and caches; `main` passes it to the view. Views never take a
  backend handle.
- winget-gui keeps app state in an `IORef AppState` and installs the `askWake`
  callback into an `IORef (IO ())` on the first frame; worker threads update
  state and then wake. Each frame gathers its reads into one record before
  building the window top to bottom, keyboard shortcuts last. Derived
  collections are memoized in `IORef (Maybe (key, value))` caches keyed by a
  revision counter.
- stronghead uses a large `App` record of `Var`s with one state type per
  panel module, plus a command palette and floating panes.
- ned keeps state in a plain `IORef App` the toolkit knows nothing about, and
  calls `requestFrame` when what was drawn no longer matches state.
- Elm-style (`nano-ui-emit`) when a pure model and an update function fit.
- Headless modes are cheap: a `--screenshot path` or `--selftest dir` flag that
  runs the real view against real data in a hidden window.

## Testing

`NanoUI.Testing.Sim` drives a view like a user:

```haskell
runSim ctx failed (withInput 300 200) counterView $ do
  click "+"
  click "+"
  expect "2"
```

`withInput w h` (from `NanoUI.Testing.Assert`) is an empty input at that window
size; `failed` is an `IORef Int` counting failed checks. `runSim` settles the
view first. Targets: string literals aim at visible text;
`At p`, `Within rect`, `On resp`, `OnWidget wid` otherwise. Actions: `click`,
`rightClick`, `doubleClick`, `typeText`, `pressKey`, `chord`, `wheel`, `drag`,
`settle`; checks: `expect`, `check`, `checkEq`. Import it explicitly or
qualified, since its short names clash with locals.

Lower level, `NanoUI.Testing`: `newContext`, `runFrame ctx input view` (result,
draw data, whether another frame was requested), `runFrameE` for emit views,
and `NanoUI.Testing.Harness` helpers (`warmup2`, `clickPair`, `held`,
`spanRect`). Warm up a frame before targeting `respRect`, set `tooltipDelay = 0`
in tooltip tests, use `newWakeSignal` for deterministic task wake-ups, and
`cancelTasks` in teardown. Custom-drawn widgets have no
text spans: click them, screenshot, and assert on the image.

## Pitfalls checklist

- `liftIO (newVar ...)` in a view allocates every pass. Move it to setup.
- Side effects not behind an event run on every pass. `whenM (button ...)`.
- Conditional widgets or `useVar`s without `scope`; list rows keyed by index.
- Overlays called inside `when` instead of every frame with a `Var Bool`.
- `useTask` inside a tab or branch is cancelled while hidden.
- `keepAnimating` from an always-run top level is a busy loop.
- Writing a freshly computed `NaN` to a `Var` you read redraws forever
  (`NaN /= NaN`). Guard the write.
- A custom widget's content key missing something the drawing reads draws
  stale pixels.
- A changing label without `fixedW` shifts its siblings; a wide cell in a
  scrolling list without `fillW` makes the list scroll sideways.
- A `scrollArea`, `panel` or `card` inside a popup or dialog is a box in a box.
  Let one level own the chrome.
- Focus queries at the top of the view always answer no.
- Escape behind the same guard as Ctrl+A or Delete stops working in fields.
- A custom widget reading `inputChars` without `useInputMethod` gets no text.
- Per-glyph advance times count drifts from a shaped run; measure the run with
  `lineWidth`.
- Parsing or rebuilding large data every frame; keep it outside the view and
  recompute only when its inputs change.
- `readVar` above the widget that edits the `Var` costs an extra pass.
- Missing `-threaded`.
