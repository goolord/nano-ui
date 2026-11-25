---
name: nano-ui
description: Build desktop user interfaces in Haskell with nano-ui, an immediate-mode GUI toolkit (nano-ui-sdl, nano-ui-rgfw, nano-ui-emit). Use when writing, reviewing, or debugging nano-ui views, widgets, layout, styling, Var state, keyboard handling, overlays, background tasks, or custom drawing, or when translating an HTML/CSS/React design or web habits into nano-ui.
---

# Writing user interfaces with nano-ui

A view is a `NanoUI ()` action that the backend runs on every frame. Widgets return what happened on the current frame (`button` returns `Bool`), input widgets take a mutable `Var` holding their state, and layout is passed as composable modifier functions. Views are ordinary Haskell: plain functions, `do` blocks, and standard control flow (`if`, `case`, `for_`).

```haskell
counter :: NanoUI ()
counter = row $ do
  n <- useVar (0 :: Int)
  whenM (button "-") (modifyVar n (subtract 1))
  label . T.pack . show =<< readVar n
  whenM (button "+") (modifyVar n (+ 1))
```

Reference files in this skill (read on demand):

- `references/html-css-mapping.md`: Maps HTML elements, CSS properties, DOM events, and React habits to `nano-ui`. Read before porting a web UI or reaching for web idioms (margins, `display: none`, `z-index`).
- `references/recurring-bugs.md`: Common pitfalls found in real `nano-ui` apps (`ned`, `stronghead`, `winget-gui`) and their fixes (layout jitter, nested borders, stale canvases, focus ordering). Check before debugging a visual or input bug.
- `references/advanced.md`: Custom drawing and content keys, images and SVG, multi-line text editing, native windows, and SDL specifics.

## Finding the API

- `NanoUI.hs` is the full public view API, grouped with Haddock overviews. Read it first to confirm exact names instead of guessing.
- Downstream projects pin the repository as a Cabal `source-repository-package` (`github.com/goolord/nano-ui`, `subdir: packages/nano-ui packages/nano-ui-sdl ...`, plus `package nano-ui-sdl` with `flags: +sdl`). Cabal downloads sources into `dist-newstyle/src/nano-ui-<hash>/`, where you can read export lists directly. The Hackage release may trail git `main`.
- The book (`https://goolord.github.io/nano-ui/book/`, or `book/src` locally) covers state, layout, every widget family, styling, input, and async tasks. `packages/nano-ui-demo/examples/` contains single-topic programs (`Hello`, `State`, `Layout`, `Components`, `Forms`, `TodoList`, `Keyboard`, `Overlays`, `Theming`, `Animation`, `Images`, `Canvas`, `Bloom`, `Tasks`, `Files`, `LongList`, `LiveChart`) and full apps (`Demo`, `Notepad`, `Logs`, `Terminal`).

## Project setup

Target GHC 9.10 through 9.14, `default-language: GHC2024`, `OverloadedStrings`, and `-threaded` on executables. `nano-ui-sdl` requires SDL3 and SDL3_ttf 3.2+ via `pkg-config` (on Windows: MSYS2 UCRT64 with `PKG_CONFIG_PATH=C:\msys64\ucrt64\lib\pkgconfig`). `nano-ui-rgfw` requires no extra system libraries and bundles a bitmap font.

```haskell
import NanoUI
import NanoUI.Backend.Sdl (SdlOptions (..), defaultSdlOptions, runSdlApp)

main :: IO ()
main = do
  app <- newAppState -- allocate newVar / newTask / newImageHandle here once
  runSdlApp
    defaultSdlOptions
      { sdlWindowSettings = defaultWindowSettings {wsTitle = "Inbox", wsSize = Size 960 640}
      , sdlAppTheme = Just appTheme
      }
    (appView app)
```

Workspace packages: `nano-ui` (core), `nano-ui-sdl` (SDL3, TrueType fonts, system font search, native file dialogs, custom title bars), `nano-ui-rgfw` (OpenGL, zero extra dependencies), `nano-ui-sdl-web` (Wasm/WebGL2 browser backend), `nano-ui-emit` (Elm-style messages), `nano-ui-diagrams` (charts in `NanoUI.Plot`), `nano-ui-form` (validated forms), `nano-ui-markdown` (streamable Markdown), `nano-ui-shader` (typed fragment shader EDSL and built-in effects), `nano-ui-svg` (SVG export).

## The frame model

Each frame runs your view action (building layout nodes, reading input, updating `Var`s), solves layout, resolves pointer/keyboard/focus states against the solved geometry, diffs damage against the previous frame, and repaints only the damaged region.

- **Views can run twice per frame.** Writing to a `Var` that was already read earlier in the same pass triggers one extra pass (with one-shot input events cleared so clicks and key presses never fire twice). Never allocate inside a view (`liftIO (newVar x)` leaks every pass), and guard every side effect behind an event (`whenM (button "Save") saveDocument`).
- **`readVar` placement matters.** Reading a `Var` below the input widget that edits it keeps updates to one pass; reading above it takes two passes. Either way, damage tracking only repaints widgets whose text, value, or style changed.
- **Geometry lags by one frame.** Layout solves after the view returns, so `respRect` reflects the previous frame's bounds (empty on first appearance).
- **Idle windows cost nothing.** The event loop sleeps on OS events when no animation, timer, or task is active. Never dirty the context every frame to force redraws.

## Widget conventions

| Form | Meaning |
| --- | --- |
| `button "Save"` | Base form: returns `Bool` (activated) or `()` |
| `button'` | Primed form: returns a `Response` |
| `buttonWith (fixedW 120)` | Takes a `Layout -> Layout` modifier first |
| `buttonConfigured cfg` | Takes a config record (every config has a `default...Config`) |

```haskell
button    :: Text -> NanoUI Bool
label     :: Text -> NanoUI ()
checkbox  :: Text -> Var Bool -> NanoUI ()
slider    :: Float -> Float -> Var Float -> NanoUI ()   -- min max
textInput :: Var Text -> NanoUI ()
select    :: Foldable f => f Text -> Var Int -> NanoUI ()
modal     :: Var Bool -> Text -> NanoUI () -> NanoUI () -- open flag, title, body
tabs      :: (Foldable f, Hashable a) => Var a -> f (Tab a (NanoUI ())) -> NanoUI ()
```

Input widgets mutate their `Var` directly without callbacks. To react to edits, inspect the primed variant's `Response`: `respClicked` (left click or Enter/Space), `respChanged` (value edited), `respSubmitted` (Enter in a text field), `respHovered`, `respPressed` (button held down), `respRightClicked`, `respClickedWith` / `respHeldWith` (specific `MouseButton`), `respRect`, and `respId`. `searchInput'` debounces `respChanged`, making it ideal for triggering expensive searches.

Public widget inventory (all exported from `NanoUI`): containers (`row`, `column`, `grid`, `panel`, `card`, `callout`, `toolbar`, `center`, `responsive`, `scroll`, `scroll2D`, `layers`, `mouseArea`, `separator`, `spacer`), text (`label`, `heading`, `muted`, `mono`, `danger`, `kv`, `selectableText`, `richText`), buttons and menus (`button`, `iconButton`, `buttonContent`, `buttonGroup`, `menuButton`, `menuItem`, `menuSeparator`, `contextMenu`), inputs (`checkbox`, `toggleButton`, `radio`/`enumRadio`, `select`/`enumSelect`, `slider`, `textInput`, `searchInput`, `numericInput`, `comboBox`, `textArea`, `textAreaDocument`, `colorPicker`), `tabs`/`tabBar`, `tree`, `table`/`simpleTable` (Colonnade, sortable by `Var SortCol`, with draggable column dividers and double-click auto-fit), overlays (`modal`, `window`, `popup`, `tooltip`/`tooltipWidget`), `paneGrid`, images and SVG icons, drag and drop (`useDrag`, `useDrop`, `dropZone`, `useReorder`), and canvas controls in `NanoUI.Widgets.Custom` (`toggleSwitch`, `knob`, `progressBar`, `circularProgress`, `spinner`, `sparkline`).

## State: `Var`

- `useVar initial`: View-local positional slot, no setup required. Supports `Int`, `Double`, `Float`, `Bool`, `Text`, `SortCol` (plus `useEnumVar` for any `Bounded`/`Enum` type). The initial value applies on frame 1 only; call `writeVar` to reset later.
- `newVar initial`: Allocates a `Var` of any type in `IO` during setup. Store in an app record and pass into views; sharing a `Var` shares state.
- `VarMap` (`varAt`, `dropVar`, `retainVars`): Keyed per-item `Var` map for dynamic collections.
- `constVar x`: Read-only `Var` for showing derived values in input widgets while handling edits via the primed `Response`.
- `withValue v widget`: Runs a `Var`-based widget on a temporary `Var` and returns both the result and updated value.
- Record projections: With `Generic` and `OverloadedRecordDot`, `settings.volume` projects a `Var Float` for that single field, tracked independently of sibling fields. Use `zoomVar` (getter/setter) or `zoomVarL` (lens) for custom projections.
- Dynamic lists: `forEachVar key list body` supplies per-element `Var`s keyed across reorders; use `withElemVars` when filtering or sorting the list yourself.
- Reading and writing: `readVar`, `writeVar`, `modifyVar` (writes with equal `==` values are no-ops), and `peekVar` from `NanoUI.Var` (read without recording a dependency). From worker threads or plain `IO`, use `readVarIO`, `writeVarIO`, and `modifyVarIO`, then invoke the callback from `askWake`.

Elm-style option: `nano-ui-emit` defines `NanoUIE msg a` as a `NanoUI a` action with an implicit `?emit` sink (`NanoUI.Emit` re-exports all of `NanoUI`). Emit with `emit`, `emitWhen (button "Save") Save`, `emitChanged (slider 0 100) v SetV`, and compose children with `mapMessages`. Give views an explicit signature (`Model -> NanoUIE Msg ()`) and run them with `runSdlAppReduce options update model view`.

## Stable widget identity

Widget IDs count sequentially among siblings inside each container, backing carets, text selections, scroll offsets, and open dropdowns.

- Keep sibling call order stable across frames.
- Wrap **conditional subtrees** in `scope` so they consume one consistent slot whether or not their body runs. `useVar` has its own separate counter, but conditional `useVar` calls still require `scope`.
- Wrap **dynamic or reorderable list rows** in `withKey` using a stable, sibling-unique key (never the list index).
- `useTask`, `useStream`, and `useImageRgba` do not consume widget IDs, so calling them conditionally does not disturb sibling identity.
- Hidden keyed state remains in memory; for unbounded growing collections, keep row state in a `VarMap` and prune deleted keys with `retainVars`.

## Layout

Logical pixels, origin top-left, `y` downward; backends apply DPI scaling automatically. Layout modifiers are `Layout -> Layout` functions composed with `(.)`, where the **leftmost modifier wins** when two set the same property.

```haskell
columnWith (gap 8 . padAll 12 . fillW) $ do
  label "Settings"
  buttonWith (fixedW 120) "Apply"
```

- **Sizing:** `fillW`, `fillH`, `grow` (both), `fixedW` / `fixedH` / `fixedWH`, `minW` / `maxW` / `minH` / `maxH`, `percent 50` (percentage of parent width), `aspect r`. Default sizing fits content.
- **Spacing:** `padAll`, `padXY` (horizontal, then vertical), `padLeft` / `padRight` / `padTop` / `padBottom`, `padLRTB`, `gap`, `tight` (zero padding, keeps gap). Default containers have 3 px padding and an 8 px gap.
- **Alignment:** `alignStart` / `alignCenter` / `alignEnd` horizontally and `alignTop` / `alignMid` / `alignBottom` / `alignBaseline` vertically position the node they are placed on. To align children across a container's cross axis (rows, columns, wrapping containers, grids), put `itemsStart` / `itemsCenter` / `itemsEnd` / `itemsBaseline` (or `itemsAlign`) on the container; explicit `align*` modifiers on individual children override it.
- **Flow:** `wrap` (with `lineGap`, `lineAlign`), `gridCols`, `layered`, and `spacer (Grow 1) Fit` (or `flex`) to push siblings apart.
- **Stacking and pinning:** `layers` stacks children in one box with later children on top. `pinAt x y` takes a child out of flow; `pinAt 0 0 . grow` covers the parent (useful for scrims or highlights).
- **Scrolling:** Scrollers need a bounded viewport along their scroll axis (`scrollWith (fixedH 240 . fillW) body`). `scrollArea` also returns the scroller's `WidgetId`. View-side commands: `scrollToUi`, `scrollByUi`, `scrollPagesUi`, `scrollToStartUi`, `scrollToEndUi`, `scrollIntoViewUi`, and `scrollRectIntoViewUi`.
- **Large lists:** Grab `sid <- currentId` before the scroller, query `getScrollMetricsUi sid`, build only visible rows between spacers, and jump to selections with `scrollRectIntoViewUi` (`examples/LongList.hs`). For keyboard-driven custom lists, set `scrollFocusable = True` in `ScrollConfig` and pair `sid` with `useListNav` or `useListNavWith`.
- **Pointer routing:** Interactive controls capture pointer events while panels and labels let them pass through. Override with `pointer PointerBlock` or `pointer PointerPass`. Use `mouseArea` for clickable rows or cards.
- **Preventing jitter:** Pin dynamic labels (counters, timers) with `fixedW`; give wide cells inside scrolling lists `fillW` so they truncate with `...` instead of triggering horizontal scrollbars.
- **Debugging:** Enable `explainLayout True` to draw layout outlines and inspect the node under the pointer.

## Theming and styling

A `Theme` holds five surface `Style` records (`themeButton`, `themeInput`, `themePanel`, `themeFloatingWindow`, `themePopup`) and semantic colors (`themeAccent`, `themeMuted`, `themeSelection`, `themeFocusRing`, `themeWindow`, `themeSeparator`, success/warning/danger). Each `Style` defines `bg`, `fg`, `border`, `borderWidth`, `cornerRadius`, `hoverBg`, and `activeBg`.

- **Built-in themes:** `defaultTheme` (dark), `defaultLightTheme`, Tomorrow variants, and `themeFromBase16` / `themeFromBase16Dark` / `themeFromBase16Light`. Set at startup with `sdlAppTheme = Just appTheme`, switch per frame with `setUiTheme`, or track OS appearance via `setUiTheme . lightDark defaultLightTheme defaultTheme =<< systemAppearance`.
- **Scoped overrides:** `styled (Theme -> Theme) subtree` adjusts the theme for a subtree (modifiers compose with `(.)` and nested `styled` calls stack outside in); `themed theme subtree` swaps the whole theme. Theme scopes affect painting only, never layout geometry.
- **Modifiers:** Style modifiers (`background`, `foreground`, `borderColor`, `borderWidth`, `borderSides`, `cornerRadius`, `hoverBackground`, `pressBackground`, `fillColor`), theme modifiers (`buttonStyle`, `inputStyle`, `panelStyle`, `everyStyle`, `accentColor`, `textColor`, `pageColor`, `panelPage`, `rounded`, `subtle`, `tinted`, `readableOn`), and semantic tones (`primary`, `destructive`, `success`, `warning`, `tone Danger`, plus `toneColor Warning` for canvas drawing).
- **Typography:** Apply font modifiers directly to text widgets (`labelWith (fontSize 20 . fontBold)`): `fontSizeScale` (scales relative to `uiFontSize` or an explicit `fontSize`), `fontMono`, `fontTone Warning`, `fontColor c`, `fontItalic`, `fontUnderline`, `fontStrike`. Font modifiers do not cascade from containers.
- **Page backdrop:** Defaults to `themeWindow` unless `themePage` is set (`pageColor`). `panelPage` puts a borderless view directly on `themePanel`, and `scrollWith grow` paints using this page color.
- **Disabled state:** `disabledWhen cond subtree` dims controls and suppresses input while keeping layout and state intact. Read the active theme inside a view with `uiTheme`.

For custom themes, define a `Palette` record, construct a full `Theme` from it, and wrap repeated treatments in small Haskell helpers (such as `quiet = subtle . buttonStyle (foreground muted)`). Three clear surface levels (well, shelf, controls) with restrained accent colors keep dense desktop UIs legible.

SDL fonts default to bundled Inter. Search system fonts with `sdlAppFont = FontSearch ["Segoe UI"]`, `sdlAppMonoFont = FontSearch ["Cascadia Mono"]`, `sdlAppFontSize`, and swap mid-session with `setSdlUiFont`. Set `sdlAppUiScale = 0` on Windows to track desktop scaling.

## Keyboard, mouse, and focus

- **Shortcuts (`NanoUI.Shortcut`):** `shortcut (ctrl <> key 's')`, `shortcutOnce (cmdOrCtrl <> shift <> key 'p')` (ignores key repeats), `parseShortcut "C-s"` for config files. Only the first matching `shortcut` call in a frame fires.
- **Automatic suppression:** Shortcuts and view-level key queries are suppressed behind open modals, inside `disabledWhen`, and when a focused widget claims the key (typing in a text field, arrows on a slider). Modified chords (`Ctrl+Enter`, `Alt+Left`) still pass through. `menuItemShortcut` activates only while its menu is open, so pair it with a top-level `shortcut` for closed-menu access.
- **Focus:** Clicking focuses fields, selects, and focusable custom widgets; Tab and Shift+Tab cycle focus. Programmatic helpers: `requestFocus (respId resp)` (or `currentId` before declaring the widget), `focusNext`, `clearFocus`, and `isFocused`. Focus updates apply at the end of the frame.
- **Check focus at the end of the view.** The layout arena resets before the view runs, so calling `isFocused` at the top always returns `False`. Put focus-dependent shortcuts after all widgets.
- **Escape is unclaimed by text fields.** Give Escape its own guard instead of reusing an "a field is focused" guard written for Ctrl+A or Delete.
- **Low-level queries:** `keyPressed`, `keyPressedOnce`, `keyHeld`, `keyReleased`, `mousePressed MouseBack`, and `askInput` for raw `Input` fields (`inputKeys`, `inputKeysNew`, `inputChars`, and so on).
- **Custom text input:** Any focused custom widget reading `inputChars` must call `useInputMethod` every frame so OS text input stays enabled.
- **Cursors and clipboard:** Override cursors for a subtree with `withCursorShape UiCursorCrosshair`, or per custom widget via `widgetCursor`. Read/write the clipboard with `getClipboard` and `setClipboard`.

## Overlays

`modal`, `window`, and `popup` take a `Var Bool` for open state. Pressing Escape, clicking outside, or clicking the close button writes `False` (floating `window`s close only via their close button). Call every overlay **unconditionally on every frame** so sibling IDs never shift; closed overlays draw nothing.

```haskell
confirm <- useVar False
whenM (button "Delete...") (writeVar confirm True)
modal confirm "Delete the file?" $
  whenM (button "Delete") (deleteFile >> writeVar confirm False)
```

For derived open flags or non-auto-dismissing dialogs, pass `constVar` and handle close clicks from `respClicked` on `modal'`, `window'`, or `popup'`. Tooltips are stateless (`tooltip resp "text"`, `tooltipWidget resp body`, `withTooltip`, `tooltipConfigured`), pass pointer events through to widgets underneath, and render their bodies inert. Right-click menus use `contextMenu resp body` or `contextMenuArea`.

## Background work

- **`Task`:** Allocate `task <- newTask` at setup, then call `useTaskStatus task key action` (`TaskRunning` / `TaskDone` / `TaskFailed`) or `useTask task key action` (`Maybe a`, preserving the previous result while loading). Changing the key cancels the previous worker with an async exception (use `bracket` inside jobs) and starts a new one. **Skipping the call for a full frame cancels the task**, so keep `useTask` outside tabs and branches when work should continue while hidden. Put an attempt counter in the key to support retries.
- **`Stream`:** Allocate `stream <- newStream` at setup and call `useStream stream key initial producer` for repeated pushes (progress, telemetry, token streams). Multiple pushes between frames coalesce automatically.
- **External threads:** Write via `writeVarIO` / `modifyVarIO` (or your own `IORef`), then call the callback from `askWake`. Never access the UI `Context` from another thread.

## Animation and timers

`animateTo` and `animateToA` smoothly animate values toward targets using `Tween ease duration delay` or `Spring` (`presetBouncy`, `presetSmooth`, `presetStiff`), retargeting mid-flight and sleeping automatically once settled. Clock-driven helpers like `animate` and `pulse` require calling `keepAnimating resp` inside the visible animating widget on every frame. For timers, clocks, and debounces, call `wakeAfter seconds` on every frame that needs a future wake (wakes reset each frame) and read `uiTime`. Never dirty the window every frame to fake animation.

## Application structure

Allocate once in `IO`, then run. Views never take a backend handle.

- Build a plain state record (`newApp :: IO App`) holding `Var`s, tasks, image handles, and caches, and pass it to the view.
- `winget-gui` keeps domain state in an `IORef AppState`, saves the `askWake` callback on frame 1, snapshots state at the top of each frame, memoizes derived lists by revision counter, and runs shortcuts last.
- `stronghead` uses a record of `Var`s split across per-panel state types, paired with a command palette and floating panes.
- `ned` keeps editor state in a plain `IORef App` outside `nano-ui` and calls `requestFrame` whenever an edit invalidates what was just drawn.
- Use `nano-ui-emit` when a pure `Msg -> Model -> Model` reducer fits best.
- Add cheap `--screenshot path` or `--selftest dir` flags that drive your real view against sample data in a hidden window.

## Headless testing

`NanoUI.Testing.Sim` scripts a view like a real user:

```haskell
runSim ctx failed (withInput 300 200) counterView $ do
  click "+"
  click "+"
  expect "2"
```

`withInput w h` (from `NanoUI.Testing.Assert`) creates an empty input at that size, and `failed` is an `IORef Int` counting mismatches. `runSim` settles the initial layout automatically. Targets accept string literals (matching visible labels), `At p`, `Within rect`, `On resp`, or `OnWidget wid`. Actions include `click`, `rightClick`, `doubleClick`, `typeText`, `pressKey`, `chord`, `wheel`, `drag`, and `settle`; assertions include `expect`, `check`, and `checkEq`. Import `NanoUI.Testing.Sim` explicitly or qualified so its short names do not shadow locals.

For lower-level tests, `NanoUI.Testing` exports `newContext` and `runFrame ctx input view` (`runFrameE` for emit views), alongside `NanoUI.Testing.Harness` (`warmup2`, `clickPair`, `held`, `spanRect`). Warm up before clicking `respRect`, set `tooltipDelay = 0` in tooltip tests, use `newWakeSignal` for deterministic task wakes, and call `cancelTasks` in teardown. Custom-drawn widgets produce no text spans, so test them by clicking and asserting on rendered screenshots.

## Quick pitfalls checklist

- `liftIO (newVar ...)` inside a view allocates every pass; move it to setup.
- Unguarded side effects run every frame; wrap them in `whenM (button ...)`.
- Conditional widgets or `useVar` calls need `scope`; dynamic list rows need `withKey` with stable IDs.
- Call overlays unconditionally every frame with a `Var Bool`, never inside `when`.
- `useTask` inside a tab or branch cancels whenever that view is hidden.
- Top-level unconditional `keepAnimating` creates a busy loop.
- Writing computed `NaN` to a `Var Float` redraws forever (`NaN /= NaN`); guard float writes.
- A custom widget content key missing an input freezes on stale pixels.
- Changing labels without `fixedW` jitter siblings; wide list cells without `fillW` trigger horizontal scrollbars.
- Nesting `scrollArea`, `panel`, or `card` inside a popup creates double borders; let one level own the chrome.
- Focus queries at the top of the view always return `False`; run them at the end.
- Do not put Escape behind the same text-editing guard as Ctrl+A or Delete.
- Custom widgets reading `inputChars` must call `useInputMethod` every focused frame.
- Measure long shaped text runs with `lineWidthUi` rather than per-glyph advance times count.
- Parse documents and run heavy derivations outside the view, updating only when inputs change.
- Put `readVar` below the widget that edits it to avoid an extra pass per change.
- Always compile executables with `-threaded`.
