# Using nano-ui

## Build a windowed application

Use GHC 9.10 through 9.14 (9.14 is used for development) and Cabal. Save the
counter example from the README as `Main.hs` and create `counter.cabal` next to
it:

```cabal
cabal-version: 3.4
name: counter
version: 0.1.0.0
build-type: Simple

executable counter
  main-is: Main.hs
  default-language: GHC2024
  ghc-options: -threaded
  build-depends:
    base >=4.20 && <4.23,
    text ^>=2.1,
    nano-ui ^>=0.2,
    nano-ui-sdl ^>=0.2
```

Install SDL3 and SDL3_ttf 3.2 or later with `pkg-config` metadata available
(`pkg-config --modversion sdl3 sdl3-ttf` should locate both), then run the
application with `cabal run counter`.

For a bitmap-font application without SDL dependencies, use `nano-ui-rgfw` and
`runRgfwApp` instead (see
[its README](https://github.com/goolord/nano-ui/blob/main/packages/nano-ui-rgfw/README.md)
for its native windowing libraries).

## What runs each frame

A `NanoUI` action describes the current interface for a frame. Each widget call
appends layout nodes, reads the current frame's input, and returns an
interaction result. Once the action finishes, layout is solved, pointer and
keyboard interactions are resolved against the computed geometry, and damaged
regions are painted. Writing to a `Var` that was already read earlier in the
same frame triggers a second pass within the same frame, with one-shot input
events cleared so clicks and key presses are not repeated.

Because the UI action can run more than once per frame, load files and allocate
long-lived resources in `IO` before entering the backend runner, and guard
side-effecting actions behind event checks such as
`whenM (button "Save") saveDocument`.

For Elm-style model/update applications, `NanoUI.Emit` (from `nano-ui-emit`)
wraps standard widgets to emit typed messages in `NanoUIE msg a`. The reducer
runner (`runSdlAppReduce`, `runRgfwAppReduce`) applies all emitted messages in
order after the frame's UI pass completes. If the model changes, a follow-up
frame is scheduled automatically. Lift ordinary `NanoUI` actions with
`Emit.liftNanoUI` and wrap layout containers with `Emit.withNanoUI` (for
example, `Emit.withNanoUI column`).

Use `Emit.emitWhen (button "Save") Save` for click activations,
`Emit.emitChanged (slider 0 100) volume SetVolume` for value changes, and
`Emit.emitEdited textArea' notes SetNotes` when you only want a message on user
edits. The same adapters work with configured and custom widgets.

## Values and interaction state

Mutable state is held in a `Var`. Input widgets take the `Var` to display and
update directly, while internal interaction state (caret position, text
selection, drag anchor, open dropdown state) is kept in the internal widget
store.

```haskell
nameField :: NanoUI ()
nameField = do
  name <- useVar ""
  textInput name
  greeting <- readVar name
  label ("Hello, " <> greeting)
```

A `Var` can be created in three ways:

- `useVar initial` allocates a positional `Var` inside `NanoUI` without prior
  setup. It supports `Int`, `Double`, `Float`, `Bool`, `Text`, and `SortCol`
  (`useEnumVar` supports `Enum` types). The `initial` value is used only when
  the slot is first initialized; to reset the value later, call `writeVar`.
- `newVar initial` allocates a `Var` of any type in `IO`. Create it once during
  setup, store it in an application record, and pass it into your UI action.
  Each call to `newVar` creates an independent `Var`; passing the same `Var` to
  multiple places shares the state.
- `VarMap` stores one `Var` per key for dynamic lists: `varAt` looks up or
  allocates the `Var` for a row key, and `dropVar` removes it.

```haskell
data Editor = Editor {document :: Var TextDocument}

newEditor :: IO Editor
newEditor = Editor <$> newVar emptyDocument

editorView :: Editor -> NanoUI ()
editorView editor = textAreaDocument (document editor)
```

Use `readVar` to read the current value, `writeVar` to replace it, and
`modifyVar` to apply a pure function to the latest value (so multiple updates
in the same frame compose cleanly). Writes that do not change the value (`==`)
do nothing. `readVar` can also be used inside event handlers: reading a `Var`
and then writing to it costs at most one extra pass on that frame. To check
whether an input widget modified its `Var` on the current frame, inspect
`respChanged` on the widget's primed variant (e.g. `textInput'`).

To read a `Var` without recording a read dependency (for example, when comparing
the previous frame's value against the current one), use `peekVar` from
`NanoUI.Var`. From `IO` or background threads, use `readVarIO`, `writeVarIO`,
and `modifyVarIO`, then invoke the callback returned by `askWake` to wake the
event loop. Never call `liftIO (newVar initial)` inside a UI action, as that
allocates a fresh `Var` on every pass.

When a `Var` holds a record with a `Generic` instance, `OverloadedRecordDot`
projects a `Var` for individual fields: `settings.volume` is a `Var Float` that
reads and writes only that field (e.g. `slider 0 100 settings.volume`). Custom
projections can be defined with `zoomVar` (getter and setter) or `zoomVarL`
(van Laarhoven lens). For lists stored in a `Var [a]`, `forEachVar key list body`
renders each item inside `withKey` with an element `Var` that tracks its key
across reorders; `withElemVars` provides the same per-element `Var`s when you
filter or sort the list yourself before rendering.

`constVar x` creates a read-only `Var` where writes do nothing, which is useful
for displaying a derived value in an input widget and handling edits through
the primed widget's `Response`. To run a `Var`-based widget against a plain
value and receive the updated value back, use `withValue value widget`.

### What a write costs

Each frame runs the UI action, solves layout, and paints. If a `Var` is written
after already being read earlier in the same pass (via `readVar` or a bound
widget), the UI action is re-run once before layout so earlier widgets reflect
the updated value (with one-shot inputs cleared so clicks and key presses do not
fire twice). If a `Var` is written before anything reads it, no extra pass is
needed: placing `readVar` below the input widget that edits it keeps typing to a
single pass. If a `Var` is only bound to an input widget and not read elsewhere
with `readVar`, only that widget is repainted on change; if `readVar` was
called on it, the full window is repainted.

Read/write tracking is granular up to two levels of nesting (a record field or
list element, plus a nested field within it). With a record in a single `Var`,
`m.title` and `m.volume` are tracked separately, so reading `m.title` above a
slider bound to `m.volume` does not trigger a second pass when the slider moves.
Reading the entire record `m` or projecting through `zoomVar` marks all fields
as read. `forEachVar` only reads the list's keys and order, so editing a single
row's `Var` does not invalidate earlier rows; `withElemVars` returns the
elements directly and therefore marks the whole list as read. Rows are told
apart by one of 63 bits of their key's hash, so past about 63 rows, or for keys
that share a bit (such as `Int` keys 64 apart), a row's edit can still cost a
second pass, and a full repaint when the rows `readVar` the field it edits.

Primed widget variants (`button'`, `textInput'`, `slider'`) return a `Response`:
`respChanged` is `True` when the value was edited, `respSubmitted` when
committed (such as pressing Enter in a text input), and `respClicked` when
activated. `respPressed` is `True` while the mouse button is held down on the
widget, rather than on a single-frame click.

`searchInput'` updates its `Var` on every keystroke but debounces
`respChanged`, so you can trigger expensive searches on `respChanged`.

## Stable identity

Widgets and positional `useVar` calls each use their own sequential counter
within a container scope. Keep call order consistent across frames, and wrap
conditional widgets or `useVar` calls in `scope`:

```haskell
details :: Bool -> NanoUI ()
details showDetails = column $ do
  scope $ when showDetails $ label "Additional details"
  nameField
```

`scope` reserves a single slot in the parent sequence regardless of whether its
body runs, so `nameField` keeps the same ID below it. Because `useVar` uses a
counter separate from widget IDs, conditional widgets alone do not shift
`useVar` slots, but conditional `useVar` calls still require `scope`.

For dynamic or reorderable lists, wrap each item in `withKey` using a unique,
stable key (not the list index):

```haskell
names :: [(Int, Text)] -> NanoUI ()
names entries = column $
  forM_ entries $ \(itemId, name) -> withKey itemId $ do
    checked <- useVar False
    checkbox name checked
```

State kept under a key, by `useVar` or by a widget such as a text field, lasts
as long as the context, even once the key is no longer shown, so hidden content
keeps its state. For a collection whose keys keep coming, such as a chat log,
keep each row's state in a `VarMap` rather than `useVar`, and drop the rows
that are gone with `retainVars`.

## Layout and coordinates

All layout dimensions use logical pixels, with `(0, 0)` at the top-left corner,
`x` increasing to the right, and `y` increasing downward. Backends scale logical
pixels to physical display pixels automatically; custom widgets should work in
logical coordinates.

Layout modifiers are `Layout -> Layout` functions composed with `(.)`:

```haskell
columnWith (gap 8 . padAll 12 . fillW) $ do
  label "Settings"
  buttonWith (fixedW 120) "Apply"
```

When two modifiers set the same property, the leftmost modifier takes
precedence. `fillW` and `fillH` distribute available space across growing
siblings; `minW` and `maxW` clamp width; `percent 50` requests 50% of the
parent's available width; `tight` zeroes padding while preserving `gap`.

Container flow along a `Row` or `Column` defaults to a single `Line`, and can
be changed to `Wrap` or `Layered`:

- `layers` stacks its children in a shared box sized to the largest child; each
  child is placed according to its alignment, and later children are drawn on
  top. The `layered` modifier applies the same behaviour to a panel or card,
  e.g. `panelWith (layered . fixedWH 240 160)`.
- `wrap` wraps a row or column onto additional lines when children exceed the
  available main-axis size, e.g.
  `rowWith (wrap . gap 6 . lineGap 4) (mapM_ chip tags)`. `lineAlign LinesCenter`
  or `lineAlign LinesEnd` aligns wrapped lines along the cross axis. Inside a
  shrink-to-fit parent, give a wrapping row an explicit `maxW` or `fixedW`.
- `pinAt x y` takes a child out of normal flow and positions it over its
  siblings at offset `(x, y)` relative to its alignment anchor in the parent's
  content box (top-left by default, or bottom-right with
  `pinAt (-16) (-16) . alignEnd . alignBottom` for a floating action button).
  `pinAt 0 0 . grow` covers the parent's content box without affecting the
  parent's size, which is useful for scrims or drop-target highlights.
- `aspect r` constrains height from width at ratio `r` (e.g.
  `box (fillW . aspect (16 / 9)) c`).

When layers or pinned nodes overlap, pointer input goes to the topmost
interactive control (buttons, inputs, sliders, custom drawings) at the cursor,
while panels, labels, images, and layout containers do not intercept pointer
events. Override this behaviour with `pointer PointerBlock` (so a panel or
overlay scrim blocks pointer events over its full rectangle) or
`pointer PointerPass` (so a decorative drawing does not intercept pointer
events):

```haskell
layers $ do
  list
  panelWith (pointer PointerBlock . alignEnd . fixedW 240) details
```

Scroll containers require a bounded viewport size, such as
`scrollWith (fixedH 240 . fillW) body`. `scrollArea` also returns the scroller's
`WidgetId` for programmatic scroll commands like `scrollToUi`, or `scrollToEnd`
with the context from `askContext` (in `NanoUI.Monad`). For large virtualized
lists, use `getScrollMetricsUi` to query the current scroll offset and viewport
size so you only build the visible rows.

To defer work until a widget enters the viewport, wrap it in `sensor` (or pass
its ID to `useVisibility`). The returned visibility record contains the widget's
previous-frame bounds (`visBounds`), its visible intersection on screen
(`visRect`), and `becameVisible` (`True` on the frame the widget enters view).
In `sensorConfigured`, `sensorAnticipate` expands the check by a given number of
pixels outside the viewport, and `sensorDelay` requires the widget to stay in
view for a given duration before `becameVisible` turns `True` (so scrolling
quickly past a list does not trigger loads for intermediate items):

```haskell
lazyImage :: Var (Maybe ImageId) -> NanoUI ImageId -> NanoUI ()
lazyImage pictureVar load = do
  picture <- readVar pictureVar
  let config = defaultSensorConfig {sensorAnticipate = 200, sensorLayout = fixedWH 96 96}
  (vis, _) <- sensorConfigured config $
    maybe (label "Loading") (image (fixedWH 96 96)) picture
  when (becameVisible vis && isNothing picture) $
    writeVar pictureVar . Just =<< load
```

Because layout is solved after the UI action completes, `respRect` returns the
widget's rectangle from the previous frame and is empty on the first frame a
widget appears. Keep this in mind when positioning custom popups or writing
headless pointer tests.

## Themes, overlays, and input

Use `styled` to modify the active theme for a subtree, or `themed` to replace
it completely. Theme scopes affect colours and painting only, not layout
metrics (font sizes are set via layout modifiers). `disabledWhen condition`
disables interaction and applies disabled styling across a subtree while
preserving layout geometry and widget state.

Semantic status colours are applied with tones: `fontTone Warning` sets a
label's text colour (combining with font faces like `fontMono . fontTone Danger`),
`tone Danger` styles buttons, and `toneColor` retrieves the `Color` value for
custom drawing. Legacy helpers (`fontMuted`, `fontDanger`, `primary`,
`destructive`, `success`, `warning`) are aliases for the corresponding tones.

To follow the operating system's light or dark mode, pass a theme selector such
as `lightDark defaultLightTheme defaultTheme` (which falls back to the dark
theme on platforms that do not report system appearance, such as RGFW). You can
apply it inside your UI action:

```haskell
setUiTheme . lightDark defaultLightTheme defaultTheme =<< systemAppearance
```

Or configure it via `followSystemTheme ctx` (a view gets `ctx` from
`askContext`), or at startup via `sdlAppThemeFor` or `optThemeFor`. Calling `setTheme` or `setUiTheme` switches to a fixed theme,
and `themeAppearance` returns whether a `Theme` is light or dark.

Modals, floating windows, and popups take a `Var Bool` controlling whether they
are open. While the `Var` is `True`, the overlay is shown; pressing Escape,
clicking outside, or clicking the close button writes `False` to the `Var`. A
floating `window` closes only by its close button.
Call the overlay unconditionally every frame so sibling widget IDs remain
stable:

```haskell
confirm <- useVar False
whenM (button "Delete...") (writeVar confirm True)
modal confirm "Delete the file?" $
  whenM (button "Delete") (deleteFile >> writeVar confirm False)
```

If the open state is derived or the dialog must not close until explicitly
confirmed, pass a `constVar` and handle dismissal via `respClicked` on the
primed variant (`modal'`, `window'`, `popup'`).

Tooltips require no state variable: passing a widget's response to `tooltip`
(or wrapping the widget with `withTooltip`, for a tooltip built from widgets)
displays the tooltip after the pointer rests on the target for `tooltipDelay`
(0.5s by default) and hides it when the pointer leaves or a mouse button is
pressed.
Tooltips also trigger on disabled widgets. `tooltipConfigured` customizes the
delay, placement, and offset; `PlacementAtCursor` positions the tooltip at the
cursor (or at the click position for popups and context menus).

Standard widgets read layer-routed input via `askInput`, where covered layers
receive no pointer events. `askFrameInput` returns raw window-wide input (used
for outside-click dismissal) and should not be used by ordinary controls.

To control keyboard focus programmatically, pass a widget's ID to
`requestFocus` (e.g. `when findPressed (requestFocus (respId resp))`, or
`requestFocus =<< currentId` immediately before the widget call). `focusNext`
and `focusPrevious` cycle focus like Tab and Shift+Tab, `clearFocus` removes
focus, and `isFocused wid` checks whether that widget is focused. Focus
requests take effect at the end of the frame (clearing any active selection or
open menu on the previously focused field and skipping disabled or
modal-blocked widgets), whereas `releaseFocus` drops focus from a specific
widget immediately during the current pass. Clicking focuses text fields,
selects, and focusable custom widgets (such as a knob, or a canvas with
`canvasFocusable`).

`withCursorShape` sets the default cursor shape for a subtree (such as
`UiCursorCrosshair` over a canvas, `UiCursorNotAllowed` over disabled controls,
or `UiCursorHidden` over a video player). Custom widgets can set their own
cursor via `widgetCursor` based on their bounds and pointer position (returning
`UiCursorDefault` to defer to the enclosing `withCursorShape` scope), and that
cursor is retained throughout an active drag even if the pointer leaves the
widget's bounds.

Keyboard state is exposed in `inputKeys` (pressed this frame, including key
repeats), `inputKeysNew` (initial presses only, excluding repeats),
`inputKeysReleased`, `inputKeysHeld`, and `inputChars` (committed text).
Printable keys are represented as `KeyChar` with the unmodified character (so
Ctrl+S produces `KeyChar 's'` with `modCtrl` set and no entry in `inputChars`).
Because a single frame's `Input` does not record the relative ordering of text
and command keys, the event runner flushes a frame whenever a command key or
chord is followed by text, another key, or a modifier change, so any text typed
before a shortcut is processed on the preceding frame. Helper predicates
`pressedIn`, `pressedOnceIn`, `releasedIn`, and `heldIn` query keys or mouse
buttons directly on an `Input` record (e.g.
`sdlAppShouldQuit = pressedOnceIn KeyEscape`).

Keyboard shortcuts are built with modifiers (`ctrl`, `shift`, `alt`, `super`,
and `cmdOrCtrl`, which maps to Command on macOS and Ctrl elsewhere) combined
with `key` via `<>` from `NanoUI.Shortcut`:

```haskell
import NanoUI.Shortcut

whenM (shortcut (ctrl <> key 's')) save
whenM (shortcut (cmdOrCtrl <> shift <> key 'p')) (setPaletteOpen True)
whenM (shortcut (key (KeyF 5))) refresh
```

Modifier-only chords without a key never fire. `parseShortcut` parses chords
from configuration strings such as `"C-s"`, `"M-S-p"`, `"A-<Enter>"`, or
`"<F5>"`.

`shortcut` triggers on press (including key repeats) for the first matching
`shortcut` call in the frame; `shortcutOnce` and `keyPressedOnce` ignore
auto-repeats. Shortcuts and view-level key queries (`keyPressed`,
`keyReleased`, `keyHeld`) are suppressed when blocked by a modal, inside
`disabledWhen`, or when handled by the focused widget (for example, typing or
Ctrl+A in a text input, Enter/Space on a focused button, or arrow keys on a
focused slider or list; modified chords like Ctrl+Enter or Alt+Left still pass
through to shortcuts). An Escape that closes an open dropdown, a focused combo
box's list or a text field's edit menu reaches neither. Custom widgets specify
which keys they handle via `widgetKeys` (for example, a terminal widget sets
`KeysAll`); one that claims Tab this way keeps focus when Tab is pressed.
`menuItemShortcut` only matches while its menu is open; to make the shortcut
active while the menu is closed, also declare a top-level `shortcut` before the
menu.

Built-in text inputs and text areas handle IME composition automatically: the
focused field renders the active pre-edit string (`inputComposition`) at the
caret and updates its `Var` only when the composition is committed. While a
composition is active, key events are suppressed so shortcuts do not trigger.
Custom text-entry widgets (such as a terminal) can opt into IME composition by
calling `useInputMethod` on every frame they hold focus, passing the caret
rectangle and input purpose (`InputNormal`, `InputSecure`, or `InputNumeric`),
and rendering the returned composition string. Backends like SDL only enable OS
text input on frames where `useInputMethod` is called, so any UI that reads
`inputChars` outside a built-in text field must call `useInputMethod` as well.

Mouse buttons are represented by `MouseButton` (`MouseLeft`, `MouseRight`,
`MouseMiddle`, `MouseBack`, `MouseForward`, and `MouseOther n`). On a widget's
`Response`, `respHeldWith` lists the buttons pressed on that widget and still
held down, and `respClickedWith` lists the buttons pressed and released on it
(`respClicked` is `True` on standard activation via left click or Enter).
Pressing a button outside a widget and dragging across it does not count as
pressing or clicking that widget. For scope-wide mouse queries (suppressed
behind modals and inside `disabledWhen`), use `mousePressed`, `mouseReleased`,
and `mouseHeld`:

```haskell
whenM (mousePressed MouseBack) goBack
tab <- button' "Report"
when (respClickedWith MouseMiddle tab) closeReport
```

`mouseArea` wraps a layout subtree and returns a `Response` for the combined
region. It registers hover and mouse button clicks over the region unless an
interactive child widget inside it handled the click:

```haskell
(_, item) <- mouseArea (fillW . gap 6) $ do
  label name
  muted path
when (respClickedWith MouseMiddle item) (openInNewTab path)
when (respClickedWith MouseRight item) (showMenuFor path)
```

Closable tabs also close on middle-click.

## Animation and background work

Backends block waiting for events when no frame is scheduled. Call
`keepAnimating` from any on-screen widget that needs continuous animation
frames. For periodic or delayed updates (clocks, debounces, timers), call
`wakeAfter` on each frame that still requires a future wake-up. Do not force
redraws by marking the context dirty every frame, as that creates an unpaced
busy loop.

Long-running or blocking operations (such as file or network I/O) should run on
a background thread via a `Task` handle allocated during setup (`task <- newTask`):

```haskell
status <- useTaskStatus task path (T.readFile path)
case status of
  TaskRunning _ -> label "Loading..."
  TaskDone contents -> label contents
  TaskFailed e _ -> danger (T.pack (displayException e))
```

`useTaskStatus` returns whether the task is running, completed with a result,
or failed with a synchronous exception (`TaskFailed`); `useTask` returns
`Maybe a` (`Nothing` until the first result is produced). A task runs once per
key and wakes the event loop when it finishes. When the key changes, the
previous thread is cancelled with an asynchronous exception and a new thread is
spawned; while the new key is still running, both `useTaskStatus` and `useTask`
continue to provide the previous key's result (`TaskRunning (Just old)`) so UI
lists do not flash empty while re-querying. To add a retry trigger, include an
attempt counter in the key, e.g. `useTaskStatus task (path, attempt)`. Because
cancelled tasks receive an asynchronous exception and may briefly overlap with
the new task, use `bracket` inside tasks that open files or hold external
resources. Task results are evaluated to WHNF on the worker thread. Compile
applications with `-threaded`.

Skipping a `useTask` or `useTaskStatus` call for a full frame cancels the
running task, so place task calls outside conditional branches or tabs if the
work should continue while hidden. Task calls do not consume widget IDs, so
calling them conditionally does not affect sibling widget identity:

```haskell
when previewOpen (void (useTask task path (renderPreview path)))
```

For streaming data (sensor readings, download progress, token-by-token
responses), allocate `stream <- newStream` during setup and pass a producer to
`useStream`. The producer is passed an atomic `update` function that modifies
the stream's state and wakes the event loop; multiple updates between frames
coalesce into a single frame. The state resets to the initial value whenever
the key changes, and the producer thread runs for as long as `useStream`
continues to be called each frame:

```haskell
sensorView stream = do
  reading <- useStream stream () Nothing $ \update -> forever $ do
    r <- readSensor
    update (const (Just r))
  label (maybe "--" (T.pack . show) reading)
```

To accumulate all emitted items into a list, fold each item into the state
(`update (r :)`).

The `Context` and its arenas must only be accessed from the UI thread. If an
external thread managed outside `useTask`/`useStream` updates shared state, it
should write the update and then invoke the wake callback obtained from
`askWake`; multiple wakes before the next frame coalesce into a single frame
with a full-window redraw.

Note that low-level damage requests only mark screen regions for repainting and
do not wake a blocked event loop on their own; external updates must trigger a
wake as well.

## Custom drawing and large text

`drawing` and `customWidget` construct `DrawOp` lists from solved logical
bounds. When caching a custom drawing with a content key, include all dynamic
inputs (including animation state) in the key so visual changes are not skipped;
unkeyed custom widgets rebuild and diff their `DrawOp` list on every frame.

Vector paths are constructed using `NanoUI.Path` (typically imported qualified
as `P`): build paths with `P.moveTo`, `P.lineTo`, `P.cubicTo`, `P.arc`, etc.,
and render them with `drawPath` / `drawPathWith` (for fills) or
`drawStrokePath` / `drawStrokePathWith` (for strokes). `drawPathWith` takes a
fill rule (`P.NonZero` or `P.EvenOdd`, where inner subpaths can cut holes such
as rings) and a paint (`P.Solid` or `P.Linear` gradient). `drawStrokePathWith`
takes a `P.Stroke` specifying stroke width, line caps, joins, miter limit, and
dash pattern. Use `withTransform` to translate, rotate, or scale canvas
operations, and `withClip` to clip them. Within a custom widget, run canvas
blocks via `runCanvasFor` (or use the `canvas` / `canvasConfigured` widget
helpers) so curve tessellation adapts to the display scale; `drawContext`
returns the current `Theme` along with the widget's hover and press state:

```haskell
import NanoUI.Path qualified as P

gauge :: Float -> NanoUI Response
gauge level =
  canvasConfigured defaultCanvasConfig {canvasLayout = fixedWH 80 80 defaultLayout, canvasContent = contentKey [level]} $
    \(Rect x y w h) -> do
      cdc <- drawContext
      let c = V2 (x + w / 2) (y + h / 2)
          theme = cdcTheme cdc
      drawPathWith P.EvenOdd (P.circle c 36 <> P.circle c 26) (P.Solid (themeSeparator theme))
      drawStrokePathWith (P.stroke 10) {P.strokeCap = P.RoundCap}
        (P.arc c 31 (-pi / 2) (2 * pi * level))
        (P.Solid (if cdcHovered cdc then themeAccent theme else themeMuted theme))
```

Path fills do not compute self-intersections: intersecting subpaths are filled
independently, and self-intersecting contours may not fill as expected. Path
strokes are centred on the path geometry, whereas `drawStrokeRoundedRect` and
`drawStrokeCircle` inset their border inside the shape bounds (matching panel
borders).

For multi-line text editing, `textArea` operates on a `Var Text` and rejoins
lines on each edit. For large documents, use `textAreaDocument` with a
`Var TextDocument`, which shares unchanged lines across edits; convert back to
a single `Text` with `documentText` only when saving or exporting.

Backends receive borrowed `DrawData` vertex/index buffers that are reused on
the next frame, so backends must consume or copy them during presentation.
Native font and renderer handles must not outlive their backend session.

## Images

For scoped image lifetimes, allocate `owner <- newImageHandle` during setup and
call `useImageRgba owner key w h pixels` in your UI action. The RGBA buffer is
uploaded on the first frame for a given key, and the texture slot in the image
atlas is freed automatically if a frame completes without calling `useImageRgba`
for that handle, or when the key changes. For static images kept for the entire
session, allocate an ID with `freshImageId` and upload once with
`registerImageRgba`; `releaseImageRgba` frees one you no longer need.

`image` stretches an image to fill its layout bounds. `imageConfigured` takes
an `ImageConfig` supporting CSS-style `ContentFit` (`FitFill`, `FitContain`,
`FitCover`, `FitScaleDown`, `FitNone`), alignment, pixel sub-rectangle cropping
(`icCrop`), zoom (`icScale`), opacity, and rotation (`RotateSolid` fits the
rotated bounding box into layout bounds; `RotateFloating` rotates in place and
clips). Unconstrained layout axes default to the image's intrinsic dimensions,
and setting one axis with `FitContain`/`FitCover` preserves aspect ratio on the
other (so `icLayout = fillW` scales height proportionally). `svgIconConfigured`
applies the same fitting rules to SVG icons (`svgIcon` defaults to
`FitContain`), and `fitRect` computes the fitted rectangle directly. On a
canvas, draw images with `drawImage`, `drawImageUV`, or `drawImageWith` (which
takes an `ImageDraw` specifying destination bounds, UV crop, rotation, tint,
and opacity).

```haskell
thumbnail :: ImageHandle FilePath -> FilePath -> Int -> Int -> ByteString -> NanoUI ()
thumbnail owner path w h pixels = do
  photo <- useImageRgba owner path w h pixels
  for_ photo $
    imageConfigured defaultImageConfig {icLayout = fixedWH 120 90, icFit = FitCover}
```

## Inspecting layout

Calling `explainLayout True` draws depth-coloured outlines around every layout
node and highlights the node under the cursor. `explainedNode` returns metadata
for the hovered node: its `WidgetId`, solved rectangle, padding, width/height
`Sizing` constraints, gap, layout direction, `Flow`, pin offset, and
`PointerMode`. To limit the debug overlay to a specific subtree, wrap that
subtree in `explainScope`. You can also enable the overlay at startup via
`sdlExplainLayout`, `optExplainLayout`, or `--explain` in the SDL demo, or
toggle it from a checkbox in a debug panel:

```haskell
on <- explainingLayout
(_, on') <- withValue on (checkbox "Outline layout nodes")
explainLayout on'
```

## Window management

Initial window parameters are configured with `WindowSettings` across all
backends (`sdlWindowSettings` in `SdlOptions`, `optWindow` in `RgfwOptions`).
Dimensions in `WindowSettings` are specified in logical layout units and scaled
by the backend when the window opens:

```haskell
main :: IO ()
main =
  runSdlApp
    defaultSdlOptions
      { sdlWindowSettings =
          defaultWindowSettings {wsTitle = "Notes", wsSize = Size 900 600, wsMinSize = Just (Size 480 320)}
      }
    notesView
```

`askWindow` returns the window's state at the start of the frame (logical size,
display scale, screen position, keyboard focus, and minimized/maximized/fullscreen
mode). UI actions that call `askWindow` automatically receive a new frame when
window state changes. Declarative setters (`setWindowTitleUi`,
`setWindowIconUi`, `setWindowMinSizeUi`, `setWindowMaxSizeUi`,
`setWindowOpacityUi`, `setWindowModeUi`) are no-ops when the requested value
matches the current state, so they can be called unconditionally each frame.
Imperative window actions (`moveWindowUi`, `centerWindowUi`, `resizeWindowUi`,
`minimizeWindowUi`, `maximizeWindowUi`, `restoreWindowUi`, `toggleMaximizedUi`)
apply on every call and should only be triggered from events. Transparent
windows (`wsTransparent = True` combined with a translucent theme
`windowColor`) are supported on the SDL backend when a desktop compositor is
present; RGFW windows are always opaque and ignore `wsOpacity` and
`setWindowOpacityUi`.

By default, clicking the window's close button exits the session immediately.
With `wsExitOnCloseRequest = False`, close requests are not handled
automatically: `winCloseRequested` on `askWindow` becomes `True` on the next
frame, and the window stays open until `quitUi` is called:

```haskell
asking <- useVar False
whenM (winCloseRequested <$> askWindow) $
  if unsaved then writeVar asking True else quitUi
modal asking "Discard your changes?" $ do
  whenM (button "Discard") quitUi
  whenM (button "Keep editing") (writeVar asking False)
```

`requestScreenshot` registers a callback that receives a `Screenshot`
(containing the frame's `RgbaPixels` and pixel-per-logical-unit scale, or
`Nothing` when running headlessly) once the current frame finishes presenting.
Call `requestScreenshot` from an event handler and keep the callback fast (or
offload encoding to a worker thread), as it runs before the next frame starts.
To capture and save a screenshot inside a background task, obtain a trigger
with `askScreenshot`:

```haskell
shoot <- askScreenshot
saved <- useTaskStatus task shots (shoot >>= traverse_ (savePng "shot.png"))
```

`useScreenshot task key` captures a screenshot per key for displaying directly
in the UI. To construct `RgbaPixels` for window icons, use `rgbaPixels` (which
checks that the width and height are positive and the buffer is exactly
`w * h * 4` bytes, and returns `Nothing` otherwise).

## Writing a backend

`NanoUI` exports the user-facing GUI API, while `NanoUI.Backend` exports the
types and functions used to implement a windowing and rendering backend:
`emptyInput` and event-folding helpers, `FontBackend` callbacks for text
measurement and shaping, `Damage` regions for partial presentation, and default
widget metrics. Because held mouse buttons, modifier state, cursor position,
and window size carry over between frames, pass the previous frame's `Input`
through `clearEphemeral` (which clears one-shot presses and text while
preserving held state) and fold new platform events into the result rather than
starting from `emptyInput` each frame. `newContext` / `newPixelContext` (from
`NanoUI.Backend`) allocate the persistent session context, and `NanoUI.Runner`
provides the shared event-loop state machine. See `NanoUI.Backend.Sdl` and
`NanoUI.Backend.Rgfw` for complete implementations.

Fold keyboard events into `Input` with `applyKey` (recording printable keys as
their unmodified `KeyChar`, appending committed text to `inputChars`, and
treating key repeats as presses) and IME pre-edit updates with
`applyComposition`. After each frame, inspect `textInputArea` to determine
whether a widget is requesting text input, its `InputPurpose`, and the caret
rectangle for positioning the IME candidate window; enable or disable OS text
input accordingly.

Initialize the native window from `WindowSettings` (title, logical size, window
mode, resizability, transparency). Before the first frame, install a
`WindowHost` callback record with `installWindowHost` constructed via record
update on `defaultWindowHost` (so fields added in future versions have sensible
defaults). Register a thread-safe wake callback with `setWakeLoop`, report OS
light/dark appearance changes with `setSystemAppearance`, update window metrics
before each frame with `reportWindowState`, and fulfill pending screenshot
requests after presentation with `answerScreenshots`. `runSessionLoop` manages
close requests and `quitUi` automatically; custom event loops can drive them
with `requestWindowClose`, `clearWindowClose`, and `quitRequested`.

## Headless tests

`NanoUI.Testing` exports `newContext` and `runFrame` for running frames without
a window or GPU. `runFrame` returns the UI action's result, the frame's
`DrawData`, and whether a follow-up frame was requested; for `NanoUIE` views,
`runFrameE` from `nano-ui-emit` also returns the emitted messages.
Provide an explicit window size and `dt` in the test `Input`. When targeting a
widget by its `respRect`, run an initial warm-up frame so layout geometry is
populated before sending press and release frames.

`NanoUI.Testing.Harness` provides test helpers including `warmup2`,
`clickPair`, `clickPairWith` (e.g. `clickPairWith MouseMiddle base pos`),
`runClick`, and text-span lookup functions. `held var widget` runs a widget
bound to a `Var` and returns both the widget result and the updated `Var`
value. `lastFramePasses` returns how many passes the previous frame executed
(note that when a backward write triggers a second pass in the same frame,
one-shot click and change flags from the first pass are cleared on the second
pass's return value).

Because tooltip hover delays use the wall clock, set `tooltipDelay = 0` in
tests. Use `newWakeSignal` to wait for background task wake-ups deterministically,
and call `cancelTasks` (from `NanoUI.Backend`) during test teardown to stop any
background threads still running.

Headless tests verify layout, state, and input logic; when modifying rendering,
font handling, native dialogs, or DPI scaling, run the native backend test
suites and demos as well.
