# Using nano-ui

## Build a windowed application

Use GHC 9.14 and Cabal. Save the counter from the README as `Main.hs` and
create `counter.cabal` beside it:

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
    base ^>=4.22,
    text ^>=2.1,
    nano-ui ^>=0.1,
    nano-ui-sdl ^>=0.1
```

Install SDL3 and SDL3_ttf 3.2 or later, with pkg-config metadata available.
`pkg-config --modversion sdl3 sdl3-ttf` should find both. Run the application
with `cabal run counter`.

For a bitmap-font application, use `nano-ui-rgfw` and `runRgfwApp` instead.
Its package README lists the native windowing libraries it needs.

## What runs each frame

A view describes the current interface. Each call registers layout nodes,
reads input, and returns a result. Layout is solved after the view runs,
then the frame resolves interactions and paints. A state-hook update can
cause an additional view pass within the same frame. One-shot input is
removed from that pass so it does not repeat a click or key press.

Code in the view can therefore run more than once. Load files and allocate
long-lived resources before entering the backend runner. Guard one-shot IO
with an event such as `whenM (button "Save") saveDocument`.

For model-driven applications, `NanoUI.Emit` adapts ordinary widgets to emit
messages. A backend reducer runner applies matching messages in order after
the view has run. A changed model requests another frame; the drawing from
the current frame still represents the model passed into it.

Use `Emit.emitWhen (button "Save") Save` for activation,
`Emit.emitChanged (slider 0 100) volume SetVolume` for changed values, and
`Emit.emitEdited textArea' notes SetNotes` when the response must also report
an edit. The same adapters accept configured and custom widgets.

## Values and interaction state

Inputs are controlled: pass their current value and retain the returned
value for the next frame. The widget keeps interaction details such as the
caret, selection, drag anchor, or open dropdown.

```haskell
nameField :: NanoUI ()
nameField = do
  (name, setName) <- useText ""
  setName =<< textInput name
```

Hooks use their initial argument only while their slot has no value.
Changing `useText ""` to a different initial value at the same widget id is
not a reset. Use the setter to change state. `useState` supports other types
with `Eq` and `Typeable`; keep its type stable at each hook position.

A primed widget exposes a `Response`. Inputs return `(Response, value)`;
buttons and labels return the response itself. `respChanged` reports an
edit, `respSubmitted` reports a commit, and `respClicked` reports activation.
`respPressed` is a held-button state, not a one-frame press event.

`searchInput'` returns edited text immediately but debounces `respChanged`.
Store its text on every frame; use the change flag to trigger a search.

## Stable identity

Widgets and hooks share an id sequence within a container. Their call order
must remain stable between frames. Conditional content needs a scope:

```haskell
details :: Bool -> NanoUI ()
details showDetails = column $ do
  scope $ when showDetails $ label "Additional details"
  nameField
```

Here `when` comes from `Control.Monad`. The scope consumes one sibling id
whether its body is empty or not, so `nameField` keeps its state.

Use stable application keys for reorderable lists:

```haskell
names :: [(Int, Text)] -> NanoUI ()
names entries = column $
  forM_ entries $ \(itemId, name) -> withKey itemId $ do
    (checked, setChecked) <- useFlag False
    setChecked =<< checkbox name checked
```

Import `forM_` from `Control.Monad` and `Text` from `Data.Text`. Keys must be
unique among siblings. A list position is not a stable key when items move.

## Layout and coordinates

Lengths in views use logical pixels. The origin is at the top-left, x grows
rightward, and y grows downward. Backends map logical pixels to display
pixels. Custom widgets should not apply the display scale themselves.

Layout modifiers compose as functions:

```haskell
columnWith (gap 8 . padAll 12 . fillW) $ do
  label "Settings"
  buttonWith (fixedW 120) "Apply"
```

The rightmost modifier runs first. When two modifiers write the same field,
the leftmost wins. `fillW` and `fillH` share available space with other
growing children; `minW` and `maxW` constrain width. `percent 50` requests
half the available width. `tight` removes padding but keeps the child gap.

`stack` layers its children in one box as large as the largest, each placed
by its alignment; a later child draws over the earlier ones. The `wrap`
modifier flows a row onto a new line where the next child would not fit, as
in `rowWith (wrap . gap 6 . lineGap 4) (mapM_ chip tags)`; inside a
container that sizes itself to its content, bound the row with `maxW` or
`fixedW`. `pinAt x y` places a node over its siblings and out of their
flow, at that offset from where its alignment puts it in its parent's
content box: from the top-left corner by default, and from the bottom-right
one with `pinAt (-16) (-16) . alignEnd . alignBottom`, as a floating button
sits.

Where a stack or a pinned node draws one node over another, a control on
top (a button, field, slider or drawing) takes the pointer from whatever is
beneath it, while a panel, label, image or container lets the pointer
through to the controls beneath. `pointer PointerBlock` makes a node take
the pointer over its whole box, as a card or a scrim over a list must, and
`pointer PointerPass` makes a node and everything in it let the pointer
through, as a decorative drawing laid over controls should. A node never
takes the pointer from what it is inside:

```haskell
stack $ do
  list
  panelWith (pointer PointerBlock . alignEnd . fixedW 240) details
```

Give a scroller a bounded viewport, for example
`scrollWith (fixedH 240 . fillW) body`. `scrollArea` also returns its id for
commands such as `scrollToEnd`. Use `getScrollMetrics` to obtain the visible
range when building only the visible rows of a large collection.

To load something as it comes into view, wrap it in a `sensor` or watch its
id with `useVisibility`. A sensor reports the last frame's layout, as
`respRect` does; `becameVisible` holds once, on the frame it comes into view,
`sensorAnticipate` reports it that many pixels early, and `sensorDelay` only
once it has stayed in view that many seconds, so a list scrolled quickly
past loads nothing. `visRect` is the part on screen and `visBounds` the
whole widget. Here `load` registers an image and returns its id:

```haskell
lazyImage :: NanoUI ImageId -> NanoUI ()
lazyImage load = do
  (picture, setPicture) <- useState Nothing
  let config = defaultSensorConfig {sensorAnticipate = 200, sensorLayout = fixedWH 96 96}
  (vis, _) <- sensorConfigured config $
    maybe (label "Loading") (image (fixedWH 96 96)) picture
  when (becameVisible vis && isNothing picture) $
    setPicture . Just =<< load
```

During view construction, `respRect` uses recorded geometry from the prior
frame. It may be empty when a widget first appears. Keep this in mind when
anchoring popups or writing scripted pointer tests.

## Themes, overlays, and input

Use `styled` to modify the theme for a subtree and `themed` to replace it.
These scopes affect painting, not layout. Font-size changes belong in layout
modifiers. `disabledWhen condition` keeps widget geometry and state while
disabling interaction and applying disabled colours.

To follow the desktop's light or dark setting rather than `setTheme`, call
`followSystemTheme ctx light dark` or `followSystemThemeUi`, or set
`sdlAppFollowSystemTheme`; `defaultLightTheme` pairs with `defaultTheme`.
RGFW cannot read the setting, so it keeps the light theme. A later
`setTheme` stops following.

Modals, floating windows, and popups return close or dismissal requests.
The application owns their open flag and must update it. Keep calling the
overlay with that flag so its identity and later siblings remain stable.

Tooltips need no flag. One opens once the pointer has rested on its target
for `tooltipDelay`, half a second by default, and shuts when the pointer
leaves or a button goes down. A disabled widget has one too, which is where
to say why it is off. `tooltipConfigured` sets the delay, the placement and
the gap to the target; `PlacementAtCursor` follows the pointer, where for a
popup or context menu it opens at the anchor point.

Ordinary widgets read routed input with `askInput`. A covered layer receives
no pointer. `askFrameInput` is for window-wide handling, such as dismissing
a popup after an outside click; using it for an ordinary control bypasses
pointer routing.

To move keyboard focus from the view, name the widget by its response:
`when findPressed (requestFocus (respId resp))` focuses a search box on
Ctrl+F, and `requestFocus (WidgetId 0)` takes the keyboard off whatever has
it. Focus moves as Tab would, from the next frame; a widget Tab would skip,
such as one disabled or behind a modal, refuses it.

`withCursorShape` sets the pointer's shape over a subtree wherever the
widgets inside pick none, such as `UiCursorCrosshair` over a canvas,
`UiCursorNotAllowed` around disabled widgets, or `UiCursorHidden` over a
video. A custom widget picks its own with `widgetCursor`, from its rect and
the pointer, so a part of it can show another shape, and keeps it through a
drag that leaves it; its `UiCursorDefault` leaves the choice to the scope
around it. A backend shows the nearest shape the platform has.

Keys arrive in `inputKeys`, `inputKeysReleased` and `inputKeysHeld`, and the
text they type in `inputChars`. A key that types is a `KeyChar` of what it
types unmodified, so Ctrl+S is `KeyChar 's'` with `modCtrl` set and types
nothing. Bind a chord with `shortcut`. A chord is the modifiers `ctrl`,
`shift`, `alt`, `super` and `cmdOrCtrl` (Command on macOS, else Ctrl) and a
`key`, a character or a `Key`, put together with `<>`. These short names
come from `NanoUI.Shortcut`, which `NanoUI` leaves out; import it where
chords are written, or qualified:

```haskell
import NanoUI.Shortcut

whenM (shortcut (ctrl <> key 's')) save
whenM (shortcut (cmdOrCtrl <> shift <> key 'p')) (setPaletteOpen True)
whenM (shortcut (key (KeyF 5))) refresh
```

A chord with no key, such as `ctrl <> shift` alone, is never pressed.
`parseShortcut` reads a chord written as text, such as one from a settings
file: `C-s`, `M-S-p`, `A-<Enter>`, `<F5>`.

A shortcut fires once per press, for the first `shortcut` declared for the
chord. It stays quiet behind a modal, inside `disabledWhen`, and for keys the
focused widget uses, such as a text field's typing and Ctrl+A.
`menuItemShortcut "Save" (ctrl <> key 's')` binds its chord only while its
menu is open; for the closed menu, bind it with `shortcut` too, declared
first. `keyPressed`, `keyReleased` and `keyHeld` read a key whatever has the
keyboard.

Text fields and text areas work with input methods by themselves: the
focused field draws the composition (`inputComposition`) at its caret and
changes its value only on commit. Meanwhile the frame drops the keys, so no
shortcut fires.

Mouse buttons come as a `MouseButton`: `MouseLeft`, `MouseRight`,
`MouseMiddle`, the side buttons `MouseBack` and `MouseForward`, and
`MouseOther n` for any other. A widget's response says which went down on it
and are still held (`respHeldWith`), and which clicked it, going down and up
on it (`respClickedWith`); `respClicked` is its activation, a left click or
Enter. A button that went down elsewhere and is dragged over a widget is not
the widget's. `mousePressed`, `mouseReleased` and `mouseHeld` hear a button
anywhere on the part of the view being declared, as `keyPressed` hears a
key, and stay quiet behind a modal and in `disabledWhen`:

```haskell
whenM (mousePressed MouseBack) goBack
tab <- button' "Report"
when (respClickedWith MouseMiddle tab) closeReport
```

`mouseArea` gives any part of a view a response of its own, as iced's
`mouse_area` does: it is hovered while the pointer is on it or anything in
it, and reports the buttons pressed and clicked there, except a click a
widget inside takes for itself.

```haskell
(_, item) <- mouseArea (fillW . gap 6) $ do
  label name
  muted path
when (respClickedWith MouseMiddle item) (openInNewTab path)
when (respClickedWith MouseRight item) (showMenuFor path)
```

A closable tab closes on a middle click.

## Animation and background work

Backends wait for events when no frame is needed. Use `keepAnimating` while
an on-screen widget needs continuous frames. Use `wakeAfter` for a clock,
debounce, or delayed update; request it again on each frame that still needs
the deadline. Repeatedly marking the context dirty creates an unpaced loop.

Work that should not hold up a frame, such as reading a file, runs in a hook
on a thread of its own:

```haskell
contents <- useTask path (T.readFile path)
label (fromMaybe "Loading..." contents)
```

`useTask` returns `Nothing` until the action finishes, then its result, and
runs once per key: a new key kills the job and starts another, which is also
how to run it again. The job's end wakes the loop. The first frame that does
not call the hook kills the job, so call it outside a tab or branch that
should not end it. An action that throws stays `Nothing`; catch the
exception inside it (`try`) to show it. Results are forced only to weak head
normal form on the job's thread. Build with `-threaded`.

The context and its arenas belong to the UI thread. Another thread that
changes what the view reads publishes the change where the view reads it,
then calls the action `askWake` returns: the next frame runs the view and
repaints the whole window, and wakes that come before it cost only that
frame. A stream, a poller or a progress report is built this way. Here the
thread is a `useTask` job that never returns, so it ends with the view:

```haskell
sensorLoop :: IORef (Maybe Double) -> IO () -> IO ()
sensorLoop latest wake = forever $ do
  reading <- readSensor
  writeIORef latest (Just reading)
  wake

sensorView :: IORef (Maybe Double) -> NanoUI ()
sensorView latest = do
  wake <- askWake
  _ <- useTask () (sensorLoop latest wake)
  reading <- uiIO (readIORef latest)
  label (maybe "--" (T.pack . show) reading)
```

`latest` is made in `main`, before the app runs. A stream that keeps every
value folds each one into the reference instead.

Damage requests describe pixels to repaint. They do not themselves wake the
loop. Context operations that change stored state generally do both; for a
manual external change, arrange a wake as well as any required damage.

## Custom drawing and large text

`drawing` and `customWidget` build `DrawOp` values from solved logical bounds.
For a cached drawing, its content key must cover every changing external
input, including animation values. A key that stays fixed while the content
changes can leave stale pixels. Custom widgets without a key rebuild and
compare their operations each frame.

Curves go through `NanoUI.Path`, imported qualified: build a path with
`P.moveTo`, `P.lineTo`, `P.cubicTo`, `P.arc` and the rest, fill it with
`drawPath` and stroke it with `drawStrokePath`. `withTransform` moves, turns
and scales a block of canvas drawing. Run a custom widget's canvas with
`runCanvasFor`, as `canvas` does, so curves stay smooth on a dense display.
A hole drawn as a second subpath is filled over, not cut out.

`textArea` accepts `Text` and joins the document when edits change it.
`textAreaDocument` accepts a `TextDocument`, sharing unchanged lines across
edits. Keep the document value in your model and call `documentText` when
saving or otherwise needing one complete `Text` value.

Backends receive borrowed `DrawData` buffers. Draw or copy them before the
next frame reuses the arena. Native font and renderer handles must not outlive
their backend session.

## Images

Register an image's RGBA pixels once with `registerImageRgba`, under an id
from `freshImageId`. `image` stretches it over its rect. `imageConfigured`
takes an `ImageConfig`: a `ContentFit` like CSS's `object-fit`, an alignment,
an opacity, and a rotation (`RotateSolid` fits the turned image in its rect,
`RotateFloating` keeps the unturned layout and crops). An axis the layout
leaves unsized takes the image's own size. A canvas draws images with
`drawImage`, `drawImageUV` and `drawImageRotated`.

```haskell
thumbnail :: ImageId -> NanoUI ()
thumbnail photo =
  imageConfigured
    defaultImageConfig {icLayout = fixedWH 120 90 defaultLayout, icFit = FitCover}
    photo
```

## Seeing the layout

`explainLayout True` outlines every layout node, coloured by depth, and
highlights the node under the pointer, which `explainedNode` describes. The
overlay only paints. `sdlExplainLayout`, `optExplainLayout` and the SDL demo's
`--explain` start with it on, and a debug window can toggle it:

```haskell
explainLayout =<< checkbox "Outline layout nodes" =<< explainingLayout
```

## The window

`SdlOptions` and `RgfwOptions` set the window's size, position, icon and size
limits, and `SdlOptions` its opacity and transparency. A view changes them
with `setWindowIconUi`, `setWindowMinSizeUi`, `setWindowMaxSizeUi` and
`setWindowOpacityUi`, which act only on a change and so can run every frame,
and `setWindowPositionUi`, which moves the window on every call. A
transparent window (`sdlWindowTransparent`) shows the desktop, given a
compositor, where the theme's `windowColor` is translucent. An RGFW window is
opaque and does not fade, so `setWindowOpacityUi` does nothing there.

`requestScreenshot` hands its action the frame as an `RgbaImage` once it is on
screen, or `Nothing` outside a window. Ask from an event, and keep the action
short or fork it, since the next frame waits for it.

## Writing a backend

`NanoUI` is the view API. What a backend is written against is in
`NanoUI.Backend`: `emptyInput` and the functions that fold a window's events
into it, the `FontBackend` callbacks that measure and shape text, the `Damage`
a frame reports so only changed rectangles are presented, and the paddings and
widths the widgets lay themselves out by. Input carries over from frame to
frame: pass the last frame's through `clearEphemeral`, which drops one-shot
events and keeps held buttons, the pointer and the window size, and fold the
new events into that rather than into a fresh `emptyInput`.
The context from `NanoUI.Testing`'s `newContext` or `newPixelContext` holds the
state that outlives a frame, and `NanoUI.Runner` sequences events, redraws and
wake-ups. The two backends in this repository,
`NanoUI.Backend.Sdl` and `NanoUI.Backend.Rgfw`, are the worked examples.

Fold keys in with `applyKey` (a typing key as the `KeyChar` it types
unmodified, with its text in `inputChars` too) and input-method updates with
`applyComposition`. After a frame, `textInputArea` from
`NanoUI.Testing` says where the candidate window goes. Before the first frame,
install a `WindowHost` (`installWindowHost`) and a wake action any thread may
call (`setWakeLoop`), and report the desktop's light or dark setting with
`setSystemAppearance`, again on each change. Once a frame is on screen, call
`answerScreenshots` with a capture of it, or `pure Nothing`.

## Headless tests

`NanoUI.Testing` exposes `newContext` and `runFrame`. A frame returns the view
result, messages, draw data, and a follow-up-frame flag. Set window size and
delta time explicitly in test input. Warm up before targeting a widget by
its response rectangle, then send separate press and release frames.

`NanoUI.Testing.Harness` supplies `warmup2`, `clickPair`, `runClick`, and
text-span queries, with `clickPairWith` for another button, as in
`clickPairWith MouseMiddle base pos`. Its `held`
helper stores controlled input values outside the hook store, so an automatic
hook rebuild does not hide a change flag that the test is trying to observe.

A tooltip's delay runs on the real clock, so a test sets `tooltipDelay = 0`.
`newWakeSignal` lets a test wait for a background job's wake, and
`cancelTasks` from `NanoUI.Backend` kills the jobs it leaves running.

Headless tests cover layout and input behaviour. Run the native backend too
when changing rendering, fonts, dialogs, or display scaling.
