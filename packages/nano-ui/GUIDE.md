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

Give a scroller a bounded viewport, for example
`scrollWith (fixedH 240 . fillW) body`. `scrollArea` also returns its id for
commands such as `scrollToEnd`. Use `getScrollMetrics` to obtain the visible
range when building only the visible rows of a large collection.

During view construction, `respRect` uses recorded geometry from the prior
frame. It may be empty when a widget first appears. Keep this in mind when
anchoring popups or writing scripted pointer tests.

## Themes, overlays, and input

Use `styled` to modify the theme for a subtree and `themed` to replace it.
These scopes affect painting, not layout. Font-size changes belong in layout
modifiers. `disabledWhen condition` keeps widget geometry and state while
disabling interaction and applying disabled colours.

Modals, floating windows, and popups return close or dismissal requests.
The application owns their open flag and must update it. Keep calling the
overlay with that flag so its identity and later siblings remain stable.

Ordinary widgets read routed input with `askInput`. A covered layer receives
no pointer. `askFrameInput` is for window-wide handling, such as dismissing
a popup after an outside click; using it for an ordinary control bypasses
pointer routing.

## Animation and background work

Backends wait for events when no frame is needed. Use `keepAnimating` while
an on-screen widget needs continuous frames. Use `wakeAfter` for a clock,
debounce, or delayed update; request it again on each frame that still needs
the deadline. Repeatedly marking the context dirty creates an unpaced loop.

The context and its arenas belong to the UI thread. A worker should publish
results through an application-owned synchronisation mechanism, then call
the backend wake action installed in `ctxWakeLoop`. The next view reads the
result. Do not mutate context stores concurrently with a frame.

Damage requests describe pixels to repaint. They do not themselves wake the
loop. Context operations that change stored state generally do both; for a
manual external change, arrange a wake as well as any required damage.

## Custom drawing and large text

`drawing` and `customWidget` build `DrawOp` values from solved logical bounds.
For a cached drawing, its content key must cover every changing external
input, including animation values. A key that stays fixed while the content
changes can leave stale pixels. Custom widgets without a key rebuild and
compare their operations each frame.

`textArea` accepts `Text` and joins the document when edits change it.
`textAreaDocument` accepts a `TextDocument`, sharing unchanged lines across
edits. Keep the document value in your model and call `documentText` when
saving or otherwise needing one complete `Text` value.

Backends receive borrowed `DrawData` buffers. Draw or copy them before the
next frame reuses the arena. Native font and renderer handles must not outlive
their backend session.

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

## Headless tests

`NanoUI.Testing` exposes `newContext` and `runFrame`. A frame returns the view
result, messages, draw data, and a follow-up-frame flag. Set window size and
delta time explicitly in test input. Warm up before targeting a widget by
its response rectangle, then send separate press and release frames.

`NanoUI.Testing.Harness` supplies `warmup2`, `clickPair`, `runClick`, and
text-span queries. Its `held` helper stores controlled input values outside
the hook store, so an automatic hook rebuild does not hide a change flag
that the test is trying to observe.

Headless tests cover layout and input behaviour. Run the native backend too
when changing rendering, fonts, dialogs, or display scaling.
