# Changelog

## Unreleased

### Added

- Scoped styling: `styled f` draws the widgets inside it with the theme
  modified by `f`, and `themed t` with another theme. Scopes nest, only
  affect how widgets look, and `uiTheme` reads the theme where it is called.
- Style modifiers (`background`, `foreground`, `borderColor`, `borderWidth`,
  `cornerRadius`, `hoverBackground`, `pressBackground`, `fillColor`) and
  theme modifiers (`buttonStyle`, `inputStyle`, `panelStyle`, `windowStyle`,
  `everyStyle`, `accentColor`, `textColor`, `mutedColor`, `linkColor`,
  `selectionColor`, `windowColor`, `rounded`), composed with `(.)`.
- Button variants as theme modifiers: `primary`, `destructive`, `success`,
  `subtle`, and `tinted` for any theme colour.
- `disabledWhen` disables the widgets inside it: they keep their layout and
  state, take no pointer or keyboard input, leave the focus order, and draw
  with `disabledTheme`.
- Undo and redo in every text field: Ctrl+Z, Ctrl+Shift+Z and Ctrl+Y, and
  Undo and Redo rows in the right-click menu. Typing is undone a word at a
  time and a run of deletes at once. Steps keep the edits, not copies of the
  document.
- `TextCommand` and `TextMotion`: what text fields do, as values. Keys and
  the context menu run them, and `runTextCommand` runs them on a field by its
  id, with `textCanUndo` and `textCanRedo` for enabling menu items.
- SVG icons: `parseSvg` and `loadSvg` read the static subset of SVG icon
  sets use (paths, basic shapes, groups, transforms, strokes with caps and
  joins, fill rules, opacity), and `svgIcon` and `svgIconWith` draw one. A
  one-colour icon is tinted with the text colour or a `fontColor`. Each
  document is rasterized with anti-aliasing once per pixel size and colour,
  at the display's scale, into the image atlas.
- `spinner`, an indeterminate loading indicator that repaints only its own
  rect while it turns.
- Rich text: `richText` draws a paragraph of `Inline` pieces, wrapped at its
  width, and returns the target of a link clicked this frame. Pieces are
  string literals, `strong`, `emphasis`, `inlineCode`, `inlineWith` with any
  font modifiers (size, weight, colour, decoration), and
  `hyperlink target label`, drawn in the theme's link colour, underlined
  while hovered, with the pointer cursor. `restyle` adds modifiers to a
  piece, and `richTextWith` sets the paragraph's default font. Mixed sizes
  share a baseline. A paragraph keeps its measured words between frames
  while its pieces, fonts and colours stay the same.
- `DrawTextStyled` draws text in a `TextFont` (size, variant, weight, style
  and decoration) from custom widgets.
- Mixed-direction lines: `NanoUI.Bidi` splits a line into left-to-right and
  right-to-left runs in visual order (the implicit rules of UAX #9: strong
  types, numbers, neutrals and reordering; no explicit embeddings or
  isolates). Carets, clicks and selections in text fields follow the shaped
  layout, so a selection across Arabic and Latin text covers a span per
  run.
- `ShapedText` and `ShapedGlyphs` let a host backend hand shaped layouts to
  the core through `fmShape` and `drawShaped`.
- Theme slots `themeOnAccent`, `themeSelection`, `themeFocusRing`,
  `themeLink`, `themeShadow` and `themeDisabledFade`, which replace colours
  that were fixed in the painters.
- Scroll tuning: `setScrollTuning` sets how far a wheel notch scrolls
  (`scrollWheelStep`) and how long a scroll takes to settle
  (`scrollSmoothTime`, `0` for no glide). `setScrollStep` gives one scroller
  its own step.
- Scroll commands: `scrollTo`, `scrollBy`, `scrollPages`, `scrollToStart`,
  `scrollToEnd`, `scrollIntoView` and `scrollRectIntoView`, each landing at
  once or gliding (`ScrollInstant` / `ScrollSmooth`).
- `getScrollMetrics` reports a scroller's viewport, offset and reachable
  range, which is what a virtualized list needs to pick the rows it builds.
- `expectText` in `NanoUI.Testing.Harness` fails with a message unless a
  collected span contains the given text.
- `Slot` (in `NanoUI.Store` and `NanoUI.Context`) names each kind of
  per-widget store entry, and `modifyStore` in `NanoUI.Context` updates the
  store in one read and write.
- `wakeAfter` asks for a frame after a number of seconds with no input to
  cause it, and the loop sleeps until then: a clock label ticks with one
  frame a second where `keepAnimating` would run one for every display
  refresh. `requestWakeAt` and `requestWakeAfter` in `NanoUI.Context` are
  the same for code that holds a `Context`, and `getWakeAt` reads the
  pending time.
- `NANO_LOOP_TRACE` prints, about once a second while the session loop runs,
  the time covered, how many passes it made, how many drew, and why. An idle
  window prints nothing, so what prints steadily is what keeps the process
  awake.
- `textAreaDocument` (with `textAreaDocument'`, `textAreaDocumentWith` and
  `textAreaDocumentWith'`): a text area over a `TextDocument`, the text as
  its lines, instead of over `Text`. An edit replaces only the lines it
  touches and a frame without edits returns the document it was passed, so
  the text is never joined or compared in full while typing. Build one with
  `textDocument` and read it with `documentText`, `documentLines`,
  `documentLine` and `documentLineCount`. `==` skips the lines two documents
  share, so storing the edited document with `useState` reads no text
  before the edit. Typing 1,000 characters into a
  100,000-line document takes 0.14 s instead of 1.5 s through `textArea`,
  and allocates 248 MB instead of 7.1 GB.
- `NanoUI.Store` names the store's maps as `Field`s (`fieldInt`,
  `fieldFloat`, `fieldPoint`, `fieldText`, ...) and reads and writes slots
  through them: `lookupSlot`, `findSlot`, `memberSlot`, `insertSlot`,
  `deleteSlot`, `flagSlot` and `setFlagSlot`, with `lookupDyn` and
  `insertDyn` for `Dynamic` values. Writes are `WidgetStore -> WidgetStore`
  and compose with `(.)`. They inline to the record code they replace, which
  the inspection suite checks, so a composition still builds the store once.
- `writeSlots` runs `slotWrite`s joined with `<>` and leaves the store alone
  when every slot already holds its value, for widgets that publish state
  each frame.
- `withContext` runs an `IO` action on the view's `Context`, and `<&&>` is a
  short-circuiting `&&` over effectful tests.
- `NanoUI.Frame.Hit.withWidgetNode`, and `walkAncestors` and `getNodeRect`
  in `NanoUI.Layout.Arena`.
- `NanoUI.Testing`: `assertJust` and `assertJustM` carry a test on with a
  value it needs or count a failure, `spanRect` and `spanRectOf` find a span
  by its text, `clipCovers` checks a frame's damage, and `warmupFocused`
  warms a view up and tabs onto its first focusable.

### Changed

- Draw data stores cumulative `drawLayerOffsets`, including a final command-count
  sentinel, instead of `drawLayerSlices` / `LayerSlice`. Use
  `forDrawCmdsInLayer_` to visit one layer without depending on the representation.

- `Animatable` uses `traverseChannels` instead of `toComponents` and
  `fromComponents`. Custom instances apply the supplied indexed action directly
  to their fields, for example `V2 <$> f 0 x <*> f 1 y`. Keep channel indices
  stable: they identify the component animations. The engine still uses
  single-precision channels, including for the `Double` instance.

- The pointer goes to one place, decided once a frame. Before the view runs,
  the frame works out what is on top under the pointer (the text-edit menu,
  an open dropdown, the floating panel in front, or the page) and routes the
  pointer there; a button that is already down keeps the route it went down
  with until it comes up, and a new press always starts over, even with the
  other button still held. Everything else reads an input with no pointer in
  it: no buttons, no wheel, and a position far off any widget. Widgets used
  to read every press and check for themselves whether something covered
  them, each with its own list of things to check, and a widget or a
  frame-end step that forgot one reacted through whatever was drawn over it.
  Now there is nothing to forget: `askInput` is already routed, a floating
  panel routes its own body, and the frame-end steps for scrolling, windows,
  presses, focus and text fields are handed the same pointerless input while
  a menu or dropdown has the pointer. `askFrameInput` is the unrouted input,
  for what watches the whole window (a press outside dismissing a popup).
  This replaces the menu pointer gesture flag, the cached dropdown rect, the
  per-widget `pointerBlockedByOverlay` check and its blocked flag, and the
  slider's and colour picker's own held-by-another-widget checks.
- `useDrag1D` starts with a press on its track, like `useDrag2D`. It used to
  start whenever a held button was over the track, so a drag begun elsewhere
  picked up every slider it crossed.
- A right click inside the text-edit menu no longer reopens the menu at the
  pointer.
- `textArea` keeps the document it last returned with its text, so a frame
  that edits nothing (a cursor move, a scroll) neither joins the document
  nor compares it with the text it was passed.
- `loadTextAreaState` and `saveTextAreaState` no longer take the text; the
  text area's lines are its state.
- `callout` tints the theme's panel colour instead of a fixed dark grey.
- `uiTheme` returns the theme of the enclosing `styled` scope.
- Undo history keeps its edits' texts as `ShortText` copies, which never
  hold on to the larger text a slice came from. A session of typing,
  deleting and pasting in a 5000-line document keeps a 426 KB history
  instead of 1.74 MB.
- SVG documents are read by hexml instead of a hand-written XML scanner. A
  DOCTYPE, which hexml rejects, is skipped. Attribute values must be quoted,
  as XML requires.
- Rasterizing an SVG allocates far less: a 16px stroked icon with round
  joins allocates 42 KB instead of 1.1 MB, and at 128px 437 KB (mostly its
  pixel buffers) instead of 7.2 MB, taking 0.37 ms instead of 4 ms. Contours
  and stroke outlines are flat arrays, and coverage sweeps edges sorted by
  row instead of filtering every edge on every sample row. Output is
  unchanged. `nano-ui-profile svg` measures it.
- Draw ops are a `SmallArray DrawOp` from `primitive` instead of a boxed
  `Vector`: `DrawingBuild`, `CustomDrawBuild`, `runCanvas`, `drawing`,
  `drawingVersioned` and `emitDrawOps`. Build them with
  `smallArrayFromList`; the arrays are `Foldable`, `Eq` and a `Semigroup`.
- `inputKeys` and `inputDrops` are `SmallArray`s. `appendDropEvent` adds a
  drop the way `appendInputKey` adds a key.
- Tree rows, table column metrics and sizes, the SVG rasterizer's buffers,
  and polygon triangulation and stroking use `primitive` arrays. Stroking a
  10,000-point line allocates 2.0 MB instead of 4.7 MB and takes about
  0.34 ms instead of 0.59 ms; a 50-row tree frame allocates 213 KB instead of
  275 KB.
- A custom widget with a measure hook and a fit height is measured again at
  the width layout gives it, as wrapped labels are, so its height can follow
  its width.
- Custom widgets with a content key rebuild their ops when a theme scope
  changes, instead of keeping the old theme's colours.
- `RunQuad`, `fmRun` and `drawRun` are replaced by `fmShape` and
  `drawShaped`. A backend without shaping sets `fmShape = const Nothing` and
  keeps per-character advances and kerning.
- `takeTextEditLastAction` reports the `TextCommand` a text field's menu
  ran.
- Text areas keep their document as a finger tree of lines and repaint only
  the lines in view, so a keystroke in a 100,000-line document costs about
  1.4 ms on the headless profiler instead of about 26 ms.
- A wheel notch scrolls three text lines instead of one, matching what
  desktops send a notch as. `setScrollTuning` puts it back.
- `adoptStoreInt`, `adoptStoreFloat` and `adoptStoreText` return the value
  they adopted.
- `runClick` in `NanoUI.Testing.Harness` returns the release frame's result.
- `NanoUI.Layout.Solve.positionWindowNode` is now `placeWindowNode`.
- `SessionDriver` takes the session's debug sampler (`sdDebug`), whether to
  redraw continuously (`sdContinuous`), a pacing wait (`sdPacingMs`) and
  whether presents wait for the display (`sdPresentPaces`). The event loop
  decides the wait itself, counts clicks with fixed thresholds, samples
  debug timing, and syncs the cursor after every pass, so `sdWaitTimeout`,
  `sdSkip`, `sdNoteLoop`, `sdClickDistance` and `sdClickTime` are gone.
  `sdShouldDraw` also takes whether the debug readout is due.
- `NanoUI.Debug` refreshes snapshots through `refreshDebugSnapshot` and
  `debugRefreshDue` instead of `makeCoreDebugSnapshot`, `presentRate` and
  `takeDebugLive`.
- Lines are drawn as one anti-aliased strip with round caps instead of a
  capsule per segment.
- Tables encode each row once and sort row indices with a merge sort, combo
  boxes filter their options only while focused, pane grid dividers and
  overlays are keyed drawings, and unchanged animations are not rewritten
  to the store, cutting per-frame work for those widgets.
- Damage is gathered into one running union instead of rect lists, paint
  keeps the opaque floating panels it culls against in a flat array built
  only when a frame has floating panels, and the node arena tracks the
  topmost modal as nodes are added instead of hit tests scanning for it.
  On the headless profiler, 3000 frames of a button grid with a pointer
  moving over it and a floating window allocate 1.46 GB instead of 1.64 GB,
  or 1.38 GB instead of 1.48 GB with a modal.

### Fixed

- Nothing reacts through what is drawn over it. `pointer-ownership` opens
  each kind of overlay (the text-edit menu, a select's and a combo's
  dropdown, a popup, a window, a modal) over each kind of pointer-driven
  widget, clicks, drags, wheels and right-clicks on the overlay, and checks
  the widget underneath noticed nothing; 35 of its 234 cases failed:
  - Select All from a text area's right-click menu selected only from the
    pointer down: the press on the menu row also placed the caret in the
    text area under it and started a drag, which the release applied.
  - A press on the text-edit menu or a dropdown opened or closed a select
    under it, dragged the thumb of a scrollbar under it, and started
    resizing a table column under it.
  - A press on a popup, window or modal opened a select under it.
  - A `knob` turned with the wheel, and dragged, through every overlay.
  - The wheel over the text-edit menu scrolled the text area or scroller
    under it.
  - A right click on a dropdown or on the text-edit menu opened the context
    menu of a text field under it and gave it focus.
  - A button that went down in a window dragged a slider on the page when
    the pointer crossed its track (`pointer-capture`).
- A window left alone uses no CPU or GPU. Three things kept the session loop
  running at the display rate behind a picture that never changed:
  - `keepAnimating`, and so `spinner`, started an animation that never ends
    and nothing removed once the widget stopped being built. A view that
    showed a spinner while it loaded kept drawing frames for the life of the
    process. It now lasts as long as it keeps being called, and the first
    frame without it lets the loop sleep.
  - A focused text field ran a full frame every 16 ms, a leftover from a
    backend whose typed bytes did not wake the loop. Typing arrives as input,
    so focus alone asks for nothing. The search field's debounce, which
    relied on those frames to notice that typing had paused, asks for the
    one frame that commits it, and a selection dragged past a field's edge
    asks for frames while the drag lasts.
  - A numeric field's held stepper arrow marked the context dirty every frame
    to reach its next repeat, which ran frames back to back with no pacing.
    It asks for the frame of the next repeat instead.
- Dragging a pane in a `paneGrid` shows the grid as the drop will leave it.
  The highlight was already the rect the dragged pane lands in, but it was
  drawn over the other panes where they sat before the drop, and a drop moves
  them too: a swap sends the target to the dragged pane's old slot, and a drop
  on the grid's outer edge squeezes every pane into one half. Once dividers
  had been dragged off centre, the highlight lined up with nothing on screen.
  While a pane hovers over a drop target the grid lays out the post-drop tree
  (`dropPreviewTree`), the dragged pane's slot empty under the highlight, and
  the release stores that same tree. A pointer in a gutter targets the nearer
  pane (`nearestPane`), so the preview holds while it crosses. A pointer
  outside the grid has no target and releasing there cancels the drag;
  `topLevelDropTarget` used to read a point beyond an edge as that edge.
- `registerImage` damages the whole frame. New pixels under an id already on
  screen change no rect or text, so nothing else would repaint them.
- Circles sharing a centre are concentric whatever their radii. A circle's
  centre snaps to the pixel grid, not its bounding box's corner, which
  rounded differently per radius and put a small disc drawn over a larger
  one up to a pixel off-centre.
- Disabled context-menu rows (`menuItemDisabled`) take the same height,
  minimum width and label inset as enabled rows, instead of a bare label
  flush against the panel edge.
- Where two widgets overlap, a press goes to the one hover highlights (the
  earlier sibling, which paints on top).
- Opening a modal or floating window no longer shifts the ids, and so the
  state, of the widgets declared after it.
- Ctrl+Alt (AltGr) characters type text in text areas, as they already did
  in single-line fields.
- `runTextCommand` focuses the field it runs on, so a command from an app
  menu (Select All, say) shows its selection and the next keystroke goes to
  the field.
- `useDrag2D` does not start a drag during a menu's pointer gesture.
- `drawingVersioned 0` is treated as unversioned, as `drawing` is.
- A grow container with a `minW` counts as that minimum, not as its content,
  in the width of the scroller around it. A long label in such a cell no
  longer makes a 2D scroller scroll sideways while the row would fit.
- Modals, windows and popups lay out their contents with the fonts and
  custom measurements the solve used. A label in a font size other than the
  base no longer wraps onto a line the modal did not make room for, which
  made the modal scroll and clip its last row.
- A window or modal that fits its width to its content and scrolls leaves
  room for its scrollbar. The bar's gutter used to narrow the body below the
  width its content measured, so right-aligned values, such as those of
  `kv` rows in a body with a `minW`, lost their last letter under the clip.
- Floating windows resize from inside their margins as well as from outside
  their edges: each side's handle reaches 12 px out and the side's padding
  in (at least 6 px, so the top edge has a strip above the title bar), and
  within 16 px of a corner it resizes both ways. Before, only the right
  padding and a 6 px bottom strip resized from inside. The window's controls
  and its body's scrollbar still take their own presses.

### Removed

- `panelStyled` and `panelStyledWith`; use
  `styled (panelStyle (background bg . borderColor border))` around a panel.
- `MenuAction`; text field menus run `TextCommand`s.
- `labelContentInset`, `panelPaintPad`, `resolveLayoutGap`,
  `resolveLayoutPadding` and `tableCellInset` from `NanoUI`.
- The `slot*` tag constants in `NanoUI.Store` and `NanoUI.Context`; use
  `slotKey` with a `Slot` constructor.
- `stopAnimation`, and store and state accessors from `NanoUI.Context` that
  only internal code used, among them `deleteWidgetStore`, `setStoreBool`,
  `writeStoreText`, `getPrevRects`, `setPrevRectsAndClips`,
  `setOpenSelectDrop`, `setWindowDrag` and `setTextFieldClickCell`.
- `withLayout` from `NanoUI.Monad` (`withIdFrame` is exported instead).
- `stopAnimation` and `textDisplayWidth` from `NanoUI.Testing`.
- `runClickPair` and `runClickRelease` from `NanoUI.Testing.Harness`; use
  `runClick`.
- The text area's `TextAreaEvent`, `handleTextAreaEvent`,
  `computeTextAreaLayout`, `processTextArea`, `textAreaInputCommands`,
  `TextAreaLayout`, `VisualLine` and `Modifiers` exports, and the
  `NanoUI.Widgets.TextBuffer` edit functions (`insertChar`, `insertText`,
  `deleteRange`, `replaceRange` and others). Drive text areas with
  `TextCommand`s. `NanoUI.Widgets.TextEditor` exports `inputTextCommands`
  in place of `ctrlCharCommand` and `isShortcutChar`.
- Internal layout arena column constants and setters from
  `NanoUI.Layout.Arena`, `TextAreaGeom`, `textAreaGeom` and
  `textAreaBarLanes` from `NanoUI.Frame.TextEdit` (which exports
  `textAreaBarLane` and `textAreaLineHeight`), and `resizeFromEdge` and `windowResizeEdgeAt`
  from `NanoUI.Frame.Window`.

## 0.1.0.0

First release.
