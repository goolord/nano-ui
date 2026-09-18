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

### Changed

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
