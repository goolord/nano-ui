# Changelog

## Unreleased

### Added

- `NanoUI.Adornment`, for qualified import: icons, texts and views drawn
  before or after a text field's value (`ticAdornments`) or a button's label
  (`buttonConfigured`, `iconButton`). `icon`, `iconSized`, `affix` and `view`
  are for display; a `control` takes its own presses. Put them on a side with
  `leading` or `trailing` and combine with `<>`. Adornments move no other
  widget's id.
- `buttonContent` and its variants: a button whose content is any view.
- A scrollbar's thumb brightens while the pointer is on the bar or dragging
  it, on scrollers, text areas and combo dropdowns. The colour,
  `scrollBarThumbHoverColor`, comes from the same surface style as the
  resting thumb, so an `inputStyle` or `windowStyle` that restyles one
  restyles both.
- A widget drawn inside another is on top of it for hover and presses.
- `FillPolygon` and `StrokePolyline` draw ops: a simple polygon, given with
  its triangulation, and a polyline with mitered joins, both anti-aliased
  along their outline only. A shape cut into `FillTriangle`s would show
  faint seams where the triangles meet.
- What a widget that works out its own input needs, without
  `NanoUI.Context`: `lastRect`, where a widget was laid out last frame, which
  is the rect a list that scrolls itself hit-tests this frame's pointer
  against; `holdFocus`, which gives a widget the keyboard without the ring Tab
  draws, and that frame's Tab with it, so an editor indents on Tab rather
  than losing focus to the next widget for a frame (`NanoUI.Monad` also has
  `releaseFocus` and `focusedWidget`); `getClipboard` and `setClipboard`, through whatever clipboard the backend
  installed; `requestFrame`, for a view whose state lives outside nano-ui and
  changed after the part showing it was declared; `resolveFontUi` and
  `lineWidthUi`, for a widget that draws in a font other than the context's.
  `NanoUI` also exports `askInput`, `uiTime`, `uiFontMetrics` and
  `foldInputKeys`, which it had only from `NanoUI.Monad` and `NanoUI.Input`.
- Scroll commands run from a view rather than handed the context:
  `getScrollMetricsUi`, `setScrollOffsetUi`, `scrollToUi`, `scrollByUi`,
  `scrollPagesUi`, `scrollRectIntoViewUi` and `setScrollStepUi`. A command
  meant for this frame names the scroller by the id `currentId` says it
  will take.
- `takeEscape`: whether Escape was pressed this frame and nothing earlier
  took it, nor an open text-field menu or dropdown, and if so takes it. For a
  dialog that Escape puts away, so the Escape that closes a menu inside it
  does not close the dialog too.
- `modalWith`, a modal with a layout modifier for its panel:
  `modalWith (fixedWH w h)` gives it a size, and a body laid out with
  `fillW . fillH` fills it.
- `widgetTrackPointer` on a custom widget: a frame for every pointer move over
  it, not only for a move onto another widget. For a widget that draws what
  is under the pointer inside itself, such as the hovered row of a list that
  draws its own rows, which otherwise had to ask for a frame on a timer while
  the pointer was over it.
- `contentKeyOf`, a content key over values of any `Hashable` types:
  `contentKeyOf [keyPart version, keyPart scrollY, keyPart query]`. Unlike
  `contentKey` it hashes an `Int` or a `Double` whole, and takes text
  without the caller hashing it first.
- `checkboxWith` and `checkboxWith'`, a checkbox with a layout modifier:
  `checkboxWith alignMid` centres it in a row taller than itself.
- `pgInitial` on `PaneGridConfig`, the split tree a grid starts from, with
  pane and split ids of the caller's choosing; `NanoUI` exports `GridNode` to
  write it with. A grid that had to be seeded in the widget store before its
  first frame, to start with a split at a given ratio, is now given one.
- `pgFocusable` on `PaneGridConfig`: off, the grid is no Tab stop and its
  arrow, `m`, `x` and Escape keys do nothing, for a grid whose panes own the
  keyboard.
- `padTop`, `padBottom` and `padLRTB`, beside `padAll` and `padXY`. `padTop`
  and `padBottom` set one edge and keep the other three, so
  `padTop 0 . padXY 12 6` pads 12 at the sides and 6 at the foot only;
  `padLRTB` sets all four, in `Padding`'s order.
- `NanoUI.Backend`, holding what a window backend is built from and a view
  never touches: `emptyInput` and the functions that fold a window's events
  into it, the `FontBackend` callbacks that measure and shape text, the
  `Damage` a frame reports, the paddings and widths the widgets lay
  themselves out by, how a `WidgetId` is derived, the compact region host
  state lives in, and `runUi` / `runNanoUI`. Most of it came out of `NanoUI`
  (see Changed). It also re-exports `MouseButton`, `applyMouseButton` and
  `clearEphemeral` from `NanoUI.Input`, so a backend's event loop needs the
  one import, and the input types `Input`, `Key`, `Modifiers`, `DropEvent`
  and `DropType`, which `NanoUI` keeps too. The names the two modules share
  are the same entities, so importing both is unambiguous.
- `NanoUI` exports `UiCursorKind`, which `widgetCursor` on a custom widget
  returns, so choosing its pointer no longer needs `NanoUI.Testing`.

- `inlineBackground`: a colour painted behind a rich-text piece, such as the
  tint behind inline code.
- `NanoUIEs`, the effect row behind `NanoUI`. A widget configuration carrying
  its caller's row, such as `PaneGridConfig`, can now be named from an
  ordinary view without depending on `effectful`.

- Caption buttons for a window that draws its own title bar: `captionButtons`
  draws minimize, maximize-or-restore and close and says which was pressed,
  `captionButton` draws one of them, and `CaptionConfig` sizes them, rounds
  the close button's corner and says what red it lights up in. The glyphs are
  drawn rather than written, so they need nothing of the font.
- `dragSpans` works out what is left of a title bar to drag a window by: the
  row, minus the rectangles of everything in it that takes a click. Hand the
  result to a backend as the window's drag region.
- `captionBarHeight`, a title bar tall enough to leave the topmost pixels to
  resizing the window.
- `menuButtonWith` and `menuButtonWith'`, for a menu bar whose row is taller
  than a label: `fillH` gives each title the height of the bar, so its text
  sits in the middle of the bar rather than at the top of it.
- `windowFrame` draws a border around the whole window with the view inside
  it, which is what tells a window with no frame of its own from whatever is
  behind it. `WindowFrame` gives its width, colour and corner radius: the
  width is also the inset the view is drawn at, the radius wants to be what
  the desktop rounds the window by or the line is cut off at the corners, and
  a width of zero draws no border without moving anything inside or
  disturbing its state. The style is the frame's own: panels inside it are
  drawn in the theme around it.

- Generic `writeSlot`, `adoptSlot`, and `recordSlot` context operations for
  controls using a typed store `Field`, alongside the existing scalar helpers.
- `withUiResource` in `NanoUI.Monad` brackets UI-thread state acquisition and
  restoration, using the same exception/masking behavior as the existing scopes.

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
- `pgFixedPanes` pins panes in a `paneGrid`: a pinned pane keeps its extent
  along its parent split's axis while the grid resizes, and the other side of
  that split takes the whole of the difference. A sidebar therefore stays the
  width the reader dragged it to however wide the window is. Only the pane's
  own split is pinned, so a pinned pane at each end of a grid keeps both
  widths and a pinned width never freezes the height of the row it sits in.
  It still gives way when the grid is too small to hold it and its
  neighbour's minimum both, and the divider still moves it anywhere.
  `reflowFixed` in `NanoUI.Widgets.SplitPane` is the pure re-ratio this does,
  and `pinnedSide` the side test it turns on.
- An open window, modal, popup or tooltip no longer turns off the reuse of
  the last frame's layout. The layout is cached as solved, before the floating
  panels are placed, and a frame that reuses it places them again. A window
  dragged or resized places the panels again over the frame's solve instead of
  solving the whole layout a second time. Over 5000 rows, a frame with a window
  open went from 15.8 to 11.1 ms and one dragging it from 20 to 11.2 ms, where
  the page alone takes 10.6.
- Damage in places far apart repaints as up to four disjoint pieces instead
  of their bounding box. Two labels changing in opposite corners no longer
  repaint the whole window: paint skips nodes that meet no piece, each piece
  gets its own backdrop, and every draw command is cut to the pieces it meets.
  A frame goes full once its pieces, not their bounding box, cover half the
  window. `DamageClip` is still the bounding box; `takeDamagePieces` in
  `NanoUI.Testing` gives the pieces, which a backend that draws text outside
  the draw commands must clip it to, and `damagePieces` is how they are made.
- `NanoUI.Path`, for qualified import: paths of lines, curves, arcs and
  shapes (`moveTo`, `cubicTo`, `arc`, `circle` and more), which a canvas fills
  with `drawPath` and strokes with `drawStrokePath` or `drawStrokePathCapped`.
- `withTransform` draws a canvas block through a `Transform` (`translate`,
  `rotate`, `scale`, `affine`, composed with `<>`), and `runCanvasFor` runs a
  custom widget's canvas with its curves flattened for its display.
- `stack` and `stackWith` layer their children in one box. The `wrap`
  modifier flows a row onto new lines, or a column into new columns, `lineGap`
  apart, and `pinAt x y` places a node at an offset over its siblings, from
  where its alignment puts it: `pinAt (-16) (-16) . alignEnd . alignBottom`
  is a floating button 16 in from the bottom-right corner.
- `useDrag2DOn` and `useWheelDeltaOn`, the drag and wheel hooks of a custom
  widget fed its `Response`: a drag starts with a press on the widget and a
  wheel turns it while it is hovered, so neither acts through something drawn
  over the widget, on the part a scroller clips off, or while it is disabled.
  `knob` uses them. `useDrag2D` and `useWheelDelta`, which test a rect, are
  deprecated.
- `pointer`, a layout modifier for what a node drawn over others by a stack
  or a pin does with the pointer: by default (`PointerAuto`) a control on top
  takes it and anything else lets it through to the controls beneath;
  `PointerBlock` makes a node, such as a card or a scrim, take it over its
  whole box, so nothing beneath is hovered, pressed, focused or scrolled, and
  `PointerPass` makes a node and all inside it let it through. A label, an
  image or a container with an id under a control on top is covered like a
  control, taking no hover or tooltip there.
- `imageConfigured` and `imageConfigured'` take an `ImageConfig`: a
  `ContentFit` like CSS's `object-fit`, an alignment, an opacity and a
  `Rotation`. The `DrawImageRotated` op and the canvas's `drawImageRotated`
  draw an image turned about its centre.
- Focus from code: `requestFocus` gives a widget the keyboard by its `respId`
  as Tab would, or with `WidgetId 0` takes it away.
- `tooltipConfigured` and `tooltipWidgetConfigured` take a `TooltipConfig`:
  the hover delay (`tooltipDelay`), the grace after another tooltip
  (`tooltipGrace`), the placement (`tooltipPlacement`) and the space between
  the tooltip and its target (`tooltipGap`). `PlacementAtCursor` makes a
  tooltip follow the pointer, and opens a popup or context menu at its
  anchor point.
- Visibility sensors, for loading what scrolls into view: `sensor`,
  `sensorWith`, `sensorConfigured` and `useVisibility` report a `Visibility`
  (`visVisible`, `visRect`, the part on screen, `visBounds`, the whole
  widget, `becameVisible`, `becameHidden`). A `SensorConfig` gives an
  anticipate margin (`sensorAnticipate`) and a delay (`sensorDelay`), the
  time a widget must stay in view before it counts as visible, which the
  sensor wakes the loop for rather than drawing frames meanwhile.
- More cursor shapes from CSS's set, from `UiCursorNotAllowed` to the one-way
  resize arrows, and `UiCursorHidden`, which the SDL and RGFW backends show
  by hiding the pointer. `withCursorShape` shows a `UiCursorKind` over part of
  a view where its widgets pick none; `UiCursorDefault` from a widget picks
  nothing, and from a scope picks the arrow.
- A layout overlay, like iced's `explain`: `explainLayout` outlines every
  layout node and highlights the one under the pointer, which `explainedNode`
  describes; `explainingLayout` says whether it is on.
- Every key as a `Key`: `KeyF n`, paging, Insert, Space, the lock and menu
  keys, and a `KeyChar` of what a typing key types unmodified. `modSuper`, and
  `modPrimary` for the platform's command key (Command on macOS, else Ctrl).
  A view reads keys with `keyPressed`, `keyReleased` and `keyHeld`. Every key
  auto-repeats while held, each repeat a press in `inputKeys`, and
  `inputKeysNew` has the presses that are not repeats: holding Enter breaks
  a text area's line again and again, while Enter and Space activate a
  focused button, Enter submits a field and Escape closes only as they go
  down. `keyPressedOnce` hears that press alone. Space activates by its key,
  not by the space it types.
- `pressedIn`, `pressedOnceIn`, `releasedIn` and `heldIn` (the `Pressable`
  class) read a key or a mouse button in an `Input` alike, as `shortcutIn`
  reads a chord: `sdlAppShouldQuit = pressedOnceIn KeyEscape`.
- Shortcuts: `shortcut (ctrl <> key 's')` is `True` once on the frame the
  chord is pressed, and again on each auto-repeat, unless a modal,
  `disabledWhen` or the focused widget takes it; `shortcutOnce` is not
  `True` on the repeats, for a chord that toggles. The new module `NanoUI.Shortcut` has chords: a `Shortcut` is
  modifiers (`ctrl`, `shift`, `alt`, `super`, `cmdOrCtrl`) and a `key` put
  together with `<>`, with `shortcutLabel` and `shortcutIn`, and
  `parseShortcut` reads one written as xmonad's EZConfig writes it (`C-s`,
  `M-S-p`, `A-<Enter>`, `<F5>`). `Modifiers` is a `Monoid`.
- Input-method composition (`inputComposition`): the focused text field or
  text area draws it at its caret until it is committed, and the frame drops
  the keys meanwhile, so no shortcut fires. `textInputArea` in
  `NanoUI.Testing` says where the input method's candidate window goes.
- Every mouse button: `MouseButton` has `MouseMiddle`, the side buttons
  `MouseBack` and `MouseForward`, and `MouseOther n` for any other, which
  the SDL and RGFW backends report by number (`mouseButtonNumber`). Each is
  held, pressed and released like the left one. `respHeldWith b` and
  `respClickedWith b` say whether button `b` went down on a widget and is
  held, or clicked it (a middle click closes a closable tab), and
  `mousePressed`, `mouseReleased` and `mouseHeld` hear a button anywhere on
  the view's layer, quiet behind a modal and in `disabledWhen`, as
  `keyPressed` is.
- `mouseArea`, iced's `mouse_area`: a column around part of a view with a
  `Response` of its own, hovered while the pointer is on it or anything in
  it, and held or clicked with any button (`respHeldWith`,
  `respClickedWith`), but for a click a widget inside takes. Nothing inside
  it covers it, so what it shows while hovered stays shown.
- The pointer leaving the window moves it off every widget
  (`applyPointerLeave`, on SDL's window-leave and RGFW's mouse-leave
  events), so nothing stays hovered.
- A warning style beside the danger one: `themeWarning`, `fontWarning` and the
  `warning` button modifier.
- Following the system's light or dark setting: `followSystemTheme ctx light
  dark` or `followSystemThemeUi`, with `defaultLightTheme` as the light theme.
  `systemAppearance` reads the setting.
- The native window, the same on every backend. `WindowSettings`
  (`defaultWindowSettings`) is what a window opens with, sized in layout
  units: its title, size, position, size limits, icon, whether it resizes,
  its `WindowMode` (windowed, fullscreen or hidden), transparency, opacity,
  and whether a close request ends the session (`wsExitOnCloseRequest`).
  `askWindow` reads the `WindowState`: size, scale, position, focus,
  maximized, minimized, fullscreen, and `winCloseRequested`, the close
  request a window that does not close by itself hands its view. From a
  view, setters that act only on a change (`setWindowTitleUi`,
  `setWindowIconUi`, `setWindowMinSizeUi`, `setWindowMaxSizeUi`,
  `setWindowOpacityUi`, `setWindowModeUi`), commands that act on every call
  (`moveWindowUi`, `centerWindowUi`, `resizeWindowUi`, `minimizeWindowUi`,
  `maximizeWindowUi`, `restoreWindowUi`, `toggleMaximizedUi`), and `quitUi`,
  which ends the session once the frame is drawn. `requestScreenshot` hands
  an action a `Screenshot`, the frame's `RgbaPixels` and its scale;
  `askScreenshot` gives another thread an action that waits for one, and
  `useScreenshot` takes one per key. `RgbaPixels` come from `rgbaPixels`,
  which checks that the bytes fit the size. `RgbaImage` moved here from
  `nano-ui-sdl`, which still exports it.
- Background work: `useTaskStatus` runs an action on its own thread and says
  whether it is running, done, or failed with the exception it threw, which
  wakes the loop too; while a new key's job runs it keeps the last key's
  result. `useTask` returns the latest result, so a list of results does not
  flicker empty as its key changes. `useStream` runs a producer that updates
  the hook's own state and wakes the loop, for a stream that needs no
  `IORef` of the app's. A job is killed once the view stops calling its
  hook. `askWake` gives the view an action any thread may call to run it
  again.
- `NanoUI.Backend` has what a backend needs for the above: `applyKey`,
  `releaseAllKeys`, which the SDL and RGFW backends call as their window
  loses the keyboard, so no key stays held, `keypadKey`, `modifiersFromBits`,
  `noModifiers`,
  `applyComposition`, `cursorFallback`, `setExplainLayout`,
  `setSystemAppearance`, `WindowHost` with `defaultWindowHost` (every field
  a no-op, to build a host from by record update), `installWindowHost`,
  which also applies the settings a window does not open with,
  `reportWindowState`, `answerScreenshots`, `requestWindowClose`,
  `clearWindowClose` and `quitRequested` for a loop of the backend's own,
  `setWakeLoop` and `cancelTasks`. `runSessionLoop` ends the session when a
  view calls `quitUi`, and hands a close request to the view when the
  window's settings say to.
- `NanoUI.Testing.Harness` has `chordInp`, `keyUpInp`, `keyRepeatInp`,
  `clickPairWith`,
  `pressWith` and `releaseWith` for any mouse button, and `newWakeSignal`
  for a test to wait on a job's wake.
- `uiFontSize`, the size text takes when its layout sets none, and
  `withFontSize` in `NanoUI.Testing`; the SDL backend reports its base size.
  `drawCheckbox` and `checkboxBoxSize` draw nano-ui's checkbox on a canvas.

### Changed

- `widgetCursor` takes the widget's rect and the pointer as well as its draw
  context, so parts of a custom widget can show different shapes, and it is
  asked through a drag that went down on the widget wherever the pointer
  goes: a `knob` keeps its resize arrows while dragged off it.
- Builds with GHC 9.10 through 9.14 (`base >=4.20 && <4.23`).
- `comboBox` filters its options only when the options list or the field
  text changes, rather than on every frame, open or closed. It also measures
  the matches' width only then. Pass the same list each frame to benefit.
- `TextInputConfig` has a field `ticAdornments`, and is no longer `Eq`.
- Checkboxes, radio options and tree rows are `NodeButton`s with a look of
  their own, as tab headers are; they look and behave as before.
  `NodeType` in `NanoUI.Internal.Layout.Arena` loses `NodeCheckbox`,
  `NodeRadio` and `NodeTree`, and `NodeClass` loses `SelectionNodes`: a
  group moves its selection to the click or key read after its members were
  added, so the frame no longer copies selection state from the store into
  the nodes after the view.
- `SessionDriver` in `NanoUI.Runner`: the session loop checks every event for
  Ctrl+C itself, so `sdIsHardQuit` is gone, and `sdDraw` returns only whether
  another frame is due (`IO Bool`); no backend changed the input it was given.
- `ComboInput` in `NanoUI.Widgets.Combo` carries the dropdown's `Input` as
  `ciInput`, in place of the eight fields (`ciMouse`, `ciPressed`, `ciDown`,
  `ciScroll`, `ciKeyUp`, `ciKeyDown`, `ciEnter`, `ciEscape`) copied out of it.
- `runClickReduce` moves from `NanoUI.Testing.Assert` to
  `NanoUI.Testing.Harness`, beside the press and release helpers it uses.
  `assertWheelTitlePinned` drops its trailing `Maybe Float`, which every
  caller passed as `Nothing`.
- `WindowResizeEdge` in `NanoUI.Internal.Context.Types` is
  `WindowResizeEdge Int Int`, the side of each axis the edge moves (-1 left
  or top, 1 right or bottom, 0 neither), in place of the eight constructors
  `ResizeN` to `ResizeSW`.
- The exposed internal modules gain what the core and SDL now share:
  `insertGen` and `cachedGen` for a `GenCache` (`NanoUI.Internal.Context.Types`),
  `freshWidget`, the next id with the context (`NanoUI.Internal.Monad`), and
  `pokeQuadIndices` (`NanoUI.Internal.SIMD`). `pokeVertexSIMD` writes a vertex
  as eight scalar stores, which the SDL profile draws about 5% faster than the
  packed ones.
- A 2D scroller's scroll changes only its offset, as a 1D scroller's did, so
  the damage pass repaints the scroller's clip instead of the whole window.
- A pane grid's commands (`pgcClose`, `pgcSplit`, maximize and resize) act on
  the grid's current state, so two splits in one frame get distinct ids; a
  pane drag's threshold counts from the press point.
- A context-menu command, or `runTextCommand`, on a single-line field repaints
  that field, as it already did a text area.

- `popupWith` honours the layout its caller shapes. It built the popup node
  from the direction and size alone, so a minimum, maximum, alignment, grid or
  font set through its `Layout -> Layout` argument was dropped; padding and gap
  stay fixed at 6 and 4 as before.

- A text change inside a scroller repaints the text, and the scroller's
  scrollbar only when the content's size changed. Every text change used to
  repaint the whole scroller.

- The measure pass skips unchanged subtrees. After the view builds, each
  node's hash covers its own inputs, its ancestors' (text wraps at an
  ancestor's width) and, through its children's hashes, its whole subtree;
  the solve restores a captured node's measured size when its hash matches
  and its children all came out their captured sizes, instead of wrapping
  its text again, so a frame that changed one label re-measures that label's
  branch only. Drawing widgets and scroll containers always measure fresh,
  and a font-metric change restores nothing. Position and quantization run
  over every node, so a partially measured solve computes exactly what a
  full one would.
- Whole-layout reuse is validated by a hash over the frame's layout inputs
  instead of comparing every node's columns: the arena folds each node's
  constraints, links, text, options, widget id, style code, and grid fields
  into `getInputSignature` as the view builds, and reuse compares one word.
  Text and option lists keep a per-node hash that a repeated `Text` object
  reuses, so a steady frame hashes no string bytes. Custom-measured widgets
  no longer disable reuse: the cache records which widgets registered a
  measure and each measure's offered space and returned size at capture,
  and reuse re-runs the measures to check they still return those sizes.
- State writes repaint per key instead of escalating to a whole-window
  repaint. A store write damages its changed keys' widgets (resolved through
  the arena, including the sub-slot spellings text fields, text areas, drop
  targets, menus and colour pickers write), and the follow-up frame such a
  write requests clips rather than repaints everything. A write still
  repaints the whole window when some changed key resolves to no widget — a
  local hook's key — and so does the frame after a model change from
  `runFrameReduce`, a focus change, or `requestFrame`: each may change paint
  state (a colour, a value) that no rect or text diff describes.
- Modules that are not API moved under `NanoUI.Internal`. `NanoUI.Context`,
  `NanoUI.Context.Types`, `NanoUI.Debug`, `NanoUI.Id`,
  `NanoUI.Layout.Arena`, `NanoUI.Layout.Solve`, `NanoUI.Store`, `NanoUI.SIMD`,
  `NanoUI.Frame.Hit`, `NanoUI.Frame.TextEdit`, `NanoUI.Frame.Window` and
  `NanoUI.Widgets.SplitPane` are still exposed, as `NanoUI.Internal.*`, and
  the hidden modules moved there too.
- `NanoUI.Monad` and `NanoUI.Input` keep what a view or custom widget uses.
  `runUi`, `runNanoUI`, `askContext`, `withContext`, `withIdFrame`,
  `burstNextIds`, `FrameMsg`, `decodeMessages`, `reduceMessages`,
  `reduceUpdates`, `stripInteractionInput`, `withoutPointer`,
  `isHardQuitInput` and `splitFrame` moved to `NanoUI.Internal.Monad` and
  `NanoUI.Internal.Input`; `NanoUI.Backend` and `NanoUI.Testing` still export
  the ones they did.
- `NanoUI.Widgets.Custom`, `.TextArea`, `.TextDocument`, `.TextEditor` and
  `.TextField` no longer export the helpers the frame uses
  (`mkCustomDrawContext`, `loadTextAreaState`, `loadTextAreaStateWithBuffer`,
  `saveTextAreaState`, `textAreaEditor`, `applyTextAreaCommand`,
  `bufferDocument`, `documentBuffer`, `sameLines`, `editorModeCode`,
  `editorModeFromCode`, `applyTextFieldCommand`, `textFieldMode`,
  `textFieldHistory`, `textFieldHasText`). They are in the matching
  `NanoUI.Internal.Widgets` module.
- `searchField` is now `searchInput`, to match `textInput`, and the rest of
  its family follows: `searchInput'`, `searchInputConfigured`,
  `searchInputConfigured'`, `SearchInputConfig` and
  `defaultSearchInputConfig`. The config's fields take `textInput`'s prefix
  shape too: `sicPlaceholder`, `sicDebounceMs` and `sicLayout`, formerly
  `sfc`.
- `NanoUI` no longer exports the backend surface; import `NanoUI.Backend` for
  it. The names that moved are `runUi`, `runNanoUI`; `emptyInput`,
  `appendInputKey`, `appendDropEvent`, `inputKeysFromList`, `inputKeysNull`,
  `foldInputKeys`, `inputInteracted`, `inputPointerHeld`; `FontBackend`, `prepareFontMetrics`,
  `prepareFontMetricsMany`, `scaleFontMetrics`, `monospaceMetrics`,
  `uiFontMetrics`, `measureTextIO`, `lineWidthIO`, `drawShaped`,
  `drawGlyph`, `drawTextBox`, `GlyphQuad`, `ShapedText`, `ShapedGlyphs`, and
  the fields of `FontMetrics` other than `fmLineHeight` and `fmAscent`;
  `Damage`, `DamageBounds`, `defaultDamageSlop`, `sliderDamageSlop`,
  `haloDamageSlop`, `resolveDamageRect`, `damageWidgetNow`, `damageKeyNow`,
  `damageRectNow`, `damageGroupNow`, `damageFullNow`; `widgetContentInset`,
  `widgetPadding`, `treeItemPadding`, `ScrollBarSlot`, `scrollBarGutter`,
  `scrollBarWidth`, `windowPad`, `windowMargin`; `IdContext`,
  `initialIdContext`, `widgetId`, `hashWidgetId`, `mix64`, `mixFnv`,
  `burstNextIds`; and `Compact`, `compactHost`, `askCompact`. `Input`, `Key`,
  `Modifiers`, `inputKeysElem`, `WidgetId`, `nextId`, `currentId`, `DrawOp`
  and `shiftDrawOp` stay in `NanoUI`: a view reads input and a custom widget
  builds draw ops. So do `FontMetrics`, with `fmLineHeight` and `fmAscent`,
  and `lineWidth`, because a custom widget's measure function and `cdcFont`
  hand it metrics to size its text with.

- SVG parsing moved to the `nano-svg` package; `NanoUI.Svg` keeps only the
  rasterizer. `parseSvg` takes a UTF-8 `ByteString` instead of `Text`, so
  `loadSvg` no longer decodes the file first. `Svg` is now `Document` from
  `Graphics.NanoSvg`, and `svgSize`, `svgKey` and `svgMonochrome` read its
  fields. Documents also gain `use` and `switch`, `display` and `visibility`,
  absolute unit lengths (`pt`, `pc`, `in`, `cm`, `mm`), percentage opacities,
  `rgba()`, `hsl()` and the full CSS colour keyword table.

- `NanoUI.Emit` exposes `emitWhen`, `emitChanged`, and `emitEdited` instead of
  separate copies of widget names. Replace `Emit.button label msg` with
  `Emit.emitWhen (button label) msg`, `Emit.slider lo hi value toMsg` with
  `Emit.emitChanged (slider lo hi) value toMsg`, and `Emit.textArea value toMsg`
  with `Emit.emitEdited textArea' value toMsg`. Configured and custom controls
  use the same adapters. `emitChanged` compares values; `emitEdited` also gates
  on `respChanged`.

- `drawCommands` is an unboxed `Vector DrawCmd`; its field representation uses
  vector's deriving-via support instead of a handwritten `Prim` instance. Vertex
  and index buffers keep their existing FFI layout. Use `drawCmdElems` or
  `forDrawCmdsInLayer_` for representation-independent command access.

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
- The node index by widget id is an unboxed table, so indexing and looking
  up a widget allocate nothing, and `nano-ui` no longer depends on
  `hashtables`.
- Tooltips open once the pointer has rested on the target for half a second
  (`defaultTooltipConfig`), or at once just after another, and shut while a
  button is held. `tooltipAt PlacementAtCursor` follows the pointer. A
  disabled widget has its tooltip too, where the pointer is on it with
  nothing drawn over it, so it can say why it is off.
- `menuItemShortcut` is also `True` when its chord is pressed while its menu
  is open, and shows the chord as its `shortcutLabel`.
- A key chord is a key, not typed text: Ctrl+C is `KeyChar 'c'` with `modCtrl`
  in `inputKeys` and nothing in `inputChars`, so look for it with `shortcut`
  or in `inputKeys`. Text fields take Command as well as Ctrl on macOS.
- `Key` is `Ord` and no longer `Enum`, and `Modifiers` is `Ord`.
- `Input` holds the mouse buttons as sets, `inputButtonsHeld`,
  `inputButtonsPressed` and `inputButtonsReleased` (`MouseButtons`, read with
  `heldIn`, `pressedIn` and `releasedIn`), in place of a field
  for each button and edge; an `Input` is 128 bytes rather than 200. Fold
  events in with `applyMouseButton`. `inputMouseDown`, `inputMousePressed`,
  `inputMouseReleased` and their `Right` forms remain as deprecated functions.
  A `Response` keeps the buttons held on the widget and those that clicked it
  the same way (`rawRespHeld`, `rawRespClickedWith`), 64 bytes rather than 96;
  `respPressed`, `respRightPressed` and `respRightClicked` read them.
- A right or middle button held is a widget's only when it went down on the
  widget: dragged across others, it no longer reports each as pressed.
- A double or triple click counts presses of one button: a left click
  after a right one starts over.
- `Input`, `Modifiers` (`modSuper`), `MouseButton`, `Response`, `Layout`,
  `Theme`, `FontVariant`, `DrawOp` and `UiCursorKind` have new fields or
  constructors, for the additions above.
- `setTheme` and `setUiTheme` stop a context following the system's
  appearance.

### Fixed

- The wheel goes to the scroller drawn on top at the pointer: a scroller
  pinned over another takes it even when declared before the one beneath.
- A text field focused by Tab no longer has its whole text selected after a
  press elsewhere.
- A text field with its own font size puts its caret, selection and
  hit-testing where it draws its glyphs.
- A widget with its own font size, such as `buttonWith (fontSize 24)`, is
  laid out for its label in that font, so the label no longer spills out.
- The first press on a text field left of its text puts the caret at the
  start, not the end.
- A text field with its own font size draws its value in that font when the
  draw arena draws text (RGFW, headless). Its pen and caret were placed in
  that font, but the glyphs came from the base font.
- Ctrl+C quits a session even when Ctrl is released later in the same batch
  of events. The loop looked only at the last event's modifiers, so RGFW
  missed it during a busy frame.
- Grow children of a column with a gap share the height left after the gaps,
  as a row's do. They shared the whole height, so the column's children ran
  past its bottom by the gaps between them.
- The text spans of an open select dropdown give the picked row the accent
  colour it is painted in. A host that draws text from the spans drew it in
  the menu's text colour.
- A settled `animateTo` value holds its target. Settled values were dropped
  once their key had gone 300 frames without a widget rect, and an
  `animateTo` key never has one, so about 300 frames after settling the value
  read 0 and animated back up. A settled value now stays while a view reads
  or sets it or its widget is laid out, and one nothing uses is dropped
  within 300 to 600 frames.
- Ctrl+Shift+Z redoes in text fields and areas when the backend delivers it
  as the control code for Z rather than the letter. It undid instead.
- A click the view missed because the widget moved on the frame the button
  came up asks for the frame that reports it. Unless the release also changed
  the widget's hover, the click could wait for the next input. Radio options and
  tab headers report such a click like any other widget, where they used to
  write their group's selection behind the view's back.
- A scroller whose content grows or shrinks without its text changing (a box
  resized, a row added inside a floating window) repaints its scrollbar.

- Table sort arrows, select and combo chevrons, numeric stepper arrows and
  `FillTriangle` are anti-aliased, and keep their shape: each corner used to
  snap to the pixel grid on its own, so an arrow centred between two pixels
  came out lopsided. The shape now moves to the grid as a whole and its
  edges fade out across one device pixel. Tree chevrons are one mitered
  anti-aliased line instead of two lines with square single-pixel caps.
- A right-aligned table column shows its sort arrow on the left of the
  header, and reserves the arrow's slot there, instead of on the right,
  across the column from its label.
- Starting or ending a drag on a colour picker's field or bars, a slider, or
  a knob repaints only the widget, not the whole window. The drag hooks kept
  their held flag in a store slot no widget owns, and a changed slot with no
  owner repaints everything. They now keep it in `storeQuiet`, a map for
  bookkeeping that no paint reads and the damage diff skips.
- A scroller's offset set past its content, or left over from content that
  has since shrunk, is held to the content's range at the next layout, on the
  axes the scroller owns. It used to stay out of range.
- A hairline rounded border is drawn at the same strength all the way round.
  A quad's alpha is read at the middle of each pixel, so a straight side,
  snapped to the grid, comes out at the colour it was asked for, while an arc
  passes between the pixels and each one is read some way down the fade
  either side of it. With a whole pixel of fade and nothing solid between,
  the brightest pixel of a corner reached about three quarters of the colour
  its own straight edges were drawn in, and the corner read lighter than
  them. The fade is half a pixel now and the rest of the width is solid: the
  same ink over a narrower band (`arcFeather` in `NanoUI.Draw.Shapes`).

- A pinned pane keeps a whole number of layout units, on whichever side of
  its split it is pinned. Its extent is kept as a ratio of a region that
  changes size, and going out to a ratio and back walked it a fraction at a
  time on every frame of a resize drag.

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
- A widget that shrinks under the pointer repaints the strip it vacated, and
  content overflowing the root takes the pointer where it is drawn.
- A widget that goes away next to a table with frozen columns repaints where
  it was.
- Content of a scroller wholly outside the viewport around it, such as an
  inner scroller below the fold, takes no pointer.
- A panel or a scroller that changes size repaints where it was and where it
  is.
- A row or column too short for its children takes the room it lacks from
  the others that shrink once one reaches its minimum, instead of overflowing.
- A canvas's `drawImage` and `drawImageUV`, and a drawing's `DrawImageRect`,
  draw the registered image rather than a rectangle in the tint colour.
- A tooltip on a label or a container (`label'`, `withTooltip`) starts its
  wait as the pointer comes onto the target and shuts as it leaves, rather
  than at the next unrelated event.
- A wrapped label keeps its indent and the runs of spaces inside its lines;
  a line ends at a run of spaces, which it drops. A label wider than its box
  used to have every run of spaces cut to one.
- Text as wide as its content in a column, such as a right-aligned label, is
  aligned at the width it wraps to there rather than its one-line width, which
  could put it past the column's left edge. Rich text places each line as its
  paragraph's horizontal alignment says.
- Past 4096 paragraphs, rich text keeps the paragraphs drawn lately instead of
  emptying its layout cache on every frame.

### Removed

- `withExternalText` from `NanoUI.Testing`: nothing read the flag it set.
- `Compact`, `compactHost` and `askCompact` from `NanoUI.Testing`;
  `NanoUI.Backend` exports them.
- `fbDrawGlyph` from `FontBackend`. No backend drew per-character glyphs
  through it (every non-empty SDL line is shaped); `drawGlyph` now reads
  `fmGlyph` from the metrics snapshot.
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
- `keyed`, an alias of `withKey`.
- The `x'` and `xWith` forms of `knob`, `toggleSwitch`, `circularProgress`,
  `spinner`, `progressBar` and `sparkline`. Each keeps `x`, at its default
  size, and `xWith'`, which takes a layout modifier and a size and returns
  the `Response` too.
- `emptyInputKeys` and `emptyDropEvents` from `NanoUI.Backend` and
  `NanoUI.Input`; use `mempty`.

## 0.1.0.0

First release.
