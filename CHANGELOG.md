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
- Shaped text in the SDL backend. Lines are laid out by SDL_ttf and
  HarfBuzz with ligatures, contextual forms and marks, and drawn glyph by
  glyph from the atlas. Arabic, Hebrew, Devanagari, CJK and other scripts
  the UI font lacks are drawn from installed fallback fonts (Noto, DejaVu,
  and the Windows and macOS system fonts), found the first time a text
  needs them.
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

### Changed

- `panelStyled` and `panelStyledWith` are gone; use
  `styled (panelStyle (background bg . borderColor border))` around a panel.
  `callout` tints the theme's panel colour instead of a fixed dark grey.
- `uiTheme` returns the theme of the enclosing `styled` scope.
- Draw ops are a `SmallArray DrawOp` from `primitive` instead of a boxed
  `Vector`: `DrawingBuild`, `CustomDrawBuild`, `runCanvas`, `drawing`,
  `drawingVersioned`, `emitDrawOps`, and in nano-ui-diagrams `diagramOps`,
  `diagramTextOps`, `diagramFrame` and `labelFitScale`. Build them with
  `smallArrayFromList`; the arrays are `Foldable`, `Eq` and a `Semigroup`.
- `inputKeys` and `inputDrops` are `SmallArray`s. `appendDropEvent` adds a
  drop the way `appendInputKey` adds a key.
- `CategoryY` holds its labels and values apart, as
  `CategoryY (SmallArray Text) (PrimArray Double)`. `bar` and `barVec` build
  it as before.
- Tree rows, table column metrics and sizes, the SVG rasterizer's buffers,
  polygon triangulation and stroking, and the Cozette glyph table use
  `primitive` arrays. Stroking a 10,000-point line allocates 2.0 MB instead
  of 4.7 MB and takes about 0.34 ms instead of 0.59 ms; a 50-row tree frame
  allocates 213 KB instead of 275 KB. Plot series stay unboxed vectors, since
  they come from users and a visible range will be sliced out of them.
  nano-ui-sdl and nano-ui-rgfw no longer depend on `vector`.
- A custom widget with a measure hook and a fit height is measured again at
  the width layout gives it, as wrapped labels are, so its height can follow
  its width.
- Custom widgets with a content key rebuild their ops when a theme scope
  changes, instead of keeping the old theme's colours.
- `RunQuad`, `fmRun` and `drawRun` are replaced by `fmShape` and
  `drawShaped`. A backend without shaping sets `fmShape = const Nothing` and
  keeps per-character advances and kerning.
- In the SDL backend, bold and italic are drawn by the core's synthetic
  weight and slant over the regular face, since SDL_ttf's style flags do not
  match the glyph images shaped text draws. Text measurement uses the shaped
  width, so layout and drawing agree.
- `MenuAction` is gone; `takeTextEditLastAction` now reports the
  `TextCommand` a text field's menu ran.
- Text areas keep their document as a finger tree of lines and repaint only
  the lines in view, so a keystroke in a 100,000-line document costs about
  1.4 ms on the headless profiler instead of about 26 ms.

- A wheel notch scrolls three text lines instead of one, matching what
  desktops send a notch as. `setScrollTuning` puts it back.

## 0.1.0.0

First release.
