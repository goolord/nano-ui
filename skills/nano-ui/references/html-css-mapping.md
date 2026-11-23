# From HTML, CSS, and React to nano-ui

How web UI mental models translate to `nano-ui`:

- **No retained DOM.** Your view is a function that runs every frame and re-declares the whole window. Keep persistent state in `Var`s.
- **No event listeners.** Widgets return what happened on the current frame; event handling is just a `when` block right after the widget call.
- **No CSS selector cascade.** Visual appearance comes from a `Theme` scoped with `styled`; layout and typography are modifier arguments on individual widgets.
- **Everything shrinks to fit by default.** A `column` is not a CSS block container: children stay at their natural width unless given `fillW`.

## Elements

| HTML | nano-ui |
| --- | --- |
| `<div style="display:flex">` | `row` / `rowWith mods` |
| `<div style="display:flex; flex-direction:column">` | `column` / `columnWith mods` |
| `<div>` with background and border | `panel` / `panelWith mods` |
| `<article class="card">` | `card` (full width, minimum width, panel surface) |
| `<aside class="alert warning">` | `callout color body` (tinted panel), color from `themeWarning <$> uiTheme` |
| `<header>`, `<nav>`, `<main>`, `<footer>`, `<section>` | `column` / `row` / `panel` organized into plain Haskell functions |
| `<h1>`..`<h6>` | `heading`, or `labelWith (fontSize 28 . fontBold)` for custom sizes |
| `<p>` | `label` (wraps inside a column), `muted` for secondary text |
| `<span style="color:...">` | `labelWith (fontColor c)` or `labelWith (fontTone Danger)` |
| `<strong>`, `<em>`, `<u>`, `<code>` | `bold`, `italic`, `underline`, `mono` for whole labels |
| Mixed inline styles | `richText ["Run ", strong "build", " then ", emphasis "test"]` |
| `<a href>` | `hyperlink url "label"` inside `richText` (returns `Just url` when clicked) |
| `<pre>` | `labelWith fontMono`, or `selectableTextWith fontMono` for copyable text |
| `<button>` | `button "Save"` returning `Bool`; `buttonContent body` for custom button contents |
| `<button>` with icon | `iconButton svg "Save"` (pass `""` for a square icon-only button) |
| `<input type="text">` | `textInput var` |
| `placeholder`, `type="password"` | `textInputConfigured defaultTextInputConfig {ticPlaceholder = "Name", ticPassword = True} var` |
| `<input type="search">` | `searchInput "Search" var` (debounced `respChanged`) |
| `<input type="number">` | `numericInput var` (`Var Double`) |
| `<input type="checkbox">` | `checkbox "Label" var` (`Var Bool`); `toggleSwitch var` for a switch |
| `<input type="radio">` group | `radio ["A", "B"] var` (`Var Int`), `enumRadio var` for an `Enum` |
| `<input type="range">` | `slider lo hi var` (`Var Float`) |
| `<input type="color">` | `colorPicker var` (`Var Color`) |
| `<input type="file">` | SDL `askOpenFileDialog` + `useFileDialog` (native, non-blocking) |
| `<select>` | `select options var` (`Var Int`), `enumSelect var` |
| `<input list>` + `<datalist>` | `comboBox "Placeholder" options var` (`Var Text`) |
| `<textarea>` | `textArea var`; `textAreaDocument` for large files |
| `<label for>` | Place a `label` next to the field (`kv` for key/value pairs) |
| `<form>` + submit | Submit on a button or `respSubmitted` of `textInput'`; use `nano-ui-form` for validated forms |
| `<fieldset disabled>`, `disabled` | `disabledWhen cond subtree` |
| `<hr>` | `separator` |
| `<img>` | `image mods imageId`; `imageConfigured` for `object-fit` options |
| Inline `<svg>` icon | `parseSvg` once at setup, then `svgIcon 16 svg` (inherits text color) |
| `<canvas>` | `canvas mods (\rect -> drawing)` from `NanoUI.Widgets.Custom` |
| `<table>` | `simpleTable headers rows`, or `table` with Colonnade and `Var SortCol` |
| `<ul>` / `<ol>` + `<li>` | `column` + `for_ items (\i -> withKey (itemId i) (row ...))` |
| `<details>` / `<summary>` | A `Var Bool`, a toggle button, and `scope (when open body)` |
| Tree view | `tree` with `TreeItem`s |
| `<dialog>` / modal | `modal openVar "Title" body`; `window` for a draggable floating panel |
| Popover / dropdown panel | `popup openVar (defaultPopupConfig (AnchorRect (respRect resp))) body` |
| `title="..."` | `tooltip resp "text"` (takes a primed widget's `Response`) |
| `oncontextmenu` | `contextMenu resp (menuItem ...)`, or `contextMenuArea` for a region |
| Menu bar | `menuButton'` triggers paired with `popup'` dropdowns (`examples/Notepad.hs`) |
| Tab strip | `tabs var [tab k "Title" body, ...]`, `enumTabs`, `tabBar` for headers only |
| `<progress>` | `progressBar fraction` (0 to 1), `circularProgress`, `spinner` |
| `<meter>`, sparkline | `sparkline values`; full charts in `nano-ui-diagrams` (`NanoUI.Plot`) |
| Split panes | `paneGrid` (resizable, dockable panes) |
| `<iframe>`, `<video>`, `<audio>` | Not built in |

## Layout properties

`nano-ui` uses one unified flexbox-style layout solver: every container is a row, column, grid, or stack, with no block/inline distinction and no CSS grid areas.

| CSS | nano-ui |
| --- | --- |
| `display: flex` | `row` |
| `flex-direction: column` | `column` |
| `gap: 8px` | `gap 8` |
| `flex-wrap: wrap` | `rowWith (wrap . gap 6)` |
| `row-gap` between wrapped lines | `lineGap 4` |
| `align-content` for wrapped lines | `lineAlign LinesCenter` |
| `flex: 1` | `fillW` inside a row, `fillH` inside a column |
| `flex: 1` on both axes | `grow` |
| `justify-content: space-between` | `spacer (Grow 1) Fit` (or `flex`) between children |
| `justify-content: flex-end` | Leading `spacer (Grow 1) Fit`, or `alignEnd` on a single child in a column |
| `justify-content: center` | `center body` (both axes), or spacers on both sides |
| `align-items: center` in a row | `alignMid` on **each child** (modifiers align the node they are placed on) |
| `align-items: center` in a column | `alignCenter` on each child |
| `align-items: baseline` | `alignBaseline` on each child |
| `align-self` | Same alignment modifier on that individual child |
| `align-items: stretch` | `fillH` (row) or `fillW` (column) on children |
| `display: grid; grid-template-columns: repeat(3, 1fr)` | `gridWith 3 (gap 8 . fillW)`, children `fillW` |
| `repeat(auto-fill, minmax(160px, 1fr))` | `gridMinColW 160` on the grid |
| `width` / `height` | `fixedW` / `fixedH` / `fixedWH` |
| `min-width`, `max-width`, `min-height`, `max-height` | `minW`, `maxW`, `minH`, `maxH` |
| `width: 50%` | `percent 50` (width only, relative to parent width) |
| `height: 50%` | Not built in: use `fillH` with siblings or compute from `windowHeight` |
| `aspect-ratio: 16/9` | `aspect (16 / 9)` |
| `width: fit-content` | Default |
| `box-sizing: border-box` | Default (padding sits inside fixed sizes) |
| `padding: 12px` | `padAll 12` |
| `padding: 4px 12px` (vertical horizontal) | `padXY 12 4` (**horizontal first**) |
| `padding: t r b l` | `padLRTB l r t b` (**left, right, top, bottom**) |
| `padding-left` etc. | `padLeft`, `padRight`, `padTop`, `padBottom` |
| `padding: 0` | `tight` (containers default to 3 px padding and an 8 px gap) |
| `margin` | None: use parent `gap` / padding or `spacer (Fixed 16) Fit` |
| `margin: 0 auto` | `alignCenter` on a child of a column |
| `margin-left: auto` | `spacer (Grow 1) Fit` before the child in a row |
| `position: relative` + `absolute` | `pinAt x y` on the child (alignment picks the anchor corner, then shifts by `x`, `y`) |
| `position: absolute; inset: 0` | `pinAt 0 0 . grow` |
| `position: fixed` | An overlay (`modal`, `window`, `popup`), or `pinAt` on a root child |
| `position: sticky` | None: place the sticky header outside the scroller |
| `z-index` | Call order inside `layers` (later draws on top); overlays always sit above the page |
| `overflow: auto` | `scrollWith (fixedH 240 . fillW) body` (needs bounded size), `scroll2D` for both axes |
| `overflow-x: hidden; text-overflow: ellipsis` | `labelWith fillW` (or `maxW`) inside a row truncates with `...` |
| `white-space: normal` | `label` inside a column wraps to the column width |
| `overflow-wrap: anywhere` | `breakAnywhere` |
| `display: none` | Skip the widget call inside `scope` |
| `visibility: hidden` | None: reserve the slot with a fixed `spacer` |
| `@media (max-width: 640px)` | `responsiveRowCol 640 mods body`, `responsive 640 wide narrow body`, or branch on `windowWidth` |
| `pointer-events: none` | `pointer PointerPass` |
| Click-blocking backdrop | `pointer PointerBlock` on a `pinAt 0 0 . grow` panel |
| `cursor: pointer` etc. | `withCursorShape UiCursorPointer subtree` |
| `em`, `rem`, `vw`, `vh` | Logical pixels only; compute from `uiFontSize`, `windowWidth`, `windowHeight` |

## Visual properties

Colors, borders, and corner radii live in the active `Theme`. Adjust them for a subtree with `styled`, targeting the surface slot (`buttonStyle`, `inputStyle`, `panelStyle`, `popupStyle`).

| CSS | nano-ui |
| --- | --- |
| CSS variables / design tokens | A `Palette` record and a `Theme` constructed from it |
| Utility class like `.btn-primary` | A `Theme -> Theme` function (`primary` or custom) applied with `styled` |
| Reusable component | A Haskell function returning `NanoUI a` |
| `background` on a panel | `styled (panelStyle (background c)) (panel body)` |
| `background` on buttons | `styled (buttonStyle (background c)) ...`; `everyStyle` for all surfaces |
| Solid colored rectangle | `box mods color` |
| `color` | `labelWith (fontColor c)` per widget, or `styled (textColor c)` for a subtree |
| `border`, `border-width`, `border-color` | `borderWidth 1 . borderColor c` inside a style modifier |
| `border-bottom` only | `borderBottom 1 c`, or `borderSides [...]` |
| `border-radius` | `cornerRadius 6` in a style, or `rounded 6` across the whole theme |
| `:hover`, `:active` background | `hoverBackground c`, `pressBackground c` |
| Other `:hover` changes | Inspect `respHovered` on a primed widget or `mouseArea` |
| `:focus` outline | Automatic (`themeFocusRing`); query with `isFocused` |
| `:disabled` | `disabledWhen` |
| `opacity` | `withAlpha` on colors; `icOpacity` on images |
| `font-size` | `fontSize 18` on the text widget; `sdlAppFontSize` for app default |
| `font-weight: bold` | `fontBold`, `fontWeight WeightSemiBold` |
| `font-style: italic` | `fontItalic` |
| `font-family` | `fontMono` per widget; `sdlAppFont` / `sdlAppMonoFont` globally |
| `text-decoration` | `fontUnderline`, `fontStrike` |
| `text-align: center` (single line) | `labelWith alignCenter` inside a column |
| `object-fit`, `object-position` | `imageConfigured defaultImageConfig {icFit = FitCover, ...}` |
| `box-shadow`, `linear-gradient` | Draw in a `canvas` (`P.Linear` for gradients) |
| `prefers-color-scheme` | `followSystemTheme`, or `lightDark` with `systemAppearance` |
| `transition` | `animateTo (Tween EaseOutCubic 0.2 0) target` |
| `@keyframes` | `animate` or `pulse` paired with `keepAnimating` |

Font modifiers do not cascade from parent containers (`columnWith (fontSize 20)` does not resize child labels). Put font modifiers on each text widget or define a small helper (`big = labelWith (fontSize 20)`). Theme changes via `styled` do cascade through the whole subtree.

## Events and DOM APIs

| Web | nano-ui |
| --- | --- |
| `onclick` | `whenM (button "Go") go`, or `when (respClicked resp)` |
| `onclick` on a card or row | `(_, resp) <- mouseArea mods body`, then check `respClicked resp` |
| `oninput` | Input mutates its `Var` directly; read it below, or check `respChanged` |
| `onchange` / `onsubmit` / Enter | `respSubmitted` from `textInput'` |
| `onmouseenter` / `onmouseleave` | Track `respHovered` across frames |
| `onmousedown`, middle/back buttons | `respPressed`, `respClickedWith MouseMiddle`, `mousePressed MouseBack` |
| `ondblclick` | On press frame (`respPressed` && `mousePressed MouseLeft`), `inputMouseClicks <$> askInput == 2` |
| `oncontextmenu` | `contextMenu resp body`, or `respRightClicked` |
| `onwheel` | Scrollers handle automatically; `useWheelDeltaOn resp` in custom widgets |
| `keydown` on `document` | `shortcut (ctrl <> key 's')`, `keyPressed KeyEscape` |
| `keydown` on an element | Focused custom widget with `focusedKeyPressed` / `useKeyNav` |
| `element.focus()`, `autofocus` | `requestFocus (respId resp)` (safe to call every frame; no-op when already focused) |
| `element.blur()` | `clearFocus` |
| `getBoundingClientRect()` | `respRect resp` (previous frame's bounds) |
| `scrollIntoView()` | `scrollIntoView` / `scrollRectIntoViewUi` |
| `IntersectionObserver` | `sensor` / `sensorConfigured` / `useVisibility` |
| `ResizeObserver` | `respRect` and `windowSize`, read each frame |
| HTML5 drag and drop | `useDrag`, `useReorder` + `insertionIndex`; `dropZone` for OS file drops |
| `setTimeout`, `setInterval` | `wakeAfter seconds` each frame, comparing against `uiTime` |
| `requestAnimationFrame` | `keepAnimating resp` inside the visible animating widget |
| `fetch` | `useTask task key (IO action)` |
| WebSocket, EventSource | `useStream stream key initial producer` |
| `navigator.clipboard` | `getClipboard`, `setClipboard` |
| `window.open(url)` | SDL `openUrl url` (in `IO`) |
| `document.title` | `setWindowTitleUi` (cached no-op when unchanged) |
| `beforeunload` | `wsExitOnCloseRequest = False`, `winCloseRequested <$> askWindow`, then `quitUi` |
| `localStorage` | Persist manually in `IO` on change or close |
| `document.querySelector` | Not needed: keep the `Response` or `WidgetId` directly |
| `window.innerWidth` | `windowWidth`, `windowSize` |

## React habits

| React | nano-ui |
| --- | --- |
| Component | Function returning `NanoUI a` |
| Props | Regular function arguments |
| `useState` | `useVar` for `Int`/`Double`/`Float`/`Bool`/`Text`/`SortCol`, `useEnumVar` for enums, `newVar` at setup for any other type |
| Lifting state up | Allocate the `Var` higher up (or in setup) and pass it down |
| Controlled input (`value` + `onChange`) | Pass the `Var`, or `constVar x` + primed `Response` for derived values |
| `useReducer`, Redux | `nano-ui-emit`: `Model -> NanoUIE Msg ()` with `runSdlAppReduce` |
| `key` on list items | `withKey stableKey` (never list index) |
| `{cond && <X />}` | `scope (when cond x)` (except overlays, which always run unconditionally) |
| `useEffect(() => fetch ..., [dep])` | `useTask task dep action` |
| `useEffect` cleanup | Task cancels on key change or skipped frame; use `bracket` inside |
| `useMemo` | Compute outside the view or cache in an `IORef` keyed by revision; pass a content key to custom widgets |
| `useRef` to DOM node | Keep the widget's `Response` or `respId` |
| Context provider | Function arguments or app record; `styled` / `themed` for themes |
| Portals | Not needed: `modal`, `popup`, `window`, and `tooltip` render on overlay layers automatically |
| `React.memo` | Not needed: the view runs every frame and damage tracking repaints only changes; keep views cheap instead |
| CSS-in-JS | Composable Haskell modifier functions |

## Worked examples

### App shell: header, sidebar, scrolling main area

```html
<body style="display:flex; flex-direction:column; height:100vh">
  <header style="display:flex; align-items:center; padding:8px 16px; gap:8px">
    <h1>Inbox</h1>
    <span style="flex:1"></span>
    <input type="search" placeholder="Search mail">
  </header>
  <div style="display:flex; flex:1">
    <nav style="width:200px">...</nav>
    <main style="flex:1; overflow:auto">...</main>
  </div>
</body>
```

```haskell
shell :: Var Text -> NanoUI ()
shell query =
  columnWith (grow . tight . gap 0) $ do
    rowWith (fillW . padXY 16 8 . gap 8) $ do
      labelWith (alignMid . fontSize 22 . fontBold) "Inbox"
      spacer (Grow 1) Fit
      searchInput "Search mail" query
    separator
    rowWith (grow . tight . gap 0) $ do
      columnWith (fixedW 200 . fillH . padAll 8) sidebar
      scrollWith grow $ columnWith (fillW . padAll 16 . gap 12) content
```

`separator` draws one clean divider at the seam instead of double borders.

### Clickable list of cards

```html
<ul>
  <li class="card" onclick="open(id)">
    <strong>{title}</strong> <span class="muted">{date}</span>
  </li>
</ul>
```

```haskell
messageList :: [Message] -> Var Int -> NanoUI ()
messageList msgs selected =
  columnWith (fillW . gap 6) $
    for_ msgs $ \m -> withKey (msgId m) $ do
      (_, area) <- panelWith fillW $
        mouseArea (fillW . padAll 10 . gap 4) $ do
          rowWith (fillW . tight) $ do
            labelWith (fillW . fontBold) (msgTitle m)
            muted (msgDate m)
          labelWith fillW (msgPreview m)
      when (respClicked area) (writeVar selected (msgId m))
```

`withKey` keeps per-row state attached across inserts and deletes, and `labelWith fillW` truncates long titles with `...` instead of stretching the list horizontally.

### Login form

```html
<form onsubmit="login()">
  <input name="user" placeholder="User name">
  <input name="pass" type="password" placeholder="Password">
  <label><input type="checkbox" name="remember"> Remember me</label>
  <button type="submit" disabled={!valid}>Sign in</button>
</form>
```

```haskell
loginForm :: Login -> NanoUI ()
loginForm f =
  columnWith (fixedW 320 . padAll 16 . gap 10) $ do
    textInputConfigured defaultTextInputConfig {ticPlaceholder = "User name"} f.user
    pass <- textInputConfigured' defaultTextInputConfig {ticPlaceholder = "Password", ticPassword = True} f.password
    checkbox "Remember me" f.remember
    user <- readVar f.user
    clicked <- disabledWhen (T.null user) $ styled primary (buttonWith fillW "Sign in")
    when (clicked || respSubmitted pass) (signIn f)
```

`Login` is a record of `Var`s allocated at setup, with `readVar` placed below the input that edits it.

### Badge pinned to a button

```html
<div style="position:relative">
  <button>Inbox</button>
  <span class="badge" style="position:absolute; top:-6px; right:-6px">3</span>
</div>
```

```haskell
inboxButton :: Int -> NanoUI Bool
inboxButton unread =
  layers $ do
    clicked <- buttonWith (fixedWH 120 36) "Inbox"
    let red = colorRGB 204 64 64
    styled (panelStyle (background red . borderColor red)) $
      panelWith (pinAt 6 (-6) . alignEnd . alignTop . padXY 6 1) $
        labelWith (fontColor colorWhite) (T.pack (show unread))
    pure clicked
```

Alignment chooses the anchor corner (`alignEnd . alignTop` is top-right), and `pinAt x y` offsets rightward and downward from there (so CSS `right: -6px` becomes `x = 6`).

### Collapsible disclosure (`details` / `summary`)

```haskell
details :: Text -> NanoUI () -> NanoUI ()
details title body = column $ do
  open <- useVar False
  isOpen <- readVar open
  whenM (button ((if isOpen then "v " else "> ") <> title)) (modifyVar open not)
  scope (when isOpen body)
```

### Hyperlinks

```haskell
footer :: NanoUI ()
footer = do
  clicked <- richText ["Read the ", hyperlink "https://goolord.github.io/nano-ui/book/" "book", " first."]
  for_ clicked (liftIO . void . openUrl)
```

`openUrl` lives in `NanoUI.Backend.Sdl`. Always check the URL scheme before opening untrusted links.

### Responsive breakpoint

```haskell
page :: NanoUI ()
page =
  responsiveRowCol 640 (fillW . tight . gap 12) $ do
    panelWith (fillW . padAll 12) filters
    panelWith (fillW . padAll 12) results
```

Below 640 logical pixels wide the panels stack vertically; at 640 px and above they sit side by side.

## Web concepts that do not carry over

- **Margins, `z-index`, `position: sticky`, CSS grid areas, percentage heights:** Use gaps/padding, child order inside `layers`, headers outside scrollers, nested rows/columns, and `fillH`.
- **Cascading text styles:** Apply font modifiers directly on each text widget.
- **Default text selection:** Plain `label`s are not selectable; use `selectableText` or `selectableRichText` where copyability matters.
- **Synchronous layout queries during render:** `respRect` and scroll metrics always come from the previous frame.
- **Unmounting cleanup:** Hidden widgets retain their state (`useVar`, caret) for when they reappear; `useTask` and `useStream` cancel automatically after one skipped frame.
- **CSS shadows and gradient backgrounds:** Draw custom gradients or shadows inside a `canvas`.
