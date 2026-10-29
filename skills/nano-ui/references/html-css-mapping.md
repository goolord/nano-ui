# From HTML, CSS and React to nano-ui

How web UI habits translate. The short version:

- There is no document. The view is a function that runs every frame and
  re-declares the whole UI. What you would keep in the DOM, keep in `Var`s.
- There are no event listeners. A widget returns what happened this frame;
  the "handler" is the `when` after the call.
- There is no cascade of selectors. Look is a `Theme` scoped with `styled`;
  layout and text style are modifier arguments on each widget.
- Everything fits its content unless told otherwise. A `column` is not a block
  element: children do not stretch to its width until they get `fillW`.

## Elements

| HTML | nano-ui |
| --- | --- |
| `<div style="display:flex">` | `row` / `rowWith mods` |
| `<div style="display:flex; flex-direction:column">`, a block of stacked content | `column` / `columnWith mods` |
| `<div>` with a background and border | `panel` / `panelWith mods` |
| `<article class="card">` | `card` (full width, minimum width, panel surface) |
| `<aside class="alert warning">` | `callout colour body` (tinted panel), colour from `themeWarning <$> uiTheme` |
| `<header>`, `<nav>`, `<main>`, `<footer>`, `<section>` | `column`/`row`/`panel`: no semantic containers, structure is just functions |
| `<h1>`..`<h6>` | `heading`, or `labelWith (fontSize 28 . fontBold)` for a custom scale |
| `<p>` | `label` (wraps inside a column), `muted` for secondary text |
| `<span style="color:...">` | `labelWith (fontColor c)` or `labelWith (fontTone Danger)` |
| `<strong>`, `<em>`, `<u>`, `<code>` | `bold`, `italic`, `underline`, `mono` as whole labels |
| mixed inline text | `richText ["Run ", strong "build", " then ", emphasis "test"]` |
| `<a href>` | `hyperlink url "label"` inside `richText`, which returns `Just url` on the clicked frame |
| `<pre>` | `labelWith fontMono`, or `selectableTextWith fontMono` for copyable text |
| `<button>` | `button "Save"` returning `Bool`; `buttonContent body` for arbitrary content |
| `<button>` with an icon | `iconButton svg "Save"` (icon then label; `""` for an icon-only square) |
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
| `<textarea>` | `textArea var`; `textAreaDocument` for large documents |
| `<label for>` | no association; put a `label` next to the field (`kv` for key/value rows) |
| `<form>` + submit | no form element: submit on a button or on `respSubmitted` of `textInput'`; `nano-ui-form` for validated forms |
| `<fieldset disabled>`, `disabled` | `disabledWhen cond subtree` |
| `<hr>` | `separator` |
| `<img>` | `image mods imageId`; `imageConfigured` for `object-fit` and friends |
| inline `<svg>` icon | `parseSvg` once at setup, then `svgIcon 16 svg` (drawn in the current text colour) |
| `<canvas>` | `canvas mods (\rect -> drawing commands)` from `NanoUI.Widgets.Custom` |
| `<table>` | `simpleTable headers rows`, or `table` with a Colonnade and `Var SortCol` for sorting |
| `<ul>`/`<ol>` + `<li>` | `column` + `for_ items (\i -> withKey (itemId i) (row ...))` |
| `<details>`/`<summary>` | a `Var Bool`, a button that flips it, and `scope (when open body)` |
| tree view | `tree` with `TreeItem`s |
| `<dialog>` / modal | `modal openVar "Title" body`; `window` for a draggable floating window |
| popover / dropdown panel | `popup openVar (defaultPopupConfig (AnchorRect (respRect resp))) body` |
| `title="..."` | `tooltip resp "text"` (needs the primed widget's `Response`) |
| `oncontextmenu` menu | `contextMenu resp (menuItem ...)`, or `contextMenuArea` for a region |
| menu bar | `menuButton'` titles opening `popup'` drop-downs of `menuItem`s (`examples/Notepad.hs`) |
| tab strip | `tabs var [tab k "Title" body, ...]`, `enumTabs`, `tabBar` for tabs without bodies |
| `<progress>` | `progressBar fraction` (0 to 1), `circularProgress`, `spinner` |
| `<meter>`, sparkline | `sparkline values`; charts in `nano-ui-diagrams` (`NanoUI.Plot`) |
| split panes | `paneGrid` (resizable, draggable panes) |
| `<iframe>`, `<video>`, `<audio>` | none |

## Layout properties

nano-ui layout is one flexbox-like model. Every container is a row or a
column; there is no block/inline distinction and no CSS grid areas.

| CSS | nano-ui |
| --- | --- |
| `display: flex` | `row` |
| `flex-direction: column` | `column` |
| `gap: 8px` | `gap 8` |
| `flex-wrap: wrap` | `rowWith (wrap . gap 6)` |
| `row-gap` between wrapped lines | `lineGap 4` |
| `align-content` for wrapped lines | `lineAlign LinesCenter` |
| `flex: 1` | `fillW` on a child of a row, `fillH` on a child of a column |
| `flex: 1` on both axes | `grow` |
| `justify-content: space-between` | `spacer (Grow 1) Fit` (or `flex`) between the children |
| `justify-content: flex-end` | a `spacer (Grow 1) Fit` first, or `alignEnd` on the single child of a column |
| `justify-content: center` | `center body` (both axes), or spacers on both sides |
| `align-items: center` in a row | `alignMid` on **each child**; the modifier positions the node it is on |
| `align-items: center` in a column | `alignCenter` on each child |
| `align-items: baseline` | `alignBaseline` on each child |
| `align-self` | the same alignment modifier on that one child |
| `align-items: stretch` | `fillH` (row) or `fillW` (column) on the children |
| `display: grid; grid-template-columns: repeat(3, 1fr)` | `gridWith 3 (gap 8 . fillW)`, children `fillW` |
| `repeat(auto-fill, minmax(160px, 1fr))` | `gridMinColW 160` on the grid |
| `width` / `height` | `fixedW` / `fixedH` / `fixedWH` |
| `min-width`, `max-width`, `min-height`, `max-height` | `minW`, `maxW`, `minH`, `maxH` |
| `width: 50%` | `percent 50` (width only, of the parent's width) |
| `height: 50%` | none: use `fillH` with siblings, or compute from `windowHeight` |
| `aspect-ratio: 16/9` | `aspect (16 / 9)` |
| `width: fit-content` | the default |
| `box-sizing: border-box` | the default: padding is inside a fixed size |
| `padding: 12px` | `padAll 12` |
| `padding: 4px 12px` (vertical horizontal) | `padXY 12 4` (**horizontal first**) |
| `padding: t r b l` | `padLRTB l r t b` (**left, right, top, bottom**) |
| `padding-left` etc. | `padLeft`, `padRight`, `padTop`, `padBottom` |
| `padding: 0` | `tight` (containers default to 3 px padding and an 8 px gap) |
| `margin` | none: use the parent's `gap` and padding, or a fixed spacer `spacer (Fixed 16) Fit` |
| `margin: 0 auto` | `alignCenter` on the child of a column |
| `margin-left: auto` | `spacer (Grow 1) Fit` before the child in a row |
| `position: relative` + `absolute` | `pinAt x y` on the child: its alignment picks the corner, then it moves `x` right and `y` down |
| `position: absolute; inset: 0` | `pinAt 0 0 . grow` |
| `position: fixed` | an overlay (`modal`, `window`, `popup`), or `pinAt` on a child of the root |
| `position: sticky` | none: put the sticky part outside the scroller |
| `z-index` | call order inside `layers` (later is on top); overlays are always above the page |
| `overflow: auto` | `scrollWith (fixedH 240 . fillW) body` (needs a bounded size), `scroll2D` for both axes |
| `overflow-x: hidden` + `text-overflow: ellipsis` | `labelWith fillW` (or `maxW`) inside a row: a single line cut with `...` |
| `white-space: normal` | a `label` in a column wraps to the column width |
| `overflow-wrap: anywhere` | `breakAnywhere` |
| `display: none` | do not call the widget; wrap the conditional part in `scope` |
| `visibility: hidden` | none; keep the slot with a `spacer` of the same size |
| `@media (max-width: 640px)` | `responsiveRowCol 640 mods body`, `responsive 640 wide narrow body`, or branch on `windowWidth` |
| `pointer-events: none` | `pointer PointerPass` |
| a click-eating backdrop | `pointer PointerBlock` on a `pinAt 0 0 . grow` panel |
| `cursor: pointer` etc. | `withCursorShape UiCursorPointer subtree` |
| `em`, `rem`, `vw`, `vh` | logical pixels only; `uiFontSize`, `windowWidth`, `windowHeight` to compute |

## Visual properties

Colour, borders and radii belong to the theme. Change them for a subtree with
`styled`, naming which surface you mean (buttons, inputs, panels, popups).

| CSS | nano-ui |
| --- | --- |
| CSS variables, design tokens | a `Palette` record and a `Theme` built from it |
| a class like `.btn-primary` | a `Theme -> Theme` function (`primary`, or your own) applied with `styled` |
| a reusable component class | a Haskell function returning `NanoUI a` |
| `background` on a panel | `styled (panelStyle (background c)) (panel body)` |
| `background` on buttons | `styled (buttonStyle (background c)) ...`; `everyStyle` for every surface |
| a plain coloured block | `box mods colour` |
| `color` | `labelWith (fontColor c)` per text, or `styled (textColor c)` for a subtree |
| `border`, `border-width`, `border-color` | `borderWidth 1 . borderColor c` inside a style modifier |
| `border-bottom` only | `borderBottom 1 c`, or `borderSides [..]` to pick which sides `borderWidth` draws |
| `border-radius` | `cornerRadius 6` in a style, or `rounded 6` for every surface in the theme |
| `:hover`, `:active` background | `hoverBackground c`, `pressBackground c` |
| `:hover` anything else | `respHovered` from the primed widget or `mouseArea` |
| `:focus` ring | automatic (`themeFocusRing`); `isFocused` to ask |
| `:disabled` | `disabledWhen` |
| `opacity` | `withAlpha` on colours; `icOpacity` for images |
| `font-size` | `fontSize 18` on the text widget; `sdlAppFontSize` for the base |
| `font-weight: bold` | `fontBold`, `fontWeight WeightSemiBold` |
| `font-style: italic` | `fontItalic` |
| `font-family` | `fontMono` per widget; `sdlAppFont`/`sdlAppMonoFont` for the app |
| `text-decoration` | `fontUnderline`, `fontStrike` |
| `text-align: center` (one line) | `labelWith alignCenter` in a column |
| `object-fit`, `object-position` | `imageConfigured defaultImageConfig {icFit = FitCover, ...}` |
| `box-shadow`, `linear-gradient` | no modifier; draw them in a `canvas` (paths take `P.Linear` gradients) |
| `prefers-color-scheme` | `followSystemTheme`, or `lightDark` with `systemAppearance` |
| `transition` | `animateTo (Tween EaseOutCubic 0.2 0) target` and use the returned value |
| `@keyframes` | `animate` or `pulse`, with `keepAnimating` in the widget |

Text style does not cascade: `columnWith (fontSize 20)` does not enlarge the
labels inside it. Put the modifier on each text widget, or wrap them in a
helper (`big = labelWith (fontSize 20)`). Themes do cascade: everything inside
`styled` sees the modified theme.

## Events and DOM APIs

| Web | nano-ui |
| --- | --- |
| `onclick` | `whenM (button "Go") go`, or `when (respClicked resp)` |
| `onclick` on a card or row | `(_, resp) <- mouseArea mods body`, then `respClicked resp` |
| `oninput` | the `Var` is already updated; read it below the field, or `respChanged` |
| `onchange` / `onsubmit` / Enter | `respSubmitted` from `textInput'` |
| `onmouseenter` / `onmouseleave` | `respHovered` on consecutive frames |
| `onmousedown`, middle/back buttons | `respPressed`, `respClickedWith MouseMiddle`, `mousePressed MouseBack` |
| `ondblclick` | on the press frame (`respPressed` and `mousePressed MouseLeft`), `inputMouseClicks <$> askInput` is 2 |
| `oncontextmenu` | `contextMenu resp body`, or `respRightClicked` |
| `onwheel` | scrollers handle it; `useWheelDeltaOn resp` in custom widgets |
| `keydown` on `document` | `shortcut (ctrl <> key 's')`, `keyPressed KeyEscape` |
| `keydown` on an element | a focused custom widget with `focusedKeyPressed` / `useKeyNav` |
| `element.focus()`, `autofocus` | `requestFocus (respId resp)` (no-op when already focused, so safe each frame) |
| `element.blur()` | `clearFocus` |
| `getBoundingClientRect()` | `respRect resp` (the previous frame's layout) |
| `scrollIntoView()` | `scrollIntoView` / `scrollRectIntoViewUi` |
| `IntersectionObserver` | `sensor` / `sensorConfigured` / `useVisibility` |
| `ResizeObserver` | `respRect` and `windowSize`, re-read each frame |
| HTML5 drag and drop | `useDrag`, `useReorder` + `insertionIndex`; `dropZone` for files from the OS |
| `setTimeout`, `setInterval` | `wakeAfter seconds` each frame, compare against `uiTime` |
| `requestAnimationFrame` | `keepAnimating resp` inside the animating widget |
| `fetch` | `useTask task key (IO action)` |
| WebSocket, EventSource | `useStream stream key initial producer` |
| `navigator.clipboard` | `getClipboard`, `setClipboard` |
| `window.open(url)` | SDL `openUrl url` (in `IO`) |
| `document.title` | `setWindowTitleUi` (cheap to call every frame) |
| `beforeunload` | `wsExitOnCloseRequest = False`, `winCloseRequested <$> askWindow`, then `quitUi` |
| `localStorage` | none: persist in `IO` yourself, e.g. on change or on close |
| `document.querySelector` | none: keep the `Response` or `WidgetId` you need |
| `window.innerWidth` | `windowWidth`, `windowSize` |

## React habits

| React | nano-ui |
| --- | --- |
| component | a function returning `NanoUI a` |
| props | function arguments |
| `useState` | `useVar` for `Int`/`Double`/`Float`/`Bool`/`Text`/`SortCol`, `useEnumVar` for enums; `newVar` at setup for anything else |
| lifting state up | allocate the `Var` higher (or at setup) and pass it down |
| controlled input (`value` + `onChange`) | pass the `Var`; or `constVar x` + the primed `Response` for derived values |
| `useReducer`, Redux | `nano-ui-emit`: `Model -> NanoUIE Msg ()` and `runSdlAppReduce` |
| `key` on list items | `withKey stableKey`, never the index |
| `{cond && <X />}` | `scope (when cond x)`; overlays are the exception and run every frame |
| `useEffect(() => fetch ..., [dep])` | `useTask task dep action` |
| `useEffect` cleanup | the task is cancelled when its key changes or its call stops; `bracket` inside |
| `useMemo` | compute outside the view, or cache in an `IORef` keyed by a revision; custom widgets take a content key |
| `useRef` to a DOM node | the widget's `Response` / `respId` |
| Context provider | function arguments or an app record; `styled`/`themed` for theme |
| portals | not needed: `modal`, `popup`, `window`, `tooltip` already draw above the page |
| `React.memo`, avoiding re-renders | not needed: the whole view runs each frame and damage tracking repaints only what changed. Keep the view cheap instead (no parsing, no big rebuilds) |
| CSS-in-JS | modifier functions in Haskell |

## Worked examples

### An app shell: header, sidebar, scrolling content

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

`separator` draws one rule at the seam rather than a border on each side.

### A list of cards with hover and click

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

`withKey` keeps each row's state with its message across inserts and
deletes. `labelWith fillW` lets a long title end in `...` instead of widening
the row.

### A form

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

`Login` is a record of `Var`s made at setup. The reads sit below the fields
that edit them.

### Badge over a button

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

The alignment picks the corner (`alignEnd . alignTop` is top-right) and `pinAt x y`
then moves the badge `x` right and `y` down from it. CSS `right: -6px` measures
inward, so it becomes `x = 6`.

### Details / summary

```haskell
details :: Text -> NanoUI () -> NanoUI ()
details title body = column $ do
  open <- useVar False
  isOpen <- readVar open
  whenM (button ((if isOpen then "v " else "> ") <> title)) (modifyVar open not)
  scope (when isOpen body)
```

### Links

```haskell
footer :: NanoUI ()
footer = do
  clicked <- richText ["Read the ", hyperlink "https://goolord.github.io/nano-ui/" "book", " first."]
  for_ clicked (liftIO . void . openUrl)
```

`openUrl` is in `NanoUI.Backend.Sdl`. Check the scheme of any URL that comes
from content you do not control before opening it.

### Responsive layout

```haskell
page :: NanoUI ()
page =
  responsiveRowCol 640 (fillW . tight . gap 12) $ do
    panelWith (fillW . padAll 12) filters
    panelWith (fillW . padAll 12) results
```

Below 640 logical pixels of window width the two panels stack; above it they
sit side by side.

## Things that do not carry over

- **Margins, `z-index`, `position: sticky`, CSS grid areas, percentage
  heights**: use gaps and padding, call order in `layers`, a header outside the
  scroller, nested rows and columns, and `fillH`.
- **Text style inheritance**: put font modifiers on each text widget.
- **Selectable text by default**: labels are not selectable; use
  `selectableText` or `selectableRichText` where copying matters.
- **Layout measurement during render**: `respRect` and scroll metrics are one
  frame old. Code that depends on its own size reads the previous frame.
- **Unmounting**: content you stop calling disappears but keeps its widget
  state (caret, `useVar` values) for when it returns; tasks stop after one
  skipped frame.
- **Shadows and gradients as properties**: draw them in a `canvas`.
