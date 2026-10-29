# Buttons, Menus & Inputs

## Buttons

```haskell
button   :: Text -> NanoUI Bool
button'  :: Text -> NanoUI Response
buttonWith :: (Layout -> Layout) -> Text -> NanoUI Bool
buttonContent :: NanoUI () -> NanoUI Bool
iconButton :: Svg -> Text -> NanoUI Bool
```

`button` returns `True` only on the frame it is activated, by left click or by Enter/Space while focused:

```haskell
whenM (button "Save Document") (liftIO saveFile)
```

Use the primed form when you need the response: hover state, a rectangle for an anchored popup, or a tooltip.

```haskell
resp <- button' "Refresh"
tooltip resp "Poll the remote server for updates"
when (respClicked resp) refresh
```

`buttonContent` puts any view inside a button. The content is for display; a click anywhere on the button activates it:

```haskell
whenM (buttonContent $ rowWith (gap 6 . alignMid) $ do
          svgIcon 16 saveIcon
          label "Save") $
  saveDocument
```

`iconButton` is the leading-icon case; an empty label gives an icon-only square button.

## Menus

`menuItem` is a row that returns `True` when clicked. `menuItemShortcut` also displays a chord and binds it while the menu is open. `menuSeparator` and `menuItemDisabled` complete the row set.

A menu bar is a `menuButton'` plus a `popup'` anchored to its rectangle:

```haskell
fileOpen <- useVar False
isOpen <- readVar fileOpen
btn <- menuButton' "File" isOpen
when (respClicked btn) (writeVar fileOpen (not isOpen))

let cfg = (defaultPopupConfig (AnchorRect (respRect btn)))
            {cfgPlacement = PlacementBelow, cfgOffset = 0}
(popupResp, _) <- popup' (constVar isOpen) cfg $
  columnWith (tight . gap 0) $ do
    whenM (menuItemShortcut "New" (cmdOrCtrl <> key 'n')) newDocument
    whenM (menuItem "Open...") openDocument
    menuSeparator
    whenM (menuItem "Exit") (writeVar fileOpen False >> quitUi)
when (respClicked popupResp) (writeVar fileOpen False)
```

`constVar` supplies a read-only open flag: the truth is the menu's own state, and a click on the popup is written back to the real `Var`.

> [!IMPORTANT]
> Call overlays unconditionally. `popup'` draws nothing while closed, but wrapping it in `when isOpen` changes later widget ids each time the menu opens.

Bind a command chord **outside** the menu as well as on its row, so it works with the menu closed:

```haskell
whenM (menuItemShortcut "Save" saveChord) saveDocument
whenM (shortcut saveChord) saveDocument
```

Only the first binding of a chord in a frame fires, so the open menu's row wins. A complete menu bar is built in [Build a Notepad](../tutorials/notepad.md).

`contextMenu` attaches a right-click menu to any widget with a `Response`:

```haskell
(_, rowResp) <- mouseArea (fillW . gap 6) (label "Right-click for actions")
_ <- contextMenu rowResp $ do
  whenM (menuItem "Inspect") inspectRow
  whenM (menuItem "Copy") copyRow
```

## Input widgets

Input widgets take the `Var` they display and edit it, and manage caret, selection, focus, and keys themselves. To reset a field, write the `Var`; to react to an edit, use the primed form's `Response`.

| Widget | Signature | Description |
| --- | --- | --- |
| `checkbox` | `Text -> Var Bool -> NanoUI ()` | Two-state box with a label |
| `toggleButton` | `Tone -> Text -> Var Bool -> NanoUI ()` | Two-state button, filled in the tone while on |
| `radio` | `Foldable f => f Text -> Var Int -> NanoUI ()` | Radio group over indexed options |
| `boundedRadio` | `(Bounded a, Enum a) => (a -> Text) -> Var a -> NanoUI ()` | Radio group over every enum value |
| `select` | `Foldable f => f Text -> Var Int -> NanoUI ()` | Drop-down select by index |
| `boundedSelect` | `(Bounded a, Enum a) => (a -> Text) -> Var a -> NanoUI ()` | Drop-down over every enum value |
| `comboBox` | `Foldable f => Text -> f Text -> Var Text -> NanoUI ()` | Placeholder, suggestions, free-text value |
| `slider` | `Float -> Float -> Var Float -> NanoUI ()` | Min, max, value |
| `textInput` | `Var Text -> NanoUI ()` | Single-line editable text |
| `searchInput` | `Text -> Var Text -> NanoUI ()` | Placeholder + search field with a clear button |
| `numericInput` | `Var Double -> NanoUI ()` | Numeric field with steppers |
| `colorPicker` | `Var Color -> NanoUI ()` | Saturation/value field and hue bar |

Every widget has the usual variants: primed (`...'`) returns a `Response`, `...With` takes a layout modifier, and `...Configured` takes a config record. For example:

```haskell
numericInputConfigured defaultNumericInputConfig
  {nicMin = 0, nicMax = 255, nicHex = True} byte
```

### Text and search fields

```haskell
query <- useVar ""
resp <- textInput' query
when (respSubmitted resp) (search =<< readVar query)
when (respChanged resp) (preview =<< readVar query)
```

`respChanged` fires on every edit; `respSubmitted` fires on Enter. For expensive work use `searchInput'`, whose `respChanged` is debounced (300 ms by default) and fires immediately when the field is cleared:

```haskell
resp <- searchInput' "Find a package" query
when (respChanged resp) (refreshResults =<< readVar query)
```

### Choices

```haskell
enabled <- useVar True
checkbox "Enable notifications" enabled

bold <- useVar False
toggleButton Accent "B" bold

data Pace = Walk | Jog | Sprint
  deriving (Eq, Show, Enum, Bounded)

pace <- useEnumVar Jog
enumRadio pace
enumSelect pace
```

`select` is index-based for data-driven options:

```haskell
choice <- useVar 0
select ["Small", "Medium", "Large"] choice
```

`comboBox` takes free text plus suggestions; its `respChanged` fires on commit, not on every model write.

### Sliders, numbers, color

```haskell
volume <- useVar (40 :: Float)
slider 0 100 volume

count <- useVar (0 :: Double)
numericInput count

accent <- newVar (colorHex "#38bdf8")
colorPicker accent
```

`numericInput` accepts digits and a leading minus when the range allows it; arrow keys step, and `Shift` steps ten times as far. `nicDecimals` controls display and accepted precision. `colorPickerRGBA` adds an alpha bar.
