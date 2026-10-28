# Buttons & Menus

Buttons are immediate-mode widgets: they draw themselves and return whether they were activated this frame, or a primed `Response` with the details.

## Signatures

```haskell
button         :: Text -> NanoUI Bool
button'        :: Text -> NanoUI Response
buttonWith     :: (Layout -> Layout) -> Text -> NanoUI Bool
buttonWith'    :: (Layout -> Layout) -> Text -> NanoUI Response
buttonConfigured  :: ButtonConfig -> Text -> NanoUI Bool
buttonConfigured' :: ButtonConfig -> Text -> NanoUI Response
buttonContent  :: NanoUI () -> NanoUI Bool
iconButton     :: Svg -> Text -> NanoUI Bool
menuButton     :: Text -> Bool -> NanoUI Bool
menuButton'    :: Text -> Bool -> NanoUI Response
menuItem       :: Text -> NanoUI Bool
menuItemShortcut :: Text -> Shortcut -> NanoUI Bool
menuItemDisabled :: Text -> NanoUI ()
menuSeparator  :: NanoUI ()
contextMenu    :: HasResponse r => r -> NanoUI a -> NanoUI (Maybe a)
```

## Push buttons

`button` returns `True` only on the frame it is activated, by left click or by Enter/Space while focused:

```haskell
whenM (button "Save Document") (liftIO saveFile)
```

The `whenM` guard is essential: the view runs every frame, so an unguarded `saveFile` would run forever. The same applies to every event in this chapter.

Use the primed form when you need the response — hover state, a rectangle for an anchored popup, or a tooltip:

```haskell
resp <- button' "Refresh"
tooltip resp "Poll the remote server for updates"
when (respClicked resp) refresh
```

### Custom content

`buttonContent` lets the button contain any view. The content is laid out like a label and takes the button's text color; a click anywhere on the button activates it:

```haskell
whenM (buttonContent $ rowWith (gap 6 . alignMid) $ do
          svgIcon 16 saveIcon
          label "Save") $
  saveDocument
```

`iconButton` is the common case of a leading icon, with an optional label:

```haskell
whenM (iconButton saveIcon "Save") saveDocument
whenM (iconButton gearIcon "") openSettings   -- icon alone, square button
```

## Menu buttons and popups

`menuButton` draws a menu-bar title. Its second argument is whether the menu is open, and it returns `True` when the title is clicked:

```haskell
fileOpen <- useVar False
btn <- menuButton' "File" =<< readVar fileOpen
when (respClicked btn) (modifyVar fileOpen not)
```

`menuButton` does not draw the menu itself. A menu is a `popup'` anchored to the button's rectangle, and its contents are ordinary `NanoUI`:

```haskell
fileMenu :: Var Bool -> Var Text -> NanoUI ()
fileMenu fileOpen docPath = do
  isOpen <- readVar fileOpen
  btn <- menuButton' "File" isOpen
  when (respClicked btn) (writeVar fileOpen (not isOpen))

  let cfg = (defaultPopupConfig (AnchorRect (respRect btn)))
              {cfgPlacement = PlacementBelow, cfgOffset = 0}
  -- constVar: the truth is `isOpen`, so writes to the popup's Var are no-ops.
  (popupResp, _) <- popup' (constVar isOpen) cfg $
    columnWith (tight . gap 0) $ do
      whenM (menuItemShortcut "New" (cmdOrCtrl <> key 'n')) $ do
        writeVar docPath ""
        writeVar fileOpen False
      whenM (menuItemShortcut "Open..." (cmdOrCtrl <> key 'o')) $ do
        startOpenDialog          -- application action: askOpenFileDialog, etc.
        writeVar fileOpen False
      menuSeparator
      whenM (menuItem "Exit") (writeVar fileOpen False >> quitUi)

  when (respClicked popupResp) (writeVar fileOpen False)
```

Why `constVar`? `popup'` requires a `Var Bool` for open state, but the real state is "this menu's label is the open one". `constVar isOpen` supplies a read-only `Var`; a click on the popup is reported through `popupResp`, and the code writes `fileOpen` itself.

`prompt` for menu rows:

- `menuItem` returns `True` when clicked.
- `menuItemShortcut` shows the chord label and binds the chord **while the menu is open**. Bind the same chord with `shortcut` outside the menu for when it is closed; the menu row declared first wins while open.
- `menuItemDisabled` draws a dimmed row that cannot be clicked.
- `menuSeparator` draws a divider.

A complete menu bar with File/Edit/View/Help menus, commands, and a status bar is built in [Build a Notepad](../tutorials/notepad.md).

> [!IMPORTANT]
> Call `popup'` (and every overlay) unconditionally. It draws nothing while closed; wrapping it in `when isOpen` changes later widget ids each time the menu opens.

## Context menus

`contextMenu` attaches a right-click menu to any widget with a `Response`:

```haskell
(_, cardResp) <- mouseArea (fillW . gap 6) $
  label "Right-click for actions"
_ <- contextMenu cardResp $ do
  whenM (menuItem "Inspect") inspectCard
  whenM (menuItem "Copy") copyCard
```

It returns the menu body's result while the menu is open (`Maybe a`). The menu closes when a row is clicked or the user clicks outside. For a whole region, `contextMenuArea` takes the layout, the area content, and a function from the click position to the menu body.

## Styling buttons

Buttons use `themeButton` for their background, border, and text. Scope a change rather than editing the theme globally:

```haskell
whenM (styled destructive (button "Delete")) deleteItem
whenM (styled (primary . rounded 999) (button "Continue")) next
```

The `buttonStyle`, `inputStyle`, and `panelStyle` scopes apply to every button in a subtree; `background`, `foreground`, `borderColor`, and the rest adjust one surface. See [Theming & Surfaces](../styling/overview.md) and the [workbenches](../styling/widget-surfaces.md).
