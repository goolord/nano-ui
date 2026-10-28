# Buttons & Menus

Buttons are immediate-mode widgets that return `Bool` indicating activation, or a primed `Response` record.

## Signatures

```haskell
button        :: Text -> NanoUI Bool
button'       :: Text -> NanoUI Response
buttonWith    :: (Layout -> Layout) -> Text -> NanoUI Bool
buttonContent :: NanoUI () -> NanoUI Bool
iconButton    :: SvgIcon -> NanoUI Bool
menuButton    :: Text -> NanoUI () -> NanoUI ()
menuItem      :: Text -> NanoUI Bool
menuSeparator :: NanoUI ()
contextMenu   :: Response -> NanoUI () -> NanoUI ()
```

## Push Buttons

A button returns `True` only in the single frame it was activated (via left mouse click or keyboard Enter/Space when focused):

```haskell
whenM (button "Save Document") $ do
  liftIO saveFile
```

### Custom Content with `buttonContent`
To embed icons, rich layout, or bold text inside a button:

```haskell
whenM (buttonContent $ rowWith (gap 6 . alignMid) $ do
        svgIcon saveIcon
        labelWith (fontBold . fontTone Danger) "Delete") $ do
  confirmDelete
```

---

## Dropdown Menus & Context Menus

`menuButton` displays a trigger that opens a dropdown popup menu:

```haskell
menuButton "File" $ do
  whenM (menuItem "New File") createNew
  whenM (menuItem "Open...") openFile
  menuSeparator
  whenM (menuItem "Exit") quitUi
```

Attach a right-click context menu to any widget using its `Response`:

```haskell
resp <- card' $ label "Right-click me for actions"
contextMenu resp $ do
  whenM (menuItem "Inspect") inspectCard
  whenM (menuItem "Copy") copyCard
```
