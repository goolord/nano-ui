# Tutorial 2: Build a Notepad

A menu bar, file dialogs, a text document with undo, a status bar, and a prompt before losing unsaved work. It covers menus, dialogs, and close confirmation.

The complete program is [`packages/nano-ui-demo/examples/Notepad.hs`](https://github.com/goolord/nano-ui/blob/main/packages/nano-ui-demo/examples/Notepad.hs). Run it with:

```sh
cabal run nano-ui-sdl-notepad
```

## 1. State that belongs to the session

A text document is not a type `useVar` can hold, and the editor's identity must outlive any single frame, so both live in a record allocated once:

```haskell
data NotepadState = NotepadState
  !(Var TextDocument)   -- the document
  !(Var WidgetId)       -- the editor's widget id, for menu commands
  !(Var (Maybe FileDialogId))  -- the open dialog
  !(Var (Maybe FileDialogId))  -- the save dialog

newNotepad :: IO NotepadState
newNotepad =
  NotepadState
    <$> newVar emptyDocument
    <*> newVar (WidgetId 0)
    <*> newVar Nothing
    <*> newVar Nothing
```

The menu state, path, dirty flag, zoom, and transient messages are simpler and can be `useVar`s inside the view. The rule of thumb: allocate in `IO` what other code needs a handle to (a document, a task, a dialog), and use `useVar` for view-local state.

## 2. Close confirmation

By default the window closes as soon as the close button is pressed. A notepad needs to intercept that and ask about unsaved changes, so the window is created with `wsExitOnCloseRequest = False`:

```haskell
main :: IO ()
main = do
  state <- newNotepad
  runSdlApp
    defaultSdlOptions
      { sdlWindowSettings =
          defaultWindowSettings
            { wsTitle = "nano-ui Notepad"
            , wsSize = Size 1000 720
            , wsExitOnCloseRequest = False
            }
      , sdlAppTheme = Just tomorrowNightMinDarkTheme
      }
    (notepadUi state)
```

With exit-on-close off, `winCloseRequested <$> askWindow` reports the request inside the view, where it can open a confirmation modal instead of quitting:

```haskell
whenM (winCloseRequested <$> askWindow) exitApp
```

`exitApp` quits immediately when the document is clean, and opens a modal when it is not. The full wiring is in section 7.

## 3. A menu bar from primitives

There is no opaque `menuBar` widget. A menu bar is a row of `menuButton'`s, each paired with a `popup'` anchored to the button's rectangle. That is a feature: the menu's contents are ordinary `NanoUI ()`.

The helper below mirrors the one in the notepad source. `openMenu` holds the label of the open menu (empty when all are closed):

```haskell
menuBar :: Var Text -> [(Text, NanoUI ())] -> NanoUI ()
menuBar openMenu entries = do
  current <- readVar openMenu
  rowWith (tight . fillW . fixedH 28) $ do
    for_ entries $ \(menuLabel, body) -> do
      let isOpen = current == menuLabel
      btn <- menuButtonWith' fillH menuLabel isOpen
      when (respClicked btn) $
        writeVar openMenu (if isOpen then "" else menuLabel)
      -- Hovering another title while a menu is open switches to it.
      when (not isOpen && not (T.null current) && respHovered btn) $
        writeVar openMenu menuLabel
      let cfg = (defaultPopupConfig (AnchorRect (respRect btn)))
                  {cfgPlacement = PlacementBelow, cfgOffset = 0}
      -- The open flag is derived from the open menu's name, so the popup
      -- gets a const Var and closing is written back here.
      (popupResp, _) <- popup' (constVar isOpen) cfg (columnWith (tight . gap 0) body)
      when (respClicked popupResp) (writeVar openMenu "")
```

`popup'` needs a `Var Bool` for whether it is open, but the state is derived from `openMenu` ("is this label the open one?"). `constVar isOpen` supplies a read-only `Var`; writes to it are no-ops. When the popup reports a click, the code writes the real state (`openMenu`) instead.

> [!IMPORTANT]
> Call overlays unconditionally. `popup'` is called for every title on every frame, even when closed; it draws nothing while closed. Wrapping it in `when isOpen` would change the widget ids of later siblings each time a menu opens.

## 4. Commands, menus, and shortcuts

Each menu row is a `menuItem` / `menuItemShortcut`. The row reports activation, and the action runs in the same frame:

```haskell
item menuRow action = whenM menuRow (writeVar openMenu "" >> action)

fileMenu = do
  item (menuItemShortcut "New" (cmdOrCtrl <> key 'n')) newDocument
  item (menuItemShortcut "Open..." (cmdOrCtrl <> key 'o')) openDocument
  item (menuItemShortcut "Save" (cmdOrCtrl <> key 's')) (saveDocument False)
  item (menuItemShortcut "Save As..." (cmdOrCtrl <> shift <> key 's')) (saveDocument True)
  menuSeparator
  item (menuItemShortcut "Exit" (cmdOrCtrl <> key 'q')) exitApp
```

(`newDocument`, `openDocument`, `saveDocument`, and `exitApp` are ordinary `NanoUI ()` actions defined in the full source; `openDocument` starts a non-blocking `askOpenFileDialog`.)

`menuItemShortcut` shows the chord label and binds it **while the menu is open**. To make the command work with the menu closed, bind the same chord with `shortcut` outside the menu:

```haskell
for_ [newCmd, openCmd, saveCmd, saveAsCmd, exitCmd] $
  \(_, chord, action) -> item (shortcut chord) action
```

Only the first binding of a chord in a frame fires, so the menu row and the view binding never both run it. The menu row is declared first, so while the menu is open its row wins.

Two more details from the source:

- `menuItemDisabled "Undo"` draws a dimmed row that cannot be clicked. The Edit menu chooses between a real item and a disabled one by checking `textCanUndo` / `textCanRedo`.
- An action that opens a file dialog writes the new `FileDialogId` into a `Var`; dialogs are non-blocking, so the view is not interrupted.

## 5. Opening and saving files

The SDL backend's file dialogs are asynchronous. `useFileDialog` polls a `Var (Maybe FileDialogId)`, consumes the finished result once, and restores focus afterwards:

```haskell
useFileDialog openDlg $ \chosenPaths ->
  for_ (listToMaybe chosenPaths) $ \filePath -> do
    loaded <- liftIO (try (BS.readFile filePath) :: IO (Either SomeException BS.ByteString))
    case loaded of
      Left _ -> writeVar statusMsg ("Could not open " <> T.pack filePath)
      Right raw -> do
        writeVar doc (textDocument (TE.decodeUtf8With (\_ _ -> Just '\xFFFD') raw))
        modifyVar docGen (+ 1)          -- a new editor, with no old caret or undo
        writeVar docPath (T.pack filePath)
        writeVar docDirty False
        writeVar statusMsg ("Opened " <> T.pack filePath)

useFileDialog saveDlg $ \chosenPaths ->
  for_ (listToMaybe chosenPaths) $ \filePath ->
    writeDocument filePath =<< readVar doc
```

Starting a dialog returns an id:

```haskell
writeVar openDlg . Just =<< askOpenFileDialog defaultFileDialogOptions
writeVar saveDlg . Just =<< askSaveFileDialog defaultFileDialogOptions
```

`writeDocument` is ordinary Haskell around `liftIO`, because file I/O is an event-driven side effect:

```haskell
writeDocument :: FilePath -> TextDocument -> NanoUI Bool
writeDocument filePath doc = do
  result <- liftIO (try (BS.writeFile filePath (TE.encodeUtf8 (documentText doc)))
                        :: IO (Either SomeException ()))
  pure (isRight result)
```

`documentText` is the only conversion from the document to a `Text`; editing works on the document directly and never rebuilds the whole string per keystroke.

## 6. The editor

`textAreaDocument` edits a `Var TextDocument` with line-sharing and an undo history. The primed variant supplies the `Response` the application needs:

```haskell
editorResp <-
  withKey gen $
    textAreaDocumentWith'
      (grow . minW 240 . minH 160 . fontSizeScale zoomVal)
      doc
when (respChanged editorResp) (writeVar docDirty True)
writeVar editorId (respId editorResp)
```

- **`withKey gen`** changes the editor's key when the document is replaced. A new document gets a fresh caret, selection, and undo stack. Merely editing does not change `gen`.
- **`respChanged`** reports an edit, not a caret move. Marking dirty on caret movement would make a read-only inspection look like a modification.
- **`respId`** is saved for menu commands. The menus can later call `runTextCommand editor Undo` and friends.

`fontSizeScale zoomVal` zooms the text without touching the document. The zoom commands are ordinary `modifyVar` calls clamped to a sane range.

## 7. Dirty state, title, and the close prompt

The title is derived every frame from the path and the dirty flag, and set declaratively:

```haskell
path <- readVar docPath
dirty <- readVar docDirty
let name = (if T.null path then "Untitled" else path) <> (if dirty then " *" else "")
setWindowTitleUi (name <> " - nano-ui Notepad")
```

`setWindowTitleUi` is a no-op when the title has not changed, so it is safe to call unconditionally. The imperative window actions (`resizeWindowUi`, `maximizeWindowUi`) are different: they run on every call and belong behind events.

The close request and the modal fit together like this:

```haskell
exitApp = do
  dirty <- readVar docDirty
  if dirty
    then writeVar aboutOpen False >> writeVar confirmExit True
    else quitUi

-- ...inside the view, called unconditionally:
modal confirmExit "Unsaved changes" $ do
  label "Discard the changes to this document and exit?"
  rowWith fillW $ do
    flex
    whenM (button "Discard") quitUi
    whenM (button "Cancel") (writeVar confirmExit False)
```

There is one modal at a time: opening the exit prompt closes the About modal first. Overlay state is a `Var Bool`, and the modal call itself is unconditional.

## 8. Undo, redo, and the rest of the editing commands

The document keeps one undo history shared by typing, context-menu actions, and programmatic commands. The Edit menu reads the history state and runs commands by widget id:

```haskell
editMenu = do
  editor <- readVar editorId
  canUndo <- textCanUndo editor
  canRedo <- textCanRedo editor
  if canUndo
    then item (menuItemShortcut "Undo" (cmdOrCtrl <> key 'z')) (runTextCommand editor Undo)
    else menuItemDisabled "Undo"
  if canRedo
    then item (menuItemShortcut "Redo" (cmdOrCtrl <> shift <> key 'z')) (runTextCommand editor Redo)
    else menuItemDisabled "Redo"
  menuSeparator
  item (menuItemShortcut "Cut" (cmdOrCtrl <> key 'x')) (runTextCommand editor Cut)
  item (menuItemShortcut "Copy" (cmdOrCtrl <> key 'c')) (runTextCommand editor Copy)
  item (menuItemShortcut "Paste" (cmdOrCtrl <> key 'v')) (runTextCommand editor Paste)
  menuSeparator
  item (menuItemShortcut "Select All" (cmdOrCtrl <> key 'a')) (runTextCommand editor SelectAll)
```

`TextCommand` covers `InsertText`, `Delete`, `Move`, `Select`, `Replace`, `ReplaceAll`, `Undo`, `Redo`, `Cut`, `Copy`, `Paste`, and `SelectAll`. Using commands rather than writing the `Var` keeps undo intact; assigning a freshly computed `TextDocument` to `doc` clears the history.

> [!TIP]
> A focused text editor claims its own editing chords (Ctrl+Z, Ctrl+X, and so on) before the application's shortcuts see them. The menu items still work because they are explicit commands, and the top-level bindings are a fallback for when focus is elsewhere.

## 9. The status bar

The status bar is a row of muted labels. Line count comes from the document without materializing its text; word and character counts are computed on demand from `documentText`:

```haskell
statusBar name doc statusMsg zoomVal = do
  d <- readVar doc
  message <- readVar statusMsg
  rowWith (tight . gap 12 . fillW . padXY 8 4) $ do
    labelWith (tight . fontMuted) message
    flex
    labelWith (tight . fontMuted) name
    labelWith (tight . fontMuted) ("Lines: " <> T.pack (show (documentLineCount d)))
    labelWith (tight . fontMuted) ("Zoom: " <> T.pack (show (round (zoomVal * 100) :: Int)) <> "%")
```

Reading `doc` here records a dependency: every edit repaints the status bar. That is intentional, because the line count changes. If the label's text length changes often, pin its width with `fixedW` so the rest of the bar does not shift (see [Recurring Bugs](../troubleshooting/recurring-bugs.md)).

## 10. Try it

- Add a recent-files list persisted between runs.
- Replace the About modal with a floating `window` that can be moved and resized.
- Add word wrap using the editor's layout options.
- Build a custom title bar with `captionButtons` and `dragSpans` (see [Windows & Screenshots](../advanced/window-management.md)).

Next: [Tutorial 3: Live Data & Background Work](live-data.md) moves slow work off the frame and streams results back.
