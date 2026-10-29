-- | Code for the book's Keyboard, Focus & IME chapter
-- (@book/src/input/index.md@).
module Book.Input where

import Control.Monad (when)
import Data.Foldable (for_)
import Data.IORef (newIORef)
import Data.Text (Text)
import NanoUI
import NanoUI.Testing (newContext)
import NanoUI.Testing.Assert (withInput)
import NanoUI.Testing.Sim (expect, pressKey, runSim, typeText)
import NanoUI.Testing.Sim qualified as Sim
-- ANCHOR: import
import NanoUI.Shortcut

saveChord :: Shortcut
saveChord = cmdOrCtrl <> key 's'

saveCommand :: NanoUI ()
saveCommand = whenM (shortcut saveChord) saveDocument
-- ANCHOR_END: import

-- ANCHOR: chords
chords :: [Shortcut]
chords =
  [ ctrl <> key 's'                 -- Ctrl+S
  , cmdOrCtrl <> shift <> key 'p'   -- Cmd+Shift+P on macOS, Ctrl+Shift+P elsewhere
  , alt <> key KeyEnter             -- Alt+Enter
  , key (KeyF 5)                    -- F5
  ]
-- ANCHOR_END: chords

-- ANCHOR: menu_and_shortcut
saveBindings :: NanoUI ()
saveBindings = do
  whenM (menuItemShortcut "Save" saveChord) saveDocument
  whenM (shortcut saveChord) saveDocument
-- ANCHOR_END: menu_and_shortcut

-- ANCHOR: take_escape
escapes :: Bool -> Var Bool -> NanoUI ()
escapes isEditing editing = do
  when isEditing (whenM takeEscape (writeVar editing False))
  whenM takeEscape clearFocus
-- ANCHOR_END: take_escape

-- ANCHOR: parse_shortcut
keymapEntry :: Text -> NanoUI ()
keymapEntry txt =
  case parseShortcut txt of -- "C-S-s"
    Right chord -> label (shortcutLabel chord)
    Left err    -> showError err
-- ANCHOR_END: parse_shortcut

-- ANCHOR: request_focus
findField :: Var Text -> NanoUI ()
findField query = do
  resp <- searchInput' "Find" query
  whenM (shortcut (cmdOrCtrl <> key 'f')) (requestFocus (respId resp))
  -- next frame: the focus ring is drawn and isFocused answers True
-- ANCHOR_END: request_focus

-- ANCHOR: delayed_focus
editableName :: Var Text -> NanoUI ()
editableName draft = do
  editing <- useVar False
  focusPending <- useVar False

  showField <- readVar editing
  scope $
    if showField
      then do
        fieldR <- textInput' draft
        whenM (readVar focusPending) $ do
          requestFocus (respId fieldR)
          writeVar focusPending False
      else label "Not editing"

  whenM (button "Edit") $ do
    writeVar editing True
    writeVar focusPending True
-- ANCHOR_END: delayed_focus

-- ANCHOR: input_method
-- Every frame the widget accepts text:
composition :: WidgetId -> Rect -> NanoUI ()
composition wid caretRect = do
  preedit <- useInputMethod wid InputNormal caretRect
  for_ preedit drawCompositionAtCaret
-- ANCHOR_END: input_method

-- ANCHOR: sim
renameTest :: IO ()
renameTest = do
  ctx <- newContext
  failed <- newIORef 0
  runSim ctx failed (withInput 400 300) editorView $ do
    Sim.click "Rename"
    typeText "notes.txt"
    pressKey KeyEnter
    expect "notes.txt"
-- ANCHOR_END: sim

-- Stand-ins for application code the snippets refer to.

saveDocument :: NanoUI ()
saveDocument = pure ()

showError :: Text -> NanoUI ()
showError = danger

drawCompositionAtCaret :: Composition -> NanoUI ()
drawCompositionAtCaret _ = pure ()

editorView :: NanoUI ()
editorView = do
  name <- useVar ""
  whenM (button "Rename") (pure ())
  textInput name
