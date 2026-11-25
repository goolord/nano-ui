-- | Code for the book's Text Editing chapter
-- (@book/src/widgets/text-editing.md@).
module Book.TextEditing where

import Control.Monad (when)
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import NanoUI
import NanoUI.Widgets.TextDocument

notes :: NanoUI ()

-- ANCHOR: text_area
notes = do
  draft <- useVar ""
  textArea draft
-- ANCHOR_END: text_area

-- ANCHOR: document
editorView :: Var WidgetId -> Var TextDocument -> NanoUI ()
editorView editorId docVar = do
  resp <- textAreaDocumentWith' (grow . minW 320 . minH 200) docVar
  when (respChanged resp) markDirty
  writeVar editorId (respId resp)
-- ANCHOR_END: document

-- ANCHOR: save
save :: FilePath -> TextDocument -> NanoUI ()
save path doc = liftIO (BS.writeFile path (TE.encodeUtf8 (documentText doc)))
-- ANCHOR_END: save

-- ANCHOR: commands
editMenu :: WidgetId -> Text -> NanoUI ()
editMenu editor today = do
  whenM (menuItem "Undo") (runTextCommand editor Undo)
  whenM (menuItem "Insert date") (runTextCommand editor (InsertText today))
-- ANCHOR_END: commands

-- ANCHOR: history
undoState :: Var WidgetId -> NanoUI (Bool, Bool)
undoState editorId = do
  editor <- readVar editorId
  canUndo <- textCanUndo editor
  canRedo <- textCanRedo editor
  pure (canUndo, canRedo)
-- ANCHOR_END: history

-- ANCHOR: rich_text
releaseNote :: NanoUI ()
releaseNote = do
  clicked <-
    selectableRichText
      [ "Build "
      , strong "v0.2"
      , " with "
      , inlineTooltip "Uses -j1 to keep memory modest" (inlineCode "cabal build -j1 all")
      , " or read the "
      , hyperlink "https://goolord.github.io/nano-ui/book/" "documentation"
      , "."
      ]
  for_ clicked openLink
-- ANCHOR_END: rich_text

-- Stand-ins for application code the snippets refer to.

markDirty :: NanoUI ()
markDirty = pure ()

openLink :: Text -> NanoUI ()
openLink _ = pure ()
