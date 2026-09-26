-- | Implementation of "NanoUI.Widgets.TextArea", plus loading and saving a
-- text area's state in the widget store and running commands on it from
-- outside its frame.
module NanoUI.Internal.Widgets.TextArea
  ( -- * Pure state
    TextAreaState (..)
  , initTextAreaState
  , setTextAreaViewport
  , setTextAreaSelection
    -- * Widget
  , textArea
  , textArea'
  , textAreaWith
  , textAreaWith'
  , textAreaDocument
  , textAreaDocument'
  , textAreaDocumentWith
  , textAreaDocumentWith'
  , textAreaLayout
  , textAreaBuffer
  , loadTextAreaState
  , saveTextAreaState
  , runTextAreaCommand
  , textAreaFieldEditor
  ) where

import Control.Monad (foldM, unless, when)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.IntMap.Strict as IM
import Effectful (Eff, type (:>))
import NanoUI.Internal.Context hiding (scrollOffset)
import NanoUI.Internal.Font (fmLineHeight)
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Internal.Input
import NanoUI.Internal.Layout.Arena (NodeType (..))
import NanoUI.Internal.Monad (Ui, askContext, askInput, freshWidget, nextId, uiIO)
import NanoUI.Internal.Store
import NanoUI.Internal.Style (FontStyle (..), FontVariant (..), FontWeight (..), Layout (..), defaultLayout, fillW, fixedH, minW)
import NanoUI.Internal.Types (DamageBounds (..), clamp)
import NanoUI.Internal.Widgets.Behavior (keyboardFocused)
import NanoUI.Internal.Widgets.Node (Response, addWidget, setChanged)
import NanoUI.Internal.Widgets.TextInput (fieldTextCommands)
import qualified NanoUI.Widgets.TextBuffer as TB
import NanoUI.Internal.Widgets.TextDocument
import NanoUI.Widgets.TextEditor

-- | Editor buffer, selection anchor, viewport, scroll offsets, and undo history.
-- Positions use zero-based character rows/columns; viewport and scrolling use
-- logical pixels. The widget stores this under its stable id.
data TextAreaState = TextAreaState
  { buffer :: !TB.TextBuffer
  , selectionAnchor :: !TB.Cursor
  , scrollOffset :: !(Double, Double)
  , viewportSize :: !(Double, Double)
  , lineHeight :: !Double
  , history :: !EditHistory
  }
  deriving (Show)

-- | Start at the document origin with no selection or history and an unset viewport.
initTextAreaState :: T.Text -> TextAreaState
initTextAreaState initial =
  TextAreaState
    { buffer = TB.fromText initial
    , selectionAnchor = TB.Cursor 0 0
    , scrollOffset = (0.0, 0.0)
    , viewportSize = (0.0, 0.0)
    , lineHeight = 16.0
    , history = emptyHistory
    }

-- | Set logical viewport width/height and line height without moving the caret.
setTextAreaViewport :: (Double, Double) -> Double -> TextAreaState -> TextAreaState
setTextAreaViewport vp lh state =
  state {viewportSize = vp, lineHeight = lh}

-- | Set anchor and cursor, in that order, and scroll vertically to reveal the caret.
setTextAreaSelection :: TB.Cursor -> TB.Cursor -> TextAreaState -> TextAreaState
setTextAreaSelection anchor cursor state =
  let buf =
        let b = TB.withCursor cursor (buffer state)
         in b {TB.preferredCol = TB.cursorCol cursor}
   in ensureCaretVisible state {buffer = buf, selectionAnchor = anchor}

-- | Extract buffer, selection, and history for the shared text-command engine.
textAreaEditor :: TextAreaState -> Editor
textAreaEditor state = Editor (buffer state) (selectionAnchor state) (history state)

withEditor :: TextAreaState -> Editor -> TextAreaState
withEditor state ed =
  ensureCaretVisible state {buffer = editorBuffer ed, selectionAnchor = editorAnchor ed, history = editorHistory ed}

-- | Run a command that needs no clipboard, keeping the caret in view.
runTextAreaCommand :: TextCommand -> TextAreaState -> TextAreaState
runTextAreaCommand cmd state = withEditor state (runCommand multiLineMode cmd (textAreaEditor state))

ensureCaretVisible :: TextAreaState -> TextAreaState
ensureCaretVisible state =
  let TB.Cursor r _ = TB.getCursor (buffer state)
      lh = lineHeight state
      vh = snd (viewportSize state)
      (sx, sy) = scrollOffset state
      caretY = fromIntegral r * lh
      caretH = lh
      contentH = fromIntegral (TB.getLineCount (buffer state)) * lh
      maxSy = max 0 (contentH - vh)
      sy'
        | vh <= 0 = 0
        | caretY < sy = caretY
        | caretY + caretH > sy + vh = caretY + caretH - vh
        | otherwise = sy
  in state {scrollOffset = (sx, clamp 0 maxSy sy')}

--------------------------------------------------------------------------------
-- Widget
--------------------------------------------------------------------------------

-- | Default viewport: grow horizontally, at least 200 pixels wide, 140 pixels tall.
textAreaLayout :: Layout
textAreaLayout = fixedH 140 . minW 200 . fillW $ defaultLayout

-- | Multi-line text editor over 'Text'. Pass the current text; the result is
-- the text after this frame's edits. Pair it with a 'label' when a caption is
-- wanted.
--
-- A frame that changes the text joins the whole document into the returned
-- 'Text'. For long documents use 'textAreaDocument', whose edits cost only
-- the lines they touch.
{-# INLINE textArea #-}
textArea :: Ui :> es => Text -> Eff es Text
textArea value = snd <$> textAreaWith' id value

-- | 'textArea' returning @(response, updatedText)@.
{-# INLINE textArea' #-}
textArea' :: Ui :> es => Text -> Eff es (Response, Text)
textArea' = textAreaWith' id

-- | 'textArea' with a modifier applied to 'textAreaLayout', for example
-- 'grow' to fill the parent.
{-# INLINE textAreaWith #-}
textAreaWith :: Ui :> es => (Layout -> Layout) -> Text -> Eff es Text
textAreaWith f value = snd <$> textAreaWith' f value

-- | 'textAreaWith' returning @(response, updatedText)@.
textAreaWith' :: Ui :> es => (Layout -> Layout) -> Text -> Eff es (Response, Text)
textAreaWith' f value = do
  (wid, ctx) <- freshWidget
  store <- uiIO (getStore ctx)
  let textKey = slotKey SlotTextAreaText (intKey wid)
      -- The text last passed or returned, and its document: passing it back
      -- neither splits it again nor, while nothing is edited, joins it.
      cached :: Maybe (Text, TextDocument) = lookupDyn textKey store
      incoming = case cached of
        Just (t, doc) | t == value -> doc
        _ -> textDocument value
  (resp, doc) <- textAreaCore f wid incoming
  case cached of
    Just (t, d) | sameDocument d doc -> pure (resp, t)
    _ -> do
      let out
            | sameDocument doc incoming = value
            | otherwise = documentText doc
      uiIO $ modifyStore ctx (insertDyn textKey (out, doc))
      pure (resp, out)

-- | Multi-line text editor over a 'TextDocument'. Pass the current document;
-- the result is the document after this frame's edits. An edit replaces the
-- lines it touches and shares the rest, and a frame without edits returns
-- the document it was passed. Edits avoid joining the whole document, though
-- their cost still depends on affected lines and tree operations. Join it
-- with 'documentText' when the whole text is wanted.
{-# INLINE textAreaDocument #-}
textAreaDocument :: Ui :> es => TextDocument -> Eff es TextDocument
textAreaDocument value = snd <$> textAreaDocumentWith' id value

-- | 'textAreaDocument' returning @(response, updatedDocument)@.
{-# INLINE textAreaDocument' #-}
textAreaDocument' :: Ui :> es => TextDocument -> Eff es (Response, TextDocument)
textAreaDocument' = textAreaDocumentWith' id

-- | 'textAreaDocument' with a modifier applied to 'textAreaLayout'.
{-# INLINE textAreaDocumentWith #-}
textAreaDocumentWith :: Ui :> es => (Layout -> Layout) -> TextDocument -> Eff es TextDocument
textAreaDocumentWith f value = snd <$> textAreaDocumentWith' f value

-- | 'textAreaDocumentWith' returning @(response, updatedDocument)@.
textAreaDocumentWith' :: Ui :> es => (Layout -> Layout) -> TextDocument -> Eff es (Response, TextDocument)
textAreaDocumentWith' f value = do
  wid <- nextId
  textAreaCore f wid value

textAreaCore :: Ui :> es => (Layout -> Layout) -> WidgetId -> TextDocument -> Eff es (Response, TextDocument)
textAreaCore f wid value = do
  ctx <- askContext
  uiIO $ registerFocusable ctx wid
  inp <- askInput
  store0 <- uiIO (getStore ctx)
  let layout = f textAreaLayout
      key = intKey wid
      seenKey = slotKey SlotSeen key
      bufKey = slotKey SlotTextAreaBuffer key
      changedSlotKey = slotKey SlotTextAreaChanged key
      stored :: Maybe TB.TextBuffer = lookupDyn bufKey store0
      adoptDocument
        | fmap bufferDocument stored == Just value = id
        | otherwise =
            insertDyn bufKey (maybe id keepCaret stored (documentBuffer value))
              . deleteSlot fieldDyn (slotKey SlotTextHistory key)
      keepCaret old new = (TB.withCursor (TB.getCursor old) new) {TB.preferredCol = TB.preferredCol old}
  -- Adopt the caller's document the way 'adoptSlot' does. Comparing the
  -- document the caller passes back with the stored one is O(1) ('==' checks
  -- identity first). A replaced document gets a buffer over its lines, whose
  -- widths are then measured afresh, keeping the caret where it was, and its
  -- undo history, recorded against the old text, goes. Seed the scroll slot
  -- too: the wheel and drag paths write offsets through setScrollOffset2D,
  -- which only updates the text area's slot once it exists.
  when (lookupDyn seenKey store0 /= Just value) $
    uiIO . setStore ctx $
      insertDyn seenKey value
        . adoptDocument
        . overField fieldPoint (IM.insertWith (\_ old -> old) (slotKey SlotTextAreaScroll key) (0, 0))
        . insertDyn (slotKey SlotTextMode key) multiLineMode
        $ store0
  store <- uiIO (getStore ctx)
  let current = maybe value bufferDocument (lookupDyn bufKey store)
      -- Set by commands run outside the frame ('applyTextFieldCommand') whose
      -- edits carry no keys or chars; folded into 'changed' so the caller
      -- gets its respChanged pulse, then cleared in the state write below.
      menuPulse = memberSlot fieldInt changedSlotKey store
  isFocus <- keyboardFocused wid
  (newDoc, stateChanged) <-
    if isFocus
      then do
        editFm <-
          if layoutFontSize layout <= 0
            then pure (ctxFontMetrics ctx)
            else fst <$> uiIO (ctxResolveFont ctx (layoutFontSize layout) WeightNormal FontStyleNormal FontRegular)
        let oldState = loadTextAreaState store key
            s1 = setTextAreaViewport (viewportSize oldState) (realToFrac (fmLineHeight editFm)) oldState
            hadInput = not (T.null (inputChars inp)) || not (inputKeysNull (inputKeys inp))
        newState <- uiIO $ do
          when hadInput $ modifyInteraction ctx (\s -> s {isTextInputDrag = Nothing})
          fieldTextCommands ctx multiLineMode inp >>= \case
            [] -> pure s1
            cmds -> withEditor s1 <$> foldM (flip (runCommandIO ctx multiLineMode)) (textAreaEditor s1) cmds
        let newBuf = buffer newState
            -- Edits make new lines and moves keep them, so this reads no
            -- text; the editor drops edits that would change nothing.
            textChanged = not (sameLines (TB.bufferLines newBuf) (documentLines current))
            doc
              | textChanged = bufferDocument newBuf
              | otherwise = current
            changed =
              textChanged
                || TB.getCursor newBuf /= TB.getCursor (buffer oldState)
                || selectionAnchor newState /= selectionAnchor oldState
                || scrollOffset newState /= scrollOffset oldState
                || menuPulse
        -- Saving writes the new buffer, which is the document, and drops the
        -- menu pulse. The store damage is keyed on slots, not the widget, so
        -- damage the widget itself: a selection-only change (Ctrl+A) would
        -- otherwise repaint nothing until the next frame.
        when changed $
          uiIO $ do
            damageWidget ctx wid DamageSelf
            modifyStore ctx (deleteSlot fieldInt changedSlotKey . saveTextAreaState key newState)
        pure (doc, changed)
      else do
        -- A command run on the unfocused area ('applyTextFieldCommand') still
        -- pulses this frame's respChanged, once.
        when menuPulse $
          uiIO $ modifyStore ctx (deleteSlot fieldInt changedSlotKey)
        pure (current, menuPulse)
  -- Record what is returned as seen, so the caller passing it back is not
  -- taken for a replacement. A caller that passes an equal copy each frame
  -- gets the stored document back every frame; rewriting the slot then would
  -- damage it and wake the loop forever.
  unless (sameDocument newDoc value) $
    uiIO $ modifyStore ctx $ \st ->
      case lookupDyn seenKey st of
        Just seen | sameDocument seen newDoc -> st
        _ -> insertDyn seenKey newDoc st
  resp <- addWidget wid NodeTextArea "" 0 layout
  pure (setChanged stateChanged resp, newDoc)

-- | The text area's 'TB.TextBuffer', which holds its document and caret. The
-- widget stores one over the lines of every document it adopts, so it is only
-- missing for a text area never declared, which holds an empty document.
textAreaBuffer :: WidgetStore -> Int -> TB.TextBuffer
textAreaBuffer store key = fromMaybe TB.empty (lookupDyn (slotKey SlotTextAreaBuffer key) store)

-- | The text area's editor state as stored.
loadTextAreaState :: WidgetStore -> Int -> TextAreaState
loadTextAreaState store key =
  let buf = textAreaBuffer store key
      TB.Cursor row col = TB.getCursor buf
      anchorRow = findSlot fieldInt row (slotKey SlotTextAreaAnchorRow key) store
      anchorCol = findSlot fieldInt col (slotKey SlotTextAreaAnchorCol key) store
      (sx, sy) = findSlot fieldPoint (0, 0) (slotKey SlotTextAreaScroll key) store
      (vw, vh) = findSlot fieldPoint (200, 96) (slotKey SlotTextAreaViewport key) store
   in TextAreaState
        { buffer = buf
        , selectionAnchor = TB.clampCursor buf (TB.Cursor anchorRow anchorCol)
        , scrollOffset = (realToFrac sx, realToFrac sy)
        , viewportSize = (realToFrac vw, realToFrac vh)
        , lineHeight = 16
        , -- Replacing the document drops its history, so the history is
          -- always the current document's.
          history = fromMaybe emptyHistory (lookupDyn (slotKey SlotTextHistory key) store)
        }

-- | Store the editor state. The buffer holds the document and the caret.
saveTextAreaState :: Int -> TextAreaState -> WidgetStore -> WidgetStore
saveTextAreaState key state =
  insertSlot fieldPoint (slotKey SlotTextAreaScroll key) (realToFrac sx, realToFrac sy)
    . insertSlot fieldPoint (slotKey SlotTextAreaViewport key) (realToFrac vw, realToFrac vh)
    . insertDyn (slotKey SlotTextAreaBuffer key) (buffer state)
    . insertDyn (slotKey SlotTextHistory key) (history state)
    . insertSlot fieldInt (slotKey SlotTextAreaAnchorRow key) anchorRow
    . insertSlot fieldInt (slotKey SlotTextAreaAnchorCol key) anchorCol
  where
    TB.Cursor anchorRow anchorCol = selectionAnchor state
    (sx, sy) = scrollOffset state
    (vw, vh) = viewportSize state

-- | A text area's stored editor, for a command run outside its frame, and
-- how to store the edited editor, keeping the caret in view.
textAreaFieldEditor :: WidgetStore -> Int -> (Editor, Editor -> WidgetStore -> WidgetStore)
textAreaFieldEditor store key =
  let state = loadTextAreaState store key
   in (textAreaEditor state, saveTextAreaState key . withEditor state)
