{-# LANGUAGE BangPatterns #-}

-- | The multi-line text area widget and its state: the document buffer,
-- caret and selection, viewport, and commands run against it.
module NanoUI.Widgets.TextArea
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
  , loadTextAreaState
  , loadTextAreaStateWithBuffer
  , saveTextAreaState
  , textAreaEditor
  , runTextAreaCommand
  , applyTextAreaCommand
  ) where

import Control.Monad (foldM, unless, when)
import Data.Dynamic (fromDynamic, toDyn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.IntMap.Strict as IM
import Effectful (Eff, type (:>))
import NanoUI.Context
  ( Context (..)
  , damageWidget
  , getStore
  , intKey
  , markDirty
  , registerFocusable
  , setStore
  , setTextInputDrag
  , modifyStore
  )
import NanoUI.Font (fmLineHeight)
import NanoUI.Frame.TextArea.Content (textAreaBuffer)
import NanoUI.Id (WidgetId)
import NanoUI.Input
  ( Input (..)
  , inputChars
  , inputKeys
  , inputKeysNull
  )
import NanoUI.Layout.Arena (NodeType (..))
import NanoUI.Monad (Ui, askContext, askInput, nextId, uiIO)
import NanoUI.Store
  ( WidgetStore (..)
  , slotKey
  , Slot (..)
  )
import NanoUI.Style (FontStyle (..), FontVariant (..), FontWeight (..), Layout (..), Sizing (..), defaultLayout)
import NanoUI.Types (DamageBounds (..), clamp)
import NanoUI.Widgets.Behavior (keyboardFocused)
import NanoUI.Widgets.Node (Response, addWidget, setChanged)
import qualified NanoUI.Widgets.TextBuffer as TB
import NanoUI.Widgets.TextDocument
  ( TextDocument
  , bufferDocument
  , documentBuffer
  , documentLines
  , documentText
  , sameDocument
  , sameLines
  , textDocument
  )
import NanoUI.Widgets.TextEditor
  ( Editor (..)
  , EditHistory
  , TextCommand (..)
  , inputTextCommands
  , editorModeCode
  , emptyHistory
  , multiLineMode
  , runCommand
  , runCommandIO
  , sealHistory
  )

data TextAreaState = TextAreaState
  { buffer :: !TB.TextBuffer
  , selectionAnchor :: !TB.Cursor
  , scrollOffset :: !(Double, Double)
  , viewportSize :: !(Double, Double)
  , lineHeight :: !Double
  , history :: !EditHistory
  }
  deriving (Show)

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

setTextAreaViewport :: (Double, Double) -> Double -> TextAreaState -> TextAreaState
setTextAreaViewport vp lh state =
  state {viewportSize = vp, lineHeight = lh}

cursorOf :: TextAreaState -> TB.Cursor
cursorOf state = TB.getCursor (buffer state)

setTextAreaSelection :: TB.Cursor -> TB.Cursor -> TextAreaState -> TextAreaState
setTextAreaSelection anchor cursor state =
  let buf =
        let b = TB.withCursor cursor (buffer state)
         in b {TB.preferredCol = TB.cursorCol cursor}
   in ensureCaretVisible state {buffer = buf, selectionAnchor = anchor}

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

textAreaLayout :: Layout
textAreaLayout =
  defaultLayout
    { layoutWidth = Grow 1
    , layoutMinW = 200
    , layoutHeight = Fixed 140
    }

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

{-# INLINE textArea' #-}
textArea' :: Ui :> es => Text -> Eff es (Response, Text)
textArea' = textAreaWith' id

-- | 'textArea' with a modifier applied to 'textAreaLayout', for example
-- 'grow' to fill the parent.
{-# INLINE textAreaWith #-}
textAreaWith :: Ui :> es => (Layout -> Layout) -> Text -> Eff es Text
textAreaWith f value = snd <$> textAreaWith' f value

textAreaWith' :: Ui :> es => (Layout -> Layout) -> Text -> Eff es (Response, Text)
textAreaWith' f value = do
  wid <- nextId
  ctx <- askContext
  store <- uiIO (getStore ctx)
  let textKey = slotKey SlotTextAreaText (intKey wid)
      -- The text last passed or returned, and its document: passing it back
      -- neither splits it again nor, while nothing is edited, joins it.
      cached :: Maybe (Text, TextDocument) = IM.lookup textKey (storeDyn store) >>= fromDynamic
      incoming = case cached of
        Just (t, doc) | t == value -> doc
        _ -> textDocument value
  (resp, doc) <- textAreaCore f wid incoming
  let out = case cached of
        Just (t, d) | sameDocument d doc -> t
        _
          | sameDocument doc incoming -> value
          | otherwise -> documentText doc
  case cached of
    Just (_, d) | sameDocument d doc -> pure ()
    _ -> uiIO $ modifyStore ctx $ \st -> st {storeDyn = IM.insert textKey (toDyn (out, doc)) (storeDyn st)}
  pure (resp, out)

-- | Multi-line text editor over a 'TextDocument'. Pass the current document;
-- the result is the document after this frame's edits. An edit replaces the
-- lines it touches and shares the rest, and a frame without edits returns
-- the document it was passed, so a keystroke costs the same in a long
-- document as in a short one. Join it with 'documentText' when the whole
-- text is wanted.
{-# INLINE textAreaDocument #-}
textAreaDocument :: Ui :> es => TextDocument -> Eff es TextDocument
textAreaDocument value = snd <$> textAreaDocumentWith' id value

{-# INLINE textAreaDocument' #-}
textAreaDocument' :: Ui :> es => TextDocument -> Eff es (Response, TextDocument)
textAreaDocument' = textAreaDocumentWith' id

-- | 'textAreaDocument' with a modifier applied to 'textAreaLayout'.
{-# INLINE textAreaDocumentWith #-}
textAreaDocumentWith :: Ui :> es => (Layout -> Layout) -> TextDocument -> Eff es TextDocument
textAreaDocumentWith f value = snd <$> textAreaDocumentWith' f value

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
      docKey = slotKey SlotTextAreaDocument key
      contentCacheKey = slotKey SlotTextAreaContentFont key
      changedSlotKey = slotKey SlotTextAreaChanged key
      dyn0 = storeDyn store0
      storedDoc k = IM.lookup k dyn0 >>= fromDynamic
      replaced = storedDoc docKey /= Just value
  -- Adopt the caller's document the way 'adoptStoreText' does. Comparing the
  -- document the caller passes back with the stored one is O(1) ('==' checks
  -- identity first). A replaced document gets a buffer over its lines and
  -- orphans the content size measured for the old one, and its undo history,
  -- recorded against the old text, goes with them. Seed the scroll slot too:
  -- the wheel and drag paths write offsets through setScrollOffset2D, which
  -- only updates the text area's slot once it exists.
  when (storedDoc seenKey /= Just value) $
    uiIO $ setStore ctx
      store0
        { storeDyn =
            IM.insert seenKey (toDyn value) $
              if replaced
                then
                  IM.insert docKey (toDyn value) $
                    IM.insert (slotKey SlotTextAreaBuffer key) (toDyn (documentBuffer value)) $
                      IM.delete (slotKey SlotTextHistory key) dyn0
                else dyn0
        , storePoint = IM.insertWith (\_ old -> old) (slotKey SlotTextAreaScroll key) (0, 0) (storePoint store0)
        , storeFloat = if replaced then IM.delete contentCacheKey (storeFloat store0) else storeFloat store0
        , storeInt = IM.insert (slotKey SlotTextMode key) (editorModeCode multiLineMode) (storeInt store0)
        }
  store <- uiIO (getStore ctx)
  let current = fromMaybe value (IM.lookup docKey (storeDyn store) >>= fromDynamic)
      -- Set by commands run outside the frame ('applyTextAreaCommand') whose
      -- edits carry no keys or chars; folded into 'changed' so the caller
      -- gets its respChanged pulse, then cleared in the state write below.
      menuPulse = IM.member changedSlotKey (storeInt store)
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
          when hadInput $ setTextInputDrag ctx Nothing
          case inputTextCommands multiLineMode inp of
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
                || cursorOf newState /= cursorOf oldState
                || selectionAnchor newState /= selectionAnchor oldState
                || scrollOffset newState /= scrollOffset oldState
                || menuPulse
        -- Saving writes the new buffer and its document together; drop only
        -- the content size measured for the old text, and the menu pulse. The
        -- store damage is keyed on slots, not the widget, so damage the widget
        -- itself: a selection-only change (Ctrl+A) would otherwise repaint
        -- nothing until the next frame.
        when changed $
          uiIO $ do
            damageWidget ctx wid DamageSelf
            modifyStore ctx $ \st0 ->
              let st = saveTextAreaState key newState st0
               in st
                    { storeInt = IM.delete changedSlotKey (storeInt st)
                    , storeFloat = if textChanged then IM.delete contentCacheKey (storeFloat st) else storeFloat st
                    }
        pure (doc, changed)
      else do
        -- A command run on the unfocused area ('applyTextAreaCommand') still
        -- pulses this frame's respChanged, once.
        when menuPulse $
          uiIO $ modifyStore ctx $ \st -> st {storeInt = IM.delete changedSlotKey (storeInt st)}
        pure (current, menuPulse)
  -- Record what is returned as seen, so the caller passing it back is not
  -- taken for a replacement. A caller that passes an equal copy each frame
  -- gets the stored document back every frame; rewriting the slot then would
  -- damage it and wake the loop forever.
  unless (sameDocument newDoc value) $
    uiIO $ modifyStore ctx $ \st ->
      case IM.lookup seenKey (storeDyn st) >>= fromDynamic of
        Just seen | sameDocument seen newDoc -> st
        _ -> st {storeDyn = IM.insert seenKey (toDyn newDoc) (storeDyn st)}
  resp <- addWidget wid NodeTextArea "" 0 layout
  pure (setChanged stateChanged resp, newDoc)

-- | The text area's editor state as stored. A text area that has not been
-- declared yet holds an empty document.
loadTextAreaState :: WidgetStore -> Int -> TextAreaState
loadTextAreaState store key = loadTextAreaStateWithBuffer store key (textAreaBuffer store key)

-- | 'loadTextAreaState' with the buffer already resolved (the paint path
-- reads the buffer and hands it straight through, avoiding a second store
-- lookup).
loadTextAreaStateWithBuffer :: WidgetStore -> Int -> TB.TextBuffer -> TextAreaState
loadTextAreaStateWithBuffer store key buf0 =
  let row = IM.findWithDefault 0 (slotKey SlotTextAreaRow key) (storeInt store)
      col = IM.findWithDefault 0 (slotKey SlotTextAreaCol key) (storeInt store)
      anchorRow = IM.findWithDefault row (slotKey SlotTextAreaAnchorRow key) (storeInt store)
      anchorCol = IM.findWithDefault col (slotKey SlotTextAreaAnchorCol key) (storeInt store)
      pref = IM.findWithDefault col (slotKey SlotTextAreaPrefCol key) (storeInt store)
      scroll =
        let (sx, sy) =
              IM.findWithDefault (0, 0) (slotKey SlotTextAreaScroll key) (storePoint store)
         in (realToFrac sx, realToFrac sy)
      viewport =
        let (vw, vh) =
              IM.findWithDefault (200, 96) (slotKey SlotTextAreaViewport key) (storePoint store)
         in (realToFrac vw, realToFrac vh)
      buf =
        let b = TB.withCursor (TB.Cursor row col) buf0
         in b {TB.preferredCol = pref}
      anchor = TB.getCursor (TB.withCursor (TB.Cursor anchorRow anchorCol) buf0)
      -- Replacing the document drops its history, so the history is always
      -- the current document's.
      hist = fromMaybe emptyHistory (IM.lookup (slotKey SlotTextHistory key) (storeDyn store) >>= fromDynamic)
   in TextAreaState
        { buffer = buf
        , selectionAnchor = anchor
        , scrollOffset = scroll
        , viewportSize = viewport
        , lineHeight = 16
        , history = hist
        }

-- | Store the editor state. When its lines are not the stored document's, the
-- document becomes theirs, so the buffer and the document stay in step.
saveTextAreaState :: Int -> TextAreaState -> WidgetStore -> WidgetStore
saveTextAreaState key state store =
  let buf = buffer state
      TB.Cursor row col = TB.getCursor buf
      TB.Cursor anchorRow anchorCol = selectionAnchor state
      docKey = slotKey SlotTextAreaDocument key
      withDocument dyn = case IM.lookup docKey dyn >>= fromDynamic of
        Just doc | sameLines (documentLines doc) (TB.bufferLines buf) -> dyn
        _ -> IM.insert docKey (toDyn (bufferDocument buf)) dyn
   in store
        { storeDyn =
            IM.insert (slotKey SlotTextAreaBuffer key) (toDyn buf) $
              IM.insert (slotKey SlotTextHistory key) (toDyn (history state)) $
                withDocument (storeDyn store)
        , storeInt =
            IM.insert (slotKey SlotTextAreaRow key) row $
              IM.insert (slotKey SlotTextAreaCol key) col $
                IM.insert (slotKey SlotTextAreaPrefCol key) (TB.preferredCol buf) $
                  IM.insert (slotKey SlotTextAreaAnchorRow key) anchorRow $
                    IM.insert (slotKey SlotTextAreaAnchorCol key) anchorCol (storeInt store)
        , storePoint =
            IM.insert (slotKey SlotTextAreaScroll key) (realToFrac sx, realToFrac sy) $
              IM.insert (slotKey SlotTextAreaViewport key) (realToFrac vw, realToFrac vh) (storePoint store)
        }
  where
    (sx, sy) = scrollOffset state
    (vw, vh) = viewportSize state

-- | Run a command on a text area outside its frame (a context menu row, an
-- app's Edit menu). A change to the text pulses 'respChanged' on the area's
-- next frame.
applyTextAreaCommand :: Context -> WidgetId -> TextCommand -> IO ()
applyTextAreaCommand ctx wid cmd = do
  store <- getStore ctx
  let key = intKey wid
      s0 = loadTextAreaState store key
  s1 <- withEditor s0 <$> runCommandIO ctx multiLineMode cmd (textAreaEditor s0 {history = sealHistory (history s0)})
  let edited = not (sameLines (TB.bufferLines (buffer s1)) (TB.bufferLines (buffer s0)))
      saved = saveTextAreaState key s1 store
  -- A changed text also drops the content size measured for the old one.
  setStore ctx $
    if edited
      then
        saved
          { storeInt = IM.insert (slotKey SlotTextAreaChanged key) 1 (storeInt saved)
          , storeFloat = IM.delete (slotKey SlotTextAreaContentFont key) (storeFloat saved)
          }
      else saved
  -- Store damage is keyed on slots, not the widget: damage the widget so a
  -- selection-only command (Select All) repaints this frame.
  damageWidget ctx wid DamageSelf
  markDirty ctx
