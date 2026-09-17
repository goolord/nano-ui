{-# LANGUAGE BangPatterns #-}

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
  , textAreaLayout
  , loadTextAreaState
  , loadTextAreaStateWithBuffer
  , saveTextAreaState
  , textAreaEditor
  , runTextAreaCommand
  , applyTextAreaCommand
  ) where

import Control.Monad (foldM, when)
import Data.Dynamic (fromDynamic, toDyn)
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

-- | Multi-line text editor. Pass the current text; the result is the text
-- after this frame's edits. Pair it with a 'label' when a caption is wanted.
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
  uiIO $ registerFocusable ctx wid
  inp <- askInput
  store0 <- uiIO (getStore ctx)
  let layout = f textAreaLayout
      key = intKey wid
      seenKey = slotKey SlotSeen key
      contentCacheKey = slotKey SlotTextAreaContentFont key
      changedSlotKey = slotKey SlotTextAreaChanged key
      texts0 = storeText store0
      replaced = IM.lookup key texts0 /= Just value
  -- Adopt the caller's text the way 'adoptStoreText' does. A replaced document
  -- orphans any cached buffer or content size for the key. Seed the scroll
  -- slot too: the wheel and drag paths write offsets through
  -- setScrollOffset2D, which only updates the text area's slot once it exists.
  -- Its undo history, recorded against the old text, goes with them.
  when (IM.lookup seenKey texts0 /= Just value) $
    uiIO $ setStore ctx
      store0
        { storeText = IM.insert seenKey value (IM.insert key value texts0)
        , storePoint = IM.insertWith (\_ old -> old) (slotKey SlotTextAreaScroll key) (0, 0) (storePoint store0)
        , storeFloat = if replaced then IM.delete contentCacheKey (storeFloat store0) else storeFloat store0
        , storeDyn =
            if replaced
              then IM.delete (slotKey SlotTextHistory key) (IM.delete (slotKey SlotTextAreaBuffer key) (storeDyn store0))
              else storeDyn store0
        , storeInt = IM.insert (slotKey SlotTextMode key) (editorModeCode multiLineMode) (storeInt store0)
        }
  store <- uiIO (getStore ctx)
  let current = IM.findWithDefault value key (storeText store)
      -- Set by commands run outside the frame ('applyTextAreaCommand') whose
      -- edits carry no keys or chars; folded into 'changed' so the caller
      -- gets its respChanged pulse, then cleared in the state write below.
      menuPulse = IM.member changedSlotKey (storeInt store)
  isFocus <- keyboardFocused wid
  (newText, stateChanged) <-
    if isFocus
      then do
        editFm <-
          if layoutFontSize layout <= 0
            then pure (ctxFontMetrics ctx)
            else fst <$> uiIO (ctxResolveFont ctx (layoutFontSize layout) WeightNormal FontStyleNormal FontRegular)
        let oldState = loadTextAreaState store key value
            s1 = setTextAreaViewport (viewportSize oldState) (realToFrac (fmLineHeight editFm)) oldState
            hadInput = not (T.null (inputChars inp)) || not (inputKeysNull (inputKeys inp))
        newState <- uiIO $ do
          when hadInput $ setTextInputDrag ctx Nothing
          case inputTextCommands multiLineMode inp of
            [] -> pure s1
            cmds -> withEditor s1 <$> foldM (flip (runCommandIO ctx multiLineMode)) (textAreaEditor s1) cmds
        let newText
              -- Commands only come from keys or chars, so idle focused frames
              -- skip the O(document) 'TB.toText' and stop at the cheap
              -- cursor/scroll checks.
              | hadInput || changed = TB.toText (buffer newState)
              | otherwise = current
            changed =
              cursorOf newState /= cursorOf oldState
                || selectionAnchor newState /= selectionAnchor oldState
                || scrollOffset newState /= scrollOffset oldState
                || menuPulse
                || (hadInput && newText /= current)
        -- Saving writes the new text and its buffer together; drop only the
        -- content size measured for the old text, and the menu pulse. The
        -- store damage is keyed on slots, not the widget, so damage the widget
        -- itself: a selection-only change (Ctrl+A) would otherwise repaint
        -- nothing until the next frame.
        when changed $
          uiIO $ do
            damageWidget ctx wid DamageSelf
            modifyStore ctx $ \st0 ->
              let st = saveTextAreaState key newText newState st0
               in st
                    { storeText = IM.insert seenKey newText (storeText st)
                    , storeInt = IM.delete changedSlotKey (storeInt st)
                    , storeFloat = IM.delete contentCacheKey (storeFloat st)
                    }
        pure (newText, changed)
      else do
        -- A command run on the unfocused area ('applyTextAreaCommand') still
        -- pulses this frame's respChanged, once.
        when menuPulse $
          uiIO $ modifyStore ctx $ \st -> st {storeInt = IM.delete changedSlotKey (storeInt st)}
        pure (current, menuPulse)
  resp <- addWidget wid NodeTextArea "" 0 layout
  pure (setChanged stateChanged resp, newText)

loadTextAreaState :: WidgetStore -> Int -> Text -> TextAreaState
loadTextAreaState store key initial =
  let text = IM.findWithDefault initial key (storeText store)
      -- The buffer cache is written together with storeText by
      -- saveTextAreaState, so a present entry is always the buffer for the
      -- stored text; no (O(document)) re-comparison is needed.
      cachedBuffer :: Maybe TB.TextBuffer =
        IM.lookup (slotKey SlotTextAreaBuffer key) (storeDyn store) >>= fromDynamic
      buf0 = case cachedBuffer of
        Just cached -> cached
        Nothing -> TB.fromText text
   in loadTextAreaStateWithBuffer store key buf0

-- | 'loadTextAreaState' with the buffer already resolved (the paint path
-- ensures the buffer cache and hands it straight through, avoiding a second
-- store lookup).
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
      -- Replacing the document drops its history, so the recorded text is
      -- always the current one here.
      hist = case IM.lookup (slotKey SlotTextHistory key) (storeDyn store) >>= fromDynamic of
        Just (_ :: Text, h) -> h
        Nothing -> emptyHistory
   in TextAreaState
        { buffer = buf
        , selectionAnchor = anchor
        , scrollOffset = scroll
        , viewportSize = viewport
        , lineHeight = 16
        , history = hist
        }

-- | Store the editor state with its text. Callers pass the text because they
-- usually have it already, and 'TB.toText' joins the whole document.
saveTextAreaState :: Int -> Text -> TextAreaState -> WidgetStore -> WidgetStore
saveTextAreaState key text state store =
  let TB.Cursor row col = TB.getCursor (buffer state)
      TB.Cursor anchorRow anchorCol = selectionAnchor state
   in store
        { storeText = IM.insert key text (storeText store)
        , storeDyn =
            IM.insert (slotKey SlotTextAreaBuffer key) (toDyn (buffer state)) $
              IM.insert (slotKey SlotTextHistory key) (toDyn (text, history state)) (storeDyn store)
        , storeInt =
            IM.insert (slotKey SlotTextAreaRow key) row $
              IM.insert (slotKey SlotTextAreaCol key) col $
                IM.insert (slotKey SlotTextAreaPrefCol key) (TB.preferredCol (buffer state)) $
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
      text = IM.findWithDefault "" key (storeText store)
      s0 = loadTextAreaState store key text
  s1 <- withEditor s0 <$> runCommandIO ctx multiLineMode cmd (textAreaEditor s0 {history = sealHistory (history s0)})
  let newText = TB.toText (buffer s1)
      saved = saveTextAreaState key newText s1 store
  -- A changed text also drops the content size measured for the old one.
  setStore ctx $
    if newText == text
      then saved
      else
        saved
          { storeInt = IM.insert (slotKey SlotTextAreaChanged key) 1 (storeInt saved)
          , storeFloat = IM.delete (slotKey SlotTextAreaContentFont key) (storeFloat saved)
          }
  -- Store damage is keyed on slots, not the widget: damage the widget so a
  -- selection-only command (Select All) repaints this frame.
  damageWidget ctx wid DamageSelf
  markDirty ctx
