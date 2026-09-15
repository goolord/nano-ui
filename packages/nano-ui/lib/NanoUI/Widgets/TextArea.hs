{-# LANGUAGE BangPatterns #-}

module NanoUI.Widgets.TextArea
  ( -- * Pure state
    TextAreaState (..)
  , initTextAreaState
  , setTextAreaViewport
  , TextAreaEvent (..)
  , Modifiers (..)
  , handleTextAreaEvent
  , setTextAreaSelection
  , TextAreaLayout (..)
  , VisualLine (..)
  , computeTextAreaLayout
    -- * Widget
  , textArea
  , textArea'
  , textAreaWith
  , textAreaWith'
  , textAreaLayout
  , processTextArea
  , loadTextAreaState
  , loadTextAreaStateWithBuffer
  , saveTextAreaState
  , applyTextAreaMenuAction
  ) where

import Control.Monad (when)
import Data.Char (isPrint, toLower)
import Data.Dynamic (fromDynamic, toDyn)
import Data.IORef (writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.IntMap.Strict as IM
import Effectful (Eff, type (:>))
import NanoUI.Context
  ( Context (..)
  , getStore
  , intKey
  , markDirty
  , registerFocusable
  , setStore
  , setTextInputDrag
  , setTextInputMenu
  )
import NanoUI.Font (fmLineHeight)
import NanoUI.Id (WidgetId)
import NanoUI.Input
  ( Input (..)
  , Key (..)
  , Modifiers (..)
  , foldInputKeys
  , inputChars
  , inputKeys
  , inputKeysNull
  , inputModifiers
  )
import NanoUI.Layout.Arena (NodeType (..))
import NanoUI.Monad (Ui, askContext, askInput, nextId, uiIO)
import NanoUI.Store
  ( WidgetStore (..)
  , slotKey
  , slotTextAreaAnchorCol
  , slotTextAreaAnchorRow
  , slotTextAreaBuffer
  , slotTextAreaChanged
  , slotTextAreaCol
  , slotTextAreaContentFont
  , slotTextAreaPrefCol
  , slotTextAreaRow
  , slotTextAreaScroll
  , slotTextAreaViewport
  , slotSeen
  )
import NanoUI.Style (FontStyle (..), FontVariant (..), FontWeight (..), Layout (..), Sizing (..), defaultLayout)
import NanoUI.Types (clamp)
import NanoUI.Widgets.Behavior (keyboardFocused)
import NanoUI.Widgets.Node (Response, addWidget, setChanged)
import qualified NanoUI.Widgets.TextBuffer as TB
import NanoUI.Widgets.TextCommon
  ( MenuAction
  , copyBufferText
  , cutBufferText
  , dispatchCtrlChar
  , dispatchMenuAction
  , isCtrlCombo
  , pasteBufferText
  )

-- | One editor input: a typed character or a key.
data TextAreaEvent
  = TAChar !Char
  | TAKey !Key
  deriving (Eq, Show)

data TextAreaState = TextAreaState
  { buffer :: !TB.TextBuffer
  , selectionAnchor :: !TB.Cursor
  , scrollOffset :: !(Double, Double)
  , viewportSize :: !(Double, Double)
  , lineHeight :: !Double
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
    }

setTextAreaViewport :: (Double, Double) -> Double -> TextAreaState -> TextAreaState
setTextAreaViewport vp lh state =
  state {viewportSize = vp, lineHeight = lh}

cursorOf :: TextAreaState -> TB.Cursor
cursorOf state = TB.getCursor (buffer state)

hasSelection :: TextAreaState -> Bool
hasSelection state = cursorOf state /= selectionAnchor state

selectionRangeOf :: TextAreaState -> Maybe (TB.Cursor, TB.Cursor)
selectionRangeOf state
  | hasSelection state = Just (TB.selectionRange (selectionAnchor state) (cursorOf state))
  | otherwise = Nothing

clearSelection :: TextAreaState -> TextAreaState
clearSelection state =
  let cur = cursorOf state
   in state {selectionAnchor = cur, buffer = TB.withCursor cur (buffer state)}

setTextAreaSelection :: TB.Cursor -> TB.Cursor -> TextAreaState -> TextAreaState
setTextAreaSelection anchor cursor state =
  let buf =
        let b = TB.withCursor cursor (buffer state)
         in b {TB.preferredCol = TB.cursorCol cursor}
   in ensureCaretVisible state {buffer = buf, selectionAnchor = anchor}

moveCursor :: Bool -> (TB.TextBuffer -> TB.TextBuffer) -> TextAreaState -> TextAreaState
moveCursor shift f state =
  let buf' = f (buffer state)
      cur = TB.getCursor buf'
   in if shift
        then ensureCaretVisible state {buffer = buf'}
        else ensureCaretVisible (state {buffer = buf', selectionAnchor = cur})

deleteSelection :: TextAreaState -> TextAreaState
deleteSelection state =
  case selectionRangeOf state of
    Nothing -> state
    Just (lo, hi) ->
      let buf' = TB.deleteRange lo hi (buffer state)
       in clearSelection state {buffer = buf'}

insertWithSelection :: Char -> TextAreaState -> TextAreaState
insertWithSelection ch state =
  let buf' = case selectionRangeOf state of
        Nothing -> TB.insertChar ch (buffer state)
        Just (lo, hi) -> TB.replaceRange (T.singleton ch) lo hi (buffer state)
   in ensureCaretVisible state {buffer = buf', selectionAnchor = TB.getCursor buf'}

-- | Apply one typed character or key. Ctrl or Alt turns Backspace, Delete,
-- Left and Right into word edits and motions; Ctrl+K/U/A/E kill to the end or
-- start of the line, select all, and jump to the line end. Escape and Tab
-- belong to the frame (menus, focus) and leave the editor untouched.
handleTextAreaEvent :: TextAreaEvent -> Modifiers -> TextAreaState -> TextAreaState
handleTextAreaEvent (TAKey KeyEscape) _ state = state
handleTextAreaEvent (TAKey KeyTab) _ state = state
handleTextAreaEvent event mods state =
  ensureCaretVisible $ case event of
    TAChar c
      | not word -> insertWithSelection c state
      | ctrl && not alt -> ctrlChar (toLower c) c
      | otherwise -> state
    TAKey key -> case key of
      KeyEnter | not word -> insertWithSelection '\n' state
      KeyBackspace
        | word -> move TB.deletePrevWord
        | otherwise -> deleteOr TB.deletePrevChar
      KeyDelete
        | word -> move TB.deleteNextWord
        | otherwise -> deleteOr TB.deleteChar
      KeyLeft -> move (if word then TB.moveWordLeft else TB.moveLeft)
      KeyRight -> move (if word then TB.moveWordRight else TB.moveRight)
      KeyUp | not word -> move TB.moveUp
      KeyDown | not word -> move TB.moveDown
      KeyHome | not word -> move TB.moveToBOL
      KeyEnd | not word -> move TB.moveToEOL
      _ -> state
  where
    shift = modShift mods
    ctrl = modCtrl mods
    alt = modAlt mods
    word = ctrl || alt
    move f = moveCursor shift f state
    deleteOr f = case selectionRangeOf state of
      Just _ -> deleteSelection state
      Nothing -> moveCursor False f state
    -- Ctrl letters may arrive as the letter or as its control code.
    ctrlChar lower c
      | lower == 'k' || c == '\v' = move TB.killToEOL
      | lower == 'u' || c == '\NAK' = move TB.killToBOL
      | lower == 'a' || c == '\x01' = selectAllTextArea state
      | lower == 'e' || c == '\ENQ' = move TB.moveToEOL
      | otherwise = state

selectAllTextArea :: TextAreaState -> TextAreaState
selectAllTextArea state =
  let end = TB.documentEnd (buffer state)
   in setTextAreaSelection (TB.Cursor 0 0) end state

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

data VisualLine = VisualLine
  { visualLineIndex :: !Int
  , visualLineText :: !T.Text
  , visualLineY :: !Double
  }
  deriving (Eq, Show)

data TextAreaLayout = TextAreaLayout
  { layoutLines :: ![VisualLine]
  , layoutCaretX :: !Double
  , layoutCaretY :: !Double
  , layoutCaretH :: !Double
  }
  deriving (Eq, Show)

computeTextAreaLayout
  :: (T.Text -> Double)
  -> Double
  -> TextAreaState
  -> TextAreaLayout
computeTextAreaLayout measureWidth lineH state =
  let buf = buffer state
      TB.Cursor r c = TB.getCursor buf
      (scrollX, scrollY) = scrollOffset state
      linesList = TB.toLines buf
      indexedLines = zip [0 ..] linesList
      visLines =
        [ VisualLine idx txt (fromIntegral idx * lineH - scrollY)
        | (idx, txt) <- indexedLines
        ]
      currentLineText = TB.lineAt r buf
      prefixText = T.take c currentLineText
      caretX = measureWidth prefixText - scrollX
      caretY = fromIntegral r * lineH - scrollY
  in TextAreaLayout
    { layoutLines = visLines
    , layoutCaretX = caretX
    , layoutCaretY = caretY
    , layoutCaretH = lineH
    }

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
      seenKey = slotKey slotSeen key
      contentCacheKey = slotKey slotTextAreaContentFont key
      changedSlotKey = slotKey slotTextAreaChanged key
      texts0 = storeText store0
      replaced = IM.lookup key texts0 /= Just value
  -- Adopt the caller's text the way 'adoptStoreText' does. A replaced document
  -- orphans any cached buffer or content size for the key. Seed the scroll
  -- slot too: the wheel and drag paths write offsets through
  -- setScrollOffset2D, which only updates the text area's slot once it exists.
  when (IM.lookup seenKey texts0 /= Just value) $
    uiIO $ setStore ctx
      store0
        { storeText = IM.insert seenKey value (IM.insert key value texts0)
        , storePoint = IM.insertWith (\_ old -> old) (slotKey slotTextAreaScroll key) (0, 0) (storePoint store0)
        , storeFloat = if replaced then IM.delete contentCacheKey (storeFloat store0) else storeFloat store0
        , storeDyn = if replaced then IM.delete (slotKey slotTextAreaBuffer key) (storeDyn store0) else storeDyn store0
        }
  store <- uiIO (getStore ctx)
  let current = IM.findWithDefault value key (storeText store)
      -- Set by menu actions (cut/paste through applyTextAreaMenuAction) whose
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
            (vw, vh) = IM.findWithDefault (200, 96) (slotKey slotTextAreaViewport key) (storePoint store)
        newState <- uiIO (processTextArea ctx inp (realToFrac vw) (realToFrac vh) (realToFrac (fmLineHeight editFm)) oldState)
        let newText
              -- 'processTextArea' only edits text when this frame carried keys
              -- or chars, so idle focused frames skip the O(document)
              -- 'TB.toText' and stop at the cheap cursor/scroll checks.
              | hadInput || changed = TB.toText (buffer newState)
              | otherwise = current
            hadInput = not (T.null (inputChars inp)) || not (inputKeysNull (inputKeys inp))
            changed =
              cursorOf newState /= cursorOf oldState
                || selectionAnchor newState /= selectionAnchor oldState
                || scrollOffset newState /= scrollOffset oldState
                || menuPulse
                || (hadInput && newText /= current)
        -- Saving writes the new text and its buffer together; drop only the
        -- content size measured for the old text, and the menu pulse.
        when changed $
          uiIO $ do
            st <- saveTextAreaState key newState <$> getStore ctx
            setStore ctx st
              { storeText = IM.insert seenKey newText (storeText st)
              , storeInt = IM.delete changedSlotKey (storeInt st)
              , storeFloat = IM.delete contentCacheKey (storeFloat st)
              }
        pure (newText, changed)
      else pure (current, False)
  resp <- addWidget wid NodeTextArea "" 0 layout
  pure (setChanged stateChanged resp, newText)

loadTextAreaState :: WidgetStore -> Int -> Text -> TextAreaState
loadTextAreaState store key initial =
  let text = IM.findWithDefault initial key (storeText store)
      -- The buffer cache is written together with storeText by
      -- saveTextAreaState, so a present entry is always the buffer for the
      -- stored text; no (O(document)) re-comparison is needed.
      cachedBuffer :: Maybe TB.TextBuffer =
        IM.lookup (slotKey slotTextAreaBuffer key) (storeDyn store) >>= fromDynamic
      buf0 = case cachedBuffer of
        Just cached -> cached
        Nothing -> TB.fromText text
   in loadTextAreaStateWithBuffer store key text buf0

-- | 'loadTextAreaState' with the buffer already resolved (the paint path
-- ensures the buffer cache and hands it straight through, avoiding a second
-- store lookup).
loadTextAreaStateWithBuffer :: WidgetStore -> Int -> Text -> TB.TextBuffer -> TextAreaState
loadTextAreaStateWithBuffer store key text buf0 =
  let row = IM.findWithDefault 0 (slotKey slotTextAreaRow key) (storeInt store)
      col = IM.findWithDefault 0 (slotKey slotTextAreaCol key) (storeInt store)
      anchorRow = IM.findWithDefault row (slotKey slotTextAreaAnchorRow key) (storeInt store)
      anchorCol = IM.findWithDefault col (slotKey slotTextAreaAnchorCol key) (storeInt store)
      pref = IM.findWithDefault col (slotKey slotTextAreaPrefCol key) (storeInt store)
      scroll =
        let (sx, sy) =
              IM.findWithDefault (0, 0) (slotKey slotTextAreaScroll key) (storePoint store)
         in (realToFrac sx, realToFrac sy)
      viewport =
        let (vw, vh) =
              IM.findWithDefault (200, 96) (slotKey slotTextAreaViewport key) (storePoint store)
         in (realToFrac vw, realToFrac vh)
      buf =
        let b = TB.withCursor (TB.Cursor row col) buf0
         in b {TB.preferredCol = pref}
      anchor = TB.getCursor (TB.withCursor (TB.Cursor anchorRow anchorCol) buf0)
   in (initTextAreaState text)
     { buffer = buf
     , selectionAnchor = anchor
     , scrollOffset = scroll
     , viewportSize = viewport
     }

saveTextAreaState :: Int -> TextAreaState -> WidgetStore -> WidgetStore
saveTextAreaState key state store =
  let TB.Cursor row col = TB.getCursor (buffer state)
      TB.Cursor anchorRow anchorCol = selectionAnchor state
   in store
        { storeText = IM.insert key (TB.toText (buffer state)) (storeText store)
        , storeDyn =
            IM.insert (slotKey slotTextAreaBuffer key) (toDyn (buffer state)) (storeDyn store)
        , storeInt =
            IM.insert (slotKey slotTextAreaRow key) row $
              IM.insert (slotKey slotTextAreaCol key) col $
                IM.insert (slotKey slotTextAreaPrefCol key) (TB.preferredCol (buffer state)) $
                  IM.insert (slotKey slotTextAreaAnchorRow key) anchorRow $
                    IM.insert (slotKey slotTextAreaAnchorCol key) anchorCol (storeInt store)
        , storePoint =
            IM.insert (slotKey slotTextAreaScroll key) (realToFrac sx, realToFrac sy) $
              IM.insert (slotKey slotTextAreaViewport key) (realToFrac vw, realToFrac vh) (storePoint store)
        }
  where
    (sx, sy) = scrollOffset state
    (vw, vh) = viewportSize state

textAreaCopy :: Context -> TextAreaState -> IO ()
textAreaCopy ctx state = copyBufferText ctx (selectionAnchor state) (buffer state)

textAreaCut :: Context -> TextAreaState -> IO TextAreaState
textAreaCut ctx state = do
  buf' <- cutBufferText ctx (selectionAnchor state) (buffer state)
  let cur = TB.getCursor buf'
  pure (ensureCaretVisible (clearSelection state {buffer = buf', selectionAnchor = cur}))

textAreaPaste :: Context -> TextAreaState -> IO TextAreaState
textAreaPaste ctx state = do
  mbuf' <- pasteBufferText ctx True (selectionAnchor state) (buffer state)
  case mbuf' of
    Nothing -> pure state
    Just buf' -> do
      let cur = TB.getCursor buf'
      pure (ensureCaretVisible state {buffer = buf', selectionAnchor = cur})

applyTextAreaMenuAction :: Context -> WidgetId -> MenuAction -> IO ()
applyTextAreaMenuAction ctx wid action = do
  store <- getStore ctx
  let key = intKey wid
      text = IM.findWithDefault "" key (storeText store)
      s0 = loadTextAreaState store key text
  s1 <- dispatchMenuAction (textAreaCut ctx) (textAreaCopy ctx) (textAreaPaste ctx) selectAllTextArea action s0
  -- The pulse flag signals the next text-area frame that its text changed
  -- outside Input, so the caller still gets a respChanged pulse. Gated on an
  -- actual text delta: selection-only actions (Select All, Copy) must not
  -- pulse.
  let setChangedFlag =
        if TB.toText (buffer s0) == TB.toText (buffer s1)
          then saveTextAreaState key s1 store
          else saveTextAreaState key s1 store {storeInt = IM.insert (slotKey slotTextAreaChanged key) 1 (storeInt store)}
  setStore ctx setChangedFlag
  -- Menu actions are how a caller edits a field that may not be under the
  -- pointer; focus it so the selection highlight and caret become visible.
  writeIORef (ctxFocusId ctx) wid
  setTextInputMenu ctx Nothing
  markDirty ctx

processTextArea :: Context -> Input -> Double -> Double -> Double -> TextAreaState -> IO TextAreaState
processTextArea ctx inp vpW vpH lineH s0 = do
  let mods = inputModifiers inp
      s1 = setTextAreaViewport (vpW, vpH) lineH s0
      ctrl = modCtrl mods
  when (not (T.null (inputChars inp)) || not (inputKeysNull (inputKeys inp))) $
    setTextInputDrag ctx Nothing
  s2 <-
    if ctrl
      then T.foldlM' (handleCtrlChar ctx) s1 (inputChars inp)
      else pure s1
  let typed = T.filter (\ch -> not (isCtrlCombo ctrl ch) && isPrint ch) (inputChars inp)
      s3 = T.foldl' (\s ch -> handleTextAreaEvent (TAChar ch) mods s) s2 typed
  pure (foldInputKeys (\s k -> handleTextAreaEvent (TAKey k) mods s) s3 (inputKeys inp))

handleCtrlChar :: Context -> TextAreaState -> Char -> IO TextAreaState
handleCtrlChar ctx =
  dispatchCtrlChar
    (pure . selectAllTextArea)
    (textAreaCopy ctx)
    (textAreaCut ctx)
    (textAreaPaste ctx)
