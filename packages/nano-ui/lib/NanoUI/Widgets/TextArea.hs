{-# LANGUAGE BangPatterns #-}

module NanoUI.Widgets.TextArea
  ( -- * Pure state
    TextAreaState (..)
  , initTextAreaState
  , setTextAreaViewport
  , KeyInput (..)
  , Modifiers (..)
  , handleTextAreaEvent
  , setTextAreaCursor
  , setTextAreaSelection
  , TextAreaLayout (..)
  , VisualLine (..)
  , computeTextAreaLayout
    -- * Widget glue
  , textAreaLayout
  , processTextArea
  , loadTextAreaState
  , saveTextAreaState
  , applyTextAreaMenuAction
  , textAreaMenuActionEnabled
  ) where

import Control.Monad (when)
import Data.Char (isPrint, toLower)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.IntMap.Strict as IM
import NanoUI.Context
  ( Context (..)
  , getStore
  , intKey
  , markDirty
  , setStore
  , setTextInputDrag
  , setTextInputMenu
  )
import NanoUI.Id (WidgetId)
import NanoUI.Input
  ( Input (..)
  , Key
  , foldInputKeys
  , inputChars
  , inputKeys
  , inputKeysNull
  , inputModifiers
  )
import qualified NanoUI.Input as Inp
import NanoUI.Store
  ( WidgetStore (..)
  , slotKey
  , slotTextAreaAnchorCol
  , slotTextAreaAnchorRow
  , slotTextAreaCol
  , slotTextAreaPrefCol
  , slotTextAreaRow
  , slotTextAreaScroll
  , slotTextAreaViewport
  )
import NanoUI.Style (Layout (..), Sizing (..), defaultLayout)
import NanoUI.Widgets.TextBuffer as TB
import NanoUI.Widgets.TextCommon
  ( copyBufferText
  , cutBufferText
  , dispatchCtrlChar
  , dispatchMenuAction
  , isCtrlCombo
  , menuActionEnabled
  , pasteBufferText
  )

data Modifiers = Modifiers
  { modShift :: !Bool
  , modCtrl :: !Bool
  , modAlt :: !Bool
  , modSuper :: !Bool
  }
  deriving (Eq, Show)

data KeyInput
  = KeyChar !Char
  | KeyEnter
  | KeyBackspace
  | KeyDelete
  | KeyLeft
  | KeyRight
  | KeyUp
  | KeyDown
  | KeyHome
  | KeyEnd
  | KeyPageUp
  | KeyPageDown
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
  ensureCaretVisible state {viewportSize = vp, lineHeight = lh}

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

setTextAreaCursor :: Int -> Int -> TextAreaState -> TextAreaState
setTextAreaCursor row col state =
  let buf =
        let b = TB.withCursor (TB.Cursor row col) (buffer state)
         in b {TB.preferredCol = col}
   in ensureCaretVisible (clearSelection state {buffer = buf})

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
  case selectionRangeOf state of
    Nothing ->
      let buf' = TB.insertChar ch (buffer state)
          cur = TB.getCursor buf'
       in ensureCaretVisible state {buffer = buf', selectionAnchor = cur}
    Just (lo, hi) ->
      let buf' = TB.replaceRange (T.singleton ch) lo hi (buffer state)
          cur = TB.getCursor buf'
       in ensureCaretVisible state {buffer = buf', selectionAnchor = cur}

handleTextAreaEvent :: KeyInput -> Modifiers -> TextAreaState -> TextAreaState
handleTextAreaEvent key mods state =
  let shift = modShift mods
      ctrl = modCtrl mods || modSuper mods
      alt = modAlt mods
      n = pageLineCount state
      state' = case (key, ctrl, alt) of
        (KeyChar c, False, False) -> insertWithSelection c state
        (KeyEnter, False, False) ->
          case selectionRangeOf state of
            Nothing ->
              let buf' = TB.breakLine (buffer state)
                  cur = TB.getCursor buf'
               in ensureCaretVisible state {buffer = buf', selectionAnchor = cur}
            Just (lo, hi) ->
              let buf' = TB.replaceRange "\n" lo hi (buffer state)
                  cur = TB.getCursor buf'
               in ensureCaretVisible state {buffer = buf', selectionAnchor = cur}
        (KeyBackspace, False, False) ->
          case selectionRangeOf state of
            Just _ -> deleteSelection state
            Nothing -> moveCursor False TB.deletePrevChar state
        (KeyBackspace, True, _) -> moveCursor shift TB.deletePrevWord state
        (KeyBackspace, _, True) -> moveCursor shift TB.deletePrevWord state
        (KeyDelete, False, False) ->
          case selectionRangeOf state of
            Just _ -> deleteSelection state
            Nothing -> moveCursor False TB.deleteChar state
        (KeyDelete, True, _) -> moveCursor shift TB.deleteNextWord state
        (KeyLeft, False, False) -> moveCursor shift TB.moveLeft state
        (KeyLeft, True, _) -> moveCursor shift TB.moveWordLeft state
        (KeyRight, False, False) -> moveCursor shift TB.moveRight state
        (KeyRight, True, _) -> moveCursor shift TB.moveWordRight state
        (KeyUp, False, False) -> moveCursor shift TB.moveUp state
        (KeyDown, False, False) -> moveCursor shift TB.moveDown state
        (KeyHome, False, False) -> moveCursor shift TB.moveToBOL state
        (KeyEnd, False, False) -> moveCursor shift TB.moveToEOL state
        (KeyPageUp, False, False) -> moveCursor shift (applyN n TB.moveUp) state
        (KeyPageDown, False, False) -> moveCursor shift (applyN n TB.moveDown) state
        (KeyChar c, True, False)
          | toLower c == 'k' || c == '\v' -> moveCursor shift TB.killToEOL state
          | toLower c == 'u' || c == '\NAK' -> moveCursor shift TB.killToBOL state
          | toLower c == 'a' || c == '\x01' -> selectAllTextArea state
          | toLower c == 'e' || c == '\ENQ' -> moveCursor shift TB.moveToEOL state
          | otherwise -> state
        _ -> state
   in ensureCaretVisible state'

selectAllTextArea :: TextAreaState -> TextAreaState
selectAllTextArea state =
  let end = TB.documentEnd (buffer state)
   in setTextAreaSelection (TB.Cursor 0 0) end state

pageLineCount :: TextAreaState -> Int
pageLineCount state =
  let h = snd (viewportSize state)
      lh = lineHeight state
  in
    if h <= 0 || lh <= 0
      then 1
      else max 1 (floor (h / lh))

applyN :: Int -> (a -> a) -> a -> a
applyN n f x = foldl' (\acc _ -> f acc) x [1 .. n]

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
  in state {scrollOffset = (sx, clampDouble 0 maxSy sy')}

clampDouble :: Double -> Double -> Double -> Double
clampDouble lo hi x = max lo (min hi x)

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
      currentLineText =
        if r < length linesList
          then linesList !! r
          else ""
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
-- Widget glue
--------------------------------------------------------------------------------

textAreaLayout :: Layout
textAreaLayout =
  defaultLayout
    { layoutWidth = Grow 1
    , layoutMinW = 200
    , layoutHeight = Fixed 140
    }

loadTextAreaState :: WidgetStore -> Int -> Text -> TextAreaState
loadTextAreaState store key initial =
  let text = IM.findWithDefault initial key (storeText store)
      row = IM.findWithDefault 0 (slotKey slotTextAreaRow key) (storeInt store)
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
      buf0 = TB.fromText text
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

textAreaMenuActionEnabled :: Context -> WidgetId -> Int -> IO Bool
textAreaMenuActionEnabled ctx wid item = do
  store <- getStore ctx
  let key = intKey wid
      text = IM.findWithDefault "" key (storeText store)
  mclip <- ctxClipboardGet ctx
  pure (menuActionEnabled (not (T.null text)) mclip item)

applyTextAreaMenuAction :: Context -> WidgetId -> Int -> IO ()
applyTextAreaMenuAction ctx wid item = do
  store <- getStore ctx
  let key = intKey wid
      text = IM.findWithDefault "" key (storeText store)
      s0 = loadTextAreaState store key text
  s1 <- dispatchMenuAction (textAreaCut ctx) (textAreaCopy ctx) (textAreaPaste ctx) selectAllTextArea item s0
  setStore ctx (saveTextAreaState key s1 store)
  setTextInputMenu ctx Nothing
  markDirty ctx

processTextArea :: Context -> Input -> Double -> Double -> Double -> TextAreaState -> IO TextAreaState
processTextArea ctx inp vpW vpH lineH s0 = do
  let mods =
        Modifiers
          { modShift = Inp.modShift (inputModifiers inp)
          , modCtrl = Inp.modCtrl (inputModifiers inp)
          , modAlt = Inp.modAlt (inputModifiers inp)
          , modSuper = False
          }
      s1 = setTextAreaViewport (vpW, vpH) lineH s0
      ctrl = Inp.modCtrl (inputModifiers inp)
  when (not (T.null (inputChars inp)) || not (inputKeysNull (inputKeys inp))) $
    setTextInputDrag ctx Nothing
  s2 <-
    if ctrl
      then T.foldlM' (handleCtrlChar ctx) s1 (inputChars inp)
      else pure s1
  let filtered = T.filter (\ch -> not (isCtrlCombo ctrl ch) && isPrint ch) (inputChars inp)
      s3 = T.foldl' (\s ch -> handleTextAreaEvent (KeyChar ch) mods s) s2 filtered
  pure
    ( foldInputKeys
        ( \s k -> maybe s (\ki -> handleTextAreaEvent ki mods s) (mapKey k)
        )
        s3
        (inputKeys inp)
    )

handleCtrlChar :: Context -> TextAreaState -> Char -> IO TextAreaState
handleCtrlChar ctx =
  dispatchCtrlChar
    (pure . selectAllTextArea)
    (textAreaCopy ctx)
    (textAreaCut ctx)
    (textAreaPaste ctx)

mapKey :: Key -> Maybe KeyInput
mapKey = \case
  Inp.KeyBackspace -> Just KeyBackspace
  Inp.KeyDelete -> Just KeyDelete
  Inp.KeyEnter -> Just KeyEnter
  Inp.KeyLeft -> Just KeyLeft
  Inp.KeyRight -> Just KeyRight
  Inp.KeyUp -> Just KeyUp
  Inp.KeyDown -> Just KeyDown
  Inp.KeyHome -> Just KeyHome
  Inp.KeyEnd -> Just KeyEnd
  _ -> Nothing
