-- | Single-line text fields: field geometry, horizontal scroll, caret and
-- selection painting, and mouse selection. Also holds the line painting, word
-- bounds and mouse selection the text area shares.
module NanoUI.Internal.Frame.TextInput
  ( textInputFieldRect
  , textInputFieldTextClip
  , nodeTextFieldGeom
  , tagTextInputClippedSpans
  , syncTextInputScroll
  , FieldEdit
  , readFieldEdit
  , drawTextInputSelection
  , drawTextInputCaret
  , drawLineSelection
  , drawLineCaret
  , searchClearHit
  , textWordBounds
  , FieldDoc (..)
  , selectWithMouse
  , textInputMouse
  ) where

import Control.Monad (forM_, when)
import Data.Char (isAlphaNum, isSpace)
import Data.Maybe (mapMaybe)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import qualified Data.Text as T
import NanoUI.Internal.Context
  ( Context (..)
  , TextFieldClickCell (..)
  , TextInputDrag (..)
  , getStore
  , intKey
  , markDirty
  , modifyStore
  , setStore
  , writeSlots
  , Slot (..)
  , slotKey
  , nodeTheme
  , InteractionState (..)
  , getsInteraction
  , modifyInteraction
  )
import NanoUI.Internal.Draw (DrawArena, pushRect)
import NanoUI.Internal.Font (FontMetrics (..), caretXIO, centeredTextY, lineWidthIO, prepareFontMetrics, selectionSpans, textIndexAtX, widgetContentInset)
import NanoUI.Internal.Frame.Chrome (textInputFocused, textInputValue)
import NanoUI.Internal.Frame.Hit (withWidgetNode)
import NanoUI.Internal.Frame.Node (nodeFontMetrics)
import NanoUI.Internal.Frame.Scroll.Geometry (padTextClipRect)
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Internal.Input
  ( Input (..)
  , inputMouseClicks
  , inputMouseDown
  , inputMousePos
  , inputMousePressed
  , inputMouseReleased
  )
import NanoUI.Internal.Layout.Arena (NodeIdx, getOptions, getRect, getStyleIdx, getWidgetId)
import NanoUI.Internal.Monad (ifM, unlessM, (<&&>))
import NanoUI.Internal.Store (fieldFloat, fieldInt, fieldText, findSlot, insertSlot, slotWriteOr)
import NanoUI.Internal.Style (themeSelection)
import NanoUI.Internal.Types (Color (..), Rect (..), V2 (..), clamp, rectContains, rectIntersect, rectOverlapArea, rectW)
import NanoUI.Internal.WidgetText
  ( hasFlag
  , comboTextClip
  , numericTextClip
  , searchInputIconRects
  , searchInputTextClip
  , textClipBetween
  , textInputFlagNumeric
  , textInputFieldHeight
  , textInputFlagSearch
  , textInputFlagSelectable
  )
import NanoUI.Widgets.TextBuffer qualified as TB

textInputFieldRect :: FontMetrics -> Float -> Float -> Float -> Float -> Rect
textInputFieldRect fm x y w h =
  let fieldH = if h > 0 then h else textInputFieldHeight fm
   in Rect x y w fieldH

textInputFieldTextClip :: FontMetrics -> Rect -> Rect
textInputFieldTextClip fm (Rect fx fy fw fh) =
  let (ix, _) = widgetContentInset fm
   in textClipBetween fm ix ix fx fy fw fh

-- | Resolve the box a field paints/hits and the clip its text is confined to.
-- Search fields are caption-less: the whole node rect is the box and text is
-- clipped around the magnifier / clear chrome. Combo boxes (search fields
-- carrying dropdown options) clip to the left of the chevron instead.
nodeTextFieldGeom :: Context -> NodeIdx -> Float -> Float -> Float -> Float -> IO (Rect, Rect)
nodeTextFieldGeom ctx idx x y w h = do
  si <- getStyleIdx (ctxNodeArena ctx) idx
  opts <- getOptions (ctxNodeArena ctx) idx
  let fm = ctxFontMetrics ctx
      box = Rect x y w h
      field = textInputFieldRect fm x y w h
      geom
        | hasFlag textInputFlagSelectable si = (box, box)
        | hasFlag textInputFlagNumeric si = (box, numericTextClip fm x y w h)
        | hasFlag textInputFlagSearch si =
            (box, if null opts then searchInputTextClip fm x y w h else comboTextClip fm x y w h)
        | otherwise = (field, textInputFieldTextClip fm field)
  pure geom

-- | Whether the pointer is over the clear (×) button of a non-empty search
-- field. Search fields reserve that slot even when empty, but the button is
-- only active when there is text to clear.
searchClearHit :: Context -> WidgetId -> V2 -> IO Bool
searchClearHit ctx wid mouse = do
  withWidgetNode ctx wid False $ \idx -> do
    si <- getStyleIdx (ctxNodeArena ctx) idx
    opts <- getOptions (ctxNodeArena ctx) idx
    pure (hasFlag textInputFlagSearch si && null opts)
      <&&> (not . T.null <$> textInputValue ctx idx)
      <&&> do
        (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
        let (_, clearRect) = searchInputIconRects (ctxFontMetrics ctx) x y w h
        pure (rectContains clearRect mouse)

-- | Clear a search field. The debounced pulse picks the empty text up as an
-- immediate (empty) commit on the next frame.
clearSearchInput :: Context -> WidgetId -> IO ()
clearSearchInput ctx wid = do
  let key = intKey wid
  modifyStore ctx $
    insertSlot fieldText key ""
      . insertSlot fieldInt (slotKey SlotAnchor key) 0
      . insertSlot fieldInt (slotKey SlotCursor key) 0
  markDirty ctx

tagTextInputClippedSpans ::
  Rect -> Float -> Float -> Float -> Float -> FontMetrics -> [(Rect, T.Text, Color, Color)] -> [(Rect, T.Text, Color, Color, Rect)]
tagTextInputClippedSpans parentClip x y w h fm spans =
  let fieldClip = textInputFieldTextClip fm (textInputFieldRect fm x y w h)
      labelClip = Rect x y w (fmLineHeight fm)
      tagOne (rect, txt, fg, bg) =
        let clipRect = padTextClipRect rect
            isField = rectOverlapArea fieldClip clipRect > rectOverlapArea labelClip clipRect
            area = if isField then fieldClip else labelClip
         in (rect, txt, fg, bg,) <$> (rectIntersect area clipRect >>= rectIntersect parentClip)
   in mapMaybe tagOne spans

-- | The selection highlight behind characters @lo@ to @hi@ of @line@, whose
-- pen starts at @x@, on a row at @y@ of height @lineH@.
drawLineSelection :: DrawArena -> FontMetrics -> Text -> Int -> Int -> Float -> Float -> Float -> Color -> IO ()
drawLineSelection da fm line lo hi x y lineH color = do
  prepared <- prepareFontMetrics fm line
  forM_ (selectionSpans prepared line lo hi) $ \(wLo, wHi) ->
    when (wHi > wLo) $
      pushRect da (Rect (x + wLo) y (max 1 (wHi - wLo)) (max 4 lineH)) color

-- | The caret before character @col@ of @line@, whose pen starts at @x@, on a
-- row at @y@ of height @lineH@.
drawLineCaret :: DrawArena -> FontMetrics -> Text -> Int -> Float -> Float -> Float -> Color -> IO ()
drawLineCaret da fm line col x y lineH fg = do
  pw <- caretXIO fm line col
  pushRect da (Rect (x + pw) (y + 1) 1 (max 4 (lineH - 2))) fg

computeTextInputScroll :: FontMetrics -> Float -> Text -> Int -> Float -> Bool -> IO Float
computeTextInputScroll fm viewportW value cursor oldScroll isFocused
  | not isFocused = pure 0
  | viewportW <= 0 = pure 0
  | otherwise = do
      caretRelX <- caretXIO fm value cursor
      totalTextW <- lineWidthIO fm value
      let maxScroll = max 0 (totalTextW + 1 - viewportW)
          s0
            | caretRelX < oldScroll = caretRelX
            | caretRelX + 1 > oldScroll + viewportW = caretRelX + 1 - viewportW
            | otherwise = oldScroll
      pure (clamp 0 maxScroll s0)

syncTextInputScroll :: Context -> NodeIdx -> Float -> Float -> Float -> Float -> IO Float
syncTextInputScroll ctx idx x y w h = do
  si <- getStyleIdx (ctxNodeArena ctx) idx
  if hasFlag textInputFlagSelectable si
    then pure 0
    else do
      wid <- getWidgetId (ctxNodeArena ctx) idx
      store <- getStore ctx
      let key = intKey wid
      value <- textInputValue ctx idx
      focus <- textInputFocused ctx idx
      (_, clip) <- nodeTextFieldGeom ctx idx x y w h
      let cursor = findSlot fieldInt (T.length value) (slotKey SlotCursor key) store
          oldScroll = findSlot fieldFloat 0 (slotKey SlotTextInputScroll key) store
      newScroll <- computeTextInputScroll (ctxFontMetrics ctx) (rectW clip) value cursor oldScroll focus
      when (newScroll /= oldScroll) $
        setStore ctx (insertSlot fieldFloat (slotKey SlotTextInputScroll key) newScroll store)
      pure newScroll

-- | What a focused single-line field paints its selection and caret from: the
-- displayed value, cursor and anchor, the node font, the field box's top and
-- height, and the x its text starts at with the scroll applied.
data FieldEdit = FieldEdit !Text !Int !Int !FontMetrics !Float !Float !Float

-- | Editing state of field @idx@ at @x y w h@ scrolled by @scrollX@ (see
-- 'syncTextInputScroll'), or Nothing while it is unfocused.
readFieldEdit :: Context -> NodeIdx -> Float -> Float -> Float -> Float -> Float -> IO (Maybe FieldEdit)
readFieldEdit ctx idx x y w h scrollX = do
  focus <- textInputFocused ctx idx
  if not focus
    then pure Nothing
    else do
      value <- textInputValue ctx idx
      wid <- getWidgetId (ctxNodeArena ctx) idx
      store <- getStore ctx
      (Rect _ boxY _ boxH, Rect clipX _ _ _) <- nodeTextFieldGeom ctx idx x y w h
      fm <- nodeFontMetrics ctx idx
      let key = intKey wid
          !cursor = findSlot fieldInt (T.length value) (slotKey SlotCursor key) store
          !anchor = findSlot fieldInt cursor (slotKey SlotAnchor key) store
      pure $! Just (FieldEdit value cursor anchor fm boxY boxH (clipX - scrollX))

drawTextInputSelection :: DrawArena -> Context -> NodeIdx -> FieldEdit -> IO ()
drawTextInputSelection da ctx idx (FieldEdit value cursor anchor fm boxY boxH textX) =
  when (anchor /= cursor) $ do
    theme <- nodeTheme ctx idx
    let lineH = fmLineHeight fm
    drawLineSelection da fm value (min anchor cursor) (max anchor cursor) textX (centeredTextY fm boxY boxH lineH) lineH (themeSelection theme)

drawTextInputCaret :: DrawArena -> FieldEdit -> Color -> IO ()
drawTextInputCaret da (FieldEdit value cursor _ fm boxY boxH textX) =
  let lineH = fmLineHeight fm
   in drawLineCaret da fm value cursor textX (centeredTextY fm boxY boxH lineH) lineH

data CharClass = WordChar | SpaceChar | OtherChar
  deriving (Eq)

-- | The run of word characters (letters, digits, underscores), spaces or
-- other characters around character @raw@ of @text@, clamped into it.
-- Positions count characters, not UTF-8 bytes.
textWordBounds :: Text -> Int -> (Int, Int)
textWordBounds text raw
  | T.null text = (0, 0)
  | otherwise =
      -- Split once: repeatedly indexing UTF-8 text makes long-word selection
      -- quadratic. The clamped index guarantees a non-empty suffix.
      let i = clamp 0 (T.length text - 1) raw
          (before, after) = T.splitAt i text
          sameClass = (== charClass (T.head after)) . charClass
       in ( i - T.length (T.takeWhileEnd sameClass before)
          , i + T.length (T.takeWhile sameClass after)
          )
 where
  charClass c
    | isAlphaNum c || c == '_' = WordChar
    | isSpace c = SpaceChar
    | otherwise = OtherChar

-- | A field's document at the pointer: the position under it, the document,
-- and how the field stores a selection (anchor, then cursor).
data FieldDoc = FieldDoc !TB.Cursor !TB.TextBuffer (TB.Cursor -> TB.Cursor -> IO ())

-- | Mouse selection in focused field @wid@: a press in @box@, with word and
-- whole-document multi-clicks, and the drag it starts. On a press
-- @chromePress@ runs first and takes the press when it lands on the field's
-- own chrome. @atMouse@ reads the document at the pointer.
selectWithMouse :: Context -> Input -> WidgetId -> Rect -> IO Bool -> IO FieldDoc -> IO ()
selectWithMouse ctx inp wid box chromePress atMouse
  | inputMousePressed inp && rectContains box (inputMousePos inp) =
      unlessM chromePress $ do
        FieldDoc pos buf select <- atMouse
        -- A press counts as a multi-click only on the cell of the press before.
        prev <- getsInteraction ctx isTextFieldClickCell
        let cell = Just (TextFieldClickCell wid pos)
            clicks = if prev == cell then max 1 (inputMouseClicks inp) else 1
        uncurry select (dragSelection buf pos pos clicks)
        modifyInteraction ctx $ \s ->
          s {isTextFieldClickCell = cell, isTextInputDrag = Just (TextInputDrag wid pos clicks)}
  | inputMouseDown inp || inputMouseReleased inp =
      getsInteraction ctx isTextInputDrag >>= \case
        Just (TextInputDrag dragWid anchor clicks) | dragWid == wid -> do
          FieldDoc pos buf select <- atMouse
          uncurry select (dragSelection buf anchor pos clicks)
        _ -> pure ()
  | otherwise = pure ()

-- | The selection a drag from @anchor@ to @pos@ makes after @clicks@ clicks:
-- characters, whole words, or the whole document. A click is a drag that has
-- not moved.
dragSelection :: TB.TextBuffer -> TB.Cursor -> TB.Cursor -> Int -> (TB.Cursor, TB.Cursor)
dragSelection buf anchor@(TB.Cursor ar ac) pos@(TB.Cursor r c) clicks
  | clicks >= 3 = (TB.Cursor 0 0, TB.documentEnd buf)
  | clicks == 2 =
      let (a0, a1) = textWordBounds (TB.lineAt ar buf) ac
          (c0, c1) = textWordBounds (TB.lineAt r buf) c
       in (TB.Cursor ar (min a0 c0), TB.Cursor r (max a1 c1))
  | otherwise = (anchor, pos)

-- | Mouse selection in single-line field @wid@ at @idx@ ('selectWithMouse'),
-- and a search field's clear button.
textInputMouse :: Context -> Input -> WidgetId -> NodeIdx -> IO ()
textInputMouse ctx inp wid idx = do
  (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
  (box, Rect clipX _ _ _) <- nodeTextFieldGeom ctx idx x y w h
  scrollX <- syncTextInputScroll ctx idx x y w h
  let mouse@(V2 mouseX _) = inputMousePos inp
      key = intKey wid
      clearPress = ifM (searchClearHit ctx wid mouse) (True <$ clearSearchInput ctx wid) (pure False)
  selectWithMouse ctx inp wid box clearPress $ do
    fm <- nodeFontMetrics ctx idx
    value <- textInputValue ctx idx
    prepared <- prepareFontMetrics fm value
    let pos = TB.Cursor 0 (textIndexAtX prepared value (max 0 (mouseX - (clipX - scrollX))))
    pure $ FieldDoc pos (TB.fromLines (Seq.singleton value)) $ \(TB.Cursor _ anchor) (TB.Cursor _ cursor) ->
      writeSlots ctx $
        slotWriteOr fieldInt cursor (slotKey SlotAnchor key) anchor
          <> slotWriteOr fieldInt 0 (slotKey SlotCursor key) cursor
