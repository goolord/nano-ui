-- | Single-line text fields: field geometry, horizontal scroll, caret and
-- selection painting, and mouse selection. Also holds the click-count and
-- caret primitives the text area shares.
module NanoUI.Internal.Frame.TextInput
  ( textInputFieldRect
  , textInputFieldTextClip
  , nodeTextFieldGeom
  , tagTextInputClippedSpans
  , syncTextInputScroll
  , FieldEdit
  , readFieldEdit
  , textInputScroll
  , drawTextInputSelection
  , drawTextInputCaret
  , drawTextCaret
  , drawTextSelectionLine
  , searchClearHit
  , normalizeTextFieldClicks
  , finalizeTextInputMouse
  , collapseTextInputSelection
  ) where

import Control.Monad (forM_, when)
import Data.Maybe (mapMaybe)
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
  , setTextInputDrag
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
import NanoUI.Internal.Layout.Arena
  ( NodeIdx
  , NodeType (NodeTextInput)
  , getNodeType
  , getOptions
  , getRect
  , getStyleIdx
  , getWidgetId
  )
import NanoUI.Internal.Monad (ifM, (<&&>))
import NanoUI.Internal.Store (fieldFloat, fieldInt, fieldText, findSlot, insertSlot, slotWriteOr)
import NanoUI.Internal.Style (themeSelection)
import NanoUI.Internal.Types (Color (..), Rect (..), V2 (..), clamp, rectContains, rectIntersect, rectOverlapArea, rectW)
import NanoUI.Internal.WidgetText
  ( hasFlag
  , comboTextClip
  , numericTextClip
  , searchInputIconRects
  , searchInputTextClip
  , textInputFlagNumeric
  , textInputFieldHeight
  , textInputFlagSearch
  , textInputFlagSelectable
  )
import NanoUI.Internal.Widgets.TextCommon
  ( selectionCaretGeom
  , textSelectionForDrag
  )

textInputFieldRect :: FontMetrics -> Float -> Float -> Float -> Float -> Rect
textInputFieldRect fm x y w h =
  let fieldH = if h > 0 then h else textInputFieldHeight fm
   in Rect x y w fieldH

textInputFieldTextClip :: FontMetrics -> Rect -> Rect
textInputFieldTextClip fm (Rect fx fy fw fh) =
  let (ix, iy) = widgetContentInset fm
   in Rect (fx + ix) (fy + iy) (max 0 (fw - 2 * ix)) (max 0 (fh - 2 * iy))

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

drawTextCaret :: DrawArena -> Float -> Float -> Float -> Color -> IO ()
drawTextCaret da caretX caretY caretH fg =
  pushRect da (Rect caretX caretY 1 caretH) fg

drawTextSelectionLine :: DrawArena -> Float -> Float -> Float -> Float -> Color -> IO ()
drawTextSelectionLine da selX selY selW selH selBg =
  when (selW > 0) $
    pushRect da (Rect selX selY (max 1 selW) (max 4 selH)) selBg

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

-- | The scroll 'syncTextInputScroll' last settled for a field.
textInputScroll :: Context -> NodeIdx -> IO Float
textInputScroll ctx idx = do
  wid <- getWidgetId (ctxNodeArena ctx) idx
  findSlot fieldFloat 0 (slotKey SlotTextInputScroll (intKey wid)) <$> getStore ctx

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
drawTextInputSelection da ctx idx (FieldEdit value cursor anchor fm boxY boxH textX) = do
  let selLo = min anchor cursor
      selHi = max anchor cursor
      lineH = fmLineHeight fm
  when (selLo < selHi) $ do
    theme <- nodeTheme ctx idx
    prepared <- prepareFontMetrics fm value
    forM_ (selectionSpans prepared value selLo selHi) $ \(wLo, wHi) ->
      drawTextSelectionLine
        da
        (textX + wLo)
        (centeredTextY fm boxY boxH lineH)
        (wHi - wLo)
        lineH
        (themeSelection theme)

drawTextInputCaret :: DrawArena -> FieldEdit -> Color -> IO ()
drawTextInputCaret da (FieldEdit value cursor _ fm boxY boxH textX) fg = do
  let lineH = fmLineHeight fm
  pw <- caretXIO fm value cursor
  let (caretX, caretY, caretH) =
        selectionCaretGeom textX (centeredTextY fm boxY boxH lineH) pw lineH
  drawTextCaret da caretX caretY caretH fg

updateTextInputSelection :: Context -> WidgetId -> Int -> Int -> IO ()
updateTextInputSelection ctx wid anchor cursor =
  writeSlots ctx $
    slotWriteOr fieldInt cursor (slotKey SlotAnchor key) anchor
      <> slotWriteOr fieldInt 0 (slotKey SlotCursor key) cursor
  where
    key = intKey wid

-- | Field box, text origin x (scroll applied), value and font of a single-line
-- field.
textInputGeomForWidget :: Context -> WidgetId -> IO (Maybe (Rect, Float, Text, FontMetrics))
textInputGeomForWidget ctx wid = do
  withWidgetNode ctx wid Nothing $ \idx -> do
    nt <- getNodeType (ctxNodeArena ctx) idx
    if nt /= NodeTextInput
      then pure Nothing
      else do
        (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
        (field, Rect clipX _ _ _) <- nodeTextFieldGeom ctx idx x y w h
        scrollX <- syncTextInputScroll ctx idx x y w h
        fm <- nodeFontMetrics ctx idx
        value <- textInputValue ctx idx
        pure (Just (field, clipX - scrollX, value, fm))

-- | Mouse selection in single-line field @wid@: click (with word and line
-- multi-clicks), drag, and the search clear button. False when @wid@ is not a
-- single-line field.
finalizeTextInputMouse :: Context -> Input -> WidgetId -> IO Bool
finalizeTextInputMouse ctx inp wid = do
  mGeom <- textInputGeomForWidget ctx wid
  case mGeom of
    Nothing -> pure False
    Just (fieldRect, contentX, value, fm) -> do
      let mouse@(V2 mouseX _) = inputMousePos inp
          charAt = do
            prepared <- prepareFontMetrics fm value
            pure (textIndexAtX prepared value (max 0 (mouseX - contentX)))
      if inputMousePressed inp && rectContains fieldRect mouse
        then ifM (searchClearHit ctx wid mouse) (clearSearchInput ctx wid) $ do
          idx <- charAt
          clicks <- normalizeTextFieldClicks ctx wid idx 0 0 False (max 1 (inputMouseClicks inp))
          uncurry (updateTextInputSelection ctx wid) (textSelectionForDrag value idx idx clicks)
          setTextInputDrag ctx (Just (TextInputDrag wid idx 0 0 False clicks))
        else do
          mDrag <- getsInteraction ctx isTextInputDrag
          case mDrag of
            Just drag
              | textInputDragWidget drag == wid
                  , not (textInputDragMultiline drag)
                  , inputMouseDown inp || inputMouseReleased inp -> do
                  idx <- charAt
                  uncurry (updateTextInputSelection ctx wid) $
                    textSelectionForDrag value (textInputDragAnchor drag) idx (textInputDragClicks drag)
            _ -> pure ()
      pure True

collapseTextInputSelection :: Context -> WidgetId -> IO ()
collapseTextInputSelection ctx wid =
  modifyStore ctx $ \store -> insertSlot fieldInt (slotKey SlotAnchor key) (findSlot fieldInt 0 (slotKey SlotCursor key) store) store
 where
  key = intKey wid

-- | Count a press as a multi-click only when it lands on the same cell as the
-- previous press; anything else restarts the count at one.
normalizeTextFieldClicks :: Context -> WidgetId -> Int -> Int -> Int -> Bool -> Int -> IO Int
normalizeTextFieldClicks ctx wid flat row col multiline rawClicks = do
  let cell =
        TextFieldClickCell
          { textFieldClickWidget = wid
          , textFieldClickFlat = flat
          , textFieldClickRow = row
          , textFieldClickCol = col
          , textFieldClickMultiline = multiline
          }
  if rawClicks <= 1
    then modifyInteraction ctx (\s -> s {isTextFieldClickCell = Just cell}) >> pure rawClicks
    else do
      mPrev <- getsInteraction ctx isTextFieldClickCell
      -- A single-line cell leaves row and column 0 and a multiline one
      -- leaves the flat index 0, so the derived equality compares the
      -- coordinates that mode uses.
      if mPrev == Just cell
        then pure rawClicks
        else modifyInteraction ctx (\s -> s {isTextFieldClickCell = Just cell}) >> pure 1
