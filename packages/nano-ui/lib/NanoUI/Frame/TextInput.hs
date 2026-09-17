{-# LANGUAGE DataKinds #-}

-- | Single-line text fields: field geometry, horizontal scroll, caret and
-- selection painting, and mouse selection. Also holds the click-count and
-- caret primitives the text area shares.
module NanoUI.Frame.TextInput
  ( textInputFieldRect
  , textInputFieldTextClip
  , nodeTextFieldGeom
  , tagTextInputClippedSpans
  , syncTextInputScroll
  , drawTextInputSelection
  , drawTextInputCaret
  , drawTextCaret
  , drawTextSelectionLine
  , searchClearHit
  , normalizeTextFieldClicks
  , finalizeTextInputMouse
  , collapseTextInputSelection
  ) where

import Control.Monad (forM_, unless, when)
import qualified Data.IntMap.Strict as IM
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import NanoUI.Context
  ( Context (..)
  , TextFieldClickCell (..)
  , TextInputDrag (..)
  , WidgetStore (..)
  , getStore
  , getTextFieldClickCell
  , getTextInputDrag
  , intKey
  , markDirty
  , setStore
  , setTextFieldClickCell
  , setTextInputDrag
  , Slot (..)
  , slotKey
  , nodeTheme
  )
import NanoUI.Draw (DrawArena, pushRect)
import NanoUI.Font (FontMetrics (..), caretXIO, centeredTextY, lineWidthIO, prepareFontMetrics, selectionSpans, textIndexAtX, widgetContentInset)
import NanoUI.Frame.Chrome (textInputFocused, textInputValue)
import NanoUI.Frame.Hit (findNodeByWidgetId)
import NanoUI.Frame.Node (nodeFontMetrics)
import NanoUI.Frame.Scroll.Geometry (padTextClipRect)
import NanoUI.Id (WidgetId)
import NanoUI.Input
  ( Input (..)
  , inputMouseClicks
  , inputMouseDown
  , inputMousePos
  , inputMousePressed
  , inputMouseReleased
  )
import NanoUI.Layout.Arena
  ( NodeIdx
  , NodeType (NodeTextInput)
  , getNodeType
  , getOptions
  , getRect
  , getStyleIdx
  , getText
  , getWidgetId
  )
import NanoUI.Style (Style (..), themeSelection)
import NanoUI.Types (Color (..), Rect (..), V2 (..), rectContains, rectIntersect, rectOverlapArea, rectW)
import NanoUI.WidgetText
  ( comboTextClip
  , numericTextClip
  , searchFieldIconRects
  , searchFieldTextClip
  , textInputNumericMode
  , textInputFieldHeight
  , textInputFieldText
  , textInputSearchMode
  , textInputSelectableMode
  )
import NanoUI.Widgets.TextCommon
  ( selectionCaretGeom
  , textSelectionForClick
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
  pure $
    if textInputSelectableMode si
      then (box, box)
      else
        if textInputNumericMode si
          then (box, numericTextClip fm x y w h)
          else
            if textInputSearchMode si
              then (box, if null opts then searchFieldTextClip fm x y w h else comboTextClip fm x y w h)
              else (field, textInputFieldTextClip fm field)

-- | Whether the pointer is over the clear (×) button of a non-empty search
-- field. Search fields reserve that slot even when empty, but the button is
-- only active when there is text to clear.
searchClearHit :: Context -> WidgetId -> V2 -> IO Bool
searchClearHit ctx wid mouse = do
  mIdx <- findNodeByWidgetId ctx wid
  case mIdx of
    Nothing -> pure False
    Just idx -> do
      si <- getStyleIdx (ctxNodeArena ctx) idx
      opts <- getOptions (ctxNodeArena ctx) idx
      if not (textInputSearchMode si) || not (null opts)
        then pure False
        else do
          value <- textInputValue ctx idx
          if T.null value
            then pure False
            else do
              (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
              let (_, clearRect) = searchFieldIconRects (ctxFontMetrics ctx) x y w h
              pure (rectContains clearRect mouse)

-- | Clear a search field. The debounced pulse picks the empty text up as an
-- immediate (empty) commit on the next frame.
clearSearchField :: Context -> WidgetId -> IO ()
clearSearchField ctx wid = do
  store <- getStore ctx
  let key = intKey wid
      storeInt' =
        IM.insert (slotKey SlotAnchor key) 0 $
          IM.insert (slotKey SlotCursor key) 0 (storeInt store)
      store' = store {storeText = IM.insert key "" (storeText store), storeInt = storeInt'}
  setStore ctx store'
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
      pure (max 0 (min maxScroll s0))

syncTextInputScroll :: Context -> NodeIdx -> Float -> Float -> Float -> Float -> IO Float
syncTextInputScroll ctx idx x y w h = do
  si <- getStyleIdx (ctxNodeArena ctx) idx
  if textInputSelectableMode si
    then pure 0
    else do
      wid <- getWidgetId (ctxNodeArena ctx) idx
      store <- getStore ctx
      let key = intKey wid
      value <- textInputValue ctx idx
      focus <- textInputFocused ctx idx
      (_, clip) <- nodeTextFieldGeom ctx idx x y w h
      let cursor = IM.findWithDefault (T.length value) (slotKey SlotCursor key) (storeInt store)
          oldScroll = IM.findWithDefault 0 (slotKey SlotTextInputScroll key) (storeFloat store)
      newScroll <- computeTextInputScroll (ctxFontMetrics ctx) (rectW clip) value cursor oldScroll focus
      when (newScroll /= oldScroll) $
        setStore ctx (store {storeFloat = IM.insert (slotKey SlotTextInputScroll key) newScroll (storeFloat store)})
      pure newScroll

drawTextInputSelection :: DrawArena -> Context -> NodeIdx -> Float -> Float -> Float -> Float -> Maybe Float -> IO ()
drawTextInputSelection da ctx idx x y w h mScrollX = do
  focus <- textInputFocused ctx idx
  when focus $ do
    value <- textInputValue ctx idx
    wid <- getWidgetId (ctxNodeArena ctx) idx
    store <- getStore ctx
    let key = intKey wid
        cursor = IM.findWithDefault (T.length value) (slotKey SlotCursor key) (storeInt store)
        anchor = IM.findWithDefault cursor (slotKey SlotAnchor key) (storeInt store)
        selLo = min anchor cursor
        selHi = max anchor cursor
    when (selLo < selHi) $ do
      theme <- nodeTheme ctx idx
      (Rect _ boxY _ boxH, Rect clipX _ _ _) <- nodeTextFieldGeom ctx idx x y w h
      fm <- nodeFontMetrics ctx idx
      let lineH = fmLineHeight fm
      prepared <- prepareFontMetrics fm value
      scrollX <- maybe (syncTextInputScroll ctx idx x y w h) pure mScrollX
      forM_ (selectionSpans prepared value selLo selHi) $ \(wLo, wHi) ->
        drawTextSelectionLine
          da
          (clipX + wLo - scrollX)
          (centeredTextY fm boxY boxH lineH)
          (wHi - wLo)
          lineH
          (themeSelection theme)

drawTextInputCaret :: DrawArena -> Context -> NodeIdx -> Float -> Float -> Float -> Float -> Style -> IO ()
drawTextInputCaret da ctx idx x y w h style = do
  si <- getStyleIdx (ctxNodeArena ctx) idx
  unless (textInputSelectableMode si) $ do
    focus <- textInputFocused ctx idx
    when focus $ do
      value <- textInputValue ctx idx
      wid <- getWidgetId (ctxNodeArena ctx) idx
      store <- getStore ctx
      let cursor = IM.findWithDefault (T.length value) (slotKey SlotCursor (intKey wid)) (storeInt store)
      lbl <- getText (ctxNodeArena ctx) idx
      fm <- nodeFontMetrics ctx idx
      let fieldTxt = textInputFieldText lbl value focus
          lineH = fmLineHeight fm
      pw <- caretXIO fm fieldTxt cursor
      (Rect _ boxY _ boxH, Rect clipX _ _ _) <- nodeTextFieldGeom ctx idx x y w h
      scrollX <- syncTextInputScroll ctx idx x y w h
      let (caretX, caretY, caretH) =
            selectionCaretGeom (clipX - scrollX) (centeredTextY fm boxY boxH lineH) pw lineH
      drawTextCaret da caretX caretY caretH (styleFg style)

updateTextInputSelection :: Context -> WidgetId -> Int -> Int -> IO ()
updateTextInputSelection ctx wid anchor cursor = do
  store <- getStore ctx
  let key = intKey wid
      oldAnchor = IM.findWithDefault cursor (slotKey SlotAnchor key) (storeInt store)
      oldCursor = IM.findWithDefault 0 (slotKey SlotCursor key) (storeInt store)
  when (oldAnchor /= anchor || oldCursor /= cursor) $ do
    setStore
      ctx
      ( store
          { storeInt =
              IM.insert (slotKey SlotAnchor key) anchor $
                IM.insert (slotKey SlotCursor key) cursor (storeInt store)
          }
      )
    markDirty ctx

-- | Field box, text origin x (scroll applied), value and font of a single-line
-- field.
textInputGeomForWidget :: Context -> WidgetId -> IO (Maybe (Rect, Float, Text, FontMetrics))
textInputGeomForWidget ctx wid = do
  mIdx <- findNodeByWidgetId ctx wid
  case mIdx of
    Nothing -> pure Nothing
    Just idx -> do
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
        then do
          cleared <- searchClearHit ctx wid mouse
          if cleared
            then clearSearchField ctx wid
            else do
              idx <- charAt
              clicks <- normalizeTextFieldClicks ctx wid idx 0 0 False (max 1 (inputMouseClicks inp))
              uncurry (updateTextInputSelection ctx wid) (textSelectionForClick value idx clicks)
              setTextInputDrag ctx (Just (TextInputDrag wid idx 0 0 False clicks))
        else do
          mDrag <- getTextInputDrag ctx
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
collapseTextInputSelection ctx wid = do
  store <- getStore ctx
  let key = intKey wid
      cur = IM.findWithDefault 0 (slotKey SlotCursor key) (storeInt store)
  setStore ctx (store {storeInt = IM.insert (slotKey SlotAnchor key) cur (storeInt store)})

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
    then setTextFieldClickCell ctx (Just cell) >> pure rawClicks
    else do
      mPrev <- getTextFieldClickCell ctx
      if maybe False (sameCell cell) mPrev
        then pure rawClicks
        else setTextFieldClickCell ctx (Just cell) >> pure 1
  where
    sameCell a b =
      textFieldClickWidget a == textFieldClickWidget b
        && textFieldClickMultiline a == textFieldClickMultiline b
        && if textFieldClickMultiline a
          then textFieldClickRow a == textFieldClickRow b && textFieldClickCol a == textFieldClickCol b
          else textFieldClickFlat a == textFieldClickFlat b
