{-# LANGUAGE DataKinds #-}

module NanoUI.Frame.Spans
  ( collectTextSpans
  , collectOverlayTextSpans
  , collectRasterSpans
  , widgetNodeCount
  , widgetHitRect
  , widgetTextSpans
  , forWidgetTextPlacements_
  , selectableTextGeometry
  , collectNodeTextSpans
  ) where

import Control.Monad (forM, unless, when)
import Data.IORef (readIORef, writeIORef)
import qualified Data.IntMap.Strict as IM
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import NanoUI.Context
  ( Context (..)
  , SpanCacheEntry (..)
  , WidgetTextCacheEntry (..)
  , WidgetTextPlacement (..)
  )
import NanoUI.Damage (floatingPanelRects)
import NanoUI.Font
  ( FontMetrics (..)
  , alignedTextPen
  , centeredTextY
  , checkboxLeading
  , labelContentInset
  , layoutLineHeight
  , menuItemPadX
  , prepareFontMetrics
  , tableCellInset
  , treeRowLeading
  , truncateTextIO
  , widgetContentInset
  , wrapTextLinesIO
  )
import NanoUI.Frame.Chrome (displayText, textInputFocused, textInputValue, widgetVisualStyle)
import NanoUI.Frame.Node (resolveFontFor, scrollViewportAt)
import NanoUI.Frame.Scroll.Geometry (padContentClip, tagClippedSpans)
import NanoUI.Frame.Select (collectSelectDropdownSpans, tagSelectClippedSpans)
import NanoUI.Frame.SpanArena (SpanArena, pushSpan, resetSpanArena, spanArenaToList, spanArenaToListOccluded, withSpanArenaSnap)
import NanoUI.Frame.TextArea.Geometry (TextAreaGeom (..), textAreaGeom)
import NanoUI.Frame.TextEdit.Menu (collectTextEditMenuSpans)
import NanoUI.Frame.TextInput (TextInputGeom (..), syncTextInputScroll, tagTextInputClippedSpans, textInputGeom)
import NanoUI.Input (Input)
import NanoUI.Layout.Arena
  ( NodeArena
  , NodeIdx
  , NodeType (..)
  , SizingTag (..)
  , arenaCount
  , forNodes_
  , getAlignX
  , getClipRect
  , getFirstChild
  , getMinMax
  , getNextSibling
  , getNodeFontColor
  , getNodeFontSize
  , getNodeType
  , getPadding
  , getParent
  , getRect
  , getStyleIdx
  , getText
  , getWidthSizing
  , isFloatingNode
  , isScrollNode
  , isWidgetNode
  , parentIsRow
  )
import NanoUI.Style (AlignX (..), FontVariant (..), Padding (..), Style (..), Theme (..), themeAccent, themeMuted, themePanel)
import NanoUI.Types (Color (..), Rect (..), lerpColor, onGrid, rectIntersect)
import NanoUI.Widgets.ColorPicker (ColorPickerPart (..), colorPickerPartOf, colorPickerPartRect, colorPickerPreviewGeom)
import NanoUI.WidgetText
  ( colorPickerCurrentLabel
  , colorPickerNewLabel
  , isCloseButtonStyle
  , isMenuItemStyle
  , isTableHeaderStyle
  , numericTextClip
  , selectChevronReserve
  , tableStripeColor
  , textInputBareMode
  , textInputNumericMode
  , textInputFieldText
  , textInputSearchMode
  , textInputSelectableMode
  , textNodeFontVariant
  , treeDecodeStyle
  )

collectTextSpans :: Context -> IO [(Rect, T.Text, Color, Color, Rect)]
collectTextSpans ctx = do
  count <- arenaCount (ctxNodeArena ctx)
  let arena = ctxSpanBase ctx
  resetSpanArena arena
  withSpanArenaSnap arena $
    when (count > 0) $
      collectClippedSpans ctx 0 (Rect 0 0 1e9 1e9) arena
  panels <- floatingPanelRects ctx
  spanArenaToListOccluded panels arena

collectOverlayTextSpans :: Context -> Input -> IO [(Rect, T.Text, Color, Color, Rect)]
collectOverlayTextSpans ctx inp = do
  let arena = ctxSpanOverlay ctx
      push (r, t, fg, bg, c) = pushSpan arena r t fg bg c
  resetSpanArena arena
  withSpanArenaSnap arena $ do
    collectFloatingSpansInto ctx NodeWindow arena
    collectFloatingSpansInto ctx NodeModal arena
    collectFloatingSpansInto ctx NodePopup arena
    drops <- collectSelectDropdownSpans ctx inp
    menu <- collectTextEditMenuSpans ctx inp
    mapM_ push drops
    mapM_ push menu
  spanArenaToList arena

collectRasterSpans :: Context -> Input -> IO ([(Rect, T.Text, Color, Color, Rect)], [(Rect, T.Text, Color, Color, Rect)])
collectRasterSpans ctx inp = (,) <$> collectTextSpans ctx <*> collectOverlayTextSpans ctx inp

widgetNodeCount :: Context -> IO Int
widgetNodeCount ctx = arenaCount (ctxNodeArena ctx)

{-# INLINE collectClippedSpans #-}
collectClippedSpans :: Context -> NodeIdx -> Rect -> SpanArena -> IO ()
collectClippedSpans ctx idx clip arena = do
  nt <- getNodeType (ctxNodeArena ctx) idx
  unless (isFloatingNode nt) $
    collectClippedSpans' ctx idx nt clip arena

collectClippedSpans' :: Context -> NodeIdx -> NodeType -> Rect -> SpanArena -> IO ()
collectClippedSpans' ctx idx nt clip arena = do
  (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
  mClipChildren <-
    if isScrollNode nt
      then
        getClipRect (ctxNodeArena ctx) idx >>= \case
          Just live -> pure (rectIntersect clip live)
          Nothing -> rectIntersect clip <$> scrollViewportAt ctx idx x y w h
      else pure (if nt == NodePanel then rectIntersect clip (Rect x y w h) else Just clip)
  case mClipChildren of
    Nothing -> pure ()
    Just clipHere -> do
      let fm = ctxFontMetrics ctx
      spans <- collectNodeTextSpans ctx idx
      here <-
        case nt of
          NodeSelect -> pure (tagSelectClippedSpans clipHere x y w h fm spans)
          NodeTextInput -> do
            si <- getStyleIdx (ctxNodeArena ctx) idx
            pure $
              if textInputNumericMode si
                then maybe [] (`tagClippedSpans` spans) (rectIntersect clipHere (numericTextClip fm x y w h))
                else
                  if textInputBareMode si || textInputSelectableMode si
                    then tagClippedSpans clipHere spans
                    else tagTextInputClippedSpans clipHere x y w h fm spans
          _ -> pure (tagClippedSpans clipHere spans)
      mapM_ (\(r, t, fg, bg, c) -> pushSpan arena r t fg bg c) here
      walkChildSpans ctx idx clipHere arena

walkChildSpans :: Context -> NodeIdx -> Rect -> SpanArena -> IO ()
walkChildSpans ctx idx clip arena = getFirstChild (ctxNodeArena ctx) idx >>= go
  where
    go ci
      | ci < 0 = pure ()
      | otherwise = do
          ns <- getNextSibling (ctxNodeArena ctx) ci
          -- Later siblings paint under earlier ones; walk reverse then collect.
          go ns
          collectClippedSpans ctx ci clip arena

findAncestorMaxW :: NodeArena -> NodeIdx -> IO Float
findAncestorMaxW na idx = go idx 0
  where
    go cur !padAccum = do
      p <- getParent na cur
      if p < 0
        then pure 1e9
        else do
          pad <- getPadding na p
          let padAccum' = padAccum + padL pad + padR pad
          (_, _, pMaxW, _) <- getMinMax na p
          (pwTag, pwVal) <- getWidthSizing na p
          if pwTag == SizingFixed
            then pure (max 0 (pwVal - padAccum'))
            else if pMaxW < 1e8
              then pure (max 0 (pMaxW - padAccum'))
              else go p padAccum'

-- | Text spans of one node. A text node's spans are cached per node until
-- its inputs change. Placement uses glyph ink ('alignedTextPen'), not
-- TTF_GetStringSize; wrapping still measures with the host so line breaks
-- stay on the TTF width.
collectNodeTextSpans :: Context -> NodeIdx -> IO [(Rect, T.Text, Color, Color)]
collectNodeTextSpans ctx idx = do
  let arena = ctxNodeArena ctx
  nt <- getNodeType arena idx
  (x, y, w, h) <- getRect arena idx
  if nt /= NodeText
    then if isWidgetNode nt then widgetTextSpans ctx nt idx x y w h else pure []
    else do
      theme <- readIORef (ctxTheme ctx)
      raw <- getText arena idx
      si <- getStyleIdx arena idx
      mCustomCol <- getNodeFontColor arena idx
      fontSize <- getNodeFontSize arena idx
      ax <- getAlignX arena idx
      (_, _, maxW, _) <- getMinMax arena idx
      (wTag, _) <- getWidthSizing arena idx
      isRowChild <- parentIsRow arena idx
      effMaxW <- if maxW < 1e8 then pure maxW else findAncestorMaxW arena idx
      let rect = Rect x y w h
          mStripe = tableStripeColor theme si
          variantFg = case textNodeFontVariant si of
            FontHeading -> themeAccent theme
            FontMuted -> themeMuted theme
            FontDanger -> themeRed theme
            _ -> styleFg (themePanel theme)
          fg = fromMaybe variantFg mCustomCol
          bg = fromMaybe (styleBg (themePanel theme)) mStripe
      cache <- readIORef (ctxSpanCache ctx)
      case IM.lookup idx cache of
        Just e
          | sceText e == raw
              && sceFg e == fg
              && sceBg e == bg
              && sceStyle e == si
              && sceFontSize e == fontSize
              && sceAlign e == fromEnum ax
              && sceWidthTag e == fromEnum wTag
              && sceRect e == rect
              && sceEffMaxW e == effMaxW
              && sceRowChild e == isRowChild ->
              pure (sceSpans e)
        _ -> do
          placed <-
            if T.null raw
              then pure []
              else do
                (fm, _, measure) <- resolveFontFor ctx NodeText fontSize si
                let ix = fst ((if isJust mStripe then tableCellInset else labelContentInset) fm)
                    measureW = fmap fst . measure
                    lineH = fmLineHeight fm
                    contentW = max 0 (w - 2 * ix)
                    wrapCap
                      | effMaxW < 1e8 = max 0 effMaxW
                      | wTag == SizingGrow && w > 0 = w
                      | otherwise = effMaxW
                tw <- measureW raw
                if T.any (== '\n') raw || (not isRowChild && wrapCap < 1e8 && wrapCap + 0.5 < tw)
                  then do
                    textLines <- wrapTextLinesIO measureW raw (max 0 (wrapCap - 2 * ix))
                    forM (zip [(0 :: Int) ..] textLines) $ \(i, line) -> do
                      prepared <- prepareFontMetrics fm line
                      let (tx, used) = alignedTextPen ax x w ix prepared line
                          ty = centeredTextY fm (y + onGrid (fmSnapScale fm) (fromIntegral i * lineH)) lineH lineH
                      pure (Rect tx ty used lineH, line)
                  else do
                    shown <-
                      if tw > contentW && contentW > 0 && (wTag == SizingGrow || maxW < 1e8)
                        then truncateTextIO measureW contentW raw
                        else pure raw
                    prepared <- prepareFontMetrics fm shown
                    let (tx, used) = alignedTextPen ax x w ix prepared shown
                    pure [(Rect tx (centeredTextY fm y h lineH) used lineH, shown)]
          let spans = [(r, line, fg, bg) | (r, line) <- placed]
          writeIORef (ctxSpanCache ctx) $
            IM.insert
              idx
              SpanCacheEntry
                { sceText = raw
                , sceFg = fg
                , sceBg = bg
                , sceStyle = si
                , sceFontSize = fontSize
                , sceAlign = fromEnum ax
                , sceWidthTag = fromEnum wTag
                , sceRect = rect
                , sceEffMaxW = effMaxW
                , sceRowChild = isRowChild
                , sceSpans = spans
                }
              cache
          pure spans

widgetHitRect :: Context -> NodeType -> NodeIdx -> Float -> Float -> Float -> Float -> IO Rect
widgetHitRect ctx nt idx x y w h = do
  let fm = ctxFontMetrics ctx
  case nt of
    NodeTextInput -> do
      si <- getStyleIdx (ctxNodeArena ctx) idx
      if textInputSearchMode si || textInputBareMode si || textInputSelectableMode si || textInputNumericMode si
        then pure (Rect x y w h)
        else pure (tigFieldRect (textInputGeom fm x y w h))
    NodeTextArea -> pure (tagFieldRect (textAreaGeom fm x y w h))
    NodeButton -> do
      si <- getStyleIdx (ctxNodeArena ctx) idx
      -- Close buttons get a padded target that stays inside the title bar, so
      -- the inner east resize still works below the control.
      if isCloseButtonStyle si
        then pure (Rect (x - 8) (y - 4) (w + 10) (h + 4))
        else pure (Rect x y w h)
    _ -> pure (Rect x y w h)

widgetTextSpans ::
  Context -> NodeType -> NodeIdx -> Float -> Float -> Float -> Float -> IO [(Rect, T.Text, Color, Color)]
widgetTextSpans ctx nt idx x y w h = do
  style <- widgetVisualStyle ctx nt idx
  mFontColor <- getNodeFontColor (ctxNodeArena ctx) idx
  placements <- widgetTextPlacements ctx nt idx x y w h
  let fg = fromMaybe (styleFg style) mFontColor
      bg = styleBg style
  case nt of
    NodeTextInput -> do
      value <- textInputValue ctx idx
      focus <- textInputFocused ctx idx
      let fieldFg = if T.null value && not focus then lerpColor fg bg 0.40 else fg
      pure [(Rect px py tw th, txt, fieldFg, bg) | (txt, px, py, tw, th) <- placements]
    _ ->
      pure [(Rect px py tw th, txt, fg, bg) | (txt, px, py, tw, th) <- placements, not (T.null txt)]

-- | Cacheable widget labels depend on text, style, font size, alignment and
-- dimensions, but not the absolute node origin. Text
-- fields / areas / colour pickers / sliders are data-dependent and stay out.
cacheableWidgetLabel :: NodeType -> Bool
cacheableWidgetLabel = \case
  NodeButton -> True
  NodeSelect -> True
  NodeTree -> True
  NodeCheckbox -> True
  NodeRadio -> True
  _ -> False

widgetTextPlacements ::
  Context -> NodeType -> NodeIdx -> Float -> Float -> Float -> Float -> IO [(T.Text, Float, Float, Float, Float)]
widgetTextPlacements ctx nt idx x y w h
  | cacheableWidgetLabel nt = do
      placement <- cachedWidgetLabel ctx nt idx w h
      pure [(txt, x + px, y + py, tw, th) | Just (WidgetTextPlacement txt px py tw th) <- [placement]]
  | otherwise = computeWidgetTextPlacements ctx nt idx x y w h

-- | Runtime consumer API. The Bool marks the last placement (for table sort
-- arrows); cached labels are translated directly into the consumer.
{-# INLINE forWidgetTextPlacements_ #-}
forWidgetTextPlacements_ ::
  Context -> NodeType -> NodeIdx -> Float -> Float -> Float -> Float ->
  (Bool -> T.Text -> Float -> Float -> Float -> Float -> IO ()) -> IO ()
forWidgetTextPlacements_ ctx nt idx x y w h emit
  | cacheableWidgetLabel nt = do
      placement <- cachedWidgetLabel ctx nt idx w h
      case placement of
        Nothing -> pure ()
        Just (WidgetTextPlacement txt px py tw th) -> emit True txt (x + px) (y + py) tw th
  | otherwise = do
      placements <- computeWidgetTextPlacements ctx nt idx x y w h
      let go [] = pure ()
          go ((txt, px, py, tw, th) : rest) =
            emit (null rest) txt px py tw th >> go rest
      go placements

cachedWidgetLabel :: Context -> NodeType -> NodeIdx -> Float -> Float -> IO (Maybe WidgetTextPlacement)
cachedWidgetLabel ctx nt idx w h = do
  fontSizeVal <- getNodeFontSize (ctxNodeArena ctx) idx
  si <- getStyleIdx (ctxNodeArena ctx) idx
  txt <- displayText ctx nt idx
  ax <-
    if nt == NodeButton && isTableHeaderStyle si
      then getAlignX (ctxNodeArena ctx) idx
      else pure AlignStart
  let ntTag = fromEnum nt
  cache <- readIORef (ctxWidgetTextCache ctx)
  case IM.lookup idx cache of
    Just e
      | wtcNodeType e == ntTag
          && wtcStyle e == si
          && wtcFontSize e == fontSizeVal
          && wtcText e == txt
          && wtcWidth e == w
          && wtcHeight e == h
          && wtcAlign e == fromEnum ax -> pure (wtcPlacement e)
    _ -> do
      placement <- computeWidgetLabel ctx nt txt si fontSizeVal ax w h
      writeIORef
        (ctxWidgetTextCache ctx)
        (IM.insert idx (WidgetTextCacheEntry ntTag si fontSizeVal txt w h (fromEnum ax) placement) cache)
      pure placement

-- All coordinates here are local. centeredTextY snaps the baseline offset,
-- not the origin; final device-pixel snapping stays in the draw backend.
computeWidgetLabel :: Context -> NodeType -> T.Text -> Int -> Float -> AlignX -> Float -> Float -> IO (Maybe WidgetTextPlacement)
computeWidgetLabel ctx nt txt si fontSizeVal ax w h
  | nt == NodeButton && isCloseButtonStyle si = pure Nothing
  | otherwise = do
      (source, _, measure) <- resolveFontFor ctx nt fontSizeVal si
      fm <- prepareFontMetrics source txt
      (tw, th) <- measure txt
      let (ix, _) = widgetContentInset fm
          (tx, used) = case nt of
            NodeButton
              | isTableHeaderStyle si -> alignedTextPen ax 0 w (fst (tableCellInset fm)) fm txt
              | isMenuItemStyle si ->
                  let inset = menuItemPadX + ix
                   in (inset, min tw (max 0 (w - inset - ix)))
              | otherwise -> alignedTextPen AlignCenter 0 w 0 fm txt
            NodeSelect -> (ix, min tw (w - ix - selectChevronReserve))
            NodeTree ->
              let (_, depth, _, _) = treeDecodeStyle si
               in (fst (labelContentInset fm) + treeRowLeading fm depth, tw)
            _ -> (fst (labelContentInset fm) + checkboxLeading fm, tw)
      let !placement = WidgetTextPlacement txt tx (centeredTextY fm 0 h th) used th
      pure (Just placement)

-- | Pure geometry of selectable text: the pen origin, centered baseline box and
-- line height. Selectable text never scrolls, so the pen is just the node x.
-- Paint uses this and skips the width measure; span placement adds it.
selectableTextGeometry :: FontMetrics -> Float -> Float -> Float -> (Float, Float, Float)
selectableTextGeometry fm x y h =
  let lineH = fmLineHeight fm
   in (x, centeredTextY fm y h lineH, lineH)

computeWidgetTextPlacements ::
  Context -> NodeType -> NodeIdx -> Float -> Float -> Float -> Float -> IO [(T.Text, Float, Float, Float, Float)]
computeWidgetTextPlacements ctx nt idx x y w h = do
  fontSizeVal <- getNodeFontSize (ctxNodeArena ctx) idx
  si <- getStyleIdx (ctxNodeArena ctx) idx
  (fm, _, measureTxt) <- resolveFontFor ctx nt fontSizeVal si
  let (ix, iy) = widgetContentInset fm
      lineH = fmLineHeight fm
  case nt of
    NodeColorPicker
      | colorPickerPartOf si /= PickerPreview -> pure []
      | otherwise -> do
          band@(Rect bx _ _ _) <- colorPickerPartRect (ctxNodeArena ctx) idx (Rect x y w h)
          let (currentY, _, newY, _) = colorPickerPreviewGeom fm band
              labelH = layoutLineHeight fm
          (cw, ch) <- measureTxt colorPickerCurrentLabel
          (nw, nh) <- measureTxt colorPickerNewLabel
          pure
            [ (colorPickerCurrentLabel, bx, centeredTextY fm currentY labelH ch, cw, ch)
            , (colorPickerNewLabel, bx, centeredTextY fm newY labelH nh, nw, nh)
            ]
    NodeSlider -> pure []
    NodeTextInput
      | textInputSelectableMode si -> do
          value <- textInputValue ctx idx
          let (penX, ty, selLineH) = selectableTextGeometry fm x y h
          (fw, _) <- measureTxt value
          pure [(value, penX, ty, fw, selLineH)]
      | otherwise -> do
          let bare = textInputBareMode si || textInputNumericMode si
          ph <- if bare then pure "" else getText (ctxNodeArena ctx) idx
          value <- textInputValue ctx idx
          focus <- textInputFocused ctx idx
          let fieldTxt = textInputFieldText ph value focus
              Rect _ fieldY _ fieldH = if bare then Rect x y w h else tigFieldRect (textInputGeom fm x y w h)
          (fw, _) <- measureTxt fieldTxt
          scrollX <- syncTextInputScroll ctx idx x y w h
          pure [(fieldTxt, x + ix - scrollX, centeredTextY fm fieldY fieldH lineH, fw, lineH)]
    NodeTextArea -> do
      lbl <- getText (ctxNodeArena ctx) idx
      value <- textInputValue ctx idx
      let Rect _ fieldY _ fieldH = tagFieldRect (textAreaGeom fm x y w h)
      (lw, lh) <- measureTxt lbl
      (fw, _) <- measureTxt (if T.null value then " " else value)
      pure
        [ (lbl, x, centeredTextY fm y lineH lh, lw, lh)
        , (value, x + ix, fieldY + iy, fw, fieldH)
        ]
    NodeDrawing -> pure []
    _ -> do
      txt <- displayText ctx nt idx
      ax <- getAlignX (ctxNodeArena ctx) idx
      (_, th) <- measureTxt txt
      prepared <- prepareFontMetrics fm txt
      let (tx, used) = alignedTextPen ax x w ix prepared txt
      pure [(txt, tx, centeredTextY fm y h th, used, th)]

-- | Spans inside every floating panel of one kind, clipped to its content box.
collectFloatingSpansInto :: Context -> NodeType -> SpanArena -> IO ()
collectFloatingSpansInto ctx wanted arena =
  forNodes_ (ctxNodeArena ctx) $ \idx -> do
    nt <- getNodeType (ctxNodeArena ctx) idx
    when (nt == wanted) $ do
      (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
      clip <-
        if isScrollNode nt
          then scrollViewportAt ctx idx x y w h
          else padContentClip (ctxFontMetrics ctx) x y w h <$> getPadding (ctxNodeArena ctx) idx
      walkChildSpans ctx idx clip arena
