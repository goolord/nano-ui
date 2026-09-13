{-# LANGUAGE DataKinds #-}

module NanoUI.Frame.Spans
  ( collectTextSpans
  , collectOverlayTextSpans
  , collectRasterSpans
  , widgetNodeCount
  , widgetHitRect
  , widgetTextSpans
  , widgetTextPlacements
  , forWidgetTextPlacements_
  , collectNodeTextSpans
  , sliderValue
  , walkChildSpans
  ) where


import Control.Monad (unless, when)
import Data.IORef (readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import NanoUI.Widgets.ColorPicker
  ( ColorPickerGeom (..)
  , colorPickerAlphaMode
  , colorPickerGeom
  )
import NanoUI.Context
  ( Context (..)
  , SpanCacheEntry (..)
  , WidgetTextCacheEntry (..)
  , WidgetTextPlacement (..)
  , WidgetStore (..)
  , getStore
  , intKey
  )
import NanoUI.Damage (floatingPanelRects)
import NanoUI.Font
  ( FontMetrics (..)
  , alignedTextPen
  , centeredTextY
  , checkboxLeading
  , labelContentInset
  , tableCellInset
  , layoutLineHeight
  , treeRowLeading
  , truncateTextAdvance
  , truncateTextIO
  , widgetContentInset
  , wrapTextLinesIO
  , measureText
  )
import NanoUI.Input (Input)
import NanoUI.Layout.Arena
  ( NodeArena
  , NodeIdx
  , NodeType (..)
  , SizingTag (..)
  , arenaCount
  , getAlignX
  , getScrollContentW
  , getClipRect
  , getDirection
  , getFirstChild
  , getMinMax
  , getNextSibling
  , getParent
  , getNodeType
  , getNodeValue
  , getPadding
  , getRect
  , getStyleIdx
  , getText
  , getWidthSizing
  , getWidgetId
  , getNodeFontSize
  , getNodeFontColor
  , parentIsRow
  , isFloatingNode
  , isScrollNode
  , isWidgetNode
  )
import NanoUI.Layout.Solve (scrollBarSlotOf)
import NanoUI.Style (AlignX (..), FontStyle (..), FontVariant (..), FontWeight (..), Padding (..), Style (..), styleBg, styleFg)
import NanoUI.Types (Color (..), Rect (..), lerpColor, onGrid, rectH, rectIntersect, rectW, rectX, rectY)
import NanoUI.WidgetText (isCloseButtonStyle, isMenuItemStyle, isTableHeaderStyle, textInputBareMode, textInputSearchMode)
import NanoUI.WidgetText
  ( colorPickerCurrentLabel
  , colorPickerNewLabel
  , selectChevronReserve
  , textInputFieldText
  , treeDecodeStyle
  , tableStripeColor
  , textNodeFontStyle
  , textNodeFontVariant
  , textNodeFontWeight
  )
import NanoUI.Frame.Chrome
  ( buildFloatingAncestorMap
  , displayText
  , floatingLabelPaint
  , textInputFocused
  , textInputMenuItemPadX
  , textInputValue
  , widgetVisualStyle
  )
import NanoUI.Frame.Scroll.Geometry
  ( decodeScrollConfig
  , isScrollStyle2D
  , padContentClip
  , scrollContentClip
  , scrollViewportClip2D
  , tagClippedSpans
  )
import NanoUI.Frame.Select (collectSelectDropdownSpans, tagSelectClippedSpans)
import NanoUI.Frame.TextEdit
  ( TextAreaGeom (..)
  , TextInputGeom (..)
  , collectTextEditMenuSpans
  , tagTextInputClippedSpans
  , textAreaGeom
  , textAreaValue
  , textInputGeom
  , syncTextInputScroll
  )
import NanoUI.Frame.SpanArena (SpanArena, pushSpan, resetSpanArena, spanArenaToList, spanArenaToListOccluded, withSpanArenaSnap)

collectTextSpans :: Context -> IO [(Rect, T.Text, Color, Color, Rect)]
collectTextSpans ctx = do
  floatCache <- buildFloatingAncestorMap ctx
  collectTextSpansCached ctx floatCache

collectOverlayTextSpans :: Context -> Input -> IO [(Rect, T.Text, Color, Color, Rect)]
collectOverlayTextSpans ctx inp = do
  floatCache <- buildFloatingAncestorMap ctx
  collectOverlayTextSpansCached ctx inp floatCache

collectRasterSpans :: Context -> Input -> IO ([(Rect, T.Text, Color, Color, Rect)], [(Rect, T.Text, Color, Color, Rect)])
collectRasterSpans ctx inp = do
  floatCache <- buildFloatingAncestorMap ctx
  base <- collectTextSpansCached ctx floatCache
  overlay <- collectOverlayTextSpansCached ctx inp floatCache
  pure (base, overlay)

collectTextSpansCached :: Context -> IM.IntMap (Maybe NodeType) -> IO [(Rect, T.Text, Color, Color, Rect)]
collectTextSpansCached ctx floatCache = do
  count <- arenaCount (ctxNodeArena ctx)
  let arena = ctxSpanBase ctx
  resetSpanArena arena
  withSpanArenaSnap arena $
    when (count > 0) $
      collectClippedSpans ctx floatCache 0 (Rect 0 0 1e9 1e9) arena
  panels <- floatingPanelRects ctx
  spanArenaToListOccluded panels arena

collectOverlayTextSpansCached :: Context -> Input -> IM.IntMap (Maybe NodeType) -> IO [(Rect, T.Text, Color, Color, Rect)]
collectOverlayTextSpansCached ctx inp floatCache = do
  let arena = ctxSpanOverlay ctx
  resetSpanArena arena
  withSpanArenaSnap arena $ do
    collectFloatingSpansInto ctx floatCache NodeWindow arena
    collectFloatingSpansInto ctx floatCache NodeModal arena
    collectFloatingSpansInto ctx floatCache NodePopup arena
    drops <- collectSelectDropdownSpans ctx inp
    menu <- collectTextEditMenuSpans ctx inp
    mapM_ (pushSpan5 arena) drops
    mapM_ (pushSpan5 arena) menu
  spanArenaToList arena

pushSpan5 :: SpanArena -> (Rect, T.Text, Color, Color, Rect) -> IO ()
pushSpan5 arena (r, t, fg, bg, c) = pushSpan arena r t fg bg c

widgetNodeCount :: Context -> IO Int
widgetNodeCount ctx = arenaCount (ctxNodeArena ctx)

{-# INLINE collectClippedSpans #-}
collectClippedSpans :: Context -> IM.IntMap (Maybe NodeType) -> NodeIdx -> Rect -> SpanArena -> IO ()
collectClippedSpans ctx floatCache idx clip arena = do
  nt <- getNodeType (ctxNodeArena ctx) idx
  unless (isFloatingNode nt) $
    collectClippedSpans' ctx floatCache idx nt clip arena

collectClippedSpans' :: Context -> IM.IntMap (Maybe NodeType) -> NodeIdx -> NodeType -> Rect -> SpanArena -> IO ()
collectClippedSpans' ctx floatCache idx nt clip arena = do
  (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
  pad <- getPadding (ctxNodeArena ctx) idx
  let nodeRect = Rect x y w h
      fm = ctxFontMetrics ctx
  mClipChildren <-
    if isScrollNode nt
      then do
        mLive <- getClipRect (ctxNodeArena ctx) idx
        case mLive of
          Just live ->
            pure (rectIntersect clip live)
          Nothing -> do
            si <- getStyleIdx (ctxNodeArena ctx) idx
            dir <- getDirection (ctxNodeArena ctx) idx
            slot <- scrollBarSlotOf (ctxNodeArena ctx) idx
            let cfg = decodeScrollConfig si
            content <-
              if isScrollStyle2D si
                then do
                  contentH <- getNodeValue (ctxNodeArena ctx) idx
                  contentW <- getScrollContentW (ctxNodeArena ctx) idx
                  pure $
                    scrollViewportClip2D fm slot cfg x y w h pad contentW contentH
                else do
                  contentSize <- getNodeValue (ctxNodeArena ctx) idx
                  pure $
                    scrollContentClip fm slot cfg dir x y w h pad contentSize
            pure (rectIntersect clip content)
      else
        if nt == NodePanel
          then pure (rectIntersect clip nodeRect)
          else pure (Just clip)
  case mClipChildren of
    Nothing -> pure ()
    Just clipHere -> do
      here <-
        case nt of
          NodeSelect -> do
            spans <- collectNodeTextSpans ctx floatCache idx
            pure (tagSelectClippedSpans clipHere x y w h fm spans)
          NodeTextInput -> do
            si' <- getStyleIdx (ctxNodeArena ctx) idx
            spans <- collectNodeTextSpans ctx floatCache idx
            if textInputBareMode si'
              then pure (tagClippedSpans clipHere spans)
              else pure (tagTextInputClippedSpans clipHere x y w h fm spans)
          _ -> tagClippedSpans clipHere <$> collectNodeTextSpans ctx floatCache idx
      mapM_ (\(r, t, fg, bg, c) -> pushSpan arena r t fg bg c) here
      walkChildSpans ctx floatCache idx clipHere arena

{-# INLINE walkChildSpans #-}
walkChildSpans :: Context -> IM.IntMap (Maybe NodeType) -> NodeIdx -> Rect -> SpanArena -> IO ()
walkChildSpans ctx floatCache idx clip arena = do
  fc <- getFirstChild (ctxNodeArena ctx) idx
  go fc
  where
    go ci
      | ci < 0 = pure ()
      | otherwise = do
          ns <- getNextSibling (ctxNodeArena ctx) ci
          -- Later siblings paint under earlier ones; walk reverse then collect.
          go ns
          collectClippedSpans ctx floatCache ci clip arena

{-# INLINE findAncestorMaxW #-}
findAncestorMaxW :: NodeArena -> NodeIdx -> IO Float
findAncestorMaxW na idx = go idx 0
  where
    go cur !padAccum = do
      p <- getParent na cur
      if p < 0
        then pure 1e9
        else do
          pad <- getPadding na p
          let padW = padL pad + padR pad
              padAccum' = padAccum + padW
          (_, _, pMaxW, _) <- getMinMax na p
          (pwTag, pwVal) <- getWidthSizing na p
          if pwTag == SizingFixed
            then pure (max 0 (pwVal - padAccum'))
            else if pMaxW < 1e8
              then pure (max 0 (pMaxW - padAccum'))
              else go p padAccum'

-- Placement uses glyph ink (alignedTextPen), not TTF_GetStringSize. Wrap
-- still measures with the host so line breaks stay on the TTF width.
collectNodeTextSpans :: Context -> IM.IntMap (Maybe NodeType) -> NodeIdx -> IO [(Rect, T.Text, Color, Color)]
collectNodeTextSpans ctx floatCache idx = do
  let arena = ctxNodeArena ctx
  nt <- getNodeType arena idx
  (x, y, w, h) <- getRect arena idx
  theme <- readIORef (ctxTheme ctx)
  if nt == NodeText
    then do
      raw <- getText arena idx
      si <- getStyleIdx arena idx
      mCustomCol <- getNodeFontColor arena idx
      fontSizeVal <- getNodeFontSize arena idx
      ax <- getAlignX arena idx
      (_, _, maxW, _) <- getMinMax arena idx
      (wTag, _) <- getWidthSizing arena idx
      isRowChild <- parentIsRow arena idx
      let fvar = textNodeFontVariant si
          (txt0, defaultFg, defaultBg) = floatingLabelPaint floatCache ctx idx theme fvar raw
          fg = fromMaybe defaultFg mCustomCol
          mStripe = tableStripeColor theme si
          paintBg = case mStripe of
            Just bg -> bg
            Nothing -> defaultBg
          stripeSpans = []
      effMaxW <-
        if maxW < 1e8
          then pure maxW
          else findAncestorMaxW arena idx
      cache <- readIORef (ctxSpanCache ctx)
      case IM.lookup idx cache of
        Just e
          | sceText e == txt0
              && sceFg e == fg
              && sceBg e == paintBg
              && sceStyle e == si
              && sceFontSize e == fontSizeVal
              && sceAlign e == fromEnum ax
              && sceWidthTag e == fromEnum wTag
              && (let r = sceRect e in rectX r == x && rectY r == y && rectW r == w && rectH r == h)
              && sceEffMaxW e == effMaxW
              && sceRowChild e == isRowChild -> pure (sceSpans e)
        _ -> do
          textSpans <-
            if T.null raw
              then pure []
              else do
                let fweight = textNodeFontWeight si
                    fstyle = textNodeFontStyle si
                    isBaseSans = fontSizeVal <= 0 && fweight == WeightNormal && fstyle == FontStyleNormal && fvar == FontRegular
                    isBaseMono = fontSizeVal <= 0 && fweight == WeightNormal && fstyle == FontStyleNormal && fvar == FontMono
                textFm <-
                  if isBaseSans
                    then pure (ctxFontMetrics ctx)
                    else if isBaseMono
                      then pure (ctxMonoFontMetrics ctx)
                      else fst <$> ctxResolveFont ctx fontSizeVal fweight fstyle fvar
                let (ix, _) =
                      case mStripe of
                        Just _ -> tableCellInset textFm
                        Nothing -> labelContentInset textFm
                    measureWord =
                      if isBaseSans
                        then \t -> fmap fst (ctxMeasureText ctx t)
                        else if isBaseMono
                          then \t -> pure (fst (measureText (ctxMonoFontMetrics ctx) t))
                          else \t -> fmap fst (ctxResolveMeasure ctx fontSizeVal fweight fstyle fvar t)
                tw0 <-
                  if isBaseSans
                    then fst <$> ctxMeasureText ctx txt0
                    else if isBaseMono
                      then pure (fst (measureText (ctxMonoFontMetrics ctx) txt0))
                      else fst <$> ctxResolveMeasure ctx fontSizeVal fweight fstyle fvar txt0
                let hasNewlines = T.any (== '\n') txt0
                    wrapCap
                      | effMaxW < 1e8 = max 0 effMaxW
                      | wTag == SizingGrow && w > 0 = w
                      | otherwise = effMaxW
                    canWrap = not isRowChild && wrapCap < 1e8
                    wrapW = max 0 (wrapCap - 2 * ix)
                    lineH = layoutLineHeight textFm
                if hasNewlines || (canWrap && wrapCap + 0.5 < tw0)
                  then do
                    textLines <- wrapTextLinesIO measureWord textFm txt0 wrapW
                    pure
                      [ ( Rect
                            tx
                            (centeredTextY textFm (y + onGrid (fmSnapScale textFm) (fromIntegral i * lineH)) lineH lineH)
                            used
                            lineH
                        , line
                        , fg
                        , paintBg
                        )
                      | (i, line) <- zip [(0 :: Int) ..] textLines
                      , let (tx, used) = alignedTextPen ax x w ix textFm line
                      ]
                  else do
                    let contentW = max 0 (w - 2 * ix)
                    dispTxt <-
                      if tw0 > contentW && contentW > 0 && (wTag == SizingGrow || maxW < 1e8)
                        then
                          if fvar == FontMono
                            then pure (truncateTextAdvance textFm contentW txt0)
                            else truncateTextIO measureWord contentW txt0
                        else pure txt0
                    let (tx, used) = alignedTextPen ax x w ix textFm dispTxt
                        py = centeredTextY textFm y h lineH
                    pure [(Rect tx py used lineH, dispTxt, fg, paintBg)]
          let spans = stripeSpans ++ textSpans
              key =
                SpanCacheEntry
                  { sceText = txt0
                  , sceFg = fg
                  , sceBg = paintBg
                  , sceStyle = si
                  , sceFontSize = fontSizeVal
                  , sceAlign = fromEnum ax
                  , sceWidthTag = fromEnum wTag
                  , sceRect = Rect x y w h
                  , sceEffMaxW = effMaxW
                  , sceRowChild = isRowChild
                  , sceSpans = spans
                  }
          writeIORef (ctxSpanCache ctx) (IM.insert idx key cache)
          pure spans
    else
      if isWidgetNode nt
        then widgetTextSpans ctx nt idx x y w h
        else pure []


widgetHitRect :: Context -> NodeType -> NodeIdx -> Float -> Float -> Float -> Float -> IO Rect
widgetHitRect ctx nt idx x y w h = do
  let fm = ctxFontMetrics ctx
  case nt of
    NodeTextInput -> do
      si <- getStyleIdx (ctxNodeArena ctx) idx
      if textInputSearchMode si || textInputBareMode si
        then pure (Rect x y w h)
        else pure (tigFieldRect (textInputGeom fm x y w h))
    NodeTextArea -> pure (tagFieldRect (textAreaGeom fm x y w h))
    NodeButton -> do
      si <- getStyleIdx (ctxNodeArena ctx) idx
      if isCloseButtonStyle si
        then pure (closeButtonHitRect fm x y w h)
        else pure (Rect x y w h)
    _ -> pure (Rect x y w h)

-- Hit rect: padded in the title bar.
closeButtonHitRect :: FontMetrics -> Float -> Float -> Float -> Float -> Rect
closeButtonHitRect _fm x y w h =
  -- Easier to tap; keep the target inside the title bar so inner east resize
  -- still works below the close control.
  Rect (x - 8) (y - 4) (w + 10) (h + 4)

widgetTextSpans ::
  Context -> NodeType -> NodeIdx -> Float -> Float -> Float -> Float -> IO [(Rect, T.Text, Color, Color)]
widgetTextSpans ctx nt idx x y w h = do
  style <- widgetVisualStyle ctx nt idx
  mFontColor <- getNodeFontColor (ctxNodeArena ctx) idx
  let fg = fromMaybe (styleFg style) mFontColor
      bg = styleBg style
  case nt of
    NodeTextInput -> do
      placements <- widgetTextPlacements ctx nt idx x y w h
      value <- textInputValue ctx idx
      focus <- textInputFocused ctx idx
      let placeholder = T.null value && not focus
          fieldFg
            | placeholder = lerpColor fg bg 0.40
            | otherwise = fg
      pure
        [ (Rect px py tw th, txt, fieldFg, bg)
        | (txt, px, py, tw, th) <- placements
        ]
    _ -> do
      placements <- widgetTextPlacements ctx nt idx x y w h
      pure
        [ (Rect px py tw th, txt, fg, bg)
        | (txt, px, py, tw, th) <- placements
        , not (T.null txt)
        ]

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
      pure $ case placement of
        Nothing -> []
        Just (WidgetTextPlacement txt px py tw th) -> [(txt, x + px, y + py, tw, th)]
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
cachedWidgetLabel ctx nt idx w h
  | cacheableWidgetLabel nt = do
      fontSizeVal <- getNodeFontSize (ctxNodeArena ctx) idx
      si <- getStyleIdx (ctxNodeArena ctx) idx
      txt <- displayText ctx nt idx
      ax <- if nt == NodeButton && isTableHeaderStyle si
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
  | otherwise = pure Nothing

-- All coordinates here are local. centeredTextY snaps the baseline offset,
-- not the origin; final device-pixel snapping stays in the draw backend.
computeWidgetLabel :: Context -> NodeType -> T.Text -> Int -> Float -> AlignX -> Float -> Float -> IO (Maybe WidgetTextPlacement)
computeWidgetLabel ctx nt txt si fontSizeVal ax w h
  | nt == NodeButton && isCloseButtonStyle si = pure Nothing
  | otherwise = do
      fm <- placementFont ctx fontSizeVal si
      (tw, th) <- measurePlacementText ctx fontSizeVal si fm txt
      let (ix, _) = widgetContentInset fm
          (tx, used) = case nt of
            NodeButton
              | isTableHeaderStyle si -> alignedTextPen ax 0 w (fst (tableCellInset fm)) fm txt
              | isMenuItemStyle si ->
                  let inset = textInputMenuItemPadX + ix
                   in (inset, min tw (max 0 (w - inset - ix)))
              | otherwise -> alignedTextPen AlignCenter 0 w 0 fm txt
            NodeSelect -> (ix, min tw (w - ix - selectChevronReserve))
            NodeTree ->
              let (_, depth, _, _) = treeDecodeStyle si
               in (fst (labelContentInset fm) + treeRowLeading fm depth, tw)
            _ -> (fst (labelContentInset fm) + checkboxLeading fm, tw)
      let !placement = WidgetTextPlacement txt tx (centeredTextY fm 0 h th) used th
      pure (Just placement)

placementFont :: Context -> Float -> Int -> IO FontMetrics
placementFont ctx sz si
  | sz <= 0 && weight == WeightNormal && style == FontStyleNormal
      && (variant == FontRegular || variant == FontMono) =
      pure (if variant == FontMono then ctxMonoFontMetrics ctx else ctxFontMetrics ctx)
  | otherwise = fst <$> ctxResolveFont ctx sz weight style variant
  where
    weight = textNodeFontWeight si
    style = textNodeFontStyle si
    variant = textNodeFontVariant si

measurePlacementText :: Context -> Float -> Int -> FontMetrics -> T.Text -> IO (Float, Float)
measurePlacementText ctx sz si fm txt
  | sz <= 0 && weight == WeightNormal && style == FontStyleNormal
      && (variant == FontRegular || variant == FontMono) =
      if variant == FontMono then pure (measureText fm txt) else ctxMeasureText ctx txt
  | otherwise = ctxResolveMeasure ctx sz weight style variant txt
  where
    weight = textNodeFontWeight si
    style = textNodeFontStyle si
    variant = textNodeFontVariant si

computeWidgetTextPlacements ::
  Context -> NodeType -> NodeIdx -> Float -> Float -> Float -> Float -> IO [(T.Text, Float, Float, Float, Float)]
computeWidgetTextPlacements ctx nt idx x y w h = do
  fontSizeVal <- getNodeFontSize (ctxNodeArena ctx) idx
  si <- getStyleIdx (ctxNodeArena ctx) idx
  fm <- placementFont ctx fontSizeVal si
  let (ix, iy) = widgetContentInset fm
      measureTxt = measurePlacementText ctx fontSizeVal si fm
  case nt of
    NodeColorPicker -> do
      let showAlpha = colorPickerAlphaMode si
          geom = colorPickerGeom showAlpha fm x y w h
      (cw, ch) <- measureTxt colorPickerCurrentLabel
      (nw, nh) <- measureTxt colorPickerNewLabel
      let currentLabelY = centeredTextY fm (cpgCurrentLabelY geom) (cpgLabelH geom) ch
          newLabelY = centeredTextY fm (cpgNewLabelY geom) (cpgLabelH geom) nh
      pure
        [ (colorPickerCurrentLabel, cpgPreviewX geom, currentLabelY, cw, ch)
        , (colorPickerNewLabel, cpgPreviewX geom, newLabelY, nw, nh)
        ]
    NodeSlider -> pure []
    NodeTextInput
      | textInputBareMode si -> do
          value <- textInputValue ctx idx
          focus <- textInputFocused ctx idx
          let fieldTxt = textInputFieldText "" value focus
              lineH = layoutLineHeight fm
          (fw, _) <- measureTxt fieldTxt
          scrollX <- syncTextInputScroll ctx idx x y w h
          pure
            [ ( fieldTxt
              , x + ix - scrollX
              , centeredTextY fm y h lineH
              , fw
              , lineH
              )
            ]
    NodeTextInput -> do
      ph <- getText (ctxNodeArena ctx) idx
      value <- textInputValue ctx idx
      focus <- textInputFocused ctx idx
      let geom = textInputGeom fm x y w h
          field = tigFieldRect geom
          fieldTxt = textInputFieldText ph value focus
          lineH = layoutLineHeight fm
      (fw, _) <- measureTxt fieldTxt
      scrollX <- syncTextInputScroll ctx idx x y w h
      pure
        [ (fieldTxt, x + ix - scrollX, centeredTextY fm (rectY field) (rectH field) lineH, fw, lineH)
        ]
    NodeTextArea -> do
      lbl <- getText (ctxNodeArena ctx) idx
      value <- textAreaValue ctx idx
      let geom = textAreaGeom fm x y w h
          field = tagFieldRect geom
          labelH = layoutLineHeight fm
      (lw, lh) <- measureTxt lbl
      (fw, _) <- measureTxt (if T.null value then " " else value)
      pure
        [ (lbl, x, centeredTextY fm y labelH lh, lw, lh)
        , (value, x + ix, rectY field + iy, fw, rectH field)
        ]
    NodeDrawing -> pure []
    _ -> do
      txt <- displayText ctx nt idx
      ax <- getAlignX (ctxNodeArena ctx) idx
      (_tw, th) <- measureTxt txt
      let (tx, used) = alignedTextPen ax x w ix fm txt
      pure [(txt, tx, centeredTextY fm y h th, used, th)]

sliderValue :: Context -> NodeIdx -> IO Float
sliderValue ctx idx = do
  wid <- getWidgetId (ctxNodeArena ctx) idx
  store <- getStore ctx
  pure (IM.findWithDefault 0 (intKey wid) (storeFloat store))

-- Returns a style whose background already reflects hover/active state, so the
-- rect fill and the text cells agree on one color.
collectFloatingSpansInto :: Context -> IM.IntMap (Maybe NodeType) -> NodeType -> SpanArena -> IO ()
collectFloatingSpansInto ctx floatCache wanted arena = do
  count <- arenaCount (ctxNodeArena ctx)
  let fm = ctxFontMetrics ctx
      go !idx
        | idx >= count = pure ()
        | otherwise = do
            nt <- getNodeType (ctxNodeArena ctx) idx
            if nt /= wanted
              then go (idx + 1)
              else do
                (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
                pad <- getPadding (ctxNodeArena ctx) idx
                dir <- getDirection (ctxNodeArena ctx) idx
                si <- getStyleIdx (ctxNodeArena ctx) idx
                slot <- scrollBarSlotOf (ctxNodeArena ctx) idx
                let cfg = decodeScrollConfig si
                clip <-
                  if not (isScrollNode nt)
                    then pure $ padContentClip fm x y w h pad
                    else if isScrollStyle2D si
                      then do
                        contentH <- getNodeValue (ctxNodeArena ctx) idx
                        contentW <- getScrollContentW (ctxNodeArena ctx) idx
                        pure $
                          scrollViewportClip2D fm slot cfg x y w h pad contentW contentH
                      else do
                        contentSize <- getNodeValue (ctxNodeArena ctx) idx
                        pure $
                          scrollContentClip fm slot cfg dir x y w h pad contentSize
                walkChildSpans ctx floatCache idx clip arena
                go (idx + 1)
  go 0
