{-# LANGUAGE DataKinds #-}

module NanoUI.Frame.Spans
  ( collectTextSpans
  , collectOverlayTextSpans
  , collectRasterSpans
  , widgetNodeCount
  , widgetHitRect
  , widgetTextSpans
  , widgetTextPlacements
  , collectNodeTextSpans
  , sliderValue
  , terminalSeparatorSpans
  , walkChildSpans
  , terminalScrollCapSpans
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
  , WidgetStore (..)
  , getScrollOffset
  , getStore
  , intKey
  , slotCursor
  , slotKey
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
  , sliderTrackBounds
  , textDisplayWidth
  , treeRowLeading
  , truncateTextAdvance
  , truncateTextIO
  , widgetContentInset
  , wrapTextLines
  , wrapTextLinesIO
  , measureText
  )
import NanoUI.Types (HostProfile, isCellHost)
import NanoUI.Icons (iconScrollDown, iconScrollUp, terminalPaintColumns)
import NanoUI.Input (Input)
import NanoUI.Layout.Arena
  ( DirTag (..)
  , NodeArena
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
import NanoUI.Style (AlignX (..), FontStyle (..), FontVariant (..), FontWeight (..), Padding (..), Style (..), Theme (..), styleBg, styleFg, themeSeparator, themeWindow)
import NanoUI.Types (Color (..), Rect (..), colorRGBA, lerpColor, onGrid, rectH, rectIntersect, rectW, rectX, rectY)
import NanoUI.WidgetText (isCloseButtonStyle, isMenuItemStyle, isTableHeaderStyle, textInputBareMode, textInputSearchMode, textInputSearchTerminalText)
import NanoUI.WidgetText
  ( colorPickerCurrentLabel
  , colorPickerNewLabel
  , selectChevronReserve
  , sliderValueText
  , textInputFieldText
  , textInputTerminalText
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
  , scrollChromeActive
  , scrollContentClip
  , scrollViewportClip2D
  , tagClippedSpans
  , terminalModalOuterClip
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
import NanoUI.Frame.Scroll (scrollBarLayout, ScrollBarLayout (..))
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
                    scrollViewportClip2D (ctxHostProfile ctx) fm slot cfg x y w h pad contentW contentH
                else do
                  contentSize <- getNodeValue (ctxNodeArena ctx) idx
                  pure $
                    scrollContentClip (ctxHostProfile ctx) fm slot cfg dir x y w h pad contentSize
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
            pure (tagSelectClippedSpans (ctxHostProfile ctx) clipHere x y w h fm spans)
          NodeTextInput
            | not (isCellHost (ctxHostProfile ctx)) -> do
                si' <- getStyleIdx (ctxNodeArena ctx) idx
                spans <- collectNodeTextSpans ctx floatCache idx
                if textInputBareMode si'
                  then pure (tagClippedSpans clipHere spans)
                  else pure (tagTextInputClippedSpans (ctxHostProfile ctx) clipHere x y w h fm spans)
          NodeSeparator
            | isCellHost (ctxHostProfile ctx) -> do
                theme <- readIORef (ctxTheme ctx)
                pure
                  ( tagClippedSpans
                      (Rect x y w h)
                      (terminalSeparatorSpans theme x y w h)
                  )
          _ -> tagClippedSpans clipHere <$> collectNodeTextSpans ctx floatCache idx
      mapM_ (\(r, t, fg, bg, c) -> pushSpan arena r t fg bg c) here
      -- TUI modal chrome does not scroll (the inner body scroller does), so it
      -- has no track to cap.
      when (isCellHost (ctxHostProfile ctx) && isScrollNode nt) $ do
        si <- getStyleIdx (ctxNodeArena ctx) idx
        let cfg = decodeScrollConfig si
            padClip = padContentClip (ctxHostProfile ctx) fm x y w h pad
            innerH = rectH padClip
        contentSize <- getNodeValue (ctxNodeArena ctx) idx
        when (scrollChromeActive cfg (isScrollStyle2D si) DirColumn contentSize innerH) $ do
          caps <- terminalScrollCapSpans ctx idx x y w h pad clip
          mapM_ (\(r, t, fg, bg, c) -> pushSpan arena r t fg bg c) caps
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
          cellHost = isCellHost (ctxHostProfile ctx)
          (txt0, defaultFg, defaultBg) = floatingLabelPaint floatCache ctx idx theme fvar raw
          fg = fromMaybe defaultFg mCustomCol
          fgFill = fg
          mStripe = tableStripeColor theme si
          paintBg = case mStripe of
            Just bg -> bg
            Nothing -> defaultBg
          stripeSpans =
            case mStripe of
              Just bg | cellHost ->
                let wi = max 1 (round w :: Int)
                    hi = max 1 (round h :: Int)
                    ox = fromIntegral (round x :: Int)
                    oy = fromIntegral (round y :: Int)
                 in
                  [ ( Rect ox (oy + fromIntegral r) (fromIntegral wi) 1
                    , T.replicate wi " "
                    , fgFill
                    , bg
                    )
                  | r <- [0 .. hi - 1]
                  ]
              _ -> []
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
              && sceRowChild e == isRowChild
              && sceCellHost e == cellHost -> pure (sceSpans e)
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
                        Just _ -> tableCellInset (ctxHostProfile ctx) textFm
                        Nothing -> labelContentInset (ctxHostProfile ctx) textFm
                    measureWord =
                      if isBaseSans
                        then \t -> fmap fst (ctxMeasureText ctx t)
                        else if isBaseMono
                          then \t -> pure (fst (measureText (ctxHostProfile ctx) (ctxMonoFontMetrics ctx) t))
                          else \t -> fmap fst (ctxResolveMeasure ctx fontSizeVal fweight fstyle fvar t)
                tw0 <-
                  if isBaseSans
                    then fst <$> ctxMeasureText ctx txt0
                    else if isBaseMono
                      then pure (fst (measureText (ctxHostProfile ctx) (ctxMonoFontMetrics ctx) txt0))
                      else fst <$> ctxResolveMeasure ctx fontSizeVal fweight fstyle fvar txt0
                let hasNewlines = T.any (== '\n') txt0
                    wrapCap
                      | effMaxW < 1e8 = max 0 effMaxW
                      | wTag == SizingGrow && w > 0 = w
                      | otherwise = effMaxW
                    canWrap = not isRowChild && wrapCap < 1e8
                    wrapW = max 0 (wrapCap - 2 * ix)
                    lineH = layoutLineHeight (ctxHostProfile ctx) textFm
                if hasNewlines || (canWrap && wrapCap + 0.5 < tw0)
                  then do
                    textLines <-
                      if isCellHost (ctxHostProfile ctx)
                        then pure (wrapTextLines (ctxHostProfile ctx) textFm txt0 wrapW)
                        else wrapTextLinesIO measureWord textFm txt0 wrapW
                    pure
                      [ ( Rect
                            tx
                            (centeredTextY (ctxHostProfile ctx) textFm (y + onGrid (fmSnapScale textFm) (fromIntegral i * lineH)) lineH lineH)
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
                          if isCellHost (ctxHostProfile ctx) || fvar == FontMono
                            then pure (truncateTextAdvance textFm contentW txt0)
                            else truncateTextIO measureWord contentW txt0
                        else pure txt0
                    let (tx, used) = alignedTextPen ax x w ix textFm dispTxt
                        py = centeredTextY (ctxHostProfile ctx) textFm y h lineH
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
                  , sceCellHost = cellHost
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
  if not (isCellHost (ctxHostProfile ctx))
    then
      case nt of
        NodeTextInput -> do
          si <- getStyleIdx (ctxNodeArena ctx) idx
          if textInputSearchMode si || textInputBareMode si
            then pure (Rect x y w h)
            else pure (tigFieldRect (textInputGeom (ctxHostProfile ctx) fm x y w h))
        NodeTextArea -> pure (tagFieldRect (textAreaGeom (ctxHostProfile ctx) fm x y w h))
        NodeButton -> do
          si <- getStyleIdx (ctxNodeArena ctx) idx
          if isCloseButtonStyle si
            then pure (closeButtonHitRect (ctxHostProfile ctx) fm x y w h)
            else pure (Rect x y w h)
        _ -> pure (Rect x y w h)
    else
      case nt of
        NodeSlider -> do
          txt <- getText (ctxNodeArena ctx) idx
          pure (sliderTrackBounds (ctxHostProfile ctx) fm txt x y w h)
        NodeButton -> do
          si <- getStyleIdx (ctxNodeArena ctx) idx
          txt <- displayText ctx nt idx
          if isCloseButtonStyle si
            then pure (closeButtonHitRect (ctxHostProfile ctx) fm x y w h)
            else pure (terminalTextHitRect (ctxHostProfile ctx) fm x y h txt True)
        _ | nt == NodeCheckbox || nt == NodeRadio || nt == NodeTree -> do
          txt <- displayText ctx nt idx
          pure (terminalTextHitRect (ctxHostProfile ctx) fm x y h txt True)
        _ | nt == NodeSelect || nt == NodeTextInput -> do
          txt <- displayText ctx nt idx
          pure (terminalTextHitRect (ctxHostProfile ctx) fm x y h txt False)
        _ -> pure (Rect x y w h)

terminalTextHitRect :: HostProfile -> FontMetrics -> Float -> Float -> Float -> T.Text -> Bool -> Rect
terminalTextHitRect host fm x y h txt atOrigin =
  let (ix, _) = widgetContentInset host fm
      tw = textDisplayWidth host fm txt
      th = layoutLineHeight host fm
      tx = if atOrigin then x else x + ix
      ty = centeredTextY host fm y h th
   in Rect tx ty tw th

-- Paint rect: one cell, centered in the 3-cell slot (Win32 / ASCII "X").
terminalClosePaintRect :: HostProfile -> FontMetrics -> Float -> Float -> Float -> Float -> T.Text -> Rect
terminalClosePaintRect host fm x y w h txt =
  let tw = fromIntegral (terminalPaintColumns txt)
      th = layoutLineHeight host fm
      lo = x
      hi = x + w - tw
      raw = x + (w - tw) / 2
      lead = fromIntegral (round (max lo (min hi raw)) :: Int)
   in Rect lead (centeredTextY host fm y h th) tw th

-- Hit rect: full close slot (cell host) or padded in the title bar (pixel host).
closeButtonHitRect :: HostProfile -> FontMetrics -> Float -> Float -> Float -> Float -> Rect
closeButtonHitRect host _fm x y w h =
  if isCellHost host
    then Rect x y w h
    else
      -- Easier to tap; keep the target inside the title bar so inner east resize
      -- still works below the close control.
      Rect (x - 8) (y - 4) (w + 10) (h + 4)

widgetTextSpans ::
  Context -> NodeType -> NodeIdx -> Float -> Float -> Float -> Float -> IO [(Rect, T.Text, Color, Color)]
widgetTextSpans ctx nt idx x y w h = do
  fm <- pure (ctxFontMetrics ctx)
  terminal <- pure (isCellHost (ctxHostProfile ctx))
  style <- widgetVisualStyle ctx nt idx
  mFontColor <- getNodeFontColor (ctxNodeArena ctx) idx
  let fg = fromMaybe (styleFg style) mFontColor
      bg = styleBg style
  if terminal
    then do
      txt <- displayText ctx nt idx
      if T.null txt
        then pure []
        else do
          let (ix, _) = widgetContentInset (ctxHostProfile ctx) fm
          (tw, th) <- ctxMeasureText ctx txt
          isClose <-
            if nt == NodeButton
              then isCloseButtonStyle <$> getStyleIdx (ctxNodeArena ctx) idx
              else pure False
          if isClose
            then
              let closeRect = terminalClosePaintRect (ctxHostProfile ctx) fm x y w h txt
               in pure [(closeRect, txt, fg, bg)]
            else do
              let tx =
                    if nt == NodeButton || nt == NodeCheckbox || nt == NodeRadio || nt == NodeTree
                      then x
                      else x + ix
                  textSpan =
                    [ ( Rect tx (centeredTextY (ctxHostProfile ctx) fm y h th) tw th
                      , txt
                      , fg
                      , bg
                      )
                    ]
              pure textSpan
    else do
      case nt of
        NodeTextInput -> do
          si <- getStyleIdx (ctxNodeArena ctx) idx
          placements <- widgetTextPlacements ctx nt idx x y w h
          value <- textInputValue ctx idx
          focus <- textInputFocused ctx idx
          theme <- readIORef (ctxTheme ctx)
          let windowBg = themeWindow theme
              labelFg = lerpColor fg windowBg 0.32
              placeholder = T.null value && not focus
              fieldFg
                | placeholder = lerpColor fg bg 0.40
                | otherwise = fg
          if textInputBareMode si
            then
              pure
                [ (Rect px py tw th, txt, fieldFg, bg)
                | (txt, px, py, tw, th) <- placements
                ]
            else
              case placements of
                (lblPl : fieldPl : _) -> do
                  let (lbl, lx, ly, lw, lh) = lblPl
                      (field, fx, fy, fw, fh) = fieldPl
                  pure
                    [ (Rect lx ly lw lh, lbl, labelFg, windowBg)
                    , (Rect fx fy fw fh, field, fieldFg, bg)
                    ]
                [lblPl] -> do
                  let (lbl, lx, ly, lw, lh) = lblPl
                  pure [(Rect lx ly lw lh, lbl, labelFg, windowBg)]
                _ -> pure []
        _ -> do
          placements <- widgetTextPlacements ctx nt idx x y w h
          pure
            [ (Rect px py tw th, txt, fg, bg)
            | (txt, px, py, tw, th) <- placements
            , not (T.null txt)
            ]

widgetTextPlacements ::
  Context -> NodeType -> NodeIdx -> Float -> Float -> Float -> Float -> IO [(T.Text, Float, Float, Float, Float)]
widgetTextPlacements ctx nt idx x y w h = do
  terminal <- pure (isCellHost (ctxHostProfile ctx))
  fontSizeVal <- getNodeFontSize (ctxNodeArena ctx) idx
  si <- getStyleIdx (ctxNodeArena ctx) idx
  let fweight = textNodeFontWeight si
      fstyle  = textNodeFontStyle si
      fvar    = textNodeFontVariant si
      isBaseSans = fontSizeVal <= 0 && fweight == WeightNormal && fstyle == FontStyleNormal && fvar == FontRegular
      isBaseMono = fontSizeVal <= 0 && fweight == WeightNormal && fstyle == FontStyleNormal && fvar == FontMono
  fm <-
    if isBaseSans
      then pure (ctxFontMetrics ctx)
      else if isBaseMono
        then pure (ctxMonoFontMetrics ctx)
        else fst <$> ctxResolveFont ctx fontSizeVal fweight fstyle fvar
  let (ix, iy) = widgetContentInset (ctxHostProfile ctx) fm
      measureTxt t =
        if isBaseSans
          then ctxMeasureText ctx t
          else if isBaseMono
            then pure (measureText (ctxHostProfile ctx) (ctxMonoFontMetrics ctx) t)
            else ctxResolveMeasure ctx fontSizeVal fweight fstyle fvar t
  case nt of
    NodeButton -> do
      if not terminal && isCloseButtonStyle si
        then pure []
        else do
          txt <- displayText ctx nt idx
          (tw, th) <- measureTxt txt
          if isTableHeaderStyle si
            then do
              ax <- getAlignX (ctxNodeArena ctx) idx
              let (labelIx, _) = tableCellInset (ctxHostProfile ctx) fm
                  (tx, used) = alignedTextPen ax x w labelIx fm txt
              pure [(txt, tx, centeredTextY (ctxHostProfile ctx) fm y h th, used, th)]
            else
              if isMenuItemStyle si
                then do
                  -- Match the text-field context menu pen exactly: label inset
                  -- of textInputMenuItemPadX plus the widget content inset.
                  let inset = textInputMenuItemPadX + ix
                      tx = x + inset
                      avail = max 0 (w - inset - ix)
                  pure [(txt, tx, centeredTextY (ctxHostProfile ctx) fm y h th, min tw avail, th)]
                else do
                  let (tx, used) = alignedTextPen AlignCenter x w 0 fm txt
                  pure [(txt, tx, centeredTextY (ctxHostProfile ctx) fm y h th, used, th)]
    NodeSelect -> do
      txt <- displayText ctx nt idx
      (tw, th) <- measureTxt txt
      pure [(txt, x + ix, centeredTextY (ctxHostProfile ctx) fm y h th, min tw (w - ix - selectChevronReserve), th)]
    NodeColorPicker -> do
      if terminal
        then do
          txt <- displayText ctx nt idx
          (tw, th) <- measureTxt txt
          pure [(txt, x + ix, centeredTextY (ctxHostProfile ctx) fm y h th, tw, th)]
        else do
          lbl <- getText (ctxNodeArena ctx) idx
          let showAlpha = colorPickerAlphaMode si
              geom = colorPickerGeom showAlpha (ctxHostProfile ctx) fm x y w h
              (lx, ly) = labelContentInset (ctxHostProfile ctx) fm
              host = ctxHostProfile ctx
          (lw, lh) <- measureTxt lbl
          (cw, ch) <- measureTxt colorPickerCurrentLabel
          (nw, nh) <- measureTxt colorPickerNewLabel
          let currentLabelY = centeredTextY host fm (cpgCurrentLabelY geom) (cpgLabelH geom) ch
              newLabelY = centeredTextY host fm (cpgNewLabelY geom) (cpgLabelH geom) nh
          pure
            [ (lbl, x + lx, y + ly, lw, lh)
            , (colorPickerCurrentLabel, cpgPreviewX geom, currentLabelY, cw, ch)
            , (colorPickerNewLabel, cpgPreviewX geom, newLabelY, nw, nh)
            ]
    _ | nt == NodeCheckbox || nt == NodeRadio -> do
      txt <- displayText ctx nt idx
      (tw, th) <- measureTxt txt
      let (cx, _) =
            if terminal
              then widgetContentInset (ctxHostProfile ctx) fm
              else labelContentInset (ctxHostProfile ctx) fm
          tx = x + cx + checkboxLeading (ctxHostProfile ctx) fm
          ty = centeredTextY (ctxHostProfile ctx) fm y h th
      pure [(txt, tx, ty, tw, th)]
    NodeTree -> do
      txt <- displayText ctx nt idx
      (tw, th) <- measureTxt txt
      let (_, depth, _, _) = treeDecodeStyle si
          (cx, _) =
            if terminal
              then widgetContentInset (ctxHostProfile ctx) fm
              else labelContentInset (ctxHostProfile ctx) fm
          tx = x + cx + treeRowLeading (ctxHostProfile ctx) fm depth
          ty = centeredTextY (ctxHostProfile ctx) fm y h th
      pure [(txt, tx, ty, tw, th)]
    NodeSlider -> do
      lbl <- displayText ctx nt idx
      if terminal
        then do
          (lw, lh) <- measureTxt lbl
          let ty = centeredTextY (ctxHostProfile ctx) fm y lh lh
          pure [(lbl, x + ix, ty, lw, lh)]
        else do
          val <- sliderValue ctx idx
          let valTxt = sliderValueText val
              (lx, _) = labelContentInset (ctxHostProfile ctx) fm
          (lw, lh) <- measureTxt lbl
          (vw, vh) <- measureTxt valTxt
          let ty = centeredTextY (ctxHostProfile ctx) fm y lh lh
          pure
            [ (lbl, x + lx, ty, lw, lh)
            , (valTxt, x + w - lx - vw, centeredTextY (ctxHostProfile ctx) fm y vh vh, vw, vh)
            ]
    NodeTextInput
      | not terminal, textInputBareMode si -> do
          value <- textInputValue ctx idx
          focus <- textInputFocused ctx idx
          let fieldTxt = textInputFieldText "" value focus
              lineH = layoutLineHeight (ctxHostProfile ctx) fm
          (fw, _) <- measureTxt fieldTxt
          scrollX <- syncTextInputScroll ctx idx x y w h
          pure
            [ ( fieldTxt
              , x + ix - scrollX
              , centeredTextY (ctxHostProfile ctx) fm y h lineH
              , fw
              , lineH
              )
            ]
    NodeTextInput -> do
      lbl <- getText (ctxNodeArena ctx) idx
      value <- textInputValue ctx idx
      focus <- textInputFocused ctx idx
      if terminal
        then do
          wid <- getWidgetId (ctxNodeArena ctx) idx
          store <- getStore ctx
          styleBits <- getStyleIdx (ctxNodeArena ctx) idx
          let cursor = IM.findWithDefault (T.length value) (slotKey slotCursor (intKey wid)) (storeInt store)
              shown =
                if textInputSearchMode styleBits
                  then textInputSearchTerminalText lbl value cursor focus
                  else textInputTerminalText lbl value cursor focus
          (tw, th) <- measureTxt shown
          pure [(shown, x + ix, centeredTextY (ctxHostProfile ctx) fm y h th, tw, th)]
        else do
          let geom = textInputGeom (ctxHostProfile ctx) fm x y w h
              field = tigFieldRect geom
              fieldTxt = textInputFieldText lbl value focus
              labelH = layoutLineHeight (ctxHostProfile ctx) fm
          (lw, lh) <- measureTxt lbl
          (fw, _) <- measureTxt fieldTxt
          let lineH = layoutLineHeight (ctxHostProfile ctx) fm
          scrollX <- syncTextInputScroll ctx idx x y w h
          pure
            [ (lbl, x, centeredTextY (ctxHostProfile ctx) fm y labelH lh, lw, lh)
            , (fieldTxt, x + ix - scrollX, centeredTextY (ctxHostProfile ctx) fm (rectY field) (rectH field) lineH, fw, lineH)
            ]
    NodeTextArea -> do
      lbl <- getText (ctxNodeArena ctx) idx
      value <- textAreaValue ctx idx
      if terminal
        then do
          (tw, th) <- measureTxt value
          pure [(value, x + ix, centeredTextY (ctxHostProfile ctx) fm y h th, tw, th)]
        else do
          let geom = textAreaGeom (ctxHostProfile ctx) fm x y w h
              field = tagFieldRect geom
              labelH = layoutLineHeight (ctxHostProfile ctx) fm
          (lw, lh) <- measureTxt lbl
          (fw, _) <- measureTxt (if T.null value then " " else value)
          pure
            [ (lbl, x, centeredTextY (ctxHostProfile ctx) fm y labelH lh, lw, lh)
            , (value, x + ix, rectY field + iy, fw, rectH field)
            ]
    NodeDrawing -> pure []
    _ -> do
      txt <- displayText ctx nt idx
      ax <- getAlignX (ctxNodeArena ctx) idx
      (_tw, th) <- measureTxt txt
      let (tx, used) = alignedTextPen ax x w ix fm txt
      pure [(txt, tx, centeredTextY (ctxHostProfile ctx) fm y h th, used, th)]

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
                  if isCellHost (ctxHostProfile ctx) && nt == NodeModal
                    then pure $ terminalModalOuterClip (ctxHostProfile ctx) fm x y w h pad
                    else if not (isScrollNode nt)
                      then pure $ padContentClip (ctxHostProfile ctx) fm x y w h pad
                      else if isScrollStyle2D si
                        then do
                          contentH <- getNodeValue (ctxNodeArena ctx) idx
                          contentW <- getScrollContentW (ctxNodeArena ctx) idx
                          pure $
                            scrollViewportClip2D (ctxHostProfile ctx) fm slot cfg x y w h pad contentW contentH
                        else do
                          contentSize <- getNodeValue (ctxNodeArena ctx) idx
                          pure $
                            scrollContentClip (ctxHostProfile ctx) fm slot cfg dir x y w h pad contentSize
                walkChildSpans ctx floatCache idx clip arena
                go (idx + 1)
  go 0

terminalSeparatorSpans :: Theme -> Float -> Float -> Float -> Float -> [(Rect, T.Text, Color, Color)]
terminalSeparatorSpans theme x y w h =
  let sepFg = themeSeparator theme
      sepBg = colorRGBA 0 0 0 0
      hair = 1 :: Float
   in if w >= h
        then
          let wi :: Int
              wi = max 1 (round w)
              rowY :: Int
              rowY = round (y + (h - hair) / 2)
              rowX :: Int
              rowX = round x
           in
            [ ( Rect (fromIntegral rowX) (fromIntegral rowY) (fromIntegral wi) hair
              , T.replicate wi (T.singleton '\x2500')
              , sepFg
              , sepBg
              )
            ]
        else
          let hi :: Int
              hi = max 1 (round h)
              colX :: Int
              colX = round (x + (w - hair) / 2)
              colY :: Int
              colY = round y
           in
            [ ( Rect (fromIntegral colX) (fromIntegral (colY + i)) hair hair
              , T.singleton '\x2502'
              , sepFg
              , sepBg
              )
            | i <- [0 .. hi - 1]
            ]

-- Glyph tiers cap a vertical TUI scrollbar with carets. ASCII draws none, and a
-- track under three cells has no room for them.
terminalScrollCapSpans ::
  Context ->
  NodeIdx ->
  Float ->
  Float ->
  Float ->
  Float ->
  Padding ->
  Rect ->
  IO [(Rect, T.Text, Color, Color, Rect)]
terminalScrollCapSpans ctx idx x y w h pad clip
  | T.null up && T.null down = pure []
  | otherwise = do
      dir <- getDirection (ctxNodeArena ctx) idx
      if dir /= DirColumn
        then pure []
        else do
          wid <- getWidgetId (ctxNodeArena ctx) idx
          contentSize <- getNodeValue (ctxNodeArena ctx) idx
          off <- getScrollOffset ctx wid
          slot <- scrollBarSlotOf (ctxNodeArena ctx) idx
          case scrollBarLayout (ctxHostProfile ctx) fm slot dir x y w h pad contentSize off of
            Just layout
              | rectH (sbTrack layout) >= 3
              , let trackW = rectW (sbTrack layout)
              , all (\t -> T.null t || fromIntegral (terminalPaintColumns t) <= trackW) [up, down] ->
                  do
              theme <- readIORef (ctxTheme ctx)
              let track = sbTrack layout
                  fg = themeSeparator theme
                  bg = colorRGBA 0 0 0 0
                  cell ty txt =
                    let pw = fromIntegral (terminalPaintColumns txt)
                     in (Rect (rectX track) ty pw 1, txt, fg, bg)
              pure $
                tagClippedSpans
                  clip
                  [ cell ty txt
                  | (ty, txt) <-
                      [ (rectY track, up)
                      , (rectY track + rectH track - 1, down)
                      ]
                  , not (T.null txt)
                  ]
            _ -> pure []
  where
    fm = ctxFontMetrics ctx
    icons = ctxIcons ctx
    up = iconScrollUp icons
    down = iconScrollDown icons
