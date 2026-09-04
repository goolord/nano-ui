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
  , collectFloatingSpans
  , terminalSeparatorSpans
  , walkChildSpans
  , terminalScrollCapSpans
  ) where


import Control.Monad (unless, when)
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import NanoUI.Widgets.ColorPicker
  ( ColorPickerGeom (..)
  , colorPickerDefaultColor
  , colorPickerGeom
  , widgetStoreColor
  )
import NanoUI.Context
  ( Context (..)
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
  , measureText
  , sliderTrackBounds
  , textDisplayWidth
  , treeRowLeading
  , truncateTextAdvance
  , truncateTextIO
  , widgetContentInset
  , wrapTextLines
  , wrapTextLinesIO
  )
import NanoUI.Types (HostProfile, isCellHost)
import NanoUI.Icons (iconScrollDown, iconScrollUp, terminalPaintColumns)
import NanoUI.Input (Input)
import NanoUI.Layout.Arena
  ( DirTag (..)
  , NodeIdx
  , NodeType (..)
  , SizingTag (..)
  , arenaCount
  , getAlignX
  , getAspect
  , getClipRect
  , getDirection
  , getFirstChild
  , getMinMax
  , getNextSibling
  , getNodeType
  , getNodeValue
  , getPadding
  , getRect
  , getStyleIdx
  , getText
  , getWidthSizing
  , getWidgetId
  , getWrap
  , parentIsNonWrapRow
  , isFloatingNode
  , isScrollNode
  , isWidgetNode
  )
import NanoUI.Layout.Solve (scrollBarSlotOf)
import NanoUI.Style (AlignX (..), FontVariant (..), Padding (..), Style (..), Theme (..), styleBg, styleFg, themeSeparator, themeWindow)
import NanoUI.Types (Color (..), Rect (..), colorRGBA, lerpColor, rectH, rectIntersect, rectW, rectX, rectY)
import NanoUI.WidgetText (isCloseButtonStyle, isTableHeaderStyle)
import NanoUI.WidgetText
  ( colorPickerCurrentLabel
  , colorPickerNewLabel
  , colorPickerToHex
  , selectChevronReserve
  , sliderLabelText
  , sliderValueText
  , textInputFieldText
  , textInputTerminalText
  , treeDecodeStyle
  , tableStripeColor
  , textNodeFontVariant
  )
import NanoUI.Frame.Chrome
  ( buildFloatingAncestorMap
  , displayText
  , floatingLabelPaint
  , textInputFocused
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
import NanoUI.Frame.TextEdit (TextInputGeom (..), collectTextEditMenuSpans, tagTextInputClippedSpans, textInputGeom)
import NanoUI.Frame.TextArea (TextAreaGeom (..), textAreaGeom, textAreaValue)
import NanoUI.Frame.Scroll (scrollBarLayout, ScrollBarLayout (..))
import NanoUI.Frame.SpanArena (SpanArena, pushSpan, resetSpanArena, spanArenaToList, spanArenaToListOccluded)

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
  when (count > 0) $
    collectClippedSpans ctx floatCache 0 (Rect 0 0 1e9 1e9) arena
  panels <- floatingPanelRects ctx
  spanArenaToListOccluded panels arena

collectOverlayTextSpansCached :: Context -> Input -> IM.IntMap (Maybe NodeType) -> IO [(Rect, T.Text, Color, Color, Rect)]
collectOverlayTextSpansCached ctx inp floatCache = do
  let arena = ctxSpanOverlay ctx
  resetSpanArena arena
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
                  contentW <- getAspect (ctxNodeArena ctx) idx
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
                spans <- collectNodeTextSpans ctx floatCache idx
                pure (tagTextInputClippedSpans (ctxHostProfile ctx) clipHere x y w h fm spans)
          NodeSeparator
            | isCellHost (ctxHostProfile ctx) ->
                pure
                  ( tagClippedSpans
                      (Rect x y w h)
                      (terminalSeparatorSpans (ctxTheme ctx) x y w h)
                  )
          _ -> tagClippedSpans clipHere <$> collectNodeTextSpans ctx floatCache idx
      mapM_ (\(r, t, fg, bg, c) -> pushSpan arena r t fg bg c) here
      -- TUI modal chrome does not scroll (the inner body scroller does), so it
      -- has no track to cap.
      when (isCellHost (ctxHostProfile ctx) && isScrollNode nt && nt /= NodeModal) $ do
        si <- getStyleIdx (ctxNodeArena ctx) idx
        let cfg = decodeScrollConfig si
            padClip = padContentClip (ctxHostProfile ctx) fm x y w h pad
            innerH = rectH padClip
        contentSize <- getNodeValue (ctxNodeArena ctx) idx
        when (scrollChromeActive cfg (isScrollStyle2D si) DirColumn contentSize innerH) $ do
          caps <- terminalScrollCapSpans ctx idx x y w h pad clip
          mapM_ (\(r, t, fg, bg, c) -> pushSpan arena r t fg bg c) caps
      walkChildSpans ctx floatCache idx clipHere arena

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

-- Placement uses glyph ink (alignedTextPen), not TTF_GetStringSize. Wrap
-- still measures with the host so line breaks stay on the TTF width.
collectNodeTextSpans :: Context -> IM.IntMap (Maybe NodeType) -> NodeIdx -> IO [(Rect, T.Text, Color, Color)]
collectNodeTextSpans ctx floatCache idx = do
  nt <- getNodeType (ctxNodeArena ctx) idx
  (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
  let fm = ctxFontMetrics ctx
      theme = ctxTheme ctx
  if nt == NodeText
    then do
      raw <- getText (ctxNodeArena ctx) idx
      si <- getStyleIdx (ctxNodeArena ctx) idx
      let mStripe = tableStripeColor theme si
          stripeSpans =
            case mStripe of
              Just bg | isCellHost (ctxHostProfile ctx) ->
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
          fvar = textNodeFontVariant si
          (txt0, fg, defaultBg) = floatingLabelPaint floatCache ctx idx theme fvar raw
          fgFill = fg
          paintBg = case mStripe of
            Just bg -> bg
            Nothing -> defaultBg
      if T.null raw
        then pure stripeSpans
        else do
          let textFm = if fvar == FontMono then ctxMonoFontMetrics ctx else fm
              (ix, _) =
                case mStripe of
                  Just _ -> tableCellInset (ctxHostProfile ctx) textFm
                  Nothing -> labelContentInset (ctxHostProfile ctx) textFm
          ax <- getAlignX (ctxNodeArena ctx) idx
          (_, _, maxW, _) <- getMinMax (ctxNodeArena ctx) idx
          (wTag, _) <- getWidthSizing (ctxNodeArena ctx) idx
          (tw0, _) <-
            if fvar == FontMono
              then pure (measureText (ctxHostProfile ctx) textFm txt0)
              else ctxMeasureText ctx txt0
          isRowChild <- parentIsNonWrapRow (ctxNodeArena ctx) idx
          wrapped <- getWrap (ctxNodeArena ctx) idx
          let hasNewlines = T.any (== '\n') txt0
              wrapCap
                | maxW < 1e8 = max 0 maxW
                | wTag == SizingGrow && w > 0 = w
                | otherwise = maxW
              canWrap = (wrapped || not isRowChild) && wrapCap < 1e8
              wrapW = max 0 (wrapCap - 2 * ix)
              lineH = layoutLineHeight (ctxHostProfile ctx) textFm
          textSpans <-
            if hasNewlines || (canWrap && wrapCap + 0.5 < tw0)
              then do
                textLines <-
                  if isCellHost (ctxHostProfile ctx)
                    then pure (wrapTextLines (ctxHostProfile ctx) textFm txt0 wrapW)
                    else wrapTextLinesIO (\t -> if fvar == FontMono then pure (fst (measureText (ctxHostProfile ctx) textFm t)) else fmap fst (ctxMeasureText ctx t)) textFm txt0 wrapW
                pure
                  [ ( Rect
                        tx
                        (centeredTextY (ctxHostProfile ctx) textFm (y + fromIntegral i * lineH) lineH lineH)
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
                        then pure (truncateTextAdvance (fmAdvance textFm) contentW txt0)
                        else truncateTextIO (\t -> fmap fst (ctxMeasureText ctx t)) contentW txt0
                    else pure txt0
                let (tx, used) = alignedTextPen ax x w ix textFm dispTxt
                pure [(Rect tx (centeredTextY (ctxHostProfile ctx) textFm y h lineH) used lineH, dispTxt, fg, paintBg)]
          pure (stripeSpans ++ textSpans)
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
        NodeTextInput -> pure (tigFieldRect (textInputGeom (ctxHostProfile ctx) fm x y w h))
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
          let lbl = sliderLabelText txt
          pure (sliderTrackBounds (ctxHostProfile ctx) fm lbl x y w h)
        NodeButton -> do
          si <- getStyleIdx (ctxNodeArena ctx) idx
          txt <- displayText ctx nt idx
          if isCloseButtonStyle si
            then pure (closeButtonHitRect (ctxHostProfile ctx) fm x y w h)
            else pure (terminalTextHitRect (ctxHostProfile ctx) fm x y h txt True)
        NodeCheckbox -> do
          txt <- displayText ctx nt idx
          pure (terminalTextHitRect (ctxHostProfile ctx) fm x y h txt True)
        NodeRadio -> do
          txt <- displayText ctx nt idx
          pure (terminalTextHitRect (ctxHostProfile ctx) fm x y h txt True)
        NodeTree -> do
          txt <- displayText ctx nt idx
          pure (terminalTextHitRect (ctxHostProfile ctx) fm x y h txt True)
        NodeSelect -> do
          txt <- displayText ctx nt idx
          pure (terminalTextHitRect (ctxHostProfile ctx) fm x y h txt False)
        NodeTextInput -> do
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
  let fg = styleFg style
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
          placements <- widgetTextPlacements ctx nt idx x y w h
          value <- textInputValue ctx idx
          focus <- textInputFocused ctx idx
          let theme = ctxTheme ctx
              windowBg = themeWindow theme
              labelFg = lerpColor fg windowBg 0.32
              placeholder = T.null value && not focus
              fieldFg
                | placeholder = lerpColor fg bg 0.40
                | otherwise = fg
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
  let fm = ctxFontMetrics ctx
      terminal = isCellHost (ctxHostProfile ctx)
      (ix, iy) = widgetContentInset (ctxHostProfile ctx) fm
  case nt of
    NodeButton -> do
      si <- getStyleIdx (ctxNodeArena ctx) idx
      if not terminal && isCloseButtonStyle si
        then pure []
        else do
          txt <- displayText ctx nt idx
          (_tw, th) <- ctxMeasureText ctx txt
          if isTableHeaderStyle si
            then do
              ax <- getAlignX (ctxNodeArena ctx) idx
              let (labelIx, _) = tableCellInset (ctxHostProfile ctx) fm
                  (tx, used) = alignedTextPen ax x w labelIx fm txt
              pure [(txt, tx, centeredTextY (ctxHostProfile ctx) fm y h th, used, th)]
            else do
              let (tx, used) = alignedTextPen AlignCenter x w 0 fm txt
              pure [(txt, tx, centeredTextY (ctxHostProfile ctx) fm y h th, used, th)]
    NodeSelect -> do
      txt <- displayText ctx nt idx
      (tw, th) <- ctxMeasureText ctx txt
      pure [(txt, x + ix, centeredTextY (ctxHostProfile ctx) fm y h th, min tw (w - ix - selectChevronReserve), th)]
    NodeColorPicker -> do
      if terminal
        then do
          txt <- displayText ctx nt idx
          (tw, th) <- ctxMeasureText ctx txt
          pure [(txt, x + ix, centeredTextY (ctxHostProfile ctx) fm y h th, tw, th)]
        else do
          lbl <- getText (ctxNodeArena ctx) idx
          store <- getStore ctx
          wid <- getWidgetId (ctxNodeArena ctx) idx
          let geom = colorPickerGeom (ctxHostProfile ctx) fm x y w h
              hex = colorPickerToHex (widgetStoreColor store wid colorPickerDefaultColor)
              (lx, ly) = labelContentInset (ctxHostProfile ctx) fm
          (lw, lh) <- ctxMeasureText ctx lbl
          (hw, hh) <- ctxMeasureText ctx hex
          (cw, ch) <- ctxMeasureText ctx colorPickerCurrentLabel
          (nw, nh) <- ctxMeasureText ctx colorPickerNewLabel
          let previewY =
                centeredTextY (ctxHostProfile ctx) fm (cpgPreviewLabelY geom) (cpgHexH geom) lh
          pure
            [ (lbl, x + lx, y + ly, lw, lh)
            , (colorPickerCurrentLabel, rectX (cpgCurrent geom), previewY, cw, ch)
            , (colorPickerNewLabel, rectX (cpgNew geom), previewY, nw, nh)
            , (hex, x + lx, centeredTextY (ctxHostProfile ctx) fm (cpgHexY geom) (cpgHexH geom) hh, hw, hh)
            ]
    NodeCheckbox -> do
      txt <- displayText ctx nt idx
      (tw, th) <- ctxMeasureText ctx txt
      let (cx, _) =
            if terminal
              then widgetContentInset (ctxHostProfile ctx) fm
              else labelContentInset (ctxHostProfile ctx) fm
          tx = x + cx + checkboxLeading (ctxHostProfile ctx) fm
          ty = centeredTextY (ctxHostProfile ctx) fm y h th
      pure [(txt, tx, ty, tw, th)]
    NodeRadio -> do
      txt <- displayText ctx nt idx
      (tw, th) <- ctxMeasureText ctx txt
      let (cx, _) =
            if terminal
              then widgetContentInset (ctxHostProfile ctx) fm
              else labelContentInset (ctxHostProfile ctx) fm
          tx = x + cx + checkboxLeading (ctxHostProfile ctx) fm
          ty = centeredTextY (ctxHostProfile ctx) fm y h th
      pure [(txt, tx, ty, tw, th)]
    NodeTree -> do
      txt <- displayText ctx nt idx
      (tw, th) <- ctxMeasureText ctx txt
      si <- getStyleIdx (ctxNodeArena ctx) idx
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
          (lw, lh) <- ctxMeasureText ctx lbl
          let ty = centeredTextY (ctxHostProfile ctx) fm y lh lh
          pure [(lbl, x + ix, ty, lw, lh)]
        else do
          val <- sliderValue ctx idx
          let valTxt = sliderValueText val
              (lx, _) = labelContentInset (ctxHostProfile ctx) fm
          (lw, lh) <- ctxMeasureText ctx lbl
          (vw, vh) <- ctxMeasureText ctx valTxt
          let ty = centeredTextY (ctxHostProfile ctx) fm y lh lh
          pure
            [ (lbl, x + lx, ty, lw, lh)
            , (valTxt, x + w - lx - vw, centeredTextY (ctxHostProfile ctx) fm y vh vh, vw, vh)
            ]
    NodeTextInput -> do
      lbl <- getText (ctxNodeArena ctx) idx
      value <- textInputValue ctx idx
      focus <- textInputFocused ctx idx
      if terminal
        then do
          wid <- getWidgetId (ctxNodeArena ctx) idx
          store <- getStore ctx
          let cursor = IM.findWithDefault (T.length value) (slotKey slotCursor (intKey wid)) (storeInt store)
              shown = textInputTerminalText lbl value cursor focus
          (tw, th) <- ctxMeasureText ctx shown
          pure [(shown, x + ix, centeredTextY (ctxHostProfile ctx) fm y h th, tw, th)]
        else do
          let geom = textInputGeom (ctxHostProfile ctx) fm x y w h
              field = tigFieldRect geom
              fieldTxt = textInputFieldText lbl value focus
              labelH = layoutLineHeight (ctxHostProfile ctx) fm
          (lw, lh) <- ctxMeasureText ctx lbl
          (fw, _) <- ctxMeasureText ctx fieldTxt
          let lineH = layoutLineHeight (ctxHostProfile ctx) fm
          pure
            [ (lbl, x, centeredTextY (ctxHostProfile ctx) fm y labelH lh, lw, lh)
            , (fieldTxt, x + ix, centeredTextY (ctxHostProfile ctx) fm (rectY field) (rectH field) lineH, fw, lineH)
            ]
    NodeTextArea -> do
      lbl <- getText (ctxNodeArena ctx) idx
      value <- textAreaValue ctx idx
      if terminal
        then do
          (tw, th) <- ctxMeasureText ctx value
          pure [(value, x + ix, centeredTextY (ctxHostProfile ctx) fm y h th, tw, th)]
        else do
          let geom = textAreaGeom (ctxHostProfile ctx) fm x y w h
              field = tagFieldRect geom
              labelH = layoutLineHeight (ctxHostProfile ctx) fm
          (lw, lh) <- ctxMeasureText ctx lbl
          (fw, _) <- ctxMeasureText ctx (if T.null value then " " else value)
          pure
            [ (lbl, x, centeredTextY (ctxHostProfile ctx) fm y labelH lh, lw, lh)
            , (value, x + ix, rectY field + iy, fw, rectH field)
            ]
    NodeDrawing -> pure []
    _ -> do
      txt <- displayText ctx nt idx
      ax <- getAlignX (ctxNodeArena ctx) idx
      (_tw, th) <- ctxMeasureText ctx txt
      let (tx, used) = alignedTextPen ax x w ix fm txt
      pure [(txt, tx, centeredTextY (ctxHostProfile ctx) fm y h th, used, th)]

sliderValue :: Context -> NodeIdx -> IO Float
sliderValue ctx idx = do
  wid <- getWidgetId (ctxNodeArena ctx) idx
  store <- getStore ctx
  pure (IM.findWithDefault 0 (intKey wid) (storeFloat store))

-- Returns a style whose background already reflects hover/active state, so the
-- rect fill and the text cells agree on one color.
collectFloatingSpans :: Context -> IM.IntMap (Maybe NodeType) -> NodeType -> IO [(Rect, T.Text, Color, Color, Rect)]
collectFloatingSpans ctx floatCache wanted = do
  let arena = ctxSpanOverlay ctx
  resetSpanArena arena
  collectFloatingSpansInto ctx floatCache wanted arena
  spanArenaToList arena

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
                    else
                      if isScrollNode nt
                        then
                          if isScrollStyle2D si
                            then do
                              contentH <- getNodeValue (ctxNodeArena ctx) idx
                              contentW <- getAspect (ctxNodeArena ctx) idx
                              pure $
                                scrollViewportClip2D (ctxHostProfile ctx) fm slot cfg x y w h pad contentW contentH
                            else do
                              contentSize <- getNodeValue (ctxNodeArena ctx) idx
                              pure $
                                scrollContentClip (ctxHostProfile ctx) fm slot cfg dir x y w h pad contentSize
                        else pure $ padContentClip (ctxHostProfile ctx) fm x y w h pad
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
              let track = sbTrack layout
                  fg = themeSeparator (ctxTheme ctx)
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
