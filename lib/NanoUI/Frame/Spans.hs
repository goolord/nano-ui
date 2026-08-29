{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

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
  , collectTooltipSpans
  , terminalSeparatorSpans
  , filterOccludedBaseSpans
  , walkChildSpans
  , terminalScrollCapSpans
  ) where


import Control.Monad (filterM, foldM, forM, forM_, unless, void, when)
import Data.Char (isAlphaNum, isSpace)
import Data.IORef (readIORef, writeIORef)
import Data.Typeable (Typeable)
import Data.List (findIndex)
import Data.Maybe (isJust)
import Data.Word (Word32)
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import NanoUI.Damage (floatingPanelRects, updatePrevRects, writeDamage)
import NanoUI.Context
  ( Context (..)
  , FrameMsg (..)
  , WidgetStore (..)
  , TextInputMenu (..)
  , TextInputDrag (..)
  , WindowResizeDrag (..)
  , WindowResizeEdge (..)
  , anyAnimating
  , decodeMessages
  , drainMessages
  , getFocusables
  , getScrollOffset
  , getStore
  , intKey
  , isDirty
  , isDisabled
  , markDirty
  , getHotId
  , getPrevRect
  , setScrollOffset
  , setStore
  , startAnimation
  , setAnimationValue
  , tickAnimations
  , getAnimationValue
  , animInProgress
  , clearTooltips
  , readTooltips
  , PendingTooltip (..)
  , ctxClipboardGet
  , clearMeasureCache
  , markEscapeConsumed
  , lookupImageUv
  , atlasTextureId
  )
import NanoUI.Draw
  ( DrawArena
  , DrawData
  , Layer (..)
  , beginLayer
  , currentLayer
  , finishDraw
  , pushLine
  , pushFilledTriangle
  , pushRect
  , pushBackdropDim
  , pushImage
  , pushRoundedRect
  , pushRoundedStroke
  , pushText
  , resetDrawArena
  , withClip
  )
import NanoUI.Font
  ( FontMetrics (..)
  , checkboxBoxSize
  , checkboxLeading
  , fmLineHeight
  , layoutLineHeight
  , hasHeadingMarker
  , hasMonoFontMarker
  , hasMutedMarker
  , labelContentInset
  , resolveLayoutPadding
  , stripWidgetMarkers
  , lineWidth
  , textDisplayWidth
  , ScrollBarSlot (..)
  , scrollBarGeomFor
  , scrollBarOuterGap
  , scrollLayoutGutter
  , sliderTrackBounds
  , widgetContentInset
  , centeredTextY
  , alignedTextBox
  , wrapTextLines
  , wrapTextLinesIO
  )
import NanoUI.Host (HostProfile, isCellHost)
import NanoUI.WidgetMarkers
  ( buttonDisplayText
  , closeButtonDisplayText
  , isCloseButtonText
  , stripButtonBrackets
  )
import NanoUI.Icons (Icons (..), checkboxMark, terminalPaintColumns)
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Input (Input (..), Key (..), Modifiers (..), inputInteracted, inputKeys, inputPointerHeld, inputMouseDown, inputMousePos, inputMousePressed, inputMouseReleased, inputMouseRightPressed, inputScroll, inputDeltaTime, inputWindowSize, modShift)
import NanoUI.Layout.Arena
  ( DirTag (..)
  , NodeIdx
  , NodeType (..)
  , SizingTag (..)
  , arenaCount
  , getAlignX
  , getDirection
  , getFirstChild
  , getHeightSizing
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
  , isWidgetNode
  , isContainerNode
  , isFloatingNode
  , isScrollNode
  , NodeType (NodeButton, NodeCheckbox, NodeSelect, NodeSlider, NodeTextInput, NodeModal, NodeImage, NodePanel, NodeWindow, NodeContainer, NodeScrollContainer, NodeText, NodeSeparator, NodeSpacer, NodeBox)
  , resetNodeArena
  , setNodeText
  , setNodeValue
  , setRect
  )
import NanoUI.Layout.Solve (placeModals, placeWindows, positionWindowNode, scrollBarSlotOf, solveLayout)
import Effectful (Eff, IOE, runEff, type (:>))
import NanoUI.Monad (NanoUI, Ui, runUi)
import NanoUI.Widgets (applyTextInputMenuAction)
import NanoUI.WidgetText
  ( checkboxLabelText
  , sliderLabelText
  , sliderPackRange
  , sliderParseRange
  , sliderPackTerminal
  , sliderValueText
  , textInputFieldHeight
  , textInputFieldText
  , textInputLabelGap
  , textInputTerminalText
  , selectParseOptions
  , selectDisplayText
  , selectChevronReserve
  , selectChevronCenterX
  )
import NanoUI.Style (Padding (..), Style (..), Theme (..), scrollBarThumbColor, scrollBarTrackColor, themeAccent, themeButton, themeFloatingWindow, themeInput, themeMuted, themeOverlayDim, themePanel, themeSeparator, themeWindow)
import NanoUI.Types (Color (..), ImageId (..), Rect (..), Size (..), V2 (..), colorRGBA, lerpColor, rectContains, rectH, rectIntersect, rectOverlapArea, rectUnion, rectW, rectX, rectY, v2X, v2Y)
import NanoUI.Frame.Chrome
  ( buildFloatingAncestorMap
  , displayText
  , floatingAncestor
  , floatingLabelPaint
  , nodeLabelPaint
  , textInputFocused
  , textInputValue
  , widgetVisualStyle
  )
import NanoUI.Frame.Clip
  ( padContentClip
  , scrollContentClip
  , tagClippedSpans
  , terminalModalOuterClip
  )
import NanoUI.Frame.Hit (widgetOverlayAllowed)
import NanoUI.Frame.Select (collectSelectDropdownSpans, selectDropRect)
import NanoUI.Frame.TextInput (TextInputGeom (..), collectTextInputMenuSpans, tagSelectClippedSpans, tagTextInputClippedSpans, textInputFieldTextClip, textInputGeom, selectTextClip)
import NanoUI.Frame.Scroll (scrollBarLayout, ScrollBarLayout (..))

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
  spans <-
    if count > 0
      then collectClippedSpans ctx floatCache 0 (Rect 0 0 1e9 1e9) []
      else pure []
  panels <- floatingPanelRects ctx
  pure (filterOccludedBaseSpans panels spans)

collectOverlayTextSpansCached :: Context -> Input -> IM.IntMap (Maybe NodeType) -> IO [(Rect, T.Text, Color, Color, Rect)]
collectOverlayTextSpansCached ctx inp floatCache = do
  drops <- collectSelectDropdownSpans ctx inp
  menu <- collectTextInputMenuSpans ctx inp
  tips <- collectTooltipSpans ctx
  windows <- collectFloatingSpans ctx floatCache NodeWindow
  modals <- collectFloatingSpans ctx floatCache NodeModal
  pure (windows ++ modals ++ drops ++ menu ++ tips)

widgetNodeCount :: Context -> IO Int
widgetNodeCount ctx = arenaCount (ctxNodeArena ctx)

collectClippedSpans :: Context -> IM.IntMap (Maybe NodeType) -> NodeIdx -> Rect -> [(Rect, T.Text, Color, Color, Rect)] -> IO [(Rect, T.Text, Color, Color, Rect)]
collectClippedSpans ctx floatCache idx clip acc = do
  nt <- getNodeType (ctxNodeArena ctx) idx
  if isFloatingNode nt
    then pure acc
    else collectClippedSpans' ctx floatCache idx nt clip acc

collectClippedSpans' :: Context -> IM.IntMap (Maybe NodeType) -> NodeIdx -> NodeType -> Rect -> [(Rect, T.Text, Color, Color, Rect)] -> IO [(Rect, T.Text, Color, Color, Rect)]
collectClippedSpans' ctx floatCache idx nt clip acc = do
  (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
  pad <- getPadding (ctxNodeArena ctx) idx
  let nodeRect = Rect x y w h
      fm = ctxFontMetrics ctx
  mClipChildren <-
    if isScrollNode nt
      then do
        dir <- getDirection (ctxNodeArena ctx) idx
        contentSize <- getNodeValue (ctxNodeArena ctx) idx
        slot <- scrollBarSlotOf (ctxNodeArena ctx) idx
        let content = scrollContentClip (ctxHostProfile ctx) fm slot dir x y w h pad contentSize
        pure (rectIntersect clip content)
      else
        if nt == NodePanel
          then pure (rectIntersect clip nodeRect)
          else pure (Just clip)
  case mClipChildren of
    Nothing -> pure acc
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
      -- TUI modal chrome does not scroll (the inner body scroller does), so it
      -- has no track to cap.
      caps <-
        if isCellHost (ctxHostProfile ctx) && isScrollNode nt && nt /= NodeModal
          then terminalScrollCapSpans ctx idx x y w h pad clip
          else pure []
      childSpans <- walkChildSpans ctx floatCache idx clipHere acc
      pure (here ++ (caps ++ childSpans))

walkChildSpans :: Context -> IM.IntMap (Maybe NodeType) -> NodeIdx -> Rect -> [(Rect, T.Text, Color, Color, Rect)] -> IO [(Rect, T.Text, Color, Color, Rect)]
walkChildSpans ctx floatCache idx clip acc = do
  fc <- getFirstChild (ctxNodeArena ctx) idx
  go fc acc
  where
    go ci kAcc
      | ci < 0 = pure kAcc
      | otherwise = do
          ns <- getNextSibling (ctxNodeArena ctx) ci
          rest <- go ns kAcc
          collectClippedSpans ctx floatCache ci clip rest

filterOccludedBaseSpans :: IM.IntMap Rect -> [(Rect, T.Text, Color, Color, Rect)] -> [(Rect, T.Text, Color, Color, Rect)]
filterOccludedBaseSpans panels spans
  | IM.null panels = spans
  | otherwise = filter (not . occluded) spans
  where
    panelRects = IM.elems panels
    occluded (rect, _, _, _, clip) =
      case rectIntersect rect clip of
        Nothing -> True
        Just visible -> any (rectFullyInside visible) panelRects

-- Visible text is gone only when a floating panel covers every pixel.
rectFullyInside :: Rect -> Rect -> Bool
rectFullyInside (Rect ix iy iw ih) (Rect ox oy ow oh) =
  iw > 0
    && ih > 0
    && ix >= ox
    && iy >= oy
    && ix + iw <= ox + ow
    && iy + ih <= oy + oh

-- TTF measure is often a fraction narrower than the rendered texture.
collectNodeTextSpans :: Context -> IM.IntMap (Maybe NodeType) -> NodeIdx -> IO [(Rect, T.Text, Color, Color)]
collectNodeTextSpans ctx floatCache idx = do
  nt <- getNodeType (ctxNodeArena ctx) idx
  (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
  let fm = ctxFontMetrics ctx
      theme = ctxTheme ctx
  if nt == NodeText
    then do
      raw <- getText (ctxNodeArena ctx) idx
      if T.null raw
        then pure []
        else do
          let (txt, fg, bg) = floatingLabelPaint floatCache ctx idx theme raw
          let (ix, _) = labelContentInset (ctxHostProfile ctx) fm
          ax <- getAlignX (ctxNodeArena ctx) idx
          (_, _, maxW, _) <- getMinMax (ctxNodeArena ctx) idx
          (wTag, _) <- getWidthSizing (ctxNodeArena ctx) idx
          (tw0, th0) <- ctxMeasureText ctx txt
          let hasNewlines = T.any (== '\n') txt
              wrapCap
                | maxW < 1e8 = max 0 maxW
                | wTag == SizingGrow && w > 0 = w
                | otherwise = maxW
              canWrap = wrapCap < 1e8
              wrapW = max 0 (wrapCap - 2 * ix)
              lineH = layoutLineHeight (ctxHostProfile ctx) fm
          if hasNewlines || (canWrap && wrapCap + 0.5 < tw0)
            then do
              textLines <-
                if isCellHost (ctxHostProfile ctx)
                  then pure (wrapTextLines (ctxHostProfile ctx) fm txt wrapW)
                  else wrapTextLinesIO (\t -> fmap fst (ctxMeasureText ctx t)) fm txt wrapW
              lineWs <-
                if isCellHost (ctxHostProfile ctx)
                  then pure (map (lineWidth fm) textLines)
                  else mapM (fmap fst . ctxMeasureText ctx) textLines
              pure
                [ ( Rect
                      tx
                      (centeredTextY (ctxHostProfile ctx) fm (y + fromIntegral i * lineH) lineH lineH)
                      tw
                      lineH
                  , line
                  , fg
                  , bg
                  )
                | (i, line, lw) <- zip3 [(0 :: Int) ..] textLines lineWs
                , let (tx, tw) = alignedTextBox ax x w ix lw
                ]
            else do
              let (tx, used) = alignedTextBox ax x w ix tw0
              pure [(Rect tx (centeredTextY (ctxHostProfile ctx) fm y h th0) used th0, txt, fg, bg)]
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
        NodeButton -> do
          stored <- getText (ctxNodeArena ctx) idx
          if isCloseButtonText stored
            then pure (closeButtonHitRect (ctxHostProfile ctx) fm x y w h)
            else pure (Rect x y w h)
        _ -> pure (Rect x y w h)
    else
      case nt of
        NodeSlider -> do
          txt <- getText (ctxNodeArena ctx) idx
          let lbl = sliderLabelText (T.takeWhile (/= '\US') txt)
          pure (sliderTrackBounds (ctxHostProfile ctx) fm lbl x y w h)
        NodeButton -> do
          stored <- getText (ctxNodeArena ctx) idx
          txt <- displayText ctx nt idx
          if isCloseButtonText stored
            then pure (closeButtonHitRect (ctxHostProfile ctx) fm x y w h)
            else pure (terminalTextHitRect (ctxHostProfile ctx) fm x y h txt True)
        NodeCheckbox -> do
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
              then isCloseButtonText <$> getText (ctxNodeArena ctx) idx
              else pure False
          if isClose
            then
              let closeRect = terminalClosePaintRect (ctxHostProfile ctx) fm x y w h txt
               in pure [(closeRect, txt, fg, bg)]
            else do
              let tx =
                    if nt == NodeButton || nt == NodeCheckbox
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
      (ix, _) = widgetContentInset (ctxHostProfile ctx) fm
  case nt of
    NodeButton -> do
      stored <- getText (ctxNodeArena ctx) idx
      if not terminal && isCloseButtonText stored
        then pure []
        else do
          txt <- displayText ctx nt idx
          (tw, th) <- ctxMeasureText ctx txt
          pure [(txt, x + (w - tw) / 2, centeredTextY (ctxHostProfile ctx) fm y h th, tw, th)]
    NodeSelect -> do
      txt <- displayText ctx nt idx
      (tw, th) <- ctxMeasureText ctx txt
      pure [(txt, x + ix, centeredTextY (ctxHostProfile ctx) fm y h th, min tw (w - ix - selectChevronReserve), th)]
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
          let cursor = IM.findWithDefault (T.length value) (intKey wid) (storeCursor store)
              shown = textInputTerminalText lbl value cursor focus
          (tw, th) <- ctxMeasureText ctx shown
          pure [(shown, x + ix, centeredTextY (ctxHostProfile ctx) fm y h th, tw, th)]
        else do
          let geom = textInputGeom (ctxHostProfile ctx) fm x y w h
              field = tigFieldRect geom
              fieldTxt = textInputFieldText lbl value focus
              labelH = layoutLineHeight (ctxHostProfile ctx) fm
          (lw, lh) <- ctxMeasureText ctx lbl
          (fw, fh) <- ctxMeasureText ctx fieldTxt
          pure
            [ (lbl, x, centeredTextY (ctxHostProfile ctx) fm y labelH lh, lw, lh)
            , (fieldTxt, x + ix, centeredTextY (ctxHostProfile ctx) fm (rectY field) (rectH field) fh, fw, fh)
            ]
    _ -> do
      txt <- displayText ctx nt idx
      ax <- getAlignX (ctxNodeArena ctx) idx
      let fm' =
            if hasMonoFontMarker txt
              then ctxMonoFontMetrics ctx
              else fm
      (tw, th) <- ctxMeasureText ctx txt
      let (tx, used) = alignedTextBox ax x w ix tw
      pure [(txt, tx, centeredTextY (ctxHostProfile ctx) fm' y h th, used, th)]

sliderValue :: Context -> NodeIdx -> IO Float
sliderValue ctx idx = do
  wid <- getWidgetId (ctxNodeArena ctx) idx
  store <- getStore ctx
  pure (IM.findWithDefault 0 (intKey wid) (storeSlider store))

-- Returns a style whose background already reflects hover/active state, so the
-- rect fill and the text cells agree on one color.
collectFloatingSpans :: Context -> IM.IntMap (Maybe NodeType) -> NodeType -> IO [(Rect, T.Text, Color, Color, Rect)]
collectFloatingSpans ctx floatCache wanted = do
  count <- arenaCount (ctxNodeArena ctx)
  let fm = ctxFontMetrics ctx
      go !idx
        | idx >= count = pure []
        | otherwise = do
            nt <- getNodeType (ctxNodeArena ctx) idx
            if nt /= wanted
              then go (idx + 1)
              else do
                (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
                pad <- getPadding (ctxNodeArena ctx) idx
                dir <- getDirection (ctxNodeArena ctx) idx
                contentSize <- getNodeValue (ctxNodeArena ctx) idx
                slot <- scrollBarSlotOf (ctxNodeArena ctx) idx
                let clip =
                      if isCellHost (ctxHostProfile ctx) && nt == NodeModal
                        then terminalModalOuterClip (ctxHostProfile ctx) fm x y w h pad
                        else
                          if isScrollNode nt
                            then scrollContentClip (ctxHostProfile ctx) fm slot dir x y w h pad contentSize
                            else padContentClip (ctxHostProfile ctx) fm x y w h pad
                rest <- go (idx + 1)
                walkChildSpans ctx floatCache idx clip rest
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

collectTooltipSpans :: Context -> IO [(Rect, T.Text, Color, Color, Rect)]
collectTooltipSpans ctx = do
  let fm = ctxFontMetrics ctx
      theme = ctxTheme ctx
  if isCellHost (ctxHostProfile ctx)
    then pure []
    else do
      tips <- readTooltips ctx
      filtered <- filterM (\(PendingTooltip wid _ _) -> widgetOverlayAllowed ctx wid) tips
      forM filtered $ \(PendingTooltip _ rect txt) -> do
        let (ix, _) = widgetContentInset (ctxHostProfile ctx) fm
            fg = styleFg (themePanel theme)
            bg = styleBg (themePanel theme)
        (tw, th) <- ctxMeasureText ctx txt
        let tx = rectX rect + ix
            ty = centeredTextY (ctxHostProfile ctx) fm (rectY rect) (rectH rect) th
            textRect = Rect tx ty tw th
        pure (textRect, txt, fg, bg, rect)
