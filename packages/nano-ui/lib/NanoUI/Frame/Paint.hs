{-# LANGUAGE DataKinds #-}

module NanoUI.Frame.Paint
  ( lowerShapes
  , lowerNode
  , walkChildren
  ) where


import Control.Monad (forM_, unless, when)
import Data.IORef (readIORef)
import Data.Maybe (fromMaybe)
import Data.Word (Word32)
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import NanoUI.Widgets.ColorPicker (colorPickerAlphaMode, drawColorPickerPanel)
import NanoUI.Widgets.Custom (mkCustomDrawContext)
import NanoUI.Context
  ( Context (..)
  , DrawingEntry (..)
  , atlasTextureId
  , cachedCustomDrawingOps
  , cachedDrawingOps
  , getStore
  , lookupCustomDrawing
  , lookupDrawing
  , lookupImageUv
  )
import NanoUI.Draw
  ( DrawArena (..)
  , emitDrawOps
  , pushFilledTriangle
  , pushImage
  , pushLine
  , pushRect
  , pushRoundedRect
  , pushRoundedRectRaw
  , pushRoundedStroke
  , pushStrokeAA
  , pushText
  , pushTextStyled
  , snapToPixel
  , withClip
  )
import NanoUI.Font
  ( FontMetrics (..)
  , centeredTextY
  , checkboxBoxSize
  , labelContentInset
  , sliderTrackBounds
  , sliderHandleDiameter
  , treeChevronRect
  , widgetContentInset
  )
import NanoUI.Types (HostProfile, isCellHost)
import NanoUI.Layout.Arena
  ( DirTag (..)
  , NodeIdx
  , NodeType (..)
  , SizingTag (..)
  , arenaCount
  , forChildNodes_
  , getScrollContentW
  , getDirection
  , getHeightSizing
  , getNodeType
  , getNodeValue
  , getOptions
  , getPadding
  , getRect
  , getStyleIdx
  , getText
  , getWidthSizing
  , getWidgetId
  , getNodeFontSize
  , getNodeFontColor
  , isFloatingNode
  )
import NanoUI.Layout.Solve (scrollBarSlotOf)
import NanoUI.Style
  ( FontStyle (..)
  , FontVariant (..)
  , FontWeight (..)
  , TextDecoration (..)
  , Style (..)
  , Theme (..)
  , styleBg
  , styleBorder
  , styleFg
  , themeAccent
  , themeFloatingWindow
  , themeInput
  , themePanel
  , unpackPanelStyle
  , themeSeparator
  , themeWindow
  )
import NanoUI.Types (Color (..), ImageId (..), Rect (..), colorA, colorRGBA, clamp01, lerpColor, rectFullyInside, rectInflate, rectH, rectW, rectX, rectY)
import NanoUI.WidgetText
  ( buttonFlagsFromStyle
  , buttonVisualStyle
  , isMenuItemStyle
  , isMenuBarStyle
  , comboTextClip
  , searchFieldIconRects
  , tableSortBlank
  , tableSortMarkOf
  , searchFieldTextClip
  , selectChevronCenterX
  , selectChevronReserve
  , tableStripeColor
  , textInputBareMode
  , textInputFieldText
  , textInputSearchBody
  , textInputSearchMode
  , textNodeFontVariant
  , textNodeFontWeight
  , textNodeFontStyle
  , textNodeTextDecoration
  , treeDecodeStyle
  )
import NanoUI.Frame.Chrome
  ( fillStyledRect
  , floatingAncestor
  , imageIdFromText
  , overlayModalStyle
  , overlayMenuStyle
  , overlayWindowStyle
  , paintTabHeader
  , paintTableHeader
  , strokeStyledRect
  , textInputFocused
  , textInputValue
  , widgetVisualStyle
  )
import NanoUI.Frame.Scroll.Geometry (borderContentClip, padContentClip, scrollContentClip)
import NanoUI.Frame.Scroll (paintScrollChrome)
import NanoUI.Frame.Scroll.Geometry
  ( decodeScrollConfig
  , isScrollStyle2D
  , scrollBare
  , scrollChromeActive
  , scrollViewportClip2D
  )
import NanoUI.Frame.Spans (collectNodeTextSpans, widgetTextPlacements, widgetTextSpans)
import NanoUI.Frame.TextEdit
  ( TextAreaGeom (..)
  , TextInputGeom (..)
  , drawTextAreaContent
  , drawTextInputCaret
  , drawTextInputSelection
  , syncTextInputScroll
  , textInputFieldTextClip
  , textInputGeom
  , textAreaGeom
  )

lowerShapes :: Context -> IO ()
lowerShapes ctx = do
  count <- arenaCount (ctxNodeArena ctx)
  when (count > 0) $ do
    occluders <- collectFloatingOccluders ctx
    lowerNodeWithOccluders ctx occluders 0

collectFloatingOccluders :: Context -> IO [Rect]
collectFloatingOccluders ctx = do
  n <- arenaCount (ctxNodeArena ctx)
  theme <- readIORef (ctxTheme ctx)
  let winStyle = overlayWindowStyle theme
      modalStyle = overlayModalStyle theme
      menuStyle = overlayMenuStyle theme
      isOpaque s = colorA (styleBg s) == 255
      winOpaque = isOpaque winStyle
      modalOpaque = isOpaque modalStyle
      menuOpaque = isOpaque menuStyle
      go idx acc
        | idx >= n = pure acc
        | otherwise = do
            nt <- getNodeType (ctxNodeArena ctx) idx
            case nt of
              NodeWindow | winOpaque -> checkPanel idx acc
              NodeModal  | modalOpaque -> checkPanel idx acc
              NodePopup  | menuOpaque -> checkPanel idx acc
              _ -> go (idx + 1) acc
      checkPanel idx acc = do
        (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
        if w > 6 && h > 6
          then do
            let !r = rectInflate (-3) (Rect x y w h)
            go (idx + 1) (r : acc)
          else go (idx + 1) acc
  go 0 []

{-# INLINE lowerNode #-}
lowerNode :: Context -> NodeIdx -> IO ()
lowerNode ctx idx = lowerNodeWithOccluders ctx [] idx

lowerNodeWithOccluders :: Context -> [Rect] -> NodeIdx -> IO ()
lowerNodeWithOccluders ctx occluders idx = do
  (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
  let da = ctxDrawArena ctx
  (cx, cy, cw, ch) <- readIORef (daCurrentClip da)
  let !l = max x cx
      !t = max y cy
      !r = min (x + w) (cx + cw)
      !b = min (y + h) (cy + ch)
  if r <= l || b <= t
    then pure ()
    else do
      if not (null occluders) && any (rectFullyInside (Rect l t (r - l) (b - t))) occluders
        then pure ()
        else do
          nt <- getNodeType (ctxNodeArena ctx) idx
          theme <- readIORef (ctxTheme ctx)
          let !rect = Rect x y w h
              !fm = ctxFontMetrics ctx
              !terminal = isCellHost (ctxHostProfile ctx)
          lowerNodeVisible ctx occluders idx nt x y w h rect fm theme terminal da

lowerNodeVisible ::
  Context ->
  [Rect] ->
  NodeIdx ->
  NodeType ->
  Float ->
  Float ->
  Float ->
  Float ->
  Rect ->
  FontMetrics ->
  Theme ->
  Bool ->
  DrawArena ->
  IO ()
lowerNodeVisible ctx occluders idx nt x y w h rect fm theme terminal da =
  case nt of
    NodeContainer -> do
      walkChildrenWithOccluders ctx occluders idx
      wid <- getWidgetId (ctxNodeArena ctx) idx
      mBuild <- lookupCustomDrawing ctx wid
      case mBuild of
        Nothing -> pure ()
        Just build -> do
          cdc <- mkCustomDrawContext ctx fm wid
          withClip da rect (emitDrawOps da fm (build cdc rect))
    NodePanel -> do
      si <- getStyleIdx (ctxNodeArena ctx) idx
      let style = if si /= 0
                    then unpackPanelStyle (themePanel theme) si
                    else themePanel theme
      fillStyledRect da terminal style rect
      strokeStyledRect da terminal style x y w h
      withClip da (borderContentClip style rect) $ walkChildrenWithOccluders ctx occluders idx
    NodeScrollContainer -> do
      mFloat <- floatingAncestor ctx idx
      let inFloating = maybe False isFloatingNode mFloat
          baseStyle
            | inFloating = themeFloatingWindow theme
            | otherwise  = themeInput theme
      pad <- getPadding (ctxNodeArena ctx) idx
      (wTag, _) <- getWidthSizing (ctxNodeArena ctx) idx
      (hTag, _) <- getHeightSizing (ctxNodeArena ctx) idx
      si <- getStyleIdx (ctxNodeArena ctx) idx
      dir <- getDirection (ctxNodeArena ctx) idx
      slot <- scrollBarSlotOf (ctxNodeArena ctx) idx
      let cfg = decodeScrollConfig si
          native2D = isScrollStyle2D si
          padClip = padContentClip (ctxHostProfile ctx) fm x y w h pad
          innerW = rectW padClip
          innerH = rectH padClip
          wellStyle = baseStyle {styleCornerRadius = 0}
      (showChrome, inner) <-
        if native2D
          then do
            contentH <- getNodeValue (ctxNodeArena ctx) idx
            contentW <- getScrollContentW (ctxNodeArena ctx) idx
            pure
              ( scrollChromeActive cfg True DirColumn contentH innerH
                  || scrollChromeActive cfg True DirRow contentW innerW
              , scrollViewportClip2D (ctxHostProfile ctx) fm slot cfg x y w h pad contentW contentH
              )
          else do
            contentSize <- getNodeValue (ctxNodeArena ctx) idx
            let innerMain =
                  case dir of
                    DirColumn -> innerH
                    DirRow -> innerW
            pure
              ( scrollChromeActive cfg False dir contentSize innerMain
              , scrollContentClip (ctxHostProfile ctx) fm slot cfg dir x y w h pad contentSize
              )
      -- A bare scroller paints nothing at all: it only lends its clip and
      -- offset, so whatever sits behind it (window, panel) keeps showing
      -- through. Grow×grow scrollers (page-level) keep no well so they blend
      -- into the window backdrop. That backdrop only exists while the runner
      -- clears it on DamageFull frames; on clip frames (scrolling, resize)
      -- the strip vacated by scrolled content has no covering command and
      -- the retained texture would show stale pixels — a ghost of a previous
      -- scroll position. Paint the full rect with the window color instead:
      -- invisible on a cleared backdrop, and clip replay then always
      -- repaints the whole viewport.
      if scrollBare cfg
        then pure ()
        else
          if wTag == SizingGrow && hTag == SizingGrow
            then pushRect da rect (if inFloating then styleBg (themeFloatingWindow theme) else themeWindow theme)
            else do
              fillStyledRect da terminal wellStyle rect
              strokeStyledRect da terminal wellStyle x y w h
      withClip da inner $ walkChildrenWithOccluders ctx occluders idx
      when showChrome $ do
        wid <- getWidgetId (ctxNodeArena ctx) idx
        paintScrollChrome ctx da idx wid x y w h pad theme terminal
    NodeText -> do
      si <- getStyleIdx (ctxNodeArena ctx) idx
      case tableStripeColor theme si of
        Just stripe | not terminal -> pushRect da rect stripe
        _ -> pure ()
      raw <- getText (ctxNodeArena ctx) idx
      unless (T.null raw) $ do
        spans <- collectNodeTextSpans ctx IM.empty idx
        fontSizeVal <- getNodeFontSize (ctxNodeArena ctx) idx
        let fvar = textNodeFontVariant si
            fweight = textNodeFontWeight si
            fstyle  = textNodeFontStyle si
            fdeco   = textNodeTextDecoration si
            isBaseSans = fontSizeVal <= 0 && fweight == WeightNormal && fstyle == FontStyleNormal && fvar == FontRegular
            isBaseMono = fontSizeVal <= 0 && fweight == WeightNormal && fstyle == FontStyleNormal && fvar == FontMono
        if isBaseSans && fdeco == DecorationNone
          then do
            let fm' = ctxFontMetrics ctx
            forM_ spans $ \(Rect tx ty _ _, line, spanFg, _) ->
              unless (T.null line) $
                pushText da fm' tx ty line spanFg
          else if isBaseMono && fdeco == DecorationNone
            then do
              let fm' = ctxMonoFontMetrics ctx
              forM_ spans $ \(Rect tx ty _ _, line, spanFg, _) ->
                unless (T.null line) $
                  pushText da fm' tx ty line spanFg
            else do
              (fm', isNative) <-
                if isBaseSans
                  then pure (ctxFontMetrics ctx, False)
                  else if isBaseMono
                    then pure (ctxMonoFontMetrics ctx, False)
                    else ctxResolveFont ctx fontSizeVal fweight fstyle fvar
              let effWeight = if isNative then WeightNormal else fweight
                  effStyle  = if isNative then FontStyleNormal else fstyle
              forM_ spans $ \(Rect tx ty _ _, line, spanFg, _) ->
                unless (T.null line) $
                  pushTextStyled da fm' effWeight effStyle fdeco tx ty line spanFg
    NodeSeparator -> do
      let hair = 1
      when (not terminal) $
        if w >= h
          then pushRect da (Rect x (y + (h - hair) / 2) w hair) (themeSeparator theme)
          else pushRect da (Rect (x + (w - hair) / 2) y hair h) (themeSeparator theme)
    NodeTextInput
      | not terminal -> do
          style <- widgetVisualStyle ctx nt idx
          focus <- textInputFocused ctx idx
          si <- getStyleIdx (ctxNodeArena ctx) idx
          if textInputBareMode si
            then paintBareField ctx da fm theme style idx focus x y w h
            else
              if textInputSearchMode si
                then do
                  opts <- getOptions (ctxNodeArena ctx) idx
                  if null opts
                    then paintSearchField ctx da fm theme style idx focus x y w h
                    else paintComboField ctx da fm theme style idx focus x y w h
                else do
              let geom = textInputGeom (ctxHostProfile ctx) fm x y w h
                  fieldRect = tigFieldRect geom
                  clip = textInputFieldTextClip (ctxHostProfile ctx) geom fm
              paintTextFieldFrame da theme style focus fieldRect
              spans <- widgetTextSpans ctx nt idx x y w h
              case spans of
                (lblSpan : fieldSpan : _) -> do
                  let (Rect lx ly _ _, lbl, lfg, _) = lblSpan
                      (Rect fx fy _ _, field, ffg, _) = fieldSpan
                  unless (T.null lbl) $ do
                    pushText da fm lx ly lbl lfg
                  paintClippedFieldText ctx da fm style idx x y w h clip fx fy field ffg
                [lblSpan] -> do
                  let (Rect lx ly _ _, lbl, lfg, _) = lblSpan
                  unless (T.null lbl) $ do
                    pushText da fm lx ly lbl lfg
                  paintClippedFieldText ctx da fm style idx x y w h clip lx ly T.empty lfg
                _ -> pure ()
    NodeTextArea
      | not terminal -> do
          style <- widgetVisualStyle ctx nt idx
          focus <- textInputFocused ctx idx
          let geom = textAreaGeom (ctxHostProfile ctx) fm x y w h
              fieldRect = tagFieldRect geom
          paintTextFieldFrame da theme style focus fieldRect
          lbl <- getText (ctxNodeArena ctx) idx
          unless (T.null lbl) $ do
            let lfg = lerpColor (styleFg style) (themeWindow theme) 0.32
            pushText da fm x y lbl lfg
          drawTextAreaContent da ctx idx x y w h style
    NodeSpacer -> pure ()
    NodeModal -> pure ()
    NodeWindow -> pure ()
    NodePopup -> pure ()
    NodeBox -> do
      si <- getStyleIdx (ctxNodeArena ctx) idx
      -- styleIdx holds RGBA Word32 bits; see `box` in NanoUI.Widgets.
      pushRect da rect (Color (fromIntegral si :: Word32))
    NodeImage -> do
      tex <- imageIdFromText <$> getText (ctxNodeArena ctx) idx
      mUv <- lookupImageUv ctx (ImageId tex)
      case mUv of
        Just (u0, v0, u1, v1)
          | not terminal ->
              pushImage da rect atlasTextureId u0 v0 u1 v1 (colorRGBA 255 255 255 255)
        _ -> pushRect da rect (themeAccent theme)
    NodeDrawing -> do
      wid <- getWidgetId (ctxNodeArena ctx) idx
      mCustomBuild <- lookupCustomDrawing ctx wid
      case mCustomBuild of
        Just customBuild -> do
          cdc <- mkCustomDrawContext ctx fm wid
          ops <- cachedCustomDrawingOps ctx wid rect cdc customBuild
          withClip da rect (emitDrawOps da fm ops)
        Nothing -> do
          mBuild <- lookupDrawing ctx wid
          case mBuild of
            Nothing -> pure ()
            Just (DrawingEntry content build) -> do
              ops <- cachedDrawingOps ctx wid content rect build
              withClip da rect (emitDrawOps da fm ops)
    _ -> do
      style <- widgetVisualStyle ctx nt idx
      value <- getNodeValue (ctxNodeArena ctx) idx
      si <- getStyleIdx (ctxNodeArena ctx) idx
      let (isClose, isTab, isTable) =
            if nt == NodeButton
              then buttonFlagsFromStyle si
              else (False, False, False)
          isMenuItem = nt == NodeButton && isMenuItemStyle si
          isMenu = nt == NodeButton && (isMenuItemStyle si || isMenuBarStyle si)
      let opaqueBg
            | isMenu = colorA (styleBg style) > 0
            | isClose = False
            | isTab = False
            | isTable = colorA (styleBg style) > 0
            | terminal, nt == NodeButton = False
            | terminal, nt == NodeCheckbox = False
            | terminal, nt == NodeRadio = False
            | nt == NodeTree = colorA (styleBg style) > 0
            | terminal, nt == NodeSlider = False
            | terminal, nt == NodeSelect = False
            | terminal, nt == NodeColorPicker = False
            | terminal, nt == NodeTextInput = False
            | terminal, nt == NodeTextArea = False
            | terminal, nt == NodeText = False
            | terminal = True
            | otherwise =
                nt /= NodeCheckbox && nt /= NodeRadio && nt /= NodeSlider && nt /= NodeTextInput && nt /= NodeTextArea && nt /= NodeColorPicker
      when opaqueBg $ fillStyledRect da terminal style rect
      when (not terminal) $ do
        when (opaqueBg && not isTab && not isTable && not isMenu && nt /= NodeTree) $ strokeStyledRect da terminal style x y w h
        when isMenu $ do
          wid <- getWidgetId (ctxNodeArena ctx) idx
          hot <- readIORef (ctxHotId ctx)
          when isMenuItem $
            when (wid == hot) $ do
              let barRect = Rect x (y + 4) 2 (max 0 (h - 8))
              pushRoundedRect da barRect 1 (themeAccent theme)
        when isTab $
          paintTabHeader
            da
            (ctxHostProfile ctx)
            theme
            (buttonVisualStyle si `mod` 4)
            (value > 0.5)
            style
            x
            y
            w
            h
        when isTable $
          paintTableHeader
            da
            (ctxHostProfile ctx)
            theme
            (value > 0.5)
            style
            x
            y
            w
            h
        when (nt == NodeCheckbox) $
          drawCheckbox
            (ctxHostProfile ctx)
            da
            fm
            style
            x
            y
            h
            value
            (themeAccent theme)
            (styleBg (themeInput theme))
        when (nt == NodeRadio) $
          drawRadio
            (ctxHostProfile ctx)
            da
            fm
            style
            x
            y
            h
            value
            (themeAccent theme)
            (styleBg (themeInput theme))
        when (nt == NodeTree) $ do
          let (_, depth, hasKids, expanded) = treeDecodeStyle si
          when hasKids $
            drawTreeChevron
              da
              (ctxHostProfile ctx)
              fm
              x
              y
              w
              h
              depth
              expanded
              (styleFg style)
        when (nt == NodeSlider) $ do
          txt <- getText (ctxNodeArena ctx) idx
          let track = sliderTrackBounds (ctxHostProfile ctx) fm txt x y w h
              tx = rectX track
              ty = rectY track
              tw = rectW track
              th = rectH track
              trackR = 3
              fillW = max 0 (tw * clamp01 value)
              outline = styleBorder (themeInput theme)
              well = lerpColor (styleBg (themeInput theme)) (styleBorder (themeInput theme)) 0.35
              fill = themeAccent theme
              bw = 1
              innerR = max 0 (trackR - bw)
              innerX = tx + bw
              innerY = ty + bw
              innerW = tw - 2 * bw
              innerH = th - 2 * bw
              innerFillW = max 0 (innerW * clamp01 value)
          pushRoundedStroke da track trackR bw outline
          when (innerW > 0 && innerH > 0) $
            pushRoundedRect da (Rect innerX innerY innerW innerH) innerR well
          when (innerFillW > 0) $ do
            let fillR =
                  if innerFillW >= innerW - 0.5
                    then innerR
                    else min innerR (innerFillW / 2)
            pushRoundedRect da (Rect innerX innerY innerFillW innerH) fillR fill
          let handleD = sliderHandleDiameter
              handleCx = tx + max (handleD / 2) (min (tw - handleD / 2) fillW)
              handleHy = ty + (th - handleD) / 2
              handle = Rect (handleCx - handleD / 2) handleHy handleD handleD
              innerD = handleD - 2
              handleInner =
                Rect
                  (handleCx - innerD / 2)
                  (handleHy + (handleD - innerD) / 2)
                  innerD
                  innerD
          pushRoundedRect da handleInner (innerD / 2) (colorRGBA 255 255 255 255)
          pushRoundedStroke da handle (handleD / 2) bw (styleBorder (themeInput theme))
        when isClose $
          drawCloseIcon (ctxHostProfile ctx) fm da x y w h (styleFg style)
        when (nt == NodeSelect) $
          drawSelectChevron da x y w h (styleFg style)
        when (nt == NodeColorPicker) $ do
          store <- getStore ctx
          wid <- getWidgetId (ctxNodeArena ctx) idx
          drawColorPickerPanel (colorPickerAlphaMode si) (ctxHostProfile ctx) fm da store wid style x y w h
      placements <- widgetTextPlacements ctx nt idx x y w h
      mFontColor <- getNodeFontColor (ctxNodeArena ctx) idx
      fontSizeVal <- getNodeFontSize (ctxNodeArena ctx) idx
      let widgetFg = fromMaybe (styleFg style) mFontColor
          fvar = textNodeFontVariant si
          fweight = textNodeFontWeight si
          fstyle  = textNodeFontStyle si
          isBaseSans = fontSizeVal <= 0 && fweight == WeightNormal && fstyle == FontStyleNormal && fvar == FontRegular
          isBaseMono = fontSizeVal <= 0 && fweight == WeightNormal && fstyle == FontStyleNormal && fvar == FontMono
          sortMark = if isTable then tableSortMarkOf si else 0
      fm' <-
        if isBaseSans
          then pure (ctxFontMetrics ctx)
          else if isBaseMono
            then pure (ctxMonoFontMetrics ctx)
            else fst <$> ctxResolveFont ctx fontSizeVal fweight fstyle fvar
      sortSlotW <-
        if isTable && sortMark /= 0 && not terminal
          then fst <$> ctxMeasureText ctx (tableSortBlank False)
          else pure 0
      let lastLine = length placements - 1
      forM_ (zip [0 :: Int ..] placements) $ \(i, (txt, px, py, tw, th)) ->
        unless (T.null txt) $ do
          pushText da fm' px py txt widgetFg
          -- Table sort arrow: the label text ends in the blank reserve slot
          -- (the ▲/▼ codepoint is not in the pruned UI font), so paint the
          -- mark as a triangle centered in that slot — once, on the line
          -- that carries the slot.
          when (isTable && sortMark /= 0 && not terminal && i == lastLine) $
            drawSortTriangle da (px + tw - sortSlotW / 2) (py + th / 2) (sortMark == 2) widgetFg

-- | Sort direction triangle for a table header: up when ascending, down when
-- descending, centered on the label line in the header's reserved slot.
drawSortTriangle :: DrawArena -> Float -> Float -> Bool -> Color -> IO ()
drawSortTriangle da cx cy down col =
  if down
    then pushFilledTriangle da (cx - 5) (cy - 3.5) (cx + 5) (cy - 3.5) cx (cy + 3.5) col
    else pushFilledTriangle da (cx - 5) (cy + 3.5) (cx + 5) (cy + 3.5) cx (cy - 3.5) col

paintTextFieldFrame :: DrawArena -> Theme -> Style -> Bool -> Rect -> IO ()
paintTextFieldFrame da theme style focus fieldRect = do
  let borderCol = if focus then themeAccent theme else styleBorder style
      fieldStyle = style {styleBorder = borderCol}
  fillStyledRect da False style fieldRect
  strokeStyledRect da False fieldStyle (rectX fieldRect) (rectY fieldRect) (rectW fieldRect) (rectH fieldRect)

-- | Draw a single-line field's text, selection, and caret inside @clip@.
-- @penX/penY@ locate @txt@ (absolute); the node rect @x y w h@ positions the
-- field box that selection / caret geometry is resolved against.
paintClippedFieldText ::
  Context ->
  DrawArena ->
  FontMetrics ->
  Style ->
  NodeIdx ->
  Float ->
  Float ->
  Float ->
  Float ->
  Rect ->
  Float ->
  Float ->
  T.Text ->
  Color ->
  IO ()
paintClippedFieldText ctx da fm style idx x y w h clip penX penY txt fg = do
  withClip da clip $ do
    drawTextInputSelection da ctx idx x y w h style
    unless (T.null txt) $ do
      pushText da fm penX penY txt fg
    drawTextInputCaret da ctx idx x y w h style

-- | Caption-less search field: box fills the node rect, magnifier on the left,
-- clear (×) on the right when there is text, and the editable value / caret /
-- selection confined to the space between them.
paintSearchField :: Context -> DrawArena -> FontMetrics -> Theme -> Style -> NodeIdx -> Bool -> Float -> Float -> Float -> Float -> IO ()
paintSearchField ctx da fm theme style idx focus x y w h = do
  let host = ctxHostProfile ctx
      box = Rect x y w h
      clip = searchFieldTextClip host fm x y w h
      (magRect, clearRect) = searchFieldIconRects host fm x y w h
  paintTextFieldFrame da theme style focus box
  value <- textInputValue ctx idx
  lbl <- getText (ctxNodeArena ctx) idx
  let bg = styleBg style
      baseFg = styleFg style
      iconCol = lerpColor baseFg bg 0.45
  drawSearchMagnifier da magRect iconCol
  let display = textInputSearchBody lbl value focus
      isEmpty = T.null value
  scrollX <- syncTextInputScroll ctx idx x y w h
  (ty, fg) <-
    if T.null display
      then pure (0, baseFg)
      else do
        (_tw, th) <- ctxMeasureText ctx display
        pure
          ( centeredTextY host fm y h th
          , if isEmpty && not focus then lerpColor baseFg bg 0.5 else baseFg
          )
  paintClippedFieldText ctx da fm style idx x y w h clip (rectX clip - scrollX) ty display fg
  when (not isEmpty) $
    drawCloseIcon host fm da (rectX clearRect) (rectY clearRect) (rectW clearRect) (rectH clearRect) iconCol

-- | Bare field: the box fills the node rect with no caption or icon chrome.
-- Callers place their own label beside it.
paintBareField :: Context -> DrawArena -> FontMetrics -> Theme -> Style -> NodeIdx -> Bool -> Float -> Float -> Float -> Float -> IO ()
paintBareField ctx da fm theme style idx focus x y w h = do
  let host = ctxHostProfile ctx
      box = Rect x y w h
      (ix, iy) = widgetContentInset host fm
      clip = Rect (x + ix) (y + iy) (max 0 (w - 2 * ix)) (max 0 (h - 2 * iy))
  paintTextFieldFrame da theme style focus box
  value <- textInputValue ctx idx
  let bg = styleBg style
      baseFg = styleFg style
      display = textInputFieldText "" value focus
      isEmpty = T.null value
  scrollX <- syncTextInputScroll ctx idx x y w h
  (ty, fg) <-
    if T.null display
      then pure (0, baseFg)
      else do
        (_tw, th) <- ctxMeasureText ctx display
        pure
          ( centeredTextY host fm y h th
          , if isEmpty && not focus then lerpColor baseFg bg 0.5 else baseFg
          )
  paintClippedFieldText ctx da fm style idx x y w h clip (rectX clip - scrollX) ty display fg

drawSearchMagnifier :: DrawArena -> Rect -> Color -> IO ()
drawSearchMagnifier da (Rect x y w h) col = do
  let cx = x + w / 2
      cy = y + h / 2
      s = min w h
      r0 = s * 0.36
      t = max 1.4 (s * 0.15)
      startOff = r0 * 0.7071
      endOff = r0 * 0.7071 + s * 0.22
  pushRoundedStroke da (Rect (cx - r0) (cy - r0) (2 * r0) (2 * r0)) r0 t col
  pushLine da (cx + startOff) (cy + startOff) (cx + endOff) (cy + endOff) t col

-- | Combo box field: the search field's full-rect editable box, but styled
-- like a dropdown — no magnifier or clear chrome, and a select chevron in the
-- right reserve that flips up while the dropdown is open (i.e. focused).
paintComboField :: Context -> DrawArena -> FontMetrics -> Theme -> Style -> NodeIdx -> Bool -> Float -> Float -> Float -> Float -> IO ()
paintComboField ctx da fm theme style idx focus x y w h = do
  let host = ctxHostProfile ctx
      box = Rect x y w h
      clip = comboTextClip host fm x y w h
      chevW = selectChevronReserve
  paintTextFieldFrame da theme style focus box
  value <- textInputValue ctx idx
  lbl <- getText (ctxNodeArena ctx) idx
  let bg = styleBg style
      baseFg = styleFg style
      iconCol = lerpColor baseFg bg 0.45
      cx = selectChevronCenterX x w
      cy = y + h / 2
      hw = 4.2
      hh = 2.6
  -- Chevron points down while closed, up while the dropdown is open.
  if focus
    then pushFilledTriangle da (cx - hw) (cy + hh * 0.35) (cx + hw) (cy + hh * 0.35) cx (cy - hh) iconCol
    else drawSelectChevron da (x + w - chevW) y chevW h iconCol
  let display = textInputSearchBody lbl value focus
      isEmpty = T.null value
  scrollX <- syncTextInputScroll ctx idx x y w h
  (ty, fg) <-
    if T.null display
      then pure (0, baseFg)
      else do
        (_tw, th) <- ctxMeasureText ctx display
        pure
          ( centeredTextY host fm y h th
          , if isEmpty && not focus then lerpColor baseFg bg 0.5 else baseFg
          )
  paintClippedFieldText ctx da fm style idx x y w h clip (rectX clip - scrollX) ty display fg

verticallyCenteredBox :: Float -> Float -> Float -> Float
verticallyCenteredBox y h box =
  let slotH = min h (box + 4)
   in y + max 0 ((slotH - box) / 2)

drawChoiceControl ::
  HostProfile ->
  DrawArena ->
  FontMetrics ->
  Style ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Color ->
  Color ->
  Bool ->
  (Float -> Float -> Float -> IO ()) ->
  IO ()
drawChoiceControl host da fm style x y h r bw value accent well solidChecked postMark = do
  let (ix, _) =
        if isCellHost host
          then widgetContentInset host fm
          else labelContentInset host fm
      box = checkboxBoxSize host fm
      bx = x + ix
      by = verticallyCenteredBox y h box
      outer = Rect bx by box box
      checked = value >= 0.5
  if checked && solidChecked
    then do
      pushRoundedRect da outer r accent
      pushRoundedStroke da outer r bw accent
      postMark bx by box
    else do
      let inner = Rect (bx + bw) (by + bw) (box - 2 * bw) (box - 2 * bw)
          innerR = max 0 (r - bw)
          strokeCol = if checked then accent else styleBorder style
      pushRoundedRect da inner innerR well
      pushRoundedStroke da outer r bw strokeCol
      when checked $ postMark bx by box

drawCheckbox ::
  HostProfile ->
  DrawArena ->
  FontMetrics ->
  Style ->
  Float ->
  Float ->
  Float ->
  Float ->
  Color ->
  Color ->
  IO ()
drawCheckbox host da fm style x y h value accent well =
  let box = checkboxBoxSize host fm
      r = min 6 (box / 3.5)
      bw = 1.5
   in drawChoiceControl host da fm style x y h r bw value accent well True $ \bx by b ->
        drawCheckboxMark da bx by b (colorRGBA 255 255 255 255)

drawCheckboxMark :: DrawArena -> Float -> Float -> Float -> Color -> IO ()
drawCheckboxMark da bx by box markCol = do
  let t = max 1.6 (box * 0.11)
      x0 = bx + box * 0.22
      y0 = by + box * 0.52
      x1 = bx + box * 0.42
      y1 = by + box * 0.72
      x2 = bx + box * 0.78
      y2 = by + box * 0.28
      capR = t / 2
      cap cx cy =
        pushRoundedRect da (Rect (cx - capR) (cy - capR) t t) capR markCol
  pushStrokeAA da x0 y0 x1 y1 t markCol
  pushStrokeAA da x1 y1 x2 y2 t markCol
  cap x0 y0
  cap x1 y1
  cap x2 y2

drawRadio ::
  HostProfile ->
  DrawArena ->
  FontMetrics ->
  Style ->
  Float ->
  Float ->
  Float ->
  Float ->
  Color ->
  Color ->
  IO ()
drawRadio host da fm style x y h value accent well =
  let box = checkboxBoxSize host fm
      r = box / 2
      bw = 2
   in drawChoiceControl host da fm style x y h r bw value accent well False $ \bx by b -> do
        s <- readIORef (daSnapScale da)
        let !sx = snapToPixel s bx
            !sy = snapToPixel s by
            !dot = b * 0.72
            !dx = sx + (b - dot) / 2
            !dy = sy + (b - dot) / 2
        pushRoundedRectRaw da (Rect dx dy dot dot) (dot / 2) accent

{-# INLINE walkChildren #-}
walkChildren :: Context -> NodeIdx -> IO ()
walkChildren ctx idx =
  forChildNodes_ (ctxNodeArena ctx) idx (lowerNode ctx)

{-# INLINE walkChildrenWithOccluders #-}
walkChildrenWithOccluders :: Context -> [Rect] -> NodeIdx -> IO ()
walkChildrenWithOccluders ctx occluders idx =
  forChildNodes_ (ctxNodeArena ctx) idx (lowerNodeWithOccluders ctx occluders)

drawCloseIcon :: HostProfile -> FontMetrics -> DrawArena -> Float -> Float -> Float -> Float -> Color -> IO ()
drawCloseIcon _host _fm da x y w h col = do
  let cx = x + w / 2
      cy = y + h / 2
      arm = min w h * 0.21
      t = max 1.75 (min w h * 0.085)
  pushLine da (cx - arm) (cy - arm) (cx + arm) (cy + arm) t col
  pushLine da (cx - arm) (cy + arm) (cx + arm) (cy - arm) t col

drawSelectChevron :: DrawArena -> Float -> Float -> Float -> Float -> Color -> IO ()
drawSelectChevron da x y w h col = do
  let cx = selectChevronCenterX x w
      cy = y + h / 2
      hw = 4.2
      hh = 2.6
  pushFilledTriangle da (cx - hw) (cy - hh * 0.35) (cx + hw) (cy - hh * 0.35) cx (cy + hh) col

drawTreeChevron ::
  DrawArena ->
  HostProfile ->
  FontMetrics ->
  Float ->
  Float ->
  Float ->
  Float ->
  Int ->
  Bool ->
  Color ->
  IO ()
drawTreeChevron da host fm x y w h depth expanded col = do
  let Rect cx cy cw ch = treeChevronRect host fm x y w h depth
      mx = cx + cw / 2
      my = cy + ch / 2
      s = min 4.5 (min cw ch * 0.28)
      t = max 1.4 (s * 0.22)
  if expanded
    then do
      pushLine da (mx - s) (my - s * 0.45) mx (my + s * 0.7) t col
      pushLine da mx (my + s * 0.7) (mx + s) (my - s * 0.45) t col
    else do
      pushLine da (mx - s * 0.45) (my - s) (mx + s * 0.7) my t col
      pushLine da (mx + s * 0.7) my (mx - s * 0.45) (my + s) t col

