{-# LANGUAGE DataKinds #-}

module NanoUI.Frame.Paint
  ( lowerShapes
  , lowerNode
  , walkChildren
  ) where


import Control.Monad (forM_, unless, when)
import Data.Word (Word32)
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import NanoUI.Widgets.ColorPicker (drawColorPickerPanel)
import NanoUI.Context
  ( Context (..)
  , atlasTextureId
  , getStore
  , lookupImageUv
  )
import NanoUI.Draw
  ( DrawArena
  , getCurrentClip
  , pushFilledTriangle
  , pushImage
  , pushLine
  , pushRect
  , pushRoundedRect
  , pushRoundedStroke
  , pushText
  , withClip
  )
import NanoUI.Font
  ( FontMetrics
  , checkboxBoxSize
  , labelContentInset
  , pickMonoFont
  , sliderTrackBounds
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
  , getAspect
  , getDirection
  , getHeightSizing
  , getNodeType
  , getNodeValue
  , getPadding
  , getRect
  , getStyleIdx
  , getText
  , getWidthSizing
  , getWidgetId
  )
import NanoUI.Layout.Solve (scrollBarSlotOf)
import NanoUI.Style
  ( Style (..)
  , Theme (..)
  , styleBg
  , styleBorder
  , styleFg
  , themeAccent
  , themeInput
  , themePanel
  , themeSeparator
  , themeWindow
  )
import NanoUI.Types (Color (..), ImageId (..), Rect (..), colorA, colorRGBA, clamp01, lerpColor, rectIntersect, rectH, rectW, rectX, rectY)
import NanoUI.WidgetText (buttonFlagsFromStyle, buttonVisualStyle, selectChevronCenterX, sliderLabelText, tableStripeColor, treeDecodeStyle)
import NanoUI.Frame.Chrome
  ( fillStyledRect
  , imageIdFromText
  , paintTabHeader
  , paintTableHeader
  , strokeStyledRect
  , textInputFocused
  , widgetVisualStyle
  )
import NanoUI.Frame.Scroll.Geometry (borderContentClip, padContentClip, scrollContentClip)
import NanoUI.Frame.Scroll (paintScrollChrome)
import NanoUI.Frame.Scroll.Geometry
  ( decodeScrollConfig
  , isScrollStyle2D
  , scrollChromeActive
  , scrollViewportClip2D
  )
import NanoUI.Frame.Spans (collectNodeTextSpans, widgetTextPlacements, widgetTextSpans)
import NanoUI.Frame.TextInput (TextInputGeom (..), drawTextInputCaret, drawTextInputSelection, textInputFieldTextClip, textInputGeom)
import NanoUI.Frame.TextArea (TextAreaGeom (..), drawTextAreaContent, textAreaGeom)

lowerShapes :: Context -> IO ()
lowerShapes ctx = do
  count <- arenaCount (ctxNodeArena ctx)
  when (count > 0) $ lowerNode ctx 0

lowerNode :: Context -> NodeIdx -> IO ()
lowerNode ctx idx = do
  nt <- getNodeType (ctxNodeArena ctx) idx
  (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
  let rect = Rect x y w h
      fm = ctxFontMetrics ctx
      theme = ctxTheme ctx
      terminal = isCellHost (ctxHostProfile ctx)
      da = ctxDrawArena ctx
  clip <- getCurrentClip da
  case rectIntersect rect clip of
    Nothing -> pure ()
    Just _ -> lowerNodeVisible ctx idx nt x y w h rect fm theme terminal da

lowerNodeVisible ::
  Context ->
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
lowerNodeVisible ctx idx nt x y w h rect fm theme terminal da =
  case nt of
    NodeContainer -> walkChildren ctx idx
    NodePanel -> do
      let style = themePanel theme
      fillStyledRect da terminal style rect
      strokeStyledRect da terminal style x y w h
      withClip da (borderContentClip style rect) $ walkChildren ctx idx
    NodeScrollContainer -> do
      let style = themeInput theme
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
          wellStyle = style {styleCornerRadius = 0}
      (showChrome, inner) <-
        if native2D
          then do
            contentH <- getNodeValue (ctxNodeArena ctx) idx
            contentW <- getAspect (ctxNodeArena ctx) idx
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
      let paintWell = showChrome && not (wTag == SizingGrow && hTag == SizingGrow)
      when paintWell $ do
        fillStyledRect da terminal wellStyle rect
        strokeStyledRect da terminal wellStyle x y w h
      withClip da inner $ walkChildren ctx idx
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
        forM_ spans $ \(Rect tx ty _ _, line, spanFg, _) ->
          unless (T.null line) $ do
            let (fm', shown) = pickMonoFont fm (ctxMonoFontMetrics ctx) line
            pushText da fm' tx ty shown spanFg
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
          let geom = textInputGeom (ctxHostProfile ctx) fm x y w h
              fieldRect = tigFieldRect geom
              borderCol =
                if focus
                  then themeAccent theme
                  else styleBorder style
              fieldStyle = style {styleBorder = borderCol}
          fillStyledRect da False style fieldRect
          strokeStyledRect
            da
            False
            fieldStyle
            (rectX fieldRect)
            (rectY fieldRect)
            (rectW fieldRect)
            (rectH fieldRect)
          spans <- widgetTextSpans ctx nt idx x y w h
          case spans of
            (lblSpan : fieldSpan : _) -> do
              let (Rect lx ly _ _, lbl, lfg, _) = lblSpan
                  (Rect fx fy _ _, field, ffg, _) = fieldSpan
                  clip = textInputFieldTextClip (ctxHostProfile ctx) geom fm
              unless (T.null lbl) $ do
                let (lblFm, lblShown) = pickMonoFont fm (ctxMonoFontMetrics ctx) lbl
                pushText da lblFm lx ly lblShown lfg
              withClip da clip $ do
                drawTextInputSelection da ctx idx x y w h style
                unless (T.null field) $ do
                  let (fieldFm, fieldShown) = pickMonoFont fm (ctxMonoFontMetrics ctx) field
                  pushText da fieldFm fx fy fieldShown ffg
                drawTextInputCaret da ctx idx x y w h style
            [lblSpan] -> do
              let (Rect lx ly _ _, lbl, lfg, _) = lblSpan
              unless (T.null lbl) $ do
                let (lblFm, lblShown) = pickMonoFont fm (ctxMonoFontMetrics ctx) lbl
                pushText da lblFm lx ly lblShown lfg
              drawTextInputCaret da ctx idx x y w h style
            _ -> pure ()
    NodeTextArea
      | not terminal -> do
          style <- widgetVisualStyle ctx nt idx
          focus <- textInputFocused ctx idx
          let geom = textAreaGeom (ctxHostProfile ctx) fm x y w h
              fieldRect = tagFieldRect geom
              borderCol =
                if focus
                  then themeAccent theme
                  else styleBorder style
              fieldStyle = style {styleBorder = borderCol}
          fillStyledRect da False style fieldRect
          strokeStyledRect
            da
            False
            fieldStyle
            (rectX fieldRect)
            (rectY fieldRect)
            (rectW fieldRect)
            (rectH fieldRect)
          lbl <- getText (ctxNodeArena ctx) idx
          unless (T.null lbl) $ do
            let (lblFm, lblShown) = pickMonoFont fm (ctxMonoFontMetrics ctx) lbl
                lfg = lerpColor (styleFg style) (themeWindow theme) 0.32
            pushText da lblFm x y lblShown lfg
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
    _ -> do
      style <- widgetVisualStyle ctx nt idx
      value <- getNodeValue (ctxNodeArena ctx) idx
      si <- getStyleIdx (ctxNodeArena ctx) idx
      let (isClose, isTab, isTable) =
            if nt == NodeButton
              then buttonFlagsFromStyle si
              else (False, False, False)
      let opaqueBg
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
        when (opaqueBg && not isTab && not isTable && nt /= NodeTree) $ strokeStyledRect da terminal style x y w h
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
            (colorRGBA 255 255 255 255)
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
          let lbl = sliderLabelText txt
              track = sliderTrackBounds (ctxHostProfile ctx) fm lbl x y w h
              tx = rectX track
              ty = rectY track
              tw = rectW track
              th = rectH track
              trackR = 3
              fillW = max 0 (tw * clamp01 value)
              outline = styleBorder (themeInput theme)
              well = colorRGBA 72 48 48 255
              fill = colorRGBA 204 102 102 255
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
          let handleD = 18
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
          drawColorPickerPanel (ctxHostProfile ctx) fm da store wid style x y w h
      placements <- widgetTextPlacements ctx nt idx x y w h
      forM_ placements $ \(txt, px, py, _, _) ->
        unless (T.null txt) $ do
          let (fm', shown) = pickMonoFont fm (ctxMonoFontMetrics ctx) txt
          pushText da fm' px py shown (styleFg style)

verticallyCenteredBox :: Float -> Float -> Float -> Float
verticallyCenteredBox y h box =
  let slotH = min h (box + 4)
   in y + max 0 ((slotH - box) / 2)

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
  Color ->
  IO ()
drawCheckbox host da fm style x y h value accent well markCol = do
  let (ix, _) =
        if isCellHost host
          then widgetContentInset host fm
          else labelContentInset host fm
      box = checkboxBoxSize host fm
      bx = x + ix
      by = verticallyCenteredBox y h box
      r = min 6 (box / 3.5)
      bw = 2
      outer = Rect bx by box box
      inner = Rect (bx + bw) (by + bw) (box - 2 * bw) (box - 2 * bw)
      innerR = max 0 (r - bw)
  if value >= 0.5
    then do
      pushRoundedRect da outer r accent
      drawCheckboxMark da bx by box markCol
    else do
      pushRoundedStroke da outer r bw (styleBorder style)
      pushRoundedRect da inner innerR well

drawCheckboxMark :: DrawArena -> Float -> Float -> Float -> Color -> IO ()
drawCheckboxMark da bx by box markCol = do
  let t = max 1.6 (box * 0.11)
      x0 = bx + box * 0.22
      y0 = by + box * 0.52
      x1 = bx + box * 0.42
      y1 = by + box * 0.72
      x2 = bx + box * 0.78
      y2 = by + box * 0.28
  pushLine da x0 y0 x1 y1 t markCol
  pushLine da x1 y1 x2 y2 t markCol

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
drawRadio host da fm style x y h value accent well = do
  let (ix, _) =
        if isCellHost host
          then widgetContentInset host fm
          else labelContentInset host fm
      box = checkboxBoxSize host fm
      bx = x + ix
      by = verticallyCenteredBox y h box
      r = box / 2
      bw = 2
      outer = Rect bx by box box
      innerR = max 0 (r - bw)
  if value >= 0.5
    then do
      pushRoundedRect da outer r accent
      let dot = box * 0.42
          dx = bx + (box - dot) / 2
          dy = by + (box - dot) / 2
      pushRoundedRect da (Rect dx dy dot dot) (dot / 2) accent
    else do
      pushRoundedRect da (Rect (bx + bw) (by + bw) (box - 2 * bw) (box - 2 * bw)) innerR well
      pushRoundedStroke da outer r bw (styleBorder style)

walkChildren :: Context -> NodeIdx -> IO ()
walkChildren ctx idx =
  forChildNodes_ (ctxNodeArena ctx) idx (lowerNode ctx)

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

