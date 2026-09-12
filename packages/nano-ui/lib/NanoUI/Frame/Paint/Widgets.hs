-- Widget chrome painters for NanoUI, extracted from NanoUI.Frame.Paint so the
-- recursive node walker stays small. Every exported painter carries the paint
-- env built by Paint.buildPaintEnv; the walker's explicit dispatch hands each
-- widget node to one of these NOINLINE seams instead of inlining a monolithic
-- body into the loop.
{-# OPTIONS_GHC -fasm -fno-specialise-aggressively #-}

{-# LANGUAGE DataKinds #-}

module NanoUI.Frame.Paint.Widgets
  ( paintWidget
  , paintTextInputNode
  , paintTextAreaNode
  ) where


import Control.Monad (forM_, unless, when)
import Data.IORef (readIORef)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import NanoUI.Widgets.ColorPicker (colorPickerAlphaMode, drawColorPickerPanel)
import NanoUI.Context
  ( Context (..)
  , getStore
  )
import NanoUI.Draw
  ( DrawArena (..)
  , pushFilledTriangle
  , pushLine
  , pushRoundedRect
  , pushRoundedRectRaw
  , pushRoundedStroke
  , pushStrokeAA
  , pushText
  , snapToPixel
  , withClip
  )
import NanoUI.Font
  ( FontMetrics (..)
  , centeredTextY
  , checkboxBoxSize
  , labelContentInset
  , menuAccentInset
  , menuAccentW
  , sliderTrackBounds
  , sliderHandleDiameter
  , treeChevronRect
  , widgetContentInset
  )
import NanoUI.Types (HostProfile, isCellHost)
import NanoUI.Layout.Arena
  ( NodeIdx
  , NodeType (..)
  , getNodeFontColor
  , getNodeFontSize
  , getNodeValue
  , getOptions
  , getStyleIdx
  , getText
  , getWidgetId
  )
import NanoUI.Style
  ( Style
  , Theme
  , styleBg
  , styleBorder
  , styleFg
  , themeAccent
  , themeInput
  )
import NanoUI.Types (Color (..), Rect (..), clamp01, colorA, colorRGBA, lerpColor, rectH, rectW, rectX, rectY)
import NanoUI.WidgetText
  ( buttonFlagsFromStyle
  , buttonVisualStyle
  , comboTextClip
  , isMenuBarStyle
  , isMenuItemStyle
  , searchFieldIconRects
  , searchFieldTextClip
  , selectChevronCenterX
  , selectChevronReserve
  , tableSortBlank
  , tableSortMarkOf
  , textInputBareMode
  , textInputFieldText
  , textInputSearchBody
  , textInputSearchMode
  , textNodeFontStyle
  , textNodeFontVariant
  , textNodeFontWeight
  , treeDecodeStyle
  )
import NanoUI.Frame.Chrome
  ( fillStyledRect
  , paintTabHeader
  , paintTableHeader
  , strokeStyledRect
  , textInputFocused
  , textInputValue
  , widgetVisualStyle
  )
import NanoUI.Frame.Spans (widgetTextPlacements, widgetTextSpans)
import NanoUI.Frame.TextEdit
  ( TextAreaGeom (..)
  , TextInputGeom (..)
  , drawTextAreaContentWith
  , drawTextInputCaret
  , drawTextInputSelection
  , resolveTextAreaFont
  , syncTextInputScroll
  , textAreaGeom
  , textInputFieldTextClip
  , textInputGeom
  )
import NanoUI.Frame.Paint.Types (PaintEnv (..), popupPanelRect, resolveNodeFont)

-- | Single-line text input: bare, search, or combo field depending on the
-- node's visual style.
{-# NOINLINE paintTextInputNode #-}
paintTextInputNode :: PaintEnv -> NodeIdx -> Rect -> IO ()
paintTextInputNode env idx (Rect x y w h) = do
  let ctx = peContext env
      da = peDrawArena env
      fm = peFontMetrics env
      theme = peTheme env
      terminal = peTerminal env
  if terminal
    then pure ()
    else do
      style <- widgetVisualStyle ctx NodeTextInput idx
      focus <- textInputFocused ctx idx
      si <- getStyleIdx (peNodeArena env) idx
      if textInputBareMode si
        then paintBareField ctx da fm theme style idx focus x y w h
        else
          if textInputSearchMode si
            then do
              opts <- getOptions (peNodeArena env) idx
              if null opts
                then paintSearchField ctx da fm theme style idx focus x y w h
                else paintComboField ctx da fm theme style idx focus x y w h
            else do
              let geom = textInputGeom (peHost env) fm x y w h
                  fieldRect = tigFieldRect geom
                  clip = textInputFieldTextClip (peHost env) geom fm
              paintTextFieldFrame da theme style focus fieldRect
              spans <- widgetTextSpans ctx NodeTextInput idx x y w h
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

-- | Multi-line text area.
{-# NOINLINE paintTextAreaNode #-}
paintTextAreaNode :: PaintEnv -> NodeIdx -> Rect -> IO ()
paintTextAreaNode env idx (Rect x y w h) = do
  let ctx = peContext env
      da = peDrawArena env
      host = peHost env
      theme = peTheme env
      terminal = peTerminal env
  if terminal
    then pure ()
    else do
      style <- widgetVisualStyle ctx NodeTextArea idx
      focus <- textInputFocused ctx idx
      areaFm <- resolveTextAreaFont ctx idx
      let fieldRect = tagFieldRect (textAreaGeom host areaFm x y w h)
      paintTextFieldFrame da theme style focus fieldRect
      drawTextAreaContentWith da ctx areaFm idx x y w h style

-- | Generic foreground / chrome widget (button, checkbox, radio, slider,
-- select, tree row, color swatch, table / tab header, ...). Splits into a
-- background pass and a label pass, both behind NOINLINE seams.
{-# NOINLINE paintWidget #-}
paintWidget :: PaintEnv -> NodeIdx -> NodeType -> Rect -> IO ()
paintWidget env idx nt rect@(Rect x y w h) = do
  let ctx = peContext env
  style <- widgetVisualStyle ctx nt idx
  value <- getNodeValue (peNodeArena env) idx
  si <- getStyleIdx (peNodeArena env) idx
  let (isClose, isTab, isTable) =
        if nt == NodeButton
          then buttonFlagsFromStyle si
          else (False, False, False)
      isMenuItem = nt == NodeButton && isMenuItemStyle si
      isMenu = nt == NodeButton && (isMenuItemStyle si || isMenuBarStyle si)
      opaqueBg = opaqueWidgetBg (peTerminal env) style nt isMenu isClose isTab isTable
  -- Menu rows paint edge-to-edge across the popup panel, exactly like the
  -- text-field context menu painter: the hover fill and the accent marker
  -- span the panel width instead of the (padded) node rect.
  menuRowRect <-
    if not isMenuItem
      then pure rect
      else do
        mPanel <- popupPanelRect ctx idx
        pure $ case mPanel of
          Nothing -> rect
          Just panel -> Rect (rectX panel) (rectY rect) (rectW panel) (rectH rect)
  paintWidgetBackground env idx nt style si isClose isTab isTable isMenu isMenuItem opaqueBg menuRowRect value x y w h
  paintWidgetForeground env idx nt style si isTable x y w h

-- | Opaque-background decision matching the original walker guard, clause
-- order preserved (NodeTree sits between the checkable/radio rules and the
-- remaining cell-aware rules).
opaqueWidgetBg :: Bool -> Style -> NodeType -> Bool -> Bool -> Bool -> Bool -> Bool
opaqueWidgetBg terminal style nt isMenu isClose isTab isTable
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

{-# NOINLINE paintWidgetBackground #-}
paintWidgetBackground :: PaintEnv -> NodeIdx -> NodeType -> Style -> Int -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Rect -> Float -> Float -> Float -> Float -> Float -> IO ()
paintWidgetBackground env idx nt style si isClose isTab isTable isMenu isMenuItem opaqueBg menuRowRect value x y w h = do
  let ctx = peContext env
      da = peDrawArena env
      fm = peFontMetrics env
      host = peHost env
      theme = peTheme env
      terminal = peTerminal env
  when opaqueBg $ fillStyledRect da terminal style menuRowRect
  when (not terminal) $ do
    when (opaqueBg && not isTab && not isTable && not isMenu && nt /= NodeTree) $
      strokeStyledRect da terminal style x y w h
    when isMenu $
      when isMenuItem $ do
        wid <- getWidgetId (peNodeArena env) idx
        hot <- readIORef (ctxHotId ctx)
        when (wid == hot) $ do
          -- Same marker geometry as the text-field context menu; derived
          -- from the shared menu metrics so the two painters cannot drift.
          let barRect =
                Rect
                  (rectX menuRowRect)
                  (y + menuAccentInset)
                  menuAccentW
                  (max 0 (h - 2 * menuAccentInset))
          pushRoundedRect da barRect 1 (themeAccent theme)
    when isTab $
      paintTabHeader
        da
        host
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
        host
        theme
        (value > 0.5)
        style
        x
        y
        w
        h
    case nt of
      NodeCheckbox ->
        drawCheckbox
          host
          da
          fm
          style
          x
          y
          h
          value
          (themeAccent theme)
          (styleBg (themeInput theme))
      NodeRadio ->
        drawRadio
          host
          da
          fm
          style
          x
          y
          h
          value
          (themeAccent theme)
          (styleBg (themeInput theme))
      NodeTree -> do
        let (_, depth, hasKids, expanded) = treeDecodeStyle si
        when hasKids $
          drawTreeChevron
            da
            host
            fm
            x
            y
            w
            h
            depth
            expanded
            (styleFg style)
      NodeSlider -> paintSliderBody env idx x y w h value
      NodeButton -> when isClose $ drawCloseIcon host fm da x y w h (styleFg style)
      NodeSelect -> drawSelectChevron da x y w h (styleFg style)
      NodeColorPicker -> do
        store <- getStore ctx
        wid <- getWidgetId (peNodeArena env) idx
        drawColorPickerPanel (colorPickerAlphaMode si) host fm da store wid style x y w h
      _ -> pure ()

{-# NOINLINE paintSliderBody #-}
paintSliderBody :: PaintEnv -> NodeIdx -> Float -> Float -> Float -> Float -> Float -> IO ()
paintSliderBody env idx x y w h value = do
  let da = peDrawArena env
      fm = peFontMetrics env
      theme = peTheme env
  txt <- getText (peNodeArena env) idx
  let track = sliderTrackBounds (peHost env) fm txt x y w h
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

{-# NOINLINE paintWidgetForeground #-}
paintWidgetForeground :: PaintEnv -> NodeIdx -> NodeType -> Style -> Int -> Bool -> Float -> Float -> Float -> Float -> IO ()
paintWidgetForeground env idx nt style si isTable x y w h = do
  let ctx = peContext env
      da = peDrawArena env
      terminal = peTerminal env
  placements <- widgetTextPlacements ctx nt idx x y w h
  mFontColor <- getNodeFontColor (peNodeArena env) idx
  fontSizeVal <- getNodeFontSize (peNodeArena env) idx
  let widgetFg = fromMaybe (styleFg style) mFontColor
      fvar = textNodeFontVariant si
      fweight = textNodeFontWeight si
      fstyle = textNodeFontStyle si
      sortMark = if isTable then tableSortMarkOf si else 0
  (fm', _) <- resolveNodeFont env fontSizeVal fweight fstyle fvar
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