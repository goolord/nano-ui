{-# OPTIONS_GHC -fasm -fno-specialise-aggressively #-}


-- | Painters for controls and text fields. Each receives the shared paint
-- environment; NOINLINE keeps these large bodies out of the recursive node walk.
module NanoUI.Internal.Frame.Paint.Widgets
  ( paintWidget
  , paintTextInputNode
  , paintTextAreaNode
  ) where

import Control.Monad (unless, when)
import Data.IORef (readIORef)
import Data.Maybe (fromMaybe)
import Data.Primitive.PrimArray (primArrayFromListN)
import qualified Data.Text as T
import NanoUI.Internal.Context (Context (..), getStore)
import NanoUI.Internal.Draw
  ( DrawArena (..)
  , pushCircle
  , pushFilledTriangle
  , pushLine
  , pushPolylineAA
  , pushRoundedRect
  , pushRoundedRectRaw
  , pushRoundedStroke
  , pushStrokeAA
  , pushText
  , withClip
  )
import NanoUI.Internal.Font
  ( FontMetrics (..)
  , centeredTextY
  , checkboxBoxSize
  , sliderHandleDiameter
  , sliderTrackBounds
  , tableCellInset
  , treeChevronRect
  )
import NanoUI.Internal.Frame.Chrome
  ( fillStyledRect
  , paintMenuAccent
  , paintTabHeader
  , paintTableHeader
  , strokeStyledRect
  , paintStyledRect
  , textInputFocused
  , textInputValue
  , widgetVisualStyle
  )
import NanoUI.Internal.Frame.Node (resolveFontFor)
import NanoUI.Internal.Frame.Paint.Types (PaintEnv (..), popupPanelRect)
import NanoUI.Internal.Frame.Spans (computeWidgetTextPlacements, forWidgetTextPlacements_, selectableTextGeometry, textInputFg)
import NanoUI.Internal.Frame.TextArea (drawTextAreaContentWith)
import NanoUI.Internal.Frame.TextArea.Content (resolveTextAreaFont)
import NanoUI.Internal.Frame.TextInput
  ( FieldEdit
  , drawTextInputCaret
  , drawTextInputSelection
  , readFieldEdit
  , textInputScroll
  , syncTextInputScroll
  , textInputFieldRect
  , textInputFieldTextClip
  )
import NanoUI.Internal.Layout.Arena
  ( NodeIdx
  , NodeType (..)
  , getAlignX
  , getNodeFontColor
  , getNodeFontSize
  , getNodeValue
  , getOptions
  , getStyleIdx
  , getText
  , getWidgetId
  )
import NanoUI.Internal.Style (AlignX (..), Style, styleBg, styleBorder, styleFg, themeAccent, themeInput, themeOnAccent)
import NanoUI.Internal.Types (Color (..), Rect (..), clamp, clamp01, colorA, lerpColor, onGrid, rectInflate)
import NanoUI.Internal.WidgetText
  ( hasFlag
  , buttonCloseTrailing
  , buttonVisualStyle
  , tabHeaderStyle
  , comboTextClip
  , buttonFlagClose
  , buttonFlagMenuBar
  , buttonFlagMenu
  , buttonFlagTab
  , buttonFlagTable
  , numericStepperRects
  , numericTextClip
  , searchInputIconRects
  , searchInputTextClip
  , selectChevronCenterX
  , selectChevronReserve
  , tableSortMarkOf
  , textInputFlagNumeric
  , textInputFieldText
  , textInputFlagSearch
  , textInputFlagSelectable
  , treeDecodeStyle
  )
import NanoUI.Internal.Widgets.ColorPicker (drawColorPickerPart)

-- | Single-line text input: selectable, bare, search, combo or captioned field
-- depending on the node's visual style.
{-# NOINLINE paintTextInputNode #-}
paintTextInputNode :: PaintEnv -> NodeIdx -> Rect -> IO ()
paintTextInputNode env idx rect@(Rect x y w h) = do
  let ctx = peContext env
      da = peDrawArena env
      fm = peFontMetrics env
  style <- widgetVisualStyle ctx NodeTextInput idx
  focus <- textInputFocused ctx idx
  si <- getStyleIdx (peNodeArena env) idx
  if hasFlag textInputFlagNumeric si
    then paintNumericField ctx da fm style idx focus rect
    else
      if hasFlag textInputFlagSelectable si
        then paintSelectableText env style idx rect
        else
          if hasFlag textInputFlagSearch si
            then do
              opts <- getOptions (peNodeArena env) idx
              if null opts
                then paintSearchInput ctx da fm style idx focus rect
                else paintComboField ctx da fm style idx focus rect
            else do
              let field = textInputFieldRect fm x y w h
              paintStyledRect da style field
              placements <- computeWidgetTextPlacements ctx NodeTextInput idx x y w h
              case placements of
                (txt, fx, fy, _, _) : _ -> do
                  ffg <- textInputFg ctx style idx focus
                  -- The placement above settled the scroll, so read it back
                  -- rather than measure the caret again.
                  mEdit <- readFieldEdit ctx idx x y w h =<< textInputScroll ctx idx
                  paintClippedFieldText ctx da fm style idx mEdit (textInputFieldTextClip fm field) fx fy txt ffg
                [] -> pure ()

-- | Multi-line text area.
{-# NOINLINE paintTextAreaNode #-}
paintTextAreaNode :: PaintEnv -> NodeIdx -> Rect -> IO ()
paintTextAreaNode env idx (Rect x y w h) = do
  let ctx = peContext env
      da = peDrawArena env
  style <- widgetVisualStyle ctx NodeTextArea idx
  areaFm <- resolveTextAreaFont ctx idx
  paintStyledRect da style (Rect x y w h)
  drawTextAreaContentWith da ctx areaFm idx x y w h style

-- | Generic foreground / chrome widget (button, checkbox, radio, slider,
-- select, tree row, color swatch, table / tab header, ...). Splits into a
-- background pass and a label pass, both behind NOINLINE seams.
{-# NOINLINE paintWidget #-}
paintWidget :: PaintEnv -> NodeIdx -> NodeType -> Rect -> IO ()
paintWidget env idx nt rect@(Rect _ ry _ rh) = do
  let ctx = peContext env
  style <- widgetVisualStyle ctx nt idx
  value <- getNodeValue (peNodeArena env) idx
  si <- getStyleIdx (peNodeArena env) idx
  -- Menu rows paint edge-to-edge across the popup panel, exactly like the
  -- text-field context menu painter: the hover fill and the accent marker
  -- span the panel width instead of the (padded) node rect.
  menuRowRect <-
    if nt == NodeButton && hasFlag buttonFlagMenu si
      then maybe rect (\(Rect px _ pw _) -> Rect px ry pw rh) <$> popupPanelRect ctx idx
      else pure rect
  paintWidgetBackground env idx nt style si menuRowRect value rect
  paintWidgetForeground env idx nt style si rect

-- The button kind flags are re-derived here from the style bits rather than
-- passed in: a flags record crossing this NOINLINE seam would be allocated
-- for every widget on every painted frame.
{-# NOINLINE paintWidgetBackground #-}
paintWidgetBackground :: PaintEnv -> NodeIdx -> NodeType -> Style -> Int -> Rect -> Float -> Rect -> IO ()
paintWidgetBackground env idx nt style si menuRowRect value (Rect x y w h) = do
  let ctx = peContext env
      da = peDrawArena env
      fm = peFontMetrics env
      theme = peTheme env
      -- Strict: lazy Bools here would allocate thunks per widget per frame.
      !isButton = nt == NodeButton
      !isClose = isButton && hasFlag buttonFlagClose si
      !isTab = isButton && hasFlag buttonFlagTab si
      !isTable = isButton && hasFlag buttonFlagTable si
      !isMenuItem = isButton && hasFlag buttonFlagMenu si
      !isMenu = isMenuItem || (isButton && hasFlag buttonFlagMenuBar si)
      !hasBg = colorA (styleBg style) > 0
      !opaqueBg
        | isMenu = hasBg
        | isClose || isTab = False
        | isTable || nt == NodeTree = hasBg
        | otherwise =
            nt /= NodeCheckbox && nt /= NodeRadio && nt /= NodeSlider
              && nt /= NodeTextInput && nt /= NodeTextArea && nt /= NodeColorPicker
  when opaqueBg $ fillStyledRect da style menuRowRect
  when (opaqueBg && not (isTab || isTable || isMenu) && nt /= NodeTree) $
    strokeStyledRect da style (Rect x y w h)
  when isMenuItem $ do
    wid <- getWidgetId (peNodeArena env) idx
    hot <- readIORef (ctxHotId ctx)
    -- Same marker as the text-field context menu, from the shared menu
    -- metrics, so the two painters cannot drift.
    when (wid == hot) $ paintMenuAccent da theme menuRowRect
  when isTab $
    paintTabHeader da theme (tabHeaderStyle si) (value > 0.5) style x y w h
  when isTable $
    paintTableHeader da theme (value > 0.5) style x y w h
  case nt of
    NodeCheckbox -> drawCheckbox da fm style x y h value (themeAccent theme) (styleBg (themeInput theme)) (themeOnAccent theme)
    NodeRadio -> drawRadio da fm style x y h value (themeAccent theme) (styleBg (themeInput theme))
    NodeTree -> do
      let (_, depth, hasKids, expanded) = treeDecodeStyle si
      when hasKids $
        drawTreeChevron da fm x y w h depth expanded (styleFg style)
    NodeSlider -> paintSliderBody env x y w h value
    NodeButton -> when isClose $ drawCloseIcon da (buttonVisualStyle si == buttonCloseTrailing) x y w h (styleFg style)
    NodeSelect -> drawSelectChevron da False x y w h (styleFg style)
    NodeColorPicker -> do
      store <- getStore ctx
      drawColorPickerPart (peNodeArena env) idx fm da store style (Rect x y w h)
    _ -> pure ()

{-# NOINLINE paintSliderBody #-}
paintSliderBody :: PaintEnv -> Float -> Float -> Float -> Float -> Float -> IO ()
paintSliderBody env x y w h value = do
  let da = peDrawArena env
      theme = peTheme env
      track@(Rect tx ty tw th) = sliderTrackBounds x y w h
      trackR = 3
      fillW = max 0 (tw * clamp01 value)
      outline = styleBorder (themeInput theme)
      well = lerpColor (styleBg (themeInput theme)) outline 0.35
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
    pushRoundedRect da (Rect innerX innerY innerFillW innerH) fillR (themeAccent theme)
  let handleD = sliderHandleDiameter
      handleCx = tx + clamp (handleD / 2) (tw - handleD / 2) fillW
      handleHy = ty + (th - handleD) / 2
      innerD = handleD - 2
  pushRoundedRect
    da
    (Rect (handleCx - innerD / 2) (handleHy + (handleD - innerD) / 2) innerD innerD)
    (innerD / 2)
    (themeOnAccent theme)
  pushRoundedStroke da (Rect (handleCx - handleD / 2) handleHy handleD handleD) (handleD / 2) bw outline

{-# NOINLINE paintWidgetForeground #-}
paintWidgetForeground :: PaintEnv -> NodeIdx -> NodeType -> Style -> Int -> Rect -> IO ()
paintWidgetForeground env idx nt style si (Rect x y w h) = do
  let ctx = peContext env
      da = peDrawArena env
  mFontColor <- getNodeFontColor (peNodeArena env) idx
  fontSize <- getNodeFontSize (peNodeArena env) idx
  (fm, _, _) <- resolveFontFor ctx nt fontSize si
  let widgetFg = fromMaybe (styleFg style) mFontColor
      sortMark = if nt == NodeButton && hasFlag buttonFlagTable si then tableSortMarkOf si else 0
  -- Table sort arrow: pinned inside the cell inset to the edge the label
  -- does not align to, the right edge for a left-aligned label and the left
  -- for a right-aligned one. The label carries a blank reserve slot on that
  -- side (the ▲/▼ codepoint is not in the pruned UI font), which keeps the
  -- column wide enough for the text and the arrow.
  sortAlign <- if sortMark /= 0 then getAlignX (peNodeArena env) idx else pure AlignStart
  let sortArrowX
        | sortAlign == AlignEnd = x + tableCellInset + 5
        | otherwise = x + w - tableCellInset - 5
      drawPlacement lastLine txt px py _ th =
        unless (T.null txt) $ do
          pushText da fm px py txt widgetFg
          when (sortMark /= 0 && lastLine) $
            drawSortTriangle da sortArrowX (py + th / 2) (sortMark == 2) widgetFg
  forWidgetTextPlacements_ ctx nt idx x y w h drawPlacement

-- | Sort direction triangle for a table header: up when ascending, down when
-- descending, centered on the label line in the header's reserved slot.
drawSortTriangle :: DrawArena -> Float -> Float -> Bool -> Color -> IO ()
drawSortTriangle da cx cy down col =
  if down
    then pushFilledTriangle da (cx - 5) (cy - 3.5) (cx + 5) (cy - 3.5) cx (cy + 3.5) col
    else pushFilledTriangle da (cx - 5) (cy + 3.5) (cx + 5) (cy + 3.5) cx (cy - 3.5) col

-- | Draw a single-line field's text, and its selection and caret while it is
-- being edited, inside @clip@. @penX/penY@ locate @txt@ (absolute).
{-# INLINE paintClippedFieldText #-}
paintClippedFieldText ::
  Context ->
  DrawArena ->
  FontMetrics ->
  Style ->
  NodeIdx ->
  Maybe FieldEdit ->
  Rect ->
  Float ->
  Float ->
  T.Text ->
  Color ->
  IO ()
paintClippedFieldText ctx da fm style idx mEdit clip penX penY txt fg =
  withClip da clip $ do
    mapM_ (drawTextInputSelection da ctx idx) mEdit
    unless (T.null txt) $
      pushText da fm penX penY txt fg
    mapM_ (\edit -> drawTextInputCaret da edit (styleFg style)) mEdit

-- | A caption-less field's value, or @placeholder@ (dimmed) while empty and
-- unfocused, scrolled to keep the caret in @clip@.
paintFieldValue :: Context -> DrawArena -> FontMetrics -> Style -> NodeIdx -> Bool -> Rect -> Rect -> T.Text -> T.Text -> IO ()
paintFieldValue ctx da fm style idx focus (Rect x y w h) clip@(Rect clipX _ _ _) placeholder value = do
  let display = textInputFieldText placeholder value focus
      baseFg = styleFg style
  scrollX <- syncTextInputScroll ctx idx x y w h
  (ty, fg) <-
    if T.null display
      then pure (0, baseFg)
      else do
        (_, th) <- ctxMeasureText ctx display
        pure
          ( centeredTextY fm y h th
          , if T.null value && not focus then lerpColor baseFg (styleBg style) 0.5 else baseFg
          )
  mEdit <- readFieldEdit ctx idx x y w h scrollX
  paintClippedFieldText ctx da fm style idx mEdit clip (clipX - scrollX) ty display fg

-- | Numeric field: the box, its value clipped left of the stepper, and the
-- stepper's up and down arrows beside a rule.
paintNumericField :: Context -> DrawArena -> FontMetrics -> Style -> NodeIdx -> Bool -> Rect -> IO ()
paintNumericField ctx da fm style idx focus box@(Rect x y w h) = do
  paintStyledRect da style box
  value <- textInputValue ctx idx
  let (up@(Rect ux _ _ _), down) = numericStepperRects x y w h
      iconCol = lerpColor (styleFg style) (styleBg style) 0.4
      ruleCol = lerpColor (styleBorder style) (styleBg style) 0.4
  pushLine da ux (y + 4) ux (y + h - 4) 1 ruleCol
  drawStepArrow da True up iconCol
  drawStepArrow da False down iconCol
  paintFieldValue ctx da fm style idx focus box (numericTextClip fm x y w h) "" value

-- | A stepper arrow in its half of the stepper, nudged toward the other half so
-- the pair reads as one control.
drawStepArrow :: DrawArena -> Bool -> Rect -> Color -> IO ()
drawStepArrow da up (Rect sx sy sw sh) col = do
  let cx = sx + sw / 2
      cy = sy + sh / 2 + (if up then 1 else -1)
      hw = 3.6
      tip = if up then -2.4 else 2.4
  pushFilledTriangle da (cx - hw) (cy - tip * 0.35) (cx + hw) (cy - tip * 0.35) cx (cy + tip) col

-- | Caption-less search field: box fills the node rect, magnifier on the left,
-- clear (×) on the right when there is text, and the editable value / caret /
-- selection confined to the space between them.
paintSearchInput :: Context -> DrawArena -> FontMetrics -> Style -> NodeIdx -> Bool -> Rect -> IO ()
paintSearchInput ctx da fm style idx focus box@(Rect x y w h) = do
  let (magRect, Rect cx cy cw ch) = searchInputIconRects fm x y w h
      iconCol = lerpColor (styleFg style) (styleBg style) 0.45
  paintStyledRect da style box
  value <- textInputValue ctx idx
  lbl <- getText (ctxNodeArena ctx) idx
  drawSearchMagnifier da magRect iconCol
  paintFieldValue ctx da fm style idx focus box (searchInputTextClip fm x y w h) lbl value
  unless (T.null value) $
    drawCloseIcon da False cx cy cw ch iconCol

-- | Selectable text: chrome-less, border-less, naturally sized text field
-- that supports mouse drag selection and text copying without an insertion caret.
paintSelectableText :: PaintEnv -> Style -> NodeIdx -> Rect -> IO ()
paintSelectableText env style idx rect@(Rect x y w h) = do
  let ctx = peContext env
      da = peDrawArena env
      arena = peNodeArena env
  si <- getStyleIdx arena idx
  mFontColor <- getNodeFontColor arena idx
  fontSize <- getNodeFontSize arena idx
  (fm, _, _) <- resolveFontFor ctx NodeTextInput fontSize si
  value <- textInputValue ctx idx
  let (penX, ty, _) = selectableTextGeometry fm x y h
  mEdit <- readFieldEdit ctx idx x y w h 0
  withClip da rect $ do
    mapM_ (drawTextInputSelection da ctx idx) mEdit
    unless (T.null value) $
      pushText da fm penX ty value (fromMaybe (styleFg style) mFontColor)

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
  pushLine da (cx + startOff) (cy + startOff) (cx + endOff) (cy + endOff) (t * 0.8) col

-- | Combo box field: the search field's full-rect editable box, but styled
-- like a dropdown: no magnifier or clear chrome, and a select chevron in the
-- right reserve that flips up while the dropdown is open (i.e. focused).
paintComboField :: Context -> DrawArena -> FontMetrics -> Style -> NodeIdx -> Bool -> Rect -> IO ()
paintComboField ctx da fm style idx focus box@(Rect x y w h) = do
  paintStyledRect da style box
  value <- textInputValue ctx idx
  lbl <- getText (ctxNodeArena ctx) idx
  drawSelectChevron
    da
    focus
    (x + w - selectChevronReserve)
    y
    selectChevronReserve
    h
    (lerpColor (styleFg style) (styleBg style) 0.45)
  paintFieldValue ctx da fm style idx focus box (comboTextClip fm x y w h) lbl value

verticallyCenteredBox :: Float -> Float -> Float -> Float
verticallyCenteredBox y h box =
  let slotH = min h (box + 4)
   in y + max 0 ((slotH - box) / 2)

drawChoiceControl ::
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
drawChoiceControl da fm style x y h r bw value accent well solidChecked postMark = do
  let box = checkboxBoxSize fm
      bx = x
      by = verticallyCenteredBox y h box
      outer = Rect bx by box box
      checked = value >= 0.5
  if checked && solidChecked
    then do
      pushRoundedRect da outer r accent
      pushRoundedStroke da outer r bw accent
      postMark bx by box
    else do
      let inner = rectInflate (-bw) outer
          innerR = max 0 (r - bw)
          strokeCol = if checked then accent else styleBorder style
      pushRoundedRect da inner innerR well
      pushRoundedStroke da outer r bw strokeCol
      when checked $ postMark bx by box

drawCheckbox :: DrawArena -> FontMetrics -> Style -> Float -> Float -> Float -> Float -> Color -> Color -> Color -> IO ()
drawCheckbox da fm style x y h value accent well mark =
  let box = checkboxBoxSize fm
      r = min 6 (box / 3.5)
      bw = 1.5
   in drawChoiceControl da fm style x y h r bw value accent well True $ \bx by b ->
        drawCheckboxMark da bx by b mark

drawCheckboxMark :: DrawArena -> Float -> Float -> Float -> Color -> IO ()
drawCheckboxMark da bx by box markCol = do
  let t = max 1.6 (box * 0.11)
      x0 = bx + box * 0.22
      y0 = by + box * 0.52
      x1 = bx + box * 0.42
      y1 = by + box * 0.72
      x2 = bx + box * 0.78
      y2 = by + box * 0.28
      -- Caps snap their centres, as the strokes snap their ends; snapping a
      -- cap's corner lands it up to a pixel off the stroke at a fractional
      -- scale.
      cap cx cy = pushCircle da cx cy (t / 2) markCol
  pushStrokeAA da x0 y0 x1 y1 t markCol
  pushStrokeAA da x1 y1 x2 y2 t markCol
  cap x0 y0
  cap x1 y1
  cap x2 y2

drawRadio :: DrawArena -> FontMetrics -> Style -> Float -> Float -> Float -> Float -> Color -> Color -> IO ()
drawRadio da fm style x y h value accent well =
  let box = checkboxBoxSize fm
      r = box / 2
      bw = 2
   in drawChoiceControl da fm style x y h r bw value accent well False $ \bx by b -> do
        s <- readIORef (daSnapScale da)
        let !dot = b * 0.72
            !dx = onGrid s bx + (b - dot) / 2
            !dy = onGrid s by + (b - dot) / 2
        pushRoundedRectRaw da (Rect dx dy dot dot) (dot / 2) accent

-- | A cross centered in the box, or against its right edge when @trailing@.
drawCloseIcon :: DrawArena -> Bool -> Float -> Float -> Float -> Float -> Color -> IO ()
drawCloseIcon da trailing x y w h col = do
  let arm = min w h * 0.21
      t = max 1.3 (min w h * 0.064)
      cx = if trailing then x + w - arm - t / 2 else x + w / 2
      cy = y + h / 2
  pushLine da (cx - arm) (cy - arm) (cx + arm) (cy + arm) t col
  pushLine da (cx - arm) (cy + arm) (cx + arm) (cy - arm) t col

-- | Select chevron centered in the right reserve of @x w@; points up when
-- @up@ (an open combo dropdown), down otherwise.
drawSelectChevron :: DrawArena -> Bool -> Float -> Float -> Float -> Float -> Color -> IO ()
drawSelectChevron da up x y w h col = do
  let cx = selectChevronCenterX x w
      cy = y + h / 2
      hw = 4.2
      tip = if up then -2.6 else 2.6
  pushFilledTriangle da (cx - hw) (cy - tip * 0.35) (cx + hw) (cy - tip * 0.35) cx (cy + tip) col

drawTreeChevron :: DrawArena -> FontMetrics -> Float -> Float -> Float -> Float -> Int -> Bool -> Color -> IO ()
drawTreeChevron da fm x y w h depth expanded col = do
  let Rect cx cy cw ch = treeChevronRect fm x y w h depth
      mx = cx + cw / 2
      my = cy + ch / 2
      s = min 4.5 (min cw ch * 0.28)
      t = max 1.5 (s * 0.3)
      -- One mitered polyline, not two capped lines: the caps of a line this
      -- thin are single-pixel squares, and the arms snapped apart.
      pts
        | expanded = [mx - s, my - s * 0.45, mx, my + s * 0.7, mx + s, my - s * 0.45]
        | otherwise = [mx - s * 0.45, my - s, mx + s * 0.7, my, mx - s * 0.45, my + s]
  pushPolylineAA da (primArrayFromListN 6 pts) t False col
