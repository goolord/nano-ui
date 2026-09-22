{-# OPTIONS_GHC -fasm -fno-specialise-aggressively #-}


-- | Painters for controls and text fields, and the paint environment they
-- share with the node walk. NOINLINE keeps these large bodies out of the
-- recursive node walk.
module NanoUI.Internal.Frame.Paint.Widgets
  ( PaintEnv (..)
  , buildPaintEnv
  , paintWidget
  , paintTextInputNode
  , paintTextAreaNode
  ) where

import Control.Monad (unless, void, when)
import Data.IORef (readIORef)
import Data.Maybe (fromMaybe)
import Data.Primitive.PrimArray (PrimArray)
import qualified Data.Text as T
import NanoUI.Internal.Context (Context (..), getStore)
import NanoUI.Internal.Draw
  ( DrawArena (..)
  , getClipPieces
  , pushCircle
  , pushFilledTriangle
  , pushLine
  , points3
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
import NanoUI.Internal.Frame.Spans (forWidgetTextPlacements_, plainFieldPen, selectableTextGeometry, textInputFg)
import NanoUI.Internal.Frame.TextArea (drawTextAreaContentWith)
import NanoUI.Internal.Frame.TextArea.Content (resolveTextAreaFont)
import NanoUI.Internal.Frame.TextInput
  ( FieldEdit
  , drawTextInputCaret
  , drawTextInputSelection
  , readFieldEdit
  , syncTextInputScroll
  , textInputFieldRect
  , textInputFieldTextClip
  )
import NanoUI.Internal.Id (WidgetId (..))
import NanoUI.Internal.Layout.Arena
  ( NodeArena
  , NodeIdx
  , NodeType (..)
  , getAlignX
  , getNodeFontColor
  , getNodeFontSize
  , getNodeRect
  , getNodeValue
  , getOptions
  , getStyleIdx
  , getText
  , getWidgetId
  , walkFloatingAncestors
  )
import NanoUI.Internal.Style (AlignX (..), Style, Theme, styleBg, styleBorder, styleFg, themeAccent, themeInput, themeOnAccent)
import NanoUI.Internal.Types (Color (..), Rect (..), clamp, clamp01, colorA, lerpColor, onGrid, rectInflate, rectNonEmpty)
import NanoUI.Internal.WidgetText
  ( hasFlag
  , buttonCloseTrailing
  , buttonVisualStyle
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

-- | Context, arenas, fonts, and interaction state read once for a paint pass.
-- Fields retain boxed references so compiler unboxing does not expand the
-- context and theme records at every recursive call.
data PaintEnv = PaintEnv
  { peContext :: Context
  , peNodeArena :: NodeArena
  , peDrawArena :: DrawArena
  , peTheme :: Theme
  , peScope :: Int
    -- ^ The node scope 'peTheme' belongs to. A node in another scope repaints
    -- its subtree with that scope's theme.
  , peFontMetrics :: FontMetrics
  , peOccluders :: PrimArray Float
    -- ^ Opaque floating panel rects as @x0, y0, x1, y1@ runs; empty when the
    -- frame has none.
  , peFocusRing :: WidgetId
    -- ^ The focused widget while its keyboard focus ring shows, else 0.
  , pePieces :: PrimArray Float
    -- ^ The frame's damage pieces as @x0, y0, x1, y1@ runs, of which a node
    -- must meet one to paint; empty when the clip is the one piece.
  }

-- | Locality helper for callers inside the paint frame loop; a fresh env
-- re-reads the theme once.
{-# NOINLINE buildPaintEnv #-}
buildPaintEnv :: Context -> PrimArray Float -> IO PaintEnv
buildPaintEnv ctx occluders = do
  theme <- readIORef (ctxTheme ctx)
  focus <- readIORef (ctxFocusId ctx)
  focusVisible <- readIORef (ctxFocusVisible ctx)
  pieces <- getClipPieces (ctxDrawArena ctx)
  pure PaintEnv
    { peContext = ctx
    , peNodeArena = ctxNodeArena ctx
    , peDrawArena = ctxDrawArena ctx
    , peTheme = theme
    , peScope = 0
    , peFontMetrics = ctxFontMetrics ctx
    , peOccluders = occluders
    , peFocusRing = if focusVisible then focus else WidgetId 0
    , pePieces = pieces
    }

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
  let paint
        -- The box, its value clipped left of the stepper, and the stepper's up
        -- and down arrows beside a rule.
        | hasFlag textInputFlagNumeric si = do
            let (up@(Rect ux _ _ _), down) = numericStepperRects x y w h
                iconCol = lerpColor (styleFg style) (styleBg style) 0.4
            void $ paintCaptionlessField env style idx focus rect (numericTextClip fm x y w h) False $ do
              pushLine da ux (y + 4) ux (y + h - 4) 1 (lerpColor (styleBorder style) (styleBg style) 0.4)
              drawStepArrow da True up iconCol
              drawStepArrow da False down iconCol
        | hasFlag textInputFlagSelectable si = paintSelectableText env style idx rect
        | hasFlag textInputFlagSearch si = do
            opts <- getOptions (peNodeArena env) idx
            let iconCol = lerpColor (styleFg style) (styleBg style) 0.45
                (magRect, Rect cx cy cw ch) = searchInputIconRects fm x y w h
                field clip = paintCaptionlessField env style idx focus rect clip True
            if null opts
              -- A search field: a magnifier on the left and a clear (×) on the
              -- right while there is text.
              then do
                value <- field (searchInputTextClip fm x y w h) (drawSearchMagnifier da magRect iconCol)
                unless (T.null value) $
                  drawCloseIcon da False cx cy cw ch iconCol
              -- A combo box: a select chevron in the right reserve that flips up
              -- while the dropdown is open (i.e. focused).
              else void $ field (comboTextClip fm x y w h) $
                drawSelectChevron da focus (x + w - selectChevronReserve) y selectChevronReserve h iconCol
        | otherwise = do
            let field = textInputFieldRect fm x y w h
            paintStyledRect da style field
            fontSizeVal <- getNodeFontSize (peNodeArena env) idx
            (ffm, _, _) <- resolveFontFor ctx NodeTextInput fontSizeVal si
            -- Paint discards the measured text width that the span path needs,
            -- so it takes the pen directly and skips the host measurement.
            (txt, fx, fy, scrollX) <- plainFieldPen ctx idx si ffm x y w h
            ffg <- textInputFg ctx style idx focus
            -- 'plainFieldPen' settled the scroll, so use it rather than
            -- measuring the caret again.
            mEdit <- readFieldEdit ctx idx x y w h scrollX
            paintClippedFieldText ctx da fm style idx mEdit (textInputFieldTextClip fm field) fx fy txt ffg
  paint

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
      then do
        let na = peNodeArena env
        panel <- walkFloatingAncestors na idx $ \p pnt ->
          if pnt == NodePopup then Just <$> getNodeRect na p else pure Nothing
        pure (maybe rect (\(Rect px _ pw _) -> Rect px ry pw rh) panel)
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
    paintTabHeader da theme (buttonVisualStyle si) (value > 0.5) style x y w h
  when isTable $
    paintTableHeader da theme (value > 0.5) style x y w h
  case nt of
    NodeCheckbox -> drawChoiceControl da fm style theme x y h value True
    NodeRadio -> drawChoiceControl da fm style theme x y h value False
    NodeTree -> do
      let (_, depth, hasKids, expanded) = treeDecodeStyle si
      when hasKids $
        drawTreeChevron da fm x y h depth expanded (styleFg style)
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
      !inner@(Rect innerX innerY innerW innerH) = rectInflate (-bw) track
      innerFillW = max 0 (innerW * clamp01 value)
  pushRoundedStroke da track trackR bw outline
  when (rectNonEmpty inner) $
    pushRoundedRect da inner innerR well
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
drawSortTriangle da cx cy down =
  let tip = if down then 3.5 else -3.5
   in pushFilledTriangle da (cx - 5) (cy - tip) (cx + 5) (cy - tip) cx (cy + tip)

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

-- | A caption-less field filling @box@: the box, @chrome@, then its value,
-- or its label as a placeholder (dimmed) while empty and unfocused when it
-- has one, scrolled to keep the caret in @clip@. Returns the value.
paintCaptionlessField :: PaintEnv -> Style -> NodeIdx -> Bool -> Rect -> Rect -> Bool -> IO () -> IO T.Text
paintCaptionlessField env style idx focus box@(Rect x y w h) clip@(Rect clipX _ _ _) hasPlaceholder chrome = do
  let ctx = peContext env
      da = peDrawArena env
      fm = peFontMetrics env
  paintStyledRect da style box
  value <- textInputValue ctx idx
  placeholder <- if hasPlaceholder then getText (ctxNodeArena ctx) idx else pure ""
  chrome
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
  pure value

-- | A stepper arrow in its half of the stepper, nudged toward the other half so
-- the pair reads as one control.
drawStepArrow :: DrawArena -> Bool -> Rect -> Color -> IO ()
drawStepArrow da up (Rect sx sy sw sh) col = do
  let cx = sx + sw / 2
      cy = sy + sh / 2 + (if up then 1 else -1)
      tip = if up then -2.4 else 2.4
  pushArrowhead da cx cy 3.6 tip col

-- | A filled arrowhead @2 * hw@ wide around @(cx, cy)@, its point @tip@ below
-- the centre (above for a negative @tip@) and its base a third of that the
-- other way. Shared by the stepper arrows and the select chevron.
pushArrowhead :: DrawArena -> Float -> Float -> Float -> Float -> Color -> IO ()
pushArrowhead da cx cy hw tip =
  pushFilledTriangle da (cx - hw) (cy - tip * 0.35) (cx + hw) (cy - tip * 0.35) cx (cy + tip)

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

-- | The box of a checkbox (@isCheckbox@) or radio button at @x@, centred in
-- a slot at most 4 pixels taller than it within @y h@. A checked checkbox is
-- a solid accent box with a check mark; otherwise the box is a well, and a
-- checked radio's has an accent ring and dot.
drawChoiceControl :: DrawArena -> FontMetrics -> Style -> Theme -> Float -> Float -> Float -> Float -> Bool -> IO ()
drawChoiceControl da fm style theme x y h value isCheckbox = do
  let box = checkboxBoxSize fm
      !r = if isCheckbox then min 6 (box / 3.5) else box / 2
      !bw = if isCheckbox then 1.5 else 2
      by = y + max 0 ((min h (box + 4) - box) / 2)
      outer = Rect x by box box
      checked = value >= 0.5
      accent = themeAccent theme
  if checked && isCheckbox
    then do
      pushRoundedRect da outer r accent
      pushRoundedStroke da outer r bw accent
      drawCheckboxMark da x by box (themeOnAccent theme)
    else do
      pushRoundedRect da (rectInflate (-bw) outer) (max 0 (r - bw)) (styleBg (themeInput theme))
      pushRoundedStroke da outer r bw (if checked then accent else styleBorder style)
      when checked $ do
        s <- readIORef (daSnapScale da)
        let !dot = box * 0.72
            !dx = onGrid s x + (box - dot) / 2
            !dy = onGrid s by + (box - dot) / 2
        pushRoundedRectRaw da (Rect dx dy dot dot) (dot / 2) accent

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
      tip = if up then -2.6 else 2.6
  pushArrowhead da cx cy 4.2 tip col

drawTreeChevron :: DrawArena -> FontMetrics -> Float -> Float -> Float -> Int -> Bool -> Color -> IO ()
drawTreeChevron da fm x y h depth expanded col = do
  let Rect cx cy cw ch = treeChevronRect fm x y h depth
      mx = cx + cw / 2
      my = cy + ch / 2
      s = min 4.5 (min cw ch * 0.28)
      t = max 1.5 (s * 0.3)
      -- One mitered polyline, not two capped lines: the caps of a line this
      -- thin are single-pixel squares, and the arms snapped apart.
      pts
        | expanded = points3 (mx - s) (my - s * 0.45) mx (my + s * 0.7) (mx + s) (my - s * 0.45)
        | otherwise = points3 (mx - s * 0.45) (my - s) (mx + s * 0.7) my (mx - s * 0.45) (my + s)
  pushPolylineAA da pts t False col
