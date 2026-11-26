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
import Data.Bits ((.&.))
import Data.IORef (readIORef)
import Data.Maybe (fromMaybe)
import Data.Primitive.PrimArray (PrimArray, readPrimArray)
import qualified Data.Text as T
import Data.Word (Word64)
import NanoUI.Internal.Context (Context (..), WidgetTextPlacement (..), activeEffectNodes, getAnimationValue, getStore)
import NanoUI.Internal.Draw
import NanoUI.Internal.Font
import NanoUI.Internal.Frame.Chrome
import NanoUI.Internal.Frame.Node (nodeAdornmentInsets, resolveFontFor)
import NanoUI.Internal.Frame.Spans (cachedWidgetLabel, forWidgetTextPlacements_, selectableTextGeometry)
import NanoUI.Internal.Frame.TextArea (drawTextAreaContentWith, resolveTextAreaFont)
import NanoUI.Internal.Frame.TextInput
import NanoUI.Internal.Id (WidgetId (..))
import NanoUI.Internal.Layout.Arena
import NanoUI.Internal.Style (AlignX (..), Style, Theme, fadeAlpha, fieldIconColor, styleBg, styleBorder, styleFg, themeAccent, themeButton, themeInput, themeOnAccent)
import NanoUI.Internal.Types (Color (..), Rect (..), clamp, clamp01, colorA, lerpColor, onGrid, rectInflate, rectNonEmpty)
import NanoUI.Internal.WidgetText
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
  , peHotId :: WidgetId
  , peActiveId :: WidgetId
  , peFocusId :: WidgetId
  , pePieces :: PrimArray Float
    -- ^ The frame's damage pieces as @x0, y0, x1, y1@ runs, of which a node
    -- must meet one to paint; empty when the clip is the one piece.
  , peHasEffects :: Bool
    -- ^ Whether the current frame declared any Tier 2 shader effect containers.
  }

-- | Locality helper for callers inside the paint frame loop; a fresh env
-- re-reads the theme once.
{-# NOINLINE buildPaintEnv #-}
buildPaintEnv :: Context -> PrimArray Float -> IO PaintEnv
buildPaintEnv ctx occluders = do
  theme <- readIORef (ctxTheme ctx)
  hot <- readIORef (ctxHotId ctx)
  active <- readIORef (ctxActiveId ctx)
  focus <- readIORef (ctxFocusId ctx)
  focusVisible <- readIORef (ctxFocusVisible ctx)
  pieces <- getClipPieces (ctxDrawArena ctx)
  hasEffects <- not . null <$> activeEffectNodes ctx
  pure PaintEnv
    { peContext = ctx
    , peNodeArena = ctxNodeArena ctx
    , peDrawArena = ctxDrawArena ctx
    , peTheme = theme
    , peScope = 0
    , peFontMetrics = ctxFontMetrics ctx
    , peOccluders = occluders
    , peFocusRing = if focusVisible then focus else WidgetId 0
    , peHotId = hot
    , peActiveId = active
    , peFocusId = focus
    , pePieces = pieces
    , peHasEffects = hasEffects
    }

-- | Single-line text input: selectable, bare, search, combo or captioned field
-- depending on the node's visual style.
{-# NOINLINE paintTextInputNode #-}
paintTextInputNode :: PaintEnv -> NodeIdx -> Rect -> IO ()
paintTextInputNode env idx rect@(Rect x y w h) = do
  let ctx = peContext env
      da = peDrawArena env
      fm = peFontMetrics env
  style <- widgetVisualStyleWithTheme ctx (peTheme env) (peHotId env) (peActiveId env) (peFocusId env) NodeTextInput idx
  focus <- (== peFocusId env) <$> getWidgetId (peNodeArena env) idx
  si <- getStyleIdx (peNodeArena env) idx
  if hasFlag textInputFlagNumeric si
    then do
      -- The box, its value clipped left of the stepper, and the stepper's up
      -- and down arrows beside a rule.
      let (up@(Rect ux _ _ _), down) = numericStepperRects x y w h
          iconCol = lerpColor (styleFg style) (styleBg style) 0.4
      void $ paintCaptionlessField env style idx focus rect (numericTextClip fm x y w h) False $ do
        pushLine da ux (y + 4) ux (y + h - 4) 1 (lerpColor (styleBorder style) (styleBg style) 0.4)
        drawStepArrow da True up iconCol
        drawStepArrow da False down iconCol
    else if hasFlag textInputFlagSelectable si
      then paintSelectableText env style idx rect
      else if hasFlag textInputFlagSearch si
        then do
          opts <- getOptions (peNodeArena env) idx
          let iconCol = fieldIconColor style
              (magRect, Rect cx cy cw ch) = searchInputIconRects fm x y w h
              field clip = paintCaptionlessField env style idx focus rect clip True
          if null opts
            -- A search field: a magnifier on the left and a clear (x) on the
            -- right while there is text.
            then do
              value <- field (searchInputTextClip fm x y w h) (drawSearchMagnifier da magRect iconCol)
              unless (T.null value) $
                drawCloseIcon da False cx cy cw ch iconCol
            -- A combo box: a select chevron in the right reserve that flips up
            -- while the dropdown is open (i.e. focused).
            else void $ field (comboTextClip fm x y w h) $
              drawSelectChevron da focus (x + w - selectChevronReserve) y selectChevronReserve h iconCol
        else do
          (field, clip) <- nodeTextFieldGeom ctx idx x y w h
          void $ paintCaptionlessField env style idx focus field clip True (pure ())

-- | Multi-line text area.
{-# NOINLINE paintTextAreaNode #-}
paintTextAreaNode :: PaintEnv -> NodeIdx -> Rect -> IO ()
paintTextAreaNode env@PaintEnv {peContext = ctx, peDrawArena = da} idx (Rect x y w h) = do
  style <- widgetVisualStyleWithTheme ctx (peTheme env) (peHotId env) (peActiveId env) (peFocusId env) NodeTextArea idx
  areaFm <- resolveTextAreaFont ctx idx
  paintStyledRect da style (Rect x y w h)
  drawTextAreaContentWith da ctx areaFm idx x y w h style

-- | Generic foreground / chrome widget (button, checkbox, radio option, tree
-- row, slider, select, color swatch, table / tab header, ...). Splits into a
-- background pass and a label pass, both behind NOINLINE seams.
{-# NOINLINE paintWidget #-}
paintWidget :: PaintEnv -> NodeIdx -> NodeType -> Rect -> IO ()
paintWidget env@PaintEnv {peContext = ctx} idx nt rect@(Rect _ ry _ rh) = do
  style <- widgetVisualStyleWithTheme ctx (peTheme env) (peHotId env) (peActiveId env) (peFocusId env) nt idx
  a <- arenaArrays (peNodeArena env)
  value <- readStyle a idx StyleNodeValue
  si <- readStyleIdx a idx
  -- Menu rows paint edge-to-edge across the popup panel, exactly like the
  -- text-field context menu painter: the hover fill and the accent marker
  -- span the panel width instead of the (padded) node rect.
  menuRowRect <-
    if nt == NodeButton && buttonKindOf si == ButtonMenu
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
paintWidgetBackground :: PaintEnv -> NodeIdx -> NodeType -> Style -> Word64 -> Rect -> Float -> Rect -> IO ()
paintWidgetBackground env idx nt style si menuRowRect value rect@(Rect x y w h) = do
  let ctx = peContext env
      da = peDrawArena env
      fm = peFontMetrics env
      theme = peTheme env
      !bkind = if nt == NodeButton then buttonKindOf si else ButtonPlain
      !hasBg = colorA (styleBg style) > 0
      !opaqueBg = case bkind of
        ButtonMenu -> hasBg
        ButtonMenuBar -> hasBg
        ButtonTable -> hasBg
        ButtonRow -> hasBg
        ButtonClose -> False
        ButtonTab -> False
        ButtonChoice -> False
        _ -> nt /= NodeSlider && nt /= NodeTextInput && nt /= NodeTextArea && nt /= NodeColorPicker
      !drawStroke = case bkind of
        ButtonTab -> False
        ButtonTable -> False
        ButtonMenu -> False
        ButtonMenuBar -> False
        ButtonRow -> False
        _ -> opaqueBg
      !canGroup =
        opaqueBg
          && drawStroke
          && (nt == NodeSelect || (nt == NodeButton && (bkind == ButtonPlain || bkind == ButtonContent)))
      na = peNodeArena env
  grouped <-
    if canGroup
      then do
        a <- arenaArrays na
        p <- readTree a idx TreeParent
        if p >= 0
          then do
            pt <- readTagEnum a p TagNodeType
            if pt == NodeContainer
              then hasFlag containerFlagButtonGroup <$> readStyleIdx a p
              else pure False
          else pure False
      else pure False
  if grouped
    then do
      (firstC, lastC) <- flowChildEnds na =<< getParent na idx
      dir <- getDirection na =<< getParent na idx
      paintGroupedButton da style dir (idx == firstC) (idx == lastC) rect
    else do
      when opaqueBg $ fillStyledRect da style menuRowRect
      when drawStroke $ strokeStyledRect da style rect
  case nt of
    NodeSlider -> paintSliderBody env x y w h value
    NodeSelect -> drawSelectChevron da False x y w h (styleFg style)
    NodeColorPicker -> do
      store <- getStore ctx
      drawColorPickerPart (peNodeArena env) idx fm da store style (Rect x y w h)
    NodeButton -> case bkind of
      ButtonMenu -> do
        wid <- getWidgetId (peNodeArena env) idx
        when (wid == peHotId env) $ paintMenuAccent da theme menuRowRect
      ButtonTab ->
        paintTabHeader da theme (buttonVisualStyle si) (value > 0.5) style (Rect x y w h)
      ButtonTable ->
        paintTableHeader da theme (value > 0.5) style x y w h
      ButtonChoice -> do
        fontSize <- getNodeFontSize (peNodeArena env) idx
        choiceFm <-
          if fontSize == 0
            then pure fm
            else (\(f, _, _) -> f) <$> resolveFontFor ctx NodeButton fontSize si
        drawChoiceControl da choiceFm style theme x y h value (buttonVisualStyle si == 0)
      ButtonRow -> do
        let (depth, hasKids, expanded) = treeDecodeStyle si
        when hasKids $
          drawTreeChevron da fm x y h depth expanded (styleFg style)
      ButtonClose
        | buttonVisualStyle si == buttonCloseTab -> paintTabClose env idx (Rect x y w h)
        | otherwise -> drawCloseIcon da (buttonVisualStyle si == buttonCloseTrailing) x y w h (styleFg style)
      ButtonPlain -> pure ()
      ButtonMenuBar -> pure ()
      ButtonContent -> pure ()
    -- 'NanoUI.Internal.Frame.Paint.lowerNodeVisible' paints the other node
    -- types itself and calls this only for the four above.
    NodeContainer -> pure ()
    NodeText -> pure ()
    NodeSpacer -> pure ()
    NodeSeparator -> pure ()
    NodeTextInput -> pure ()
    NodeTextArea -> pure ()
    NodeScrollContainer -> pure ()
    NodeModal -> pure ()
    NodeImage -> pure ()
    NodePanel -> pure ()
    NodeWindow -> pure ()
    NodeBox -> pure ()
    NodePopup -> pure ()
    NodeDrawing -> pure ()
    NodeShader -> pure ()

-- | A tab header's close button: its cross in the header's label colour
-- ('NanoUI.Internal.Style.fontColor'), dimmed until the pointer is over it,
-- on a round fill while it is.
{-# NOINLINE paintTabClose #-}
paintTabClose :: PaintEnv -> NodeIdx -> Rect -> IO ()
paintTabClose env idx (Rect x y w h) = do
  let ctx = peContext env
      na = peNodeArena env
      da = peDrawArena env
  wid <- getWidgetId na idx
  let hot = peHotId env
  animT <- getAnimationValue ctx wid
  label <- fromMaybe (styleFg (themeButton (peTheme env))) <$> getNodeFontColor na idx
  let hotT = clamp01 (if wid == hot && not (animT > 0) then 1 else animT)
      d = min w h
  when (hotT > 0) $
    pushRoundedRect da (Rect (x + (w - d) / 2) (y + (h - d) / 2) d d) (d / 2) (fadeAlpha label (round (36 * hotT)))
  drawCloseIcon da False x y w h (lerpColor (fadeAlpha label 150) label hotT)

{-# NOINLINE paintSliderBody #-}
paintSliderBody :: PaintEnv -> Float -> Float -> Float -> Float -> Float -> IO ()
paintSliderBody PaintEnv {peDrawArena = da, peTheme = theme} x y w h value = do
  let track@(Rect tx ty tw th) = sliderTrackBounds x y w h
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
paintWidgetForeground :: PaintEnv -> NodeIdx -> NodeType -> Style -> Word64 -> Rect -> IO ()
paintWidgetForeground env idx nt style si (Rect x y w h) = do
  let ctx = peContext env
      na = peNodeArena env
      da = peDrawArena env
  a <- arenaArrays na
  fcWord <- readPrimArray (naArrFontColor a) idx
  fontSize <- readStyle a idx StyleFontSize
  let !font = nodeFontBits nt si
      !widgetFg =
        if (fcWord .&. 0x100000000) /= 0
          then Color (fromIntegral (fcWord .&. 0xFFFFFFFF))
          else labelColor (peTheme env) style font Nothing
  if hasCenteredLabel nt
    then do
      -- Fast path for plain buttons and selects: skip resolveFontFor when
      -- using the default font and avoid boxing labelBox tuples or
      -- drawPlacement closures.
      (fm, native) <-
        if fontSize == 0 && font == 0
          then pure (peFontMetrics env, True)
          else (\(f, nat, _) -> (f, nat)) <$> resolveFontFor ctx nt fontSize si
      kids <- if nt == NodeButton then readTree a idx TreeFirstChild else pure (-1)
      placement <-
        if kids < 0 && not (nt == NodeButton && tabLabelAtStart si)
          then cachedWidgetLabel ctx nt idx AlignCenter w h
          else paintCenteredBoxed ctx na idx nt si x w h
      case placement of
        Nothing -> pure ()
        Just (WidgetTextPlacement txt px py _ th) ->
          unless (T.null txt) $ do
            (!lx, _) <-
              if kids < 0 && not (nt == NodeButton && tabLabelAtStart si)
                then pure (x, w)
                else buttonLabelOrigin na idx nt si x w
            if font == 0
              then pushText da fm (lx + px) (y + py) txt widgetFg
              else pushStyledLabel da fm native font (lx + px) (y + py) txt widgetFg
            when (nt == NodeButton && buttonKindOf si == ButtonTable) $ do
              let !sortMark = tableSortMarkOf si
              when (sortMark /= 0) $ do
                sortAlign <- readAlignX a idx
                let !sortArrowX =
                      if sortAlign == AlignEnd
                        then x + tableCellInset + 5
                        else x + w - tableCellInset - 5
                drawSortTriangle da sortArrowX (y + py + th / 2) (sortMark == 2) widgetFg
    else do
      (fm, native, _) <- resolveFontFor ctx nt fontSize si
      forWidgetTextPlacements_ ctx nt idx x y w h $ \_ txt px py _ _ ->
        unless (T.null txt) $
          if font == 0 then pushText da fm px py txt widgetFg else pushStyledLabel da fm native font px py txt widgetFg

{-# NOINLINE paintCenteredBoxed #-}
paintCenteredBoxed :: Context -> NodeArena -> NodeIdx -> NodeType -> Word64 -> Float -> Float -> Float -> IO (Maybe WidgetTextPlacement)
paintCenteredBoxed ctx na idx nt si x w h = do
  (lead, trail) <- if nt == NodeButton then nodeAdornmentInsets na idx x w else pure (0, 0)
  let !start = if nt == NodeButton && tabLabelAtStart si then tabHeaderPadX else 0
      (!lw, !ax)
        | lead > 0 = (max 0 (w - lead - trail), AlignStart)
        | trail > 0 = (max 0 (w - trail), AlignEnd)
        | start > 0 = (max 0 (w - start), AlignStart)
        | otherwise = (w, AlignCenter)
  cachedWidgetLabel ctx nt idx ax lw h

{-# NOINLINE buttonLabelOrigin #-}
buttonLabelOrigin :: NodeArena -> NodeIdx -> NodeType -> Word64 -> Float -> Float -> IO (Float, Float)
buttonLabelOrigin na idx nt si x w = do
  (lead, trail) <- if nt == NodeButton then nodeAdornmentInsets na idx x w else pure (0, 0)
  let !start = if nt == NodeButton && tabLabelAtStart si then tabHeaderPadX else 0
  pure $!
    if lead > 0
      then (x + lead, max 0 (w - lead - trail))
      else if trail > 0
        then (x, max 0 (w - trail))
        else if start > 0
          then (x + start, max 0 (w - start))
          else (x, w)

-- | A label in the weight, slant and decoration of font bits @font@
-- ('pushFieldText'). Out of line: inlined, it costs every plain label
-- (measured: 260 bytes per label per frame).
{-# NOINLINE pushStyledLabel #-}
pushStyledLabel :: DrawArena -> FontMetrics -> Bool -> Word64 -> Float -> Float -> T.Text -> Color -> IO ()
pushStyledLabel = pushFieldText

-- | Sort direction triangle for a table header: up when ascending, down when
-- descending, centered on the label line in the header's reserved slot.
drawSortTriangle :: DrawArena -> Float -> Float -> Bool -> Color -> IO ()
drawSortTriangle da cx cy down =
  let tip = if down then 3.5 else -3.5
   in pushFilledTriangle da (cx - 5) (cy - tip) (cx + 5) (cy - tip) cx (cy + tip)

-- | Draw a single-line field's text, its selection, and (when @showCaret@ is
-- set) its caret while it is being edited, inside @clip@. @penX/penY@ locate
-- @txt@ (absolute), drawn in the font style packed in @si@ ('pushFieldText').
{-# INLINE paintClippedFieldText #-}
paintClippedFieldText ::
  Context ->
  DrawArena ->
  FontMetrics ->
  Bool ->
  Word64 ->
  Style ->
  NodeIdx ->
  Bool ->
  Maybe FieldEdit ->
  Rect ->
  Float ->
  Float ->
  T.Text ->
  Color ->
  IO ()
paintClippedFieldText ctx da fm native si style idx showCaret mEdit clip penX penY txt fg =
  withClip da clip $ do
    mapM_ (drawTextInputSelection da ctx idx) mEdit
    unless (T.null txt) $
      pushFieldText da fm native si penX penY txt fg
    when showCaret $
      mapM_ (\edit -> drawTextInputCaret da edit (styleFg style)) mEdit

-- | A caption-less field filling @box@: the box, @chrome@, then its value,
-- or its label as a placeholder (dimmed) while empty and unfocused when it
-- has one, scrolled to keep the caret in @clip@. Returns the value.
paintCaptionlessField :: PaintEnv -> Style -> NodeIdx -> Bool -> Rect -> Rect -> Bool -> IO () -> IO T.Text
paintCaptionlessField env style idx focus box clip hasPlaceholder chrome = do
  let ctx = peContext env
      da = peDrawArena env
  si <- getStyleIdx (peNodeArena env) idx
  paintStyledRect da style box
  chrome
  TextInputFrame fm native value shown penX rowY mEdit <-
    resolveTextInputFrame ctx idx si focus box clip
  placeholder <- if hasPlaceholder then getText (ctxNodeArena ctx) idx else pure ""
  let display = textInputFieldText placeholder shown focus
  fg <- textInputFg ctx idx (styleFg style) (styleBg style) value focus
  paintClippedFieldText ctx da fm native si style idx True mEdit clip penX rowY display fg
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
paintSelectableText env style idx rect@(Rect x y _ h) = do
  let ctx = peContext env
      da = peDrawArena env
  fg <- fieldTextColor ctx idx (styleFg style)
  si <- getStyleIdx (peNodeArena env) idx
  size <- getNodeFontSize (peNodeArena env) idx
  (fm, native, _) <- resolveFontFor ctx NodeTextInput size si
  value <- textInputValue ctx idx
  let (penX, ty, _) = selectableTextGeometry fm x y h
  mEdit <- readFieldEdit ctx idx rect penX
  paintClippedFieldText ctx da fm native si style idx False mEdit rect penX ty value fg

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

-- | The box of a checkbox (@isCheckbox@) or radio option at @x@, centred
-- vertically within @y h@ to match its single-line label. A checkbox draws
-- like 'checkboxOps'; a radio box is a well, with an accent ring and dot when
-- checked.
drawChoiceControl :: DrawArena -> FontMetrics -> Style -> Theme -> Float -> Float -> Float -> Float -> Bool -> IO ()
drawChoiceControl da fm style theme x y h value isCheckbox = do
  let box = checkboxBoxSize fm
      by = y + max 0 ((h - box) / 2)
      checked = value >= 0.5
  if isCheckbox
    then checkboxOps (pushShapeOp da) theme (styleBorder style) x by box checked
    else do
      let !r = box / 2
          !bw = 2
          outer = Rect x by box box
          accent = themeAccent theme
      pushRoundedRect da (rectInflate (-bw) outer) (max 0 (r - bw)) (styleBg (themeInput theme))
      pushRoundedStroke da outer r bw (if checked then accent else styleBorder style)
      when checked $ do
        s <- readIORef (daSnapScale da)
        let !dot = box * 0.72
            !dx = onGrid s x + (box - dot) / 2
            !dy = onGrid s by + (box - dot) / 2
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
  pushPolylineAA da pts t False ButtCap MiterJoin 4 (Flat col)
