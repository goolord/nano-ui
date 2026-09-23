-- | Colour picker. The saturation / value field, the hue bar, the alpha bar and
-- the Current / New preview are separate 'NodeColorPicker' nodes in one row,
-- so each bar is its own focus stop with its own keyboard control.
module NanoUI.Internal.Widgets.ColorPicker
  ( ColorPickerPart (..)
  , colorPickerPartOf
  , widgetStoreColor
  , widgetStoreBaseColor
  , colorPickerSvSquare
  , colorPickerPartRect
  , colorPickerPreviewGeom
  , drawColorPickerPart
  , colorPicker
  , colorPicker'
  , colorPickerRGBA
  , colorPickerRGBA'
  )
where

import Control.Monad (forM_, guard, void, when)
import Data.Bits ((.&.))
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Word (Word8)
import Effectful (Eff, type (:>))
import NanoUI.Internal.Context
import NanoUI.Internal.Draw
import NanoUI.Internal.Font
import NanoUI.Internal.Id (WidgetId (..), mix64)
import NanoUI.Internal.Input (Input (..), Key (..), inputKeys, inputKeysElem, inputModifiers, modShift)
import NanoUI.Internal.Layout.Arena
import NanoUI.Internal.Monad (Ui, (<&&>), askContext, askInput, nextId, uiIO, withKey)
import NanoUI.Internal.Store (fieldFloat, fieldInt, fieldPoint, findSlot, insertSlot, lookupSlot, slotWriteOr)
import NanoUI.Internal.Style
import NanoUI.Internal.Types
import NanoUI.Internal.WidgetText
import NanoUI.Internal.Widgets.Behavior
import NanoUI.Internal.Widgets.Node
import NanoUI.Internal.Widgets.NumericInput (NumericInputConfig (..), defaultNumericInputConfig, numericInputConfigured)
import NanoUI.Widgets.TextEditor (singleLineMode)
import NanoUI.Internal.Widgets.TextInput (editTextField)

colorPickerDefaultColor :: Color
colorPickerDefaultColor = colorRGBA 128 128 128 255

colorPickerBarW :: Float
colorPickerBarW = 14

-- | The piece of a colour picker a 'NodeColorPicker' node paints, kept in the
-- low bits of its style.
data ColorPickerPart = PickerSv | PickerHue | PickerAlpha | PickerPreview
  deriving (Eq, Show, Enum, Bounded)

{-# INLINE colorPickerPartOf #-}
colorPickerPartOf :: Int -> ColorPickerPart
colorPickerPartOf si = toEnum (si .&. 3)

-- | The id of the container holding a picker's parts, derived from the
-- field's id. The picker's state is keyed on it: every part reads that state
-- at paint time, and a write to the container's key damages the container's
-- rect, which covers them all. Readers take the field's id (the picker's
-- response id) and map it here.
pickerStateId :: WidgetId -> WidgetId
pickerStateId (WidgetId w) = WidgetId (mix64 w 0x434F4C5243414E56)

-- | The store key of a picker's state, from the field's id.
pickerKey :: WidgetId -> Int
pickerKey = intKey . pickerStateId

-- | A colour as the store's int slots hold it.
packColor :: Color -> Int
packColor = fromIntegral . colorToWord32

storeColorAt :: WidgetStore -> Int -> Color -> Color
storeColorAt store key fallback =
  colorFromWord32 (fromIntegral (findSlot fieldInt (packColor fallback) key store))

-- | Picker's current stored colour, or the supplied fallback if absent.
widgetStoreColor :: WidgetStore -> WidgetId -> Color -> Color
widgetStoreColor store wid fallback = storeColorAt store (pickerKey wid) fallback

-- | Picker's comparison colour, falling back to its current colour and then
-- the caller's default.
widgetStoreBaseColor :: WidgetStore -> WidgetId -> Color -> Color
widgetStoreBaseColor store wid fallback =
  storeColorAt
    store
    (slotKey SlotColorBase (pickerKey wid))
    (widgetStoreColor store wid fallback)

-- | The picker's hue, saturation and value. RGB cannot tell hue 0 from 360,
-- and black collapses S, so the hue and S/V the user last set are kept: the
-- hue slider stays at the end it was put at, and the marker does not jitter.
widgetStoreHsv :: WidgetStore -> WidgetId -> Color -> (Float, Float, Float)
widgetStoreHsv store wid fallback =
  let
    (h0, s0, v0) = rgbToHsv (widgetStoreColor store wid fallback)
    (s, v) = findSlot fieldPoint (s0, v0) (pickerKey wid) store
   in
    (findSlot fieldFloat h0 (pickerKey wid) store, s, v)

-- | Store the live colour with the hue and S/V it was set through.
putColorState :: Int -> Color -> (Float, Float, Float) -> WidgetStore -> WidgetStore
putColorState key col (h, s, v) =
  insertSlot fieldInt key (packColor col)
    . insertSlot fieldFloat key h
    . insertSlot fieldPoint key (s, v)

-- | The square the saturation / value field fills, centered in its node.
colorPickerSvSquare :: Rect -> Rect
colorPickerSvSquare (Rect x y w h) =
  let s = max 0 (min w h)
   in Rect (x + (w - s) / 2) (y + (h - s) / 2) s s

-- | The field node of the picker a part belongs to: the part's sibling that
-- paints the saturation / value square. Its widget id names the picker.
pickerSvNode :: NodeArena -> NodeIdx -> IO NodeIdx
pickerSvNode na idx = do
  parent <- getParent na idx
  if parent < 0
    then pure idx
    else fmap (fromMaybe idx) . firstChildJustM na parent $ \ci -> do
      nt <- getNodeType na ci
      si <- getStyleIdx na ci
      pure (ci <$ guard (nt == NodeColorPicker && colorPickerPartOf si == PickerSv))

-- | Where the part at @idx@ (laid out at @rect@) draws: the field's square, or
-- the part's column cut to the square's height so the bars and the preview
-- line up with the field.
colorPickerPartRect :: NodeArena -> NodeIdx -> Rect -> IO Rect
colorPickerPartRect na idx rect@(Rect x _ w _) = do
  si <- getStyleIdx na idx
  case colorPickerPartOf si of
    PickerSv -> pure (colorPickerSvSquare rect)
    _ -> do
      (sx, sy0, sw, sh) <- pickerSvNode na idx >>= getRect na
      let Rect _ sy _ side = colorPickerSvSquare (Rect sx sy0 sw sh)
      pure (Rect x sy w side)

-- | The preview column's rows, stacked and centered in its band: the Current
-- label's top, its swatch, the New label's top, and its swatch.
colorPickerPreviewGeom :: FontMetrics -> Rect -> (Float, Rect, Float, Rect)
colorPickerPreviewGeom fm (Rect x y w h) =
  let
    labelH = fmLineHeight fm
    swatchW = min 80 w
    swatchH = clamp 0 30 (h - labelH * 2 - colorPickerGap)
    stackH = labelH + swatchH + colorPickerGap + labelH + swatchH
    top = y + max 0 ((h - stackH) / 2)
    currentY = top + labelH
    newLabelY = currentY + swatchH + colorPickerGap
   in
    (top, Rect x currentY swatchW swatchH, newLabelY, Rect x (newLabelY + labelH) swatchW swatchH)

drawChecker :: DrawArena -> Rect -> IO ()
drawChecker da (Rect x y w h) = goRows 0
  where
    s = 6 :: Float
    cols = ceiling (max 0 w / s) :: Int
    rows = ceiling (max 0 h / s) :: Int
    -- Nested range folds retain a shared column list under -O2. Explicit
    -- counters keep both loops numeric, without allocating that list.
    goRows !ry = when (ry < rows) $ do
      goCols ry 0
      goRows (ry + 1)
    goCols !ry !cx = when (cx < cols) $ do
      let
        col =
          if even (ry + cx) then colorRGBA 190 190 190 255 else colorRGBA 140 140 140 255
        rx = x + fromIntegral cx * s
        ry' = y + fromIntegral ry * s
        cw = clamp 0 s (x + w - rx)
        ch = clamp 0 s (y + h - ry')
      pushRect da (Rect rx ry' cw ch) col
      goCols ry (cx + 1)

-- | Paint one part of a picker from its state in the store.
drawColorPickerPart :: NodeArena -> NodeIdx -> FontMetrics -> DrawArena -> WidgetStore -> Style -> Rect -> IO ()
drawColorPickerPart na idx fm da store style rect = do
  si <- getStyleIdx na idx
  owner <- pickerSvNode na idx >>= getWidgetId na
  area@(Rect x y w h) <- colorPickerPartRect na idx rect
  let
    newCol = widgetStoreColor store owner colorPickerDefaultColor
    (hue, sat, val) = widgetStoreHsv store owner colorPickerDefaultColor
    border = styleBorder style
    white = colorRGBA 255 255 255 255
    handleCol = colorRGBA 0 0 0 180
    -- A bar's outline and its handle, @frac@ of the way down.
    barHandle frac = do
      let handle = Rect (x - 3) (y + frac * h - 2) (w + 6) 4
      pushRoundedStroke da area 3 1 border
      pushRoundedRect da handle 2 white
      pushRoundedStroke da handle 2 1 handleCol
  case colorPickerPartOf si of
    PickerSv -> do
      let
        hueCol = hsvToRgb hue 1 1
        dot = Rect (x + sat * w - 3) (y + (1 - val) * h - 3) 6 6
      -- Horizontal: white to hue. Vertical overlay: fade to black (alpha over).
      pushQuadGradient da area white hueCol hueCol white
      pushQuadGradient da area (colorRGBA 0 0 0 0) (colorRGBA 0 0 0 0) (colorRGBA 0 0 0 255) (colorRGBA 0 0 0 255)
      pushRoundedStroke da area 4 1 border
      pushRoundedRect da dot 3 white
      pushRoundedStroke da dot 3 1 handleCol
    PickerHue -> do
      -- Vertical rainbow: each of six stop bands fades into the next.
      let
        cellH = h / 6
        stop i = hsvToRgb (60 * fromIntegral i) 1 1
      forM_ [0 .. 5 :: Int] $ \i ->
        pushQuadGradient da (Rect x (y + fromIntegral i * cellH) w cellH) (stop i) (stop i) (stop (i + 1)) (stop (i + 1))
      barHandle (hue / 360)
    PickerAlpha -> do
      drawChecker da area
      pushQuadGradient da area (fadeAlpha newCol 0) (fadeAlpha newCol 0) (fadeAlpha newCol 255) (fadeAlpha newCol 255)
      barHandle (fromIntegral (colorA newCol) / 255)
    PickerPreview -> do
      let
        (_, current, _, new) = colorPickerPreviewGeom fm area
        swatch r col = do
          drawChecker da r
          pushRect da r col
          pushRoundedStroke da r 0 1 border
      swatch current (widgetStoreBaseColor store owner colorPickerDefaultColor)
      swatch new newCol

-- | The picker's column, and with 'Row' the row of its parts.
colorPickerLayout :: Direction -> Layout
colorPickerLayout dir = tight . fillW . gap colorPickerGap $ defaultLayout {layoutDirection = dir}

-- The field grows up to a square as tall as the row.
colorPickerSvLayout :: Layout
colorPickerSvLayout =
  tight . fillW . fixedH colorPickerSvH . minW 60 . maxW colorPickerSvH $ defaultLayout

-- A row of channel fields. Children are groups sized by 'percent' so the
-- R/G/B(/A) and H/S/V rows share the same column widths.
colorPickerRowLayout :: Layout
colorPickerRowLayout = alignMid (colorPickerLayout Row)

-- One channel field: an inline label plus its bare box, taking @pct@ of the row.
colorPickerFieldGroupLayout :: Float -> Layout
colorPickerFieldGroupLayout pct = percent pct colorPickerRowLayout

colorPickerFieldLayout :: Layout
colorPickerFieldLayout = tight . fillW . minW 40 $ defaultLayout

-- | RGB colour picker: a saturation/value field, a hue bar, and RGB, HSV and
-- hex fields. Pass the current colour; the result is the colour after this
-- frame's edits.
--
-- The field and each bar take keyboard focus in turn. On the field the arrow
-- keys move the marker (left and right for saturation, up and down for
-- value); on a bar they move its handle, and Home and End jump to its ends.
-- Shift takes steps ten times larger.
{-# INLINE colorPicker #-}
colorPicker :: Ui :> es => Color -> Eff es Color
colorPicker value = snd <$> colorPickerWith False value

-- | 'colorPicker' returning @(response, updatedColour)@.
colorPicker' :: Ui :> es => Color -> Eff es (Response, Color)
colorPicker' = colorPickerWith False

-- | 'colorPicker' with an alpha bar and an A / @#RRGGBBAA@ field.
{-# INLINE colorPickerRGBA #-}
colorPickerRGBA :: Ui :> es => Color -> Eff es Color
colorPickerRGBA value = snd <$> colorPickerWith True value

-- | 'colorPickerRGBA' returning its response and colour, including alpha.
colorPickerRGBA' :: Ui :> es => Color -> Eff es (Response, Color)
colorPickerRGBA' = colorPickerWith True

-- | The byte fields: label, the channel read, and the channel write.
rgbChannels, rgbaChannels :: [(Text, Color -> Word8, Word8 -> Color -> Color)]
rgbChannels =
  [ ("R", colorR, \v c -> colorRGBA v (colorG c) (colorB c) (colorA c))
  , ("G", colorG, \v c -> colorRGBA (colorR c) v (colorB c) (colorA c))
  , ("B", colorB, \v c -> colorRGBA (colorR c) (colorG c) v (colorA c))
  ]
rgbaChannels = rgbChannels ++ [("A", colorA, flip fadeAlpha)]

-- | The HSV fields: label, the largest value, the shown value, and the
-- (hue, s, v) a typed value makes.
hsvChannels :: [(Text, Int, (Float, Float, Float) -> Int, Int -> (Float, Float, Float) -> (Float, Float, Float))]
hsvChannels =
  [ ("H", 360, \(h, _, _) -> round h, \n (_, s, v) -> (fromIntegral n, s, v))
  , ("S", 100, \(_, s, _) -> round (s * 100), \n (h, _, v) -> (h, fraction n, v))
  , ("V", 100, \(_, _, v) -> round (v * 100), \n (h, s, _) -> (h, s, fraction n))
  ]
  where
    fraction n = fromIntegral n / 100

colorPickerWith ::
  Ui :> es => Bool -> Color -> Eff es (Response, Color)
colorPickerWith showAlpha value = do
  ctx <- askContext
  -- The field's id keys the picker's state.
  wid <- nextId
  hueWid <- nextId
  alphaWid <- nextId
  previewWid <- nextId
  let
    key = pickerKey wid
    pct = 100 / (if showAlpha then 4 else 3)
    readColor = (\st -> widgetStoreColor st wid value) <$> uiIO (getStore ctx)
    writePicker col hsv = uiIO (modifyStore ctx (putColorState key col hsv))
    writeColor col = writePicker col (rgbToHsv col)
    -- Without the alpha bar the colour stays opaque.
    alphaOf c = if showAlpha then colorA c else 255
    part pid p lay = (,) pid <$> addWidgetStyled pid NodeColorPicker "" 0 lay (fromEnum p)
    column w = fixedW w colorPickerSvLayout
  uiIO $ do
    adoptColorPickerValue ctx wid value
    mapM_ (registerFocusable ctx) (wid : hueWid : [alphaWid | showAlpha])
  (start, final, svResp) <- container NodeContainer (colorPickerLayout Column) $ do
    (sv, hue, alphaPart) <-
      container NodeContainer (colorPickerLayout Row) $ do
        tagContainer (pickerStateId wid)
        sv <- part wid PickerSv colorPickerSvLayout
        hue <- part hueWid PickerHue (column colorPickerBarW)
        alphaPart <-
          if showAlpha
            then Just <$> part alphaWid PickerAlpha (column colorPickerBarW)
            else pure Nothing
        -- The Current / New preview: a label above each swatch.
        void (part previewWid PickerPreview (column 112))
        pure (sv, hue, alphaPart)
    start <- colorPickerCanvas sv hue alphaPart value
    -- Only the focused field edits, so each row's fields share one store read.
    rgb <- readColor
    _ <- container NodeContainer colorPickerRowLayout $
      forM_ (if showAlpha then rgbaChannels else rgbChannels) $ \(lbl, get, set) -> do
        let shown = fromIntegral (get rgb)
        n <- channelField pct lbl 255 shown
        when (n /= shown) $
          writeColor (set (fromIntegral n) (fadeAlpha rgb (alphaOf rgb)))
    hsvStore <- uiIO (getStore ctx)
    let
      hsv = widgetStoreHsv hsvStore wid value
      alpha = alphaOf (widgetStoreColor hsvStore wid value)
    _ <- container NodeContainer colorPickerRowLayout $ do
      forM_ hsvChannels $ \(lbl, hi, shown, edit) -> do
        n <- channelField pct lbl hi (shown hsv)
        when (n /= shown hsv) $ do
          let hsv'@(h, s, v) = edit n hsv
          writePicker (fadeAlpha (hsvToRgb h s v) alpha) hsv'
      when showAlpha $
        void (container NodeContainer (colorPickerFieldGroupLayout pct) (pure ()))
    hex <- readColor
    let hexText = if showAlpha then colorToHexA hex else colorToHex hex
    hexWid <- nextId
    (_, thex, fhex, _) <- editTextField hexWid singleLineMode hexText (Just hexText)
    _ <-
      container NodeContainer (colorPickerFieldGroupLayout 100) $
        addWidgetStyled hexWid NodeTextInput "" 0 colorPickerFieldLayout 0
    when (fhex && thex /= hexText) $
      forM_ (colorPickerParseHex thex) $ \(r, g, b, ma) ->
        writeColor (colorRGBA r g b (if showAlpha then fromMaybe (colorA hex) ma else 255))
    final <- readColor
    pure (start, final, snd sv)
  uiIO $ recordSlot fieldInt ctx key (packColor final)
  pure (setChanged (final /= start) svResp, final)

-- | The field and the bars, each its part's id and response: pointer drags,
-- then arrow keys on whichever part holds focus, then committing the
-- "current" swatch when a drag ends or a key moved the colour. Returns the
-- colour the frame started with.
colorPickerCanvas ::
  Ui :> es =>
  (WidgetId, Response) ->
  (WidgetId, Response) ->
  Maybe (WidgetId, Response) ->
  Color ->
  Eff es Color
colorPickerCanvas (wid, svResp) (hueWid, hueResp) alphaPart initial = do
  ctx <- askContext
  inp <- askInput
  store0 <- uiIO (getStore ctx)
  let
    current0 = widgetStoreColor store0 wid initial
    (h0, s0, v0) = widgetStoreHsv store0 wid initial
    -- A press starts the drag of the part it lands on: the parts' hit rects
    -- share no point, the bars' slack being narrower than the gaps.
    svSquare = colorPickerSvSquare (respRect svResp)
    -- A bar's column cut to the field's height, and wider than the painted
    -- bar so the handle is easy to grab.
    barHit resp =
      Rect (rectX (respRect resp) - 2) (rectY svSquare) (rectW (respRect resp) + 4) (rectH svSquare)
  -- An idle drag hands back the value it was given.
  (s, sA, sHeld) <- withKey ("s" :: Text) (useDrag1D DragAxisX 0 1 s0 svSquare)
  (v, vA, vHeld) <- withKey ("v" :: Text) (useDrag1D DragAxisY 1 0 v0 svSquare)
  (h, hA, hHeld) <- withKey ("hue" :: Text) (useDrag1D DragAxisY 0 360 h0 (barHit hueResp))
  (a, aA, aHeld) <-
    withKey ("alpha" :: Text) $
      useDrag1D DragAxisY 0 255 (fromIntegral (colorA current0)) (maybe (Rect 0 0 0 0) (barHit . snd) alphaPart)
  let
    dragging = sA || vA || hA || aA
    alpha = fromIntegral (clamp 0 255 (round a :: Int))
    dragged
      | aA && not (sA || vA || hA) = fadeAlpha current0 alpha
      | otherwise = fadeAlpha (hsvToRgb h s v) (if isJust alphaPart then alpha else 255)
  holdActiveWhile wid dragging
  when (dragging && (dragged /= current0 || h /= h0 || s /= s0 || v /= v0)) $
    uiIO $ modifyStore ctx (putColorState (pickerKey wid) dragged (h, s, v))
  svFocus <- keyboardFocused wid
  hueFocus <- keyboardFocused hueWid
  alphaFocus <- maybe (pure False) (keyboardFocused . fst) alphaPart
  keyMoved <-
    pure (svFocus || hueFocus || alphaFocus)
      <&&> uiIO (applyColorPickerKeys ctx wid initial inp svFocus hueFocus)
  let releasedDrag = (sHeld || vHeld || hHeld || aHeld) && not dragging
  when (releasedDrag || keyMoved) $
    uiIO $ do
      st <- getStore ctx
      commitColorPickerCurrent ctx wid (widgetStoreColor st wid initial)
  pure current0

-- | One channel field: an inline label and a numeric box over @0..hi@ that
-- shows @value@ while unfocused. Returns the value after this frame's edits.
channelField :: Ui :> es => Float -> Text -> Int -> Int -> Eff es Int
channelField pct label hi value =
  container NodeContainer (colorPickerFieldGroupLayout pct) $ do
    labelWid <- nextId
    void (addWidget labelWid NodeText label 0 (tight (alignMid defaultLayout)))
    -- Room for three digits beside the stepper.
    round
      <$> numericInputConfigured
        defaultNumericInputConfig {nicMin = 0, nicMax = fromIntegral hi, nicLayout = minW 60 colorPickerFieldLayout}
        (fromIntegral value)

-- | Adopt the caller's colour as 'adoptSlot' does. A new
-- colour also resets the hue, S/V, and the "current" swatch.
adoptColorPickerValue :: Context -> WidgetId -> Color -> IO ()
adoptColorPickerValue ctx wid value = do
  store0 <- getStore ctx
  let
    key = pickerKey wid
    packed = packColor value
    seenKey = slotKey SlotSeen key
    seen = insertSlot fieldInt seenKey packed store0
  when (lookupSlot fieldInt seenKey store0 /= Just packed) $
    setStore ctx $
      if lookupSlot fieldInt key store0 == Just packed
        then seen
        else putColorState key value (rgbToHsv value) (insertSlot fieldInt (slotKey SlotColorBase key) packed seen)

commitColorPickerCurrent :: Context -> WidgetId -> Color -> IO ()
commitColorPickerCurrent ctx wid col =
  let packed = packColor col
   in writeSlots ctx (slotWriteOr fieldInt packed (slotKey SlotColorBase (pickerKey wid)) packed)

-- | Arrow, Home and End keys on the focused part: the field when @svFocus@,
-- the hue bar when @hueFocus@, otherwise the alpha bar. Arrows move a part
-- the way it is drawn: the marker right for more saturation and up for more
-- value, a bar's handle down (or right) towards its bottom end. Returns
-- whether the colour moved.
applyColorPickerKeys :: Context -> WidgetId -> Color -> Input -> Bool -> Bool -> IO Bool
applyColorPickerKeys ctx wid fallback inp svFocus hueFocus = do
  store <- getStore ctx
  let
    keys = inputKeys inp
    down k = inputKeysElem k keys
    step = if modShift (inputModifiers inp) then 10 else 1
    along neg pos = (if down pos then 1 else 0) - (if down neg then 1 else 0) :: Float
    dx = along KeyLeft KeyRight
    dy = along KeyUp KeyDown
    current = widgetStoreColor store wid fallback
    hsv@(h, s, v) = widgetStoreHsv store wid fallback
    bar lo hi cur
      | down KeyHome = lo
      | down KeyEnd = hi
      | otherwise = clamp lo hi (cur + (dx + dy) * step)
    (col', hsv')
      | svFocus =
          let sat = clamp01 (s + dx * step / 100)
              val = clamp01 (v - dy * step / 100)
           in (fadeAlpha (hsvToRgb h sat val) (colorA current), (h, sat, val))
      | hueFocus =
          let hue = bar 0 360 h
           in (fadeAlpha (hsvToRgb hue s v) (colorA current), (hue, s, v))
      | otherwise = (fadeAlpha current (round (bar 0 255 (fromIntegral (colorA current)))), hsv)
    moved = col' /= current || hsv' /= hsv
  when moved $
    setStore ctx (putColorState (pickerKey wid) col' hsv' store)
  pure moved
