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

import Control.Monad (forM_, void, when)
import Data.Bits ((.&.))
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Word (Word8)
import Effectful (Eff, type (:>))
import NanoUI.Internal.Context
  ( recordSlot
  , Context (..)
  , WidgetStore
  , getStore
  , intKey
  , registerFocusable
  , setStore
  , modifyStore
  , writeSlots
  )
import NanoUI.Internal.Draw
  ( DrawArena
  , pushQuadGradient
  , pushRect
  , pushRoundedRect
  , pushRoundedStroke
  )
import NanoUI.Internal.Font
  ( FontMetrics (..)
  )
import NanoUI.Internal.Id (WidgetId (..))
import NanoUI.Internal.Input (Input (..), Key (..), inputKeys, inputKeysElem, inputModifiers, modShift)
import NanoUI.Internal.Layout.Arena
  ( NodeArena
  , NodeIdx
  , NodeType (..)
  , findChildM
  , getNodeType
  , getParent
  , getRect
  , getStyleIdx
  , getWidgetId
  )
import NanoUI.Internal.Monad (Ui, askContext, askInput, nextId, uiIO, withKey)
import NanoUI.Internal.Store (Slot (..), fieldFloat, fieldInt, fieldPoint, findSlot, insertSlot, lookupSlot, slotKey, slotWriteOr)
import NanoUI.Internal.Style
  ( Direction (..)
  , Layout (..)
  , Style (..)
  , alignMid
  , defaultLayout
  , fillW
  , fixedH
  , fixedW
  , gap
  , maxW
  , minW
  , percent
  , tight
  )
import NanoUI.Internal.Types
  ( Color (..)
  , Rect (..)
  , clamp
  , clamp01
  , colorA
  , colorB
  , colorFromWord32
  , colorG
  , colorR
  , colorRGBA
  , colorToWord32
  , hsvToRgb
  , rectH
  , rectW
  , rectX
  , rectY
  , rgbToHsv
  )
import NanoUI.Internal.WidgetText
  ( colorPickerGap
  , colorPickerParseHex
  , colorPickerSvH
  , colorToHex
  , colorToHexA
  )
import NanoUI.Internal.Widgets.Behavior
  ( DragAxis (..)
  , holdActiveWhile
  , keyboardFocused
  , keyedDragHeld
  , useDrag1D
  )
import NanoUI.Internal.Widgets.Node
  ( Response (..)
  , addWidget
  , addWidgetStyled
  , container
  , respRect
  , setChanged
  )
import NanoUI.Internal.Widgets.NumericInput (NumericInputConfig (..), defaultNumericInputConfig, numericInputConfigured)
import NanoUI.Internal.Widgets.TextEditor (singleLineMode)
import NanoUI.Internal.Widgets.TextInput (editTextField)

colorPickerDefaultColor :: Color
colorPickerDefaultColor = colorRGBA 128 128 128 255

colorPickerBarW :: Float
colorPickerBarW = 14

colorPickerSwatchH :: Float
colorPickerSwatchH = 30

colorPickerSwatchW :: Float
colorPickerSwatchW = 80

-- Width reserved for the Current / New preview column (label plus swatch).
colorPickerPreviewW :: Float
colorPickerPreviewW = 112

-- | The piece of a colour picker a 'NodeColorPicker' node paints, kept in the
-- low bits of its style.
data ColorPickerPart = PickerSv | PickerHue | PickerAlpha | PickerPreview
  deriving (Eq, Show, Enum, Bounded)

{-# INLINE colorPickerPartOf #-}
colorPickerPartOf :: Int -> ColorPickerPart
colorPickerPartOf si = toEnum (si .&. 3)

storeColorAt :: WidgetStore -> Int -> Color -> Color
storeColorAt store key fallback =
  colorFromWord32 (fromIntegral (findSlot fieldInt (fromIntegral (colorToWord32 fallback)) key store))

-- | Picker's current stored colour, or the supplied fallback if absent.
widgetStoreColor :: WidgetStore -> WidgetId -> Color -> Color
widgetStoreColor store wid fallback = storeColorAt store (intKey wid) fallback

-- | Picker's comparison colour, falling back to its current colour and then
-- the caller's default.
widgetStoreBaseColor :: WidgetStore -> WidgetId -> Color -> Color
widgetStoreBaseColor store wid fallback =
  storeColorAt
    store
    (slotKey SlotColorBase (intKey wid))
    (widgetStoreColor store wid fallback)

-- RGB cannot tell hue 0 from 360. Keep the slider end the user last set.
widgetStoreHue :: WidgetStore -> WidgetId -> Color -> Float
widgetStoreHue store wid fallback =
  let
    (h0, _, _) = rgbToHsv (widgetStoreColor store wid fallback)
   in
    findSlot fieldFloat h0 (intKey wid) store

-- Black collapses S in RGB. Keep the last mouse S/V so the marker does not jitter.
widgetStoreSv :: WidgetStore -> WidgetId -> Color -> (Float, Float)
widgetStoreSv store wid fallback =
  let
    (_, s0, v0) = rgbToHsv (widgetStoreColor store wid fallback)
   in
    findSlot fieldPoint (s0, v0) (intKey wid) store

-- | Store the live colour with the hue and S/V it was set through.
putColorState :: Int -> Color -> Float -> (Float, Float) -> WidgetStore -> WidgetStore
putColorState key col hue sv =
  insertSlot fieldInt key (fromIntegral (colorToWord32 col))
    . insertSlot fieldFloat key hue
    . insertSlot fieldPoint key sv

withAlpha :: Word8 -> Color -> Color
withAlpha a c = colorRGBA (colorR c) (colorG c) (colorB c) a

-- | The square the saturation / value field fills, centered in its node.
colorPickerSvSquare :: Rect -> Rect
colorPickerSvSquare (Rect x y w h) =
  let s = max 0 (min w h)
   in Rect (x + (w - s) / 2) (y + (h - s) / 2) s s

-- | The field node of the picker a part belongs to: the part's sibling that
-- paints the saturation / value square. Its widget id keys the picker's state.
pickerSvNode :: NodeArena -> NodeIdx -> IO NodeIdx
pickerSvNode na idx = do
  parent <- getParent na idx
  if parent < 0
    then pure idx
    else fmap (fromMaybe idx) . findChildM na parent $ \ci -> do
      nt <- getNodeType na ci
      si <- getStyleIdx na ci
      pure (nt == NodeColorPicker && colorPickerPartOf si == PickerSv)

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
    swatchW = min colorPickerSwatchW w
    swatchH = clamp 0 colorPickerSwatchH (h - labelH * 2 - colorPickerGap)
    stackH = labelH + swatchH + colorPickerGap + labelH + swatchH
    top = y + max 0 ((h - stackH) / 2)
    currentY = top + labelH
    newLabelY = currentY + swatchH + colorPickerGap
   in
    (top, Rect x currentY swatchW swatchH, newLabelY, Rect x (newLabelY + labelH) swatchW swatchH)

-- Wider than the painted bar so the handle is easy to grab.
colorPickerBarHitRect :: Rect -> Rect
colorPickerBarHitRect (Rect x y w h) =
  let
    pad = 2
   in
    Rect (x - pad) y (w + pad * 2) h

drawSvField :: DrawArena -> Rect -> Float -> IO ()
drawSvField da rect hue = do
  let
    white = colorRGBA 255 255 255 255
    hueCol = hsvToRgb hue 1 1
    clear = colorRGBA 0 0 0 0
    black = colorRGBA 0 0 0 255
  -- Horizontal: white to hue. Vertical overlay: fade to black (alpha over).
  pushQuadGradient da rect white hueCol hueCol white
  pushQuadGradient da rect clear clear black black

-- Vertical rainbow: each stop band fades into the next.
drawHueBar :: DrawArena -> Rect -> IO ()
drawHueBar da rect =
  let
    stops = (6 :: Int)
    cellH = rectH rect / fromIntegral stops
    stopCol i = hsvToRgb (360 * fromIntegral i / fromIntegral stops) 1 1
   in
    mapM_
      ( \i ->
          let
            cell = Rect (rectX rect) (rectY rect + fromIntegral i * cellH) (rectW rect) cellH
           in
            pushQuadGradient
              da
              cell
              (stopCol i)
              (stopCol i)
              (stopCol (i + 1))
              (stopCol (i + 1))
      )
      [0 .. stops - 1]

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

drawAlphaBar :: DrawArena -> Rect -> Color -> IO ()
drawAlphaBar da rect col = do
  drawChecker da rect
  let
    c0 = withAlpha 0 col
    c1 = withAlpha 255 col
  pushQuadGradient da rect c0 c0 c1 c1

drawBarHandle :: DrawArena -> Rect -> Float -> Color -> IO ()
drawBarHandle da bar cy col = do
  let
    w = rectW bar
    x = rectX bar
    h = 4
    handle = Rect (x - 3) (cy - h / 2) (w + 6) h
  pushRoundedRect da handle 2 (colorRGBA 255 255 255 255)
  pushRoundedStroke da handle 2 1 col

-- | Paint one part of a picker from its state in the store.
drawColorPickerPart :: NodeArena -> NodeIdx -> FontMetrics -> DrawArena -> WidgetStore -> Style -> Rect -> IO ()
drawColorPickerPart na idx fm da store style rect = do
  si <- getStyleIdx na idx
  owner <- pickerSvNode na idx >>= getWidgetId na
  area <- colorPickerPartRect na idx rect
  let
    newCol = widgetStoreColor store owner colorPickerDefaultColor
    border = styleBorder style
    handleCol = colorRGBA 0 0 0 180
  case colorPickerPartOf si of
    PickerSv -> do
      let
        hue = widgetStoreHue store owner colorPickerDefaultColor
        (sat, val) = widgetStoreSv store owner colorPickerDefaultColor
        marker = 6
        mx = rectX area + sat * rectW area
        my = rectY area + (1 - val) * rectH area
        dot = Rect (mx - marker / 2) (my - marker / 2) marker marker
      drawSvField da area hue
      pushRoundedStroke da area 4 1 border
      pushRoundedRect da dot (marker / 2) (colorRGBA 255 255 255 255)
      pushRoundedStroke da dot (marker / 2) 1 handleCol
    PickerHue -> do
      let hue = widgetStoreHue store owner colorPickerDefaultColor
      drawHueBar da area
      pushRoundedStroke da area 3 1 border
      drawBarHandle da area (rectY area + (hue / 360) * rectH area) handleCol
    PickerAlpha -> do
      drawAlphaBar da area newCol
      pushRoundedStroke da area 3 1 border
      drawBarHandle da area (rectY area + (fromIntegral (colorA newCol) / 255) * rectH area) handleCol
    PickerPreview -> do
      let
        (_, current, _, new) = colorPickerPreviewGeom fm area
        swatch r col = do
          drawChecker da r
          pushRect da r col
          pushRoundedStroke da r 0 1 border
      swatch current (widgetStoreBaseColor store owner colorPickerDefaultColor)
      swatch new newCol

colorPickerLayout :: Layout
colorPickerLayout = tight . fillW . gap colorPickerGap $ defaultLayout

-- The field, bars and preview side by side.
colorPickerCanvasLayout :: Layout
colorPickerCanvasLayout = colorPickerLayout {layoutDirection = Row}

-- The field grows up to a square as tall as the row.
colorPickerSvLayout :: Layout
colorPickerSvLayout =
  tight . fillW . fixedH colorPickerSvH . minW 60 . maxW colorPickerSvH $ defaultLayout

colorPickerColumnLayout :: Float -> Layout
colorPickerColumnLayout w = fixedW w colorPickerSvLayout

-- A row of channel fields. Children are groups sized by 'percent' so the
-- R/G/B(/A) and H/S/V rows share the same column widths.
colorPickerRowLayout :: Layout
colorPickerRowLayout = alignMid colorPickerCanvasLayout

-- One channel field: an inline label plus its bare box, taking @pct@ of the row.
colorPickerFieldGroupLayout :: Float -> Layout
colorPickerFieldGroupLayout pct = percent pct colorPickerRowLayout

colorPickerFieldLayout :: Layout
colorPickerFieldLayout = tight . fillW . minW 40 $ defaultLayout

-- A numeric channel field: room for three digits beside its stepper.
colorPickerChannelLayout :: Layout
colorPickerChannelLayout = minW 60 colorPickerFieldLayout

colorPickerLabelLayout :: Layout
colorPickerLabelLayout = tight (alignMid defaultLayout)

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
rgbaChannels = rgbChannels ++ [("A", colorA, \v c -> colorRGBA (colorR c) (colorG c) (colorB c) v)]

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

-- | Widget ids of a picker's parts. The field's id keys the picker's state.
data PickerParts = PickerParts
  { ppSv :: !WidgetId
  , ppHue :: !WidgetId
  , ppAlpha :: !WidgetId
  , ppPreview :: !WidgetId
  }

colorPickerWith ::
  Ui :> es => Bool -> Color -> Eff es (Response, Color)
colorPickerWith showAlpha value = do
  ctx <- askContext
  parts <- PickerParts <$> nextId <*> nextId <*> nextId <*> nextId
  let
    wid = ppSv parts
    key = intKey wid
    pct = 100 / (if showAlpha then 4 else 3)
    readColor = (\st -> widgetStoreColor st wid value) <$> uiIO (getStore ctx)
    writePicker col hue sv = uiIO (modifyStore ctx (putColorState key col hue sv))
    writeColor col =
      let (h, s, v) = rgbToHsv col
       in writePicker col (clamp 0 360 h) (s, v)
    -- Without the alpha bar the colour stays opaque.
    alphaOf c = if showAlpha then colorA c else 255
    part pid p lay = addWidgetStyled pid NodeColorPicker "" 0 lay (fromEnum p)
  uiIO $ do
    adoptColorPickerValue ctx wid value
    mapM_ (registerFocusable ctx) (wid : ppHue parts : [ppAlpha parts | showAlpha])
  (start, final, svResp) <- container NodeContainer colorPickerLayout $ do
    (svResp, hueResp, alphaResp) <-
      container NodeContainer colorPickerCanvasLayout $ do
        sv <- part wid PickerSv colorPickerSvLayout
        hue <- part (ppHue parts) PickerHue (colorPickerColumnLayout colorPickerBarW)
        alpha <-
          if showAlpha
            then Just <$> part (ppAlpha parts) PickerAlpha (colorPickerColumnLayout colorPickerBarW)
            else pure Nothing
        void (part (ppPreview parts) PickerPreview (colorPickerColumnLayout colorPickerPreviewW))
        pure (sv, hue, alpha)
    start <- colorPickerCanvas parts value svResp hueResp alphaResp
    -- Only the focused field edits, so each row's fields share one store read.
    rgb <- readColor
    _ <- container NodeContainer colorPickerRowLayout $
      forM_ (if showAlpha then rgbaChannels else rgbChannels) $ \(lbl, get, set) -> do
        let shown = fromIntegral (get rgb)
        n <- channelField pct lbl 255 shown
        when (n /= shown) $
          writeColor (set (fromIntegral n) (withAlpha (alphaOf rgb) rgb))
    hsvStore <- uiIO (getStore ctx)
    let
      (s0, v0) = widgetStoreSv hsvStore wid value
      hsv = (widgetStoreHue hsvStore wid value, s0, v0)
      alpha = alphaOf (widgetStoreColor hsvStore wid value)
    _ <- container NodeContainer colorPickerRowLayout $ do
      forM_ hsvChannels $ \(lbl, hi, shown, edit) -> do
        n <- channelField pct lbl hi (shown hsv)
        when (n /= shown hsv) $ do
          let (h, s, v) = edit n hsv
          writePicker (withAlpha alpha (hsvToRgb h s v)) h (s, v)
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
    pure (start, final, svResp)
  uiIO $ recordSlot fieldInt ctx key (fromIntegral (colorToWord32 final))
  pure (setChanged (final /= start) svResp, final)

-- | The field and the bars: pointer drags, then arrow keys on whichever part
-- holds focus, then committing the "current" swatch when a drag ends or a key
-- moved the colour. Returns the colour the frame started with.
colorPickerCanvas :: Ui :> es => PickerParts -> Color -> Response -> Response -> Maybe Response -> Eff es Color
colorPickerCanvas parts initial svResp hueResp alphaResp = do
  ctx <- askContext
  inp <- askInput
  store0 <- uiIO (getStore ctx)
  heldBefore <- or <$> mapM keyedDragHeld (["s", "v", "hue", "alpha"] :: [Text])
  let
    wid = ppSv parts
    showAlpha = isJust alphaResp
    current0 = widgetStoreColor store0 wid initial
    h0 = widgetStoreHue store0 wid initial
    (s0, v0) = widgetStoreSv store0 wid initial
    -- A press starts the drag of the part it lands on: the parts' hit rects
    -- share no point, the bars' slack being narrower than the gaps.
    svSquare = colorPickerSvSquare (respRect svResp)
    band resp = Rect (rectX (respRect resp)) (rectY svSquare) (rectW (respRect resp)) (rectH svSquare)
    hueRect = colorPickerBarHitRect (band hueResp)
    alphaRect = maybe (Rect 0 0 0 0) (colorPickerBarHitRect . band) alphaResp
  (sDrag, sA) <- withKey ("s" :: Text) (useDrag1D DragAxisX 0 1 s0 svSquare)
  (vDrag, vA) <- withKey ("v" :: Text) (useDrag1D DragAxisY 1 0 v0 svSquare)
  (hDrag, hA) <- withKey ("hue" :: Text) (useDrag1D DragAxisY 0 360 h0 hueRect)
  (aDrag, aA) <- withKey ("alpha" :: Text) (useDrag1D DragAxisY 0 255 (fromIntegral (colorA current0)) alphaRect)
  let
    svA = sA || vA
    dragging = svA || hA || aA
    nextHue = if hA then hDrag else h0
    nextS = if sA then sDrag else s0
    nextV = if vA then vDrag else v0
    nextA =
      if aA then clamp 0 255 (round aDrag :: Int) else fromIntegral (colorA current0)
    base = hsvToRgb nextHue nextS nextV
    dragged
      | aA && not (svA || hA) = withAlpha (fromIntegral nextA) current0
      | otherwise = withAlpha (if showAlpha then fromIntegral nextA else 255) base
  holdActiveWhile wid dragging
  when (dragging && (dragged /= current0 || nextHue /= h0 || nextS /= s0 || nextV /= v0)) $
    uiIO $ modifyStore ctx (putColorState (intKey wid) dragged nextHue (nextS, nextV))
  svFocus <- keyboardFocused wid
  hueFocus <- keyboardFocused (ppHue parts)
  alphaFocus <- if showAlpha then keyboardFocused (ppAlpha parts) else pure False
  keyMoved <-
    if not (svFocus || hueFocus || alphaFocus)
      then pure False
      else uiIO (applyColorPickerKeys ctx wid initial inp svFocus hueFocus)
  let releasedDrag = heldBefore && not dragging
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
    void (addWidget labelWid NodeText label 0 colorPickerLabelLayout)
    round
      <$> numericInputConfigured
        defaultNumericInputConfig {nicMin = 0, nicMax = fromIntegral hi, nicLayout = colorPickerChannelLayout}
        (fromIntegral value)

-- | Adopt the caller's colour as 'adoptSlot' does. A new
-- colour also resets the hue, S/V, and the "current" swatch.
adoptColorPickerValue :: Context -> WidgetId -> Color -> IO ()
adoptColorPickerValue ctx wid value = do
  store0 <- getStore ctx
  let
    key = intKey wid
    packed = fromIntegral (colorToWord32 value)
    seenKey = slotKey SlotSeen key
    seen = insertSlot fieldInt seenKey packed store0
  when (lookupSlot fieldInt seenKey store0 /= Just packed) $
    setStore ctx $
      if lookupSlot fieldInt key store0 == Just packed
        then seen
        else
          let (h, s, v) = rgbToHsv value
           in putColorState key value (clamp 0 360 h) (s, v) (insertSlot fieldInt (slotKey SlotColorBase key) packed seen)

commitColorPickerCurrent :: Context -> WidgetId -> Color -> IO ()
commitColorPickerCurrent ctx wid col =
  let packed = fromIntegral (colorToWord32 col)
   in writeSlots ctx (slotWriteOr fieldInt packed (slotKey SlotColorBase (intKey wid)) packed)

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
    h = widgetStoreHue store wid current
    (s, v) = widgetStoreSv store wid current
    a = fromIntegral (colorA current) :: Float
    bar lo hi cur
      | down KeyHome = lo
      | down KeyEnd = hi
      | otherwise = clamp lo hi (cur + (dx + dy) * step)
    (col', h', sv')
      | svFocus =
          let sat = clamp01 (s + dx * step / 100)
              val = clamp01 (v - dy * step / 100)
           in (withAlpha (colorA current) (hsvToRgb h sat val), h, (sat, val))
      | hueFocus =
          let hue = bar 0 360 h
           in (withAlpha (colorA current) (hsvToRgb hue s v), hue, (s, v))
      | otherwise = (withAlpha (round (bar 0 255 a)) current, h, (s, v))
    moved = col' /= current || h' /= h || sv' /= (s, v)
  when moved $
    setStore ctx (putColorState (intKey wid) col' h' sv' store)
  pure moved
