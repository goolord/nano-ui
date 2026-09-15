{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Widgets.ColorPicker
  ( ColorPickerGeom (..)
  , colorPickerDefaultColor
  , colorPickerMinWidth
  , colorPickerSvH
  , colorPickerExtraH
  , colorPickerAlphaFlag
  , colorPickerAlphaMode
  , widgetStoreColor
  , widgetStoreBaseColor
  , widgetStoreHue
  , widgetStoreSv
  , colorPickerGeom
  , colorPickerBarHitRect
  , drawColorPickerPanel
  , colorPicker
  , colorPickerRGBA
  )
where

import Control.Monad (forM_, void, when)
import Data.Bits ((.&.))
import Data.IORef (readIORef, writeIORef)
import Data.IntMap.Strict qualified as IM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Data.Word (Word8)
import Effectful (Eff, type (:>))
import NanoUI.Context
  ( Context (..)
  , WidgetStore (..)
  , getLastPointerBlocked
  , getMenuPointerGesture
  , getStore
  , intKey
  , registerFocusable
  , setStore
  )
import NanoUI.Draw
  ( DrawArena
  , pushQuadGradient
  , pushRect
  , pushRoundedRect
  , pushRoundedStroke
  )
import NanoUI.Font
  ( FontMetrics (..)
  , layoutLineHeight
  )
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Input (Input (..), inputMouseDown, inputMousePressed)
import NanoUI.Layout.Arena (NodeType (..))
import NanoUI.Monad (Ui, askContext, askInput, nextId, uiIO, withKey)
import NanoUI.Store (slotColorBase, slotKey)
import NanoUI.Style
  ( AlignY (..)
  , Direction (..)
  , Layout (..)
  , Padding (..)
  , Sizing (..)
  , Style (..)
  , defaultLayout
  , fillW
  )
import NanoUI.Types
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
import NanoUI.WidgetText
  ( colorPickerExtraH
  , colorPickerGap
  , colorPickerMinWidth
  , colorPickerParseHex
  , colorPickerSvH
  , colorPickerToHex
  , colorPickerToHexA
  , textInputFlagBare
  , intValueText
  )
import NanoUI.Widgets.Behavior
  ( DragAxis (..)
  , KeyNav (..)
  , keyedDragHeld
  , useDrag1D
  , useKeyNav
  )
import NanoUI.Widgets.Node
  ( Response (..)
  , addWidget
  , addWidgetStyled
  , container
  , respRect
  , setChanged
  )
import NanoUI.Widgets.TextInput (editTextField)

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

-- Marks a @NodeColorPicker@ canvas that also paints the alpha slider. Lives in
-- the high style bits so it survives the arena's int storage.
colorPickerAlphaFlag :: Int
colorPickerAlphaFlag = 0x02000000

{-# INLINE colorPickerAlphaMode #-}
colorPickerAlphaMode :: Int -> Bool
colorPickerAlphaMode si = si .&. colorPickerAlphaFlag /= 0

data ColorPickerGeom = ColorPickerGeom
  { cpgLabelH :: !Float
  , cpgSv :: !Rect
  , cpgHue :: !Rect
  , cpgAlpha :: !Rect
  , cpgShowAlpha :: !Bool
  , cpgCurrent :: !Rect
  , cpgNew :: !Rect
  , cpgPreviewX :: !Float
  , cpgCurrentLabelY :: !Float
  , cpgNewLabelY :: !Float
  }
  deriving (Eq, Show)

storeColorAt :: WidgetStore -> Int -> Color -> Color
storeColorAt store key fallback =
  colorFromWord32
    ( fromIntegral
        ( IM.findWithDefault
            (fromIntegral (colorToWord32 fallback))
            key
            (storeInt store)
        )
    )

widgetStoreColor :: WidgetStore -> WidgetId -> Color -> Color
widgetStoreColor store wid fallback = storeColorAt store (intKey wid) fallback

widgetStoreBaseColor :: WidgetStore -> WidgetId -> Color -> Color
widgetStoreBaseColor store wid fallback =
  storeColorAt
    store
    (slotKey slotColorBase (intKey wid))
    (widgetStoreColor store wid fallback)

-- RGB cannot tell hue 0 from 360. Keep the slider end the user last set.
widgetStoreHue :: WidgetStore -> WidgetId -> Color -> Float
widgetStoreHue store wid fallback =
  let
    (h0, _, _) = rgbToHsv (widgetStoreColor store wid fallback)
   in
    IM.findWithDefault h0 (intKey wid) (storeFloat store)

-- Black collapses S in RGB. Keep the last mouse S/V so the marker does not jitter.
widgetStoreSv :: WidgetStore -> WidgetId -> Color -> (Float, Float)
widgetStoreSv store wid fallback =
  let
    (_, s0, v0) = rgbToHsv (widgetStoreColor store wid fallback)
   in
    fromMaybe (s0, v0) (IM.lookup (intKey wid) (storePoint store))

-- | Store the live colour with the hue and S/V it was set through.
putColorState :: Int -> Color -> Float -> (Float, Float) -> WidgetStore -> WidgetStore
putColorState key col hue sv st =
  st
    { storeInt = IM.insert key (fromIntegral (colorToWord32 col)) (storeInt st)
    , storeFloat = IM.insert key hue (storeFloat st)
    , storePoint = IM.insert key sv (storePoint st)
    }

withAlpha :: Word8 -> Color -> Color
withAlpha a c = colorRGBA (colorR c) (colorG c) (colorB c) a

colorPickerGeom ::
  Bool
  -> FontMetrics
  -> Float
  -> Float
  -> Float
  -> Float
  -> ColorPickerGeom
colorPickerGeom showAlpha fm x y w h =
  let
    labelH = layoutLineHeight fm
    contentTop = y
    contentAvailH = h
    barW = colorPickerBarW
    barGap = colorPickerGap
    previewW = clamp 0 colorPickerPreviewW (w * 0.5)
    barsW = barW + if showAlpha then barGap + barW else 0
    fixedW = barGap + barsW + barGap + previewW
    svAvail = max 0 (w - fixedW)
    side = clamp 0 contentAvailH svAvail
    groupX = x + max 0 ((w - (side + fixedW)) / 2)
    hueX = groupX + side + barGap
    alphaX = hueX + barW + barGap
    previewX = groupX + side + barGap + barsW + barGap
    contentY = contentTop + (contentAvailH - side) / 2
    swatchW = min colorPickerSwatchW previewW
    swatchH = clamp 0 colorPickerSwatchH (side - labelH * 2 - colorPickerGap)
    stackH = labelH + swatchH + colorPickerGap + labelH + swatchH
    previewY = contentY + max 0 ((side - stackH) / 2)
    currentY = previewY + labelH
    newLabelY = currentY + swatchH + colorPickerGap
    newY = newLabelY + labelH
   in
    ColorPickerGeom
      { cpgLabelH = labelH
      , cpgSv = Rect groupX contentY side side
      , cpgHue = Rect hueX contentY barW side
      , cpgAlpha =
          Rect
            alphaX
            contentY
            (if showAlpha then barW else 0)
            (if showAlpha then side else 0)
      , cpgShowAlpha = showAlpha
      , cpgCurrent = Rect previewX currentY swatchW swatchH
      , cpgNew = Rect previewX newY swatchW swatchH
      , cpgPreviewX = previewX
      , cpgCurrentLabelY = previewY
      , cpgNewLabelY = newLabelY
      }


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

drawColorPickerPanel ::
  Bool
  -> FontMetrics
  -> DrawArena
  -> WidgetStore
  -> WidgetId
  -> Style
  -> Float
  -> Float
  -> Float
  -> Float
  -> IO ()
drawColorPickerPanel showAlpha fm da store wid style x y w h = do
  let
    geom = colorPickerGeom showAlpha fm x y w h
    hue = widgetStoreHue store wid colorPickerDefaultColor
    (sat, val) = widgetStoreSv store wid colorPickerDefaultColor
    newCol = widgetStoreColor store wid colorPickerDefaultColor
    a = colorA newCol
    currentCol = widgetStoreBaseColor store wid colorPickerDefaultColor
    sv = cpgSv geom
    hueRect = cpgHue geom
    alphaRect = cpgAlpha geom
    border = styleBorder style
    marker = 6
    mx = rectX sv + sat * rectW sv
    my = rectY sv + (1 - val) * rectH sv
    hueCy = rectY hueRect + (hue / 360) * rectH hueRect
    alphaCy = rectY alphaRect + (fromIntegral a / 255) * rectH alphaRect
  drawSvField da sv hue
  pushRoundedStroke da sv 4 1 border
  drawHueBar da hueRect
  pushRoundedStroke da hueRect 3 1 border
  when showAlpha $ do
    drawAlphaBar da alphaRect newCol
    pushRoundedStroke da alphaRect 3 1 border
  drawChecker da (cpgCurrent geom)
  pushRect da (cpgCurrent geom) currentCol
  pushRoundedStroke da (cpgCurrent geom) 0 1 border
  drawChecker da (cpgNew geom)
  pushRect da (cpgNew geom) newCol
  pushRoundedStroke da (cpgNew geom) 0 1 border
  pushRoundedRect
    da
    (Rect (mx - marker / 2) (my - marker / 2) marker marker)
    (marker / 2)
    (colorRGBA 255 255 255 255)
  pushRoundedStroke
    da
    (Rect (mx - marker / 2) (my - marker / 2) marker marker)
    (marker / 2)
    1
    (colorRGBA 0 0 0 180)
  drawBarHandle da hueRect hueCy (colorRGBA 0 0 0 180)
  when showAlpha $ drawBarHandle da alphaRect alphaCy (colorRGBA 0 0 0 180)

colorPickerLayout :: Layout
colorPickerLayout =
  defaultLayout
    { layoutDirection = Column
    , layoutWidth = Grow 1
    , layoutGap = colorPickerGap
    , layoutPadding = Padding 0 0 0 0
    }

-- A row of channel fields. Children are groups sized by 'percent' so the
-- R/G/B(/A) and H/S/V rows share the same column widths.
colorPickerRowLayout :: Layout
colorPickerRowLayout =
  colorPickerLayout {layoutDirection = Row, layoutAlignY = AlignMiddle}

-- One channel field: an inline label plus its bare box, taking @pct@ of the row.
colorPickerFieldGroupLayout :: Float -> Layout
colorPickerFieldGroupLayout pct =
  colorPickerLayout
    { layoutDirection = Row
    , layoutWidth = Percent pct
    , layoutAlignY = AlignMiddle
    }

colorPickerFieldLayout :: Layout
colorPickerFieldLayout =
  defaultLayout
    { layoutWidth = Grow 1
    , layoutMinW = 40
    , layoutPadding = Padding 0 0 0 0
    }

colorPickerLabelLayout :: Layout
colorPickerLabelLayout =
  defaultLayout {layoutPadding = Padding 0 0 0 0, layoutAlignY = AlignMiddle}

colorPicker :: Ui :> es => Color -> Eff es (Response, Color)
colorPicker = colorPickerWith False

-- | Alpha-aware picker: adds a vertical alpha bar and an A / @#RRGGBBAA@ field.
colorPickerRGBA :: Ui :> es => Color -> Eff es (Response, Color)
colorPickerRGBA = colorPickerWith True

-- | The byte fields: label, the channel read, and the channel write.
rgbChannels, rgbaChannels :: [(Text, Color -> Word8, Word8 -> Color -> Color)]
rgbChannels =
  [ ("R", colorR, \v c -> colorRGBA v (colorG c) (colorB c) (colorA c))
  , ("G", colorG, \v c -> colorRGBA (colorR c) v (colorB c) (colorA c))
  , ("B", colorB, \v c -> colorRGBA (colorR c) (colorG c) v (colorA c))
  ]
rgbaChannels = rgbChannels ++ [("A", colorA, \v c -> colorRGBA (colorR c) (colorG c) (colorB c) v)]

-- | The HSV fields: label, the shown value, and the (hue, s, v) a typed value makes.
hsvChannels :: [(Text, (Float, Float, Float) -> Int, Int -> (Float, Float, Float) -> (Float, Float, Float))]
hsvChannels =
  [ ("H", \(h, _, _) -> round h, \n (_, s, v) -> (clamp 0 360 (fromIntegral n), s, v))
  , ("S", \(_, s, _) -> round (s * 100), \n (h, _, v) -> (h, percent n, v))
  , ("V", \(_, _, v) -> round (v * 100), \n (h, s, _) -> (h, s, percent n))
  ]
  where
    percent n = clamp01 (fromIntegral (clamp 0 100 n) / 100)

colorPickerWith ::
  Ui :> es => Bool -> Color -> Eff es (Response, Color)
colorPickerWith showAlpha initial = do
  ctx <- askContext
  wid <- nextId
  uiIO $ registerFocusable ctx wid
  uiIO $ initColorPickerStore ctx wid initial
  let
    key = intKey wid
    pct = 100 / (if showAlpha then 4 else 3)
    readColor = (\st -> widgetStoreColor st wid initial) <$> uiIO (getStore ctx)
    writePicker col hue sv = uiIO (getStore ctx >>= setStore ctx . putColorState key col hue sv)
    writeColor col =
      let (h, s, v) = rgbToHsv col
       in writePicker col (clamp 0 360 h) (s, v)
    -- Without the alpha bar the colour stays opaque.
    alphaOf c = if showAlpha then colorA c else 255
  (start, final, cResp) <- container NodeContainer colorPickerLayout $ do
    cResp <-
      addWidgetStyled
        wid
        NodeColorPicker
        ""
        0
        (fillW defaultLayout)
        (if showAlpha then colorPickerAlphaFlag else 0)
        Nothing
    start <- colorPickerCanvas showAlpha wid initial cResp
    -- Only the focused field edits, so each row's fields share one store read.
    rgb <- readColor
    _ <- container NodeContainer colorPickerRowLayout $
      forM_ (if showAlpha then rgbaChannels else rgbChannels) $ \(lbl, get, set) -> do
        let shown = fromIntegral (get rgb)
        (txt, focused) <- colorField pct lbl (intValueText shown)
        -- A focused field is re-read every frame; only a different number edits.
        when focused $
          forM_ (readIntText txt) $ \n ->
            when (n >= 0 && n <= 255 && n /= shown) $
              writeColor (set (fromIntegral n) (withAlpha (alphaOf rgb) rgb))
    hsvStore <- uiIO (getStore ctx)
    let
      (s0, v0) = widgetStoreSv hsvStore wid initial
      hsv = (widgetStoreHue hsvStore wid initial, s0, v0)
      alpha = alphaOf (widgetStoreColor hsvStore wid initial)
    _ <- container NodeContainer colorPickerRowLayout $ do
      forM_ hsvChannels $ \(lbl, shown, edit) -> do
        (txt, focused) <- colorField pct lbl (intValueText (shown hsv))
        when focused $
          forM_ (readIntText txt) $ \n ->
            when (n /= shown hsv) $ do
              let (h, s, v) = edit n hsv
              writePicker (withAlpha alpha (hsvToRgb h s v)) h (s, v)
      when showAlpha $
        void (container NodeContainer (colorPickerFieldGroupLayout pct) (pure ()))
    hex <- readColor
    let hexText = if showAlpha then colorPickerToHexA hex else colorPickerToHex hex
    (thex, fhex) <- colorField 100 "" hexText
    when (fhex && thex /= hexText) $
      forM_ (colorPickerParseHex thex) $ \(r, g, b, ma) ->
        writeColor (colorRGBA r g b (if showAlpha then fromMaybe (colorA hex) ma else 255))
    final <- readColor
    pure (start, final, cResp)
  pure (setChanged (final /= start) cResp, final)

-- | The SV field and the hue / alpha bars: pointer drags, then arrow keys,
-- then committing the "current" swatch when a drag ends or a key moved the
-- colour. Returns the colour the frame started with.
colorPickerCanvas :: Ui :> es => Bool -> WidgetId -> Color -> Response -> Eff es Color
colorPickerCanvas showAlpha wid initial cResp = do
  ctx <- askContext
  inp <- askInput
  active <- uiIO (readIORef (ctxActiveId ctx))
  blocked <- uiIO (getLastPointerBlocked ctx)
  gesture <- uiIO (getMenuPointerGesture ctx)
  store0 <- uiIO (getStore ctx)
  hueHeld0 <- keyedDragHeld ("hue" :: Text)
  alphaHeld0 <- keyedDragHeld ("alpha" :: Text)
  sHeld0 <- keyedDragHeld ("s" :: Text)
  vHeld0 <- keyedDragHeld ("v" :: Text)
  let
    current0 = widgetStoreColor store0 wid initial
    h0 = widgetStoreHue store0 wid initial
    (s0, v0) = widgetStoreSv store0 wid initial
    svHeld0 = sHeld0 || vHeld0
    Rect cx cy cw ch = respRect cResp
    geom = colorPickerGeom showAlpha (ctxFontMetrics ctx) cx cy cw ch
    empty = Rect 0 0 0 0
    isActive = active == wid
    heldByOther =
      inputMouseDown inp
        && not (inputMousePressed inp)
        && hashWidgetId active /= 0
        && not isActive
    locked = blocked || heldByOther || gesture
    svRect = if locked || hueHeld0 || alphaHeld0 then empty else cpgSv geom
    hueRect = if locked || svHeld0 || alphaHeld0 then empty else colorPickerBarHitRect (cpgHue geom)
    alphaRect =
      if showAlpha && not (locked || svHeld0 || hueHeld0)
        then colorPickerBarHitRect (cpgAlpha geom)
        else empty
  (sDrag, sA) <- withKey ("s" :: Text) (useDrag1D DragAxisX 0 1 s0 svRect)
  (vDrag, vA) <- withKey ("v" :: Text) (useDrag1D DragAxisY 1 0 v0 svRect)
  let svA = sA || vA
  (hDrag, hA) <-
    withKey ("hue" :: Text) (useDrag1D DragAxisY 0 360 h0 (if svA then empty else hueRect))
  (aDrag, aA) <-
    withKey
      ("alpha" :: Text)
      (useDrag1D DragAxisY 0 255 (fromIntegral (colorA current0)) (if svA || hA then empty else alphaRect))
  let
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
  when (dragging && not isActive) $ uiIO $ writeIORef (ctxActiveId ctx) wid
  when ((not dragging || blocked) && isActive) $
    uiIO $ writeIORef (ctxActiveId ctx) (WidgetId 0)
  when (dragging && (dragged /= current0 || nextHue /= h0 || nextS /= s0 || nextV /= v0)) $
    uiIO $ getStore ctx >>= setStore ctx . putColorState (intKey wid) dragged nextHue (nextS, nextV)
  nav <- useKeyNav wid
  let
    keyMoved = knLeft nav || knRight nav || knUp nav || knDown nav
    releasedDrag = (hueHeld0 || alphaHeld0 || svHeld0) && not dragging
  when keyMoved $ uiIO $ applyColorPickerKeys ctx wid initial nav
  when (releasedDrag || keyMoved) $
    uiIO $ do
      st <- getStore ctx
      commitColorPickerCurrent ctx wid (widgetStoreColor st wid initial)
  pure current0

-- | One channel field: an optional inline label and a bare text box that
-- shows @expected@ while unfocused. Returns its text and whether it is focused.
colorField :: Ui :> es => Float -> Text -> Text -> Eff es (Text, Bool)
colorField pct label expected = do
  wid <- nextId
  (_, shown, isFocus) <- editTextField wid False expected (Just expected)
  _ <-
    container NodeContainer (colorPickerFieldGroupLayout pct) $ do
      when (not (T.null label)) $ do
        labelWid <- nextId
        void (addWidget labelWid NodeText label 0 colorPickerLabelLayout)
      addWidgetStyled
        wid
        NodeTextInput
        ""
        0
        colorPickerFieldLayout
        textInputFlagBare
        Nothing
  pure (shown, isFocus)

initColorPickerStore :: Context -> WidgetId -> Color -> IO ()
initColorPickerStore ctx wid initial = do
  store0 <- getStore ctx
  let
    key = intKey wid
  when (not (IM.member key (storeInt store0))) $
    let
      (hInit, sInit, vInit) = rgbToHsv initial
      packed = fromIntegral (colorToWord32 initial)
     in
      setStore
        ctx
        ( putColorState key initial (clamp 0 360 hInit) (sInit, vInit) $
            store0 {storeInt = IM.insert (slotKey slotColorBase key) packed (storeInt store0)}
        )

commitColorPickerCurrent :: Context -> WidgetId -> Color -> IO ()
commitColorPickerCurrent ctx wid col = do
  st <- getStore ctx
  let
    packed = fromIntegral (colorToWord32 col)
    k = slotKey slotColorBase (intKey wid)
    old = IM.findWithDefault packed k (storeInt st)
  when (old /= packed) $
    setStore ctx (st {storeInt = IM.insert k packed (storeInt st)})

applyColorPickerKeys :: Context -> WidgetId -> Color -> KeyNav -> IO ()
applyColorPickerKeys ctx wid fallback nav = do
  store <- getStore ctx
  let
    current = widgetStoreColor store wid fallback
    h = widgetStoreHue store wid current
    (s, v) = widgetStoreSv store wid current
    stepHue = if knLeft nav then -6 else if knRight nav then 6 else 0
    stepVal = if knUp nav then 0.05 else if knDown nav then -0.05 else 0
    nextHue = clamp 0 360 (h + stepHue)
    nextV = clamp01 (v + stepVal)
    next = withAlpha (colorA current) (hsvToRgb nextHue s nextV)
  when (next /= current || nextHue /= h || nextV /= v) $
    setStore ctx (putColorState (intKey wid) next nextHue (s, nextV) store)

readIntText :: Text -> Maybe Int
readIntText t =
  case TR.signed TR.decimal (T.strip t) of
    Right (n, rest) | T.null rest -> Just n
    _ -> Nothing
