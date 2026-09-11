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
  , clampHue
  , colorPickerGeom
  , colorPickerMeasureSize
  , svFromMouse
  , hueFromMouse
  , alphaFromMouse
  , colorPickerBarHitRect
  , drawColorPickerPanel
  , colorPicker
  , colorPickerRGBA
  )
where

import Control.Monad (forM_, unless, void, when)
import Data.Bits ((.&.))
import Data.IORef (readIORef, writeIORef)
import Data.IntMap.Strict qualified as IM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64, Word8)
import Effectful (Eff, type (:>))
import NanoUI.Context
  ( Context (..)
  , WidgetStore (..)
  , getFocusId
  , getLastPointerBlocked
  , getStore
  , intKey
  , menuPointerGestureActive
  , pointerBlockedByModal
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
import NanoUI.Store (slotAnchor, slotCursor, slotKey)
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
  , HostProfile
  , Rect (..)
  , V2 (..)
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
  , isCellHost
  , rectH
  , rectW
  , rectX
  , rectY
  , rgbToHsv
  )
import NanoUI.WidgetText
  ( colorPickerLabelText
  , colorPickerParseHex
  , colorPickerToHex
  , colorPickerToHexA
  , textInputFlagBare
  )
import NanoUI.Widgets.Behavior
  ( DragAxis (..)
  , KeyNav (..)
  , keyedDragHeld
  , useDrag1D
  , useKeyNav
  )
import NanoUI.Widgets.Node
  ( Responding (..)
  , Response (..)
  , addWidget
  , addWidgetStyled
  , container
  , setChanged
  )
import NanoUI.Widgets.TextInput (TextInputState (..), processTextInput)

colorPickerDefaultColor :: Color
colorPickerDefaultColor = colorRGBA 128 128 128 255

colorPickerMinWidth :: Float
colorPickerMinWidth = 240

colorPickerGap :: Float
colorPickerGap = 4

-- Side of the square SV field; the canvas reserves the label above it.
colorPickerSvH :: Float
colorPickerSvH = 250

colorPickerBarW :: Float
colorPickerBarW = 14

colorPickerSwatchH :: Float
colorPickerSwatchH = 30

colorPickerSwatchW :: Float
colorPickerSwatchW = 80

-- Width reserved for the Current / New preview column (label plus swatch).
colorPickerPreviewW :: Float
colorPickerPreviewW = 112

-- Live colour in storeInt at the widget key. Opening colour in the base slot.
slotColorBase :: Word64
slotColorBase = 0x4355524300000001

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

clampHue :: Float -> Float
clampHue h = max 0 (min 360 h)

withAlpha :: Word8 -> Color -> Color
withAlpha a c = colorRGBA (colorR c) (colorG c) (colorB c) a

-- extraH below the title: SV field + gap. The solver adds the measured label.
colorPickerExtraH :: Float -> Float
colorPickerExtraH _ = colorPickerSvH + colorPickerGap

colorPickerGeom ::
  Bool
  -> HostProfile
  -> FontMetrics
  -> Float
  -> Float
  -> Float
  -> Float
  -> ColorPickerGeom
colorPickerGeom showAlpha host fm x y w h =
  let
    labelH = layoutLineHeight host fm
    contentTop = y + labelH + colorPickerGap
    contentAvailH = max 0 (h - labelH - colorPickerGap)
    barW = colorPickerBarW
    barGap = colorPickerGap
    previewW = min colorPickerPreviewW (max 0 (w * 0.5))
    barsW = barW + if showAlpha then barGap + barW else 0
    fixedW = barGap + barsW + barGap + previewW
    svAvail = max 0 (w - fixedW)
    side = max 0 (min contentAvailH svAvail)
    groupX = x + max 0 ((w - (side + fixedW)) / 2)
    hueX = groupX + side + barGap
    alphaX = hueX + barW + barGap
    previewX = groupX + side + barGap + barsW + barGap
    contentY = contentTop + (contentAvailH - side) / 2
    swatchW = min colorPickerSwatchW previewW
    swatchH = min colorPickerSwatchH (max 0 (side - labelH * 2 - colorPickerGap))
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

colorPickerMeasureSize ::
  HostProfile
  -> FontMetrics
  -> (Text -> IO (Float, Float))
  -> Text
  -> IO (Float, Float, Float)
colorPickerMeasureSize host fm measure lbl =
  if isCellHost host
    then do
      (mw, mh) <- measure (lbl <> ": #000000")
      pure (mw, mh, 0)
    else do
      (lw, lh) <- measure (if T.null lbl then " " else lbl)
      let
        contentW = max colorPickerMinWidth lw
      pure (contentW, lh, colorPickerExtraH (layoutLineHeight host fm))

-- Clamp each axis on its own so a corner is S/V 0 or 1, not a frozen mid value.
svFromMouse :: Rect -> V2 -> (Float, Float)
svFromMouse rect (V2 mx my) =
  ( clamp01 ((mx - rectX rect) / max (rectW rect) 1)
  , clamp01 (1 - (my - rectY rect) / max (rectH rect) 1)
  )

-- Hue runs down a vertical bar: 0 at the top, 360 at the bottom.
hueFromMouse :: Rect -> V2 -> Float
hueFromMouse rect (V2 _ my) =
  clamp01 ((my - rectY rect) / max (rectH rect) 1) * 360

-- Alpha runs down the bar: transparent at the top, opaque at the bottom.
alphaFromMouse :: Rect -> V2 -> Float
alphaFromMouse rect (V2 _ my) =
  clamp01 ((my - rectY rect) / max (rectH rect) 1) * 255

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
drawChecker da (Rect x y w h) =
  let
    s = 6 :: Float
    cols = ceiling (max 0 w / s) :: Int
    rows = ceiling (max 0 h / s) :: Int
   in
    forM_ [0 .. rows - 1] $ \ry ->
      forM_ [0 .. cols - 1] $ \cx -> do
        let
          col =
            if even (ry + cx) then colorRGBA 190 190 190 255 else colorRGBA 140 140 140 255
          rx = x + fromIntegral cx * s
          ry' = y + fromIntegral ry * s
          cw = min s (max 0 (x + w - rx))
          ch = min s (max 0 (y + h - ry'))
        pushRect da (Rect rx ry' cw ch) col

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
  -> HostProfile
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
drawColorPickerPanel showAlpha host fm da store wid style x y w h = do
  unless (isCellHost host) $ do
    let
      geom = colorPickerGeom showAlpha host fm x y w h
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

colorPicker :: Ui :> es => Text -> Color -> Eff es (Response, Color)
colorPicker = colorPickerWith False

-- | Alpha-aware picker: adds a vertical alpha bar and an A / @#RRGGBBAA@ field.
colorPickerRGBA :: Ui :> es => Text -> Color -> Eff es (Response, Color)
colorPickerRGBA = colorPickerWith True

colorPickerWith ::
  Ui :> es => Bool -> Text -> Color -> Eff es (Response, Color)
colorPickerWith showAlpha lbl initial = do
  ctx <- askContext
  if isCellHost (ctxHostProfile ctx)
    then colorPickerCell lbl initial
    else colorPickerRich showAlpha lbl initial

-- Terminal hosts keep the single-line @label: #RRGGBB@ representation.
colorPickerCell :: Ui :> es => Text -> Color -> Eff es (Response, Color)
colorPickerCell lbl initial = do
  ctx <- askContext
  wid <- nextId
  uiIO $ registerFocusable ctx wid
  uiIO $ initColorPickerStore ctx wid initial
  resp <-
    addWidget wid NodeColorPicker (colorPickerLabelText lbl) 0 (fillW defaultLayout)
  store <- uiIO (getStore ctx)
  let
    final = widgetStoreColor store wid initial
  pure (setChanged (final /= initial) resp, final)

colorPickerRich ::
  Ui :> es => Bool -> Text -> Color -> Eff es (Response, Color)
colorPickerRich showAlpha lbl initial = do
  ctx <- askContext
  inp <- askInput
  wid <- nextId
  uiIO $ registerFocusable ctx wid
  uiIO $ initColorPickerStore ctx wid initial
  let
    host = ctxHostProfile ctx
    fm = ctxFontMetrics ctx
    key = intKey wid
    readColor = do
      st <- uiIO (getStore ctx)
      pure (widgetStoreColor st wid initial)
    readHue = do
      st <- uiIO (getStore ctx)
      pure (widgetStoreHue st wid initial)
    readSv = do
      st <- uiIO (getStore ctx)
      pure (widgetStoreSv st wid initial)
    readAlpha = if showAlpha then colorA <$> readColor else pure 255
    keepAlpha c = if showAlpha then colorA c else 255
    writeColorState col hue sv = do
      st <- uiIO (getStore ctx)
      let
        packed = fromIntegral (colorToWord32 col)
      uiIO $
        setStore
          ctx
          ( st
              { storeInt = IM.insert key packed (storeInt st)
              , storeFloat = IM.insert key hue (storeFloat st)
              , storePoint = IM.insert key sv (storePoint st)
              }
          )
    writeColor col =
      let
        (h, s, v) = rgbToHsv col
       in
        writeColorState col (clampHue h) (s, v)
    applyByteField setter txt =
      case readIntText txt of
        Just n | n >= 0 && n <= 255 -> do
          c <- readColor
          writeColor (setter (fromIntegral n) c)
        _ -> pure ()
    applyHueField txt =
      case readIntText txt of
        Just n -> do
          (s, v) <- readSv
          a <- readAlpha
          let
            h = clampHue (fromIntegral n)
          writeColorState (withAlpha a (hsvToRgb h s v)) h (s, v)
        _ -> pure ()
    applySField txt =
      case readIntText txt of
        Just n -> do
          (_, v) <- readSv
          h <- readHue
          a <- readAlpha
          let
            s = clamp01 (fromIntegral (clamp 0 100 n) / 100)
          writeColorState (withAlpha a (hsvToRgb h s v)) h (s, v)
        _ -> pure ()
    applyVField txt =
      case readIntText txt of
        Just n -> do
          (s, _) <- readSv
          h <- readHue
          a <- readAlpha
          let
            v = clamp01 (fromIntegral (clamp 0 100 n) / 100)
          writeColorState (withAlpha a (hsvToRgb h s v)) h (s, v)
        _ -> pure ()
    applyHexField txt =
      case colorPickerParseHex txt of
        Just (r, g, b, ma) -> do
          c <- readColor
          let
            a = if showAlpha then fromMaybe (colorA c) ma else 255
          writeColor (colorRGBA r g b a)
        Nothing -> pure ()
  (final, cResp) <- container NodeContainer colorPickerLayout $ do
    cResp <-
      addWidgetStyled
        wid
        NodeColorPicker
        (colorPickerLabelText lbl)
        0
        (fillW defaultLayout)
        (if showAlpha then colorPickerAlphaFlag else 0)
        Nothing
    active <- uiIO (readIORef (ctxActiveId ctx))
    blocked <- uiIO (getLastPointerBlocked ctx)
    gesture <- uiIO (menuPointerGestureActive ctx)
    store0 <- uiIO (getStore ctx)
    hueHeld0 <- keyedDragHeld ("hue" :: Text)
    alphaHeld0 <- keyedDragHeld ("alpha" :: Text)
    svHeld0 <- do
      s <- keyedDragHeld ("s" :: Text)
      v <- keyedDragHeld ("v" :: Text)
      pure (s || v)
    let
      Rect cx cy cw ch = respRect cResp
      geom = colorPickerGeom showAlpha host fm cx cy cw ch
      empty = Rect 0 0 0 0
      isActive = active == wid
      heldByOther =
        inputMouseDown inp
          && not (inputMousePressed inp)
          && hashWidgetId active /= 0
          && not isActive
      svRect =
        if blocked || heldByOther || hueHeld0 || alphaHeld0 || gesture
          then empty
          else cpgSv geom
      hueRect =
        if blocked || heldByOther || svHeld0 || alphaHeld0 || gesture
          then empty
          else colorPickerBarHitRect (cpgHue geom)
      alphaRect =
        if showAlpha && not (blocked || heldByOther || svHeld0 || hueHeld0 || gesture)
          then colorPickerBarHitRect (cpgAlpha geom)
          else empty
      current0 = widgetStoreColor store0 wid initial
      h0 = widgetStoreHue store0 wid initial
      (s0, v0) = widgetStoreSv store0 wid initial
    (sDrag, sA) <- withKey ("s" :: Text) (useDrag1D DragAxisX 0 1 s0 svRect)
    (vDrag, vA) <- withKey ("v" :: Text) (useDrag1D DragAxisY 1 0 v0 svRect)
    let
      svA = sA || vA
    (hDrag, hA) <-
      withKey
        ("hue" :: Text)
        (useDrag1D DragAxisY 0 360 h0 (if svA then empty else hueRect))
    (aDrag, aA) <-
      withKey
        ("alpha" :: Text)
        ( useDrag1D
            DragAxisY
            0
            255
            (fromIntegral (colorA current0))
            (if svA || hA then empty else alphaRect)
        )
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
    when ((not dragging || blocked) && isActive)
      $ uiIO
      $ writeIORef (ctxActiveId ctx) (WidgetId 0)
    when
      (dragging && (dragged /= current0 || nextHue /= h0 || nextS /= s0 || nextV /= v0))
      $ writeColorState dragged nextHue (nextS, nextV)
    nav <- useKeyNav wid
    let
      keyMoved = knLeft nav || knRight nav || knUp nav || knDown nav
    when keyMoved $ do
      stKeys <- uiIO (getStore ctx)
      uiIO $ applyColorPickerKeys ctx wid (widgetStoreColor stKeys wid initial) nav
    store1 <- uiIO (getStore ctx)
    let
      releasedDrag = (hueHeld0 || alphaHeld0 || svHeld0) && not dragging
      afterKeys = widgetStoreColor store1 wid initial
    when (releasedDrag || keyMoved)
      $ uiIO
      $ commitColorPickerCurrent ctx wid afterKeys
    let
      cols = if showAlpha then 4 else 3 :: Int
      pct = 100 / fromIntegral cols :: Float
    _ <- container NodeContainer colorPickerRowLayout $ do
      c1 <- readColor
      (_, t1, f1) <- buildColorField pct "R" (showByte (colorR c1))
      when f1 $
        applyByteField (\v c -> colorRGBA v (colorG c) (colorB c) (keepAlpha c)) t1
      c2 <- readColor
      (_, t2, f2) <- buildColorField pct "G" (showByte (colorG c2))
      when f2 $
        applyByteField (\v c -> colorRGBA (colorR c) v (colorB c) (keepAlpha c)) t2
      c3 <- readColor
      (_, t3, f3) <- buildColorField pct "B" (showByte (colorB c3))
      when f3 $
        applyByteField (\v c -> colorRGBA (colorR c) (colorG c) v (keepAlpha c)) t3
      when showAlpha $ do
        c4 <- readColor
        (_, t4, f4) <- buildColorField pct "A" (showByte (colorA c4))
        when f4 $
          applyByteField (\v c -> colorRGBA (colorR c) (colorG c) (colorB c) v) t4
      pure ()
    _ <- container NodeContainer colorPickerRowLayout $ do
      hueCur <- readHue
      (_, th, fh) <- buildColorField pct "H" (showInt (round hueCur))
      when fh $ applyHueField th
      (sCur, vCur) <- readSv
      (_, ts, fs) <- buildColorField pct "S" (showInt (round (sCur * 100)))
      when fs $ applySField ts
      (_, tv, fv) <- buildColorField pct "V" (showInt (round (vCur * 100)))
      when fv $ applyVField tv
      when showAlpha $
        void (container NodeContainer (colorPickerFieldGroupLayout pct) (pure ()))
      pure ()
    cHex <- readColor
    (_, thex, fhex) <-
      buildColorField
        100
        ""
        (if showAlpha then colorPickerToHexA cHex else colorPickerToHex cHex)
    when fhex $ applyHexField thex
    final <- readColor
    pure (final, cResp)
  pure (setChanged (final /= initial) cResp, final)

buildColorField ::
  Ui :> es => Float -> Text -> Text -> Eff es (Response, Text, Bool)
buildColorField pct label expected = do
  wid <- nextId
  ctx <- askContext
  uiIO $ registerFocusable ctx wid
  inp <- askInput
  store <- uiIO (getStore ctx)
  let
    key = intKey wid
  stored0 <- case IM.lookup key (storeText store) of
    Nothing -> do
      uiIO $
        setStore ctx (store {storeText = IM.insert key expected (storeText store)})
      pure expected
    Just t -> pure t
  let
    cursor =
      fromMaybe
        (T.length stored0)
        (IM.lookup (slotKey slotCursor key) (storeInt store))
    anchor = fromMaybe cursor (IM.lookup (slotKey slotAnchor key) (storeInt store))
  focus <- uiIO (getFocusId ctx)
  blocked <- uiIO (pointerBlockedByModal ctx)
  let
    isFocus = focus == wid && not blocked
  newState <-
    if isFocus
      then uiIO (processTextInput ctx inp (TextInputState stored0 cursor anchor))
      else pure (TextInputState stored0 cursor anchor)
  let
    newText = tisText newState
    newCursor = tisCursor newState
    newAnchor = tisAnchor newState
    shown = if isFocus then newText else expected
  when (shown /= stored0 || newCursor /= cursor || newAnchor /= anchor)
    $ uiIO
    $ setStore
      ctx
      ( store
          { storeText = IM.insert key shown (storeText store)
          , storeInt =
              IM.insert (slotKey slotCursor key) newCursor $
                IM.insert (slotKey slotAnchor key) newAnchor (storeInt store)
          }
      )
  resp <-
    container NodeContainer (colorPickerFieldGroupLayout pct) $ do
      when (not (T.null label)) $ do
        labelWid <- nextId
        _ <- addWidget labelWid NodeText label 0 colorPickerLabelLayout
        pure ()
      addWidgetStyled
        wid
        NodeTextInput
        ""
        0
        colorPickerFieldLayout
        textInputFlagBare
        Nothing
  pure (resp, shown, isFocus)

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
        ( store0
            { storeInt =
                IM.insert (slotKey slotColorBase key) packed $
                  IM.insert key packed (storeInt store0)
            , storeFloat = IM.insert key (clampHue hInit) (storeFloat store0)
            , storePoint = IM.insert key (sInit, vInit) (storePoint store0)
            }
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
applyColorPickerKeys ctx wid current nav = do
  store <- getStore ctx
  let
    h = widgetStoreHue store wid current
    (s, v) = widgetStoreSv store wid current
    a = colorA current
    stepHue = if knLeft nav then -6 else if knRight nav then 6 else 0
    stepVal = if knUp nav then 0.05 else if knDown nav then -0.05 else 0
    nextHue = clampHue (if stepHue /= 0 then h + stepHue else h)
    nextV = clamp01 (v + stepVal)
    next = withAlpha a (hsvToRgb nextHue s nextV)
  when (next /= current || nextHue /= h || nextV /= v) $
    setStore
      ctx
      ( store
          { storeInt =
              IM.insert (intKey wid) (fromIntegral (colorToWord32 next)) (storeInt store)
          , storeFloat = IM.insert (intKey wid) nextHue (storeFloat store)
          , storePoint = IM.insert (intKey wid) (s, nextV) (storePoint store)
          }
      )

showByte :: Word8 -> Text
showByte n = T.pack (show (fromIntegral n :: Int))

showInt :: Int -> Text
showInt = T.pack . show

readIntText :: Text -> Maybe Int
readIntText t =
  case reads (T.unpack (T.strip t)) of
    [(n, rest)] | all (== ' ') rest -> Just n
    _ -> Nothing
