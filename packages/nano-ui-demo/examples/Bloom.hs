-- | Bloom: rose curves on a canvas, set by sliders and moved by springs.
--
-- The picture is a stack of rings. A ring is a closed curve whose radius
-- at angle @a@ follows the cosine of @petals * a@. The next ring out is
-- larger and turned by @twist@, and the colour runs from the chosen swatch
-- at the centre to its neighbour at the edge.
--
-- The sliders and "Shuffle" write plain 'Var's. The drawing does not read
-- those: every number goes through 'animateTo' with a 'Spring' first, so a
-- new value is a new target and the curve swings over to it and settles.
-- Petals and rings are whole numbers at rest and fractions on the way.
--
-- "Spin" adds to the angle on every frame and asks for the next one with
-- 'keepAnimating'. With it off, the loop sleeps once the springs land.
-- Dragging the canvas turns the flower by the angle the pointer moves
-- around its centre.
--
-- Run it with @cabal run nano-ui-example-bloom@. It is also the first tab
-- of the program on the project's landing page ("Landing").
module Bloom (bloomView, bloomViewWith) where

import Control.Monad (forM_, when)
import Data.Text qualified as T
import NanoUI
import NanoUI.Path qualified as P

-- | What one frame draws: the sliders' values after their springs.
data Bloom = Bloom
  { bloomPetals :: !Float
  , bloomRings :: !Float
  , bloomTwist :: !Float
  , bloomDepth :: !Float
  , bloomAngle :: !Float
  , bloomInner :: !Color
  , bloomOuter :: !Color
  }

bloomView :: NanoUI ()
bloomView = bloomViewWith True

-- | 'bloomView' with "Spin" starting ticked or not.
bloomViewWith :: Bool -> NanoUI ()
bloomViewWith spinning = do
  petals <- useVar 6
  rings <- useVar 5
  twist <- useVar 0.3
  depth <- useVar 0.55
  swatch <- useVar (0 :: Int)
  spin <- useVar spinning
  seed <- useVar (0 :: Int)
  angle <- useVar 0
  theme <- uiTheme
  wide <- (>= 640) <$> windowWidth
  let swatches = [themeAccent theme, themePurple theme, themeRed theme, themeOrange theme, themeGreen theme]
      colourAt i = swatches !! (i `mod` length swatches)

  -- The spin's step is written before anything reads the angle, so it
  -- costs no second pass of the view.
  dt <- inputDeltaTime <$> askInput
  whenM (readVar spin) (modifyVar angle (+ dt * 0.2))

  (if wide then rowWith else columnWith) (grow . padAll 16 . gap 16) $ do
    panelWith ((if wide then fixedW 250 . fillH else fillW) . padAll 16 . gap 10) $ do
      -- One column of sliders beside the canvas, two above it.
      gridWith (if wide then 1 else 2) (tight . gap 10 . fillW) $ do
        setting "Petals" whole 3 12 petals
        setting "Rings" whole 1 8 rings
        setting "Twist" degrees (-0.6) 0.6 twist
        setting "Depth" percentage 0.1 0.9 depth
      chosen <- readVar swatch
      rowWith (tight . wrap . gap 8 . lineGap 10 . alignMid . fillW . padTop 4) $ do
        forM_ (zip [0 ..] swatches) $ \(i, c) -> do
          let ring = if i == chosen then buttonStyle (borderWidth 3 . borderColor (styleFg (themePanel theme))) else id
          whenM (styled (ring . tinted (const c)) (buttonWith (fixedWH 28 28) "")) (writeVar swatch i)
        checkbox "Spin" spin
      when wide flex
      muted "Drag the flower to turn it."
      whenM (styled primary (buttonWith fillW "Shuffle")) $ do
        n <- (+ 1) <$> readVar seed
        writeVar seed n
        writeVar petals (nearest (3 + 9 * noise n 0))
        writeVar rings (nearest (2 + 6 * noise n 1))
        writeVar twist (1.2 * noise n 2 - 0.6)
        writeVar depth (0.2 + 0.7 * noise n 3)
        writeVar swatch (floor (noise n 4 * fromIntegral (length swatches)))

    -- Each spring's target is a slider's value. Petals and rings aim at
    -- whole numbers, which close the curve and finish the outer ring.
    bloom <- do
      p <- animateTo bouncy . nearest =<< readVar petals
      l <- animateTo bouncy . nearest =<< readVar rings
      t <- animateTo bouncy =<< readVar twist
      d <- animateTo bouncy =<< readVar depth
      a <- readVar angle
      i <- readVar swatch
      inner <- animateToA fade (colourAt i)
      outer <- animateToA fade (colourAt (i + 1))
      pure (Bloom p l t d a inner outer)

    resp <-
      canvasConfigured
        defaultCanvasConfig
          { canvasLayout = (grow . minH 200) defaultLayout
          , canvasContent = bloomKey bloom
          , canvasCursor = Just (\cdc _ _ -> if cdcActive cdc then UiCursorGrabbing else UiCursorGrab)
          }
        (drawBloom bloom)
    whenM (readVar spin) (keepAnimating resp)
    drag <- useDrag2DOn resp
    when (dragActive drag) (modifyVar angle (+ turned (respRect resp) drag))
  where
    bouncy = Spring presetBouncy
    fade = Tween EaseInOutCubic 0.4 0
    nearest = fromIntegral . (round :: Float -> Int)

-- | A slider under its name, with its value at the right.
setting :: T.Text -> (Float -> T.Text) -> Float -> Float -> Var Float -> NanoUI ()
setting name shown lo hi var = columnWith (tight . gap 2 . fillW) $ do
  rowWith (tight . fillW) $ do
    labelWith (tight . fontMuted) name
    labelWith (tight . fillW . alignEnd) . shown =<< readVar var
  sliderWith fillW lo hi var

whole, degrees, percentage :: Float -> T.Text
whole = T.pack . show . (round :: Float -> Int)
degrees x = whole (x * 180 / pi) <> " deg"
percentage x = whole (x * 100) <> "%"

-- | A value from 0 up to 1 for a seed and an index: the fraction of a
-- large multiple of a sine, which has no visible pattern from one seed to
-- the next.
noise :: Int -> Int -> Float
noise seed k = x - fromIntegral (floor x :: Int)
  where
    x = abs (sin (fromIntegral seed * 12.9898 + fromIntegral k * 78.233) * 43758.5453)

-- | The angle a drag's last move turned around the rectangle's centre,
-- between @-pi@ and @pi@.
turned :: Rect -> Drag2D -> Float
turned (Rect x y w h) drag = atan2 (sin delta) (cos delta)
  where
    centre = V2 (x + w / 2) (y + h / 2)
    bearing p = let V2 dx dy = v2Sub p centre in atan2 dy dx
    here = dragPosition drag
    delta = bearing here - bearing (v2Sub here (dragDelta drag))

-- | Everything 'drawBloom' reads, so a frame with the same values reuses
-- the last drawing.
bloomKey :: Bloom -> Int
bloomKey (Bloom p l t d a inner outer) =
  contentKeyOf [keyPart [p, l, t, d, a], keyPart (colorToWord32 inner, colorToWord32 outer)]

-- | The rings from the outside in, so the small ones are drawn on top.
-- The outermost ring may be a fraction: it fades in as it grows out of
-- the ring inside it.
drawBloom :: Bloom -> Rect -> CanvasM ()
drawBloom b (Rect x y w h) =
  forM_ (reverse [0 .. ceiling rings - 1 :: Int]) $ \i -> do
    let n = fromIntegral i
        presence = min 1 (rings - n)
        size = radius * (n + presence) / rings
        colour = lerpColor (bloomInner b) (bloomOuter b) (min 1 (n / max 1 (rings - 1)))
        outline = P.polygon (map (point size (bloomAngle b + bloomTwist b * n)) [0 .. samples - 1])
    drawPathWith P.NonZero outline (P.Solid (withAlpha colour (0.12 * presence)))
    drawStrokePathWith (P.stroke 1.5) {P.strokeJoin = P.RoundJoin} outline (P.Solid (withAlpha colour presence))
  where
    rings = max 1 (bloomRings b)
    depth = max 0 (min 1 (bloomDepth b))
    radius = min w h / 2 - 8
    centre = V2 (x + w / 2) (y + h / 2)
    samples = 240 :: Int
    -- A point of a ring: around the circle once, pulled toward the centre
    -- between petals.
    point size turn j =
      let a = 2 * pi * fromIntegral j / fromIntegral samples - pi
          r = size * (1 - depth * (0.5 - 0.5 * cos (bloomPetals b * a)))
       in v2Add centre (V2 (r * cos (a + turn)) (r * sin (a + turn)))
