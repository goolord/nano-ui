-- | Bloom: layered rose curves driven by sliders, spring physics, and a 2D canvas.
--
-- The flower is built from a stack of closed rings. Each ring's radius at
-- angle @a@ modulates with @cos (petals * a)@. Successive rings scale outward,
-- rotate by @twist@, and blend from the selected swatch color at the center
-- to the next palette color at the outer edge.
--
-- The sliders and Shuffle button write directly to plain 'Var's. Before
-- drawing, each parameter passes through 'animateTo' with a bouncy 'Spring'
-- so changes swing smoothly into place and settle. Petal and ring counts
-- target whole numbers while passing through fractional values mid-transition.
--
-- When Spin is checked, each frame advances the rotation angle and calls
-- 'keepAnimating' to schedule the next frame. With Spin unchecked, the event
-- loop sleeps once the springs settle. Dragging across the canvas rotates
-- the flower around its center.
--
-- Run standalone with @cabal run nano-ui-example-bloom@. This view also
-- powers the first tab of the landing-page demo ("Landing").
module Bloom (bloomView, bloomViewWith) where

import Control.Monad (forM_, when)
import Data.Text qualified as T
import NanoUI
import NanoUI.Path qualified as P

-- | Animated parameters passed to the canvas for a single frame.
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

-- | 'bloomView' with an explicit initial state for the Spin checkbox.
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
      colorAt i = swatches !! (i `mod` length swatches)

  -- Advance rotation before anything reads @angle@ so no second view pass
  -- is triggered.
  dt <- inputDeltaTime <$> askInput
  whenM (readVar spin) (modifyVar angle (+ dt * 0.2))

  (if wide then rowWith else columnWith) (grow . padAll 16 . gap 16) $ do
    panelWith ((if wide then fixedW 250 . fillH else fillW) . padAll 16 . gap 10) $ do
      -- Stack sliders in one column beside the canvas on wide screens, or
      -- two columns above it on narrow screens.
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
      muted "Drag the flower to rotate it."
      whenM (styled primary (buttonWith fillW "Shuffle")) $ do
        n <- (+ 1) <$> readVar seed
        writeVar seed n
        writeVar petals (nearest (3 + 9 * noise n 0))
        writeVar rings (nearest (2 + 6 * noise n 1))
        writeVar twist (1.2 * noise n 2 - 0.6)
        writeVar depth (0.2 + 0.7 * noise n 3)
        writeVar swatch (floor (noise n 4 * fromIntegral (length swatches)))

    -- Follow each slider target through a spring. Rounding petals and rings
    -- keeps closed curves and complete outer rings once settled.
    bloom <- do
      p <- animateTo bouncy . nearest =<< readVar petals
      l <- animateTo bouncy . nearest =<< readVar rings
      t <- animateTo bouncy =<< readVar twist
      d <- animateTo bouncy =<< readVar depth
      a <- readVar angle
      i <- readVar swatch
      inner <- animateToA fade (colorAt i)
      outer <- animateToA fade (colorAt (i + 1))
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

-- | A labeled slider with its formatted current value aligned to the right.
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

-- | Deterministic pseudo-random float in @[0, 1)@ for shuffling presets.
noise :: Int -> Int -> Float
noise seed k = x - fromIntegral (floor x :: Int)
  where
    x = abs (sin (fromIntegral seed * 12.9898 + fromIntegral k * 78.233) * 43758.5453)

-- | Angular delta of the last pointer drag step around the canvas center,
-- wrapped to @[-pi, pi]@.
turned :: Rect -> Drag2D -> Float
turned (Rect x y w h) drag = atan2 (sin delta) (cos delta)
  where
    centerPt = V2 (x + w / 2) (y + h / 2)
    bearing p = let V2 dx dy = v2Sub p centerPt in atan2 dy dx
    here = dragPosition drag
    delta = bearing here - bearing (v2Sub here (dragDelta drag))

-- | Content key covering every input read by 'drawBloom' so unchanged frames
-- reuse cached vector geometry.
bloomKey :: Bloom -> Int
bloomKey (Bloom p l t d a inner outer) =
  contentKeyOf [keyPart [p, l, t, d, a], keyPart (colorToWord32 inner, colorToWord32 outer)]

-- | Draw rings from outside in so inner petals layer cleanly on top. When
-- @bloomRings@ is fractional during a transition, the outermost ring fades
-- in as it grows past the ring beneath it.
drawBloom :: Bloom -> Rect -> CanvasM ()
drawBloom b (Rect x y w h) =
  forM_ (reverse [0 .. ceiling rings - 1 :: Int]) $ \i -> do
    let n = fromIntegral i
        presence = min 1 (rings - n)
        size = radius * (n + presence) / rings
        col = lerpColor (bloomInner b) (bloomOuter b) (min 1 (n / max 1 (rings - 1)))
        outline = P.polygon (map (point size (bloomAngle b + bloomTwist b * n)) [0 .. samples - 1])
    drawPathWith P.NonZero outline (P.Solid (withAlpha col (0.12 * presence)))
    drawStrokePathWith (P.stroke 1.5) {P.strokeJoin = P.RoundJoin} outline (P.Solid (withAlpha col presence))
  where
    rings = max 1 (bloomRings b)
    depth = max 0 (min 1 (bloomDepth b))
    radius = min w h / 2 - 8
    centerPt = V2 (x + w / 2) (y + h / 2)
    samples = 240 :: Int
    -- Sample one vertex around the rose curve, pulling inward between petals.
    point size turn j =
      let a = 2 * pi * fromIntegral j / fromIntegral samples - pi
          r = size * (1 - depth * (0.5 - 0.5 * cos (bloomPetals b * a)))
       in v2Add centerPt (V2 (r * cos (a + turn)) (r * sin (a + turn)))
