{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Shaders: interactive gallery of Tier 1 surface shaders and Tier 2 subtree
-- effects written in the pure-Haskell @nano-ui-shader@ EDSL.
--
-- Surface shaders ('shaderBox', 'shaderConfigured') fill a single layout leaf
-- with zero offscreen render targets. Subtree effects ('withShader') wrap any
-- interactive widget tree, fusing compatible effects ('composeColor',
-- 'distortThenColor') into one GPU pass and running multi-pass chains such as
-- separable 'gaussianBlur' through pooled offscreen targets.
--
-- When animation is unchecked the window stays completely idle and uses no CPU
-- or GPU time.
--
-- Run it with @cabal run nano-ui-example-shaders@.
module Main (main) where

import Data.Text qualified as T
import GHC.Generics (Generic)
import NanoUI
import NanoUI.Backend.Sdl (SdlOptions (..), defaultSdlOptions, runSdlApp)
import NanoUI.Shader

data WaveGlowParams = WaveGlowParams
  { wgColorA :: !Color
  , wgColorB :: !Color
  , wgPhase :: !Float
  }
  deriving (Eq, Generic, Uniforms)

-- | Custom leaf surface shader built with OverloadedRecordDot field and
-- vector swizzle syntax. Compiled once at top level; changing uniform values
-- never recompiles the GPU program.
waveGlow :: Shader 'Surface WaveGlowParams
waveGlow = surfaceShader $ \u ctx ->
  let p = ctx.uv *^ 2.0 - vec2 1.0 1.0
      r = vLength p
      band = 0.5 + 0.5 * sin (r * 10.0 - u.wgPhase + p.x * 3.0)
      col = mix u.wgColorA u.wgColorB band
      half = ctx.size *^ 0.5
      d = sdRoundedBox (ctx.pos - half) half ctx.cornerRadius
      aa = 0.75 / maxE ctx.scale 1.0
      mask = 1.0 - smoothstep (negate aa) aa d
   in vec4_31 col.rgb (col.a * mask)

-- | Fused single-pass ColorEffect: desaturate then tint without an extra
-- offscreen target.
grayAndTint :: Shader 'ColorEffect (Float, TintParams)
grayAndTint = composeColor grayscale tint

-- | Fused single-pass distortion + color pass.
waveVignette :: Shader 'LayerEffect (WaveParams, VignetteParams)
waveVignette = distortThenColor waveDistort vignette

data EffectMode
  = ModeNone
  | ModeFusedColor
  | ModeBlur
  | ModeWaveVignette
  | ModeShadow
  | ModeOpacity
  deriving (Eq, Ord, Enum, Bounded, Show)

modeLabel :: EffectMode -> T.Text
modeLabel = \case
  ModeNone -> "None"
  ModeFusedColor -> "Grayscale + Tint (1 pass)"
  ModeBlur -> "Gaussian Blur (2 pass)"
  ModeWaveVignette -> "Wave + Vignette (1 pass)"
  ModeShadow -> "Drop Shadow"
  ModeOpacity -> "Group Opacity"

main :: IO ()
main =
  runSdlApp
    defaultSdlOptions
      { sdlAppShouldQuit = pressedOnceIn KeyEscape
      , sdlWindowSettings = defaultWindowSettings {wsTitle = "Shaders", wsSize = Size 920 620}
      }
    view

view :: NanoUI ()
view = do
  animateOn <- useVar False
  modeVar <- useEnumVar ModeFusedColor
  grayAmt <- useVar (0.75 :: Float)
  tintAmt <- useVar (0.35 :: Float)
  blurSigma <- useVar (4.0 :: Float)
  waveAmp <- useVar (6.0 :: Float)
  vigAmt <- useVar (0.45 :: Float)
  shadowRad <- useVar (8.0 :: Float)
  opacityVal <- useVar (0.55 :: Float)
  clicks <- useVar (0 :: Int)

  caps <- askShaderCapabilities
  anim <- readVar animateOn
  now <- uiTime
  let phase = if anim then realToFrac now * 2.5 else 0.0

  columnWith (padAll 16 . gap 12 . grow) $ do
    rowWith (tight . gap 12 . alignMiddle . fillW) $ do
      heading "Typed Fragment Shaders"
      flex
      checkbox "Animate" animateOn
      let capText =
            "Surface: "
              <> (if capSurfaceShaders caps then "GPU" else "Fallback")
              <> "  |  Effects: "
              <> (if capNodeEffects caps then "GPU" else "Fallback")
      muted capText

    rowWith (tight . gap 16 . grow) $ do
      -- Left column: Tier 1 leaf surface shaders.
      panelWith (gap 12 . padAll 14 . fillH . fixedW 340) $ do
        labelWith fontBold "Tier 1: Leaf Surface Shaders"
        muted "Single draw call, zero offscreen targets."

        let mgParams =
              MeshGradientParams
                { mgTopLeft = colorRGB 40 90 220
                , mgTopRight = colorRGB 220 60 150
                , mgBottomLeft = colorRGB 20 180 160
                , mgBottomRight = colorRGB 245 190 50
                }
            mgCfg =
              defaultShaderConfig
                { scLayout = fixedH 110 . fillW
                , scCornerRadius = 12
                , scFallback = colorRGB 50 90 180
                }
        shaderConfigured mgCfg meshGradient4 mgParams

        let glowCfg =
              defaultShaderConfig
                { scLayout = fixedH 110 . fillW
                , scCornerRadius = 12
                , scFallback = colorRGB 80 50 140
                }
            glowParams =
              WaveGlowParams
                { wgColorA = colorRGB 30 40 90
                , wgColorB = colorRGB 80 220 200
                , wgPhase = phase
                }
        glowResp <- shaderConfigured' glowCfg waveGlow glowParams
        if anim then keepAnimating glowResp else pure ()

        let sdfCfg =
              defaultShaderConfig
                { scLayout = fixedH 96 . fillW
                , scFallback = colorRGB 40 120 160
                }
            sdfParams =
              SdfBoxParams
                { sbFill = colorRGB 28 42 62
                , sbBorder = colorRGB 90 200 250
                , sbBorderWidth = 3
                , sbRadius = 18
                }
        shaderConfigured sdfCfg sdfRoundedBox sdfParams

      -- Right column: Tier 2 subtree effects over interactive widgets.
      panelWith (gap 12 . padAll 14 . grow) $ do
        labelWith fontBold "Tier 2: Subtree Effects"
        muted "Wraps any interactive subtree. Controls inside stay clickable."

        enumRadioBy modeLabel modeVar
        mode <- readVar modeVar

        scope $ case mode of
          ModeNone -> muted "Original unshaded card."
          ModeFusedColor -> do
            sliderRow "Grayscale" 0 1 grayAmt
            sliderRow "Tint" 0 1 tintAmt
          ModeBlur ->
            sliderRow "Sigma (px)" 0.5 10 blurSigma
          ModeWaveVignette -> do
            sliderRow "Wave (px)" 0 16 waveAmp
            sliderRow "Vignette" 0 1 vigAmt
          ModeShadow ->
            sliderRow "Shadow blur" 0 16 shadowRad
          ModeOpacity ->
            sliderRow "Opacity" 0.1 1 opacityVal

        let sampleCard =
              panelWith (padAll 16 . gap 10 . fixedW 360) $ do
                rowWith (tight . gap 8 . alignMiddle) $ do
                  box (fixedWH 28 28) (colorRGB 235 80 70)
                  box (fixedWH 28 28) (colorRGB 70 195 110)
                  box (fixedWH 28 28) (colorRGB 70 145 245)
                  flex
                  n <- readVar clicks
                  labelWith fontMono ("Clicks: " <> T.pack (show n))
                label "Interactive controls keep working inside withShader:"
                rowWith (tight . gap 8) $ do
                  whenM (styled primary (button "Increment")) (modifyVar clicks (+ 1))
                  whenM (button "Reset") (writeVar clicks 0)

        -- Extra padding gives blur and shadow margins room to breathe.
        columnWith (padAll 24 . alignCenter) $ do
          gVal <- readVar grayAmt
          tVal <- readVar tintAmt
          bVal <- readVar blurSigma
          wVal <- readVar waveAmp
          vVal <- readVar vigAmt
          sVal <- readVar shadowRad
          oVal <- readVar opacityVal
          scope $ case mode of
            ModeNone -> sampleCard
            ModeFusedColor ->
              withShader grayAndTint (gVal, TintParams (colorRGB 60 140 255) tVal) sampleCard
            ModeBlur ->
              withShader gaussianBlur bVal sampleCard
            ModeWaveVignette -> do
              (_, effResp) <-
                mouseArea tight $
                  withShader
                    waveVignette
                    (WaveParams wVal 0.08 phase, VignetteParams vVal 0.45)
                    sampleCard
              if anim then keepAnimating effResp else pure ()
            ModeShadow ->
              withShader dropShadow (DropShadowParams (V2 6 8) sVal (colorRGBA 0 0 0 180)) sampleCard
            ModeOpacity ->
              withShader groupOpacity oVal sampleCard

sliderRow :: T.Text -> Float -> Float -> Var Float -> NanoUI ()
sliderRow title lo hi v = rowWith (tight . gap 10 . alignMiddle . fillW) $ do
  labelWith (fixedW 100) title
  sliderWith (fixedW 180) lo hi v
  cur <- readVar v
  labelWith (fontMono . fixedW 48) (T.pack (show (fromIntegral (round (cur * 100) :: Int) / 100.0 :: Float)))
