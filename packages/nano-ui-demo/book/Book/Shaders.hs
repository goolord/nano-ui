{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Code for the book's Shaders chapter (@book/src/advanced/shaders.md@).
module Book.Shaders where

import GHC.Generics (Generic)
import NanoUI
import NanoUI.Shader

-- ANCHOR: surface_basic
heroBanner :: NanoUI ()
heroBanner =
  shaderConfigured cfg meshGradient4 params
  where
    cfg =
      defaultShaderConfig
        { scLayout = fixedH 140 . fillW
        , scCornerRadius = 12
        , scFallback = colorRGB 40 80 180
        }
    params =
      MeshGradientParams
        { mgTopLeft = colorRGB 40 90 220
        , mgTopRight = colorRGB 220 60 150
        , mgBottomLeft = colorRGB 20 180 160
        , mgBottomRight = colorRGB 245 190 50
        }
-- ANCHOR_END: surface_basic

-- ANCHOR: custom_uniforms
data PulseParams = PulseParams
  { ppColor :: !Color
  , ppPhase :: !Float
  , ppRings :: !Float
  }
  deriving (Eq, Generic, Uniforms)
-- ANCHOR_END: custom_uniforms

-- ANCHOR: custom_surface
pulseRing :: Shader 'Surface PulseParams
pulseRing = surfaceShader $ \u ctx ->
  let centered = ctx.uv *^ 2.0 - vec2 1.0 1.0
      dist = vLength centered
      wave = 0.5 + 0.5 * sin (dist * u.ppRings - u.ppPhase)
      half = ctx.size *^ 0.5
      edge = sdRoundedBox (ctx.pos - half) half ctx.cornerRadius
      aa = 0.75 / maxE ctx.scale 1.0
      mask = 1.0 - smoothstep (negate aa) aa edge
   in vec4_31 (u.ppColor.rgb *^ wave) (u.ppColor.a * mask)
-- ANCHOR_END: custom_surface

-- ANCHOR: subtree_effect
blurredCard :: Float -> NanoUI () -> NanoUI ()
blurredCard sigma body =
  withShader gaussianBlur sigma (panel body)
-- ANCHOR_END: subtree_effect

-- ANCHOR: fused_effect
disabledTint :: Shader 'ColorEffect (Float, TintParams)
disabledTint = composeColor grayscale tint

dimmedPanel :: NanoUI () -> NanoUI ()
dimmedPanel body =
  withShader disabledTint (0.85, TintParams (colorRGB 30 60 110) 0.25) (panel body)
-- ANCHOR_END: fused_effect

-- ANCHOR: blur_loop
boxAverage5 :: Shader 'LayerEffect Float
boxAverage5 =
  layerEffectM (\step -> V2 (2 * abs step) (2 * abs step)) $ \u ctx layer -> do
    step <- bind (maxE (uVal u) 0.0)
    zero <- bind (vec4 0.0 0.0 0.0 0.0)
    acc <- forRange (-2) 3 zero $ \iy rowAcc ->
      forRange (-2) 3 rowAcc $ \ix colAcc -> do
        let offset = vec2 (intToFloat ix * step) (intToFloat iy * step)
            sample = sampleLayer layer (ctx.pos + offset)
        pure (colAcc + sample)
    pure (acc *^ (1.0 / 25.0))
-- ANCHOR_END: blur_loop

-- ANCHOR: capabilities
shaderStatus :: NanoUI ()
shaderStatus = do
  caps <- askShaderCapabilities
  if capSurfaceShaders caps
    then label "GPU shaders active"
    else label "Using flat fallback color"
-- ANCHOR_END: capabilities
