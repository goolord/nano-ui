{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}

-- | Standard library of typed fragment shaders for @nano-ui@:
--
-- * Tier 1 leaf 'Surface' fills ('meshGradient4', 'radialGlow',
--   'sdfRoundedBox', 'colorWheelHsv', 'noiseGrain', 'checkerboard')
-- * Fusible 'ColorEffect's ('groupOpacity', 'grayscale', 'tint',
--   'brightnessContrast', 'colorMatrix', 'vignette', 'scanlines')
-- * Fusible 'DistortionEffect's ('waveDistort', 'rippleDistort', 'pixelate')
-- * Multi-sample and multi-pass 'LayerEffect's ('boxBlur', 'gaussianBlur',
--   'dropShadow', 'outerGlow', 'chromaticAberration')
module NanoUI.Shader.Effects
  ( -- * Surface shaders
    MeshGradientParams (..)
  , meshGradient4
  , RadialGlowParams (..)
  , radialGlow
  , SdfBoxParams (..)
  , sdfRoundedBox
  , colorWheelHsv
  , NoiseParams (..)
  , noiseGrain
  , CheckerParams (..)
  , checkerboard
    -- * Color effects
  , groupOpacity
  , grayscale
  , TintParams (..)
  , tint
  , BrightnessContrastParams (..)
  , brightnessContrast
  , ColorMatrixParams (..)
  , colorMatrix
  , VignetteParams (..)
  , vignette
  , ScanlineParams (..)
  , scanlines
    -- * Distortion effects
  , WaveParams (..)
  , waveDistort
  , RippleParams (..)
  , rippleDistort
  , pixelate
    -- * Layer effects
  , boxBlur
  , gaussianBlur
  , DropShadowParams (..)
  , dropShadow
  , OuterGlowParams (..)
  , outerGlow
  , chromaticAberration
  ) where

import GHC.Generics (Generic)
import NanoUI.Internal.Shader (Shader (..), ShaderKind (..), contramapShader)
import NanoUI (Color, V2 (..))
import NanoUI.Internal.Uniforms (Uniforms, V3 (..), V4)
import NanoUI.Shader.Compose
  ( chainLayer
  , colorEffect
  , distortionEffect
  , layerEffect
  , layerEffectM
  , surfaceShader
  )
import NanoUI.Shader.Expr
import NanoUI.Shader.Swizzle ()

-- ---------------------------------------------------------------------------
-- Helpers

-- | Soft anti-aliased rounded-corner coverage mask for a surface node using
-- @ctx.cornerRadius@ and @ctx.scale@.
cornerMask :: FragCtx -> Expr Float
cornerMask ctx =
  let halfSize = ctx.size *^ floatE 0.5
      p = ctx.pos - halfSize
      r = minE ctx.cornerRadius (minE halfSize.x halfSize.y)
      d = sdRoundedBox p halfSize r
      aa = floatE 0.75 / maxE ctx.scale (floatE 1.0)
      mask = floatE 1.0 - smoothstep (negate aa) aa d
   in select (ctx.cornerRadius .> floatE 0.0) mask (floatE 1.0)

-- | Porter-Duff source-over compositing in straight-alpha RGBA.
overStraight :: Expr V4 -> Expr V4 -> Expr V4
overStraight src dst =
  let sa = src.a
      da = dst.a
      outA = sa + da * (floatE 1.0 - sa)
      rgb = (src.rgb *^ sa + dst.rgb *^ (da * (floatE 1.0 - sa))) /^ maxE outA (floatE 1.0e-5)
   in vec4_31 rgb outA

-- ---------------------------------------------------------------------------
-- Surface shaders

-- | Corner colors for 'meshGradient4'.
data MeshGradientParams = MeshGradientParams
  { mgTopLeft :: !Color
  , mgTopRight :: !Color
  , mgBottomLeft :: !Color
  , mgBottomRight :: !Color
  }
  deriving (Eq, Show, Generic, Uniforms)

-- | Smooth bilinear 4-corner gradient clipped to @ctx.cornerRadius@.
meshGradient4 :: Shader 'Surface MeshGradientParams
meshGradient4 = surfaceShader $ \u ctx ->
  let tx = smoothstep (floatE 0.0) (floatE 1.0) ctx.uv.x
      ty = smoothstep (floatE 0.0) (floatE 1.0) ctx.uv.y
      top = mix u.mgTopLeft u.mgTopRight tx
      bot = mix u.mgBottomLeft u.mgBottomRight tx
      col = mix top bot ty * ctx.color
   in withAlphaE (col.a * cornerMask ctx) col

-- | Parameters for 'radialGlow'.
data RadialGlowParams = RadialGlowParams
  { rgColor :: !Color
  , rgCenter :: !V2
  -- ^ Normalized center in @[0, 1] x [0, 1]@.
  , rgRadius :: !Float
  -- ^ Normalized glow radius (e.g. @0.5@).
  , rgSoftness :: !Float
  -- ^ Edge falloff softness (higher values soften the core).
  }
  deriving (Eq, Show, Generic, Uniforms)

-- | Soft radial glow centered at @rgCenter@.
radialGlow :: Shader 'Surface RadialGlowParams
radialGlow = surfaceShader $ \u ctx ->
  let d = vDistance ctx.uv u.rgCenter
      r = maxE u.rgRadius (floatE 1.0e-4)
      t = clamp01E (floatE 1.0 - d / r)
      falloff = powE t (maxE u.rgSoftness (floatE 0.25))
      col = u.rgColor * ctx.color
   in withAlphaE (col.a * falloff * cornerMask ctx) col

-- | Parameters for 'sdfRoundedBox'.
data SdfBoxParams = SdfBoxParams
  { sbFill :: !Color
  , sbBorder :: !Color
  , sbBorderWidth :: !Float
  -- ^ Border width in logical pixels.
  , sbRadius :: !Float
  -- ^ Corner radius in logical pixels (falls back to @ctx.cornerRadius@ when @<= 0@).
  }
  deriving (Eq, Show, Generic, Uniforms)

-- | Anti-aliased SDF rounded rectangle with optional inner border.
sdfRoundedBox :: Shader 'Surface SdfBoxParams
sdfRoundedBox = surfaceShader $ \u ctx ->
  let halfSize = ctx.size *^ floatE 0.5
      p = ctx.pos - halfSize
      rReq = select (u.sbRadius .> floatE 0.0) u.sbRadius ctx.cornerRadius
      r = minE rReq (minE halfSize.x halfSize.y)
      d = sdRoundedBox p halfSize r
      aa = floatE 0.75 / maxE ctx.scale (floatE 1.0)
      outerMask = floatE 1.0 - smoothstep (negate aa) aa d
      innerMask = floatE 1.0 - smoothstep (negate aa) aa (d + u.sbBorderWidth)
      baseCol = mix u.sbBorder u.sbFill innerMask
      col = select (u.sbBorderWidth .> floatE 0.0) baseCol u.sbFill * ctx.color
   in withAlphaE (col.a * outerMask) col

-- | Smooth HSV color wheel parameterized by value (brightness in @[0, 1]@).
colorWheelHsv :: Shader 'Surface Float
colorWheelHsv = surfaceShader $ \u ctx ->
  let p = ctx.uv - vec2 (floatE 0.5) (floatE 0.5)
      r = vLength p * floatE 2.0
      ang = atan2E p.y p.x
      hue = fractE (ang / floatE (2.0 * pi) + floatE 1.0)
      sat = clamp01E r
      rgb = hsvToRgbE hue sat (clamp01E (uVal u))
      aa = floatE 2.0 / maxE (minE ctx.size.x ctx.size.y) (floatE 1.0)
      alpha = floatE 1.0 - smoothstep (floatE 1.0 - aa) (floatE 1.0 + aa) r
   in vec4_31 rgb alpha * ctx.color

-- | Parameters for 'noiseGrain'.
data NoiseParams = NoiseParams
  { npScale :: !Float
  -- ^ Feature size in logical pixels.
  , npTime :: !Float
  -- ^ Animation phase offset.
  , npColorA :: !Color
  , npColorB :: !Color
  }
  deriving (Eq, Show, Generic, Uniforms)

-- | Procedural 2-octave 2D value noise surface.
noiseGrain :: Shader 'Surface NoiseParams
noiseGrain = surfaceShader $ \u ctx ->
  let sc = maxE u.npScale (floatE 1.0)
      p = ctx.pos /^ sc + vec2 u.npTime (u.npTime * floatE 0.73)
      n1 = valueNoise2D p
      n2 = valueNoise2D (p *^ floatE 2.03 + vec2 (floatE 17.1) (floatE 31.7))
      v = clamp01E (n1 * floatE 0.65 + n2 * floatE 0.35)
      col = mix u.npColorA u.npColorB v * ctx.color
   in withAlphaE (col.a * cornerMask ctx) col

-- | Parameters for 'checkerboard'.
data CheckerParams = CheckerParams
  { cpCellSize :: !Float
  -- ^ Checker square size in logical pixels.
  , cpColorA :: !Color
  , cpColorB :: !Color
  }
  deriving (Eq, Show, Generic, Uniforms)

-- | Crisp logical-pixel checkerboard (commonly used as a transparency backdrop).
checkerboard :: Shader 'Surface CheckerParams
checkerboard = surfaceShader $ \u ctx ->
  let cell = maxE u.cpCellSize (floatE 1.0)
      gx = floorE (ctx.pos.x / cell)
      gy = floorE (ctx.pos.y / cell)
      oddCell = modE (gx + gy) (floatE 2.0) .>= floatE 1.0
      col = select oddCell u.cpColorB u.cpColorA * ctx.color
   in withAlphaE (col.a * cornerMask ctx) col

-- ---------------------------------------------------------------------------
-- Color effects (single-pass fusible)

-- | Multiply the entire subtree's alpha as one composited group (avoiding
-- internal double-blending). Also carries 'shaderSvgOpacity' so SVG export
-- emits a native @<g opacity="...">@ group.
groupOpacity :: Shader 'ColorEffect Float
groupOpacity =
  (colorEffect $ \u _ c -> withAlphaE (c.a * clamp01E (uVal u)) c)
    { shaderSvgOpacity = Just (\a -> max 0 (min 1 a))
    }

-- | Desaturate a subtree toward perceptual luminance by strength in @[0, 1]@.
grayscale :: Shader 'ColorEffect Float
grayscale = colorEffect $ \u _ c ->
  let lum = luminanceE c
      gray = vec3 lum lum lum
   in vec4_31 (mix c.rgb gray (clamp01E (uVal u))) c.a

-- | Parameters for 'tint'.
data TintParams = TintParams
  { tpColor :: !Color
  , tpAmount :: !Float
  -- ^ Blend strength in @[0, 1]@.
  }
  deriving (Eq, Show, Generic, Uniforms)

-- | Blend subtree RGB toward @tpColor@ by @tpAmount@, preserving alpha.
tint :: Shader 'ColorEffect TintParams
tint = colorEffect $ \u _ c ->
  vec4_31 (mix c.rgb u.tpColor.rgb (clamp01E u.tpAmount)) c.a

-- | Parameters for 'brightnessContrast'.
data BrightnessContrastParams = BrightnessContrastParams
  { bcBrightness :: !Float
  -- ^ Additive offset in @[-1, 1]@ (@0@ is neutral).
  , bcContrast :: !Float
  -- ^ Multiplicative contrast around @0.5@ (@1@ is neutral).
  }
  deriving (Eq, Show, Generic, Uniforms)

-- | Adjust brightness and contrast of a subtree.
brightnessContrast :: Shader 'ColorEffect BrightnessContrastParams
brightnessContrast = colorEffect $ \u _ c ->
  let half = vec3 (floatE 0.5) (floatE 0.5) (floatE 0.5)
      adj = (c.rgb - half) *^ u.bcContrast + half +^ u.bcBrightness
   in vec4_31 (clampE adj (vec3 (floatE 0.0) (floatE 0.0) (floatE 0.0)) (vec3 (floatE 1.0) (floatE 1.0) (floatE 1.0))) c.a

-- | 3x3 RGB matrix rows plus additive RGB offset for 'colorMatrix'.
data ColorMatrixParams = ColorMatrixParams
  { cmRowR :: !V3
  , cmRowG :: !V3
  , cmRowB :: !V3
  , cmOffset :: !V3
  }
  deriving (Eq, Show, Generic, Uniforms)

-- | Apply an arbitrary 3x3 RGB color matrix and offset (sepia, color-blindness
-- simulation, channel swaps).
colorMatrix :: Shader 'ColorEffect ColorMatrixParams
colorMatrix = colorEffect $ \u _ c ->
  let r = dot c.rgb u.cmRowR + u.cmOffset.x
      g = dot c.rgb u.cmRowG + u.cmOffset.y
      b = dot c.rgb u.cmRowB + u.cmOffset.z
      rgb = clampE (vec3 r g b) (vec3 (floatE 0.0) (floatE 0.0) (floatE 0.0)) (vec3 (floatE 1.0) (floatE 1.0) (floatE 1.0))
   in vec4_31 rgb c.a

-- | Parameters for 'vignette'.
data VignetteParams = VignetteParams
  { vpStrength :: !Float
  -- ^ Edge darkening factor in @[0, 1]@.
  , vpSoftness :: !Float
  -- ^ Falloff width (e.g. @0.4@).
  }
  deriving (Eq, Show, Generic, Uniforms)

-- | Radial vignette darkening the corners of a subtree.
vignette :: Shader 'ColorEffect VignetteParams
vignette = colorEffect $ \u ctx c ->
  let d = vDistance ctx.uv (vec2 (floatE 0.5) (floatE 0.5)) * floatE 1.4142135
      soft = maxE u.vpSoftness (floatE 0.05)
      fade = smoothstep (floatE 1.0 - soft) (floatE 1.0) d
      factor = floatE 1.0 - clamp01E u.vpStrength * fade
   in vec4_31 (c.rgb *^ factor) c.a

-- | Parameters for 'scanlines'.
data ScanlineParams = ScanlineParams
  { slPitch :: !Float
  -- ^ Vertical spacing in logical pixels (e.g. @3.0@).
  , slDarken :: !Float
  -- ^ Scanline trough darkening in @[0, 1]@.
  }
  deriving (Eq, Show, Generic, Uniforms)

-- | Horizontal CRT scanline modulation.
scanlines :: Shader 'ColorEffect ScanlineParams
scanlines = colorEffect $ \u ctx c ->
  let pitch = maxE u.slPitch (floatE 1.0)
      wave = floatE 0.5 + floatE 0.5 * sin (ctx.pos.y * floatE (2.0 * pi) / pitch)
      factor = floatE 1.0 - clamp01E u.slDarken * (floatE 1.0 - wave)
   in vec4_31 (c.rgb *^ factor) c.a

-- ---------------------------------------------------------------------------
-- Distortion effects (single-pass fusible)

-- | Parameters for 'waveDistort'.
data WaveParams = WaveParams
  { wpAmplitude :: !Float
  -- ^ Horizontal wave displacement in logical pixels.
  , wpFrequency :: !Float
  -- ^ Spatial frequency in radians per logical pixel.
  , wpPhase :: !Float
  -- ^ Animation phase in radians.
  }
  deriving (Eq, Show, Generic, Uniforms)

-- | Sinusoidal horizontal wave warp.
waveDistort :: Shader 'DistortionEffect WaveParams
waveDistort =
  distortionEffect
    (\p -> let a = abs (wpAmplitude p) in V2 a 0)
    (\u _ pos -> vec2 (pos.x + u.wpAmplitude * sin (pos.y * u.wpFrequency + u.wpPhase)) pos.y)

-- | Parameters for 'rippleDistort'.
data RippleParams = RippleParams
  { rpCenter :: !V2
  -- ^ Ripple center in logical pixels.
  , rpAmplitude :: !Float
  -- ^ Radial displacement amplitude in logical pixels.
  , rpFrequency :: !Float
  -- ^ Spatial frequency in radians per logical pixel.
  , rpPhase :: !Float
  -- ^ Animation phase in radians.
  }
  deriving (Eq, Show, Generic, Uniforms)

-- | Concentric radial water-ripple warp around @rpCenter@.
rippleDistort :: Shader 'DistortionEffect RippleParams
rippleDistort =
  distortionEffect
    (\p -> let a = abs (rpAmplitude p) in V2 a a)
    $ \u _ pos ->
      let delta = pos - u.rpCenter
          dist = vLength delta
          dir = delta /^ maxE dist (floatE 1.0e-4)
          disp = u.rpAmplitude * sin (dist * u.rpFrequency - u.rpPhase)
       in pos + dir *^ disp

-- | Quantize sample coordinates to square mosaic blocks of side @blockSize@
-- logical pixels.
pixelate :: Shader 'DistortionEffect Float
pixelate =
  distortionEffect
    (\sz -> let m = max 0 sz in V2 m m)
    $ \u _ pos ->
      let sz = maxE (uVal u) (floatE 1.0)
       in (floorE (pos /^ sz) + vec2 (floatE 0.5) (floatE 0.5)) *^ sz

-- ---------------------------------------------------------------------------
-- Layer effects (single-pass and multi-pass)

-- | 5x5 uniform box blur scaled by logical pixel radius. Accumulates in
-- premultiplied space and un-premultiplies so blurred edges over transparent
-- offscreen targets avoid dark fringes.
boxBlur :: Shader 'LayerEffect Float
boxBlur =
  layerEffectM (\r -> let m = max 0 r in V2 m m) $ \u ctx layer -> do
    stepPx <- bind (maxE (uVal u) (floatE 0.0) * floatE 0.5)
    acc0 <- bind (vec4 (floatE 0.0) (floatE 0.0) (floatE 0.0) (floatE 0.0))
    total <- forRange (-2) 3 acc0 $ \iy rowAcc ->
      forRange (-2) 3 rowAcc $ \ix colAcc -> do
        let off = vec2 (intToFloat ix * stepPx) (intToFloat iy * stepPx)
            s = sampleLayer layer (ctx.pos + off)
            premul = vec4_31 (s.rgb *^ s.a) s.a
        pure (colAcc + premul)
    let avg = total *^ floatE (1.0 / 25.0)
        straightRgb = avg.rgb /^ maxE avg.a (floatE 1.0e-5)
    pure (vec4_31 straightRgb avg.a)

-- | One 1D 9-tap Gaussian blur pass along axis @(ax, ay)@.
gaussianBlur1D :: Float -> Float -> Shader 'LayerEffect Float
gaussianBlur1D ax ay =
  layerEffectM (\sigma -> let m = max 0 (sigma * 3.0) in V2 (m * abs ax) (m * abs ay)) $ \u ctx layer -> do
    sigma <- bind (maxE (uVal u) (floatE 1.0e-3))
    stepPx <- bind (sigma * floatE 0.75)
    invTwoSig2 <- bind (floatE 0.5 / (sigma * sigma))
    let zero4 = vec4 (floatE 0.0) (floatE 0.0) (floatE 0.0) (floatE 0.0)
    (colSum, wSum) <- forRange (-4) 5 (zero4, floatE 0.0) $ \ik (cAcc, wAcc) -> do
      d <- bind (intToFloat ik * stepPx)
      w <- bind (exp (negate (d * d) * invTwoSig2))
      let off = vec2 (d * floatE ax) (d * floatE ay)
          s = sampleLayer layer (ctx.pos + off)
          premul = vec4_31 (s.rgb *^ s.a) s.a
      pure (cAcc + premul *^ w, wAcc + w)
    let avg = colSum /^ maxE wSum (floatE 1.0e-5)
        straightRgb = avg.rgb /^ maxE avg.a (floatE 1.0e-5)
    pure (vec4_31 straightRgb avg.a)

-- | Separable 2-pass Gaussian blur parameterized by blur sigma in logical
-- pixels (horizontal pass chained with vertical pass).
gaussianBlur :: Shader 'LayerEffect Float
gaussianBlur =
  contramapShader (\sigma -> (sigma, sigma)) $
    chainLayer (gaussianBlur1D 1.0 0.0) (gaussianBlur1D 0.0 1.0)

-- | Parameters for 'dropShadow'.
data DropShadowParams = DropShadowParams
  { dsOffset :: !V2
  -- ^ Shadow offset in logical pixels (positive moves right and down).
  , dsRadius :: !Float
  -- ^ Softness blur radius in logical pixels.
  , dsColor :: !Color
  -- ^ Shadow RGBA color.
  }
  deriving (Eq, Show, Generic, Uniforms)

-- | Soft offset drop shadow composited behind the source subtree.
dropShadow :: Shader 'LayerEffect DropShadowParams
dropShadow =
  layerEffectM
    (\p -> V2 (abs (v2X (dsOffset p)) + max 0 (dsRadius p)) (abs (v2Y (dsOffset p)) + max 0 (dsRadius p)))
    $ \u ctx layer -> do
      src <- bind (sampleLayer layer ctx.pos)
      stepPx <- bind (maxE u.dsRadius (floatE 0.0) * floatE 0.5)
      basePos <- bind (ctx.pos - u.dsOffset)
      aSum <- forRange (-2) 3 (floatE 0.0) $ \iy rAcc ->
        forRange (-2) 3 rAcc $ \ix cAcc -> do
          let off = vec2 (intToFloat ix * stepPx) (intToFloat iy * stepPx)
              s = sampleLayer layer (basePos + off)
          pure (cAcc + s.a)
      let shadowA = (aSum * floatE (1.0 / 25.0)) * u.dsColor.a
          shadowCol = vec4_31 u.dsColor.rgb shadowA
      pure (overStraight src shadowCol)

-- | Parameters for 'outerGlow'.
data OuterGlowParams = OuterGlowParams
  { ogRadius :: !Float
  -- ^ Glow spread radius in logical pixels.
  , ogIntensity :: !Float
  -- ^ Glow brightness multiplier.
  , ogColor :: !Color
  }
  deriving (Eq, Show, Generic, Uniforms)

-- | Soft halo glow behind non-transparent edges of the source subtree.
outerGlow :: Shader 'LayerEffect OuterGlowParams
outerGlow =
  layerEffectM (\p -> let r = max 0 (ogRadius p) in V2 r r) $ \u ctx layer -> do
    src <- bind (sampleLayer layer ctx.pos)
    stepPx <- bind (maxE u.ogRadius (floatE 0.0) * floatE 0.5)
    aSum <- forRange (-2) 3 (floatE 0.0) $ \iy rAcc ->
      forRange (-2) 3 rAcc $ \ix cAcc -> do
        let off = vec2 (intToFloat ix * stepPx) (intToFloat iy * stepPx)
            s = sampleLayer layer (ctx.pos + off)
        pure (cAcc + s.a)
    let glowA = clamp01E ((aSum * floatE (1.0 / 25.0)) * u.ogIntensity) * u.ogColor.a
        glowCol = vec4_31 u.ogColor.rgb glowA
    pure (overStraight src glowCol)

-- | Horizontal RGB chromatic aberration split by logical pixel shift.
chromaticAberration :: Shader 'LayerEffect Float
chromaticAberration =
  layerEffect (\shift -> V2 (abs shift) 0) $ \u ctx layer ->
    let d = vec2 (uVal u) (floatE 0.0)
        cR = sampleLayer layer (ctx.pos + d)
        cG = sampleLayer layer ctx.pos
        cB = sampleLayer layer (ctx.pos - d)
     in vec4 cR.r cG.g cB.b cG.a
