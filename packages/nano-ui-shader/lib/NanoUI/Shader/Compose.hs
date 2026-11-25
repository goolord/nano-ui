{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

-- | Shader compilation from the EDSL to multi-target 'Shader' values, plus
-- algebraic pass fusion ('composeColor', 'composeDistort', 'distortThenColor')
-- and multi-pass chaining ('chainLayer').
module NanoUI.Shader.Compose
  ( -- * Compiling shaders
    surfaceShader
  , surfaceShaderM
  , colorEffect
  , colorEffectM
  , distortionEffect
  , distortionEffectM
  , layerEffect
  , layerEffectM
    -- * Algebraic fusion and multi-pass chaining
  , composeColor
  , composeDistort
  , distortThenColor
  , chainLayer
    -- * Lowering to IR and backend artifacts
  , compileSurfaceIR
  , compileColorIR
  , compileLayerIR
  , compileIRToProgram
  , compileIRToShader
  ) where

import Data.Bits (shiftR, xor)
import Data.Dynamic (fromDynamic, toDyn)
import Data.Proxy (Proxy (..))
import Data.Word (Word64)
import NanoUI (V2 (..))
import NanoUI.Internal.Shader
  ( CpuFragCtx (..)
  , IsNodeEffect (..)
  , Shader (..)
  , ShaderKind (..)
  , ShaderPass (..)
  , ShaderProgram (..)
  )
import NanoUI.Internal.Uniforms
  ( Uniforms (..)
  , V4
  , packUniformBytes
  , uniformLayout
  , uniformSize
  )
import NanoUI.Shader.Internal.Emit.Glsl (emitGlsl150, emitGlslEs300)
import NanoUI.Shader.Internal.Emit.Msl (emitMsl)
import NanoUI.Shader.Internal.Emit.Spirv (emitSpirv)
import NanoUI.Shader.Internal.Eval.Cpu (evalShaderIRColor)
import NanoUI.Shader.Expr
  ( Expr
  , ExprU (..)
  , FragCtx (..)
  , FragM
  , Layer
  , bind
  , mkFragCtx
  , mkLayer
  , runFragM
  , sampleLayer
  , sampleLayerUv
  )
import NanoUI.Shader.Internal.IR (ShaderIR (..), emptyIRBuilder, hashShaderIR, runIRBuilder)
import System.IO.Unsafe (unsafeDupablePerformIO)

-- | Monomorphic EDSL bodies stored in 'shaderFusion' so compatible effects
-- fuse into a single GPU pass with full CSE and zero extra offscreen targets.
data FusionBody
  = FusionColor !(Int -> FragCtx -> Expr V4 -> FragM (Expr V4))
  | FusionDistort !(Int -> FragCtx -> Expr V2 -> FragM (Expr V2))

-- | Compile a 'ShaderIR' into all four GPU backend payloads (GLSL 150,
-- GLSL ES 300, SPIR-V 1.0, MSL). Fields are lazy on 'ShaderProgram' so a
-- backend only pays for the format it consumes.
compileIRToProgram :: ShaderIR -> ShaderProgram
compileIRToProgram ir =
  ShaderProgram
    { spGlsl150 = Just (emitGlsl150 ir)
    , spGlslEs300 = Just (emitGlslEs300 ir)
    , spSpirv = Just (emitSpirv ir)
    , spMsl = Just (emitMsl ir)
    , spNeedsTex = irNeedsInput ir
    , spUniformLayout = irUniforms ir
    , spUniformBytes = irUniformBytes ir
    }

-- | Build a single-pass 'Shader' from a lowered 'ShaderIR', attaching a
-- pure CPU evaluator and optional fusion metadata.
compileIRToShader ::
  forall k u.
  Uniforms u =>
  (u -> V2) ->
  Maybe FusionBody ->
  ShaderIR ->
  Shader k u
compileIRToShader margin mFusion ir =
  let prog = compileIRToProgram ir
      !h = hashShaderIR ir
      !sz = uniformSize (Proxy :: Proxy u)
      pass =
        ShaderPass
          { spHash = h
          , spProgram = prog
          , spUniformSize = sz
          , spPack = \u mba base -> do
              _ <- pokeUniformAt u mba base 0
              pure ()
          }
      cpuEval u ctx pixelSampler =
        let !uBytes = unsafeDupablePerformIO (packUniformBytes u)
         in evalShaderIRColor ir uBytes ctx pixelSampler
   in Shader
        { shaderHash = h
        , shaderPasses = [pass]
        , shaderMargin = margin
        , shaderCpuEval = Just cpuEval
        , shaderSvgOpacity = Nothing
        , shaderFusion = fmap toDyn mFusion
        }

buildFragIR :: Uniforms u => Proxy u -> FragM (Expr V4) -> ShaderIR
buildFragIR p action =
  let fields = uniformLayout p
      uBytes = uniformSize p
      (outId, b) = runFragM action emptyIRBuilder
   in runIRBuilder fields uBytes b outId

-- | Lower a 'Surface' shader body into 'ShaderIR'.
compileSurfaceIR ::
  forall u.
  Uniforms u =>
  (ExprU u -> FragCtx -> FragM (Expr V4)) ->
  ShaderIR
compileSurfaceIR body = buildFragIR (Proxy :: Proxy u) (body (ExprU 0) mkFragCtx)

-- | Lower a 'ColorEffect' body into 'ShaderIR', reading the current pixel
-- from the subtree texture at @ctx.uv@.
compileColorIR ::
  forall u.
  Uniforms u =>
  (ExprU u -> FragCtx -> Expr V4 -> FragM (Expr V4)) ->
  ShaderIR
compileColorIR body =
  let ctx = mkFragCtx
      layer = mkLayer
   in buildFragIR (Proxy :: Proxy u) $ do
        c0 <- bind (sampleLayerUv layer (uv ctx))
        body (ExprU 0) ctx c0

-- | Lower a 'LayerEffect' body into 'ShaderIR'.
compileLayerIR ::
  forall u.
  Uniforms u =>
  (ExprU u -> FragCtx -> Layer -> FragM (Expr V4)) ->
  ShaderIR
compileLayerIR body = buildFragIR (Proxy :: Proxy u) (body (ExprU 0) mkFragCtx mkLayer)

-- | Compile a pure Tier 1 leaf 'Surface' fragment shader.
surfaceShader ::
  Uniforms u =>
  (ExprU u -> FragCtx -> Expr V4) ->
  Shader 'Surface u
surfaceShader f = surfaceShaderM (\u ctx -> pure (f u ctx))

-- | Compile a monadic Tier 1 leaf 'Surface' fragment shader with explicit
-- 'bind' and 'forRange' support.
surfaceShaderM ::
  Uniforms u =>
  (ExprU u -> FragCtx -> FragM (Expr V4)) ->
  Shader 'Surface u
surfaceShaderM body = compileIRToShader (const (V2 0 0)) Nothing (compileSurfaceIR body)

-- | Compile a pure per-pixel 'ColorEffect' (eligible for single-pass fusion
-- via 'composeColor' and 'distortThenColor').
colorEffect ::
  Uniforms u =>
  (ExprU u -> FragCtx -> Expr V4 -> Expr V4) ->
  Shader 'ColorEffect u
colorEffect f = colorEffectM (\u ctx c -> pure (f u ctx c))

-- | Compile a monadic per-pixel 'ColorEffect' (eligible for single-pass fusion).
colorEffectM ::
  Uniforms u =>
  (ExprU u -> FragCtx -> Expr V4 -> FragM (Expr V4)) ->
  Shader 'ColorEffect u
colorEffectM body =
  let fusion = FusionColor (\base ctx c -> body (ExprU base) ctx c)
   in compileIRToShader (const (V2 0 0)) (Just fusion) (compileColorIR body)

-- | Compile a pure coordinate-warp 'DistortionEffect' with an explicit maximum
-- sample offset in logical pixels (eligible for single-pass fusion via
-- 'composeDistort' and 'distortThenColor').
distortionEffect ::
  Uniforms u =>
  (u -> V2) ->
  (ExprU u -> FragCtx -> Expr V2 -> Expr V2) ->
  Shader 'DistortionEffect u
distortionEffect margin f = distortionEffectM margin (\u ctx p -> pure (f u ctx p))

-- | Compile a monadic coordinate-warp 'DistortionEffect' with an explicit
-- maximum sample offset in logical pixels.
distortionEffectM ::
  forall u.
  Uniforms u =>
  (u -> V2) ->
  (ExprU u -> FragCtx -> Expr V2 -> FragM (Expr V2)) ->
  Shader 'DistortionEffect u
distortionEffectM margin body =
  let fusion = FusionDistort (\base ctx p -> body (ExprU base) ctx p)
      ir = compileLayerIR @u $ \u ctx layer -> do
        p' <- body u ctx (pos ctx)
        pure (sampleLayer layer p')
   in compileIRToShader margin (Just fusion) ir

-- | Compile a pure neighborhood-sampling 'LayerEffect' with an explicit
-- maximum sample offset in logical pixels.
layerEffect ::
  Uniforms u =>
  (u -> V2) ->
  (ExprU u -> FragCtx -> Layer -> Expr V4) ->
  Shader 'LayerEffect u
layerEffect margin f = layerEffectM margin (\u ctx layer -> pure (f u ctx layer))

-- | Compile a monadic neighborhood-sampling 'LayerEffect' with explicit
-- 'bind' and 'forRange' support.
layerEffectM ::
  Uniforms u =>
  (u -> V2) ->
  (ExprU u -> FragCtx -> Layer -> FragM (Expr V4)) ->
  Shader 'LayerEffect u
layerEffectM margin body = compileIRToShader margin Nothing (compileLayerIR body)

-- | Fuse two 'ColorEffect's into a single fragment pass (0 extra offscreen
-- targets). Falls back to two sequential passes if either input is a raw
-- hand-written shader without EDSL fusion metadata.
composeColor ::
  forall u1 u2.
  (Uniforms u1, Uniforms u2) =>
  Shader 'ColorEffect u1 ->
  Shader 'ColorEffect u2 ->
  Shader 'ColorEffect (u1, u2)
composeColor s1 s2 =
  case (shaderFusion s1 >>= fromDynamic, shaderFusion s2 >>= fromDynamic) of
    (Just (FusionColor f1), Just (FusionColor f2)) ->
      let !n1 = length (uniformLayout (Proxy :: Proxy u1))
          fused base ctx c0 = do
            c1 <- f1 base ctx c0 >>= bind
            f2 (base + n1) ctx c1
          ir = compileColorIR @(u1, u2) (\_ ctx c0 -> fused 0 ctx c0)
       in compileIRToShader (const (V2 0 0)) (Just (FusionColor fused)) ir
    _ -> chainPasses s1 s2

-- | Fuse two 'DistortionEffect's into a single coordinate-warp pass (0 extra
-- offscreen targets). Margins sum conservatively in X and Y.
composeDistort ::
  forall u1 u2.
  (Uniforms u1, Uniforms u2) =>
  Shader 'DistortionEffect u1 ->
  Shader 'DistortionEffect u2 ->
  Shader 'DistortionEffect (u1, u2)
composeDistort s1 s2 =
  case (shaderFusion s1 >>= fromDynamic, shaderFusion s2 >>= fromDynamic) of
    (Just (FusionDistort f1), Just (FusionDistort f2)) ->
      let !n1 = length (uniformLayout (Proxy :: Proxy u1))
          fused base ctx p0 = do
            p1 <- f1 base ctx p0 >>= bind
            f2 (base + n1) ctx p1
          margin (u1, u2) = addV2 (shaderMargin s1 u1) (shaderMargin s2 u2)
          ir = compileLayerIR @(u1, u2) $ \_ ctx layer -> do
            p' <- fused 0 ctx (pos ctx)
            pure (sampleLayer layer p')
       in compileIRToShader margin (Just (FusionDistort fused)) ir
    _ -> chainPasses s1 s2

-- | Fuse a 'DistortionEffect' followed by a 'ColorEffect' into a single
-- 'LayerEffect' pass (warping coordinates before sampling, then applying the
-- color transform with no intermediate offscreen target).
distortThenColor ::
  forall u1 u2.
  (Uniforms u1, Uniforms u2) =>
  Shader 'DistortionEffect u1 ->
  Shader 'ColorEffect u2 ->
  Shader 'LayerEffect (u1, u2)
distortThenColor s1 s2 =
  case (shaderFusion s1 >>= fromDynamic, shaderFusion s2 >>= fromDynamic) of
    (Just (FusionDistort fDist), Just (FusionColor fCol)) ->
      let !n1 = length (uniformLayout (Proxy :: Proxy u1))
          margin (u1, _) = shaderMargin s1 u1
          ir = compileLayerIR @(u1, u2) $ \_ ctx layer -> do
            p' <- fDist 0 ctx (pos ctx)
            c0 <- bind (sampleLayer layer p')
            fCol n1 ctx c0
       in compileIRToShader margin Nothing ir
    _ -> chainLayer s1 s2

-- | Chain any two subtree effects sequentially as multi-pass offscreen
-- rendering (for example, horizontal then vertical separable Gaussian blur).
chainLayer ::
  forall k1 k2 u1 u2.
  (IsNodeEffect k1, IsNodeEffect k2) =>
  Shader k1 u1 ->
  Shader k2 u2 ->
  Shader 'LayerEffect (u1, u2)
chainLayer s1 s2 =
  let !_ = nodeEffectKind (Proxy @k1)
      !_ = nodeEffectKind (Proxy @k2)
   in chainPasses s1 s2

chainPasses :: Shader k1 u1 -> Shader k2 u2 -> Shader k3 (u1, u2)
chainPasses s1 s2 =
  Shader
    { shaderHash = mix64 (shaderHash s1) (shaderHash s2 `xor` 0x434841494e)
    , shaderPasses =
        [p {spPack = \(u1, _) -> spPack p u1} | p <- shaderPasses s1]
          ++ [p {spPack = \(_, u2) -> spPack p u2} | p <- shaderPasses s2]
    , shaderMargin = \(u1, u2) -> addV2 (shaderMargin s1 u1) (shaderMargin s2 u2)
    , shaderCpuEval = case (shaderCpuEval s1, shaderCpuEval s2) of
        (Just e1, Just e2) ->
          Just $ \(u1, u2) ctx sampler ->
            let V2 sw sh' = cfcSize ctx
                pass1 pos@(V2 px py) =
                  let uv' = V2 (px / max 1 sw) (py / max 1 sh')
                   in e1 u1 ctx {cfcPos = pos, cfcUv = uv'} sampler
             in e2 u2 ctx pass1
        _ -> Nothing
    , shaderSvgOpacity = Nothing
    , shaderFusion = Nothing
    }

addV2 :: V2 -> V2 -> V2
addV2 (V2 ax ay) (V2 bx by) = V2 (ax + bx) (ay + by)

mix64 :: Word64 -> Word64 -> Word64
mix64 a b =
  let !x0 = (a `xor` (b + 0x9e3779b97f4a7c15)) * 0xbf58476d1ce4e5b9
      !x1 = (x0 `xor` (x0 `shiftR` 30)) * 0x94d049bb133111eb
   in x1 `xor` (x1 `shiftR` 31)
