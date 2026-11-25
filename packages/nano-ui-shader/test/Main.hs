{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Test suite for @nano-ui-shader@: OverloadedRecordDot swizzling, typed
-- Generic uniform access, CSE, bounded loops, GLSL/MSL/SPIR-V structural
-- validation, algebraic pass fusion, CPU evaluation, and standard effects.
module Main (main) where

import Control.Monad (forM_, when)
import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Int (Int32)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Word (Word32)
import GHC.Generics (Generic)
import NanoUI
  ( Color
  , V2 (..)
  , colorA
  , colorB
  , colorG
  , colorR
  , colorRGBA
  , colorWhite
  )
import NanoUI.Internal.Shader
  ( CpuFragCtx (..)
  , Shader (..)
  , ShaderPass (..)
  , ShaderProgram (..)
  )
import NanoUI.Shader
import NanoUI.Shader.Compose (compileSurfaceIR)
import NanoUI.Shader.Emit.Glsl (emitGlsl150, emitGlslEs300)
import NanoUI.Shader.Emit.Msl (emitMsl)
import NanoUI.Shader.Emit.Spirv (emitSpirvWords)
import NanoUI.Shader.IR (ShaderIR (..))
import System.Exit (exitFailure)

data DemoUniforms = DemoUniforms
  { duTint :: !Color
  , duMix :: !Float
  , duFlag :: !Bool
  , duOffset :: !V2
  , duWeight :: !V3
  , duSteps :: !Int32
  }
  deriving (Eq, Show, Generic, Uniforms)

check :: IORef Int -> String -> Bool -> IO ()
check failed name ok
  | ok = putStrLn ("[PASS] " ++ name)
  | otherwise = do
      putStrLn ("[FAIL] " ++ name)
      modifyIORef' failed (+ 1)

nearByte :: Int -> Int -> Bool
nearByte a b = abs (a - b) <= 2

evalAt :: Shader k u -> u -> V2 -> V2 -> (V2 -> Color) -> Color
evalAt sh u pos@(V2 px py) sz@(V2 sw sh') sampler =
  case shaderCpuEval sh of
    Nothing -> error "missing CPU evaluator"
    Just f ->
      f
        u
        CpuFragCtx
          { cfcUv = V2 (px / max 1 sw) (py / max 1 sh')
          , cfcPos = pos
          , cfcSize = sz
          , cfcScale = 1.0
          , cfcColor = colorWhite
          , cfcCornerRadius = 0.0
          }
        sampler

-- | Structural validator for an emitted SPIR-V 1.0 binary stream.
validateSpirv :: ByteString -> Either String ()
validateSpirv bs
  | BS.length bs < 20 = Left "SPIR-V shorter than 5-word header"
  | BS.length bs `rem` 4 /= 0 = Left "SPIR-V byte length not a multiple of 4"
  | otherwise =
      let ws = unpackWords bs
       in case ws of
            (magic : ver : _gen : bound : schema : body)
              | magic /= 0x07230203 -> Left "bad SPIR-V magic"
              | ver /= 0x00010000 -> Left "unexpected SPIR-V version"
              | bound == 0 -> Left "zero ID bound"
              | schema /= 0 -> Left "non-zero schema word"
              | otherwise -> walkInsts bound False False body
            _ -> Left "truncated SPIR-V header"
  where
    unpackWords b =
      [ let i = k * 4
            b0 = fromIntegral (BS.index b i) :: Word32
            b1 = fromIntegral (BS.index b (i + 1)) :: Word32
            b2 = fromIntegral (BS.index b (i + 2)) :: Word32
            b3 = fromIntegral (BS.index b (i + 3)) :: Word32
         in b0 + b1 * 256 + b2 * 65536 + b3 * 16777216
      | k <- [0 .. BS.length b `quot` 4 - 1]
      ]
    walkInsts _ sawEntry sawEnd []
      | not sawEntry = Left "missing OpEntryPoint"
      | not sawEnd = Left "missing OpFunctionEnd"
      | otherwise = Right ()
    walkInsts bound sawEntry sawEnd (hdr : rest) =
      let wc = fromIntegral (hdr `shiftR` 16) :: Int
          op = hdr .&. 0xFFFF
       in if wc < 1 || wc - 1 > length rest
            then Left ("invalid instruction word count " ++ show wc ++ " for opcode " ++ show op)
            else
              let (_ops, next) = splitAt (wc - 1) rest
                  sawEntry' = sawEntry || op == 15
                  sawEnd' = sawEnd || op == 56
               in walkInsts bound sawEntry' sawEnd' next

checkAllTargets :: IORef Int -> String -> Shader k u -> IO ()
checkAllTargets failed label sh = do
  let passes = shaderPasses sh
  check failed (label ++ ": non-empty passes") (not (null passes))
  forM_ (zip [1 :: Int ..] passes) $ \(i, p) -> do
    let prog = spProgram p
        tag = label ++ " pass " ++ show i
    check failed (tag ++ ": GLSL 150 header") $
      maybe False ("#version 150" `T.isPrefixOf`) (spGlsl150 prog)
    check failed (tag ++ ": GLSL ES 300 header") $
      maybe False ("#version 300 es" `T.isPrefixOf`) (spGlslEs300 prog)
    check failed (tag ++ ": MSL main0") $
      maybe False ("fragment float4 main0" `T.isInfixOf`) (spMsl prog)
    case spSpirv prog of
      Nothing -> check failed (tag ++ ": SPIR-V present") False
      Just spv -> case validateSpirv spv of
        Right () -> check failed (tag ++ ": SPIR-V valid") True
        Left err -> do
          putStrLn ("  SPIR-V error (" ++ tag ++ "): " ++ err)
          check failed (tag ++ ": SPIR-V valid") False

main :: IO ()
main = do
  failed <- newIORef (0 :: Int)

  -- 1. Swizzle + Generic Uniform record + branchless select
  let customSurf :: Shader 'Surface DemoUniforms
      customSurf = surfaceShader $ \u ctx ->
        let uv = ctx.uv.xy
            flipped = uv.yx
            cRgb = select u.duFlag u.duTint.rgb u.duTint.bgr
            k = clamp01E (u.duMix + flipped.x * floatE 0.0 + u.duOffset.x * floatE 0.0 + u.duWeight.z * floatE 0.0 + intToFloat u.duSteps * floatE 0.0)
         in vec4_31 (cRgb *^ k) (floatE 1.0)
  checkAllTargets failed "customSurf" customSurf
  let u1 = DemoUniforms (colorRGBA 200 100 50 255) 0.5 True (V2 1 2) (V3 0 0 0) 0
      u2 = u1 {duFlag = False}
      c1 = evalAt customSurf u1 (V2 10 10) (V2 100 100) (const colorWhite)
      c2 = evalAt customSurf u2 (V2 10 10) (V2 100 100) (const colorWhite)
  check failed "swizzle .rgb + select True" $
    nearByte 100 (fromIntegral (colorR c1))
      && nearByte 50 (fromIntegral (colorG c1))
      && nearByte 25 (fromIntegral (colorB c1))
  check failed "swizzle .bgr + select False" $
    nearByte 25 (fromIntegral (colorR c2))
      && nearByte 50 (fromIntegral (colorG c2))
      && nearByte 100 (fromIntegral (colorB c2))

  -- 2. CSE / structural hash-consing collapses duplicate subexpressions
  let irOnce = compileSurfaceIR @() (\_ ctx -> pure (vec4_22 ctx.uv ctx.uv))
      irDup = compileSurfaceIR @() (\_ ctx -> let a = ctx.uv + ctx.uv; b = ctx.uv + ctx.uv in pure (vec4_22 a b))
      irSingle = compileSurfaceIR @() (\_ ctx -> let a = ctx.uv + ctx.uv in pure (vec4_22 a a))
  check failed "CSE collapses duplicate expressions" (V.length (irNodes irDup) == V.length (irNodes irSingle))
  check failed "compileSurfaceIR emits GLSL/MSL/SPIR-V" $
    T.isInfixOf "NanoShaderBlock" (emitGlsl150 irOnce)
      && T.isInfixOf "NanoShaderBlock" (emitGlslEs300 irOnce)
      && T.isInfixOf "NanoShaderBlock" (emitMsl irOnce)
      && length (emitSpirvWords irOnce) > 20

  -- 3. Algebraic fusion pass counts & semantics
  let grayTint = composeColor grayscale tint
      tripleColor = composeColor grayTint vignette
      doubleDistort = composeDistort waveDistort pixelate
      warpAndGray = distortThenColor waveDistort grayscale
  check failed "composeColor fuses into 1 GPU pass" (length (shaderPasses grayTint) == 1)
  check failed "triple composeColor fuses into 1 GPU pass" (length (shaderPasses tripleColor) == 1)
  check failed "composeDistort fuses into 1 GPU pass" (length (shaderPasses doubleDistort) == 1)
  check failed "distortThenColor fuses into 1 GPU pass" (length (shaderPasses warpAndGray) == 1)
  check failed "gaussianBlur uses 2 separable passes" (length (shaderPasses gaussianBlur) == 2)
  checkAllTargets failed "composeColor(grayscale, tint)" grayTint
  checkAllTargets failed "distortThenColor(wave, grayscale)" warpAndGray

  let srcCol = colorRGBA 200 100 0 255
      tp = TintParams (colorRGBA 0 0 255 255) 0.5
      fusedPx = evalAt grayTint (1.0, tp) (V2 50 50) (V2 100 100) (const srcCol)
      step1 = evalAt grayscale 1.0 (V2 50 50) (V2 100 100) (const srcCol)
      step2 = evalAt tint tp (V2 50 50) (V2 100 100) (const step1)
  check failed "fused composeColor matches sequential CPU evaluation" $
    nearByte (fromIntegral (colorR fusedPx)) (fromIntegral (colorR step2))
      && nearByte (fromIntegral (colorG fusedPx)) (fromIntegral (colorG step2))
      && nearByte (fromIntegral (colorB fusedPx)) (fromIntegral (colorB step2))
      && nearByte (fromIntegral (colorA fusedPx)) (fromIntegral (colorA step2))

  -- 4. Standard library compilation + SPIR-V validation + SVG opacity hook
  checkAllTargets failed "meshGradient4" meshGradient4
  checkAllTargets failed "radialGlow" radialGlow
  checkAllTargets failed "sdfRoundedBox" sdfRoundedBox
  checkAllTargets failed "colorWheelHsv" colorWheelHsv
  checkAllTargets failed "noiseGrain" noiseGrain
  checkAllTargets failed "checkerboard" checkerboard
  checkAllTargets failed "groupOpacity" groupOpacity
  checkAllTargets failed "grayscale" grayscale
  checkAllTargets failed "tint" tint
  checkAllTargets failed "brightnessContrast" brightnessContrast
  checkAllTargets failed "colorMatrix" colorMatrix
  checkAllTargets failed "vignette" vignette
  checkAllTargets failed "scanlines" scanlines
  checkAllTargets failed "waveDistort" waveDistort
  checkAllTargets failed "rippleDistort" rippleDistort
  checkAllTargets failed "pixelate" pixelate
  checkAllTargets failed "boxBlur" boxBlur
  checkAllTargets failed "gaussianBlur" gaussianBlur
  checkAllTargets failed "dropShadow" dropShadow
  checkAllTargets failed "outerGlow" outerGlow
  checkAllTargets failed "chromaticAberration" chromaticAberration

  check failed "groupOpacity provides shaderSvgOpacity" $
    case shaderSvgOpacity groupOpacity of
      Just f -> abs (f 0.4 - 0.4) < 1e-4
      Nothing -> False

  -- 5. Bounded loop CPU sanity: boxBlur of uniform green stays uniform green
  let green = colorRGBA 40 200 80 255
      blurred = evalAt boxBlur 4.0 (V2 50 50) (V2 100 100) (const green)
  check failed "boxBlur preserves constant color" $
    nearByte 40 (fromIntegral (colorR blurred))
      && nearByte 200 (fromIntegral (colorG blurred))
      && nearByte 80 (fromIntegral (colorB blurred))
      && nearByte 255 (fromIntegral (colorA blurred))

  let gBlurred = evalAt gaussianBlur 3.0 (V2 50 50) (V2 100 100) (const green)
  check failed "gaussianBlur preserves constant color" $
    nearByte 40 (fromIntegral (colorR gBlurred))
      && nearByte 200 (fromIntegral (colorG gBlurred))
      && nearByte 80 (fromIntegral (colorB gBlurred))
      && nearByte 255 (fromIntegral (colorA gBlurred))

  n <- readIORef failed
  when (n > 0) $ do
    putStrLn (show n ++ " check(s) failed.")
    exitFailure
  putStrLn "All nano-ui-shader checks passed."
