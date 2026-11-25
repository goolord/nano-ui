{-# OPTIONS_GHC -O0 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Core shader tests: std140 uniform packing, surface shader draw commands,
-- non-GPU fallback, paint-hash invalidation, and Tier 2 effect spans.
module Cases.Shader (tests) where

import Data.Int (Int32)
import Data.Primitive.ByteArray (ByteArray, indexByteArray, sizeofByteArray)
import Data.Primitive.SmallArray (indexSmallArray, sizeofSmallArray)
import Data.Proxy (Proxy (..))
import Data.Vector.Unboxed qualified as U
import Data.Word (Word8)
import GHC.Float (castWord32ToFloat)
import GHC.Generics (Generic)
import NanoUI.Internal.Shader (hashShaderProgram)
import NanoUI.Internal.Uniforms (packUniformBytes)
import Spec

tests :: [Spec]
tests =
  [ spec "shader-uniforms" runUniformsTest
  , spec "shader-surface-fallback" runSurfaceFallbackTest
  , spec "shader-surface-draw" runSurfaceDrawTest
  , spec "shader-damage" runShaderDamageTest
  , spec "shader-effect-span" runEffectSpanTest
  , spec "shader-effect-damage" runEffectDamageTest
  ]

data MixedUniforms = MixedUniforms
  { muSpeed :: !Float
  , muTint :: !V3
  , muPhase :: !Float
  , muBounds :: !Rect
  }
  deriving (Eq, Generic, Show)

instance Uniforms MixedUniforms

data NestedUniforms = NestedUniforms
  { nuTag :: !Int32
  , nuColor :: !Color
  , nuPos :: !V2
  }
  deriving (Eq, Generic, Show)

instance Uniforms NestedUniforms

readF32 :: ByteArray -> Int -> Float
readF32 ba off = castWord32ToFloat (indexByteArray ba (off `div` 4))

readI32 :: ByteArray -> Int -> Int32
readI32 ba off = indexByteArray ba (off `div` 4)

readU8 :: ByteArray -> Int -> Word8
readU8 = indexByteArray

nearF :: Float -> Float -> Bool
nearF a b = abs (a - b) < 1.0e-4

runUniformsTest :: Context -> IORef Int -> IO ()
runUniformsTest _ failed = do
  assertEq failed 0 (uniformSize (Proxy :: Proxy ()))
  assertEq failed 16 (uniformSize (Proxy :: Proxy Float))
  assertEq failed 16 (uniformSize (Proxy :: Proxy V2))
  assertEq failed 16 (uniformSize (Proxy :: Proxy V3))
  assertEq failed 16 (uniformSize (Proxy :: Proxy V4))
  assertEq failed 16 (uniformSize (Proxy :: Proxy Color))
  assertEq failed 16 (uniformSize (Proxy :: Proxy Rect))
  -- (Float, V3): Float at 0, V3 aligned to 16 (size 12), rounded to 32.
  assertEq failed 32 (uniformSize (Proxy :: Proxy (Float, V3)))
  -- (V3, Float): V3 at 0 (size 12), Float fits at 12 (size 4), total 16.
  assertEq failed 16 (uniformSize (Proxy :: Proxy (V3, Float)))

  -- MixedUniforms layout:
  -- muSpeed :: Float @ 0
  -- muTint  :: V3    @ 16 (12 bytes -> ends at 28)
  -- muPhase :: Float @ 28 (4 bytes -> ends at 32)
  -- muBounds:: Rect  @ 32 (16 bytes -> ends at 48)
  let fields = uniformLayout (Proxy :: Proxy MixedUniforms)
  assertEq
    failed
    [ UniformField "muSpeed" GFloat 0
    , UniformField "muTint" GVec3 16
    , UniformField "muPhase" GFloat 28
    , UniformField "muBounds" GVec4 32
    ]
    fields
  assertEq failed 48 (uniformSize (Proxy :: Proxy MixedUniforms))

  ba <- packUniformBytes (MixedUniforms 2.5 (V3 0.25 0.5 0.75) 1.25 (Rect 10 20 30 40))
  assertEq failed 48 (sizeofByteArray ba)
  assert failed (nearF 2.5 (readF32 ba 0))
  -- Padding bytes 4..15 must be zeroed.
  forM_ [4 .. 15] $ \i -> assertEq failed (0 :: Word8) (readU8 ba i)
  assert failed (nearF 0.25 (readF32 ba 16))
  assert failed (nearF 0.5 (readF32 ba 20))
  assert failed (nearF 0.75 (readF32 ba 24))
  assert failed (nearF 1.25 (readF32 ba 28))
  assert failed (nearF 10 (readF32 ba 32))
  assert failed (nearF 20 (readF32 ba 36))
  assert failed (nearF 30 (readF32 ba 40))
  assert failed (nearF 40 (readF32 ba 44))

  -- NestedUniforms: Int32 @ 0, Color (vec4) @ 16, V2 @ 32 -> size 48.
  nba <- packUniformBytes (NestedUniforms 7 (colorRGBA 255 0 128 255) (V2 3 4))
  assertEq failed 48 (sizeofByteArray nba)
  assertEq failed (7 :: Int32) (readI32 nba 0)
  assert failed (nearF 1.0 (readF32 nba 16))
  assert failed (nearF 0.0 (readF32 nba 20))
  assert failed (nearF (128 / 255) (readF32 nba 24))
  assert failed (nearF 1.0 (readF32 nba 28))
  assert failed (nearF 3.0 (readF32 nba 32))
  assert failed (nearF 4.0 (readF32 nba 36))

testSurfaceShader :: Shader 'Surface Float
testSurfaceShader =
  rawShader emptyShaderProgram {spGlsl150 = Just "// test surface\n"}

testColorEffect :: Shader 'ColorEffect Float
testColorEffect =
  rawShader emptyShaderProgram {spGlsl150 = Just "// test color effect\n", spNeedsTex = True}

runSurfaceFallbackTest :: Context -> IORef Int -> IO ()
runSurfaceFallbackTest ctx failed = do
  -- Default capabilities report no shader support.
  (caps, _, _) <- runFrame ctx (withInput 320 200) askShaderCapabilities
  assertEq failed noShaderCapabilities caps

  let fallback = colorRGBA 200 100 50 255
      cfg =
        defaultShaderConfig
          { scLayout = fixedWH 120 60
          , scCornerRadius = 8
          , scFallback = fallback
          }
  (_, dd, _) <- run2Frames ctx (withInput 320 200) (column (shaderConfigured cfg testSurfaceShader 0.5))
  assertEq failed 0 (sizeofSmallArray (drawShaderCalls dd))
  let cmds = U.toList (drawCommands dd)
  assert failed (not (null cmds))
  assert failed (all ((== 0) . cmdShader) cmds)

runSurfaceDrawTest :: Context -> IORef Int -> IO ()
runSurfaceDrawTest ctx failed = do
  setShaderCapabilities ctx (ShaderCapabilities True False)
  setDrawPixelScale ctx 2.0
  let mapped = contramapShader (* 2.0) testSurfaceShader
      fixed = fixUniform 3.5 mapped
      cfg =
        defaultShaderConfig
          { scLayout = fixedWH 100 50
          , scCornerRadius = 6
          }
  (r, dd, _) <- run2Frames ctx (withInput 320 200) (columnWith tight (shaderConfigured' cfg fixed ()))
  requireEq "shader call count" 1 (sizeofSmallArray (drawShaderCalls dd))
  let sc = indexSmallArray (drawShaderCalls dd) 0
      expectedHash = hashShaderProgram (scProgram sc)
  assertEq failed expectedHash (scHash sc)
  assertEq failed (sysUniformBytes + 16) (scUniformLength sc)
  assert failed (scUniformOffset sc + scUniformLength sc <= sizeofByteArray (drawUniformBytes dd))
  let ub = drawUniformBytes dd
      off = scUniformOffset sc
      Rect rx ry rw rh = respRect r
  -- SysUniforms: uRect at +0, uUvRect at +16, uMeta at +32.
  assert failed (nearF rx (readF32 ub off))
  assert failed (nearF ry (readF32 ub (off + 4)))
  assert failed (nearF rw (readF32 ub (off + 8)))
  assert failed (nearF rh (readF32 ub (off + 12)))
  assert failed (nearF 1.0 (readF32 ub (off + 24)))
  assert failed (nearF 1.0 (readF32 ub (off + 28)))
  assert failed (nearF 2.0 (readF32 ub (off + 32)))
  assert failed (nearF 6.0 (readF32 ub (off + 36)))
  -- User uniform: fixed 3.5 mapped through (* 2.0) = 7.0 at +sysUniformBytes.
  assert failed (nearF 7.0 (readF32 ub (off + sysUniformBytes)))
  let shaderCmds = filter ((> 0) . cmdShader) (U.toList (drawCommands dd))
  assertEq failed [1 :: Int32] (map cmdShader shaderCmds)

runShaderDamageTest :: Context -> IORef Int -> IO ()
runShaderDamageTest ctx failed = do
  setShaderCapabilities ctx (ShaderCapabilities True False)
  let ui t =
        columnWith (gap 8) $ do
          label "Header"
          shaderBox (fixedWH 80 40) testSurfaceShader t
      dmgOf t = runFrame ctx (withInput 320 200) (ui t) >> takeDamage ctx
  -- Frame 1: initial full paint.
  _ <- dmgOf 1.0
  -- Frame 2: identical uniform must produce zero damage (idle costs nothing).
  d2 <- dmgOf 1.0
  assert failed (damageIsEmpty d2)
  -- Frame 3: changing uniform must invalidate locally without full repaint.
  d3 <- dmgOf 2.0
  case d3 of
    DamageClip _ -> pure ()
    other -> putStrLn ("expected DamageClip on uniform change, got " ++ show other) >> bump failed
  -- Frame 4: settling at 2.0 must return to empty damage.
  d4 <- dmgOf 2.0
  assert failed (damageIsEmpty d4)

runEffectSpanTest :: Context -> IORef Int -> IO ()
runEffectSpanTest ctx failed = do
  let ui =
        columnWith (gap 8) $ do
          box (fixedWH 40 20) (colorRGBA 255 0 0 255)
          withShader testColorEffect 0.5 $
            box (fixedWH 60 30) (colorRGBA 0 255 0 255)
          box (fixedWH 40 20) (colorRGBA 0 0 255 255)

  -- Without capNodeEffects, no effect spans are emitted and children draw directly.
  setShaderCapabilities ctx (ShaderCapabilities True False)
  (_, ddOff, _) <- run2Frames ctx (withInput 320 200) ui
  assertEq failed 0 (sizeofSmallArray (drawEffectSpans ddOff))

  -- With capNodeEffects, one EffectSpan brackets the middle box without
  -- merging commands from the sibling boxes before or after it.
  setShaderCapabilities ctx (ShaderCapabilities True True)
  (_, ddOn, _) <- run2Frames ctx (withInput 320 200) ui
  requireEq "effect span count" 1 (sizeofSmallArray (drawEffectSpans ddOn))
  let es = indexSmallArray (drawEffectSpans ddOn) 0
  assertGt failed (esCmdEnd es) (esCmdStart es)
  assertEq failed 1 (sizeofSmallArray (esPasses es))
  assert failed (length (U.toList (drawCommands ddOn)) >= 3)

runEffectDamageTest :: Context -> IORef Int -> IO ()
runEffectDamageTest ctx failed = do
  setShaderCapabilities ctx (ShaderCapabilities True True)
  let blurProg =
        emptyShaderProgram
          { spGlsl150 = Just "#version 150\nout vec4 fragColor;\nvoid main(){ fragColor = vec4(1.0); }\n"
          , spNeedsTex = True
          }
      marginEffect :: Shader 'LayerEffect Float
      marginEffect = rawLayerShader (\m -> V2 m m) blurProg
      ui col =
        columnWith (padAll 40 . tight) $
          withShader marginEffect 12.0 $
            box (fixedWH 40 30) col
      red = colorRGBA 220 30 30 255
      green = colorRGBA 30 220 30 255
      step col = runFrame ctx (withInput 320 200) (ui col) >> takeDamage ctx
  _ <- step red
  -- Identical subtree + uniform: zero damage.
  dIdle <- step red
  assert failed (damageIsEmpty dIdle)
  -- Child change inside margin effect: damage rect expands by (12, 12) around (40, 40, 40, 30) -> covers [28..92] x [28..82].
  dDirty <- step green
  case dDirty of
    DamageClip (Rect dx dy dw dh) -> do
      assert failed (dx <= 28.5 && dy <= 28.5)
      assert failed (dx + dw >= 91.5 && dy + dh >= 81.5)
    other -> putStrLn ("expected margin-expanded DamageClip, got " ++ show other) >> bump failed
  -- Settles back to zero damage.
  dSettle <- step green
  assert failed (damageIsEmpty dSettle)
