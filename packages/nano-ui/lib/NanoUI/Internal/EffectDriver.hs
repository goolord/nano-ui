-- | Shared Tier 2 'EffectSpan' execution engine and uniform-header patcher for
-- GPU backends (SDL3, WebGL2, OpenGL 3.2).
--
-- Handles recursive span tree walking, damage culling, pixel-grid bounding,
-- UV calculation (both top-down and bottom-up FBO conventions), multi-pass
-- ping-pong target pooling, graceful fallback when a pass is unsupported or
-- target acquisition fails, and std140 system-uniform header patching.
module NanoUI.Internal.EffectDriver
  ( EffectPassDraw (..)
  , EffectDriver (..)
  , spanEffectiveClip
  , execEffectSpans
  , withEffectUniforms
  ) where

import Control.Exception (finally)
import Control.Monad (when)
import Data.Foldable (for_)
import Data.List (sortOn)
import Data.Primitive.ByteArray (ByteArray, copyByteArrayToAddr, sizeofByteArray)
import Data.Primitive.SmallArray (SmallArray, indexSmallArray, sizeofSmallArray)
import Data.Word (Word8)
import Foreign.C.Types (CFloat (..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (Ptr)
import Foreign.Storable (pokeByteOff)
import NanoUI.Internal.Draw.Types (EffectSpan (..), ShaderCall (..))
import NanoUI.Internal.Types (Rect (..), V2 (..), rectIntersect)
import NanoUI.Internal.Uniforms (sysUniformBytes)

-- | Geometry, UVs, and clipping state for compositing one Tier 2 shader pass.
data EffectPassDraw = EffectPassDraw
  { epdSrcSlot :: !Int
  , epdDstX :: !Float
  , epdDstY :: !Float
  , epdDstW :: !Float
  , epdDstH :: !Float
  , epdU0 :: !Float
  , epdV0 :: !Float
  , epdU1 :: !Float
  , epdV1 :: !Float
  , epdNodeU0 :: !Float
  , epdNodeV0 :: !Float
  , epdNodeUSpan :: !Float
  , epdNodeVSpan :: !Float
  , epdSrcCapW :: !Float
  , epdSrcCapH :: !Float
  , epdIntermediate :: !Bool
    -- ^ 'True' when writing to a pooled ping-pong target (replace blend);
    -- 'False' on the final composite into the parent target (alpha blend).
  , epdClip :: !(Maybe Rect)
    -- ^ Effective scissor rectangle shifted to the parent target's local
    -- logical coordinates; 'Nothing' means unclipped.
  }

-- | Backend callbacks plugging a GPU renderer into 'execEffectSpans'.
data EffectDriver passHandle = EffectDriver
  { edScale :: !Float
  , edFlipY :: !Bool
    -- ^ 'True' when offscreen FBO textures have @V=1@ at the top (OpenGL /
    -- WebGL2); 'False' when @V=0@ is at the top (SDL3).
  , edPreparePasses :: SmallArray ShaderCall -> IO (Maybe (SmallArray passHandle))
    -- ^ Validate or compile all passes for a span. Returning 'Nothing' falls
    -- back to rendering child commands directly into the parent target.
  , edAcquireTarget :: Int -> Int -> IO (Int, Int, Int)
    -- ^ Acquire a pooled offscreen target at least @(pw, ph)@ pixels; returns
    -- @(slot, capW, capH)@ or @(-1, 0, 0)@ on allocation failure.
  , edReleaseTarget :: Int -> IO ()
  , edBindTarget :: Int -> Bool -> Float -> Float -> IO Bool
    -- ^ Bind target @slot@ (@-1@ = root target) with @clear@ flag and logical
    -- origin @(ox, oy)@. Returns 'False' if binding failed.
  , edFlush :: IO ()
    -- ^ Flush any pending batched geometry before switching targets.
  , edEmitPlain :: Int -> Int -> Int -> Float -> Float -> Maybe Rect -> Int -> Int -> IO ()
    -- ^ Emit plain draw commands in @[from, to)@ onto the bound target
    -- @(slot, capW, capH, ox, oy, mDamage)@.
  , edAfterSpanContent :: Int -> IO ()
    -- ^ Hook invoked right after a span's child commands finish (used by
    -- external-text backends such as RGFW to emit text belonging to the span).
  , edCompositePass :: passHandle -> ShaderCall -> EffectPassDraw -> Int -> Int -> IO ()
    -- ^ Composite one pass onto the currently bound target of size @(dstW, dstH)@.
  }

-- | Combine a parent's optional damage rect with a span's logical clip rect.
-- Returns 'Nothing' when the intersection is empty (the span can be skipped),
-- @Just Nothing@ for unclipped, or @Just (Just r)@ for logical clip @r@.
spanEffectiveClip :: Maybe Rect -> Rect -> Maybe (Maybe Rect)
spanEffectiveClip mDamage clip@(Rect _ _ cw ch) =
  let !open = cw >= 1e8 || ch >= 1e8
   in case (mDamage, open) of
        (Nothing, True) -> Just Nothing
        (Nothing, False) -> Just (Just clip)
        (Just dmg, True) -> Just (Just dmg)
        (Just dmg, False) -> Just <$> rectIntersect dmg clip

-- | Recursively execute draw commands in @[rangeStart, rangeEnd)@ with Tier 2
-- 'EffectSpan' capture, multi-pass ping-pong shader execution, and compositing.
{-# NOINLINE execEffectSpans #-}
execEffectSpans ::
  EffectDriver passHandle ->
  SmallArray EffectSpan ->
  Int ->
  Int ->
  (Int, Int) ->
  Float ->
  Float ->
  Maybe Rect ->
  Int ->
  Int ->
  IO ()
execEffectSpans drv spans parentId parentSlot parentSize@(parentW, parentH) parentOx parentOy parentDmg rangeStart rangeEnd =
  go rangeStart directChildren
  where
    !scale = edScale drv
    !flipY = edFlipY drv

    directChildren =
      sortOn (\sp -> (esCmdStart sp, esId sp))
        [ sp
        | i <- [0 .. sizeofSmallArray spans - 1]
        , let sp = indexSmallArray spans i
        , esParent sp == parentId
        , esCmdStart sp >= rangeStart
        , esCmdEnd sp <= rangeEnd
        ]

    emitPlain from to =
      when (to > from) $
        edEmitPlain drv parentSlot parentW parentH parentOx parentOy parentDmg from to

    go !cur [] = emitPlain cur rangeEnd
    go !cur (sp : rest) = do
      let !s = max cur (esCmdStart sp)
          !e = min rangeEnd (esCmdEnd sp)
      when (s > cur) (emitPlain cur s)
      when (e > s) (runSpan sp s e)
      go (max cur e) rest

    fallbackSpan sp s e = do
      execEffectSpans drv spans (esId sp) parentSlot parentSize parentOx parentOy parentDmg s e
      edAfterSpanContent drv (esId sp)

    runSpan sp s e = do
      let Rect bx by bw bh = esBounds sp
          V2 mx my = esMargin sp
          expanded@(Rect ex ey ew eh) = Rect (bx - mx) (by - my) (bw + 2 * mx) (bh + 2 * my)
          passes = esPasses sp
          !nPasses = sizeofSmallArray passes
          inDamage = case parentDmg of
            Nothing -> True
            Just dmg -> case rectIntersect dmg expanded of
              Just _ -> True
              Nothing -> False
      when inDamage $ do
        mHandles <-
          if nPasses <= 0 || ew <= 0 || eh <= 0
            then pure Nothing
            else edPreparePasses drv passes
        case mHandles of
          Nothing -> fallbackSpan sp s e
          Just handles -> do
            let !px0 = floor (ex * scale) :: Int
                !py0 = floor (ey * scale) :: Int
                !px1 = ceiling ((ex + ew) * scale) :: Int
                !py1 = ceiling ((ey + eh) * scale) :: Int
                !pw = max 1 (px1 - px0)
                !ph = max 1 (py1 - py0)
                !ox = fromIntegral px0 / scale
                !oy = fromIntegral py0 / scale
            (slot0, cap0W, cap0H) <- edAcquireTarget drv pw ph
            if slot0 < 0
              then fallbackSpan sp s e
              else do
                let restoreParent = do
                      edFlush drv
                      _ <- edBindTarget drv parentSlot False parentOx parentOy
                      pure ()
                    runOffscreen = do
                      edFlush drv
                      okBind <- edBindTarget drv slot0 True ox oy
                      when okBind $ do
                        execEffectSpans drv spans (esId sp) slot0 (cap0W, cap0H) ox oy Nothing s e
                        edAfterSpanContent drv (esId sp)
                        edFlush drv
                        let !nodePx = bx * scale - fromIntegral px0
                            !nodePy = by * scale - fromIntegral py0
                            !nodePw = max 1.0e-4 (bw * scale)
                            !nodePh = max 1.0e-4 (bh * scale)
                            !destPx = fromIntegral px0 - parentOx * scale
                            !destPy = fromIntegral py0 - parentOy * scale
                            mkPassDraw !srcSlot !srcCapW !srcCapH !dx !dy !inter mClip =
                              let !fcw = fromIntegral srcCapW
                                  !fch = fromIntegral srcCapH
                                  !u1 = fromIntegral pw / fcw
                                  !v0 = if flipY then 1.0 else 0.0
                                  !v1 = if flipY then 1.0 - fromIntegral ph / fch else fromIntegral ph / fch
                                  !nodeU0 = nodePx / fcw
                                  !nodeV0 = if flipY then 1.0 - nodePy / fch else nodePy / fch
                                  !nodeUSpan = nodePw / fcw
                                  !nodeVSpan = if flipY then negate (nodePh / fch) else nodePh / fch
                               in EffectPassDraw
                                    { epdSrcSlot = srcSlot
                                    , epdDstX = dx
                                    , epdDstY = dy
                                    , epdDstW = fromIntegral pw
                                    , epdDstH = fromIntegral ph
                                    , epdU0 = 0
                                    , epdV0 = v0
                                    , epdU1 = u1
                                    , epdV1 = v1
                                    , epdNodeU0 = nodeU0
                                    , epdNodeV0 = nodeV0
                                    , epdNodeUSpan = nodeUSpan
                                    , epdNodeVSpan = nodeVSpan
                                    , epdSrcCapW = fcw
                                    , epdSrcCapH = fch
                                    , epdIntermediate = inter
                                    , epdClip = mClip
                                    }
                            runPasses !srcSlot !srcCapW !srcCapH !dstSlot !dstCapW !dstCapH !passIdx
                              | passIdx >= nPasses = pure ()
                              | otherwise = do
                                  let sc = indexSmallArray passes passIdx
                                      handle = indexSmallArray handles passIdx
                                  if passIdx < nPasses - 1 && dstSlot >= 0
                                    then do
                                      okDst <- edBindTarget drv dstSlot True 0 0
                                      when okDst $ do
                                        let pd = mkPassDraw srcSlot srcCapW srcCapH 0 0 True Nothing
                                        edCompositePass drv handle sc pd dstCapW dstCapH
                                        runPasses dstSlot dstCapW dstCapH srcSlot srcCapW srcCapH (passIdx + 1)
                                    else do
                                      okPar <- edBindTarget drv parentSlot False parentOx parentOy
                                      when okPar $
                                        for_ (spanEffectiveClip parentDmg (esClip sp)) $ \mClip -> do
                                          let shiftedClip =
                                                if parentOx == 0 && parentOy == 0
                                                  then mClip
                                                  else fmap (\(Rect x y w h) -> Rect (x - parentOx) (y - parentOy) w h) mClip
                                              pd = mkPassDraw srcSlot srcCapW srcCapH destPx destPy False shiftedClip
                                          edCompositePass drv handle sc pd parentW parentH
                        if nPasses > 1
                          then do
                            (slot1, cap1W, cap1H) <- edAcquireTarget drv pw ph
                            runPasses slot0 cap0W cap0H slot1 cap1W cap1H 0
                              `finally` when (slot1 >= 0) (edReleaseTarget drv slot1)
                          else
                            runPasses slot0 cap0W cap0H (-1) 0 0 0
                runOffscreen
                  `finally` (restoreParent >> edReleaseTarget drv slot0)

-- | Copy a 'ShaderCall''s packed std140 uniform block into a temporary buffer,
-- patch the per-pass @uUvRect@ (bytes 16..31) and @uMeta.zw@ target capacity
-- (bytes 40..47) from 'EffectPassDraw', and pass @(ptr, byteLen)@ to @k@.
withEffectUniforms ::
  ShaderCall ->
  ByteArray ->
  EffectPassDraw ->
  (Ptr Word8 -> Int -> IO a) ->
  IO a
withEffectUniforms sc uniformBytes pd k = do
  let !uOff = scUniformOffset sc
      !avail = max 0 (sizeofByteArray uniformBytes - uOff)
      !uLen = max sysUniformBytes (min (scUniformLength sc) avail)
      !copyLen = min uLen avail
  allocaBytes uLen $ \uPtr -> do
    when (copyLen < uLen) $ fillBytes uPtr 0 uLen
    when (copyLen > 0) $ copyByteArrayToAddr uPtr uniformBytes uOff copyLen
    pokeByteOff uPtr 16 (CFloat (epdNodeU0 pd))
    pokeByteOff uPtr 20 (CFloat (epdNodeV0 pd))
    pokeByteOff uPtr 24 (CFloat (epdNodeUSpan pd))
    pokeByteOff uPtr 28 (CFloat (epdNodeVSpan pd))
    pokeByteOff uPtr 40 (CFloat (epdSrcCapW pd))
    pokeByteOff uPtr 44 (CFloat (epdSrcCapH pd))
    k uPtr uLen
