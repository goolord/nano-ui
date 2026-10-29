{-# OPTIONS_GHC -O0 #-}
module Cases.SIMD (tests) where

import Spec
import Data.List (sort)
import Data.Primitive.PrimArray (primArrayToList)
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import Data.Word (Word32, Word8)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekByteOff)
import NanoUI.Internal.SIMD

tests :: [Spec]
tests =
  [ spec "simd-writes" runSimdWritesTest
  , spec "draw-layer-offsets" runDrawLayersTest
  ]

-- | The renderer's interleaved vertex ABI, triangle winding and byte offsets,
-- checked directly. Guard bytes catch writes past either buffer's range.
runSimdWritesTest :: Context -> IORef Int -> IO ()
runSimdWritesTest _ failed = do
  let checkQuad :: (Ptr Word8 -> Ptr Word8 -> IO ()) -> [[Float]] -> IO ()
      checkQuad write expected =
        allocaBytes 160 $ \vp -> allocaBytes 56 $ \ip -> do
          fillBytes vp 0xa5 160
          fillBytes ip 0xa5 56
          write vp ip
          let words4 p n = forM [0 .. n - 1 :: Int] (\i -> peekByteOff p (16 + i * 4))
          assertEq failed (concat expected) =<< words4 vp 32
          assertEq failed ([7, 8, 9, 7, 9, 10] :: [Word32]) =<< words4 ip 6
          forM_ [(vp, 144), (ip, 40)] $ \(ptr, end) -> do
            guards <- mapM (peekByteOff ptr) ([0 .. 15] ++ [end .. end + 15])
            assertEq failed (replicate 32 0xa5 :: [Word8]) guards
  checkQuad
    (\vp ip -> pokeQuadSIMD vp 16 ip 16 2 3 5 7 0 0.25 0.5 1 1 0 0.5 1 7)
    [ [2, 3, 1, 0, 0.5, 1, 0, 0.25]
    , [7, 3, 1, 0, 0.5, 1, 0.5, 0.25]
    , [7, 10, 1, 0, 0.5, 1, 0.5, 1]
    , [2, 10, 1, 0, 0.5, 1, 0, 1]
    ]
  checkQuad
    ( \vp ip ->
        pokeQuadGradientSIMD vp 16 ip 16 2 3 5 7 0.25 0.5 (1, 0, 0, 1) (0, 1, 0, 1) (0, 0, 1, 1) (1, 1, 1, 0.5) 7
    )
    [ [2, 3, 1, 0, 0, 1, 0.25, 0.5]
    , [7, 3, 0, 1, 0, 1, 0.25, 0.5]
    , [7, 10, 0, 0, 1, 1, 0.25, 0.5]
    , [2, 10, 1, 1, 1, 0.5, 0.25, 0.5]
    ]
  assertEq failed ((1, 2), (4, 6), (7, 10), (10, 14)) (concentricOffsetsSIMD 1 2 3 4 0 1 2 3)

-- | Draw commands survive an unboxed vector growing. Empty layers have equal
-- bounds, grouping keeps command order within a layer, and visiting the
-- layers rebuilds the whole command stream.
runDrawLayersTest :: Context -> IORef Int -> IO ()
runDrawLayersTest _ failed = do
  let commands = [DrawCmd 1 2 3 4 (-7) 19 maxBound layer | layer <- [minBound .. maxBound]]
      grown = U.create $ do
        v <- U.thaw (U.fromList commands)
        w <- UM.grow v (length commands)
        UM.copy (UM.drop (length commands) w) v
        pure w
  assertEq failed (commands ++ commands) (U.toList grown)
  let overlay = column (button "outside" >> void (modal' (constVar True) "overlay" (button "inside")))
  forM_ [pure (), label "content", overlay] $ \view -> do
    ctx <- newContext
    replicateM_ 3 $ do
      (_, dd, _) <- runFrame ctx emptyInput view
      let cmds = drawCmdElems dd
          tags = map (fromEnum . cmdLayer) cmds
          offsets = primArrayToList (drawLayerOffsets dd)
      assertEq failed (sort tags) tags
      assertEq failed (5, [0], [length cmds]) (length offsets, take 1 offsets, drop 4 offsets)
      visited <- forM [minBound .. maxBound :: Layer] $ \layer -> do
        ref <- newIORef []
        forDrawCmdsInLayer_ layer dd (\cmd -> modifyIORef' ref (cmd :))
        actual <- reverse <$> readIORef ref
        assertEq failed (filter ((== layer) . cmdLayer) cmds) actual
        assertEq failed (sort (map cmdIndexOffset actual)) (map cmdIndexOffset actual)
        pure actual
      assertEq failed cmds (concat visited)
