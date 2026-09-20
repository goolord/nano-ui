module Cases.SIMD (runSimdWritesTest, runDrawLayersTest) where

import Control.Monad (forM, forM_, void)
import Data.IORef (IORef, newIORef, modifyIORef', readIORef)
import Data.List (sort)
import Data.Primitive.PrimArray (primArrayToList)
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import Data.Word (Word32, Word8)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekByteOff)
import NanoUI.SIMD
import NanoUI (NanoUI, button, column, emptyInput, label, modal)
import NanoUI.Testing
  ( Context, DrawCmd (..), DrawData (..), Layer (..), drawCmdElems,
    forDrawCmdsInLayer_, newContext, runFrame )
import NanoUI.Testing.Assert (assertEq)

-- Check the renderer's interleaved vertex ABI, triangle winding and byte
-- offsets directly. Guard bytes also catch writes beyond either buffer range.
runSimdWritesTest :: Context -> IORef Int -> IO ()
runSimdWritesTest _ failed = do
  let
    solid vp ip = pokeQuadSIMD vp 16 ip 16 2 3 5 7 0 0.25 0.5 1 1 0 0.5 1 7
    gradient vp ip =
      pokeQuadGradientSIMD
        vp
        16
        ip
        16
        2
        3
        5
        7
        0.25
        0.5
        (1, 0, 0, 1)
        (0, 1, 0, 1)
        (0, 0, 1, 1)
        (1, 1, 1, 0.5)
        7
    solidVertices =
      [ [2, 3, 1, 0, 0.5, 1, 0, 0.25]
      , [7, 3, 1, 0, 0.5, 1, 0.5, 0.25]
      , [7, 10, 1, 0, 0.5, 1, 0.5, 1]
      , [2, 10, 1, 0, 0.5, 1, 0, 1]
      ]
    gradientVertices =
      [ [2, 3, 1, 0, 0, 1, 0.25, 0.5]
      , [7, 3, 0, 1, 0, 1, 0.25, 0.5]
      , [7, 10, 0, 0, 1, 1, 0.25, 0.5]
      , [2, 10, 1, 1, 1, 0.5, 0.25, 0.5]
      ]
    checkQuad :: (Ptr Word8 -> Ptr Word8 -> IO ()) -> [[Float]] -> IO ()
    checkQuad write expected =
      allocaBytes 160 $ \vp -> allocaBytes 56 $ \ip -> do
        fillBytes vp 0xa5 160
        fillBytes ip 0xa5 56
        write vp ip
        actual <- forM [0 .. 31 :: Int] $ \i -> peekByteOff vp (16 + i * 4)
        indices <- forM [0 .. 5 :: Int] $ \i -> peekByteOff ip (16 + i * 4)
        assertEq failed (concat expected) actual
        assertEq failed ([7, 8, 9, 7, 9, 10] :: [Word32]) indices
        forM_ [(vp, 144), (ip, 40)] $ \(ptr, end) -> do
          guardBytes <- mapM (peekByteOff ptr) ([0 .. 15] ++ [end .. end + 15])
          assertEq failed (replicate 32 0xa5 :: [Word8]) guardBytes
  checkQuad solid solidVertices
  checkQuad gradient gradientVertices
  let
    expectedOffsets = ((1, 2), (4, 6), (7, 10), (10, 14))
  assertEq failed expectedOffsets (concentricOffsetsSIMD 1 2 3 4 0 1 2 3)

-- Empty layers have equal bounds; grouping preserves command order within a
-- layer and visiting the layers reconstructs the complete command stream.
runDrawLayersTest :: Context -> IORef Int -> IO ()
runDrawLayersTest _ failed = do
  let commands = [DrawCmd 1 2 3 4 (-7) 19 maxBound layer | layer <- [minBound .. maxBound]]
      grown = U.create $ do
        v <- U.thaw (U.fromList commands)
        w <- UM.grow v (length commands)
        UM.copy (UM.drop (length commands) w) v
        pure w
  assertEq failed (commands ++ commands) (U.toList grown)
  let views :: [NanoUI ()]
      views = [pure (), label "content", column $ do
        void (button "outside")
        void (modal True "overlay" (button "inside"))]
  forM_ views $ \view -> do
    ctx <- newContext
    forM_ [1 .. 3 :: Int] $ \_ -> do
      (_, _, dd, _) <- runFrame ctx emptyInput view
      let cmds = drawCmdElems dd
          tags = map (fromEnum . cmdLayer) cmds
          offsets = primArrayToList (drawLayerOffsets dd)
      assertEq failed (sort tags) tags
      assertEq failed 5 (length offsets)
      assertEq failed [0] (take 1 offsets)
      assertEq failed [length cmds] (drop 4 offsets)
      visited <- forM [minBound .. maxBound :: Layer] $ \layer -> do
        ref <- newIORef []
        forDrawCmdsInLayer_ layer dd (\cmd -> modifyIORef' ref (cmd :))
        actual <- reverse <$> readIORef ref
        assertEq failed (filter ((== layer) . cmdLayer) cmds) actual
        assertEq failed (sort (map cmdIndexOffset actual)) (map cmdIndexOffset actual)
        pure actual
      assertEq failed cmds (concat visited)
