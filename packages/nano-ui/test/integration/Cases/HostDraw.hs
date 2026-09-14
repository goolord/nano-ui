module Cases.HostDraw
  ( runSquareGeometryTest
  , runExternalTextTest
  ) where

import Control.Monad (forM, void)
import Data.IORef (IORef)
import Data.Word (Word32, Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekByteOff)
import NanoUI
import NanoUI.Context (setDrawExternalText, setDrawSquareGeometry)
import NanoUI.Testing
import NanoUI.Testing.Assert (assert, assertEq, withInput)

-- | Alpha of every vertex of every indexed triangle.
triangleAlphas :: DrawData -> IO [(Float, Float, Float)]
triangleAlphas dd =
  withForeignPtr (drawVertices dd) $ \vp ->
    withForeignPtr (drawIndices dd) $ \ip ->
      forM [0, 3 .. drawIndexCount dd - 3] $ \i -> do
        a <- alphaAt vp ip i
        b <- alphaAt vp ip (i + 1)
        c <- alphaAt vp ip (i + 2)
        pure (a, b, c)
  where
    alphaAt :: Ptr Word8 -> Ptr Word8 -> Int -> IO Float
    alphaAt vp ip i = do
      vi <- peekByteOff ip (i * indexSize) :: IO Word32
      peekByteOff vp (fromIntegral vi * vertexSize + 20)

controls :: NanoUI ()
controls = column $ do
  void (button "ok")
  void (checkbox "check" True)
  void (slider 0 1 0.5)
  void (button' "menu")

-- | Rounded fills and AA strokes carry transparent fringe vertices next to
-- opaque ones. Square geometry emits only flat primitives, so every triangle
-- has a uniform alpha.
runSquareGeometryTest :: Context -> IORef Int -> IO ()
runSquareGeometryTest ctx failed = do
  let inp = withInput 300 200
      uniform (a, b, c) = a == b && b == c
  (_, _, dRound, _) <- runFrame ctx inp controls
  roundTris <- triangleAlphas dRound
  assert failed (not (all uniform roundTris))
  setDrawSquareGeometry ctx True
  (_, _, dSquare, _) <- runFrame ctx inp controls
  squareTris <- triangleAlphas dSquare
  assert failed (not (null squareTris))
  assert failed (all uniform squareTris)
  setDrawSquareGeometry ctx False

-- | External text keeps text spans but pushes no text quads, so the buffer
-- does not grow with the label length.
runExternalTextTest :: Context -> IORef Int -> IO ()
runExternalTextTest ctx failed = do
  let inp = withInput 400 100
      ui txt = column (void (label txt))
  setDrawExternalText ctx True
  (_, _, dShort, _) <- runFrame ctx inp (ui "ab")
  (_, _, dLong, _) <- runFrame ctx inp (ui "abcdefghijklmnop")
  spans <- collectTextSpans ctx
  assertEq failed (drawVertexCount dLong) (drawVertexCount dShort)
  assert failed (any (\(_, t, _, _, _) -> t == "abcdefghijklmnop") spans)
  setDrawExternalText ctx False
