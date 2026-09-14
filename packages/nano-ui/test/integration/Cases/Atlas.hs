module Cases.Atlas (runAtlasGrowthTest) where

import Control.Monad (forM_)
import Data.ByteString qualified as BS
import Data.IORef (IORef)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (plusPtr)
import NanoUI (ImageId (..))
import NanoUI.Context (lookupImageUv)
import NanoUI.Testing (Context, atlasSnapshot, newContext, registerImage)
import NanoUI.Testing.Assert (assert, assertEq)

runAtlasGrowthTest :: Context -> IORef Int -> IO ()
runAtlasGrowthTest ctx failed = do
  let
    red = BS.pack [255, 0, 0, 255]
    blue = BS.pack [0, 0, 255, 255]
    pixels w h color = BS.concat (replicate (w * h) color)
    insert tid w h color = registerImage ctx (ImageId tid) w h (pixels w h color)
    checkPixel tid expected = do
      snapshot <- atlasSnapshot ctx >>= maybe (fail "missing atlas snapshot") pure
      uv <- lookupImageUv ctx (ImageId tid) >>= maybe (fail "missing image UV") pure
      let
        (w, h, fp, _) = snapshot
        (u0, v0, u1, v1) = uv
        xs = [round (u0 * fromIntegral w), round (u1 * fromIntegral w) - 1]
        ys = [round (v0 * fromIntegral h), round (v1 * fromIntegral h) - 1]
      forM_ [(x, y) | x <- xs, y <- ys] $ \(x, y) -> do
        actual <- withForeignPtr fp $ \ptr -> BS.pack <$> peekArray 4 (ptr `plusPtr` ((y * w + x) * 4))
        assertEq failed actual expected
  insert 1 2 2 red >>= assert failed
  initialUv <- lookupImageUv ctx (ImageId 1)
  -- A wider image grows the atlas; wrapping another image starts a new shelf.
  insert 2 300 3 blue >>= assert failed
  insert 3 400 1 red >>= assert failed
  grownUv <- lookupImageUv ctx (ImageId 1)
  assert failed (initialUv /= grownUv)
  checkPixel 1 red
  checkPixel 2 blue
  checkPixel 3 red
  -- Same-size updates retain their location and change only their pixels.
  insert 1 2 2 blue >>= assert failed
  lookupImageUv ctx (ImageId 1) >>= assertEq failed grownUv
  checkPixel 1 blue
  checkPixel 2 blue
  before <- atlasSnapshot ctx
  insert 1 3 2 red >>= assert failed . not
  atlasSnapshot ctx >>= assertEq failed before

  -- Width growth cannot rescue this insertion: neither the current shelf nor
  -- a new shelf can accommodate it within the atlas's maximum dimensions.
  full <- newContext
  registerImage full (ImageId 1) 1 3000 (BS.replicate (3000 * 4) 255)
    >>= assert failed
  fullBefore <- atlasSnapshot full
  registerImage full (ImageId 2) 4094 1100 (BS.replicate (4094 * 1100 * 4) 0)
    >>= assert failed . not
  atlasSnapshot full >>= assertEq failed fullBefore
  lookupImageUv full (ImageId 2) >>= assertEq failed Nothing
