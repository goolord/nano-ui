module Cases.Atlas (tests) where

import Spec
import Data.ByteString qualified as BS
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (plusPtr)
import NanoUI.Internal.Context (lookupImageUv, releaseImage)

tests :: [Spec]
tests =
  [ spec "atlas-growth" runAtlasGrowthTest
  , spec "atlas-changes" runAtlasChangesTest
  , spec "atlas-fills-width" runAtlasFillsWidthTest
  , spec "atlas-release-reuse" runAtlasReleaseReuseTest
  ]

-- | A released image draws no more, and the next image that fits takes its
-- room, with the padding round it cleared and uploaded; one that does not
-- fit goes on the shelves. Its id is not handed out again.
runAtlasReleaseReuseTest :: Context -> IORef Int -> IO ()
runAtlasReleaseReuseTest ctx failed = do
  let insert tid w h v = registerImage ctx (ImageId tid) w h (BS.replicate (w * h * 4) v)
      pixelAt x y = do
        (w, _, fp, _) <- atlasSnapshot ctx >>= maybe (fail "missing atlas snapshot") pure
        withForeignPtr fp $ \ptr -> BS.pack <$> peekArray 4 (ptr `plusPtr` ((y * w + x) * 4))
      origin tid = do
        (w, h, _, _) <- atlasSnapshot ctx >>= maybe (fail "missing atlas snapshot") pure
        fmap (\(u0, v0, _, _) -> (round (u0 * fromIntegral w), round (v0 * fromIntegral h))) <$> lookupImageUv ctx (ImageId tid)
  mapM_ (\(tid, w, v) -> insert tid w w v >>= assert failed) [(1, 8, 200), (2, 4, 100)]
  Just (x, y) <- origin 1
  sizeBefore <- fmap (\(w, h, _, _) -> (w, h)) <$> atlasSnapshot ctx
  releaseImage ctx (ImageId 1)
  lookupImageUv ctx (ImageId 1) >>= assertEq failed Nothing
  -- A smaller image takes the room, its padding cleared of the old pixels.
  insert 3 6 6 50 >>= assert failed
  origin 3 >>= assertEq failed (Just (x, y))
  fmap (\(w, h, _, _) -> (w, h)) <$> atlasSnapshot ctx >>= assertEq failed sizeBefore
  forM_ [(x + 6, y), (x + 6, y + 5), (x, y + 6), (x + 6, y + 6)] $ \(px, py) -> pixelAt px py >>= assertEq failed (BS.replicate 4 0)
  pixelAt (x + 5) (y + 5) >>= assertEq failed (BS.replicate 4 50)
  fmap (\(_, _, _, _, u) -> u) <$> (atlasChanges ctx . subtract 1 =<< maybe 0 (\(_, _, _, g) -> g) <$> atlasSnapshot ctx)
    >>= assertEq failed (Just (AtlasRegions [(x - 1, y - 1, 8, 8)]))
  -- What is left beside it is too thin for a 4 by 4 image, which goes on the shelves.
  insert 4 4 4 60 >>= assert failed
  origin 4 >>= assert failed . (/= Just (x, y))
  -- Released, an id is not handed out again.
  releaseImage ctx (ImageId 4)
  fresh <- evalUi ctx (withInput 100 100) freshImageId
  assert failed (fresh > ImageId 4)

-- | Narrow images spread across the atlas's allowed width instead of
-- stacking in one strip until the height limit.
runAtlasFillsWidthTest :: Context -> IORef Int -> IO ()
runAtlasFillsWidthTest ctx failed = do
  let px = BS.replicate (100 * 100 * 4) 9
      go n
        | n > 1000 = pure n
        | otherwise = do
            ok <- registerImage ctx (ImageId n) 100 100 px
            if ok then go (n + 1) else pure n
  placed <- go 1
  assertGt failed placed 900

-- | A texture of the atlas uploads only what was written since, unless the
-- atlas grew or the writes are too far back to know.
runAtlasChangesTest :: Context -> IORef Int -> IO ()
runAtlasChangesTest ctx failed = do
  let insert tid w h = registerImage ctx (ImageId tid) w h (BS.replicate (w * h * 4) 7)
      upload since = fmap (\(_, _, _, gen, u) -> (gen, u)) <$> atlasChanges ctx since
  upload 0 >>= assertEq failed Nothing
  insert 1 8 8 >>= assert failed
  -- A texture that holds nothing takes the whole atlas.
  upload 0 >>= assertEq failed (Just (1, AtlasWhole))
  upload 1 >>= assertEq failed Nothing
  -- Pixels written in place, and a new image that fits, are rects.
  insert 1 8 8 >>= assert failed
  upload 1 >>= assertEq failed (Just (2, AtlasRegions [(1, 1, 8, 8)]))
  insert 2 4 4 >>= assert failed
  upload 1 >>= assertEq failed (Just (3, AtlasRegions [(10, 1, 4, 4), (1, 1, 8, 8)]))
  upload 2 >>= assertEq failed (Just (3, AtlasRegions [(10, 1, 4, 4)]))
  -- An image wider than the atlas grows it: the texture must be remade.
  insert 3 300 2 >>= assert failed
  upload 3 >>= assertEq failed (Just (4, AtlasWhole))
  upload 4 >>= assertEq failed Nothing
  -- Past what the log keeps, the writes are not known.
  forM_ [1 .. 200 :: Int] $ \_ -> insert 2 4 4
  upload 4 >>= assertEq failed (Just (204, AtlasWhole))
  upload 203 >>= assertEq failed (Just (204, AtlasRegions [(10, 1, 4, 4)]))

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
