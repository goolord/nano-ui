{-# OPTIONS_GHC -O0 #-}
module Cases.Atlas (tests) where

import Spec
import Data.ByteString qualified as BS
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (plusPtr)
import NanoUI.Internal.Context (lookupImageUv)

tests :: [Spec]
tests =
  [ spec "atlas-growth" runAtlasGrowthTest
  , spec "atlas-changes" runAtlasChangesTest
  , spec "atlas-release" runAtlasReleaseTest
  ]

-- | Register a @w@ x @h@ image of one repeated pixel.
insert :: Context -> Int -> Int -> Int -> BS.ByteString -> IO Bool
insert ctx tid w h px = registerImage ctx (ImageId tid) w h (BS.concat (replicate (w * h) px))

solid :: Int -> BS.ByteString
solid v = BS.replicate 4 (fromIntegral v)

snapshot :: Context -> IO (Int, Int, ForeignPtr Word8, Int)
snapshot ctx = atlasSnapshot ctx >>= maybe (fail "missing atlas snapshot") pure

-- | The atlas pixel at @x@, @y@.
pixelAt :: Context -> Int -> Int -> IO BS.ByteString
pixelAt ctx x y = do
  (w, _, fp, _) <- snapshot ctx
  withForeignPtr fp $ \ptr -> BS.pack <$> peekArray 4 (ptr `plusPtr` ((y * w + x) * 4))

-- | An image's top-left and bottom-right pixel in the atlas.
corners :: Context -> Int -> IO (Maybe ((Int, Int), (Int, Int)))
corners ctx tid = do
  (w, h, _, _) <- snapshot ctx
  let at u v = (round (u * fromIntegral w), round (v * fromIntegral h))
  fmap (\(u0, v0, u1, v1) -> (at u0 v0, at u1 v1)) <$> lookupImageUv ctx (ImageId tid)

origin :: Context -> Int -> IO (Maybe (Int, Int))
origin ctx tid = fmap fst <$> corners ctx tid

-- | Growing and new shelves keep pixels; a same-size update stays put, and a
-- new size or an image no shelf holds is refused. Narrow images use the
-- allowed width rather than one strip.
runAtlasGrowthTest :: Context -> IORef Int -> IO ()
runAtlasGrowthTest ctx failed = do
  let (red, blue) = (BS.pack [255, 0, 0, 255], BS.pack [0, 0, 255, 255])
      checkPixel tid want = corners ctx tid >>= maybe (fail "missing image UV") (\((x0, y0), (x1, y1)) ->
        forM_ [(x, y) | x <- [x0, x1 - 1], y <- [y0, y1 - 1]] $ \(x, y) -> pixelAt ctx x y >>= assertEq failed want)
  insert ctx 1 2 2 red >>= assert failed
  initialUv <- lookupImageUv ctx (ImageId 1)
  assert failed . and =<< sequence [insert ctx 2 300 3 blue, insert ctx 3 400 1 red]
  grownUv <- lookupImageUv ctx (ImageId 1)
  assert failed (initialUv /= grownUv)
  mapM_ (uncurry checkPixel) [(1, red), (2, blue), (3, red)]
  insert ctx 1 2 2 blue >>= assert failed
  lookupImageUv ctx (ImageId 1) >>= assertEq failed grownUv
  mapM_ (uncurry checkPixel) [(1, blue), (2, blue)]
  before <- atlasSnapshot ctx
  insert ctx 1 3 2 red >>= assert failed . not
  atlasSnapshot ctx >>= assertEq failed before
  full <- newContext
  insert full 1 1 3000 (solid 255) >>= assert failed
  fullBefore <- atlasSnapshot full
  insert full 2 4094 1100 (solid 0) >>= assert failed . not
  atlasSnapshot full >>= assertEq failed fullBefore
  lookupImageUv full (ImageId 2) >>= assertEq failed Nothing
  wide <- newContext
  let go n = insert wide n 100 100 (solid 9) >>= \ok -> if ok && n <= 1000 then go (n + 1) else pure n
  placed <- go 1
  assertGt failed placed 900

-- | A texture of the atlas uploads only what was written since, unless the
-- atlas grew or the writes are too far back to know.
runAtlasChangesTest :: Context -> IORef Int -> IO ()
runAtlasChangesTest ctx failed = do
  let put tid w h = insert ctx tid w h (solid 7) >>= assert failed
      upload since want = (fmap (\(_, _, _, gen, u) -> (gen, u)) <$> atlasChanges ctx since) >>= assertEq failed want
  upload 0 Nothing
  put 1 8 8
  -- A texture that holds nothing takes the whole atlas.
  upload 0 (Just (1, AtlasWhole))
  upload 1 Nothing
  -- Pixels written in place, and a new image that fits, are rects.
  put 1 8 8
  upload 1 (Just (2, AtlasRegions [(1, 1, 8, 8)]))
  put 2 4 4
  upload 1 (Just (3, AtlasRegions [(10, 1, 4, 4), (1, 1, 8, 8)]))
  upload 2 (Just (3, AtlasRegions [(10, 1, 4, 4)]))
  -- An image wider than the atlas grows it: the texture must be remade.
  put 3 300 2
  upload 3 (Just (4, AtlasWhole))
  upload 4 Nothing
  -- Past what the log keeps, the writes are not known.
  replicateM_ 200 (put 2 4 4)
  upload 4 (Just (204, AtlasWhole))
  upload 203 (Just (204, AtlasRegions [(10, 1, 4, 4)]))

-- | A released slot goes to the next image that fits, padding cleared, and
-- merges back whole once a split-off image goes too; ids are not reused.
-- Releasing an image on screen repaints it as the placeholder.
runAtlasReleaseTest :: Context -> IORef Int -> IO ()
runAtlasReleaseTest ctx failed = do
  mapM_ (\(tid, w, v) -> insert ctx tid w w (solid v) >>= assert failed) [(1, 8, 200), (2, 4, 100)]
  Just (x, y) <- origin ctx 1
  sizeBefore <- (\(w, h, _, _) -> (w, h)) <$> snapshot ctx
  releaseImage ctx (ImageId 1)
  lookupImageUv ctx (ImageId 1) >>= assertEq failed Nothing
  insert ctx 3 6 6 (solid 50) >>= assert failed
  origin ctx 3 >>= assertEq failed (Just (x, y))
  (\(w, h, _, _) -> (w, h)) <$> snapshot ctx >>= assertEq failed sizeBefore
  forM_ [(x + 6, y), (x + 6, y + 5), (x, y + 6), (x + 6, y + 6)] $ \(px, py) -> pixelAt ctx px py >>= assertEq failed (solid 0)
  pixelAt ctx (x + 5) (y + 5) >>= assertEq failed (solid 50)
  (\(_, _, _, g) -> g) <$> snapshot ctx >>= atlasChanges ctx . subtract 1
    >>= assertEq failed (Just (AtlasRegions [(x - 1, y - 1, 8, 8)])) . fmap (\(_, _, _, _, u) -> u)
  -- The leftover strip is too thin for a 4x4 image, so it goes on a shelf.
  insert ctx 4 4 4 (solid 60) >>= assert failed
  origin ctx 4 >>= assert failed . (/= Just (x, y))
  releaseImage ctx (ImageId 4)
  fresh <- evalUi ctx (withInput 100 100) freshImageId
  assert failed (fresh > ImageId 4)
  merged <- newContext
  insert merged 1 100 100 (solid 9) >>= assert failed
  first <- origin merged 1
  releaseImage merged (ImageId 1)
  forM_ [(2, 60, 100, 3), (4, 99, 99, 5)] $ \(small, w, h, large) -> do
    insert merged small w h (solid 9) >>= assert failed
    releaseImage merged (ImageId small)
    insert merged large 100 100 (solid 9) >>= assert failed
    origin merged large >>= assertEq failed first
    releaseImage merged (ImageId large)
  shown <- newContext
  let inp = withInput 200 100
      ui = image (fixedWH 20 20) (ImageId 1)
  insert shown 1 2 2 (solid 255) >>= assert failed
  replicateM_ 3 (runFrame shown inp ui)
  assert failed . (/= DamageFull) =<< takeDamage shown
  clearDirty shown
  releaseImage shown (ImageId 7)
  assert failed . not =<< isDirty shown
  releaseImage shown (ImageId 1)
  assert failed =<< isDirty shown
  _ <- runFrame shown inp ui
  assertEq failed DamageFull =<< takeDamage shown
