-- | Context-owned RGBA image atlas with shelf packing and versioned pixel
-- snapshots. Space freed by 'releaseImage' is reused by later images that fit.
module NanoUI.Internal.Atlas
  ( ImageAtlas
  , newImageAtlas
  , atlasTextureId
  , registerImage
  , releaseImage
  , freshImageId
  , lookupImageUv
  , lookupImageSize
  , withImageSlot
  , AtlasToken
  , atlasToken
  , atlasSnapshot
  , AtlasUpload (..)
  , atlasChanges
  )
where

import Control.Applicative ((<|>))
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.IntMap.Strict qualified as IM
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import Foreign.Marshal.Utils (copyBytes, fillBytes)
import Foreign.Ptr (Ptr, plusPtr)
import NanoUI.Internal.Draw.Types (glyphAtlasPages, glyphPageTextureId)
import NanoUI.Internal.Store (ptrEq)
import NanoUI.Internal.Types (ImageId (..))

-- | GPU texture id shared by every packed image so draw cmds batch. It is
-- reserved, like the glyph atlas pages' ids above it
-- ('NanoUI.Internal.Draw.Types.glyphPageTextureId'), rather than a small
-- number: a drawing's image op names a registered 'ImageId' or a raw
-- texture, and 'freshImageId' starts at 1, so an unregistered image id must
-- not also name the atlas. It is the id below the last glyph page's, so more
-- pages move it down rather than onto a page.
atlasTextureId :: Int
atlasTextureId = glyphPageTextureId glyphAtlasPages

atlasPad :: Int
atlasPad = 1

atlasStart :: Int
atlasStart = 256

atlasMax :: Int
atlasMax = 4096

data AtlasSlot = AtlasSlot
  { slotX :: {-# UNPACK #-} !Int
  , slotY :: {-# UNPACK #-} !Int
  , slotW :: {-# UNPACK #-} !Int
  , slotH :: {-# UNPACK #-} !Int
  }

data AtlasState = AtlasState
  { asW :: {-# UNPACK #-} !Int
  , asH :: {-# UNPACK #-} !Int
  , asPtr :: ForeignPtr Word8
  , asSlots :: IM.IntMap AtlasSlot
  , asX :: {-# UNPACK #-} !Int
  , asY :: {-# UNPACK #-} !Int
  , asRowH :: {-# UNPACK #-} !Int
  , asGen :: {-# UNPACK #-} !Int
  , asLastFresh :: {-# UNPACK #-} !Int
  -- ^ The last id 'freshImageId' returned.
  , asResizedGen :: {-# UNPACK #-} !Int
  -- ^ Generation of the last atlas resize.
  , asWrites :: [AtlasSlot]
  -- ^ The slot each recent generation wrote (each writes one), newest first:
  -- at least the last 'atlasWriteLog', and fewer than twice as many.
  , asWriteCount :: {-# UNPACK #-} !Int
  , asFree :: [AtlasSlot]
  -- ^ Space freed by released images, including each one's trailing padding.
  -- 'takeFree' tries these before the shelves.
  }

newtype ImageAtlas = ImageAtlas (IORef AtlasState)

-- | The atlas as it is at one moment. Two tokens are equal only while no
-- image was registered or released between them: each write installs a new
-- state.
newtype AtlasToken = AtlasToken AtlasState

instance Eq AtlasToken where
  AtlasToken a == AtlasToken b = ptrEq a b

atlasToken :: ImageAtlas -> IO AtlasToken
atlasToken (ImageAtlas ref) = AtlasToken <$> readIORef ref

newImageAtlas :: IO ImageAtlas
newImageAtlas = do
  fp <- allocPixels atlasStart atlasStart
  ImageAtlas
    <$> newIORef
      AtlasState
        { asW = atlasStart
        , asH = atlasStart
        , asPtr = fp
        , asSlots = IM.empty
        , asX = atlasPad
        , asY = atlasPad
        , asRowH = 0
        , asGen = 0
        , asLastFresh = 0
        , asResizedGen = 0
        , asWrites = []
        , asWriteCount = 0
        , asFree = []
        }

registerImage :: ImageAtlas -> ImageId -> Int -> Int -> ByteString -> IO Bool
registerImage (ImageAtlas ref) (ImageId tid) w h pixels
  | tid <= 0 || w <= 0 || h <= 0 = pure False
  | w > atlasMax - 2 * atlasPad || h > atlasMax - 2 * atlasPad = pure False
  | BS.length pixels < w * h * 4 = pure False
  | otherwise = do
      st0 <- readIORef ref
      case IM.lookup tid (asSlots st0) of
        Just slot
          | slotW slot == w && slotH slot == h -> do
              blitPixels (asPtr st0) (asW st0) (slotX slot) (slotY slot) w h pixels
              writeIORef ref (recordWrite slot st0)
              pure True
          | otherwise -> pure False
        Nothing -> case takeFree st0 w h of
          Just (x, y, st1) -> True <$ (writeIORef ref =<< placeInFree st1 tid x y w h pixels)
          Nothing ->
            fitImage st0 tid w h pixels
              >>= maybe (pure False) (\st1 -> True <$ writeIORef ref st1)

-- | Remove an image. Its id stops drawing and its space goes on the free
-- list. The old pixels stay until another image overwrites them, and
-- 'freshImageId' never returns the id again. Unregistered ids are ignored.
-- Returns whether the id was registered.
releaseImage :: ImageAtlas -> ImageId -> IO Bool
releaseImage (ImageAtlas ref) (ImageId tid) = do
  st <- readIORef ref
  case IM.lookup tid (asSlots st) of
    Nothing -> pure False
    Just (AtlasSlot x y w h) -> do
      writeIORef
        ref
        st
          { asSlots = IM.delete tid (asSlots st)
          , asFree = mergeFree (AtlasSlot x y (w + atlasPad) (h + atlasPad)) (asFree st)
          , asLastFresh = max tid (asLastFresh st)
          }
      pure True

-- | Put @slot@ on the free list joined with the free slots it shares a whole
-- edge with, so space split up for smaller images is whole again once they
-- are all released.
mergeFree :: AtlasSlot -> [AtlasSlot] -> [AtlasSlot]
mergeFree slot@(AtlasSlot x y w h) frees = case break beside frees of
  (before, AtlasSlot fx fy fw fh : after)
    | fy == y -> mergeFree (AtlasSlot (min x fx) y (w + fw) h) (before ++ after)
    | otherwise -> mergeFree (AtlasSlot x (min y fy) w (h + fh)) (before ++ after)
  _ -> slot : frees
  where
    beside (AtlasSlot fx fy fw fh) =
      (fy == y && fh == h && (fx + fw == x || x + w == fx))
        || (fx == x && fw == w && (fy + fh == y || y + h == fy))

-- | Find the first free slot that fits a @w@ x @h@ image plus padding, and
-- return the image position and the updated state. The leftover space is
-- split into a strip to the right (image height plus padding) and a strip
-- below (full slot width). A strip too thin for any image is kept for
-- 'mergeFree' to join back.
takeFree :: AtlasState -> Int -> Int -> Maybe (Int, Int, AtlasState)
takeFree st w h = go [] (asFree st)
  where
    go _ [] = Nothing
    go seen (slot@(AtlasSlot fx fy fw fh) : rest)
      | w + atlasPad <= fw && h + atlasPad <= fh =
          let right = [AtlasSlot (fx + w + atlasPad) fy (fw - w - atlasPad) (h + atlasPad) | fw - w - atlasPad > 0]
              below = [AtlasSlot fx (fy + h + atlasPad) fw (fh - h - atlasPad) | fh - h - atlasPad > 0]
           in Just (fx, fy, st {asFree = foldl' (flip (:)) (right ++ below ++ rest) seen})
      | otherwise = go (slot : seen) rest

-- | Write an image into freed space at @x@ @y@ and zero the padding around
-- it. The previous image may have drawn there, and texture filtering at the
-- edge samples the padding. The recorded write includes the padding so the
-- upload clears it on the GPU too.
placeInFree :: AtlasState -> Int -> Int -> Int -> Int -> Int -> ByteString -> IO AtlasState
placeInFree st tid x y w h pixels = do
  blitPixels (asPtr st) (asW st) x y w h pixels
  withForeignPtr (asPtr st) $ \p -> do
    let clear px py n = fillBytes (p `plusPtr` ((py * asW st + px) * 4)) 0 (n * 4)
    clear (x - atlasPad) (y - atlasPad) (w + 2 * atlasPad)
    clear (x - atlasPad) (y + h) (w + 2 * atlasPad)
    forM_ [y .. y + h - 1] $ \row -> clear (x - atlasPad) row atlasPad >> clear (x + w) row atlasPad
  pure
    ( recordWrite
        (AtlasSlot (x - atlasPad) (y - atlasPad) (w + 2 * atlasPad) (h + 2 * atlasPad))
        st {asSlots = IM.insert tid (AtlasSlot x y w h) (asSlots st)}
    )

-- | An id above every registered image's and every id this returned before.
-- An id the app picks itself can still collide with one returned and not yet
-- registered, so register those first.
freshImageId :: ImageAtlas -> IO ImageId
freshImageId (ImageAtlas ref) = do
  st <- readIORef ref
  let tid = 1 + maybe (asLastFresh st) (max (asLastFresh st) . fst) (IM.lookupMax (asSlots st))
  writeIORef ref st {asLastFresh = tid}
  pure (ImageId tid)

lookupImageUv ::
  ImageAtlas -> ImageId -> IO (Maybe (Float, Float, Float, Float))
lookupImageUv atlas iid = withImageSlot atlas iid (pure Nothing) $ \_ _ u0 v0 u1 v1 -> pure (Just (u0, v0, u1, v1))

-- | A registered image's pixel size and atlas UV bounds, from one lookup,
-- passed to @k@; @none@ when the image is not registered.
{-# INLINE withImageSlot #-}
withImageSlot :: ImageAtlas -> ImageId -> IO r -> (Int -> Int -> Float -> Float -> Float -> Float -> IO r) -> IO r
withImageSlot (ImageAtlas ref) (ImageId tid) none k = do
  st <- readIORef ref
  case IM.lookup tid (asSlots st) of
    Nothing -> none
    Just (AtlasSlot x y w h) -> do
      let !fw = fromIntegral (asW st) :: Float
          !fh = fromIntegral (asH st) :: Float
          !u0 = fromIntegral x / fw
          !v0 = fromIntegral y / fh
          !u1 = fromIntegral (x + w) / fw
          !v1 = fromIntegral (y + h) / fh
      k w h u0 v0 u1 v1

-- | Pixel width and height of a registered image.
lookupImageSize :: ImageAtlas -> ImageId -> IO (Maybe (Int, Int))
lookupImageSize atlas iid = withImageSlot atlas iid (pure Nothing) $ \w h _ _ _ _ -> pure (Just (w, h))

-- | Writes 'asWrites' keeps: enough for a few frames of a few changing
-- images between two uploads.
atlasWriteLog :: Int
atlasWriteLog = 64

-- | Advance the generation for a write to @slot@, and log it. The log is cut
-- back to 'atlasWriteLog' only once it holds twice that, so a write copies
-- no list.
recordWrite :: AtlasSlot -> AtlasState -> AtlasState
recordWrite slot st
  | n < 2 * atlasWriteLog = st {asGen = gen, asWrites = logged, asWriteCount = n}
  -- The cut is forced: left lazy, it would keep the whole log behind it.
  | otherwise = let cut = take atlasWriteLog logged in length cut `seq` st {asGen = gen, asWrites = cut, asWriteCount = atlasWriteLog}
  where
    n = asWriteCount st + 1
    gen = asGen st + 1
    logged = slot : asWrites st

-- | Upload required to bring a GPU texture up to the current atlas generation.
data AtlasUpload
  = AtlasWhole
  -- ^ Upload the full buffer at the current atlas size (no texture yet,
  -- resized since @since@, or written since longer ago than the atlas's
  -- recent write log reaches).
  | AtlasRegions [(Int, Int, Int, Int)]
  -- ^ Upload only these @(x, y, width, height)@ pixel rects into the existing texture.
  deriving (Eq, Show)

-- | Current atlas dimensions, pixel buffer, generation, and the upload
-- required since generation @since@ (0 when uninitialized). 'Nothing' until
-- the first image is registered, and when the atlas is already at @since@.
atlasChanges :: ImageAtlas -> Int -> IO (Maybe (Int, Int, ForeignPtr Word8, Int, AtlasUpload))
atlasChanges (ImageAtlas ref) since = do
  st <- readIORef ref
  let gen = asGen st
      upload
        | since <= 0 || since > gen || since < asResizedGen st = AtlasWhole
        -- The log holds one write a generation, newest first.
        | gen - since > asWriteCount st = AtlasWhole
        | otherwise =
            AtlasRegions [(x, y, w, h) | AtlasSlot x y w h <- take (gen - since) (asWrites st)]
  pure $
    if gen == 0 || gen == since
      then Nothing
      else Just (asW st, asH st, asPtr st, gen, upload)

-- | The atlas's width, height, pinned pixels and generation; 'Nothing' until
-- the first image is registered. SDL uploads the pointer; do not copy to
-- ByteString first.
atlasSnapshot :: ImageAtlas -> IO (Maybe (Int, Int, ForeignPtr Word8, Int))
atlasSnapshot atlas = fmap (\(w, h, p, gen, _) -> (w, h, p, gen)) <$> atlasChanges atlas 0

fitImage ::
  AtlasState -> Int -> Int -> Int -> ByteString -> IO (Maybe AtlasState)
fitImage st0 tid w h pixels =
  -- Plan the shelf position before allocating or copying the atlas. A full
  -- atlas must reject an image without repeatedly allocating doomed growth.
  case cursorFor st0 w h <|> firstGrowth <|> secondGrowth of
    Nothing -> pure Nothing
    Just (x, y, placed) -> do
      let resized = asW placed /= asW st0 || asH placed /= asH st0
      fp <-
        if resized
          then do
            buf <- allocPixels (asW placed) (asH placed)
            withForeignPtr (asPtr st0) $ \sp ->
              withForeignPtr buf $ \dp -> copyRows dp (asW placed) 0 sp (asW st0) (asW st0) (asH st0)
            pure buf
          else pure (asPtr st0)
      blitPixels fp (asW placed) x y w h pixels
      let slot = AtlasSlot x y w h
          written = recordWrite slot placed
      pure $
        Just
          written
            { asPtr = fp
            , asSlots = IM.insert tid slot (asSlots placed)
            , asX = x + w + atlasPad
            , asY = y
            , asRowH = max (asRowH placed) h
            , asResizedGen = if resized then asGen written else asResizedGen placed
            }
 where
  -- Grow the shorter side first. Widening gives the current shelf room on
  -- its right and later shelves the full width; growing only taller would
  -- stack narrow images in one column until the height limit, with the rest
  -- of the allowed area unused.
  wider = st0 {asW = growDim (asW st0) (max (w + 2 * atlasPad) (asX st0 + w + atlasPad))}
  taller = st0 {asH = growDim (asH st0) (asY st0 + asRowH st0 + h + 2 * atlasPad)}
  both = taller {asW = asW wider}
  (firstGrowth, secondGrowth)
    | asW st0 <= asH st0 = (cursorFor wider w h, cursorFor taller w h <|> cursorFor both w h)
    | otherwise = (cursorFor taller w h, cursorFor wider w h <|> cursorFor both w h)

cursorFor :: AtlasState -> Int -> Int -> Maybe (Int, Int, AtlasState)
cursorFor st w h
  | asX st + w + atlasPad <= asW st && asY st + h + atlasPad <= asH st =
      Just (asX st, asY st, st)
  | asY st + asRowH st + atlasPad + h + atlasPad <= asH st
      && w + 2 * atlasPad <= asW st =
      let
        y = asY st + asRowH st + atlasPad
       in
        Just (atlasPad, y, st {asX = atlasPad, asY = y, asRowH = 0})
  | otherwise = Nothing

growDim :: Int -> Int -> Int
growDim cur need
  | need <= cur = cur
  | otherwise = min atlasMax (max need (cur * 2))

allocPixels :: Int -> Int -> IO (ForeignPtr Word8)
allocPixels w h = do
  let
    n = w * h * 4
  fp <- mallocForeignPtrBytes n
  withForeignPtr fp $ \p -> fillBytes p 0 n
  pure fp

blitPixels ::
  ForeignPtr Word8 -> Int -> Int -> Int -> Int -> Int -> ByteString -> IO ()
blitPixels dest destW destX destY w h pixels =
  withForeignPtr dest $ \dp ->
    BS.useAsCStringLen pixels $ \(sp, _) ->
      copyRows dp destW (destY * destW + destX) sp w w h

-- | Copy @rows@ rows of @w@ RGBA pixels from @src@, @srcW@ pixels a row, to
-- @dst@, @dstW@ pixels a row, starting @dstOff@ pixels in.
copyRows :: Ptr a -> Int -> Int -> Ptr b -> Int -> Int -> Int -> IO ()
copyRows dst dstW dstOff src srcW w rows =
  mapM_ copyRow [0 .. rows - 1]
 where
  copyRow row =
    copyBytes
      (dst `plusPtr` ((dstOff + row * dstW) * 4))
      (src `plusPtr` (row * srcW * 4))
      (w * 4)
