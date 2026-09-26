-- | Context-owned RGBA image atlas with shelf packing and versioned pixel
-- snapshots. Room an image lets go ('releaseImage') is reused for the next
-- images that fit it.
module NanoUI.Internal.Atlas
  ( ImageAtlas
  , newImageAtlas
  , atlasTextureId
  , registerImage
  , releaseImage
  , freshImageId
  , lookupImageUv
  , lookupImageSize
  , atlasSnapshot
  , AtlasUpload (..)
  , atlasChanges
  )
where

import Control.Applicative ((<|>))
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Functor ((<&>))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.IntMap.Strict qualified as IM
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import Foreign.Marshal.Utils (copyBytes, fillBytes)
import Foreign.Ptr (Ptr, plusPtr)
import NanoUI.Internal.Types (ImageId (..))

-- | GPU texture id shared by every packed image so draw cmds batch.
atlasTextureId :: Int
atlasTextureId = 1

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
  -- ^ The generation that last gave the atlas a new size.
  , asWrites :: [AtlasSlot]
  -- ^ The slot each recent generation wrote (each writes one), newest first:
  -- at least the last 'atlasWriteLog', and fewer than twice as many.
  , asWriteCount :: {-# UNPACK #-} !Int
  -- ^ The length of 'asWrites'.
  , asFree :: [AtlasSlot]
  -- ^ Room that released images let go, each with the padding after it,
  -- which a new image that fits takes before the shelves ('takeFree').
  }

newtype ImageAtlas = ImageAtlas (IORef AtlasState)

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

-- | Take an image out of the atlas: its id no longer draws, and its room
-- goes to the next images that fit it. Its pixels stay where they were
-- until one does, and 'freshImageId' does not hand its id out again. An id
-- that is not registered is left alone.
releaseImage :: ImageAtlas -> ImageId -> IO ()
releaseImage (ImageAtlas ref) (ImageId tid) = do
  st <- readIORef ref
  forM_ (IM.lookup tid (asSlots st)) $ \(AtlasSlot x y w h) ->
    writeIORef
      ref
      st
        { asSlots = IM.delete tid (asSlots st)
        , asFree = AtlasSlot x y (w + atlasPad) (h + atlasPad) : asFree st
        , asLastFresh = max tid (asLastFresh st)
        }

-- | Where the first free room an image @w@ by @h@ fits in, with its
-- padding, puts it, and the atlas with the rest of that room still free:
-- the part right of the image, as tall as the image and its padding, and
-- the part below, as wide as the room. A part too thin for any image is
-- dropped.
takeFree :: AtlasState -> Int -> Int -> Maybe (Int, Int, AtlasState)
takeFree st w h = go [] (asFree st)
  where
    go _ [] = Nothing
    go seen (slot@(AtlasSlot fx fy fw fh) : rest)
      | w + atlasPad <= fw && h + atlasPad <= fh =
          let right = [AtlasSlot (fx + w + atlasPad) fy (fw - w - atlasPad) (h + atlasPad) | fw - w - atlasPad > atlasPad]
              below = [AtlasSlot fx (fy + h + atlasPad) fw (fh - h - atlasPad) | fh - h - atlasPad > atlasPad]
           in Just (fx, fy, st {asFree = foldl' (flip (:)) (right ++ below ++ rest) seen})
      | otherwise = go (slot : seen) rest

-- | Put an image in room another let go, at @x@ @y@, and clear the
-- padding round it: the image before may have drawn there, and a texture
-- filtered at the image's edge reads it. The write covers the padding, so
-- a texture takes it too.
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
lookupImageUv (ImageAtlas ref) (ImageId tid) = do
  st <- readIORef ref
  let fw = fromIntegral (asW st)
      fh = fromIntegral (asH st)
  pure $
    IM.lookup tid (asSlots st) <&> \(AtlasSlot x y w h) ->
      ( fromIntegral x / fw
      , fromIntegral y / fh
      , fromIntegral (x + w) / fw
      , fromIntegral (y + h) / fh
      )

-- | The width and height in pixels of the image registered under an id.
lookupImageSize :: ImageAtlas -> ImageId -> IO (Maybe (Int, Int))
lookupImageSize (ImageAtlas ref) (ImageId tid) =
  fmap (\slot -> (slotW slot, slotH slot)) . IM.lookup tid . asSlots <$> readIORef ref

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
  | otherwise = st {asGen = gen, asWrites = take atlasWriteLog logged, asWriteCount = atlasWriteLog}
  where
    n = asWriteCount st + 1
    gen = asGen st + 1
    logged = slot : asWrites st

-- | What a texture that holds the atlas as of some generation must upload to
-- hold it as of now.
data AtlasUpload
  = AtlasWhole
  -- ^ All of it, into a texture of the atlas's current size: the atlas was
  -- resized since, the writes since are forgotten, or there is no texture.
  | AtlasRegions [(Int, Int, Int, Int)]
  -- ^ Only these x, y, width, height pixel rects, into the same texture.
  deriving (Eq, Show)

-- | The atlas's width, height, pixels and generation, and what a texture
-- uploaded at generation @since@ (0 for none) needs from them. 'Nothing'
-- when it needs nothing, or the atlas holds no image.
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

-- | The atlas's width, height, pinned pixels and generation; 'Nothing' while
-- it holds no image. SDL uploads the pointer; do not copy to ByteString first.
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
