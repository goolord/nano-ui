module NanoUI.Atlas
  ( ImageAtlas
  , newImageAtlas
  , atlasTextureId
  , registerImage
  , lookupImageUv
  , atlasSnapshot
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.IntMap.Strict as IM
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import Foreign.Marshal.Utils (copyBytes, fillBytes)
import Foreign.Ptr (plusPtr)
import NanoUI.Types (ImageId (..))

-- | GPU texture id shared by every packed image so draw cmds batch.
atlasTextureId :: Int
atlasTextureId = 1

atlasPad :: Int
atlasPad = 1

atlasStart :: Int
atlasStart = 256

atlasMax :: Int
atlasMax = 4096

maxImageDim :: Int
maxImageDim = 8192

data AtlasSlot = AtlasSlot
  { slotX :: Int
  , slotY :: Int
  , slotW :: Int
  , slotH :: Int
  }

data AtlasState = AtlasState
  { asW :: Int
  , asH :: Int
  , asPtr :: ForeignPtr Word8
  , asSlots :: IM.IntMap AtlasSlot
  , asX :: Int
  , asY :: Int
  , asRowH :: Int
  , asGen :: Int
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
        }

registerImage :: ImageAtlas -> ImageId -> Int -> Int -> ByteString -> IO Bool
registerImage (ImageAtlas ref) (ImageId tid) w h pixels
  | tid <= 0 || w <= 0 || h <= 0 = pure False
  | w > maxImageDim || h > maxImageDim = pure False
  | BS.length pixels < w * h * 4 = pure False
  | otherwise = do
      st0 <- readIORef ref
      case IM.lookup tid (asSlots st0) of
        Just slot
          | slotW slot == w && slotH slot == h -> do
              blitPixels (asPtr st0) (asW st0) (slotX slot) (slotY slot) w h pixels
              writeIORef ref st0 {asGen = asGen st0 + 1}
              pure True
          | otherwise -> pure False
        Nothing -> do
          mSt <- fitImage st0 tid w h pixels
          case mSt of
            Nothing -> pure False
            Just st1 -> do
              writeIORef ref st1
              pure True

lookupImageUv :: ImageAtlas -> ImageId -> IO (Maybe (Float, Float, Float, Float))
lookupImageUv (ImageAtlas ref) (ImageId tid) = do
  st <- readIORef ref
  pure $
    case IM.lookup tid (asSlots st) of
      Nothing -> Nothing
      Just (AtlasSlot x y w h) ->
        let fw = fromIntegral (asW st)
            fh = fromIntegral (asH st)
         in Just
              ( fromIntegral x / fw
              , fromIntegral y / fh
              , fromIntegral (x + w) / fw
              , fromIntegral (y + h) / fh
              )

-- Pinned pixel buffer. SDL uploads this pointer; do not copy to ByteString first.
atlasSnapshot :: ImageAtlas -> IO (Maybe (Int, Int, ForeignPtr Word8, Int))
atlasSnapshot (ImageAtlas ref) = do
  st <- readIORef ref
  if asGen st == 0
    then pure Nothing
    else pure (Just (asW st, asH st, asPtr st, asGen st))

fitImage :: AtlasState -> Int -> Int -> Int -> ByteString -> IO (Maybe AtlasState)
fitImage st0 tid w h pixels = do
  st1 <- ensureCapacity st0 w h
  case cursorFor st1 w h of
    Nothing -> pure Nothing
    Just (x, y, st2) -> do
      blitPixels (asPtr st2) (asW st2) x y w h pixels
      let rowH = max (asRowH st2) h
      pure $
        Just
          st2
            { asSlots = IM.insert tid (AtlasSlot x y w h) (asSlots st2)
            , asX = x + w + atlasPad
            , asY = y
            , asRowH = rowH
            , asGen = asGen st2 + 1
            }

cursorFor :: AtlasState -> Int -> Int -> Maybe (Int, Int, AtlasState)
cursorFor st w h
  | asX st + w + atlasPad <= asW st && asY st + h + atlasPad <= asH st =
      Just (asX st, asY st, st)
  | asY st + asRowH st + atlasPad + h + atlasPad <= asH st && w + 2 * atlasPad <= asW st =
      let y = asY st + asRowH st + atlasPad
       in Just (atlasPad, y, st {asX = atlasPad, asY = y, asRowH = 0})
  | otherwise = Nothing

ensureCapacity :: AtlasState -> Int -> Int -> IO AtlasState
ensureCapacity st w h
  | fits st w h = pure st
  | otherwise = growUntil st w h

fits :: AtlasState -> Int -> Int -> Bool
fits st w h =
  case cursorFor st w h of
    Just _ -> True
    Nothing -> False

growUntil :: AtlasState -> Int -> Int -> IO AtlasState
growUntil st w h
  | w + 2 * atlasPad > atlasMax || h + 2 * atlasPad > atlasMax = pure st
  | otherwise = do
      let needW = max (asW st) (w + 2 * atlasPad)
          needH =
            max
              (asH st)
              (asY st + asRowH st + atlasPad + h + atlasPad)
          newW = min atlasMax (growDim (asW st) needW)
          newH = min atlasMax (growDim (asH st) needH)
      if newW == asW st && newH == asH st
        then pure st
        else do
          st' <- resizeAtlas st newW newH
          if fits st' w h
            then pure st'
            else growUntil st' w h

growDim :: Int -> Int -> Int
growDim cur need
  | need <= cur = cur
  | otherwise = max need (min atlasMax (cur * 2))

resizeAtlas :: AtlasState -> Int -> Int -> IO AtlasState
resizeAtlas st newW newH = do
  fp <- allocPixels newW newH
  copyAtlas (asPtr st) (asW st) (asH st) fp newW
  pure st {asW = newW, asH = newH, asPtr = fp}

allocPixels :: Int -> Int -> IO (ForeignPtr Word8)
allocPixels w h = do
  let n = w * h * 4
  fp <- mallocForeignPtrBytes n
  withForeignPtr fp $ \p -> fillBytes p 0 n
  pure fp

copyAtlas :: ForeignPtr Word8 -> Int -> Int -> ForeignPtr Word8 -> Int -> IO ()
copyAtlas src oldW oldH dst newW =
  withForeignPtr src $ \sp ->
    withForeignPtr dst $ \dp ->
      mapM_ (copyRow sp dp) [0 .. oldH - 1]
  where
    rowBytes = oldW * 4
    copyRow sp dp row =
      copyBytes
        (dp `plusPtr` (row * newW * 4))
        (sp `plusPtr` (row * oldW * 4))
        rowBytes

blitPixels :: ForeignPtr Word8 -> Int -> Int -> Int -> Int -> Int -> ByteString -> IO ()
blitPixels dest destW destX destY w h pixels =
  withForeignPtr dest $ \dp ->
    BS.useAsCStringLen pixels $ \(sp, _) ->
      mapM_ (copyRow dp sp) [0 .. h - 1]
  where
    copyRow dp sp row =
      copyBytes
        (dp `plusPtr` (((destY + row) * destW + destX) * 4))
        (sp `plusPtr` (row * w * 4))
        (w * 4)
