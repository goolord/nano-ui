{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}

module NanoUI.Rgfw.Font.Cozette
  ( CozetteFont (..)
  , getCozetteFont
  , cozetteMetrics
  , cozetteCharAdvance
  , cozetteLineHeight
  , cozetteAscent
  , cozetteGlyphWidth
  , cozetteGlyphHeight
  , charToGlyphId
  , renderGlyphToBuffer
  , renderTextToBuffer
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (ord)
import Data.FileEmbed (embedFileRelative)
import Data.Primitive.PrimArray
  ( PrimArray
  , indexPrimArray
  , newPrimArray
  , unsafeFreezePrimArray
  , writePrimArray
  )
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Word (Word32, Word8)
import Foreign.Ptr (Ptr)
import Foreign.Storable (pokeElemOff)
import GHC.IO (unsafePerformIO)
import NanoUI (FontMetrics (..))

-- 6x13 Cozette metrics
cozetteCharAdvance :: Float
cozetteCharAdvance = 6.0

cozetteLineHeight :: Float
cozetteLineHeight = 13.0

cozetteAscent :: Float
cozetteAscent = 10.0

cozetteGlyphWidth :: Int
cozetteGlyphWidth = 7

cozetteGlyphHeight :: Int
cozetteGlyphHeight = 13

-- Group record in cmap format 12
data CmapGroup = CmapGroup
  { cgStart :: {-# UNPACK #-} !Word32
  , cgEnd   :: {-# UNPACK #-} !Word32
  , cgGlyph :: {-# UNPACK #-} !Word32
  }
  deriving (Show)

data CozetteFont = CozetteFont
  { cfNumGlyphs :: {-# UNPACK #-} !Int
  , cfGroups    :: !(V.Vector CmapGroup)
  , cfGlyphData :: !(PrimArray Word8) -- 921 * 12 bytes of packed 7x13 bitmap bits
  }

{-# NOINLINE embeddedFontBytes #-}
embeddedFontBytes :: ByteString
embeddedFontBytes = $(embedFileRelative "data/cozette.min.otb")

{-# NOINLINE getCozetteFont #-}
getCozetteFont :: CozetteFont
getCozetteFont = parseCozette embeddedFontBytes

{-# INLINE parseWord16 #-}
parseWord16 :: ByteString -> Int -> Word32
parseWord16 bs off =
  let !b0 = fromIntegral (BS.index bs off)
      !b1 = fromIntegral (BS.index bs (off + 1))
   in (b0 `Data.Bits.shiftL` 8) .|. b1

{-# INLINE parseWord32 #-}
parseWord32 :: ByteString -> Int -> Word32
parseWord32 bs off =
  let !b0 = fromIntegral (BS.index bs off)
      !b1 = fromIntegral (BS.index bs (off + 1))
      !b2 = fromIntegral (BS.index bs (off + 2))
      !b3 = fromIntegral (BS.index bs (off + 3))
   in (b0 `Data.Bits.shiftL` 24)
        .|. (b1 `Data.Bits.shiftL` 16)
        .|. (b2 `Data.Bits.shiftL` 8)
        .|. b3

parseCozette :: ByteString -> CozetteFont
parseCozette bs = unsafePerformIO $ do
  -- Parse SFNT table directory
  let numTables = fromIntegral (parseWord16 bs 4)
      findTable tag i
        | i >= numTables = (0 :: Int, 0 :: Int)
        | otherwise =
            let off = 12 + i * 16
                tTag = BS.take 4 (BS.drop off bs)
                tOff = fromIntegral (parseWord32 bs (off + 8))
                tLen = fromIntegral (parseWord32 bs (off + 12))
             in if tTag == tag
                  then (tOff, tLen)
                  else findTable tag (i + 1)
      (cmapOff, _) = findTable "cmap" 0
      (ebdtOff, _) = findTable "EBDT" 0
      (eblcOff, _) = findTable "EBLC" 0

  -- Parse cmap format 12
  let subOff = fromIntegral (parseWord32 bs (cmapOff + 8))
      fmt12Off = cmapOff + subOff
      nGroups = fromIntegral (parseWord32 bs (fmt12Off + 12))
      groups =
        [ let gOff = fmt12Off + 16 + g * 12
              s = parseWord32 bs gOff
              e = parseWord32 bs (gOff + 4)
              gid = parseWord32 bs (gOff + 8)
           in CmapGroup s e gid
        | g <- [0 .. nGroups - 1]
        ]

  -- Parse EBLC subtable for glyph offsets
  let subArrayOff = fromIntegral (parseWord32 bs (eblcOff + 8))
      addOff = fromIntegral (parseWord32 bs (eblcOff + subArrayOff + 4))
      stOff = eblcOff + subArrayOff + addOff
      lastGlyph = fromIntegral (parseWord16 bs (eblcOff + subArrayOff + 2))
      numGlyphs = lastGlyph + 1
      imgDataOff = fromIntegral (parseWord32 bs (stOff + 4))

  -- Allocate glyph data buffer (numGlyphs * 12 bytes)
  mutArr <- newPrimArray (numGlyphs * 12)
  let loadGlyphs !g
        | g >= numGlyphs = pure ()
        | otherwise = do
            let off1 = fromIntegral (parseWord32 bs (stOff + 8 + g * 4))
                off2 = fromIntegral (parseWord32 bs (stOff + 8 + (g + 1) * 4))
                len = off2 - off1
                srcBase = ebdtOff + imgDataOff + off1
                copyBytes !b
                  | b >= 12 = pure ()
                  | b < len = do
                      let !byteVal = BS.index bs (srcBase + b)
                      writePrimArray mutArr (g * 12 + b) byteVal
                      copyBytes (b + 1)
                  | otherwise = do
                      writePrimArray mutArr (g * 12 + b) 0
                      copyBytes (b + 1)
            copyBytes 0
            loadGlyphs (g + 1)
  loadGlyphs 0
  frozen <- unsafeFreezePrimArray mutArr
  pure $ CozetteFont numGlyphs (V.fromList groups) frozen

{-# INLINE charToGlyphId #-}
charToGlyphId :: CozetteFont -> Char -> Word32
charToGlyphId font c =
  let !cp = fromIntegral (ord c)
   in if cp >= 32 && cp <= 126
        then cp - 31 -- Fast ASCII path
        else binarySearch (cfGroups font) cp
  where
    binarySearch grps cp = go 0 (V.length grps - 1)
      where
        go !lo !hi
          | lo > hi = 0
          | otherwise =
              let !mid = (lo + hi) `div` 2
                  grp = grps V.! mid
               in if cp < cgStart grp
                    then go lo (mid - 1)
                    else if cp > cgEnd grp
                      then go (mid + 1) hi
                      else cgGlyph grp + (cp - cgStart grp)

{-# INLINE renderGlyphToBuffer #-}
renderGlyphToBuffer ::
  Ptr Word32 ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Word32 ->
  CozetteFont ->
  Word32 ->
  IO ()
renderGlyphToBuffer !dstPtr !stride !clipX0 !clipY0 !clipX1 !clipY1 !penX !penY !color !font !gid = do
  let !safeGid = if fromIntegral gid < cfNumGlyphs font then fromIntegral gid else 0
      !baseOff = safeGid * 12
      !arr = cfGlyphData font
      renderRow !r
        | r >= 13 = pure ()
        | otherwise = do
            let !y = penY + r
            if y >= clipY0 && y < clipY1
              then do
                let renderCol !c
                      | c >= 7 = pure ()
                      | otherwise = do
                          let !x = penX + c
                          if x >= clipX0 && x < clipX1
                            then do
                              let !bitIdx = r * 7 + c
                                  !byteIdx = baseOff + (bitIdx `shiftR` 3)
                                  !bitInByte = 7 - (bitIdx .&. 7)
                                  !b = indexPrimArray arr byteIdx
                              if (b `shiftR` bitInByte) .&. 1 == 1
                                then pokeElemOff dstPtr (y * stride + x) color
                                else pure ()
                              renderCol (c + 1)
                            else renderCol (c + 1)
                renderCol 0
                renderRow (r + 1)
              else renderRow (r + 1)
  renderRow 0

{-# INLINE renderTextToBuffer #-}
renderTextToBuffer ::
  Ptr Word32 ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Word32 ->
  CozetteFont ->
  Text ->
  IO ()
renderTextToBuffer !dstPtr !stride !clipX0 !clipY0 !clipX1 !clipY1 !startX !startY !color !font !txt =
  go startX (T.unpack txt)
  where
    go _ [] = pure ()
    go !penX (c : cs) = do
      let !gid = charToGlyphId font c
      renderGlyphToBuffer dstPtr stride clipX0 clipY0 clipX1 clipY1 penX startY color font gid
      go (penX + 6) cs

cozetteMetrics :: FontMetrics
cozetteMetrics =
  FontMetrics
    { fmLineHeight = cozetteLineHeight
    , fmAscent = cozetteAscent
    , fmAdvance = \_ -> cozetteCharAdvance
    , fmGlyph = \_ -> Nothing
    }
