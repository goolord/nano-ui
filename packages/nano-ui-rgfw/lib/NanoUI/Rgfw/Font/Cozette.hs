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
  , scale2x
  , boxAverageCoverage
  , cozetteGlyphBit1x
  , cozetteGlyphBit2x
  , cozetteGlyphBit4x
  , renderGlyphToBuffer
  , renderGlyphScaledToBuffer
  , renderTextToBuffer
  , renderTextScaledToBuffer
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
import Data.Word (Word16, Word32, Word8)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekElemOff, pokeElemOff)
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
  { cfNumGlyphs   :: {-# UNPACK #-} !Int
  , cfGroups      :: !(V.Vector CmapGroup)
  , cfGlyphData   :: !(PrimArray Word8)  -- 921 * 12 bytes of packed 7x13 bitmap bits
  , cfGlyphData2x :: !(PrimArray Word16) -- 921 * 26 Word16s (14 bits per row, 26 rows per glyph)
  , cfGlyphData4x :: !(PrimArray Word32) -- 921 * 52 Word32s (28 bits per row, 52 rows per glyph)
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
  frozen1x <- unsafeFreezePrimArray mutArr
  frozen2x <- buildScale2xGlyphs numGlyphs frozen1x
  frozen4x <- buildScale4xGlyphs numGlyphs frozen2x
  pure $ CozetteFont numGlyphs (V.fromList groups) frozen1x frozen2x frozen4x

-- | Scale a 2D boolean grid using the Scale2x (AdvMAME2x / EPX) algorithm.
-- Given width W, height H, and a pixel query function (col -> row -> Bool),
-- returns a scaled pixel function (col -> row -> Bool) for width (W * 2) and height (H * 2).
scale2x :: Int -> Int -> (Int -> Int -> Bool) -> (Int -> Int -> Bool)
scale2x !w !h getPixel = \ !c2 !r2 ->
  if c2 < 0 || c2 >= w * 2 || r2 < 0 || r2 >= h * 2
    then False
    else
      let !c = c2 `div` 2
          !r = r2 `div` 2
          !subX = c2 .&. 1
          !subY = r2 .&. 1
          !b = if r > 0 then getPixel c (r - 1) else False
          !d = if c > 0 then getPixel (c - 1) r else False
          !e = getPixel c r
          !f = if c + 1 < w then getPixel (c + 1) r else False
          !h' = if r + 1 < h then getPixel c (r + 1) else False
       in if b /= h' && d /= f
            then case (subX, subY) of
              (0, 0) -> if d == b then d else e
              (1, 0) -> if b == f then f else e
              (0, 1) -> if d == h' then d else e
              _      -> if h' == f then f else e
            else e

{-# INLINE getGlyphBit1x #-}
getGlyphBit1x :: PrimArray Word8 -> Int -> Int -> Int -> Word8
getGlyphBit1x !arr !gid !c !r
  | c < 0 || c >= 7 || r < 0 || r >= 13 = 0
  | otherwise =
      let !bitIdx = r * 7 + c
          !byteIdx = gid * 12 + (bitIdx `shiftR` 3)
          !bitInByte = 7 - (bitIdx .&. 7)
          !b = indexPrimArray arr byteIdx
       in (b `shiftR` bitInByte) .&. 1

-- | Build Scale2x glyphs: 14x26 per glyph, packed into Word16 (14 bits per row).
buildScale2xGlyphs :: Int -> PrimArray Word8 -> IO (PrimArray Word16)
buildScale2xGlyphs !numGlyphs !arr = do
  mutArr2x <- newPrimArray (numGlyphs * 26)
  let forEachGlyph !gid
        | gid >= numGlyphs = pure ()
        | otherwise = do
            let forEachRow !r
                  | r >= 13 = pure ()
                  | otherwise = do
                      let buildRowPair !c !topAcc !botAcc
                            | c >= 7 = (topAcc, botAcc)
                            | otherwise =
                                let !b = getGlyphBit1x arr gid c (r - 1)
                                    !d = getGlyphBit1x arr gid (c - 1) r
                                    !e = getGlyphBit1x arr gid c r
                                    !f = getGlyphBit1x arr gid (c + 1) r
                                    !h = getGlyphBit1x arr gid c (r + 1)
                                    !(e0, e1, e2, e3) =
                                      if b /= h && d /= f
                                        then ( if d == b then d else e
                                             , if b == f then f else e
                                             , if d == h then d else e
                                             , if h == f then f else e
                                             )
                                        else (e, e, e, e)
                                    !c0 = 2 * c
                                    !c1 = c0 + 1
                                    !topBit0 = fromIntegral e0 `shiftL` (15 - c0)
                                    !topBit1 = fromIntegral e1 `shiftL` (15 - c1)
                                    !botBit0 = fromIntegral e2 `shiftL` (15 - c0)
                                    !botBit1 = fromIntegral e3 `shiftL` (15 - c1)
                                 in buildRowPair (c + 1) (topAcc .|. topBit0 .|. topBit1) (botAcc .|. botBit0 .|. botBit1)
                      let !(topRowWord, botRowWord) = buildRowPair 0 0 0
                      writePrimArray mutArr2x (gid * 26 + 2 * r) topRowWord
                      writePrimArray mutArr2x (gid * 26 + 2 * r + 1) botRowWord
                      forEachRow (r + 1)
            forEachRow 0
            forEachGlyph (gid + 1)
  forEachGlyph 0
  unsafeFreezePrimArray mutArr2x

{-# INLINE getGlyphBit2x #-}
getGlyphBit2x :: PrimArray Word16 -> Int -> Int -> Int -> Word8
getGlyphBit2x !arr2x !gid !c !r
  | c < 0 || c >= 14 || r < 0 || r >= 26 = 0
  | otherwise =
      let !w = indexPrimArray arr2x (gid * 26 + r)
       in fromIntegral ((w `shiftR` (15 - c)) .&. 1)

-- | Build Scale4x glyphs: 28x52 per glyph, packed into Word32 (28 bits per row).
buildScale4xGlyphs :: Int -> PrimArray Word16 -> IO (PrimArray Word32)
buildScale4xGlyphs !numGlyphs !arr2x = do
  mutArr4x <- newPrimArray (numGlyphs * 52)
  let forEachGlyph !gid
        | gid >= numGlyphs = pure ()
        | otherwise = do
            let forEachRow !r
                  | r >= 26 = pure ()
                  | otherwise = do
                      let buildRowPair !c !topAcc !botAcc
                            | c >= 14 = (topAcc, botAcc)
                            | otherwise =
                                let !b = getGlyphBit2x arr2x gid c (r - 1)
                                    !d = getGlyphBit2x arr2x gid (c - 1) r
                                    !e = getGlyphBit2x arr2x gid c r
                                    !f = getGlyphBit2x arr2x gid (c + 1) r
                                    !h = getGlyphBit2x arr2x gid c (r + 1)
                                    !(e0, e1, e2, e3) =
                                      if b /= h && d /= f
                                        then ( if d == b then d else e
                                             , if b == f then f else e
                                             , if d == h then d else e
                                             , if h == f then f else e
                                             )
                                        else (e, e, e, e)
                                    !c0 = 2 * c
                                    !c1 = c0 + 1
                                    !topBit0 = fromIntegral e0 `shiftL` (31 - c0)
                                    !topBit1 = fromIntegral e1 `shiftL` (31 - c1)
                                    !botBit0 = fromIntegral e2 `shiftL` (31 - c0)
                                    !botBit1 = fromIntegral e3 `shiftL` (31 - c1)
                                 in buildRowPair (c + 1) (topAcc .|. topBit0 .|. topBit1) (botAcc .|. botBit0 .|. botBit1)
                      let !(topRowWord, botRowWord) = buildRowPair 0 0 0
                      writePrimArray mutArr4x (gid * 52 + 2 * r) topRowWord
                      writePrimArray mutArr4x (gid * 52 + 2 * r + 1) botRowWord
                      forEachRow (r + 1)
            forEachRow 0
            forEachGlyph (gid + 1)
  forEachGlyph 0
  unsafeFreezePrimArray mutArr4x

-- | Query whether a pixel is set in the 1x glyph
cozetteGlyphBit1x :: CozetteFont -> Word32 -> Int -> Int -> Bool
cozetteGlyphBit1x font gid c r =
  let !safeGid = if fromIntegral gid < cfNumGlyphs font then fromIntegral gid else 0
   in getGlyphBit1x (cfGlyphData font) safeGid c r == 1

-- | Query whether a pixel is set in the Scale2x 2x glyph (width 14, height 26)
cozetteGlyphBit2x :: CozetteFont -> Word32 -> Int -> Int -> Bool
cozetteGlyphBit2x font gid c r
  | c < 0 || c >= 14 || r < 0 || r >= 26 = False
  | otherwise =
      let !safeGid = if fromIntegral gid < cfNumGlyphs font then fromIntegral gid else 0
          !w = indexPrimArray (cfGlyphData2x font) (safeGid * 26 + r)
       in (w `shiftR` (15 - c)) .&. 1 == 1

-- | Query whether a pixel is set in the Scale4x 4x glyph (width 28, height 52)
cozetteGlyphBit4x :: CozetteFont -> Word32 -> Int -> Int -> Bool
cozetteGlyphBit4x font gid c r
  | c < 0 || c >= 28 || r < 0 || r >= 52 = False
  | otherwise =
      let !safeGid = if fromIntegral gid < cfNumGlyphs font then fromIntegral gid else 0
          !w = indexPrimArray (cfGlyphData4x font) (safeGid * 52 + r)
       in (w `shiftR` (31 - c)) .&. 1 == 1

{-# INLINE charToGlyphId #-}
charToGlyphId :: CozetteFont -> Char -> Word32
charToGlyphId font c =
  let !cp = fromIntegral (ord c)
   in if cp >= 32 && cp <= 126
        then cp - 31 -- Fast ASCII path
        else case cp of
          0xf00d -> 57 -- FontAwesome times (\xf00d) -> 'X'
          0x00d7 -> 57 -- Multiplication sign (×, \xd7) -> 'X'
          0x2715 -> 57 -- Multiplication X (✕) -> 'X'
          0x2716 -> 57 -- Heavy multiplication X (✖) -> 'X'
          0xf078 -> 87 -- FontAwesome chevron-down (\xf078) -> 'v'
          0xf054 -> 31 -- FontAwesome chevron-right (\xf054) -> '>'
          0xf0d8 -> 63 -- FontAwesome caret-up (\xf0d8) -> '^'
          0xf0d7 -> 87 -- FontAwesome caret-down (\xf0d7) -> 'v'
          0xf046 -> 89 -- FontAwesome check-square (\xf046) -> 'x'
          0xf096 -> 1  -- FontAwesome square (\xf096) -> ' '
          1      -> 0  -- Control marker \x01 (closeButtonMarker) -> empty
          2      -> 0  -- Control marker \x02 (tabButtonMarker) -> empty
          3      -> 0  -- Control marker \x03 (tableHeaderMarker) -> empty
          _      -> binarySearch (cfGroups font) cp
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
renderGlyphToBuffer !dstPtr !stride !clipX0 !clipY0 !clipX1 !clipY1 !penX !penY !color !font !gid =
  renderGlyphScaledToBuffer dstPtr stride clipX0 clipY0 clipX1 clipY1 1.0 penX penY color font gid

{-# INLINE renderGlyphScaledToBuffer #-}
renderGlyphScaledToBuffer ::
  Ptr Word32 ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Float ->
  Int ->
  Int ->
  Word32 ->
  CozetteFont ->
  Word32 ->
  IO ()
renderGlyphScaledToBuffer !dstPtr !stride !clipX0 !clipY0 !clipX1 !clipY1 !scale !penX !penY !color !font !gid
  | scale <= 1.0 = do
      -- 1x fast path (original 7x13 bitmap)
      let !safeGid = if fromIntegral gid < cfNumGlyphs font then fromIntegral gid else 0
          !baseOff = safeGid * 12
          !arr = cfGlyphData font
          renderRow1x !r
            | r >= 13 = pure ()
            | otherwise = do
                let !y = penY + r
                if y >= clipY0 && y < clipY1
                  then do
                    let renderCol1x !c
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
                                  renderCol1x (c + 1)
                                else renderCol1x (c + 1)
                    renderCol1x 0
                    renderRow1x (r + 1)
                  else renderRow1x (r + 1)
      renderRow1x 0

  | abs (scale - 2.0) < 0.05 = do
      -- Exact 2x Scale2x fast path (14x26 bitmap)
      let !safeGid = if fromIntegral gid < cfNumGlyphs font then fromIntegral gid else 0
          !baseOff = safeGid * 26
          !arr2x = cfGlyphData2x font
          renderRow2x !r
            | r >= 26 = pure ()
            | otherwise = do
                let !y = penY + r
                if y >= clipY0 && y < clipY1
                  then do
                    let !rowWord = indexPrimArray arr2x (baseOff + r)
                        renderCol2x !c
                          | c >= 14 = pure ()
                          | otherwise = do
                              let !x = penX + c
                              if x >= clipX0 && x < clipX1
                                then do
                                  if (rowWord `shiftR` (15 - c)) .&. 1 == 1
                                    then pokeElemOff dstPtr (y * stride + x) color
                                    else pure ()
                                  renderCol2x (c + 1)
                                else renderCol2x (c + 1)
                    renderCol2x 0
                    renderRow2x (r + 1)
                  else renderRow2x (r + 1)
      renderRow2x 0

  | abs (scale - 4.0) < 0.05 = do
      -- Exact 4x Scale4x fast path (28x52 bitmap)
      let !safeGid = if fromIntegral gid < cfNumGlyphs font then fromIntegral gid else 0
          !baseOff = safeGid * 52
          !arr4x = cfGlyphData4x font
          renderRow4x !r
            | r >= 52 = pure ()
            | otherwise = do
                let !y = penY + r
                if y >= clipY0 && y < clipY1
                  then do
                    let !rowWord = indexPrimArray arr4x (baseOff + r)
                        renderCol4x !c
                          | c >= 28 = pure ()
                          | otherwise = do
                              let !x = penX + c
                              if x >= clipX0 && x < clipX1
                                then do
                                  if (rowWord `shiftR` (31 - c)) .&. 1 == 1
                                    then pokeElemOff dstPtr (y * stride + x) color
                                    else pure ()
                                  renderCol4x (c + 1)
                                else renderCol4x (c + 1)
                    renderCol4x 0
                    renderRow4x (r + 1)
                  else renderRow4x (r + 1)
      renderRow4x 0

  | scale < 2.0 = do
      -- Fractional scale < 2.0 (e.g. 0.75, 1.25, 1.33, 1.5, 1.75)
      -- Scaled with Scale2x to the next integer 2 (14x26 bitmap),
      -- then downscaled using Box / Area Averaging.
      let !safeGid = if fromIntegral gid < cfNumGlyphs font then fromIntegral gid else 0
          !baseOff = safeGid * 26
          !arr2x = cfGlyphData2x font
          !targetW = max 1 (round (7.0 * scale)) :: Int
          !targetH = max 1 (round (13.0 * scale)) :: Int
          !fSrcW = 14.0 :: Float
          !fSrcH = 26.0 :: Float
          !scaleX = fSrcW / fromIntegral targetW
          !scaleY = fSrcH / fromIntegral targetH
          !srcR = fromIntegral ((color `shiftR` 16) .&. 0xFF) :: Int
          !srcG = fromIntegral ((color `shiftR` 8) .&. 0xFF) :: Int
          !srcB = fromIntegral (color .&. 0xFF) :: Int
          !srcA = fromIntegral ((color `shiftR` 24) .&. 0xFF) :: Int

          renderRowFrac2x !dy
            | dy >= targetH = pure ()
            | otherwise = do
                let !y = penY + dy
                if y >= clipY0 && y < clipY1
                  then do
                    let !boxY0 = fromIntegral dy * scaleY
                        !boxY1 = fromIntegral (dy + 1) * scaleY
                        !syMin = max 0 (floor boxY0) :: Int
                        !syMax = min 25 (floor (boxY1 - 1e-5)) :: Int

                        renderColFrac2x !dx
                          | dx >= targetW = pure ()
                          | otherwise = do
                              let !x = penX + dx
                              if x >= clipX0 && x < clipX1
                                then do
                                  let !boxX0 = fromIntegral dx * scaleX
                                      !boxX1 = fromIntegral (dx + 1) * scaleX
                                      !boxArea = (boxX1 - boxX0) * (boxY1 - boxY0)
                                      !sxMin = max 0 (floor boxX0) :: Int
                                      !sxMax = min 13 (floor (boxX1 - 1e-5)) :: Int

                                      loopY !sy !accY
                                        | sy > syMax = accY
                                        | otherwise =
                                            let !rowWord = indexPrimArray arr2x (baseOff + sy)
                                                !y0 = fromIntegral sy :: Float
                                                !y1 = fromIntegral (sy + 1) :: Float
                                                !ovY = max 0.0 (min y1 boxY1 - max y0 boxY0)
                                                loopX !sx !accX
                                                  | sx > sxMax = accX
                                                  | otherwise =
                                                      let !isSet = (rowWord `shiftR` (15 - sx)) .&. 1 == 1
                                                          !x0 = fromIntegral sx :: Float
                                                          !x1 = fromIntegral (sx + 1) :: Float
                                                          !ovX = max 0.0 (min x1 boxX1 - max x0 boxX0)
                                                          !inc = if isSet then ovX * ovY else 0.0
                                                       in loopX (sx + 1) (accX + inc)
                                                !rowSum = loopX sxMin 0.0
                                             in loopY (sy + 1) (accY + rowSum)

                                      !cov = if boxArea > 0.0 then loopY syMin 0.0 / boxArea else 0.0
                                      !covClamped = max 0.0 (min 1.0 cov)
                                      !effA = round (fromIntegral srcA * covClamped) :: Int

                                  if effA <= 3
                                    then pure ()
                                    else if effA >= 252
                                      then pokeElemOff dstPtr (y * stride + x) color
                                      else do
                                        let !off = y * stride + x
                                        dst <- peekElemOff dstPtr off
                                        let !invA = 255 - effA
                                            !dstR = fromIntegral ((dst `shiftR` 16) .&. 0xFF) :: Int
                                            !dstG = fromIntegral ((dst `shiftR` 8) .&. 0xFF) :: Int
                                            !dstB = fromIntegral (dst .&. 0xFF) :: Int
                                            !outR = (srcR * effA + dstR * invA + 127) `div` 255
                                            !outG = (srcG * effA + dstG * invA + 127) `div` 255
                                            !outB = (srcB * effA + dstB * invA + 127) `div` 255
                                            !outColor = (0xFF `shiftL` 24)
                                                    .|. (fromIntegral outR `shiftL` 16)
                                                    .|. (fromIntegral outG `shiftL` 8)
                                                    .|. fromIntegral outB
                                        pokeElemOff dstPtr off outColor
                                  renderColFrac2x (dx + 1)
                                else renderColFrac2x (dx + 1)
                    renderColFrac2x 0
                    renderRowFrac2x (dy + 1)
                  else renderRowFrac2x (dy + 1)
      renderRowFrac2x 0

  | otherwise = do
      -- Fractional scale >= 2.0 (e.g. 2.25, 2.5, 2.75, 3.0, 3.5)
      -- Scaled with Scale2x twice to next integer power 4 (28x52 bitmap),
      -- then downscaled using Box / Area Averaging.
      let !safeGid = if fromIntegral gid < cfNumGlyphs font then fromIntegral gid else 0
          !baseOff = safeGid * 52
          !arr4x = cfGlyphData4x font
          !targetW = max 1 (round (7.0 * scale)) :: Int
          !targetH = max 1 (round (13.0 * scale)) :: Int
          !fSrcW = 28.0 :: Float
          !fSrcH = 52.0 :: Float
          !scaleX = fSrcW / fromIntegral targetW
          !scaleY = fSrcH / fromIntegral targetH
          !srcR = fromIntegral ((color `shiftR` 16) .&. 0xFF) :: Int
          !srcG = fromIntegral ((color `shiftR` 8) .&. 0xFF) :: Int
          !srcB = fromIntegral (color .&. 0xFF) :: Int
          !srcA = fromIntegral ((color `shiftR` 24) .&. 0xFF) :: Int

          renderRowFrac4x !dy
            | dy >= targetH = pure ()
            | otherwise = do
                let !y = penY + dy
                if y >= clipY0 && y < clipY1
                  then do
                    let !boxY0 = fromIntegral dy * scaleY
                        !boxY1 = fromIntegral (dy + 1) * scaleY
                        !syMin = max 0 (floor boxY0) :: Int
                        !syMax = min 51 (floor (boxY1 - 1e-5)) :: Int

                        renderColFrac4x !dx
                          | dx >= targetW = pure ()
                          | otherwise = do
                              let !x = penX + dx
                              if x >= clipX0 && x < clipX1
                                then do
                                  let !boxX0 = fromIntegral dx * scaleX
                                      !boxX1 = fromIntegral (dx + 1) * scaleX
                                      !boxArea = (boxX1 - boxX0) * (boxY1 - boxY0)
                                      !sxMin = max 0 (floor boxX0) :: Int
                                      !sxMax = min 27 (floor (boxX1 - 1e-5)) :: Int

                                      loopY !sy !accY
                                        | sy > syMax = accY
                                        | otherwise =
                                            let !rowWord = indexPrimArray arr4x (baseOff + sy)
                                                !y0 = fromIntegral sy :: Float
                                                !y1 = fromIntegral (sy + 1) :: Float
                                                !ovY = max 0.0 (min y1 boxY1 - max y0 boxY0)
                                                loopX !sx !accX
                                                  | sx > sxMax = accX
                                                  | otherwise =
                                                      let !isSet = (rowWord `shiftR` (31 - sx)) .&. 1 == 1
                                                          !x0 = fromIntegral sx :: Float
                                                          !x1 = fromIntegral (sx + 1) :: Float
                                                          !ovX = max 0.0 (min x1 boxX1 - max x0 boxX0)
                                                          !inc = if isSet then ovX * ovY else 0.0
                                                       in loopX (sx + 1) (accX + inc)
                                                !rowSum = loopX sxMin 0.0
                                             in loopY (sy + 1) (accY + rowSum)

                                      !cov = if boxArea > 0.0 then loopY syMin 0.0 / boxArea else 0.0
                                      !covClamped = max 0.0 (min 1.0 cov)
                                      !effA = round (fromIntegral srcA * covClamped) :: Int

                                  if effA <= 3
                                    then pure ()
                                    else if effA >= 252
                                      then pokeElemOff dstPtr (y * stride + x) color
                                      else do
                                        let !off = y * stride + x
                                        dst <- peekElemOff dstPtr off
                                        let !invA = 255 - effA
                                            !dstR = fromIntegral ((dst `shiftR` 16) .&. 0xFF) :: Int
                                            !dstG = fromIntegral ((dst `shiftR` 8) .&. 0xFF) :: Int
                                            !dstB = fromIntegral (dst .&. 0xFF) :: Int
                                            !outR = (srcR * effA + dstR * invA + 127) `div` 255
                                            !outG = (srcG * effA + dstG * invA + 127) `div` 255
                                            !outB = (srcB * effA + dstB * invA + 127) `div` 255
                                            !outColor = (0xFF `shiftL` 24)
                                                    .|. (fromIntegral outR `shiftL` 16)
                                                    .|. (fromIntegral outG `shiftL` 8)
                                                    .|. fromIntegral outB
                                        pokeElemOff dstPtr off outColor
                                  renderColFrac4x (dx + 1)
                                else renderColFrac4x (dx + 1)
                    renderColFrac4x 0
                    renderRowFrac4x (dy + 1)
                  else renderRowFrac4x (dy + 1)
      renderRowFrac4x 0

-- | Pure Box / Area Averaging calculation.
-- Computes the coverage [0.0 .. 1.0] of source grid (srcW x srcH) within
-- the continuous bounding box of destination pixel (dx, dy) in target grid (targetW x targetH).
{-# INLINE boxAverageCoverage #-}
boxAverageCoverage ::
  Int ->
  Int ->
  Int ->
  Int ->
  (Int -> Int -> Bool) ->
  Int ->
  Int ->
  Float
boxAverageCoverage !srcW !srcH !targetW !targetH isSet !dx !dy =
  let !fSrcW = fromIntegral srcW :: Float
      !fSrcH = fromIntegral srcH :: Float
      !fTgtW = fromIntegral targetW :: Float
      !fTgtH = fromIntegral targetH :: Float
      !scaleX = fSrcW / fTgtW
      !scaleY = fSrcH / fTgtH
      !boxX0 = fromIntegral dx * scaleX
      !boxX1 = fromIntegral (dx + 1) * scaleX
      !boxY0 = fromIntegral dy * scaleY
      !boxY1 = fromIntegral (dy + 1) * scaleY
      !boxArea = (boxX1 - boxX0) * (boxY1 - boxY0)
      !syMin = max 0 (floor boxY0)
      !syMax = min (srcH - 1) (floor (boxY1 - 1e-5))
      !sxMin = max 0 (floor boxX0)
      !sxMax = min (srcW - 1) (floor (boxX1 - 1e-5))

      loopY !sy !accY
        | sy > syMax = accY
        | otherwise =
            let !y0 = fromIntegral sy :: Float
                !y1 = fromIntegral (sy + 1) :: Float
                !ovY = max 0.0 (min y1 boxY1 - max y0 boxY0)
                loopX !sx !accX
                  | sx > sxMax = accX
                  | otherwise =
                      let !x0 = fromIntegral sx :: Float
                          !x1 = fromIntegral (sx + 1) :: Float
                          !ovX = max 0.0 (min x1 boxX1 - max x0 boxX0)
                          !inc = if isSet sx sy then ovX * ovY else 0.0
                       in loopX (sx + 1) (accX + inc)
                !rowSum = loopX sxMin 0.0
             in loopY (sy + 1) (accY + rowSum)

      !covered = loopY syMin 0.0
   in if boxArea > 0.0 then max 0.0 (min 1.0 (covered / boxArea)) else 0.0

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
  renderTextScaledToBuffer dstPtr stride clipX0 clipY0 clipX1 clipY1 1.0 (fromIntegral startX) (fromIntegral startY) color font txt

{-# INLINE renderTextScaledToBuffer #-}
renderTextScaledToBuffer ::
  Ptr Word32 ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Float ->
  Float ->
  Float ->
  Word32 ->
  CozetteFont ->
  Text ->
  IO ()
renderTextScaledToBuffer !dstPtr !stride !clipX0 !clipY0 !clipX1 !clipY1 !scale !logX !logY !color !font !txt =
  go (0 :: Int) (0 :: Int) (T.unpack txt)
  where
    go _ _ [] = pure ()
    go !_ !line ('\r' : cs) = go 0 line cs
    go !_ !line ('\n' : cs) = go 0 (line + 1) cs
    go !col !line (c : cs) = do
      let !gid = charToGlyphId font c
          !penX = round ((logX + (fromIntegral col :: Float) * 6.0) * scale)
          !penY = round ((logY + (fromIntegral line :: Float) * 13.0) * scale)
      renderGlyphScaledToBuffer dstPtr stride clipX0 clipY0 clipX1 clipY1 scale penX penY color font gid
      go (col + 1) line cs

cozetteMetrics :: FontMetrics
cozetteMetrics =
  FontMetrics
    { fmLineHeight = cozetteLineHeight
    , fmAscent = cozetteAscent
    , fmAdvance = \_ -> cozetteCharAdvance
    , fmGlyph = \_ -> Nothing
    }

