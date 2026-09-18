{-# LANGUAGE TemplateHaskell #-}

-- | The bundled Cozette bitmap font: glyph lookup, metrics, and glyph bitmaps
-- at 1x, 2x and 4x (EPX-scaled), box-averaged for other scales.
module NanoUI.Rgfw.Font.Cozette
  ( CozetteFont (..)
  , getCozetteFont
  , cozetteMetrics
  , cozetteCharAdvance
  , cozetteLineHeight
  , cozetteAscent
  , cozetteGlyphWidth
  , cozetteGlyphHeight
  , CozetteScalePath (..)
  , cozetteScalePath
  , cozetteGlyphFootprint
  , charToGlyphId
  , cozetteGlyphBit1x
  , cozetteGlyphBit2x
  , cozetteGlyphBit4x
  , renderGlyphScaledToBuffer
  , foldPenPositions
  ) where

import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Data.Bits (Bits, setBit, shiftL, shiftR, testBit, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (ord)
import Data.FileEmbed (embedFileRelative)
import Data.Primitive.PrimArray
  ( PrimArray
  , indexPrimArray
  , primArrayFromList
  , sizeofPrimArray
  , newPrimArray
  , unsafeFreezePrimArray
  , writePrimArray
  )
import Data.Primitive.Types (Prim)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word16, Word32, Word8)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekElemOff, pokeElemOff)
import NanoUI (FontMetrics (..), roundHalfUp)

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

data CozetteFont = CozetteFont
  { cfNumGlyphs   :: {-# UNPACK #-} !Int
  , cfGroups      :: !(PrimArray Word32) -- start, end, glyph: three a group
  , cfGlyphData   :: !(PrimArray Word8)  -- 921 * 12 bytes of packed 7x13 bitmap bits
  , cfGlyphData1x :: !(PrimArray Word8)  -- 921 * 13 bytes of row-unpacked Word8s (1 byte per row)
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
parseCozette bs = runST $ do
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
           in (s, e, gid)
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
  frozen1xRows <- build1xRowGlyphs numGlyphs frozen1x
  frozen2x <- buildEpxTable numGlyphs 7 13 15 (getGlyphBit1x frozen1x)
  frozen4x <- buildEpxTable numGlyphs 14 26 31 (getGlyphBit2x frozen2x)
  pure $ CozetteFont numGlyphs (primArrayFromList (concat [[start, end, glyph] | (start, end, glyph) <- groups])) frozen1x frozen1xRows frozen2x frozen4x

-- | Build unpacked 1x glyphs: 13 bytes per glyph (1 byte per row, bit (7 - c) for col c).
build1xRowGlyphs :: Int -> PrimArray Word8 -> ST s (PrimArray Word8)
build1xRowGlyphs !numGlyphs !arr = do
  mutArr1x <- newPrimArray (numGlyphs * 13)
  let forEachGlyph !gid
        | gid >= numGlyphs = pure ()
        | otherwise = do
            let forEachRow !r
                  | r >= 13 = pure ()
                  | otherwise = do
                      let buildRow !c !acc
                            | c >= 7 = acc
                            | otherwise =
                                let !bit = getGlyphBit1x arr gid c r
                                    !bitVal = bit `shiftL` (7 - c)
                                 in buildRow (c + 1) (acc .|. bitVal)
                      let !rowByte = buildRow 0 0
                      writePrimArray mutArr1x (gid * 13 + r) rowByte
                      forEachRow (r + 1)
            forEachRow 0
            forEachGlyph (gid + 1)
  forEachGlyph 0
  unsafeFreezePrimArray mutArr1x

-- | The EPX (Scale2x) rule: the 2x2 block replacing pixel @e@, given its
-- neighbours above (@b@), left (@d@), right (@f@) and below (@h@), as
-- (top-left, top-right, bottom-left, bottom-right).
{-# INLINE epx #-}
epx :: Eq a => a -> a -> a -> a -> a -> (a, a, a, a)
epx b d e f h
  | b /= h && d /= f =
      ( if d == b then d else e
      , if b == f then f else e
      , if d == h then d else e
      , if h == f then f else e
      )
  | otherwise = (e, e, e, e)

-- | EPX-double every glyph of a table: @srcW@ x @srcH@ source bits per glyph
-- (@bitAt gid col row@, 0 outside the glyph) become 2 * @srcH@ rows of
-- 2 * @srcW@ bits, column 0 at bit @msb@.
{-# INLINE buildEpxTable #-}
buildEpxTable :: (Prim w, Bits w, Num w) => Int -> Int -> Int -> Int -> (Int -> Int -> Int -> Word8) -> ST s (PrimArray w)
buildEpxTable !numGlyphs !srcW !srcH !msb bitAt = do
  out <- newPrimArray (numGlyphs * 2 * srcH)
  let forGlyph !gid = when (gid < numGlyphs) $ do
        forRow gid 0
        forGlyph (gid + 1)
      forRow !gid !r = when (r < srcH) $ do
        let (!top, !bot) = rowPair gid r 0 0 0
            !base = gid * 2 * srcH + 2 * r
        writePrimArray out base top
        writePrimArray out (base + 1) bot
        forRow gid (r + 1)
      rowPair !gid !r !c !top !bot
        | c >= srcW = (top, bot)
        | otherwise =
            let (e0, e1, e2, e3) =
                  epx (bitAt gid c (r - 1)) (bitAt gid (c - 1) r) (bitAt gid c r) (bitAt gid (c + 1) r) (bitAt gid c (r + 1))
                !bit0 = msb - 2 * c
                put v i acc = if v /= 0 then setBit acc i else acc
             in rowPair gid r (c + 1) (put e1 (bit0 - 1) (put e0 bit0 top)) (put e3 (bit0 - 1) (put e2 bit0 bot))
  forGlyph 0
  unsafeFreezePrimArray out

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

{-# INLINE getGlyphBit2x #-}
getGlyphBit2x :: PrimArray Word16 -> Int -> Int -> Int -> Word8
getGlyphBit2x !arr2x !gid !c !r
  | c < 0 || c >= 14 || r < 0 || r >= 26 = 0
  | otherwise =
      let !w = indexPrimArray arr2x (gid * 26 + r)
       in fromIntegral ((w `shiftR` (15 - c)) .&. 1)

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
          _      -> binarySearch (cfGroups font) cp
  where
    binarySearch grps cp = go 0 (sizeofPrimArray grps `div` 3 - 1)
      where
        go !lo !hi
          | lo > hi = 0
          | otherwise =
              let !mid = (lo + hi) `div` 2
                  !start = indexPrimArray grps (mid * 3)
                  !end = indexPrimArray grps (mid * 3 + 1)
                  !glyph = indexPrimArray grps (mid * 3 + 2)
               in if cp < start
                    then go lo (mid - 1)
                    else if cp > end
                      then go (mid + 1) hi
                      else glyph + (cp - start)

-- | How a glyph is drawn at a scale.
data CozetteScalePath
  = ScaleExact1x   -- ^ at or below 1x: the 7x13 bitmap
  | ScaleExact2x   -- ^ within 0.05 of 2x: the EPX 14x26 bitmap
  | ScaleExact4x   -- ^ within 0.05 of 4x: the double-EPX 28x52 bitmap
  | ScaleBoxFrom2x -- ^ other scales below 2x: the 14x26 bitmap, box-averaged
  | ScaleBoxFrom4x -- ^ other scales: the 28x52 bitmap, box-averaged
  deriving (Eq, Show)

cozetteScalePath :: Float -> CozetteScalePath
cozetteScalePath s
  | s <= 1.0 = ScaleExact1x
  | abs (s - 2.0) < 0.05 = ScaleExact2x
  | abs (s - 4.0) < 0.05 = ScaleExact4x
  | s < 2.0 = ScaleBoxFrom2x
  | otherwise = ScaleBoxFrom4x

-- | Pixel footprint of one glyph drawn at a scale.
cozetteGlyphFootprint :: Float -> (Int, Int)
cozetteGlyphFootprint s = case cozetteScalePath s of
  ScaleExact1x -> (cozetteGlyphWidth, cozetteGlyphHeight)
  ScaleExact2x -> (2 * cozetteGlyphWidth, 2 * cozetteGlyphHeight)
  ScaleExact4x -> (4 * cozetteGlyphWidth, 4 * cozetteGlyphHeight)
  _ ->
    ( max 1 (round (fromIntegral cozetteGlyphWidth * s))
    , max 1 (round (fromIntegral cozetteGlyphHeight * s))
    )

-- | Stamp one glyph with its top-left at (penX, penY) into a @stride@-wide
-- BGRA buffer, clipped to [clipX0, clipX1) x [clipY0, clipY1). Exact scales
-- copy a bitmap; other scales box-average the next larger EPX bitmap and
-- blend by coverage. Glyph 1 (space) draws nothing; unknown ids draw glyph 0.
-- Not inlined: the GL host only calls it while baking its glyph atlas.
{-# NOINLINE renderGlyphScaledToBuffer #-}
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
renderGlyphScaledToBuffer !dst !stride !clipX0 !clipY0 !clipX1 !clipY1 !scale !penX !penY !color !font !gid =
  when (glyph /= 1) $ case cozetteScalePath scale of
    ScaleExact1x -> blitMask dst stride penX penY color (rowsOf 13) (colsOf 7) row1x
    ScaleExact2x -> blitMask dst stride penX penY color (rowsOf 26) (colsOf 14) row2x
    ScaleExact4x -> blitMask dst stride penX penY color (rowsOf 52) (colsOf 28) row4x
    ScaleBoxFrom2x -> boxed 14 26 row2x
    ScaleBoxFrom4x -> boxed 28 52 row4x
  where
    !glyph = if fromIntegral gid < cfNumGlyphs font then fromIntegral gid else 0 :: Int
    -- Bitmap rows as masks with column 0 at bit 31.
    row1x r = fromIntegral (indexPrimArray (cfGlyphData1x font) (glyph * 13 + r)) `shiftL` 24
    row2x r = fromIntegral (indexPrimArray (cfGlyphData2x font) (glyph * 26 + r)) `shiftL` 16
    row4x r = indexPrimArray (cfGlyphData4x font) (glyph * 52 + r)
    -- The part of an h-row / w-column footprint inside the clip.
    rowsOf h = (max 0 (clipY0 - penY), min h (clipY1 - penY))
    colsOf w = (max 0 (clipX0 - penX), min w (clipX1 - penX))
    boxed :: Int -> Int -> (Int -> Word32) -> IO ()
    boxed !srcW !srcH row = goRow dy0
      where
        (!tw, !th) = cozetteGlyphFootprint scale
        !scaleX = fromIntegral srcW / fromIntegral tw :: Float
        !scaleY = fromIntegral srcH / fromIntegral th :: Float
        (!dy0, !dy1) = rowsOf th
        (!dx0, !dx1) = colsOf tw
        goRow !dy = when (dy < dy1) $ do
          let !y0 = fromIntegral dy * scaleY
              !y1 = fromIntegral (dy + 1) * scaleY
              !base = (penY + dy) * stride + penX
              goCol !dx = when (dx < dx1) $ do
                let !off = base + dx
                    !cov = boxCoverage srcW srcH row (fromIntegral dx * scaleX) (fromIntegral (dx + 1) * scaleX) y0 y1
                    !effA = round (fromIntegral srcA * cov) :: Int
                -- Skip near-transparent coverage; overwrite near-opaque.
                when (effA > 3) $
                  if effA >= 252
                    then pokeElemOff dst off color
                    else do
                      d <- peekElemOff dst off
                      let !invA = 255 - effA
                          mix s = fromIntegral ((channel color s * effA + channel d s * invA + 127) `div` 255) :: Word32
                      pokeElemOff dst off (0xFF000000 .|. (mix 16 `shiftL` 16) .|. (mix 8 `shiftL` 8) .|. mix 0)
                goCol (dx + 1)
          goCol dx0
          goRow (dy + 1)
        !srcA = channel color 24
        channel w s = fromIntegral ((w `shiftR` s) .&. 0xFF) :: Int

-- | Write @color@ wherever a row mask (column 0 at bit 31) has a bit set, over
-- the given row and column ranges of a glyph at (penX, penY).
{-# INLINE blitMask #-}
blitMask :: Ptr Word32 -> Int -> Int -> Int -> Word32 -> (Int, Int) -> (Int, Int) -> (Int -> Word32) -> IO ()
blitMask !dst !stride !penX !penY !color (!r0, !r1) (!c0, !c1) row = goRow r0
  where
    goRow !r = when (r < r1) $ do
      let !bits = row r
      when (bits /= 0) $ goCol ((penY + r) * stride + penX) bits c0
      goRow (r + 1)
    goCol !base !bits !c = when (c < c1) $ do
      when (testBit bits (31 - c)) $ pokeElemOff dst (base + c) color
      goCol base bits (c + 1)

-- | Covered fraction, in [0, 1], of the texel box [x0, x1) x [y0, y1) on a
-- @srcW@ x @srcH@ grid whose rows are masks with column 0 at bit 31.
{-# INLINE boxCoverage #-}
boxCoverage :: Int -> Int -> (Int -> Word32) -> Float -> Float -> Float -> Float -> Float
boxCoverage !srcW !srcH row !x0 !x1 !y0 !y1
  | area > 0 = max 0 (min 1 (goRow (max 0 (floor y0)) 0 / area))
  | otherwise = 0
  where
    !area = (x1 - x0) * (y1 - y0)
    !syMax = min (srcH - 1) (floor (y1 - 1e-5))
    !sxMin = max 0 (floor x0)
    !sxMax = min (srcW - 1) (floor (x1 - 1e-5))
    goRow !sy !acc
      | sy > syMax = acc
      | otherwise =
          let !ovY = max 0 (min (fromIntegral (sy + 1)) y1 - max (fromIntegral sy) y0)
           in goRow (sy + 1) (acc + goCol (row sy) ovY sxMin 0)
    goCol !bits !ovY !sx !acc
      | sx > sxMax = acc
      | otherwise =
          let !ovX = max 0 (min (fromIntegral (sx + 1)) x1 - max (fromIntegral sx) x0)
              !inc = if testBit bits (31 - sx) then ovX * ovY else 0
           in goCol bits ovY (sx + 1) (acc + inc)

-- | Visit the glyph pen positions (physical pixels) of a text run laid out
-- from logical (logX, logY) at a scale, threading an accumulator. @\\r@
-- returns to column 0, @\\n@ starts the next line, and space (glyph 1)
-- advances without a visit.
{-# INLINE foldPenPositions #-}
foldPenPositions :: CozetteFont -> Float -> Float -> Float -> a -> (a -> Int -> Int -> Word32 -> IO a) -> Text -> IO a
foldPenPositions font !scale !logX !logY z step = go (0 :: Int) (0 :: Int) z
  where
    -- Ties up like nano-ui's onGrid, so pens stay in phase with snapped
    -- geometry at fractional scales (125%: a 6px advance is 7.5 device px).
    pen origin i advance = roundHalfUp ((origin + fromIntegral i * advance) * scale)
    go !col !line !acc t = case T.uncons t of
      Nothing -> pure acc
      Just ('\r', rest) -> go 0 line acc rest
      Just ('\n', rest) -> go 0 (line + 1) acc rest
      Just (c, rest) -> do
        let !gid = charToGlyphId font c
        acc' <-
          if gid == 1
            then pure acc
            else step acc (pen logX col cozetteCharAdvance) (pen logY line cozetteLineHeight) gid
        go (col + 1) line acc' rest

cozetteMetrics :: FontMetrics
cozetteMetrics =
  FontMetrics
    { fmLineHeight = cozetteLineHeight
    , fmAscent = cozetteAscent
    , fmSnapScale = 1.0
    , fmAdvance = \_ -> cozetteCharAdvance
    , fmKerning = \_ _ -> 0
    , fmShape = \_ -> Nothing
    , fmGlyph = \_ -> Nothing
    , fmBackend = Nothing
    }
