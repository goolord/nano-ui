-- | Serialises a cell grid to ANSI escape bytes.
--
-- Frames are diffed against the previously drawn grid and only changed spans
-- are rewritten, which matters because hover makes every pointer movement a
-- redraw: repainting a full screen per motion event would be tens of kilobytes
-- of output per frame.
module NanoUI.Term.Ansi
  ( setup
  , teardown
  , frameBytes
  ) where

import Data.Bits (shiftR, (.&.))
import Data.ByteString.Builder (Builder, charUtf8, intDec, string7)
import Data.Word (Word32)
import NanoUI (fontAwesomeIcon)
import NanoUI.Term.Cells (Cells, cellBg, cellChar, cellFg, cellsH, cellsSize, cellsW, wideTrailChar)

-- | Switch to the alternate screen buffer so the user's scrollback survives,
-- and hide the cursor since widgets draw their own.
setup :: Builder
setup = string7 "\ESC[?1049h\ESC[?25l\ESC[?7l\ESC[2J\ESC[H"

teardown :: Builder
teardown = string7 "\ESC[0m\ESC[?25h\ESC[?7h\ESC[?1049l"

-- | Bytes needed to turn @prev@ into @cur@. Pass 'Nothing' for the first frame
-- after setup or a resize, which forces a full repaint.
frameBytes :: Maybe Cells -> Cells -> Builder
frameBytes prev cur = fst (foldl' rowStep (mempty, Pen Nothing Nothing) rows)
  where
    rows = [0 .. cellsH cur - 1]
    reference =
      case prev of
        Just p | cellsSize p == cellsSize cur -> Just p
        _ -> Nothing
    rowStep (acc, pen) y =
      let (b, pen') = emitRow reference cur y pen
       in (acc <> b, pen')

-- | Current foreground/background as the terminal understands it. Escape state
-- persists across cursor jumps, so this threads through the whole frame.
data Pen = Pen
  { penFg :: !(Maybe Word32)
  , penBg :: !(Maybe Word32)
  }

-- | Repainting a cell costs ~1 byte; moving the cursor costs ~8. Bridging a
-- short gap of unchanged cells is cheaper than starting a new span.
gapTolerance :: Int
gapTolerance = 6

isActiveWideTrail :: Cells -> Int -> Int -> Bool
isActiveWideTrail cells x y =
  x > 0 && fontAwesomeIcon (cellChar cells (x - 1) y)

-- Prev partner still holds a real glyph (often '['). Space and trail are not stale.
staleWidePartner :: Cells -> Int -> Int -> Bool
staleWidePartner p x y =
  x + 1 < cellsW p
    && let q = cellChar p (x + 1) y
        in q /= wideTrailChar && q /= ' '

emitRow :: Maybe Cells -> Cells -> Int -> Pen -> (Builder, Pen)
emitRow reference cur y = go 0 mempty
  where
    w = cellsW cur
    changed x =
      case reference of
        Nothing -> True
        Just p ->
          cellChar p x y /= cellChar cur x y
            || cellFg p x y /= cellFg cur x y
            || cellBg p x y /= cellBg cur x y
            || widePartnerNeedsClear p x
            || wideCoversStale p x
            || wideOrphanNeedsClear p x
    widePartnerNeedsClear p x =
      x + 1 < w
        && fontAwesomeIcon (cellChar p x y)
        && not (fontAwesomeIcon (cellChar cur x y))
        && cellChar p (x + 1) y == wideTrailChar
    wideCoversStale p x =
      fontAwesomeIcon (cellChar cur x y) && staleWidePartner p x y
    wideOrphanNeedsClear _ x =
      cellChar cur x y == wideTrailChar
        && not (isActiveWideTrail cur x y)
    go x acc pen
      | x >= w = (acc, pen)
      | cellChar cur x y == wideTrailChar, isActiveWideTrail cur x y =
          go (x + 1) acc pen
      | not (changed x) = go (x + 1) acc pen
      | otherwise =
          let end0 = spanEnd (x + 1) 0
              end = extendWideClear reference end0 x
              (b, pen') = emitSpan reference cur y x end pen
           in go end (acc <> moveTo x y <> b) pen'
    extendWideClear ref end x =
      case ref of
        Just p
          | fontAwesomeIcon (cellChar p x y)
          , not (fontAwesomeIcon (cellChar cur x y)) ->
              max end (min w (x + 2))
        _ -> end
    -- Extend the span while cells change, tolerating short unchanged gaps.
    spanEnd x gap
      | x >= w = x - gap
      | changed x = spanEnd (x + 1) 0
      | gap >= gapTolerance = x - gap
      | otherwise = spanEnd (x + 1) (gap + 1)

emitSpan :: Maybe Cells -> Cells -> Int -> Int -> Int -> Pen -> (Builder, Pen)
emitSpan reference cur y from to pen0 = foldl' cell (mempty, pen0) [from .. to - 1]
  where
    cell (acc, pen) x =
      let ch = cellChar cur x y
       in if ch == wideTrailChar
            then
              if isActiveWideTrail cur x y
                then (acc, pen)
                else emitCell acc pen ' ' x
            else
              if fontAwesomeIcon ch
                then emitWide acc pen ch x
                else emitCell acc pen ch x
    -- Windows often advances one column for a PUA icon while the font paints
    -- two. Always CUP to x+2 after the glyph so the next cell cannot overlap.
    -- Space-space first only when the old partner still holds a real glyph, or
    -- when a new icon lands on a blank pair (modal open, window move).
    emitWide acc pen ch x =
      let nextCol = x + 2
          finish acc' pen' =
            if nextCol < cellsW cur
              then (acc' <> moveTo nextCol y, pen')
              else (acc', pen')
          dance =
            case reference of
              Just p ->
                let q =
                      if x + 1 < cellsW cur
                        then cellChar p (x + 1) y
                        else wideTrailChar
                    leadChanged = cellChar p x y /= ch
                 in q /= wideTrailChar && (q /= ' ' || leadChanged)
              Nothing -> False
       in if dance
            then
              let (acc1, pen1) = emitCell acc pen ' ' x
                  trailX = min (cellsW cur - 1) (x + 1)
                  (acc2, pen2) = emitCell acc1 pen1 ' ' trailX
                  (sgr, pen3) = penUpdate pen2 (cellFg cur x y) (cellBg cur x y)
               in finish (acc2 <> moveTo x y <> sgr <> charUtf8 ch) pen3
            else
              let (acc1, pen1) = emitCell acc pen ch x
               in finish acc1 pen1
    emitCell acc pen ch x =
      let fg = cellFg cur x y
          bg = cellBg cur x y
          (sgr, pen') = penUpdate pen fg bg
       in (acc <> sgr <> charUtf8 ch, pen')

penUpdate :: Pen -> Word32 -> Word32 -> (Builder, Pen)
penUpdate pen fg bg
  | not fgDiff && not bgDiff = (mempty, pen)
  | otherwise =
      ( string7 "\ESC[" <> body <> string7 "m"
      , Pen {penFg = Just fg, penBg = Just bg}
      )
  where
    fgDiff = penFg pen /= Just fg
    bgDiff = penBg pen /= Just bg
    body =
      case (fgDiff, bgDiff) of
        (True, True) -> colorParams True fg <> string7 ";" <> colorParams False bg
        (True, False) -> colorParams True fg
        (False, True) -> colorParams False bg
        _ -> mempty

-- | Truecolor SGR parameters. An alpha below 32 means the frame wants the
-- terminal's own default colour rather than a specific RGB value.
colorParams :: Bool -> Word32 -> Builder
colorParams isFg rgba
  | rgba .&. 0xff < 32 = intDec (if isFg then 39 else 49)
  | otherwise =
      intDec (if isFg then 38 else 48)
        <> string7 ";2;"
        <> intDec (channel 24)
        <> string7 ";"
        <> intDec (channel 16)
        <> string7 ";"
        <> intDec (channel 8)
  where
    channel shift = fromIntegral ((rgba `shiftR` shift) .&. 0xff)

moveTo :: Int -> Int -> Builder
moveTo x y =
  string7 "\ESC[" <> intDec (y + 1) <> string7 ";" <> intDec (x + 1) <> string7 "H"
