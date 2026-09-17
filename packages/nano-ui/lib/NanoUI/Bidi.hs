-- | Direction runs of a line of mixed left-to-right and right-to-left text,
-- for hosts that shape one direction at a time.
--
-- This is the implicit part of the Unicode bidirectional algorithm (UAX #9)
-- for a single line: the paragraph direction from the first strong
-- character, European and Arabic numbers, neutrals between runs, and
-- reordering by level. Explicit embeddings, overrides, isolates and bracket
-- pairs are not handled; text using them lays out as if they were absent.
module NanoUI.Bidi
  ( BidiRun (..)
  , bidiRuns
  , needsBidi
  ) where

import Data.Char (ord)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T

-- | Characters @runStart@ up to @runEnd@ (exclusive) shaped in one direction.
data BidiRun = BidiRun
  { runStart :: !Int
  , runEnd :: !Int
  , runRightToLeft :: !Bool
  }
  deriving (Eq, Show)

data Class = L | R | AL | EN | AN | NSM | WS | ON
  deriving (Eq, Show)

-- | Whether a line has characters that can flow right to left. A line
-- without them is one left-to-right run.
needsBidi :: Text -> Bool
needsBidi = T.any (\c -> ord c >= 0x0590 && rtlOrArabic (classify c))
  where
    rtlOrArabic k = k == R || k == AL || k == AN

-- | The runs of a line in visual order, left to right.
bidiRuns :: Text -> [BidiRun]
bidiRuns txt
  | T.null txt = []
  | not (needsBidi txt) = [BidiRun 0 (T.length txt) False]
  | otherwise =
      let classes0 = map classify (T.unpack txt)
          paragraphRtl = case [k | k <- classes0, k == L || k == R || k == AL] of
            k : _ -> k /= L
            [] -> False
          base = if paragraphRtl then 1 else 0 :: Int
          classes = resolveNeutrals paragraphRtl (resolveWeak paragraphRtl classes0)
          levels = map (implicitLevel base) classes
          indexed = zip [0 :: Int ..] levels
          runs =
            [ (start, fst (NE.last grp) + 1, lvl)
            | grp <- NE.groupBy (\a b -> snd a == snd b) indexed
            , let (start, lvl) = NE.head grp
            ]
       in [BidiRun s e (odd lvl) | (s, e, lvl) <- reorder runs]

classify :: Char -> Class
classify c
  | n < 0x0590 = latin
  | n <= 0x05FF = if n >= 0x0591 && n <= 0x05C7 && n /= 0x05BE && n /= 0x05C0 && n /= 0x05C3 && n /= 0x05C6 then NSM else R
  | n >= 0x0660 && n <= 0x0669 = AN
  | n >= 0x06F0 && n <= 0x06F9 = EN
  | n >= 0x064B && n <= 0x065F || n == 0x0670 || n >= 0x06D6 && n <= 0x06ED = NSM
  | n <= 0x06FF = AL
  | n <= 0x07BF = if n >= 0x0730 && n <= 0x074A || n >= 0x07A6 && n <= 0x07B0 then NSM else AL
  | n <= 0x07FF = R
  | n <= 0x085F = R
  | n <= 0x08FF = if n >= 0x08D3 then NSM else AL
  | n >= 0x200E && n <= 0x200F = if n == 0x200E then L else R
  | n >= 0xFB1D && n <= 0xFB4F = R
  | n >= 0xFB50 && n <= 0xFDFF = AL
  | n >= 0xFE70 && n <= 0xFEFF = AL
  | n >= 0x0300 && n <= 0x036F = NSM
  | n >= 0x2000 && n <= 0x206F = if n <= 0x200A || n == 0x2028 || n == 0x2029 then WS else ON
  | n >= 0x10800 && n <= 0x10FFF = R
  | n >= 0x1E800 && n <= 0x1EFFF = AL
  | otherwise = L
  where
    n = ord c
    latin
      | c >= '0' && c <= '9' = EN
      | c == ' ' || c == '\t' = WS
      | n < 0x80 && not (isAsciiLetter c) = ON
      | n >= 0x80 && n <= 0xBF = ON
      | n == 0xD7 || n == 0xF7 = ON
      | n >= 0x0300 && n <= 0x036F = NSM
      | otherwise = L
    isAsciiLetter ch = (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z')

-- | Weak types: a mark takes the class before it, a European number after
-- Arabic letters reads as an Arabic number and one after Latin letters (or
-- at the start of a left-to-right line) as Latin.
resolveWeak :: Bool -> [Class] -> [Class]
resolveWeak paragraphRtl = go ON (if paragraphRtl then R else L)
  where
    go _ _ [] = []
    go prev lastStrong (k : ks) =
      let k1 = if k == NSM then prev else k
          k2
            | k1 == EN && lastStrong == AL = AN
            | k1 == EN && lastStrong == L = L
            | otherwise = k1
          k3 = if k2 == AL then R else k2
          lastStrong' = if k1 == L || k1 == R || k1 == AL then k1 else lastStrong
       in k3 : go k1 lastStrong' ks

-- | Neutrals between two characters of the same direction take it (numbers
-- count as right to left here); other neutrals take the paragraph's.
resolveNeutrals :: Bool -> [Class] -> [Class]
resolveNeutrals paragraphRtl classes =
  let direction k
        | k == L = Just False
        | k == R || k == AN || k == EN = Just True
        | otherwise = Nothing
      directions = map direction classes
      before = scanl (\acc d -> maybe acc Just d) Nothing directions
      after = drop 1 (scanr (\d acc -> maybe acc Just d) Nothing directions)
      resolve k b a
        | k == WS || k == ON = case (b, a) of
            (Just x, Just y) | x == y -> if x then R else L
            _ -> if paragraphRtl then R else L
        | otherwise = k
   in zipWith3 resolve classes before after

-- | The embedding level of a resolved class at paragraph level @base@.
implicitLevel :: Int -> Class -> Int
implicitLevel base k
  | even base = case k of
      R -> base + 1
      AL -> base + 1
      AN -> base + 2
      EN -> base + 2
      _ -> base
  | otherwise = case k of
      L -> base + 1
      EN -> base + 1
      AN -> base + 1
      _ -> base

-- | Reverse every maximal sequence of runs at or above each odd level, from
-- the highest level down.
reorder :: [(Int, Int, Int)] -> [(Int, Int, Int)]
reorder runs =
  let maxLevel = maximum (0 : [l | (_, _, l) <- runs])
      lowestOdd = minimum (maxLevel + 1 : [l | (_, _, l) <- runs, odd l])
      pass lvl rs =
        concatMap
          (\grp -> if atLeast (NE.head grp) then reverse (NE.toList grp) else NE.toList grp)
          (NE.groupBy (\a b -> atLeast a == atLeast b) rs)
        where
          atLeast (_, _, l) = l >= lvl
   in foldl (flip pass) runs [maxLevel, maxLevel - 1 .. lowestOdd]
