-- | User-perceived characters, closely enough for caret motion and wrapping,
-- told apart without Unicode's segmentation tables.
module NanoUI.Internal.TextCluster
  ( clusterLength
  , clusterBounds
  , prevClusterStart
  ) where

import Data.Char (isMark)
import Data.Text (Text)
import Data.Text qualified as T

attaches :: Char -> Bool
attaches c =
  isMark c
    || (c >= '\x1F3FB' && c <= '\x1F3FF')
    || (c >= '\xE0020' && c <= '\xE007F')

isRegional :: Char -> Bool
isRegional c = c >= '\x1F1E6' && c <= '\x1F1FF'

-- | How many characters the user-perceived character @t@ starts with spans,
-- 0 for empty text: a character keeps the combining marks (variation
-- selectors among them), emoji modifiers and tag characters after it, a
-- zero-width joiner joins the next character on, and regional indicators
-- pair into flags. A newline is its own cluster: nothing joins or attaches
-- to it.
clusterLength :: Text -> Int
clusterLength t = case T.uncons t of
  Nothing -> 0
  Just ('\n', _) -> 1
  Just (c, rest) -> 1 + extend (isRegional c) rest
  where
    -- Characters after the first that stay in its cluster.
    extend regional s = case T.uncons s of
      Just (c, rest)
        | c == '\x200D' -> case T.uncons rest of
            Just (n, rest') | n /= '\n' -> 2 + extend False rest'
            _ -> 1
        | attaches c -> 1 + extend False rest
        | regional && isRegional c -> 1 + extend False rest
      _ -> 0

-- | The user-perceived character boundaries around character @k@ of @t@:
-- the last at or before it and the first at or after it. Both are @k@ where
-- a character starts, and both the length of @t@ past its end.
clusterBounds :: Text -> Int -> (Int, Int)
clusterBounds t k = go 0 t
  where
    go !i s
      | i >= k || n == 0 = (i, i)
      | i + n > k = (i, i + n)
      | otherwise = go (i + n) (T.drop n s)
      where
        n = clusterLength s

-- | Start index of the cluster immediately preceding character offset @k@
-- (for @k > 0@), equal to @fst (clusterBounds t (k - 1))@ while only walking
-- back to the nearest guaranteed cluster boundary.
prevClusterStart :: Text -> Int -> Int
prevClusterStart t !k
  | k <= 1 = 0
  | otherwise =
      let (prefix, suffix) = T.splitAt k t
          -- 'T.null suffix' holds only at or past end-of-line; checking length
          -- there keeps out-of-bounds offsets consistent with 'clusterBounds'.
          !len = if T.null suffix then T.length prefix else k
       in if k > len
            then len
            else case T.unsnoc prefix of
              Nothing -> 0
              Just (rest, c)
                | not (attaches c || c == '\x200D' || isRegional c)
                , not (endsWithZWJ rest) -> k - 1
                | otherwise ->
                    let !target = k - 1
                        !back = rewind 0 prefix
                        !syncPos = k - back
                        run !i s =
                          let n = clusterLength s
                           in if n == 0 || i + n > target
                                then i
                                else run (i + n) (T.drop n s)
                     in run syncPos (T.takeEnd back prefix)
  where
    endsWithZWJ s = case T.unsnoc s of
      Just (_, '\x200D') -> True
      _ -> False
    rewind !back s = case T.unsnoc s of
      Nothing -> back
      Just (rest, c)
        | c == '\n' -> back + 1
        | attaches c || c == '\x200D' || isRegional c || endsWithZWJ rest -> rewind (back + 1) rest
        | otherwise -> back + 1
