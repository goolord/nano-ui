-- | User-perceived characters, closely enough for caret motion and wrapping,
-- told apart without Unicode's segmentation tables.
module NanoUI.Internal.TextCluster
  ( clusterLength
  ) where

import Data.Char (isMark)
import Data.Text (Text)
import Data.Text qualified as T

-- | How many characters the user-perceived character @t@ starts with spans,
-- 0 for empty text: a character keeps the combining marks, variation
-- selectors and emoji modifiers after it, a zero-width joiner joins the next
-- character on, and regional indicators pair into flags. A newline is its
-- own cluster.
clusterLength :: Text -> Int
clusterLength t = case T.uncons t of
  Nothing -> 0
  Just (c, rest) -> 1 + extend (isRegional c) rest
  where
    -- Characters after the first that stay in its cluster.
    extend regional s = case T.uncons s of
      Just (c, rest)
        | c == '\x200D' -> case T.uncons rest of
            Just (_, rest') -> 2 + extend False rest'
            Nothing -> 1
        | attaches c -> 1 + extend False rest
        | regional && isRegional c -> 1 + extend False rest
      _ -> 0
    attaches c =
      isMark c
        || (c >= '\xFE00' && c <= '\xFE0F')
        || (c >= '\x1F3FB' && c <= '\x1F3FF')
        || (c >= '\xE0020' && c <= '\xE007F')
        || (c >= '\xE0100' && c <= '\xE01EF')
    isRegional c = c >= '\x1F1E6' && c <= '\x1F1FF'
