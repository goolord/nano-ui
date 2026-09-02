module NanoUI.Sdl.NanoUIFont
  ( NanoUIFont (..)
  ) where

-- | SDL text font selection.
-- 'DefaultFont' uses the embedded Inter subset.
-- 'FontSearch' tries each family name via fontconfig (or the platform font
-- database). When nothing matches, the embedded font is used as fallback.
-- 'FontFilePath' loads that file and does not search.
data NanoUIFont
  = DefaultFont
  | FontSearch [String]
  | FontFilePath FilePath
  deriving (Eq, Show)
