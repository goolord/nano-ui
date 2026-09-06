module NanoUI.Sdl.NanoUIFont
  ( NanoUIFont (..)
  ) where

-- | SDL text font selection.
-- 'DefaultFont' uses the embedded Inter subset.
-- 'FontSearch' tries each family name against the platform font directories
-- (walked recursively at runtime). When nothing matches, the embedded font
-- is used as fallback.
-- 'FontFilePath' loads that file and does not search.
data NanoUIFont
  = DefaultFont
  | FontSearch [String]
  | FontFilePath FilePath
  deriving (Eq, Show)
