-- | Font selection for the SDL backend.
module NanoUI.Sdl.Internal.NanoUIFont
  ( NanoUIFont (..)
  ) where

-- | SDL text font selection.
-- 'DefaultFont' uses the embedded Inter subset.
-- 'FontSearch' tries each family name against the platform font directories
-- (walked recursively at runtime). Names are matched against font file names,
-- ignoring case, spaces, @-@ and @_@, not against the families the fonts
-- declare; @monospace@, @sans-serif@ and @serif@ stand for lists of common
-- families. When nothing matches, the embedded font is used as fallback.
-- 'FontFilePath' loads that file and does not search. A file that will not
-- open is replaced by the embedded font.
data NanoUIFont
  = DefaultFont
  | FontSearch [String]
  | FontFilePath FilePath
  deriving (Eq, Show)
