-- | Resolve font requests to installed files or the bundled Inter fallback.
module NanoUI.Sdl.Internal.Font.Resolve
  ( embeddedFontSource
  , resolveNanoUIFont
  , defaultFontSearch
  , defaultFontSearchMono
  ) where

import NanoUI.Sdl.Internal.Font (FontSource (..))
import NanoUI.Sdl.Internal.Font.Inter (fontInterBytes, fontInterLabel)
import NanoUI.Sdl.Internal.Font.Search (searchFonts)
import NanoUI.Sdl.Internal.NanoUIFont (NanoUIFont (..))

-- | Bundled Inter bytes and their diagnostic label.
embeddedFontSource :: FontSource
embeddedFontSource = FontFromMemory fontInterBytes fontInterLabel

-- | Ordered sans-serif family preferences, starting with Inter.
defaultFontSearch :: NanoUIFont
defaultFontSearch =
  FontSearch
    [ "Inter"
    , "Montserrat"
    , "Work Sans"
    , "Roboto"
    , "Open Sans"
    , "Helvetica Neue"
    ]

-- | Ordered monospace preferences, starting with Consolas.
defaultFontSearchMono :: NanoUIFont
defaultFontSearchMono =
  FontSearch
    [ "Consolas"
    , "Courier New"
    , "Liberation Mono"
    , "DejaVu Sans Mono"
    , "monospace"
    ]

-- | Resolve a search request, using bundled Inter if no family matches.
-- An explicit file path is passed through; loading it can still fail later.
resolveNanoUIFont :: NanoUIFont -> IO FontSource
resolveNanoUIFont DefaultFont = pure embeddedFontSource
resolveNanoUIFont (FontFilePath path) = pure (FontFromPath path)
resolveNanoUIFont (FontSearch names) =
  searchFonts names >>= \case
    Just path -> pure (FontFromPath path)
    Nothing -> pure embeddedFontSource
