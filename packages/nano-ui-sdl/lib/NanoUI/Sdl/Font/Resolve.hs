{-# LANGUAGE CPP #-}

module NanoUI.Sdl.Font.Resolve
  ( embeddedFontSource
  , resolveNanoUIFont
  , needsFontconfig
  , defaultFontSearch
  , defaultFontSearchMono
  ) where

import NanoUI.Sdl.Font (FontSource (..))
import NanoUI.Sdl.Font.Inter (fontInterBytes, fontInterLabel)
import NanoUI.Sdl.Font.Search (searchFonts)
import NanoUI.Sdl.NanoUIFont (NanoUIFont (..))

embeddedFontSource :: FontSource
embeddedFontSource = FontFromMemory fontInterBytes fontInterLabel

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

defaultFontSearchMono :: NanoUIFont
defaultFontSearchMono =
  FontSearch
    [ "Consolas"
    , "Courier New"
    , "Liberation Mono"
    , "DejaVu Sans Mono"
    , "monospace"
    ]

needsFontconfig :: NanoUIFont -> Bool
needsFontconfig DefaultFont = False
needsFontconfig (FontFilePath _) = False
needsFontconfig (FontSearch _) =
#if HAVE_FONTCONFIG
  True
#else
  False
#endif

resolveNanoUIFont :: NanoUIFont -> IO FontSource
resolveNanoUIFont DefaultFont = pure embeddedFontSource
resolveNanoUIFont (FontFilePath path) = pure (FontFromPath path)
resolveNanoUIFont (FontSearch names) =
  searchFonts names >>= \case
    Just path -> pure (FontFromPath path)
    Nothing -> pure embeddedFontSource
