{-# LANGUAGE TemplateHaskell #-}

-- | Embedded default UI font. Import only from font bootstrap code.
module NanoUI.Ttf.Internal.Inter
  ( fontInterBytes
  , fontInterLabel
  ) where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFileRelative)

-- | Display label for the bundled Inter font.
fontInterLabel :: FilePath
fontInterLabel = "inter.ttf"

-- | Raw TTF bytes for the bundled Inter font.
{-# NOINLINE fontInterBytes #-}
fontInterBytes :: ByteString
fontInterBytes = $(embedFileRelative "data/inter.ttf")
