{-# LANGUAGE TemplateHaskell #-}

-- | Embedded default UI font. Import only from font bootstrap code.
module NanoUI.Sdl.Font.Inter
  ( fontInterBytes
  , fontInterLabel
  ) where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFileRelative)

fontInterLabel :: FilePath
fontInterLabel = "inter.ttf"

{-# NOINLINE fontInterBytes #-}
fontInterBytes :: ByteString
fontInterBytes = $(embedFileRelative "data/inter.ttf")
