{-# LANGUAGE TemplateHaskell #-}

-- | The package's @data@ directory, compiled into the browser sampler. It
-- is a library of its own (@sampler-files@) because GHC's wasm backend runs
-- a splice with every C library of its component loaded, and it cannot load
-- the web backend's: FreeType's setjmp imports an exception tag.
module DemoFiles (dataFiles) where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedDir, makeRelativeToProject)

-- | Each file under @data@, by its path there (@lick.gif@,
-- @icons/star.svg@), with its bytes.
dataFiles :: [(FilePath, ByteString)]
dataFiles = $(makeRelativeToProject "data" >>= embedDir)
