module NanoUI.Host
  ( HostProfile (..)
  , isCellHost
  ) where

-- Pixel hosts (SDL, headless). Cell hosts (terminal).
data HostProfile
  = PixelHost
  | CellHost
  deriving (Eq, Show)

{-# INLINE isCellHost #-}
isCellHost :: HostProfile -> Bool
isCellHost CellHost = True
isCellHost PixelHost = False
