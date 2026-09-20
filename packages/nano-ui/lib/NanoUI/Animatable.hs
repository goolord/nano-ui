-- | Conversion of values to scalar channels for tween and spring animations.
module NanoUI.Animatable
  ( Animatable (..)
  ) where

import Data.Word (Word8)
import NanoUI.Types (Color, V2 (..), clamp01, colorA, colorB, colorG, colorR, colorRGBA)

-- | Float components for multi-component tweens. Extra components are dropped.
-- Short Color lists pad RGB with 0 and alpha with 1. Other types pad with 0.
class Animatable a where
  -- | Channels in a stable order; colours use RGBA normalised to 0-1.
  toComponents :: a -> [Float]
  -- | Reconstruct a value, padding missing channels as described above.
  fromComponents :: [Float] -> a

instance Animatable Float where
  toComponents v = [v]
  fromComponents (v : _) = v
  fromComponents [] = 0

instance Animatable Double where
  toComponents v = [realToFrac v]
  fromComponents (v : _) = realToFrac v
  fromComponents [] = 0

instance Animatable V2 where
  toComponents (V2 x y) = [x, y]
  fromComponents (x : y : _) = V2 x y
  fromComponents [x] = V2 x 0
  fromComponents [] = V2 0 0

instance Animatable Color where
  toComponents c =
    [chan (colorR c), chan (colorG c), chan (colorB c), chan (colorA c)]
  fromComponents (r : g : b : a : _) =
    colorRGBA (byte r) (byte g) (byte b) (byte a)
  fromComponents xs = fromComponents (take 3 (xs ++ repeat 0) ++ [1])

chan :: Word8 -> Float
chan w = fromIntegral w / 255

byte :: Float -> Word8
byte x = fromIntegral (round (clamp01 x * 255) :: Int)
