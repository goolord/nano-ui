module NanoUI.Animatable
  ( Animatable (..)
  ) where

import Data.Word (Word8)
import NanoUI.Types (Color, V2 (..), colorA, colorB, colorG, colorR, colorRGBA, lerpColor)

-- Vector-space ops for multi-component tweens. Extra components are dropped.
-- Short Color lists pad RGB with 0 and alpha with 1. Other types pad with 0.
class Animatable a where
  lerp :: Float -> a -> a -> a
  distance :: a -> a -> Float
  toComponents :: a -> [Float]
  fromComponents :: [Float] -> a

instance Animatable Float where
  lerp t a b = a + t * (b - a)
  distance a b = abs (b - a)
  toComponents v = [v]
  fromComponents (v : _) = v
  fromComponents [] = 0

instance Animatable Double where
  lerp t a b = a + realToFrac t * (b - a)
  distance a b = realToFrac (abs (b - a))
  toComponents v = [realToFrac v]
  fromComponents (v : _) = realToFrac v
  fromComponents [] = 0

instance Animatable V2 where
  lerp t (V2 x1 y1) (V2 x2 y2) =
    V2 (x1 + t * (x2 - x1)) (y1 + t * (y2 - y1))
  distance (V2 x1 y1) (V2 x2 y2) =
    sqrt ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))
  toComponents (V2 x y) = [x, y]
  fromComponents (x : y : _) = V2 x y
  fromComponents [x] = V2 x 0
  fromComponents [] = V2 0 0

instance Animatable Color where
  lerp t a b = lerpColor a b t
  distance a b =
    let cs = zipWith (-) (toComponents b) (toComponents a)
     in sqrt (sum (map (\c -> c * c) cs))
  toComponents c =
    [chan (colorR c), chan (colorG c), chan (colorB c), chan (colorA c)]
  fromComponents (r : g : b : a : _) =
    colorRGBA (byte r) (byte g) (byte b) (byte a)
  fromComponents xs = fromComponents (take 4 (xs ++ [0, 0, 0, 1]))

chan :: Word8 -> Float
chan w = fromIntegral w / 255

byte :: Float -> Word8
byte x = fromIntegral (round (max 0 (min 1 x) * 255) :: Int)
