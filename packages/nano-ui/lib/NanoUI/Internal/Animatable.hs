-- | Indexed traversal of scalar channels for tween and spring animations.
module NanoUI.Internal.Animatable
  ( Animatable (..)
  )
where

import Data.Word (Word8)
import NanoUI.Internal.Types (Color, V2 (..), clamp01, colorA, colorB, colorG, colorR, colorRGBA)

-- | Visit each scalar channel once, in stable index order starting at zero.
-- Channel indices become animation keys. Reconstruction has the same shape as
-- the input, without an intermediate list. The animation engine uses 'Float';
-- the 'Double' instance therefore retains only single-precision channel values.
class Animatable a where
  -- | Transform scalar channels; colours use RGBA normalised to 0-1 and clamp
  -- and round transformed channels back to bytes.
  traverseChannels :: Applicative f => (Int -> Float -> f Float) -> a -> f a

instance Animatable Float where
  {-# INLINE traverseChannels #-}
  traverseChannels f = f 0

instance Animatable Double where
  {-# INLINE traverseChannels #-}
  traverseChannels f v = realToFrac <$> f 0 (realToFrac v)

instance Animatable V2 where
  {-# INLINE traverseChannels #-}
  traverseChannels f (V2 x y) = V2 <$> f 0 x <*> f 1 y

instance Animatable Color where
  {-# INLINE traverseChannels #-}
  traverseChannels f c =
    colorRGBA
      <$> channel 0 (colorR c)
      <*> channel 1 (colorG c)
      <*> channel 2 (colorB c)
      <*> channel 3 (colorA c)
   where
    channel i w = byte <$> f i (chan w)

chan :: Word8 -> Float
chan w = fromIntegral w / 255

byte :: Float -> Word8
byte x = fromIntegral (round (clamp01 x * 255) :: Int)
