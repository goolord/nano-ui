{-# LANGUAGE UnliftedFFITypes #-}

module NanoUI.Types
  ( V2 (..)
  , Rect (..)
  , Size (..)
  , Color (..)
  , colorRGBA
  , colorToWord32
  , rectContains
  , rectUnion
  , rectIntersect
  , v2Add
  , v2Sub
  ) where

import Data.Bits (shiftL, (.|.))
import Data.Word (Word8, Word32)

data V2 = V2
  { v2X :: {-# UNPACK #-} !Float
  , v2Y :: {-# UNPACK #-} !Float
  }
  deriving (Eq, Show)

data Size = Size
  { sizeW :: {-# UNPACK #-} !Float
  , sizeH :: {-# UNPACK #-} !Float
  }
  deriving (Eq, Show)

data Rect = Rect
  { rectX :: {-# UNPACK #-} !Float
  , rectY :: {-# UNPACK #-} !Float
  , rectW :: {-# UNPACK #-} !Float
  , rectH :: {-# UNPACK #-} !Float
  }
  deriving (Eq, Show)

newtype Color = Color Word32
  deriving (Eq, Show)

{-# INLINE colorRGBA #-}
colorRGBA :: Word8 -> Word8 -> Word8 -> Word8 -> Color
colorRGBA r g b a =
  Color $
    (word32Of r `shiftL` 24)
      .|. (word32Of g `shiftL` 16)
      .|. (word32Of b `shiftL` 8)
      .|. word32Of a

{-# INLINE colorToWord32 #-}
colorToWord32 :: Color -> Word32
colorToWord32 (Color w) = w

{-# INLINE word32Of #-}
word32Of :: Word8 -> Word32
word32Of = fromIntegral

{-# INLINE rectContains #-}
rectContains :: Rect -> V2 -> Bool
rectContains (Rect x y w h) (V2 px py) =
  px >= x && px < x + w && py >= y && py < y + h

{-# INLINE rectUnion #-}
rectUnion :: Rect -> Rect -> Rect
rectUnion (Rect x1 y1 w1 h1) (Rect x2 y2 w2 h2) =
  let x = min x1 x2
      y = min y1 y2
      xEnd = max (x1 + w1) (x2 + w2)
      yEnd = max (y1 + h1) (y2 + h2)
   in Rect x y (xEnd - x) (yEnd - y)

{-# INLINE rectIntersect #-}
rectIntersect :: Rect -> Rect -> Maybe Rect
rectIntersect (Rect x1 y1 w1 h1) (Rect x2 y2 w2 h2) =
  let x = max x1 x2
      y = max y1 y2
      xEnd = min (x1 + w1) (x2 + w2)
      yEnd = min (y1 + h1) (y2 + h2)
      w = xEnd - x
      h = yEnd - y
   in if w > 0 && h > 0 then Just (Rect x y w h) else Nothing

{-# INLINE v2Add #-}
v2Add :: V2 -> V2 -> V2
v2Add (V2 x1 y1) (V2 x2 y2) = V2 (x1 + x2) (y1 + y2)

{-# INLINE v2Sub #-}
v2Sub :: V2 -> V2 -> V2
v2Sub (V2 x1 y1) (V2 x2 y2) = V2 (x1 - x2) (y1 - y2)
