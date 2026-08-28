{-# LANGUAGE UnliftedFFITypes #-}

module NanoUI.Types
  ( V2 (..)
  , Rect (..)
  , Size (..)
  , Color (..)
  , colorRGBA
  , colorToWord32
  , colorR
  , colorG
  , colorB
  , colorA
  , lerpColor
  , colorLuminance
  , contrastRatio
  , ImageId (..)
  , rectContains
  , rectUnion
  , rectIntersect
  , rectOverlapArea
  , rectInflate
  , rectArea
  , Damage (..)
  , damageIsEmpty
  , sliderTrackRect
  , sliderTrackMargin
  , sliderBarCells
  , v2Add
  , v2Sub
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
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

newtype ImageId = ImageId
  { unImageId :: Int
  }
  deriving (Eq, Ord, Show)

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

{-# INLINE colorR #-}
colorR :: Color -> Word8
colorR (Color w) = fromIntegral ((w `shiftR` 24) .&. 0xFF)

{-# INLINE colorG #-}
colorG :: Color -> Word8
colorG (Color w) = fromIntegral ((w `shiftR` 16) .&. 0xFF)

{-# INLINE colorB #-}
colorB :: Color -> Word8
colorB (Color w) = fromIntegral ((w `shiftR` 8) .&. 0xFF)

{-# INLINE colorA #-}
colorA :: Color -> Word8
colorA (Color w) = fromIntegral (w .&. 0xFF)

-- | WCAG 2 relative-luminance contrast. 4.5 is AA for normal text.
--
-- Alpha is ignored, so both colours must be opaque. Passing a translucent
-- colour such as 'NanoUI.Style.themeOverlayDim' gives a meaningless ratio;
-- composite it over its backdrop first.
contrastRatio :: Color -> Color -> Double
contrastRatio a b =
  let hi = max (colorLuminance a) (colorLuminance b)
      lo = min (colorLuminance a) (colorLuminance b)
   in (hi + 0.05) / (lo + 0.05)

colorLuminance :: Color -> Double
colorLuminance = relLum

relLum :: Color -> Double
relLum c =
  0.2126 * srgb (colorR c) + 0.7152 * srgb (colorG c) + 0.0722 * srgb (colorB c)

{-# INLINE lerpColor #-}
lerpColor :: Color -> Color -> Float -> Color
lerpColor (Color a) (Color b) t =
  let u = max 0 (min 1 t)
      ch shift =
        round $
          fromIntegral ((a `shiftR` shift) .&. 0xFF) * (1 - u)
            + fromIntegral ((b `shiftR` shift) .&. 0xFF) * u
   in Color
        ( (ch 24 `shiftL` 24)
            .|. (ch 16 `shiftL` 16)
            .|. (ch 8 `shiftL` 8)
            .|. ch 0
        )

srgb :: Word8 -> Double
srgb ch =
  let x = fromIntegral ch / 255
   in if x <= 0.04045 then x / 12.92 else ((x + 0.055) / 1.055) ** 2.4

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

{-# INLINE rectOverlapArea #-}
rectOverlapArea :: Rect -> Rect -> Float
rectOverlapArea a b =
  maybe 0 (\r -> rectW r * rectH r) (rectIntersect a b)

{-# INLINE rectInflate #-}
rectInflate :: Float -> Rect -> Rect
rectInflate pad (Rect x y w h) =
  Rect (x - pad) (y - pad) (w + pad * 2) (h + pad * 2)

{-# INLINE rectArea #-}
rectArea :: Rect -> Float
rectArea (Rect _ _ w h) = w * h

-- Full window vs a scissor box around widgets that actually changed (hover, anim).
data Damage
  = DamageFull
  | DamageClip Rect
  deriving (Eq, Show)

{-# INLINE damageIsEmpty #-}
damageIsEmpty :: Damage -> Bool
damageIsEmpty dmg =
  case dmg of
    DamageFull -> False
    DamageClip r -> rectW r <= 0 || rectH r <= 0

-- End inset above and below the hit band (handle sits in this slack).
sliderTrackMargin :: Float
sliderTrackMargin = 3

-- Terminal inline slider bar width in cells (matches WidgetText.sliderText).
sliderBarCells :: Int
sliderBarCells = 12

{-# INLINE sliderTrackRect #-}
sliderTrackRect :: Float -> Float -> Float -> Float -> Rect
sliderTrackRect x y w h =
  let trackH = max 4 (h * 0.18)
      trackY = y + h - trackH - sliderTrackMargin
   in Rect x trackY w trackH

{-# INLINE v2Add #-}
v2Add :: V2 -> V2 -> V2
v2Add (V2 x1 y1) (V2 x2 y2) = V2 (x1 + x2) (y1 + y2)

{-# INLINE v2Sub #-}
v2Sub :: V2 -> V2 -> V2
v2Sub (V2 x1 y1) (V2 x2 y2) = V2 (x1 - x2) (y1 - y2)
