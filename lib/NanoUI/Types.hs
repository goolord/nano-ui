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
  , colorFromWord32
  , rgbToHsv
  , hsvToRgb
  , clamp01
  , lerpColor
  , colorLuminance
  , contrastRatio
  , ImageId (..)
  , rectContains
  , rectUnion
  , rectIntersect
  , rectFullyInside
  , rectOverlapArea
  , rectInflate
  , rectArea
  , Damage (..)
  , DamageBounds (..)
  , defaultDamageSlop
  , sliderDamageSlop
  , haloDamageSlop
  , resolveDamageRect
  , damageIsEmpty
  , sliderBarCells
  , v2Add
  , v2Sub
  , PopupAnchor (..)
  , PopupPlacement (..)
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

{-# INLINE colorFromWord32 #-}
colorFromWord32 :: Word32 -> Color
colorFromWord32 = Color

{-# INLINE clamp01 #-}
clamp01 :: Float -> Float
clamp01 x = max 0 (min 1 x)

{-# INLINE rgbToHsv #-}
rgbToHsv :: Color -> (Float, Float, Float)
rgbToHsv c =
  let r = fromIntegral (colorR c) / 255
      g = fromIntegral (colorG c) / 255
      b = fromIntegral (colorB c) / 255
      maxC = max r (max g b)
      minC = min r (min g b)
      delta = maxC - minC
      v = maxC
      s = if maxC <= 0 then 0 else delta / maxC
      rawH
        | delta <= 0 = 0
        | maxC == r =
            let t = (g - b) / delta
             in if t < 0 then 60 * (t + 6) else 60 * t
        | maxC == g = 60 * (((b - r) / delta) + 2)
        | otherwise = 60 * (((r - g) / delta) + 4)
      h = if rawH < 0 then rawH + 360 else rawH
   in (h, s, v)

{-# INLINE hsvToRgb #-}
hsvToRgb :: Float -> Float -> Float -> Color
hsvToRgb h s v =
  let hi = floor (h / 60) :: Int
      f = h / 60 - fromIntegral hi
      p = v * (1 - s)
      q = v * (1 - f * s)
      t = v * (1 - (1 - f) * s)
      (r, g, b) =
        case hi `mod` 6 of
          0 -> (v, t, p)
          1 -> (q, v, p)
          2 -> (p, v, t)
          3 -> (p, q, v)
          4 -> (t, p, v)
          _ -> (v, p, q)
      toCh x = round (clamp01 x * 255) :: Word8
   in colorRGBA (toCh r) (toCh g) (toCh b) 255

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

{-# INLINE rectFullyInside #-}
rectFullyInside :: Rect -> Rect -> Bool
rectFullyInside (Rect ix iy iw ih) (Rect ox oy ow oh) =
  iw > 0
    && ih > 0
    && ix >= ox
    && iy >= oy
    && ix + iw <= ox + ow
    && iy + ih <= oy + oh

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

-- | Invalidation bounding strategy for a widget and its interaction events.
data DamageBounds
  = DamageSelf                              -- ^ Exact layout bounding box Rect
  | DamageInflated !Float                   -- ^ Layout bounding box inflated by margin (focus rings, shadows, text slop)
  | DamageExact !Rect                       -- ^ Explicit rectangle in window space
  | DamageCustom (Rect -> Rect)             -- ^ Custom transformation on layout bounding box
  | DamageUnion !DamageBounds !DamageBounds -- ^ Combined invalidation bounds
  | DamageNone                              -- ^ No invalidation bounds

instance Show DamageBounds where
  show DamageSelf = "DamageSelf"
  show (DamageInflated f) = "DamageInflated " ++ show f
  show (DamageExact r) = "DamageExact " ++ show r
  show (DamageCustom _) = "DamageCustom <fn>"
  show (DamageUnion a b) = "DamageUnion (" ++ show a ++ ") (" ++ show b ++ ")"
  show DamageNone = "DamageNone"

instance Eq DamageBounds where
  DamageSelf == DamageSelf = True
  DamageInflated a == DamageInflated b = a == b
  DamageExact a == DamageExact b = a == b
  DamageUnion a1 b1 == DamageUnion a2 b2 = a1 == a2 && b1 == b2
  DamageNone == DamageNone = True
  _ == _ = False

instance Semigroup DamageBounds where
  (<>) = DamageUnion

instance Monoid DamageBounds where
  mempty = DamageSelf

-- | Standard damage slop for text overhang, focus rings, and border anti-aliasing.
defaultDamageSlop :: Float
defaultDamageSlop = 4.0

-- | Damage slop for slider handles that extend past track bounds.
sliderDamageSlop :: Float
sliderDamageSlop = 8.0

-- | Damage slop for window resize halos and shadows.
haloDamageSlop :: Float
haloDamageSlop = 12.0

-- | Resolve damage bounds against a given layout rect.
{-# INLINE resolveDamageRect #-}
resolveDamageRect :: DamageBounds -> Rect -> Rect
resolveDamageRect bounds r =
  case bounds of
    DamageSelf -> r
    DamageInflated pad -> rectInflate pad r
    DamageExact exactR -> exactR
    DamageCustom f -> f r
    DamageUnion a b -> rectUnion (resolveDamageRect a r) (resolveDamageRect b r)
    DamageNone -> Rect 0 0 0 0

-- Terminal inline slider bar width in cells (matches WidgetText.sliderText).
sliderBarCells :: Int
sliderBarCells = 12

{-# INLINE v2Add #-}
v2Add :: V2 -> V2 -> V2
v2Add (V2 x1 y1) (V2 x2 y2) = V2 (x1 + x2) (y1 + y2)

{-# INLINE v2Sub #-}
v2Sub :: V2 -> V2 -> V2
v2Sub (V2 x1 y1) (V2 x2 y2) = V2 (x1 - x2) (y1 - y2)

data PopupAnchor
  = AnchorPoint !V2
  | AnchorRect !Rect
  deriving (Eq, Show)

data PopupPlacement
  = PlacementBelow
  | PlacementAbove
  | PlacementRight
  | PlacementLeft
  | PlacementAtCursor
  | PlacementAuto
  deriving (Eq, Show)
