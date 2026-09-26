-- | Geometry in logical pixels, packed RGBA colours, and repaint bounds.
-- Window coordinates start at the top-left, with x rightward and y downward.
module NanoUI.Internal.Types
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
  , rgbToHsv
  , hsvToRgb
  , clamp
  , clamp01
  , finite
  , onGrid
  , gridSpan
  , roundHalfUp
  , lerpColor
  , colorLuminance
  , contrastRatio
  , ImageId (..)
  , rectContains
  , rectNonEmpty
  , rectHit
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
  , v2Add
  , v2Sub
  , PopupAnchor (..)
  , PopupPlacement (..)
  , foldUpTo
  , forUpTo_
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Word (Word8, Word32)

-- | A two-component point, offset, or vector; units depend on its use.
data V2 = V2
  { v2X :: {-# UNPACK #-} !Float
  , v2Y :: {-# UNPACK #-} !Float
  }
  deriving (Eq, Show)

-- | Width and height in logical pixels.
data Size = Size
  { sizeW :: {-# UNPACK #-} !Float
  , sizeH :: {-# UNPACK #-} !Float
  }
  deriving (Eq, Show)

-- | Top-left origin, width, and height in logical pixels. Hit tests include
-- the left and top edges and exclude the right and bottom edges.
data Rect = Rect
  { rectX :: {-# UNPACK #-} !Float
  , rectY :: {-# UNPACK #-} !Float
  , rectW :: {-# UNPACK #-} !Float
  , rectH :: {-# UNPACK #-} !Float
  }
  deriving (Eq, Show)

-- | An image registered in a context's atlas. It is not a native texture handle.
newtype ImageId = ImageId
  { unImageId :: Int
  }
  deriving (Eq, Ord, Show)

-- | Straight-alpha colour packed as @0xRRGGBBAA@, with 8 bits per channel.
newtype Color = Color Word32
  deriving (Eq, Show, Num)

-- | Pack red, green, blue, and alpha channels; alpha 0 is transparent, 255 opaque.
{-# INLINE colorRGBA #-}
colorRGBA :: Word8 -> Word8 -> Word8 -> Word8 -> Color
colorRGBA r g b a =
  Color $
    (fromIntegral r `shiftL` 24)
      .|. (fromIntegral g `shiftL` 16)
      .|. (fromIntegral b `shiftL` 8)
      .|. fromIntegral a

-- | The packed @0xRRGGBBAA@ representation.
{-# INLINE colorToWord32 #-}
colorToWord32 :: Color -> Word32
colorToWord32 (Color w) = w

-- | Red channel, in the range 0-255.
{-# INLINE colorR #-}
colorR :: Color -> Word8
colorR (Color w) = fromIntegral ((w `shiftR` 24) .&. 0xFF)

-- | Green channel, in the range 0-255.
{-# INLINE colorG #-}
colorG :: Color -> Word8
colorG (Color w) = fromIntegral ((w `shiftR` 16) .&. 0xFF)

-- | Blue channel, in the range 0-255.
{-# INLINE colorB #-}
colorB :: Color -> Word8
colorB (Color w) = fromIntegral ((w `shiftR` 8) .&. 0xFF)

-- | Alpha channel, from 0 (transparent) to 255 (opaque).
{-# INLINE colorA #-}
colorA :: Color -> Word8
colorA (Color w) = fromIntegral (w .&. 0xFF)

-- | Restrict a value to inclusive lower and upper bounds, which must be ordered.
{-# INLINE clamp #-}
clamp :: Ord a => a -> a -> a -> a
clamp lo hi x = max lo (min hi x)

-- | Restrict a value to the inclusive range 0-1.
{-# INLINE clamp01 #-}
clamp01 :: Float -> Float
clamp01 x = clamp 0 1 x

-- | A number: neither NaN nor infinite.
{-# INLINE finite #-}
finite :: Float -> Bool
finite v = not (isNaN v || isInfinite v)

-- | Round a logical coordinate onto the device-pixel grid implied by draw
-- scale @s@ (device px = logical * s). Every layer that positions pixels --
-- the layout solve, text pens, scroll offsets, paint and glyph rasterization --
-- must route its coordinates through this single function (backends through
-- 'roundHalfUp'), so geometry can never dephase from text. An identity when
-- @s <= 0@ (no scaling).
{-# INLINE onGrid #-}
onGrid :: Float -> Float -> Float
onGrid s v
  | s > 0 = fromIntegral (roundHalfUp (v * s)) / s
  | otherwise = v

-- | Size between two edges once both are snapped with 'onGrid'. Counted in
-- device pixels and scaled once, so an edge pair a whole number of device
-- pixels apart gives that size exactly; subtracting two 'onGrid' results
-- can land a float step off it. An identity on @b - a@ when @s <= 0@.
{-# INLINE gridSpan #-}
gridSpan :: Float -> Float -> Float -> Float
gridSpan s a b
  | s > 0 = fromIntegral (roundHalfUp (b * s) - roundHalfUp (a * s)) / s
  | otherwise = b - a

-- | Round to the nearest integer, ties up: the device-pixel rounding shared by
-- 'onGrid' and the backends. Not ties-to-even (@round@): at a fractional scale
-- (125%: a 20px row is 25 device px) a column of rows can all sit on half
-- pixels, and ties-to-even would alternate them down and up, leaving uneven
-- gaps. Compares the exact fractional part rather than @floor (r + 0.5)@,
-- whose addition itself rounds: it lifts the float just below 0.5 to 1 and
-- odd integers past 2^23 up by one.
{-# INLINE roundHalfUp #-}
roundHalfUp :: Float -> Int
roundHalfUp r =
  let f = floor r
   in if r - fromIntegral f >= 0.5 then f + 1 else f

-- | Hue in degrees (0-360), saturation and value in 0-1. Alpha is ignored;
-- grey colours have hue 0.
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
      h
        | delta <= 0 = 0
        | maxC == r =
            let t = (g - b) / delta
             in if t < 0 then 60 * (t + 6) else 60 * t
        | maxC == g = 60 * (((b - r) / delta) + 2)
        | otherwise = 60 * (((r - g) / delta) + 4)
   in (h, s, v)

-- | Convert hue in degrees and saturation/value in 0-1 to an opaque colour.
-- Hue wraps every 360 degrees; output channels are clamped.
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
-- colour such as 'NanoUI.Internal.Style.themeOverlayDim' gives a meaningless ratio;
-- composite it over its backdrop first.
contrastRatio :: Color -> Color -> Double
contrastRatio a b =
  let hi = max (colorLuminance a) (colorLuminance b)
      lo = min (colorLuminance a) (colorLuminance b)
   in (hi + 0.05) / (lo + 0.05)

-- | Relative luminance after sRGB linearisation, in 0-1. Ignores alpha.
colorLuminance :: Color -> Double
colorLuminance c =
  0.2126 * srgb (colorR c) + 0.7152 * srgb (colorG c) + 0.0722 * srgb (colorB c)

-- | Interpolate all four packed channels. The factor is clamped to 0-1;
-- interpolation is in sRGB channel space, not linear light.
lerpColor :: Color -> Color -> Float -> Color
lerpColor (Color a) (Color b) t =
  let u = clamp01 t
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

-- | Test a point against half-open rectangle bounds.
{-# INLINE rectContains #-}
rectContains :: Rect -> V2 -> Bool
rectContains (Rect x y w h) (V2 px py) =
  px >= x && px < x + w && py >= y && py < y + h

-- | Whether both width and height are strictly positive.
{-# INLINE rectNonEmpty #-}
rectNonEmpty :: Rect -> Bool
rectNonEmpty r = rectW r > 0 && rectH r > 0

-- | Hit test that rejects empty and negative-size rectangles.
{-# INLINE rectHit #-}
rectHit :: Rect -> V2 -> Bool
rectHit r p = rectNonEmpty r && rectContains r p

-- | Smallest bounding rectangle containing both inputs. Empty inputs are
-- still included by their coordinates; filter them first if they mean no area.
{-# INLINE rectUnion #-}
rectUnion :: Rect -> Rect -> Rect
rectUnion (Rect x1 y1 w1 h1) (Rect x2 y2 w2 h2) =
  let x = min x1 x2
      y = min y1 y2
      xEnd = max (x1 + w1) (x2 + w2)
      yEnd = max (y1 + h1) (y2 + h2)
   in Rect x y (xEnd - x) (yEnd - y)

-- | Shared positive-area rectangle, or 'Nothing' for disjoint or touching edges.
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

-- | Whether the first rectangle has positive size and lies inside the second.
{-# INLINE rectFullyInside #-}
rectFullyInside :: Rect -> Rect -> Bool
rectFullyInside (Rect ix iy iw ih) (Rect ox oy ow oh) =
  iw > 0
    && ih > 0
    && ix >= ox
    && iy >= oy
    && ix + iw <= ox + ow
    && iy + ih <= oy + oh

-- | Shared area, or zero when there is no positive-area intersection.
{-# INLINE rectOverlapArea #-}
rectOverlapArea :: Rect -> Rect -> Float
rectOverlapArea a b =
  maybe 0 rectArea (rectIntersect a b)

-- | Extend every edge by the margin. A negative margin shrinks the rectangle.
{-# INLINE rectInflate #-}
rectInflate :: Float -> Rect -> Rect
rectInflate pad (Rect x y w h) =
  Rect (x - pad) (y - pad) (w + pad * 2) (h + pad * 2)

-- | Width times height. Requires non-negative dimensions for a geometric area.
{-# INLINE rectArea #-}
rectArea :: Rect -> Float
rectArea (Rect _ _ w h) = w * h

-- | Region to repaint: the whole window or a clip in logical window coordinates.
-- An empty 'DamageClip' means no repaint is needed.
data Damage
  = DamageFull
  | DamageClip Rect
  deriving (Eq, Show)

-- | Whether damage is a clip with non-positive width or height.
{-# INLINE damageIsEmpty #-}
damageIsEmpty :: Damage -> Bool
damageIsEmpty dmg =
  case dmg of
    DamageFull -> False
    DamageClip r -> rectW r <= 0 || rectH r <= 0

-- | Invalidation bounding strategy for a widget and its interaction events.
data DamageBounds
  = DamageSelf                              -- ^ Exact layout bounding box Rect
  | DamageInflated {-# UNPACK #-} !Float    -- ^ Layout bounding box inflated by margin (focus rings, shadows, text slop)
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
resolveDamageRect :: DamageBounds -> Rect -> Rect
resolveDamageRect bounds r =
  case bounds of
    DamageSelf -> r
    DamageInflated pad -> rectInflate pad r
    DamageExact exactR -> exactR
    DamageCustom f -> f r
    DamageUnion a b ->
      -- An empty side (DamageNone, or an unlaid-out rect) contributes
      -- nothing; a plain rect union would stretch the damage to the origin.
      let ra = resolveDamageRect a r
          rb = resolveDamageRect b r
       in if not (rectNonEmpty ra)
            then rb
            else if not (rectNonEmpty rb) then ra else rectUnion ra rb
    DamageNone -> Rect 0 0 0 0

-- | Add corresponding components, for example a point and an offset.
{-# INLINE v2Add #-}
v2Add :: V2 -> V2 -> V2
v2Add (V2 x1 y1) (V2 x2 y2) = V2 (x1 + x2) (y1 + y2)

-- | Subtract corresponding components, for example the offset between points.
{-# INLINE v2Sub #-}
v2Sub :: V2 -> V2 -> V2
v2Sub (V2 x1 y1) (V2 x2 y2) = V2 (x1 - x2) (y1 - y2)

-- | Point or rectangle a popup is placed relative to, in logical window coordinates.
data PopupAnchor
  = AnchorPoint !V2
  | AnchorRect !Rect
  deriving (Eq, Show)

-- | Preferred side of a popup anchor. Placement also accounts for available
-- window space; 'PlacementAuto' lets the positioner choose a side.
data PopupPlacement
  = PlacementBelow
  | PlacementAbove
  | PlacementRight
  | PlacementLeft
  | PlacementAtCursor
  -- ^ For a tooltip ('NanoUI.tooltipPlacement'), just below the pointer,
  -- following it as it moves. For a popup or a context menu, at the anchor
  -- point itself, its top-left corner there, where the menu opens at the
  -- pointer of the click that opened it and stays.
  | PlacementAuto
  deriving (Eq, Show)

-- | Strict left fold over @0 .. n - 1@.
{-# INLINE foldUpTo #-}
foldUpTo :: Int -> (a -> Int -> IO a) -> a -> IO a
foldUpTo n f = go 0
  where
    go !i !acc
      | i >= n = pure acc
      | otherwise = f acc i >>= go (i + 1)

-- | Run @f@ on @0 .. n - 1@ in order, without allocating a range list.
{-# INLINE forUpTo_ #-}
forUpTo_ :: Int -> (Int -> IO ()) -> IO ()
forUpTo_ n f = foldUpTo n (\() i -> f i) ()
