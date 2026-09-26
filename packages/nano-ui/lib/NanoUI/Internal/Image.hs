-- | Pure image geometry: fit, alignment, crop, zoom, opacity and rotation.
-- Covers the user options ('ImageConfig'), the per-node paint data
-- ('ImageLook', 'lookDraw') and the canvas image record ('ImageDraw').
module NanoUI.Internal.Image
  ( ContentFit (..)
  , Rotation (..)
  , rotationAngle
  , ImageConfig (..)
  , defaultImageConfig
  , fitRect
  , ImageLook (..)
  , imageLook
  , lookSize
  , lookDraw
  , ImageDraw (..)
  , imageDraw
  , imageDrawOp
  , fadeBy
  ) where

import Data.Maybe (fromMaybe)
import NanoUI.Internal.Draw.Types (DrawOp (..))
import NanoUI.Internal.Style (AlignX (..), AlignY (..), Layout, fadeAlpha)
import NanoUI.Internal.Types (Color, ImageId (..), Rect (..), clamp, clamp01, colorA, colorRGBA, finite, rectIntersect)

-- | How an image fills its layout rect, like CSS @object-fit@. Every fit but
-- 'FitFill' keeps the image's aspect ratio.
data ContentFit
  = -- | Largest size that fits inside the rect; may leave empty space.
    FitContain
  | -- | Smallest size that covers the rect, cropped to it.
    FitCover
  | -- | Stretched to the rect.
    FitFill
  | -- | Natural size, cropped to the rect.
    FitNone
  | -- | 'FitNone' when the image fits, else 'FitContain'.
    FitScaleDown
  deriving (Eq, Show, Enum, Bounded)

-- | Clockwise rotation in radians about the centre of the drawn image, and
-- whether the rotation affects layout.
data Rotation
  = -- | Fitted as if unrotated, then rotated. Layout is unchanged and corners
    -- outside the rect are cropped. For spinning icons.
    RotateFloating !Float
  | -- | Fitted by the rotated bounding box, so the whole image stays inside
    -- its rect (unless 'FitCover' or 'FitNone' crops it). An unsized axis
    -- takes the rotated extent.
    RotateSolid !Float
  deriving (Eq, Show)

rotationAngle :: Rotation -> Float
rotationAngle (RotateFloating a) = a
rotationAngle (RotateSolid a) = a

-- | Options for 'NanoUI.imageConfigured'.
data ImageConfig = ImageConfig
  { icLayout :: Layout -> Layout
  -- ^ Layout modifier. A 'NanoUI.Fit' axis (the default) takes the image's
  -- size: one image pixel per logical pixel, after 'icCrop' and any solid
  -- rotation. The image's ratio acts as 'NanoUI.aspect' unless the modifier
  -- sets one, so @fillW@ keeps the shape. 'NanoUI.fontColor' tints the
  -- image, as it does an SVG icon.
  , icFit :: !ContentFit
  -- ^ How the image fills its rect.
  , icAlignX :: !AlignX
  -- ^ Horizontal placement when the image does not fill its rect, and which
  -- side survives cropping.
  , icAlignY :: !AlignY
  -- ^ Vertical counterpart of 'icAlignX'. 'AlignBaseline' acts as 'AlignTop'.
  , icOpacity :: !Float
  -- ^ 0 (invisible) to 1 (opaque).
  , icRotation :: !Rotation
  , icCrop :: !(Maybe Rect)
  -- ^ Sub-rectangle to draw, in image pixels from the top-left, clamped to
  -- the image. Size, fit, alignment and rotation apply to this part only.
  -- 'Nothing' (the default) draws the whole image.
  , icScale :: !Float
  -- ^ Zoom about the fitted image's centre, clipped to its rect: above 1
  -- zooms in, below 1 out. Does not affect layout. Default 1.
  }

-- | Default layout, 'FitFill' (as 'NanoUI.image'), centred, opaque, with no
-- rotation, crop or zoom.
defaultImageConfig :: ImageConfig
defaultImageConfig =
  ImageConfig
    { icLayout = id
    , icFit = FitFill
    , icAlignX = AlignCenter
    , icAlignY = AlignMiddle
    , icOpacity = 1
    , icRotation = RotateFloating 0
    , icCrop = Nothing
    , icScale = 1
    }

-- | Rect of content @w@ by @h@ fitted into @box@ and aligned like CSS
-- @object-position@. The result may overflow the box ('FitCover',
-- 'FitNone'); the caller crops it.
--
-- > fitRect FitContain AlignCenter AlignMiddle (40, 20) (Rect 0 0 100 80) == Rect 0 15 100 50
fitRect :: ContentFit -> AlignX -> AlignY -> (Float, Float) -> Rect -> Rect
fitRect fit ax ay (w, h) (Rect bx by bw bh) = Rect (bx + (bw - dw) * fx) (by + (bh - dh) * fy) dw dh
  where
    empty = not (w > 0 && h > 0)
    contain = min (bw / w) (bh / h)
    scaled k = if empty then (0, 0) else (k * w, k * h)
    (dw, dh) = case fit of
      FitContain -> scaled contain
      FitCover -> scaled (max (bw / w) (bh / h))
      FitFill -> (bw, bh)
      FitNone -> scaled 1
      FitScaleDown -> scaled (min 1 contain)
    fx = case ax of
      AlignStart -> 0
      AlignCenter -> 0.5
      AlignEnd -> 1
    fy = case ay of
      AlignMiddle -> 0.5
      AlignBottom -> 1
      _ -> 0

-- | Bounding size after a solid rotation; floating rotations keep the size.
turnedSize :: Rotation -> (Float, Float) -> (Float, Float)
turnedSize (RotateSolid angle) (w, h) =
  let c = abs (cos angle)
      s = abs (sin angle)
   in (w * c + h * s, w * s + h * c)
turnedSize (RotateFloating _) size = size

-- | Unrotated draw rect of an @iw@ by @ih@ image in @box@; rotation is then
-- about its centre. Solid rotations fit by the rotated bounds. 'FitFill'
-- with a solid rotation stretches so the rotated bounds fill the box, falling
-- back to 'FitContain' at odd multiples of 45 degrees or when no stretch can
-- match the box's shape.
turnedRect :: ContentFit -> AlignX -> AlignY -> Rotation -> (Float, Float) -> Rect -> Rect
turnedRect fit ax ay rot (iw, ih) box@(Rect _ _ bw bh) = Rect (px + (pw - dw) / 2) (py + (ph - dh) / 2) dw dh
  where
    (rw, rh) = turnedSize rot (iw, ih)
    solidFill = case (fit, rot) of
      (FitFill, RotateSolid angle) ->
        let c = abs (cos angle)
            s = abs (sin angle)
            det = c * c - s * s
            w = (c * bw - s * bh) / det
            h = (c * bh - s * bw) / det
         in if abs det > 1.0e-3 && w > 0 && h > 0 then Just (w, h) else Nothing
      _ -> Nothing
    fit' = case (fit, rot, solidFill) of
      (FitFill, RotateSolid _, Nothing) -> FitContain
      _ -> fit
    Rect px py pw ph = fitRect fit' ax ay (rw, rh) box
    (dw, dh) = fromMaybe (if rw > 0 && rh > 0 then (iw * pw / rw, ih * ph / rh) else (0, 0)) solidFill

-- | Crop rect clamped to an @iw@ by @ih@ image, at least one pixel each way.
-- The whole image when there is no crop.
cropRegion :: Maybe Rect -> (Int, Int) -> Rect
cropRegion crop (iw, ih) = case crop of
  Nothing -> Rect 0 0 w h
  Just (Rect x y cw ch) ->
    let x' = clamp 0 (w - 1) x
        y' = clamp 0 (h - 1) y
     in Rect x' y' (clamp 1 (w - x') cw) (clamp 1 (h - y') ch)
  where
    w = fromIntegral iw
    h = fromIntegral ih

-- | Paint settings for an image node: its 'ImageConfig' fields plus tint.
-- Paint draws it with 'lookDraw'; damage repaints a node when its look
-- changes.
data ImageLook = ImageLook
  { lookFit :: !ContentFit
  , lookAlignX :: !AlignX
  , lookAlignY :: !AlignY
  , lookOpacity :: !Float
  , lookRotation :: !Rotation
  , lookCrop :: !(Maybe Rect)
  , lookScale :: !Float
  , lookTint :: !Color
  }
  deriving (Eq, Show)

-- | Build a look from a config and tint. NaN opacity or scale and
-- non-finite rotation fall back to their defaults.
imageLook :: ImageConfig -> Color -> ImageLook
imageLook cfg =
  ImageLook
    (icFit cfg)
    (icAlignX cfg)
    (icAlignY cfg)
    (unitOpacity (icOpacity cfg))
    (if finite (rotationAngle (icRotation cfg)) then icRotation cfg else RotateFloating 0)
    (icCrop cfg)
    (if icScale cfg > 0 && finite (icScale cfg) then icScale cfg else 1)

-- | Clamp to [0, 1], NaN to 1.
unitOpacity :: Float -> Float
unitOpacity o = if isNaN o then 1 else clamp01 o

-- | Scale a colour's alpha by an opacity in [0, 1].
fadeBy :: Float -> Color -> Color
fadeBy o c
  | o >= 1 = c
  | otherwise = fadeAlpha c (round (fromIntegral (colorA c) * o))

-- | Natural size of an image node for unsized axes: the cropped image size,
-- rotated if the rotation is solid.
lookSize :: ImageLook -> (Int, Int) -> (Float, Float)
lookSize look size =
  let Rect _ _ w h = cropRegion (lookCrop look) size in turnedSize (lookRotation look) (w, h)

-- | Draw record for image @iid@ (@iw@ by @ih@ pixels) in @box@, with
-- opacity scaled by @fade@ (below 1 when disabled). An unrotated image is
-- cut to the box by adjusting its UVs; a rotated one can extend past the
-- box and the caller must clip it. 'Nothing' when nothing is visible.
lookDraw :: ImageLook -> (Int, Int) -> ImageId -> Float -> Rect -> Maybe ImageDraw
lookDraw look size@(iw, ih) iid fade box
  | lookOpacity look * fade <= 0 || colorA (lookTint look) == 0 = Nothing
  | turned = Just draw
  | otherwise = case rectIntersect dest box of
      Just (Rect x y w h)
        | dw > 0 && dh > 0 && w > 0 && h > 0 ->
            -- Shrink the UVs to match the visible part of the rect.
            let Rect u v uw vh = uvCrop
             in Just draw {imageRect = Rect x y w h, imageUV = Rect (u + (x - dx) / dw * uw) (v + (y - dy) / dh * vh) (w / dw * uw) (h / dh * vh)}
      _ -> Nothing
  where
    Rect cx cy cw ch = cropRegion (lookCrop look) size
    uvCrop = Rect (cx / fromIntegral iw) (cy / fromIntegral ih) (cw / fromIntegral iw) (ch / fromIntegral ih)
    rot = lookRotation look
    angle = rotationAngle rot
    turned = not (abs (sin angle) < 1.0e-6 && cos angle > 0)
    Rect fx fy fw fh = turnedRect (lookFit look) (lookAlignX look) (lookAlignY look) rot (cw, ch) box
    k = lookScale look
    dest@(Rect dx dy dw dh) = Rect (fx + fw * (1 - k) / 2) (fy + fh * (1 - k) / 2) (fw * k) (fh * k)
    draw = ImageDraw dest iid uvCrop (if turned then angle else 0) (lookTint look) (lookOpacity look * fade)

-- | An image for 'NanoUI.Widgets.Custom.drawImageWith'. Start from
-- 'imageDraw' and override fields:
--
-- > drawImageWith (imageDraw r photo) {imageAngle = t, imageOpacity = 0.5}
data ImageDraw = ImageDraw
  { imageRect :: !Rect
  -- ^ Destination rect, before rotation.
  , imageId :: !ImageId
  -- ^ An image registered with the context.
  , imageUV :: !Rect
  -- ^ Source region in UVs (0 to 1): @Rect 0 0 1 1@ is the whole image,
  -- @Rect 0.5 0 0.5 1@ its right half.
  , imageAngle :: !Float
  -- ^ Clockwise rotation in radians about the rect's centre. At 0 the image
  -- snaps to the pixel grid like a rect.
  , imageTint :: !Color
  -- ^ Multiplied into each pixel. White leaves the image unchanged; a
  -- single-colour white image takes the tint's colour.
  , imageOpacity :: !Float
  -- ^ 0 (invisible) to 1 (the tint's own alpha).
  }
  deriving (Eq, Show)

-- | The whole image @iid@ over @r@: unrotated, untinted, opaque.
imageDraw :: Rect -> ImageId -> ImageDraw
imageDraw r iid = ImageDraw r iid (Rect 0 0 1 1) 0 (colorRGBA 255 255 255 255) 1

-- | Draw op with opacity folded into the tint alpha and a non-finite angle
-- treated as 0. 'Nothing' when invisible.
imageDrawOp :: ImageDraw -> Maybe DrawOp
imageDrawOp (ImageDraw r (ImageId tid) (Rect u v uw vh) angle tint opacity)
  | colorA tint' == 0 = Nothing
  | otherwise = Just (DrawImage r (if finite angle then angle else 0) tid u v (u + uw) (v + vh) tint')
  where
    tint' = fadeBy (unitOpacity opacity) tint
