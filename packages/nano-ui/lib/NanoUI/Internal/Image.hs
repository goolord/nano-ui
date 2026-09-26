-- | Images drawn fitted, aligned, cropped, zoomed, faded and turned: the
-- options an image takes ('ImageConfig'), what paint draws an image node
-- with ('ImageLook', 'lookDraw'), the canvas's image record ('ImageDraw'),
-- and the geometry behind them ('fitRect'). Everything here is pure.
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
  ) where

import Data.Maybe (fromMaybe)
import NanoUI.Internal.Draw.Types (DrawOp (..))
import NanoUI.Internal.Style (AlignX (..), AlignY (..), Layout, fadeAlpha)
import NanoUI.Internal.Types (Color, ImageId (..), Rect (..), clamp, colorA, colorRGBA, rectIntersect)

-- | How an image fills the rect its layout gives it, as CSS's @object-fit@
-- does. The image keeps its own shape under every fit but 'FitFill'.
data ContentFit
  = -- | As large as fits inside the rect. Where the shapes differ, part of
    -- the rect stays empty.
    FitContain
  | -- | As small as covers the rect, and cropped to it.
    FitCover
  | -- | Stretched to the rect.
    FitFill
  | -- | At its own size, and cropped to the rect.
    FitNone
  | -- | 'FitNone' when the image fits, else 'FitContain'.
    FitScaleDown
  deriving (Eq, Show, Enum, Bounded)

-- | How far an image is turned, in radians, clockwise on screen about the
-- centre of where it is drawn, and whether the turn takes room.
data Rotation
  = -- | Fitted to its rect as if it were not turned, then turned: its layout
    -- stays what it was, and a corner that leaves the rect is cropped. For an
    -- icon that spins.
    RotateFloating !Float
  | -- | Fitted by its turned bounding box, so the whole image stays inside
    -- its rect (unless 'FitCover' or 'FitNone' crops it), and an axis the
    -- layout leaves unsized takes the turned image's extent.
    RotateSolid !Float
  deriving (Eq, Show)

-- | The angle of a rotation, in radians.
rotationAngle :: Rotation -> Float
rotationAngle (RotateFloating a) = a
rotationAngle (RotateSolid a) = a

-- | Options for 'NanoUI.imageConfigured'.
data ImageConfig = ImageConfig
  { icLayout :: Layout -> Layout
  -- ^ The image's layout, a modifier of the default layout. An axis left at
  -- 'NanoUI.Fit', the default, takes the image's own size: one pixel of the
  -- image a logical pixel, of the part 'icCrop' keeps, turned by a solid
  -- rotation. A fit height follows the width the layout gives the image in
  -- the image's shape, and a fit width a fixed height, as 'NanoUI.aspect'
  -- makes them, unless the modifier gives an aspect of its own: @fillW@
  -- fills the width and keeps the shape. 'NanoUI.fontColor' tints the
  -- image, as it does an SVG icon.
  , icFit :: !ContentFit
  -- ^ How the image fills its rect.
  , icAlignX :: !AlignX
  -- ^ Where the fitted image sits across its rect when it does not fill
  -- it, and which part of it a crop keeps.
  , icAlignY :: !AlignY
  -- ^ The same, up and down. 'AlignBaseline' is 'AlignTop'.
  , icOpacity :: !Float
  -- ^ From 0, invisible, to 1, opaque.
  , icRotation :: !Rotation
  , icCrop :: !(Maybe Rect)
  -- ^ The part of the image to draw, in the image's pixels from its
  -- top-left corner, kept within the image: the image is drawn as if it
  -- were only that part, which its size, fit, alignment and rotation are
  -- of. 'Nothing', the default, draws all of it.
  , icScale :: !Float
  -- ^ How much larger than its fit the image is drawn, about the centre of
  -- where the fit puts it, and cut to its rect: above 1 zooms in, below 1
  -- out, as a zoom that animates. It takes no room of its own. 1 by default.
  }

-- | The default layout, stretched to it as 'NanoUI.image' is, centred,
-- opaque, not turned, not cropped and not zoomed.
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

-- | Where content @w@ by @h@ goes in @box@ under a fit: the whole of it,
-- placed where the fit leaves room, or where it overflows the box ('FitCover',
-- 'FitNone'), by the alignment, as CSS's @object-position@ does. What reaches
-- past the box is for the caller to crop.
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

-- | The room an image of this size takes turned: its bounding box under a
-- solid rotation, else the size itself.
turnedSize :: Rotation -> (Float, Float) -> (Float, Float)
turnedSize (RotateSolid angle) (w, h) =
  let c = abs (cos angle)
      s = abs (sin angle)
   in (w * c + h * s, w * s + h * c)
turnedSize (RotateFloating _) size = size

-- | Where an image @iw@ by @ih@ is drawn in @box@, before it turns about
-- the rect's centre: fitted and aligned by its turned bounds under a solid
-- rotation, and as if not turned under a floating one. 'FitFill' under a
-- solid rotation stretches the image so that its turned bounds are the box,
-- except at an odd multiple of an eighth turn, or where the box's shape is
-- out of the turn's reach, where it fits as 'FitContain' does.
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

-- | The part of an image @iw@ by @ih@ pixels that a crop keeps, within the
-- image and at least a pixel each way: all of it without one.
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

-- | How an image node draws its image, beside its id: what an
-- 'ImageConfig' says of it, and its tint. Paint draws a node that has one
-- ('lookDraw'), and the frame's damage repaints a node whose look changes
-- where it stands.
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

-- | The look of an image drawn by a configuration in a tint. An opacity or
-- a scale that is not a number, and a turn that is not finite, are taken as
-- none.
imageLook :: ImageConfig -> Color -> ImageLook
imageLook cfg =
  ImageLook
    (icFit cfg)
    (icAlignX cfg)
    (icAlignY cfg)
    (if isNaN (icOpacity cfg) then 1 else clamp 0 1 (icOpacity cfg))
    (if isNaN angle || isInfinite angle then RotateFloating 0 else icRotation cfg)
    (icCrop cfg)
    (if icScale cfg > 0 && not (isInfinite (icScale cfg)) then icScale cfg else 1)
  where
    angle = rotationAngle (icRotation cfg)

-- | The size an image node with this look takes where its layout leaves an
-- axis unsized, its image @iw@ by @ih@ pixels: the part its crop keeps,
-- turned by a solid rotation.
lookSize :: ImageLook -> (Int, Int) -> (Float, Float)
lookSize look size =
  let Rect _ _ w h = cropRegion (lookCrop look) size in turnedSize (lookRotation look) (w, h)

-- | What an image node with this look draws of image @iid@, @iw@ by @ih@
-- pixels, in @box@, its opacity scaled by @fade@ (below 1 for a disabled
-- node): the part its crop keeps, fitted and aligned in the box, scaled
-- about its centre, and turned. An unturned image is cut to the box by its
-- UVs; a turned one reaches past the box, and its caller clips it there.
-- 'Nothing' when nothing of it shows.
lookDraw :: ImageLook -> (Int, Int) -> ImageId -> Float -> Rect -> Maybe ImageDraw
lookDraw look size@(iw, ih) iid fade box
  | lookOpacity look * fade <= 0 || colorA (lookTint look) == 0 = Nothing
  | turned = Just draw
  | otherwise = case rectIntersect dest box of
      Just (Rect x y w h)
        | dw > 0 && dh > 0 && w > 0 && h > 0 ->
            -- The part of the UVs that the part of the rect in the box shows.
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

-- | An image for the canvas to draw ('NanoUI.Widgets.Custom.drawImageWith'),
-- built from 'imageDraw' with the fields that differ changed:
--
-- > drawImageWith (imageDraw r photo) {imageAngle = t, imageOpacity = 0.5}
data ImageDraw = ImageDraw
  { imageRect :: !Rect
  -- ^ Where the image goes, before it turns.
  , imageId :: !ImageId
  -- ^ The image, registered with the context.
  , imageUV :: !Rect
  -- ^ The part of the image drawn over the rect, in UVs, which run from 0
  -- to 1 across the image: @Rect 0 0 1 1@ for all of it, and
  -- @Rect 0.5 0 0.5 1@ for its right half.
  , imageAngle :: !Float
  -- ^ How far it turns about the rect's centre, in radians clockwise on
  -- screen. 0 snaps it to the pixel grid, as a rect is.
  , imageTint :: !Color
  -- ^ The colour its pixels are multiplied by: white leaves them as they
  -- are, and a one-colour image drawn in white takes the tint's colour.
  , imageOpacity :: !Float
  -- ^ From 0, invisible, to 1, the tint's own alpha.
  }
  deriving (Eq, Show)

-- | All of image @iid@ over @r@, unturned, untinted and opaque.
imageDraw :: Rect -> ImageId -> ImageDraw
imageDraw r iid = ImageDraw r iid (Rect 0 0 1 1) 0 (colorRGBA 255 255 255 255) 1

-- | The draw op for an image: its opacity folded into its tint's alpha, and
-- a turn that is not finite taken as none. 'Nothing' for an invisible one.
imageDrawOp :: ImageDraw -> Maybe DrawOp
imageDrawOp (ImageDraw r (ImageId tid) (Rect u v uw vh) angle tint opacity)
  | colorA tint' == 0 = Nothing
  | otherwise = Just (DrawImage r angle' tid u v (u + uw) (v + vh) tint')
  where
    o = if isNaN opacity then 1 else clamp 0 1 opacity
    tint' = if o >= 1 then tint else fadeAlpha tint (round (fromIntegral (colorA tint) * o))
    angle' = if isNaN angle || isInfinite angle then 0 else angle
