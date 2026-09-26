-- | Images drawn with a content fit, an opacity and a rotation: drawings
-- whose image ops name the image by its id, which paint finds in the atlas.
module NanoUI.Internal.Widgets.Image
  ( ContentFit (..)
  , Rotation (..)
  , rotationAngle
  , ImageConfig (..)
  , defaultImageConfig
  , imageConfigured
  , imageConfigured'
  )
where

import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Data.Primitive.SmallArray (SmallArray, emptySmallArray, smallArrayFromList)
import Effectful (Eff, type (:>))
import NanoUI.Internal.Context
import NanoUI.Internal.Draw (DrawOp (..))
import NanoUI.Internal.Monad (Ui, freshWidget, uiIO)
import NanoUI.Internal.Style
import NanoUI.Internal.Types
import NanoUI.Internal.Widgets.Node (Response)
import NanoUI.Widgets.Custom (CustomWidgetSpec (..), contentKeyOf, customWidgetWithId, defaultCustomWidgetSpec, keyPart)

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

-- | Options for 'imageConfigured'.
data ImageConfig = ImageConfig
  { icLayout :: !Layout
  -- ^ The widget's layout. An axis left at 'Fit', the default, takes the
  -- image's own size: one pixel of the image a logical pixel. Beside a fixed
  -- size on the other axis it keeps the image's shape instead; beside a
  -- growing one it does not, and stays the image's own height or less, so
  -- give a banner a fixed height and 'FitCover' or 'FitContain'.
  -- 'NanoUI.fontColor' tints the image, as it does an SVG icon.
  , icFit :: !ContentFit
  -- ^ How the image fills the widget.
  , icAlignX :: !AlignX
  -- ^ Where the fitted image sits across the widget when it does not fill
  -- it, and which part of it a crop keeps.
  , icAlignY :: !AlignY
  -- ^ The same, up and down. 'AlignBaseline' is 'AlignTop'.
  , icOpacity :: !Float
  -- ^ From 0, invisible, to 1, opaque.
  , icRotation :: !Rotation
  }
  deriving (Eq, Show)

-- | The default layout, stretched to it as 'NanoUI.image' is, centred,
-- opaque and not turned.
defaultImageConfig :: ImageConfig
defaultImageConfig =
  ImageConfig
    { icLayout = defaultLayout
    , icFit = FitFill
    , icAlignX = AlignCenter
    , icAlignY = AlignMiddle
    , icOpacity = 1
    , icRotation = RotateFloating 0
    }

-- | An image registered with the host, drawn with a fit, an alignment, an
-- opacity and a rotation:
--
-- > imageConfigured defaultImageConfig {icLayout = fixedWH 160 90 defaultLayout, icFit = FitCover} photo
imageConfigured :: Ui :> es => ImageConfig -> ImageId -> Eff es ()
imageConfigured cfg iid = void (imageConfigured' cfg iid)

-- | 'imageConfigured' with its 'Response'. The image takes the pointer as a
-- canvas does, so the response reports hover and clicks.
imageConfigured' :: Ui :> es => ImageConfig -> ImageId -> Eff es Response
imageConfigured' cfg iid@(ImageId tid) = do
  (wid, ctx) <- freshWidget
  natural <- uiIO (lookupImageSize ctx iid)
  let lay = icLayout cfg
      rot = finiteRotation (icRotation cfg)
      tint0 = fromMaybe (colorRGBA 255 255 255 255) (layoutFontColor lay)
      opacity = icOpacity cfg
  fst
    <$> customWidgetWithId
      wid
      defaultCustomWidgetSpec
        { widgetLayout = lay
        , widgetMeasure = Just (naturalMeasure lay (turnedSize rot (naturalOf natural)))
        , widgetContent =
            contentKeyOf
              [ keyPart tid
              , keyPart natural
              , keyPart (fromEnum (icFit cfg))
              , keyPart (fromEnum (icAlignX cfg))
              , keyPart (fromEnum (icAlignY cfg))
              , keyPart (case rot of RotateSolid a -> (True, a); RotateFloating a -> (False, a))
              , keyPart opacity
              , keyPart (colorToWord32 tint0)
              ]
        , widgetDraw = \cdc box ->
            case natural of
              -- An image that is not registered draws the accent colour, as
              -- 'NanoUI.image' does.
              Nothing -> smallArrayFromList [FillRect box (themeAccent (cdcTheme cdc))]
              Just (iw, ih) ->
                let tint = imageTint cdc opacity tint0
                    dest = placeImage (icFit cfg) (icAlignX cfg) (icAlignY cfg) rot (fromIntegral iw) (fromIntegral ih) box
                    angle = rotationAngle rot
                 in if colorA tint == 0
                      then emptySmallArray
                      else
                        if abs (sin angle) < 1.0e-6 && cos angle > 0
                          then croppedImage tid tint dest box
                          else smallArrayFromList [DrawImageRotated dest angle tid 0 0 1 1 tint]
        }

-- | Where the image of @iw@ by @ih@ pixels is drawn in @box@: its rect before
-- any turn, which turns about its own centre.
placeImage :: ContentFit -> AlignX -> AlignY -> Rotation -> Float -> Float -> Rect -> Rect
placeImage fit ax ay rot iw ih (Rect bx by bw bh) =
  Rect (cx - dw / 2) (cy - dh / 2) dw dh
  where
    (rw, rh) = turnedSize rot (iw, ih)
    contain = min (bw / rw) (bh / rh)
    scaled k = (k * iw, k * ih)
    (dw, dh) = case fit of
      FitContain -> scaled contain
      FitCover -> scaled (max (bw / rw) (bh / rh))
      FitNone -> (iw, ih)
      FitScaleDown -> scaled (min 1 contain)
      FitFill -> case rot of
        RotateSolid angle -> fillTurned angle
        RotateFloating _ -> (bw, bh)
    -- The stretch whose turned bounding box is the box: none at a quarter
    -- turn's odd multiples of 45 degrees, nor where the box's shape is out of
    -- the turn's reach, and those fit as 'FitContain' does.
    fillTurned angle =
      let c = abs (cos angle)
          s = abs (sin angle)
          det = c * c - s * s
          w = (c * bw - s * bh) / det
          h = (c * bh - s * bw) / det
       in if abs det > 1.0e-3 && w > 0 && h > 0 then (w, h) else scaled contain
    -- The fitted image, turned when the turn takes room, placed by alignment.
    (pw, ph) = turnedSize rot (dw, dh)
    cx = case ax of
      AlignStart -> bx + pw / 2
      AlignCenter -> bx + bw / 2
      AlignEnd -> bx + bw - pw / 2
    cy = case ay of
      AlignMiddle -> by + bh / 2
      AlignBottom -> by + bh - ph / 2
      _ -> by + ph / 2

-- | The room an image of this size takes turned: its bounding box under a
-- solid rotation, else the size itself.
turnedSize :: Rotation -> (Float, Float) -> (Float, Float)
turnedSize (RotateSolid angle) (w, h) =
  let c = abs (cos angle)
      s = abs (sin angle)
   in (w * c + h * s, w * s + h * c)
turnedSize (RotateFloating _) size = size

-- | An image's own size in logical pixels, or the 32 pixels 'NanoUI.image'
-- takes when it is not registered.
naturalOf :: Maybe (Int, Int) -> (Float, Float)
naturalOf = maybe (32, 32) (\(w, h) -> (fromIntegral w, fromIntegral h))

-- | The size an image widget asks for: its image's (@nw@ by @nh@), shrunk to
-- fit the limits it is offered, or the image's shape against a fixed extent
-- on the other axis. A fixed axis keeps its own size whatever this says.
naturalMeasure :: Layout -> (Float, Float) -> CustomMeasureFn
naturalMeasure lay (nw, nh) _ (offerW, offerH) =
  case (layoutWidth lay, layoutHeight lay) of
    (Fixed w, Fixed h) -> (w, h)
    (Fixed w, _) -> (w, w * nh / nw)
    (_, Fixed h) -> (h * nw / nh, h)
    _ ->
      let k = minimum [1, offerW / nw, offerH / nh]
       in (k * nw, k * nh)

-- | The unturned image drawn at @dest@, cropped to @box@ by its UVs rather
-- than drawn past it.
croppedImage :: Int -> Color -> Rect -> Rect -> SmallArray DrawOp
croppedImage tid tint dest@(Rect dx dy dw dh) box =
  case rectIntersect dest box of
    Just (Rect x y w h)
      | dw > 0 && dh > 0 && w > 0 && h > 0 ->
          smallArrayFromList
            [DrawImageRect (Rect x y w h) tid ((x - dx) / dw) ((y - dy) / dh) ((x + w - dx) / dw) ((y + h - dy) / dh) tint]
    _ -> emptySmallArray

-- | The tint an image is drawn in: its colour, faded by the opacity, and
-- further when the widget is disabled.
imageTint :: CustomDrawContext -> Float -> Color -> Color
imageTint cdc opacity tint =
  let fade = if cdcDisabled cdc then 1 - themeDisabledFade (cdcTheme cdc) else 1
      opacity' = if isNaN opacity then 1 else clamp01 opacity
   in fadeAlpha tint (round (fromIntegral (colorA tint) * opacity' * fade))

-- | A rotation by an angle that is not a number, or not finite, as none.
finiteRotation :: Rotation -> Rotation
finiteRotation rot
  | isNaN a || isInfinite a = RotateFloating 0
  | otherwise = rot
  where
    a = rotationAngle rot
