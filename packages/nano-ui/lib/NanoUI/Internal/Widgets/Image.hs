-- | Images with fit, alignment, crop, zoom, opacity and rotation
-- ('imageConfigured'), the underlying image node ('imageNode'), and images
-- registered for as long as a view shows them ('useImageRgba').
module NanoUI.Internal.Widgets.Image
  ( ContentFit (..)
  , Rotation (..)
  , rotationAngle
  , ImageConfig (..)
  , defaultImageConfig
  , fitRect
  , imageNode
  , imageConfigured
  , imageConfigured'
  , useImageRgba
  )
where

import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe, isJust)
import Data.Text qualified as T
import Data.Typeable (Typeable)
import Effectful (Eff, type (:>))
import NanoUI.Internal.Atlas qualified as Atlas
import NanoUI.Internal.Context (Context (..), lookupImageSize, registerImage, releaseImage)
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Internal.Image
import NanoUI.Internal.Layout.Arena (ImageNode (..), NodeType (NodeImage), setImageNode)
import NanoUI.Internal.Monad (Ui, freshWidget, uiIO)
import NanoUI.Internal.Style (Layout (..), Sizing (..), aspect, defaultLayout)
import NanoUI.Internal.Tasks (useHeld)
import NanoUI.Internal.Types (ImageId (..), colorRGBA)
import NanoUI.Internal.WidgetText (intValueText)
import NanoUI.Internal.Widgets.Node (Response, addWidgetNode)

-- | An image node for image @iid@ under widget @wid@. Its text is the image
-- id, which paint uses to find the image and damage uses to spot a switched
-- image. With an 'ImageNode', paint fits, fades and rotates the image
-- ('lookDraw') and an unsized axis takes the image's size; without one the
-- image is stretched, as with 'NanoUI.image'. Like a label, it passes the
-- pointer to a control it is drawn over; its response still reports hover
-- and clicks.
imageNode :: Ui :> es => WidgetId -> Maybe ImageNode -> Layout -> ImageId -> Eff es Response
imageNode wid node lay (ImageId tid) =
  addWidgetNode wid NodeImage (if tid <= 0 then T.empty else intValueText tid) 0 lay $ \arena idx ->
    mapM_ (setImageNode arena idx) node

-- | A registered image drawn with a fit, alignment, crop, zoom, opacity and
-- rotation:
--
-- > imageConfigured defaultImageConfig {icLayout = fixedWH 160 90, icFit = FitCover} photo
-- > imageConfigured defaultImageConfig {icLayout = fillW} photo   -- column width, photo's aspect ratio
imageConfigured :: Ui :> es => ImageConfig -> ImageId -> Eff es ()
imageConfigured cfg iid = void (imageConfigured' cfg iid)

-- | 'imageConfigured' returning its 'Response'. An unregistered image
-- behaves like 'NanoUI.image': 32 pixels on an unsized axis, painted in the
-- theme accent.
imageConfigured' :: Ui :> es => ImageConfig -> ImageId -> Eff es Response
imageConfigured' cfg iid = do
  (wid, ctx) <- freshWidget
  natural <- uiIO (lookupImageSize ctx iid)
  let lay0 = icLayout cfg defaultLayout
      look = imageLook cfg (fromMaybe (colorRGBA 255 255 255 255) (layoutFontColor lay0))
      (w, h) = maybe (32, 32) (lookSize look) natural
      -- A registered image keeps its aspect ratio on an unsized axis.
      lay
        | isJust natural && (layoutWidth lay0 == Fit || layoutHeight lay0 == Fit) && layoutAspect lay0 <= 0 = aspect (w / h) lay0
        | otherwise = lay0
      -- Both axes fixed with a default look: a plain stretched image, which
      -- paint and damage handle on a faster path.
      plain = fixed (layoutWidth lay) && fixed (layoutHeight lay) && look == imageLook defaultImageConfig (lookTint look)
      fixed = \case Fixed _ -> True; _ -> False
  imageNode wid (if plain then Nothing else Just (ImageNode look w h)) lay iid

-- | Register an RGBA image (4 bytes per pixel, rows top to bottom), @w@ by
-- @h@ pixels, on the first frame this is called with a key, and return its
-- id on every frame while the key stays the same:
--
-- > thumb <- useImageRgba path w h pixels
-- > mapM_ (image (fixedWH 96 96)) thumb
--
-- The image lives as long as the view keeps calling the hook, like a
-- 'NanoUI.useTask' job. A new key registers the new image and releases the
-- old one; a frame that skips the call releases the image and frees its
-- atlas space. Ids are never reused, so a stale id draws the placeholder
-- 'NanoUI.image' shows for an unknown id. Returns 'Nothing' if the size or
-- pixels are invalid or the atlas is full, until the key changes. Like any
-- hook it takes the next widget id: call it on every frame that shows the
-- image, or inside 'NanoUI.scope' if only some frames call it.
useImageRgba :: (Eq k, Typeable k, Ui :> es) => k -> Int -> Int -> ByteString -> Eff es (Maybe ImageId)
useImageRgba k w h pixels = useHeld k $ \ctx _ -> do
  iid <- Atlas.freshImageId (ctxImageAtlas ctx)
  ok <- registerImage ctx iid w h pixels
  pure (if ok then (Just iid, releaseImage ctx iid) else (Nothing, pure ()))
