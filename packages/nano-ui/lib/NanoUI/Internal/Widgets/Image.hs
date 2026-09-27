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
import NanoUI.Internal.Atlas qualified as Atlas
import NanoUI.Internal.Context (Context (..), lookupImageSize, registerImage, releaseImage)
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Internal.Image
import NanoUI.Internal.Layout.Arena (ImageNode (..), NodeType (NodeImage), setImageId, setImageNode)
import NanoUI.Internal.Monad (NanoUI, freshWidget, liftIO)
import NanoUI.Internal.Style (Layout (..), Sizing (..), aspect, defaultLayout)
import NanoUI.Internal.Tasks (useHeld)
import NanoUI.Internal.Types (ImageId (..), colorRGBA)
import NanoUI.Internal.Widgets.Node (Response, addWidgetNode)

-- | An image node for image @iid@ under widget @wid@. Paint finds the image
-- by the node's image id ('setImageId'), and damage spots a switched image
-- by it. With an 'ImageNode', paint fits, fades and rotates the image
-- ('lookDraw') and an unsized axis takes the image's size; without one the
-- image is stretched, as with 'NanoUI.image'. Like a label, it passes the
-- pointer to a control it is drawn over; its response still reports hover
-- and clicks.
imageNode :: WidgetId -> Maybe ImageNode -> Layout -> ImageId -> NanoUI Response
imageNode wid node lay (ImageId tid) =
  addWidgetNode wid NodeImage T.empty 0 lay $ \arena idx -> do
    setImageId arena idx (max 0 tid)
    mapM_ (setImageNode arena idx) node

-- | A registered image drawn with a fit, alignment, crop, zoom, opacity and
-- rotation:
--
-- > imageConfigured defaultImageConfig {icLayout = fixedWH 160 90, icFit = FitCover} photo
-- > imageConfigured defaultImageConfig {icLayout = fillW} photo   -- column width, photo's aspect ratio
imageConfigured :: ImageConfig -> ImageId -> NanoUI ()
imageConfigured cfg iid = void (imageConfigured' cfg iid)

-- | 'imageConfigured' returning its 'Response'. An unregistered image
-- behaves like 'NanoUI.image': 32 pixels on an unsized axis, painted in the
-- theme accent.
imageConfigured' :: ImageConfig -> ImageId -> NanoUI Response
imageConfigured' cfg iid = do
  (wid, ctx) <- freshWidget
  natural <- liftIO (lookupImageSize ctx iid)
  let !lay0 = icLayout cfg defaultLayout
      !look = imageLook cfg (fromMaybe (colorRGBA 255 255 255 255) (layoutFontColor lay0))
      fixed = \case Fixed _ -> True; _ -> False
  -- Both axes fixed with a default look: a plain stretched image, which
  -- paint and damage handle on a faster path.
  if fixed (layoutWidth lay0) && fixed (layoutHeight lay0) && plainLook look
    then imageNode wid Nothing lay0 iid
    else do
      let !(w, h) = maybe (32, 32) (lookSize look) natural
          -- A registered image keeps its aspect ratio on an unsized axis.
          lay
            | isJust natural && (layoutWidth lay0 == Fit || layoutHeight lay0 == Fit) && layoutAspect lay0 <= 0 = aspect (w / h) lay0
            | otherwise = lay0
      imageNode wid (Just $! ImageNode look w h) lay iid

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
useImageRgba :: (Eq k, Typeable k) => k -> Int -> Int -> ByteString -> NanoUI (Maybe ImageId)
useImageRgba k w h pixels = useHeld k $ \ctx _ -> do
  iid <- Atlas.freshImageId (ctxImageAtlas ctx)
  ok <- registerImage ctx iid w h pixels
  pure (if ok then (Just iid, releaseImage ctx iid) else (Nothing, pure ()))
