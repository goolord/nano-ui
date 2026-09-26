-- | Image nodes fitted, aligned, cropped, zoomed, faded and turned
-- ('imageConfigured'), the node every image is ('imageNode'), and images
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
  , sweepImageHooks
  )
where

import Control.Monad (forM_, unless, void)
import Data.ByteString (ByteString)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.Maybe (fromMaybe, isJust)
import Data.Text qualified as T
import Data.Typeable (Typeable, cast)
import Effectful (Eff, type (:>))
import NanoUI.Internal.Atlas qualified as Atlas
import NanoUI.Internal.Context (Context (..), askHostIO, hostOrInit, intKey, lookupImageSize, registerImage, releaseImage)
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Internal.Image
import NanoUI.Internal.Layout.Arena (ImageNode (..), NodeType (NodeImage), setImageNode)
import NanoUI.Internal.Monad (Ui, freshWidget, uiIO)
import NanoUI.Internal.Style (Layout (..), Sizing (..), aspect, defaultLayout)
import NanoUI.Internal.Types (ImageId (..), colorRGBA)
import NanoUI.Internal.WidgetText (intValueText)
import NanoUI.Internal.Widgets.Node (Response, addWidgetNode)

-- | An image node for image @iid@ under widget @wid@. Its text is the
-- image's id, which paint finds the image by and damage repaints a switched
-- image by. With an 'ImageNode' paint fits, fades and turns the image
-- ('lookDraw'), and an unsized axis takes its size; without one it
-- stretches the image, as 'NanoUI.image' does. It lets the pointer through
-- to a control it is drawn over, as a label does, and its response reports
-- hover and clicks.
imageNode :: Ui :> es => WidgetId -> Maybe ImageNode -> Layout -> ImageId -> Eff es Response
imageNode wid node lay (ImageId tid) =
  addWidgetNode wid NodeImage (if tid <= 0 then T.empty else intValueText tid) 0 lay $ \arena idx ->
    mapM_ (setImageNode arena idx) node

-- | An image registered with the host, drawn with a fit, an alignment, a
-- crop, a zoom, an opacity and a rotation:
--
-- > imageConfigured defaultImageConfig {icLayout = fixedWH 160 90, icFit = FitCover} photo
-- > imageConfigured defaultImageConfig {icLayout = fillW} photo   -- the column's width, in the photo's shape
imageConfigured :: Ui :> es => ImageConfig -> ImageId -> Eff es ()
imageConfigured cfg iid = void (imageConfigured' cfg iid)

-- | 'imageConfigured' with its 'Response'. An image not registered takes
-- what 'NanoUI.image' does: 32 pixels on an unsized axis, painted in the
-- theme's accent.
imageConfigured' :: Ui :> es => ImageConfig -> ImageId -> Eff es Response
imageConfigured' cfg iid = do
  (wid, ctx) <- freshWidget
  natural <- uiIO (lookupImageSize ctx iid)
  let lay0 = icLayout cfg defaultLayout
      look = imageLook cfg (fromMaybe (colorRGBA 255 255 255 255) (layoutFontColor lay0))
      (w, h) = maybe (32, 32) (lookSize look) natural
      -- A registered image keeps its shape on an axis the layout leaves to it.
      lay
        | isJust natural && (layoutWidth lay0 == Fit || layoutHeight lay0 == Fit) && layoutAspect lay0 <= 0 = aspect (w / h) lay0
        | otherwise = lay0
      -- Stretched over a size the layout fixes, it is a plain image, which
      -- paint and damage take the short way.
      plain = fixed (layoutWidth lay) && fixed (layoutHeight lay) && look == imageLook defaultImageConfig (lookTint look)
      fixed = \case Fixed _ -> True; _ -> False
  imageNode wid (if plain then Nothing else Just (ImageNode look w h)) lay iid

-- | The context's 'useImageRgba' images, kept on it as a host value
-- ('hostOrInit').
newtype ImageHooks = ImageHooks (IORef HookTable)

-- | The images by the store key of their hook's widget id, and the keys
-- whose hook ran this frame, in any of its view passes.
data HookTable = HookTable !(IntMap HookImage) !IntSet

-- | A hook's key, compared by value, and the image registered for it, or
-- 'Nothing' when the atlas refused it.
data HookImage = forall k. (Eq k, Typeable k) => HookImage !k !(Maybe ImageId)

-- | Register an RGBA image (4 bytes a pixel, rows top to bottom), @w@ by
-- @h@ pixels, the first frame this is called with a key, and hand back its
-- id on that frame and every one after while the key stays the same:
--
-- > thumb <- useImageRgba path w h pixels
-- > mapM_ (image (fixedWH 96 96)) thumb
--
-- The image lives as long as the view calls the hook, as a 'NanoUI.useTask'
-- job does: a frame that calls it with another key registers the new image
-- and lets the old one go, and a frame that does not call it lets its image
-- go, which frees its room in the image atlas for another. Its id is not
-- used again, so a stale copy of it draws the placeholder 'NanoUI.image'
-- draws for an id it does not know. 'Nothing' when the size or pixels are
-- invalid or the atlas is full, until the key changes. Like any hook it
-- takes the next widget id; call it on every frame that shows the image, or
-- inside 'NanoUI.scope' where it is called on some frames and not others.
useImageRgba :: (Eq k, Typeable k, Ui :> es) => k -> Int -> Int -> ByteString -> Eff es (Maybe ImageId)
useImageRgba k w h pixels = do
  (wid, ctx) <- freshWidget
  uiIO $ do
    ImageHooks ref <- hostOrInit ctx (ImageHooks <$> newIORef (HookTable IM.empty IS.empty))
    HookTable hooks called <- readIORef ref
    let key = intKey wid
    case IM.lookup key hooks of
      Just (HookImage k0 iid) | cast k0 == Just k -> do
        unless (IS.member key called) $ writeIORef ref $! HookTable hooks (IS.insert key called)
        pure iid
      old -> do
        forM_ old (releaseHookImage ctx)
        iid <- Atlas.freshImageId (ctxImageAtlas ctx)
        ok <- registerImage ctx iid w h pixels
        let registered = if ok then Just iid else Nothing
        writeIORef ref $! HookTable (IM.insert key (HookImage k registered) hooks) (IS.insert key called)
        pure registered

-- | End a frame for the 'useImageRgba' images: those whose hook ran stay,
-- and the rest are let go. Two view passes of one frame count as one frame.
sweepImageHooks :: Context -> IO ()
sweepImageHooks ctx = askHostIO ctx >>= mapM_ sweep
  where
    sweep (ImageHooks ref) = do
      HookTable hooks called <- readIORef ref
      unless (IM.null hooks && IS.null called) $ do
        let (kept, gone) = IM.partitionWithKey (\k _ -> IS.member k called) hooks
        writeIORef ref $! HookTable kept IS.empty
        mapM_ (releaseHookImage ctx) gone

-- | Take a hook's image out of the atlas.
releaseHookImage :: Context -> HookImage -> IO ()
releaseHookImage ctx (HookImage _ iid) = mapM_ (releaseImage ctx) iid
