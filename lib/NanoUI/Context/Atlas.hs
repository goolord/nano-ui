module NanoUI.Context.Atlas
  ( atlasTextureId
  , registerImage
  , registerImages
  , lookupImageUv
  , atlasSnapshot
  ) where

import Control.Monad (when)
import Data.ByteString (ByteString)
import Foreign.ForeignPtr (ForeignPtr)
import Data.Word (Word8)
import qualified NanoUI.Atlas as Atlas
import NanoUI.Atlas (atlasTextureId)
import NanoUI.Context.Internal (Context (..), markDirty)
import NanoUI.Types (ImageId (..))

registerImage :: Context -> ImageId -> Int -> Int -> ByteString -> IO Bool
registerImage ctx iid w h pixels = do
  ok <- Atlas.registerImage (ctxImageAtlas ctx) iid w h pixels
  when ok (markDirty ctx)
  pure ok

registerImages :: Context -> [(ImageId, Int, Int, ByteString)] -> IO Bool
registerImages ctx = fmap and . mapM (\(iid, w, h, px) -> registerImage ctx iid w h px)

lookupImageUv :: Context -> ImageId -> IO (Maybe (Float, Float, Float, Float))
lookupImageUv ctx = Atlas.lookupImageUv (ctxImageAtlas ctx)

atlasSnapshot :: Context -> IO (Maybe (Int, Int, ForeignPtr Word8, Int))
atlasSnapshot ctx = Atlas.atlasSnapshot (ctxImageAtlas ctx)
