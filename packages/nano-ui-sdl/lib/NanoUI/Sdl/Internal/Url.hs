-- | Open a URL with the platform's default handler.
module NanoUI.Sdl.Internal.Url (openUrl) where

import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import SDL3.Sys.Bindgen.Runtime.PtrConst qualified as PtrConst
import SDL3.Sys.Misc qualified as SDL

-- | Open a URL with the platform's default application, such as a browser or
-- mail client. Call from the application's main thread.
openUrl :: Text -> IO Bool
openUrl url = BS.useAsCString (encodeUtf8 url) $ \cUrl ->
  SDL.openURL (PtrConst.unsafeFromPtr cUrl)
