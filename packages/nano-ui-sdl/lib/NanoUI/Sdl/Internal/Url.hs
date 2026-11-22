-- | Open a URL with the platform's default handler.
module NanoUI.Sdl.Internal.Url (openUrl) where

import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import SDL3.Sys.Bindgen.Runtime.PtrConst qualified as PtrConst
import SDL3.Sys.Misc qualified as SDL

-- | Open a URL with the platform's default application, such as a browser or
-- mail client. Call from the application's main thread.
--
-- The call is a safe foreign call: on Windows the shell can send messages to
-- this program's windows while it starts the handler, and a window
-- procedure that runs Haskell (the resize watch, the hit test) must not be
-- entered from inside an unsafe call.
--
-- The string goes to the platform unchecked: a @file:@ URL opens a local
-- file, and on Windows a path to a program runs it. Before opening a URL
-- from content the application does not control, such as a link in a
-- Markdown document, check that its scheme is one it means to allow
-- (@http@, @https@, @mailto@).
openUrl :: Text -> IO Bool
openUrl url = BS.useAsCString (encodeUtf8 url) $ \cUrl ->
  SDL.openURLSafe (PtrConst.unsafeFromPtr cUrl)
