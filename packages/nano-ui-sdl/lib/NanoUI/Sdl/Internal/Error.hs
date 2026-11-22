-- | SDL failures reported with SDL's own explanation of them.
module NanoUI.Sdl.Internal.Error
  ( sdlErrorText
  , sdlFail
  , withSdlError
  ) where

import Foreign.Ptr (nullPtr)
import GHC.Foreign qualified as GHC
import GHC.IO.Encoding (utf8)
import SDL3.Sys.Bindgen.Runtime.PtrConst qualified as PtrConst
import SDL3.Sys.Error (getError)

-- | The calling thread's last SDL error message (@SDL_GetError@), empty when
-- SDL has set none.
sdlErrorText :: IO String
sdlErrorText = do
  p <- PtrConst.unsafeToPtr <$> getError
  if p == nullPtr then pure "" else GHC.peekCString utf8 p

-- | A message with SDL's last error appended after a colon, or the message
-- alone when SDL has set none.
withSdlError :: String -> IO String
withSdlError what = sdlErrorText >>= \e -> pure (if null e then what else what ++ ": " ++ e)

-- | Throw an 'IOError' for a failed SDL call, naming the call and appending
-- SDL's last error.
sdlFail :: String -> IO a
sdlFail what = ioError . userError =<< withSdlError what
