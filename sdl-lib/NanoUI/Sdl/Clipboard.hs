module NanoUI.Sdl.Clipboard
  ( getClipboardText
  , setClipboardText
  , withSdlClipboard
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Foreign (peekCString, withCString)
import Foreign.C.String (CString)
import Foreign.C.Types (CChar, CInt (CInt))
import Foreign.Ptr (Ptr, nullPtr)
import NanoUI.Testing (Context, withClipboard)

foreign import ccall unsafe "SDL_SetClipboardText"
  c_SDL_SetClipboardText :: CString -> IO CInt

foreign import ccall unsafe "SDL_GetClipboardText"
  c_SDL_GetClipboardText :: IO (Ptr CChar)

foreign import ccall unsafe "SDL_free"
  c_SDL_free :: Ptr a -> IO ()

setClipboardText :: Text -> IO Bool
setClipboardText txt =
  withCString txt $ \cstr -> do
    ok <- c_SDL_SetClipboardText cstr
    pure (ok /= 0)

getClipboardText :: IO (Maybe Text)
getClipboardText = do
  ptr <- c_SDL_GetClipboardText
  if ptr == nullPtr
    then pure Nothing
    else do
      txt <- peekCString ptr
      c_SDL_free ptr
      pure (if T.null txt then Nothing else Just txt)

withSdlClipboard :: Context -> Context
withSdlClipboard ctx =
  withClipboard ctx getClipboardText setClipboardText
