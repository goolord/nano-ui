module NanoUI.Sdl.Clipboard
  ( getClipboardText
  , setClipboardText
  , withSdlClipboard
  ) where

import Foreign.C.Types (CChar, CInt(CInt))
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.Ptr (Ptr, nullPtr)
import NanoUI (Context, withClipboard)

foreign import ccall unsafe "SDL_SetClipboardText"
  c_SDL_SetClipboardText :: CString -> IO CInt

foreign import ccall unsafe "SDL_GetClipboardText"
  c_SDL_GetClipboardText :: IO (Ptr CChar)

foreign import ccall unsafe "SDL_free"
  c_SDL_free :: Ptr a -> IO ()

setClipboardText :: String -> IO Bool
setClipboardText txt =
  withCString txt $ \cstr -> do
    ok <- c_SDL_SetClipboardText cstr
    pure (ok /= 0)

getClipboardText :: IO (Maybe String)
getClipboardText = do
  ptr <- c_SDL_GetClipboardText
  if ptr == nullPtr
    then pure Nothing
    else do
      txt <- peekCString ptr
      c_SDL_free ptr
      pure (if null txt then Nothing else Just txt)

withSdlClipboard :: Context -> Context
withSdlClipboard ctx = withClipboard ctx getClipboardText setClipboardText
