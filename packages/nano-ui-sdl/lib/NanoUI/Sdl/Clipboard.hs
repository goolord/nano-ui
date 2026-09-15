module NanoUI.Sdl.Clipboard
  ( withSdlClipboard
  ) where

import qualified Data.Text as T
import Data.Text.Foreign (peekCString, withCString)
import Foreign.Ptr (castPtr, nullPtr)
import NanoUI.Testing (Context, withClipboard)
import SDL3.Sys.Bindgen.Runtime.PtrConst qualified as PtrConst
import SDL3.Sys.Clipboard (getClipboardText, setClipboardText)
import SDL3.Sys.Stdinc (free)

-- | Route the context's clipboard through SDL (UTF-8 text both ways).
withSdlClipboard :: Context -> Context
withSdlClipboard ctx = withClipboard ctx readClipboard writeClipboard
  where
    writeClipboard txt = withCString txt (setClipboardText . PtrConst.unsafeFromPtr)
    readClipboard = do
      ptr <- getClipboardText
      if ptr == nullPtr
        then pure Nothing
        else do
          txt <- peekCString ptr
          free (castPtr ptr)
          pure (if T.null txt then Nothing else Just txt)
