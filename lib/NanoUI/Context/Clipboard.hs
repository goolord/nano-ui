module NanoUI.Context.Clipboard
  ( withClipboard
  ) where

import NanoUI.Context.Internal (Context (..))

withClipboard :: Context -> IO (Maybe String) -> (String -> IO Bool) -> Context
withClipboard ctx get set =
  ctx {ctxClipboardGet = get, ctxClipboardSet = set}
