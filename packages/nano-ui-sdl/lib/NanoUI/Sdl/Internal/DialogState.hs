-- | Thread-safe dialog completion and one-shot consumption, independent of SDL.
module NanoUI.Sdl.Internal.DialogState
  ( FileDialogResult (..)
  , takeResult
  , completeResult
  ) where

import Data.IORef (IORef, atomicModifyIORef')

-- | Lifecycle state of a launched file dialog.
data FileDialogResult
  = FileDialogPending
  -- ^ Still waiting for the user.
  | FileDialogCancelled
  -- ^ The user dismissed the dialog.
  | FileDialogFailed
  -- ^ SDL reported an error.
  | FileDialogSelected [FilePath]
  -- ^ The selected paths.
  | FileDialogUnknown
  -- ^ The handle was consumed, abandoned, or belongs to another session.
  deriving (Eq, Show)

-- | A pending result can be polled again; a completion is delivered once.
takeResult :: IORef FileDialogResult -> IO FileDialogResult
takeResult ref = atomicModifyIORef' ref $ \r ->
  (if r == FileDialogPending then r else FileDialogUnknown, r)

-- | Publish a completion unless the application abandoned the dialog.
completeResult :: IORef FileDialogResult -> FileDialogResult -> IO ()
completeResult ref result = atomicModifyIORef' ref $ \r ->
  (if r == FileDialogPending then result else r, ())
