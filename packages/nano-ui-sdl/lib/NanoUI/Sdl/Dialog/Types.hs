-- | Types and pending-dialog state shared between the SDL dialog backend and
-- the SDL window lifecycle.
module NanoUI.Sdl.Dialog.Types
  ( FileDialogId (..)
  , FileDialogResult (..)
  , PendingDialog (..)
  , DialogCallback
  , DialogCallbackFunPtr
  , DialogState (..)
  , newDialogState
  , clearDialogState
  ) where

import Data.Int (Int32)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IORef (IORef, newIORef, writeIORef)
import Foreign.Ptr (FunPtr, Ptr)

-- | Opaque handle returned by a non-blocking dialog launch. @0@ is never a
-- valid handle.
newtype FileDialogId = FileDialogId Int
  deriving (Eq, Ord, Show)

-- | Lifecycle state of a launched file dialog.
data FileDialogResult
  = FileDialogPending
  -- ^ Still waiting for the user.
  | FileDialogCancelled
  -- ^ The user dismissed the dialog without choosing.
  | FileDialogFailed
  -- ^ SDL reported an error.
  | FileDialogSelected [FilePath]
  -- ^ The user chose one or more paths.
  | FileDialogUnknown
  -- ^ No dialog with this handle is being tracked. A handle becomes unknown
  -- once its result has been delivered and consumed by 'pollFileDialog', or
  -- after the dialog was abandoned via 'cancelFileDialog'. Never poll a
  -- handle that returns 'FileDialogUnknown' again.
  deriving (Eq, Show)

-- | Shape of the SDL3 dialog callback, flattened to 'Ptr' at the FFI
-- boundary.
type DialogCallback = Ptr () -> Ptr () -> Int32 -> IO ()

-- | A marshalled 'DialogCallback' allocated once per dialog launch.
type DialogCallbackFunPtr = FunPtr DialogCallback

-- | A tracked dialog: its current status plus the FFI callback that owns its
-- completion. The callback is released only after the dialog completes and a
-- poll consumes the result, so it is never freed while SDL could still invoke
-- it.
data PendingDialog = PendingDialog
  { pendingStatus :: !FileDialogResult
  , pendingCallback :: !DialogCallbackFunPtr
  }

-- | Pending dialogs, keyed by 'FileDialogId'.
data DialogState = DialogState
  { dsNextId :: !(IORef Int)
  , dsPending :: !(IORef (IntMap PendingDialog))
  }

-- | Create an empty dialog state.
newDialogState :: IO DialogState
newDialogState = DialogState <$> newIORef 0 <*> newIORef IM.empty

-- | Forget every pending dialog. Used during SDL teardown: dialogs still
-- open on the OS side keep running and their callbacks are left to the
-- process, but all handles become 'FileDialogUnknown'.
clearDialogState :: DialogState -> IO ()
clearDialogState st = writeIORef (dsPending st) IM.empty