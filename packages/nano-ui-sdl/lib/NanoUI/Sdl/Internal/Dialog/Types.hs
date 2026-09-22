-- | Types and pending-dialog state shared between the SDL dialog backend and
-- the SDL window lifecycle.
module NanoUI.Sdl.Internal.Dialog.Types
  ( FileDialogId (..)
  , FileDialogResult (..)
  , DialogState (..)
  , newDialogState
  ) where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IORef (IORef, newIORef)

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

-- | The session's tracked dialogs, keyed by 'FileDialogId': the cell each
-- one's SDL callback writes its result to.
data DialogState = DialogState
  { dsNextId :: !(IORef Int)
  , dsPending :: !(IORef (IntMap (IORef FileDialogResult)))
  }

-- | Create an empty dialog state.
newDialogState :: IO DialogState
newDialogState = DialogState <$> newIORef 0 <*> newIORef IM.empty
