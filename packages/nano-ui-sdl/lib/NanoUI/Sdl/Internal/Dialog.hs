-- | SDL3 native file dialogs.
--
-- These wrap SDL3's asynchronous dialog API ('SDL_ShowOpenFileDialog',
-- 'SDL_ShowSaveFileDialog', and 'SDL_ShowOpenFolderDialog') into a
-- non-blocking, poll-based interface. Launching a dialog returns a
-- 'FileDialogId' immediately and the app keeps running its normal event loop;
-- poll the handle on later frames to observe completion.
--
-- Threading: SDL3 may invoke the dialog callback on a background thread, so
-- the callback here does little: it decodes the result, frees the FFI
-- buffers the launch allocated, records the outcome, and wakes the event
-- loop. All UI-affecting work ('markDirty', reclaiming focus) is deferred to
-- the thread that polls the result.
module NanoUI.Sdl.Internal.Dialog
  ( FileFilter (..)
  , FileDialogOptions (..)
  , defaultFileDialogOptions
  , FileDialogId (..)
  , FileDialogResult (..)
  , openFileDialog
  , saveFileDialog
  , openFolderDialog
  , pollFileDialog
  , cancelFileDialog
  , askOpenFileDialog
  , askSaveFileDialog
  , askOpenFolderDialog
  , pollFileDialogUi
  ) where

import Control.Monad (forM, unless, void, (>=>))
import Data.Int (Int32)
import Data.IntMap.Strict qualified as IM
import Data.IORef (IORef, atomicModifyIORef', atomicWriteIORef, newIORef, readIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Effectful (Eff, type (:>))
import Foreign.C.String (newCString, peekCString)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Array (newArray, peekArray0)
import Foreign.Marshal.Utils (maybePeek)
import Foreign.Ptr (FunPtr, Ptr, castFunPtr, castPtr, nullPtr)
import Foreign.StablePtr (castPtrToStablePtr, castStablePtrToPtr, deRefStablePtr, freeStablePtr, newStablePtr)
import NanoUI.Sdl.Internal.Display (pushRefreshEvent)
import NanoUI.Sdl.Internal.Window (SdlEnv (..))
import NanoUI.Testing (Ui, askHost, markDirty, uiIO)
import SDL3.Sys.Bindgen.Dialog (SDL_DialogFileCallback (..), SDL_DialogFileFilter (..))
import SDL3.Sys.Bindgen.Runtime.PtrConst qualified as PtrConst
import SDL3.Sys.Dialog
  ( showOpenFileDialogSafe
  , showOpenFolderDialogSafe
  , showSaveFileDialogSafe
  )
import SDL3.Sys.Video (raiseWindowSafe, restoreWindowSafe)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)

-- | A file type filter shown in open/save dialogs.
data FileFilter = FileFilter
  { filterName :: !Text
  -- ^ Human-readable label, e.g. @"Haskell source"@.
  , filterPattern :: !Text
  -- ^ Semicolon-separated extension list, e.g. @"hs;lhs"@, or @"*"@.
  }
  deriving (Eq, Show)

-- | Common options for native file dialogs.
data FileDialogOptions = FileDialogOptions
  { dialogFilters :: ![FileFilter]
  -- ^ File filters (ignored by folder dialogs).
  , dialogDefaultLocation :: !(Maybe FilePath)
  -- ^ Starting folder or file.
  , dialogAllowMany :: !Bool
  -- ^ Allow selecting more than one entry (ignored by save dialogs).
  }
  deriving (Eq, Show)

-- | Sensible defaults: no filters, no default location, single selection.
defaultFileDialogOptions :: FileDialogOptions
defaultFileDialogOptions = FileDialogOptions [] Nothing False

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

-- | The last handle given out, and the tracked dialogs by handle: the cell
-- each one's callback writes its result to. Native dialogs belong to the
-- process, like the callback they share, rather than to a session.
{-# NOINLINE dialogs #-}
dialogs :: IORef (Int, IM.IntMap (IORef FileDialogResult))
dialogs = unsafePerformIO (newIORef (0, IM.empty))

-- | Launch an open-file dialog. Returns a handle to poll for completion.
openFileDialog :: SdlEnv -> FileDialogOptions -> IO FileDialogId
openFileDialog env = launchDialog env OpenDialog

-- | Launch a save-file dialog. Returns a handle to poll for completion.
saveFileDialog :: SdlEnv -> FileDialogOptions -> IO FileDialogId
saveFileDialog env = launchDialog env SaveDialog

-- | Launch a folder-selection dialog. Returns a handle to poll for completion.
openFolderDialog :: SdlEnv -> FileDialogOptions -> IO FileDialogId
openFolderDialog env = launchDialog env FolderDialog

-- | Poll a previously launched dialog without blocking.
--
-- Each result is delivered exactly once: after the dialog completes, the
-- first poll that observes the finished state returns it and forgets the
-- handle, so later polls return 'FileDialogUnknown'.
pollFileDialog :: SdlEnv -> FileDialogId -> IO FileDialogResult
pollFileDialog env (FileDialogId did) = do
  result <- maybe (pure FileDialogUnknown) readIORef . IM.lookup did . snd =<< readIORef dialogs
  case result of
    FileDialogPending -> pure result
    FileDialogUnknown -> pure result
    _ -> do
      -- Only the poll that takes the handle out delivers the result.
      taken <- atomicModifyIORef' dialogs $ \(n, m) -> ((n, IM.delete did m), IM.member did m)
      if not taken
        then pure FileDialogUnknown
        else do
          -- The native dialog stole window focus; reclaim it so the app keeps
          -- receiving hover/motion/wheel events without an extra click.
          -- Restoration is a best-effort no-op when the window was never
          -- minimized (its result is platform-dependent, so it is not a
          -- reliable failure signal); only a failed raise means the window
          -- may still lack focus and worth an audible warning.
          void (restoreWindowSafe (sdlWindow env))
          raised <- raiseWindowSafe (sdlWindow env)
          unless raised $
            hPutStrLn stderr "nano-ui: dialog completed but window raise failed; input may need a click"
          -- The dialog finished; request a redraw so the caller can reflect
          -- the result. Safe here: this runs on the polling (UI) thread.
          markDirty =<< readIORef (sdlCachedCtx env)
          pure result

-- | Stop tracking a dialog handle without waiting for the native dialog to
-- finish. The handle returns 'FileDialogUnknown' if polled afterwards.
-- The native dialog keeps running until the user dismisses it; its result is
-- discarded.
cancelFileDialog :: SdlEnv -> FileDialogId -> IO ()
cancelFileDialog _ (FileDialogId did) =
  atomicModifyIORef' dialogs $ \(n, m) -> ((n, IM.delete did m), ())

-- | Open-file dialog, usable from within 'NanoUI' widget code. Returns
-- 'Nothing' when there is no SDL host to launch a dialog.
askOpenFileDialog :: Ui :> es => FileDialogOptions -> Eff es (Maybe FileDialogId)
askOpenFileDialog opts = askHost >>= traverse (uiIO . (`openFileDialog` opts))

-- | Save-file dialog, usable from within 'NanoUI' widget code. Returns
-- 'Nothing' when there is no SDL host to launch a dialog.
askSaveFileDialog :: Ui :> es => FileDialogOptions -> Eff es (Maybe FileDialogId)
askSaveFileDialog opts = askHost >>= traverse (uiIO . (`saveFileDialog` opts))

-- | Folder dialog, usable from within 'NanoUI' widget code. Returns
-- 'Nothing' when there is no SDL host to launch a dialog.
askOpenFolderDialog :: Ui :> es => FileDialogOptions -> Eff es (Maybe FileDialogId)
askOpenFolderDialog opts = askHost >>= traverse (uiIO . (`openFolderDialog` opts))

-- | Poll a dialog from within 'NanoUI' widget code.
pollFileDialogUi :: Ui :> es => FileDialogId -> Eff es FileDialogResult
pollFileDialogUi did = askHost >>= maybe (pure FileDialogUnknown) (uiIO . (`pollFileDialog` did))

data DialogKind = OpenDialog | SaveDialog | FolderDialog
  deriving (Eq)

launchDialog :: SdlEnv -> DialogKind -> FileDialogOptions -> IO FileDialogId
launchDialog env kind opts = do
  -- SDL reads the filters and the location until it runs the callback, which
  -- frees them.
  names <-
    forM (if kind == FolderDialog then [] else dialogFilters opts) $ \(FileFilter name pattern_) ->
      (,) <$> newCString (T.unpack name) <*> newCString (T.unpack pattern_)
  filters <-
    if null names
      then pure nullPtr
      else newArray [SDL_DialogFileFilter (PtrConst.unsafeFromPtr n) (PtrConst.unsafeFromPtr p) | (n, p) <- names]
  location <- traverse newCString (dialogDefaultLocation opts)
  result <- newIORef FileDialogPending
  let release = do
        mapM_ (\(n, p) -> free n >> free p) names
        free filters
        mapM_ free location
  -- Register the handle before showing: the callback may fire before this
  -- function returns, and its result must be found.
  did <- atomicModifyIORef' dialogs $ \(n, m) -> ((n + 1, IM.insert (n + 1) result m), n + 1)
  userdata <- castPtr . castStablePtrToPtr <$> newStablePtr (result, release)
  let win = sdlWindow env
      filtersConst = PtrConst.unsafeFromPtr filters
      nfilters = fromIntegral (length names)
      locationConst = PtrConst.unsafeFromPtr (fromMaybe nullPtr location)
  case kind of
    OpenDialog ->
      showOpenFileDialogSafe dialogCallback userdata win filtersConst nfilters locationConst (dialogAllowMany opts)
    SaveDialog ->
      showSaveFileDialogSafe dialogCallback userdata win filtersConst nfilters locationConst
    FolderDialog ->
      showOpenFolderDialogSafe dialogCallback userdata win locationConst (dialogAllowMany opts)
  pure (FileDialogId did)

-- | The callback every dialog shares, made once a process. A launch's
-- userdata carries its result cell and the release of its buffers, so there
-- is no callback to free while SDL might still call it.
{-# NOINLINE dialogCallback #-}
dialogCallback :: SDL_DialogFileCallback
dialogCallback = unsafePerformIO (SDL_DialogFileCallback . castFunPtr <$> mkDialogCallback onResult)

-- | SDL invoked the callback: decode the file list, release the FFI buffers
-- the launch owned, record the outcome, and only then wake the (possibly
-- idle) event loop. The status must be visible before the wake, or the woken
-- frame polls 'FileDialogPending', skips, and the result waits for an
-- unrelated event. A null list pointer means SDL hit an error; a null first
-- entry means the user canceled.
onResult :: Ptr () -> Ptr () -> Int32 -> IO ()
onResult userdata filelist _filterIdx = do
  let launch = castPtrToStablePtr userdata
  (result, release) <- deRefStablePtr launch
  freeStablePtr launch
  paths <- maybePeek (peekArray0 nullPtr >=> traverse peekCString) (castPtr filelist)
  release
  atomicWriteIORef result $ case paths of
    Nothing -> FileDialogFailed
    Just [] -> FileDialogCancelled
    Just ps -> FileDialogSelected ps
  pushRefreshEvent

-- SDL_DialogFileCallback is `void (*)(void *, const char * const *, int)`,
-- flattened to `void *` pointers at the FFI boundary.
foreign import ccall "wrapper"
  mkDialogCallback ::
    (Ptr () -> Ptr () -> Int32 -> IO ()) -> IO (FunPtr (Ptr () -> Ptr () -> Int32 -> IO ()))
